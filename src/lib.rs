/*

Problems:
1. Integrating "normal" tracing GC in Rust == Hard and Unsafe
2. RC-based GC -> Leaks cycles (cf. leakpocalypse)
  * Solution is usually tracing GC on top of RC (brings us back to problem 1)

Observation:
Rust has `Rc<T>`, a reference-counted pointer to a shared `T`, as well as
`Weak<T>`, a "weak" reference-counted pointer. The `Weak<T>`s to an object use a
separate reference count and can be (fallibly) "upgraded" to a full `Rc<T>`
which gives you access to the `T`.
When no `Rc<T>`s are around, the `T` gets dropped. When there's still `Weak<T>`s
around, the actual space allocated to store the refcounts and the `T` stays
allocated until the last `Weak<T>` is dropped.

Let's use `Weak<T>` as our primary reference to a GC'd object. This means that
we have to keep one `Rc<T>` per object alive. The GC does that. When accessing
an object, we upgrade the `Weak<T>` to an `Rc<T>` temporarily. This can never
fail, since the GC always ensures objects that can still be referenced are kept
around (except in the presence of bugs).

When the GC performs a collection, it can now use the weak reference count to
guide it:
* for all objects
  * if the `Rc<T>`s weak count is 0
    * destroy the object, since no references to it exist anymore

This will not collect cycles, since those cause weak counts to never drop to 0.
For cycle collection, we basically have to build the usual tracing GC
infrastructure: A `Trace` trait, a root set, ...
However, we can do all of that in completely safe Rust, since accesses to
wrongly collected objects are caught. The `Rc` infrastructure can also help us
with that: When the GC notices that an object's "strong" count (the number of
`Rc<T>`s referring to the object) is larger than 1 (the GC always has exactly 1
`Rc` to every live object), it knows that the code calling the GC has another
reference to the object somewhere *and* is unable to free the object due to the
semantics of `Rc<T>`.

(actually this could be rather easily augmented to be done in a generational
fashion, but that's not important here)

*/

#![doc(html_root_url = "https://docs.rs/rcgc/0.1.0")]

use log::trace;
use std::cell::Cell;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::rc::{Rc, Weak};

/// Trait for GC-compatible objects that may contain traceable references to
/// other GC-managed objects.
pub trait Trace<T: Trace + ?Sized = Self> {
    /// Trace all contained `Handle`s to other GC objects by calling
    /// `tracer.trace_handle`.
    fn trace(&self, tracer: &mut Tracer<T>);
}

/// Since GC handles aren't `Copy`, they cannot be contained within types that
/// are `Copy`. Thus any type implementing `Copy` cannot contain references that
/// need tracing, allowing this no-op blanket impl.
impl<T: Copy> Trace for T {
    fn trace(&self, _tracer: &mut Tracer<Self>) {}
}

pub struct Tracer<T: Trace + ?Sized> {
    /// The value of the `traced` metadata field that means "traced" during this
    /// collection.
    traced: bool,
    worklist: Vec<Handle<T>>,
}

impl<T: Trace> Tracer<T> {
    /// Enqueue the object behind `handle` for marking and tracing.
    pub fn trace_handle(&mut self, handle: &Handle<T>) {
        let traced = handle.with(|gc| gc.traced() == self.traced);

        if !traced {
            self.worklist.push(handle.clone());
        }
    }

    /// Starting with the root set in `self.worklist`, marks all transitively
    /// reachable objects by setting their `traced` metadata field to
    /// `self.traced`.
    fn mark_all(&mut self) {
        let mut worklist = mem::replace(&mut self.worklist, Vec::new());

        while !worklist.is_empty() {
            trace!(
                "mark_all iter: processing {} items in worklist",
                worklist.len()
            );
            for handle in worklist.drain(..) {
                trace!("mark_all: marking/tracing {:p}", handle);
                handle.with(|gc| {
                    if gc.traced() != self.traced {
                        // Hasn't been traced yet
                        gc.metadata.traced.set(self.traced);
                        gc.object.trace(self);
                    }
                });
            }

            mem::swap(&mut self.worklist, &mut worklist);
        }
    }
}

/// GC metadata maintained for every object on the heap.
struct Metadata {
    /// The "color" bit, indicating whether we've already traced this object
    /// during this collection, used to prevent unnecessary work.
    traced: Cell<bool>,
}

/// Representation of a `T` on the garbage-collected heap.
struct GcData<T: Trace + ?Sized> {
    metadata: Metadata,
    object: T,
}

impl<T: Trace + ?Sized> GcData<T> {
    /// Gets the value of the `traced` metadata field.
    ///
    /// The meaning of this value changes every collection and depends on GC
    /// state.
    fn traced(&self) -> bool {
        self.metadata.traced.get()
    }
}

/// A handle to a garbage-collected object of type `T`.
///
/// A handle does not by itself keep the object it points to alive.
pub struct Handle<T: Trace + ?Sized> {
    inner: Weak<GcData<T>>,
}

impl<T: Trace> Handle<T> {
    fn from_weak(weak: Weak<GcData<T>>) -> Self {
        Self { inner: weak }
    }

    pub(crate) fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Rc<GcData<T>>) -> R,
    {
        f(self.inner.upgrade().expect("use after free"))
    }
}

impl<T: Trace> Trace<T> for Handle<T> {
    fn trace(&self, tracer: &mut Tracer<T>) {
        tracer.trace_handle(self);
    }
}

impl<T: Trace> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Handle {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Trace> fmt::Pointer for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.with(|rc| write!(f, "{:p}", rc))
    }
}

/// A rooted reference to a garbage-collected object of type `T` that keeps the
/// object alive as long as it exists.
pub struct Rooted<T: Trace> {
    inner: Rc<GcData<T>>,
}

impl<T: Trace> Rooted<T> {
    /// Creates a new garbage-collected unrooted handle to the object.
    ///
    /// As long as `self` still exists, the handle will not be invalidated.
    pub fn new_handle(&self) -> Handle<T> {
        Handle::from_weak(Rc::downgrade(&self.inner))
    }
}

impl<T: Trace> Deref for Rooted<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner.object
    }
}

/// A garbage collector managing objects of type `T`.
///
/// When dropped, all unrooted objects will be destroyed. Any rooted objects
/// should no longer be used.
pub struct Gc<T: Trace> {
    /// All allocated objects owned by this GC.
    ///
    /// This is basically a large set of `Rooted` that will be culled during
    /// collection.
    objs: Vec<Rc<GcData<T>>>,
    traced_color: bool,
    /// Total number of objects allocated after which we do the next collection.
    next_gc: usize,
}

impl<T: Trace> Gc<T> {
    pub fn new() -> Self {
        Self {
            objs: Vec::new(),
            traced_color: true,
            next_gc: 32,
        }
    }

    pub fn allocate(&mut self, t: T) -> Rooted<T> {
        let root = self.allocate_nocollect(t);

        if self.estimate_heap_size() >= self.next_gc {
            trace!("heap at {}, collecting", self.estimate_heap_size());
            self.do_collect();

            // Do the next collection after the *remaining* heap has doubled
            self.next_gc = self.estimate_heap_size() * 2;
            trace!(
                "heap remaining: {}. next gc at heap {}",
                self.estimate_heap_size(),
                self.next_gc
            );
        }

        root
    }

    /// Allocate `t` on the garbage-collected heap without triggering a
    /// collection.
    pub fn allocate_nocollect(&mut self, t: T) -> Rooted<T> {
        let rc = Rc::new(GcData {
            metadata: Metadata {
                traced: Cell::new(!self.traced_color), // initially not traced
            },
            object: t,
        });
        self.objs.push(rc.clone());

        Rooted { inner: rc }
    }

    /// Collect each and every garbage object, atomically.
    ///
    /// The root set is determined by taking all objects whose strong reference
    /// count `>1`. This only happens during active access (which implies that
    /// the object is still reachable) and because there's a `Rooted` instance
    /// pointing to the object.
    ///
    /// As an optimization, whenever we come across an object with a weak count
    /// of 0, we know that it has no traced reference pointing to it. If that
    /// object also has a strong count of 1, that object isn't rooted (the only
    /// strong reference coming from the GC itself) and can be freed immediately
    /// without having to finish the mark phase. Note that this might in turn
    /// drop the weak count to other objects to 0 and make them collectible.
    pub fn force_full_collect(&mut self) {
        let size_before_collect = self.estimate_heap_size();

        // Keep all objects that are rooted or have references pointing to them
        // TODO split this into 2 generations (and maybe an additional root list?)
        for i in 1.. {
            let before = self.objs.len();
            self.objs
                .retain(|obj| Rc::strong_count(obj) > 1 || Rc::weak_count(obj) > 0);

            trace!(
                "collect weak fp: iteration {}, {}->{}",
                i,
                before,
                self.objs.len()
            );

            // Run until fixpoint
            if self.objs.len() == before {
                trace!(
                    "collect weak fp: reached fixpoint after {} iteration(s) (heap {}->{})",
                    i,
                    size_before_collect,
                    self.estimate_heap_size()
                );
                break;
            }
        }

        // Do cycle collection via mark-and-sweep GC

        // Determine root set
        let worklist = self
            .roots()
            .map(Rc::downgrade)
            .map(Handle::from_weak)
            .collect::<Vec<_>>();
        trace!(
            "mark-sweep: initial worklist (root set) len: {}",
            worklist.len()
        );
        trace!("mark-sweep: tracing, traced color={}", self.traced_color);

        // Mark
        let mut tracer = Tracer {
            traced: self.traced_color,
            worklist,
        };
        tracer.mark_all();

        // Sweep. Retain only the objects marked with the current color
        let traced_color = self.traced_color;
        self.objs.retain(|obj| obj.traced() == traced_color);

        // Flip colors
        self.traced_color = !self.traced_color;
    }

    fn do_collect(&mut self) {
        // TODO employ smart heuristics for how much to collect
        trace!("do_collect: we're not very smart, forcing full GC");
        self.force_full_collect();
    }

    fn estimate_heap_size(&self) -> usize {
        self.objs.len()
    }

    /// Returns an iterator over all rooted objects.
    ///
    /// An object is rooted when it has a strong count of at least 2.
    fn roots(&self) -> impl Iterator<Item = &Rc<GcData<T>>> {
        self.objs
            .iter()
            .filter(|rc| Rc::strong_count(rc) > 1)
            .inspect(|rc| {
                trace!("root {:p}", rc);
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::cell::{Cell, RefCell};

    thread_local! {
        static DROPPED_OBJECTS: Cell<usize> = Cell::new(0);
    }

    fn reset_dropped_objects() {
        DROPPED_OBJECTS.with(|o| o.set(0))
    }

    fn dropped_objects() -> usize {
        DROPPED_OBJECTS.with(|o| o.get())
    }

    struct Object {
        refs: RefCell<Vec<Handle<Object>>>,
    }

    impl Trace for Object {
        fn trace(&self, tracer: &mut Tracer<Self>) {
            for handle in self.refs.borrow().iter() {
                tracer.trace_handle(handle);
            }
        }
    }

    impl Object {
        fn new() -> Self {
            Self {
                refs: RefCell::new(Vec::new()),
            }
        }

        fn add_ref(&self, handle: Handle<Object>) {
            self.refs.borrow_mut().push(handle);
        }
    }

    impl Drop for Object {
        fn drop(&mut self) {
            DROPPED_OBJECTS.with(|o| o.set(o.get() + 1));
        }
    }

    fn test<F>(f: F)
    where
        F: FnOnce(Gc<Object>),
    {
        reset_dropped_objects();
        assert_eq!(dropped_objects(), 0);
        env_logger::try_init().ok();

        f(Gc::new());
    }

    /// Creates a pair of cyclic objects, returning a rooted handle to one of
    /// them.
    fn mkcycle(gc: &mut Gc<Object>) -> Rooted<Object> {
        let rooted = gc.allocate_nocollect(Object::new());
        assert_eq!(dropped_objects(), 0);
        let rooted2 = gc.allocate_nocollect(Object::new());
        assert_eq!(dropped_objects(), 0);
        rooted.add_ref(rooted2.new_handle());
        rooted2.add_ref(rooted.new_handle());
        rooted
    }

    #[test]
    fn simple() {
        test(|mut gc| {
            {
                let rooted = gc.allocate_nocollect(Object::new());
                assert_eq!(dropped_objects(), 0);
                let rooted2 = gc.allocate_nocollect(Object::new());
                assert_eq!(dropped_objects(), 0);
                // -> rooted -> rooted2
                rooted.add_ref(rooted2.new_handle());
                gc.force_full_collect();
                assert_eq!(dropped_objects(), 0);
            }
            gc.force_full_collect();
            assert_eq!(dropped_objects(), 2);
        });
    }

    #[test]
    fn traces_references() {
        test(|mut gc| {
            // -> root -> nonroot
            // Make sure that nonroot doesn't get collected.

            let root = gc.allocate_nocollect(Object::new());
            let nonroot = gc.allocate_nocollect(Object::new());
            root.add_ref(nonroot.new_handle());
            drop(nonroot);
            assert_eq!(dropped_objects(), 0);
            gc.force_full_collect();
            assert_eq!(dropped_objects(), 0);
        })
    }

    #[test]
    fn destroys_objects_on_drop() {
        test(|mut gc| {
            {
                let rooted = gc.allocate_nocollect(Object::new());
                assert_eq!(dropped_objects(), 0);
                let rooted2 = gc.allocate_nocollect(Object::new());
                assert_eq!(dropped_objects(), 0);
                rooted.add_ref(rooted2.new_handle());
                assert_eq!(dropped_objects(), 0);
            }
            drop(gc);
            assert_eq!(dropped_objects(), 2);
        });
    }

    #[test]
    fn destroys_cycles_on_drop() {
        test(|mut gc| {
            mkcycle(&mut gc);
            drop(gc);
            assert_eq!(dropped_objects(), 2);
        });
    }

    #[test]
    fn destroys_cycles() {
        test(|mut gc| {
            mkcycle(&mut gc);
            gc.force_full_collect();
            assert_eq!(dropped_objects(), 2);
            drop(gc);
            assert_eq!(dropped_objects(), 2);
        });
    }

    #[test]
    fn collects_when_heap_grows() {
        test(|mut gc| {
            let mut roots = Vec::new();
            for i in 0..100 {
                let root = gc.allocate(Object::new());
                if i % 4 == 0 {
                    roots.push(root);
                }
            }

            assert!(
                gc.estimate_heap_size() < 50,
                "GC did not collect when filling heap (heap size = {})",
                gc.estimate_heap_size()
            );

            gc.force_full_collect();
        });
    }
}
