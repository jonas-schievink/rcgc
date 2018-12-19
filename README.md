# A garbage collector using Rc/Weak

[![crates.io](https://img.shields.io/crates/v/rcgc.svg)](https://crates.io/crates/rcgc)
[![docs.rs](https://docs.rs/rcgc/badge.svg)](https://docs.rs/rcgc/)
[![Build Status](https://travis-ci.org/jonas-schievink/rcgc.svg?branch=master)](https://travis-ci.org/jonas-schievink/rcgc)

This crate implements a pretty simple garbage collector. Its selling point is
that it uses no `unsafe` at all: Instead, it represents references between
objects as `Weak` pointers that are upgraded to `Rc`s on access.

The GC holds an `Rc` to every object on its heap and can hand out further `Rc`s
acting as roots. It can then locate rooted objects and perform a tracing
garbage collection to free objects with no incoming references (even in the
presence of cycles). It also uses the underlying reference counters as a faster
way to locate garbage objects: If an object has a weak count of 0 and a strong
count of 1, it is only reachable via the GCs own `Rc`, not via any object
reference, and can be freed immediately without having to perform a tracing
phase.

If there's a bug in the GC, and an object ends up getting collected while it's
still reachable, any access to the object will fail to upgrade the `Weak`
pointer and panic instead of becoming an unsafe use-after-free.

Please refer to the [changelog](CHANGELOG.md) to see what changed in the last
releases.

## Usage

Start by adding an entry to your `Cargo.toml`:

```toml
[dependencies]
rcgc = "0.0.0"
```

Then import the crate into your Rust code:

```rust
extern crate rcgc;
```
