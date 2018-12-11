# What to do to publish a new release

1. Ensure all notable changes are in the changelog under "Unreleased".

2. Execute `cargo release <level>` to bump version(s), tag and publish
   everything. External subcommand, must be installed with `cargo install
   cargo-release`.
   
   `<level>` can be one of `major|minor|patch`. If this is the first release
   (`0.1.0`), don't specify the level at all or you will publish `0.1.1`
   instead.

3. Go to GitHub and "add release notes" to the just-pushed tag. Copy them from
   the changelog.
