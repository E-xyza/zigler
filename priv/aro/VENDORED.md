# Vendored aro

The [Aro](https://codeberg.org/ziglang/arocc) C frontend, which the vendored
`translate-c` next to this directory depends on.  See `../translate_c/VENDORED.md`
for why both are vendored rather than fetched.

- upstream: https://codeberg.org/ziglang/arocc
- commit:   d0c8c4d9c55daa7ef6e40cf0f630a5b5e900989b
- license:  MIT, plus LICENSE-UNICODE for the unicode tables

Unmodified.  Only the paths that upstream's own `build.zig.zon` declares shippable
are kept, which excludes its test suite.
