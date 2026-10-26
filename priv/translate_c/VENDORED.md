# Vendored translate-c

This is a copy of the ZSF `translate-c` package, which zigler uses to translate
`erl_nif.h` (and any headers you pull in with the `headers:` option) into zig.
Zig 0.17 deprecated the built-in `std.Build.Step.TranslateC` in favour of it.

It is vendored rather than fetched because hex packages may only depend on other
hex packages -- a git dependency cannot be published -- and because compiling a
nif should not require network access.  This is the same approach phoenix
live_view takes with its javascript: non-elixir payload ships as files in the
tarball.

- upstream: https://codeberg.org/ziglang/translate-c (branch `zig-0.17.x`)
- commit:   02ff0c523fb92a38939b04d0145c4a9602a74d56
- license:  MIT, see LICENSE

Only the paths that upstream's own `build.zig.zon` declares shippable are kept.

## Local change

`build.zig.zon` declares aro as `.{ .path = "../aro" }` rather than the upstream
`git+https://...#<hash>` url, so that `zig build` resolves it from the sibling
`priv/aro` copy instead of reaching for the network.  Nothing else is modified.

## Updating

Re-copy the declared paths from a fresh checkout at the new commit, re-apply that
one `build.zig.zon` edit, and update the commit hash above.
