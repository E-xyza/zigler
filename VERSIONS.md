# Zig versions

## 0.1.0

- zig compilation and automatic nif generation
- safety checks on ingress and egress data
- documentation of zig code through ExDoc
- unit test integration

## 0.1.1

Thanks to Dave Cottlehuber @dch for testing.

- freebsd tested and unsupported Logger error removed.
- zig documentation is now correctly linked.

## 0.1.2

- updated documentation for `mix zig_doc`
- fixed compilation so that importing zig stdlib doesn't error
- added better compilation failures for bad types in args and retvals
- added `beam` struct support for ok and error tuples

## 0.1.3

- fixed error reporting for the `/// nif:` directive
- added ok/error tuple with string
- added c header path support
- restored documentation for exceptions

## 0.2.0

- general rewrite of nif parsing routines
- initial support for resources
- experimental addition of `:dirty`, `:long` and `:safe` function directives

## 0.3.0-pre

- supported zig version: 0.6.0
- disabled `:long` mode

## 0.3.0-pre3

- support for (and autodetect) nerves
- use erlang nif headers taken from the `include` directory of your erts.
- correct and verified support for releases
- adds a mechanism for patching errors in zig/llvm releases
- adds ~z for substituted values
- zigler fetches zig on compile when it's in prod.

## 0.3.0

- verified that zig tests run correctly
- updated documentation.
  - preliminary "under the hood" category.

## 0.3.1

- support for `amd64` in freebsd, (thanks @dch)

## 0.3.2

- better support for nerves (thanks @fhunleth)
