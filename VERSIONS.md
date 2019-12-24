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
- added better compilation failures for bad types in params and retvals
- added `beam` struct support for ok and error tuples

## 0.1.3

- fixed error reporting for the `/// nif:` directive
- added ok/error tuple with string
- added c header path support
- restored documentation for exceptions