## mix zig.get

Provides `zig.get` installer as an archive.

This installer will obtain the zig compiler toolchain and put it into 
a user cache directory.

To install from Hex, run:

    $ mix archive.install hex zig_get

To build and install it locally,
ensure any previous archive versions are removed:

    $ mix archive.uninstall zig_get

Then run:

    $ cd installer
    $ MIX_ENV=prod mix do archive.build, archive.install
