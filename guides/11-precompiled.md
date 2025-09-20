# Precompiled Modules

Zigler provides a way to precompile modules. This is useful for packaging libraries providing nifs,
without requiring the user to install the zig compiler.

## Local assets

To use a locally precompiled library to load NIF code, supply the `:precompiled` option to `use
Zig`. Paths starting with `./` are relative to the project, files starting with `/` are absolute,
and all other paths are relative to the elixir file.

for example:

```elixir
  use Zig, otp_app: :zigler, precompiled: "./priv/lib/precompiled.so"
```

The file at the provided path will be copied to the priv directory normalized to the expected nif
filename.

## Assets on the web.

It's also possible to store assets on the web, by providing the triple `{:web, address, shasum}`.

The base case is to have a single file, in which case the address is directly pulled and the shasum
should be a base-16 encoded sha256 string:

for example:

```elixir
    use Zig, 
      otp_app: :zigler, 
      precompiled: {:web, "https://address-for-archive/precompiled-artifact.so", "935f9829d4c0058acba4118c9dc1a98dbdda5c4035e16a7893c33a3aff2caee8"}, 
      ...
```

For multiplatform files, you can provide an address template and a keyword list of hashes for each
platform triple. The following values will be interpolated:

- `#VERSION` - version number for your project
- `#TRIPLE` - architecture-os-abi triple. This will match the keywords in the shasum lookup
- `#EXT` - os-based file extension. For windows, this is `.dll`, for all other OSes it's `.so`.

```elixir
  @lib_address "https://address-for-archived-libraries/MyModuleName.#VERSION.#TRIPLE.#EXT"

  @shasum [
    "aarch64-freebsd-none": "e7b413de8bbf37876773dac05f3410f0bf3cc80f99d897557b6819e8189fb006",
    "aarch64-linux-gnu": "17546c34adf8b6a14dd38ebb9d5485610348e5afff72e88b991f18d6b818197f",
    "aarch64-linux-musl": "4ccae1cb87fbef01a556b7e8834da256a5dfa6f8a68d57d95005680ce4e37e16",
    "aarch64-macos-none": "5006c503b8b5d4efff875a2979f6aab7e1cc3c0360fa8618343157964aec1d15",
    "arm-linux-gnueabi": "4e56cd1447ba3b565756d7298a6d89e8cbff13966a27c56fce5a72ad23a9ea4d",
    "arm-linux-gnueabihf": "2355e863c21c1120de0776c98b3ab268e6d633ee3e48d47a6477c9a4a4efee58",
    "arm-linux-musleabi": "5de29e68005eddf6fa31d6f10532de094071fef89f9ffc3eb701e0d0ea550ff3",
    "arm-linux-musleabihf": "28bc7c872d21eb8524bb2c35b502f4f42f10f0f05c2bc4420b45142e51224b35",
    "x86_64-freebsd-none": "a7ff7317461d0a43ffac60c131320c3e5e005f0e75f9562bb710718c476c85fc",
    "x86_64-linux-gnu": "e38e1dfdd85d711f85e0ecbb42fc34e102cd9dc37079c200e2075cb894acad7c",
    "x86_64-linux-musl": "857b0ed35c78b588fa43dc4a5c4e3dd9143d9102c559a67812b0af6185eae117",
    "x86_64-macos-none": "e5ff347c2d11b463b5e6f0097687d578c800e7fbb725aa33a2e802d3f473371c",
    "x86_64-windows-gnu": "3ee18aec252eca92f19d920f6de143e59700e0f038916fb3717aa9869b2c5102",
    "x86-linux-gnu": "9ef68f25143827e6cbea13fa264f0002d7d1f2fc2ad7c07febe5be5fbc4f75c6",
    "x86-linux-musl": "227afab8c4a20b4dc88ae0efeb0f6a93952c9ad54aa3165d166cebd66ae5d158",
    "x86-windows-gnu": "4602d68145becb66d4f357ab163a5cb8a7f5466a4004cd6419f998c241b52d2b"
  ]

  use Zig, 
    otp_app: :zigler, 
    precompiled: {:web, @lib_address, @shasum},
    ...
```

## Forcing reload

You may force reloading the dynamic library by setting `ZIGLER_PRECOMPILE_FORCE_RELOAD`. This will
purge the nif file and set it to whatever contents are provided by the url or the local file.

## Forcing recompilation

you may force recompilation of precompiled modules by setting `ZIGLER_PRECOMPILE_FORCE_RECOMPILE`.
