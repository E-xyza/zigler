default_locals = [
  defdelegate: 1,
  throw: 1
]

[
  inputs: ~w[
    {mix,.formatter,.credo}.exs
    {config,lib,rel,test}/**/*.{ex,exs,zig}
    installer/**/*.{ex,exs}
  ],
  locals_without_parens: default_locals,
  plugins: [Zig.Formatter]
]
