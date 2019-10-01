defmodule Zigler.Compiler do

  alias Zigler.Zig

  @zig_nif_h Application.app_dir(:zigler, "priv/include/erl_nif.h")

  defmacro __before_compile__(context) do
    app = Module.get_attribute(context.module, :zigler_app)

    zig_code = Module.get_attribute(context.module, :zig_code)
    zig_specs = Module.get_attribute(context.module, :zig_specs)
    |> Enum.flat_map(&(&1))

    full_code = [Zig.nif_header(@zig_nif_h),
      zig_code,
      Enum.map(zig_specs, &Zig.nif_adapter/1),
      Zig.nif_exports(zig_specs),
      Zig.nif_footer(context.module, zig_specs)]

    mod_name = context.module |> Atom.to_string |> String.downcase
    tmp_dir = Path.join("/tmp/.elixir-nifs", mod_name)
    nif_dir = Application.app_dir(app, "priv/nifs")

    File.mkdir_p!(tmp_dir)

    Enum.into(full_code, File.stream!(Path.join(tmp_dir, "zig_nif.zig")))

    # now use zig to build the library in the temporary directory
    System.cmd("zig", ~w(build-lib zig_nif.zig -dynamic --disable-gen-h), cd: tmp_dir)

    # move the dynamic library out of the temporary directory and into the priv directory.
    File.cp(Path.join(tmp_dir, "libzig_nif.so.0.0.0"), Path.join(nif_dir, mod_name <> ".so"))

    quote do
    end
  end
end
