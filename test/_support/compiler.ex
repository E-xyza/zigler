defmodule ZiglerTest.Compiler do
  @root_dir "test/code"

  def init do
    if File.dir?(@root_dir) do
      File.rm_rf!(@root_dir)
    end

    File.mkdir_p!(@root_dir)

    :code.add_pathz(~c'#{@root_dir}')
  end

  defmacro compile(file) do
    quote do
      [{mod, bin}] =
        __DIR__
        |> Path.join(unquote(file))
        |> Code.compile_file()

      beamfile = Path.join(unquote(@root_dir), "#{mod}.beam")

      File.write!(beamfile, bin)

      mod
    end
  end

  def compile_erlang(file) do
    {:ok, mod} =
      :compile.file(file, [
        :return_errors,
        outdir: :filename.join(:code.lib_dir(:zigler), ~c"ebin")
      ])

    Code.ensure_loaded(mod)
  end
end
