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
    end
  end
end
