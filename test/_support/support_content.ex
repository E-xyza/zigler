defmodule ZiglerTest.SupportContent do

  @zig_dir_path Path.expand("../../zig", Path.dirname(__ENV__.file))

  defp directory, do: Path.join(@zig_dir_path, "zig-#{Zig.Command.os_arch()}-#{Zig.version()}")

  def ensure_lib(path_to_lib, path_to_code) do
    unless File.exists?(path_to_lib) do
      zig_path = Path.join(directory(), "zig")
      System.cmd(zig_path, ~w(build-lib #{path_to_code} -fPIC -static -femit-bin=#{path_to_lib}))

      o_file = "#{path_to_lib}.o"

      if File.exists?(o_file) do
        File.rm!(o_file)
      end
    end
  end
end