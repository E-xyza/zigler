defmodule ZiglerTest.SupportContent do

  @zig_dir_path Path.expand("../../zig", Path.dirname(__ENV__.file))

  def ensure_lib(path_to_lib, path_to_code) do
    unless File.exists?(path_to_lib) do
      zig_path = Zig.Command.executable_path()
      System.cmd(zig_path, ~w(build-lib #{path_to_code} -fPIC -static -femit-bin=#{path_to_lib}))

      o_file = "#{path_to_lib}.o"

      if File.exists?(o_file) do
        File.rm!(o_file)
      end
    end
  end
end