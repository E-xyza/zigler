defmodule Zig.Sema do
  alias Zig.Type

  require EEx

  EEx.function_from_string(
    :defp,
    :file_for,
    """
    const std = @import("std");
    const tgt = @import("<%= opts[:file] %>");

    pub fn main() !void {
      const stdout = std.io.getStdOut().writer();
      <%= for nif <- opts[:nifs] do %>
      const <%= nif %> = @typeInfo(@TypeOf(tgt.<%= nif %>)).Fn;
      try stdout.print("<%= nif %> {any}", .{<%= nif %>.return_type});
      inline for (<%= nif %>.args) |param| {
        try stdout.print(" {any}", .{param.arg_type});
      }
      try stdout.print("\\n", .{});
      <% end %>
    }
    """,
    [:opts]
  )

  def analyze_file!(file, opts) do
    dir = Path.dirname(file)
    shim_file = Path.join(dir, "shim.zig")

    opts = Keyword.merge([file: Path.basename(file)], opts)

    system_dir = "#{:code.root_dir()}/erts-#{:erlang.system_info(:version)}/include"

    File.write!(shim_file, file_for(opts))
    {result, 0} = System.cmd("zig", ["run", "-I#{system_dir}", "-lc", shim_file])

    result
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [name, return | params] = String.split(line)

      return = Type.parse(return)

      params =
        case Enum.map(params, &Type.parse/1) do
          [Env | params] -> params
          params -> params
        end

      # for now, we will need to take out beam.env's.
      arity = length(params)

      %Zig.Type.Function{name: String.to_atom(name), arity: arity, params: params, return: return}
    end)
  end
end
