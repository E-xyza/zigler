defmodule Zig.Resources do
  require EEx

  @type t :: atom | {:root, atom}

  resources = Path.join(__DIR__, "templates/resources.zig.eex")
  EEx.function_from_file(:def, :render, resources, [:assigns])

  def resource_decl({:root, resource}), do: resource_decl(resource)

  def resource_decl(resource) when is_atom(resource) do
    "var typeFor#{resource}: *e.ErlNifResourceType = undefined;"
  end

  def resource_prong({:root, resource}), do: "#{resource} => typeFor#{resource},"

  def resource_prong(resource) when is_atom(resource) do
    "#{call_for(resource)} => typeFor#{resource},"
  end

  def init_resource_type({:root, resource}, module) do
    "typeFor#{resource} = #{resource}.init(\"#{module}\", env);"
  end

  def init_resource_type(resource, module) when is_atom(resource) do
    "typeFor#{resource} = #{call_for(resource)}.init(\"#{module}\", env);"
  end

  @builtins ~w(isize usize c_short c_ushort c_int c_uint c_long c_ulong c_longlong c_ulonglong c_longdouble f16 f32 f64 f80 f128 bool)

  defp call_for(resource) do
    case Atom.to_string(resource) do
      "i" <> rest ->
        call_for_int("i", rest)

      "u" <> rest ->
        call_for_int("u", rest)

      builtin when builtin in @builtins ->
        builtin

      other ->
        "nif.#{other}"
    end
  end

  defp call_for_int(prefix, rest) do
    case Integer.parse(rest) do
      {int, ""} when int in 0..65_535 -> "#{prefix}#{int}"
      _ -> "nif.#{prefix}#{rest}"
    end
  end
end
