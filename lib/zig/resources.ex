defmodule Zig.Resources do
  require EEx

  resources = Path.join(__DIR__, "templates/resources.zig.eex")
  EEx.function_from_file(:def, :render, resources, [:assigns])

  def resource_decl(resource) when is_atom(resource) do
    "var typeFor#{resource}: *e.ErlNifResourceType = undefined;"
  end

  def resource_prong(resource) when is_atom(resource) do
    "nif.#{resource} => typeFor#{resource},"
  end

  def init_resource_type(resource, module) when is_atom(resource) do
    "typeFor#{resource} = beam.resource.init(nif.#{resource}, \"#{module}\", env, .{});"
  end
end
