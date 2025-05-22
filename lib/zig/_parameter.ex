defmodule Zig.Parameter do
  @moduledoc false

  @enforce_keys [:cleanup]
  defstruct @enforce_keys ++ ~w[in_out type sentinel]a

  alias Zig.Options
  alias Zig.Type
  alias Zig.Type.Cpointer

  import Type, only: [sigil_t: 2]

  # information supplied by the user. 
  @type unmerged :: %__MODULE__{
          cleanup: boolean,
          in_out: boolean
        }

  # information obtained by semantic analysis.  Cleanup must be present
  # as the cleanup clause is inherited by the module rules cleanup.
  @type sema :: %__MODULE__{
          cleanup: boolean,
          type: Type.t()
        }

  # type as merged after semantic analysis.
  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          in_out: boolean,
          sentinel: boolean
        }

  @spec new(Zig.parameter_options(), Options.context()) :: unmerged
  def new(options, context) do
    options
    |> List.wrap()
    |> Options.normalize(:cleanup, Options.boolean_normalizer(noclean: false), context)
    |> Options.normalize(:in_out, Options.boolean_normalizer(in_out: true), context)
    |> Options.normalize(:sentinel, Options.boolean_normalizer(sentinel: true), context)
    |> Options.scrub_non_keyword(context)
    |> force_in_out_no_cleanup()
    |> Keyword.put_new(:cleanup, true)
    |> then(&struct!(__MODULE__, &1))
  rescue
    e in KeyError ->
      Options.raise_with("was supplied the invalid option `#{e.key}`", context)
  end

  def force_in_out_no_cleanup(options) do
    if options[:in_out] do
      Keyword.put(options, :cleanup, false)
    else
      Keyword.put(options, :in_out, false)
    end
  end

  # merging semantic analysis with unmerged options

  @spec merge(sema, unmerged) :: t
  def merge(sema, specified) do
    %{merge_sentinel(sema, specified) | cleanup: specified.cleanup, in_out: specified.in_out}
  end

  defp merge_sentinel(sema, %{sentinel: nil}), do: sema

  defp merge_sentinel(sema, %{sentinel: sentinel}) do
    %{sema | type: set_sentinel(sema.type, sentinel)}
  end

  defp set_sentinel(type, sentinel) do
    case {type, sentinel} do
      {%Cpointer{child: ~t[u8]}, true} ->
        %{type | sentinel: 0}

      {%Cpointer{child: %Cpointer{}}, true} ->
        %{type | sentinel: :null}

      {type, false} ->
        type

      {type, sentinel} ->
        raise CompileError,
          description: "sentinel #{sentinel} not supported for type `#{Type.render_zig(type)}`"
    end
  end

  # code rendering

  def render_accessory_variables(parameter, index),
    do: Type.render_accessory_variables(parameter.type, parameter, "arg#{index}")

  def render_payload_options(parameter, index) do
    parameter.type
    |> Type.payload_options("arg#{index}")
    |> then(fn
      options when parameter.in_out -> Keyword.put(options, :in_out, "&arg#{index}")
      options -> options
    end)
    |> assemble_payload_options
  end

  defp assemble_payload_options(options) do
    [".{", Enum.map(options, fn {k, v} -> ~s(.#{k} = #{v},) end), "},"]
  end

  def render_cleanup(parameter, index) do
    cond do
      parameter.cleanup ->
        Type.render_cleanup(parameter.type, index)

      :else ->
        ".{.cleanup = false},"
    end
  end
end
