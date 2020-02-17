defmodule Zigler.Parser.Nif do

  @moduledoc """
  This datastructure represents structured information about a single nif
  inside of a `Zigler.sigil_Z/2` block.  This is used to generate the
  `exported_nifs` variable which is an array of `ErlNifFunc` structs.  The
  following keys are implemented:

  - name: (`t:atom/0`) the function name to be bound into the module
  - arity: (`t:arity/0`) the arity of the erlang function (the zig
    function may have a different arity).
  - doc: (`t:iodata/0`) zig docstrings which should be turned into elixir docs
  - params: (`t:String.t/0`) a list of zig types which are the parameters for
    the function
  - retval: (`t:String.t/0`) the type of the return value
  - opts: (`t:keyword`) list of nif options.
    - long: true  -- if the nif should run in a separate OS thread.
    - dirty: :cpu -- if the nif should run in a dirty cpu scheduler.
    - dirty: :io  -- if the nif should run in a dirty io scheduler.
  """

  @float_types  ~w(f16 f32 f64)
  @int_types    ~w(u16 i32 u32 i64 u64 c_int c_uint c_long c_ulong isize usize)
  @bool         ["bool"]
  @char         ["u8"]
  @beam_params  ~w(beam.term beam.atom beam.pid)
  @enif_params  ~w(e.ErlNifTerm e.ErlNifPid)
  @scalar_types @float_types ++ @int_types ++ @bool ++ @char ++ @beam_params ++ @enif_params
  @void         ["void"]
  @env          ~w(?*e.ErlNifEnv beam.env)
  @array_types  Enum.flat_map(@scalar_types, &["[]#{&1}", "[*c]#{&1}", "[_]#{&1}"])

  @valid_params  @scalar_types ++ @array_types ++ @env

  @enforce_keys [:name, :arity]

  defstruct @enforce_keys ++ [
    doc:    nil,
    params: [],
    retval: nil,
    opts:   []
  ]

  @type option ::
    {:long, boolean} |
    {:dirty, :cpu | :io}

  @type t :: %__MODULE__{
    name: String.t,
    arity: arity,
    doc:    iodata | nil,
    params: [String.t],
    retval: String.t,
    opts: [option]
  }

  @beam_envs ["beam.env", "?*e.ErlNifEnv"]

  # validate_arity/3: checks to make sure the arity of nif declaration matches the function
  @spec validate_arity([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return

  def validate_arity([env | rest], context, line) when env in @beam_envs do
    validate_arity(rest, context, line)
  end
  def validate_arity(rest, context = %{local: %{arity: arity}}, line) when length(rest) != arity do
    raise CompileError,
      file: context.file,
      line: line,
      description: "nif declaration arity (#{arity}) doesn't match the expected function arity #{length(rest)}"
  end
  def validate_arity(_, _, _), do: :ok

  # validate_params/3 : raises if an invalid parameter type is sent to to the function
  @spec validate_params([String.t], t, non_neg_integer)
    :: :ok | no_return
  def validate_params([], _context, _line), do: :ok
  def validate_params([params | rest], context, line) when params in @valid_params do
    validate_params(rest, context, line)
  end
  def validate_params([invalid_type | _], context, line) do
    raise CompileError,
      file: context.file,
      line: line,
      description: "nif function #{context.local.name} demands an invalid parameter type #{invalid_type}"
  end
  def validate_params(_, _, _), do: :ok


end
