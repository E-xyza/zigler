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
  - args: (`t:String.t/0`) a list of zig types which are the arguments for
    the function
  - retval: (`t:String.t/0`) the type of the return value
  - opts: (`t:keyword`) list of nif options.
    - long: true  -- if the nif should run in a separate OS thread.
    - dirty: :cpu -- if the nif should run in a dirty cpu scheduler.
    - dirty: :io  -- if the nif should run in a dirty io scheduler.
  """

  alias Zigler.Code.LongRunning
  alias Zigler.Parser.Resource

  @float_types  ~w(f16 f32 f64)
  @int_types    ~w(u16 i32 u32 i64 u64 c_int c_uint c_long c_ulong isize usize)
  @bool         ["bool"]
  @char         ["u8"]
  @beam_args  ~w(beam.term beam.atom beam.pid)
  @enif_args  ~w(e.ErlNifTerm e.ErlNifPid)
  @scalar_types @float_types ++ @int_types ++ @bool ++ @char ++ @beam_args ++ @enif_args
  @void         ["void"]
  @env          ~w(?*e.ErlNifEnv beam.env)
  @array_types  Enum.flat_map(@scalar_types, &["[]#{&1}", "[*c]#{&1}", "[_]#{&1}"])

  @valid_args  @scalar_types ++ @array_types ++ @env
  @valid_retvals @scalar_types ++ @array_types ++ @void

  @enforce_keys [:name, :arity]

  defstruct @enforce_keys ++ [
    doc:    nil,
    args:   [],
    retval: nil,
    opts:   [],
    test:   nil # only to be used for tests.  This is the string name
                # of the test which is going to be bound in.
  ]

  @type option ::
    {:long, boolean} |
    {:dirty, :cpu | :io}

  @type t :: %__MODULE__{
    name:   String.t,
    arity:  arity,
    doc:    iodata | nil,
    args:   [String.t],
    retval: String.t,
    opts:   [option],
    test:   String.t
  }

  @beam_envs ["beam.env", "?*e.ErlNifEnv"]

  # validate_arity/3: checks to make sure the arity of nif declaration matches the function
  @spec validate_arity([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return

  def validate_arity([env | rest], context, line) when env in @beam_envs do
    validate_arity(rest, context, line)
  end
  def validate_arity(rest, context = %{local: %{arity: arity}}, line) when length(rest) != arity do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line - 1,
      description: "nif declaration arity (#{arity}) doesn't match the expected function arity #{length(rest)}"
  end
  def validate_arity(_, _, _), do: :ok

  # validate_args/3 : raises if an invalid argument type is sent to to the function
  @spec validate_args([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_args([], _context, _line), do: :ok
  def validate_args([args | rest], context, line) when args in @valid_args do
    validate_args(rest, context, line)
  end
  def validate_args([invalid_type | _], context, line) do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line,
      description: "nif function #{context.local.name} demands an invalid argument type #{invalid_type}"
  end
  def validate_args(_, _, _), do: :ok

  @spec validate_retval([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_retval([retval | _], _context, _line) when retval in @valid_retvals, do: :ok
  def validate_retval([retval | _], context, line) do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line,
      description: "nif function #{context.local.name} returns an invalid type #{retval}"
  end

  def register_function_header([retval | args], context) do
    final_nif = %{context.local | retval: retval, args: Enum.reverse(args)}
    # long nifs require a resource
    resource = if context.local.opts[:long] do
      [%Resource{
        name: LongRunning.cache_ptr(context.local.name),
        cleanup: LongRunning.cache_cleanup(context.local.name)}]
    else
      []
    end
    %{context | global: resource ++ [final_nif | context.global]}
  end

end
