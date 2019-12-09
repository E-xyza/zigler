defmodule Zigler.Doc do

  @moduledoc """
  Translates the docstrings from your Zig code into Elixir documentation.

  For instructions on how to incorporate this into an Elixir project,
  consult `Mix.Tasks.ZigDoc`

  ## Documentation forms

  Currently, Zigler recognizes four types of code segments which should be
  documented.

  - functions
  - types
  - values
  - errors

  ### Functions

  functions have the following signature:

  `pub fn <identifier>(<parameters>) <type> {`

  and may have the property of being `comptime` which is due to either the
  function being itself a `comptime` function or it having a `comptime`
  parameter.

  **NB** only `pub` functions are documented in Zigler, following the Elixir
  philosophy that only public functions should be documented.

  ### Types

  types have the following signature:

  `pub const <identifier>=<value>;`

  ### Values

  values can have one of the following signatures:

  - `pub const <identifier>=<value>;`
  - `pub var <identifier>=<value>;`

  Note that the constant value form is indistinguishable from the type form
  without doing a full parse and evaluation of the Zig code.  In order to
  avoid doing this, to disambiguate between the two, you must prepend constant
  *value* docststrings with a `!value` token.

  #### Example

  ```
  /// !value
  ///
  /// a constant representing the value 47.
  pub const fortyseven = 47;
  ```

  ### Errors

  errors appear inside special [error struct](https://ziglang.org/documentation/0.5.0/#Errors)
  and should be documented at the per-value level:

  #### Example

  ```
  /// docstring for this error struct (if desired)
  pub const my_error = error {
    /// docstring for ErrorEnum1
    ErrorEnum1,

    /// docstring for ErrorEnum2
    ErrorEnum2
  }
  ```

  """

  @doc false
  @spec generate_docs(String.t(), String.t(), Keyword.t()) :: atom
  def generate_docs(project, vsn, options)
      when is_binary(project) and is_binary(vsn) and is_list(options) do
    ExDoc.generate_docs(project, vsn, [retriever: Zigler.Doc.Retriever] ++ options)
  end
end
