defmodule Zig.C do
  @moduledoc false

  # module creates a struct that defines the options for
  # c-interoperability with zigler

  defstruct include_dirs: [],
            library_dirs: [],
            src: [],
            link_lib: [],
            link_libcpp: false

  @type t :: %__MODULE__{
          include_dirs: [String.t() | {:system, String.t()}],
          library_dirs: [String.t() | {:system, String.t()}],
          link_lib: [String.t() | {:system, String.t()}],
          link_libcpp: boolean,
          src: [{String.t(), [String.t()]}]
        }

  @type dirpath :: String.t() | {:priv, String.t()} | {:system, String.t()}
  @type dirspec :: dirpath() | [dirpath()]
  @type srcspec :: String.t() | {String.t(), [String.t()]}

  @type opts :: [
          include_dirs: dirspec(),
          library_dirs: dirspec(),
          link_lib: dirspec(),
          link_libcpp: boolean,
          src: srcspec()
        ]

  @valid_keys ~w[include_dirs library_dirs src link_lib link_libcpp]a

  def new(opts, module_opts) do
    module_dir =
      cond do
        dir = module_opts[:dir] -> dir
        file = module_opts[:file] -> Path.dirname(file)
      end

    otp_app = Keyword.fetch!(module_opts, :otp_app)

    Enum.each(opts, fn
      {key, _} ->
        if key not in @valid_keys do
          raise CompileError,
            file: module_opts[:file],
            line: module_opts[:line],
            description: "`c` option had invalid key `#{key}`"
        end
    end)

    __MODULE__
    |> struct!(
      include_dirs: normalize_filelist(opts, :include_dirs, module_dir, otp_app),
      library_dirs: normalize_filelist(opts, :library_dirs, module_dir, otp_app),
      link_lib: normalize_filelist(opts, :link_lib, module_dir, otp_app),
      link_libcpp: Keyword.get(opts, :link_libcpp, false),
      src: normalized_srclist(opts, module_dir, otp_app)
    )
    |> validate(module_opts)
  catch
    {:dirspec_error, key, value} ->
      raise CompileError,
        file: module_opts[:file],
        line: module_opts[:line],
        description:
          "`c` option `#{key}` must be a string, `{:priv, string}`, `{:system, string}`, or a list of those, got: `#{inspect(value)}`"

    {:src_error, value} ->
      raise CompileError,
        file: module_opts[:file],
        line: module_opts[:line],
        description:
          "`c` option `src` must be a string, `{string, [string]}`, `{:priv, string}`, `{:priv, string, [string]}`, `{:system, string}`, `{:system, string, [string]}`, or a list of those, got: `#{inspect(value)}`"
  end

  defp normalize_filelist(opts, key, module_dir, otp_app) do
    opts
    |> Keyword.get(key)
    |> List.wrap()
    |> Enum.map(&solve_relative(&1, key, module_dir, otp_app))
  end

  defp normalized_srclist(opts, module_dir, otp_app) do
    opts
    |> Keyword.get(:src)
    |> List.wrap()
    |> Enum.flat_map(&normalize_src(&1, module_dir, otp_app))
  end

  defp solve_relative({:system, file} = system, key, _, _) do
    if not is_binary(file), do: throw({:dirspec_error, key, file})
    system
  end

  defp solve_relative({:priv, file}, key, _, otp_app) do
    if not is_binary(file), do: throw({:dirspec_error, key, file})
    Path.expand(file, :code.priv_dir(otp_app))
  end

  defp solve_relative(file, key, module_dir, _) do
    if not is_binary(file), do: throw({:dirspec_error, key, file})
    Path.expand(file, module_dir)
  end

  defp normalize_src(file, module_dir, otp_app) when is_binary(file) do
    maybe_with_wildcard(file, module_dir, [], otp_app)
  end

  @tags ~w[priv system]a

  defp normalize_src({tag, file}, module_dir, otp_app) when tag in @tags and is_binary(file) do
    maybe_with_wildcard({tag, file}, module_dir, [], otp_app)
  end

  defp normalize_src({tag, file, opts}, module_dir, otp_app)
       when tag in @tags and is_binary(file) do
    maybe_with_wildcard({tag, file}, module_dir, opts, otp_app)
  end

  defp normalize_src({file, opts}, module_dir, otp_app) do
    maybe_with_wildcard(file, module_dir, opts, otp_app)
  end

  defp normalize_src(other, _, _), do: throw({:src_error, other})

  defp maybe_with_wildcard(file, module_dir, opts, otp_app) do
    if String.ends_with?(file, "/*") do
      file
      |> Path.dirname()
      |> solve_relative(:src, module_dir, otp_app)
      |> then(fn wildcard_dir ->
        wildcard_dir
        |> File.ls!()
        |> Enum.map(&{Path.join(wildcard_dir, &1), opts})
      end)
    else
      [{solve_relative(file, :src, module_dir, otp_app), opts}]
    end
  end

  defp validate(module, opts) do
    assert_directory_list(module, :include_dirs, opts)
    assert_directory_list(module, :library_dirs, opts)
    assert_directory_list(module, :link_lib, opts)

    if not is_boolean(module.link_libcpp) do
      raise CompileError,
        file: opts[:file],
        line: opts[:line],
        description:
          "`c` option `link_libcpp` must be a boolean, got: `#{inspect(module.link_libcpp)}`"
    end

    module
  end

  defp assert_directory_list(module, key, opts) do
    val = Map.fetch!(module, key)

    if not is_list(val) or Enum.any?(val, &invalid_dirspec/1) do
      raise CompileError,
        file: opts[:file],
        line: opts[:line],
        description: "`c` option #{key} must be a string list"
    end
  end

  defp invalid_dirspec({:system, binary}) when is_binary(binary), do: false
  defp invalid_dirspec(binary) when is_binary(binary), do: false
  defp invalid_dirspec(_), do: true
end
