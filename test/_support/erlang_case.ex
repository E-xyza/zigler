defmodule ZiglerTest.ErlangCase do
  defmacro initialize do
    quote do
      setup_all do
        {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
        Code.ensure_loaded(mod)
      end
    end
  end
end
