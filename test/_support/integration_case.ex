defmodule ZiglerTest.IntegrationCase do
  defmacro __using__(opts) do
    opts =
      if Application.get_env(:zigler, :id_integration) do
        Keyword.put(opts, :async, false)
      else
        opts
      end

    announce_test =
      if Application.get_env(:zigler, :id_integration) do
        quote do
          setup params do
            this = self()

            lock =
              spawn(fn ->
                :global.set_lock({:ziglertest, self()})
                send(this, :ok)

                receive do
                  :done -> :ok
                end

                Process.sleep(100)
                IO.puts("✨ Finished #{params.test} in #{inspect(params.module)}")
              end)

            receive do
              :ok -> :ok
            end

            IO.puts("✨ Running #{params.test} in #{inspect(params.module)}")

            on_exit(fn ->
              send(lock, :done)
            end)
          end
        end
      end

    quote do
      use ExUnit.Case, unquote(opts)

      unquote(announce_test)
    end
  end
end
