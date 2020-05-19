Logger.configure(level: :warn)

File.rm_rf!("priv/nifs")

ExUnit.start()
