Logger.configure(level: :warning)

# uncomment the following line to run integration tests one at a time.
# this is good for identifying segfaults.
# Application.put_env(:zigler, :id_integration, true)

ZiglerTest.MakeGuides.go()

ExUnit.start()
