
# clear any zigler compilation artifacts.
if File.dir?("/tmp/zigler") do
  File.rm_rf!("/tmp/zigler")
end

ExUnit.start()
