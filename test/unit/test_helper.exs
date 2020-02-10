
# clear any zigler compilation artifacts.
if File.dir?("/tmp/zigler_compiler") do
  File.rm_rf!("/tmp/zigler_compiler")
end

ExUnit.start()
