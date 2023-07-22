# Collection Datatypes

Collection datatype parameters and returns have more specific detail.

## Returning array-like datatypes

```elixir
~Z"""
pub fn return_array(input: f32) [3]f32 {
  var result: [3]f32 = undefined;
  result[0] = input;
  result[1] = input + 1;
  result[2] = input + 2;
  return result;
}
"""

test "returning an array" do
  assert [47.0, 48.0, 49.0] == return_array(47.0)
end
```