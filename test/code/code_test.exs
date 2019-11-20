defmodule ZiglerTest.CodeTest do
  use ExUnit.Case, async: true

  describe "when given a code multiline string the code module can" do
    test "return plain code with no nifs, and assign files and lines correctly" do

      code = """
      pub fn test_fn(val:i8) i8 {
        return val + 1;
      }
      """

      code_with_file_line = "// my_file.ex line: 15\n" <> code

      assert %Zigler.Code{
        code: new_code,
        file: "my_file.ex",
        line: 15,
        nifs: []
      } = Zigler.Code.from_string(code, "my_file.ex", 15)

      assert code_with_file_line == IO.iodata_to_binary(new_code)
    end

    test "identify more complex nif headers" do
      code = """
        /// nif: double_atom/1
        fn double_atom(env: beam.env, string: []u8) beam.atom {
        }
      """
      assert %Zigler.Code{nifs: [%Zigler.Nif{}]} = Zigler.Code.from_string(code, "my_file.ex", 15)
    end


    test "find a nif and correctly assign it, adding line numbers" do
      code = """
      /// nif: test_nif/1
      fn test_nif(val:i8) i8 {
        return 47;
      }
      """

      assert %Zigler.Code{
        code: new_code,
        nifs: [%Zigler.Nif{}]
      } = Zigler.Code.from_string(code, "my_file.ex", 15)

      assert """
      // my_file.ex line: 15
      /// nif: test_nif/1
      fn test_nif(val:i8) i8 {
        return 47;
      }
      """ == IO.iodata_to_binary(new_code)
    end

    test "do the correct thing with multiple functions" do
      code = """
      /// nif: test_nif_1/1
      fn test_nif_1(val: i8) i8 {
        return val + 1;
      }

      /// nif: test_nif_2/0
      fn test_nif_2() i8 {
        return 47;
      }
      """

      assert %Zigler.Code{
        code: new_code,
        nifs: [%Zigler.Nif{}, %Zigler.Nif{}]
      } = Zigler.Code.from_string(code, "my_file.ex", 12)

      assert """
      // my_file.ex line: 12
      /// nif: test_nif_1/1
      fn test_nif_1(val: i8) i8 {
        return val + 1;
      }

      /// nif: test_nif_2/0
      fn test_nif_2() i8 {
        return 47;
      }
      """ == IO.iodata_to_binary(new_code)
    end
  end

end
