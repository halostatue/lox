defmodule Ilox.InterpreterTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ilox.Env
  alias Ilox.Interpreter

  import Ilox.StreamData

  describe "run/2: expression statements" do
    property "value == value;" do
      check all(value <- literal()) do
        assert {:ok, output: []} = run("#{inspect(value)} == #{inspect(value)};")
      end
    end

    property "left < right;" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, output: []} = run("#{inspect(left)} < (#{inspect(right)});")
      end
    end

    for operator <- ["<", "<=", ">", ">=", "*", "/", "+", "-"] do
      test "non_number #{operator} non_number; // error" do
        message =
          case unquote(operator) do
            "+" -> "[line 1] Error at '+': Operands must be two numbers or two strings."
            _ -> "[line 1] Error at '#{unquote(operator)}': Operands must be numbers."
          end

        assert {:error, :runtime, ^message, _} = run("nil #{unquote(operator)} true;")
      end
    end

    for input <- [true, false, nil, "string"] do
      test "-#{inspect(input)}; // error" do
        assert {:error, :runtime, "[line 1] Error at '-': Operand must be a number.", _} =
                 run("-#{inspect(unquote(input))};")
      end
    end
  end

  describe "run/2: print statements" do
    test "print 123;" do
      assert {:ok, output: ["123"]} = run("print 123;")
    end

    test "print \"hello\";" do
      assert {:ok, output: ["hello"]} = run("print \"hello\";")
    end
  end

  describe "run/1: block statements" do
    test "one block" do
      assert {:ok, output: ~w[1 2 3 3]} =
               run("""
               var value = 1;
               print value;
               { value = 2; print value; value = 3; print value; }
               print value;
               """)
    end

    test "two blocks" do
      assert {:ok, output: ~w[1 2 3 3]} =
               run("""
               var value = 1;
               print value;
               { value = 2; print value; }
               { value = 3; print value; }
               print value;
               """)
    end

    test "nested blocks 1" do
      assert {:error, :runtime, "[line 13] Error: Undefined variable 'value'.", ~w[1 2 1]} =
               run("""
               {
                 var value = 1;
                 print value;

                 {
                   var value = 2;
                   print value;
                 }

                 print value;
               }

               print value;
               """)
    end

    test "scope" do
      assert {:ok,
              output: [
                "inner a",
                "outer b",
                "global c",
                "outer a",
                "outer b",
                "global c",
                "global a",
                "global b",
                "global c"
              ]} =
               run("""
               var a = "global a";
               var b = "global b";
               var c = "global c";
               {
                 var a = "outer a";
                 var b = "outer b";
                 {
                   var a = "inner a";
                   print a;
                   print b;
                   print c;
                 }
                 print a;
                 print b;
                 print c;
               }
               print a;
               print b;
               print c;
               """)
    end
  end

  describe "run/1: if statements" do
    test "if" do
      source = """
      if (:cond)
        print 2;
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: []} = run(source, cond: "false")
    end

    test "if block" do
      source = """
      if (:cond) {
        print 2;
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: []} = run(source, cond: "false")
    end

    test "if/else" do
      source = """
      if (:cond)
        print 2;
      else
        print 3;
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: ["3"]} = run(source, cond: "false")
    end

    test "if/else (block)" do
      source = """
      if (:cond) {
        print 2;
      } else {
        print 3;
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: ["3"]} = run(source, cond: "false")
    end

    test "if if/else" do
      # This is pathological. Do not do this.
      source = """
      if (:cond1) if (:cond2) print 2; else print 3;
      """

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "false")
    end

    test "if if/else block" do
      source = """
      if (:cond1) {
        if (:cond2) {
          print 2;
        } else {
          print 3;
        }
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "false")
    end

    test "if if/else else" do
      # This is pathological. Do not do this.
      source = """
      if (:cond1)
        if (:cond2)
          print 2;
        else
          print 3;
      else
        print 4;
      """

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "false")
    end

    test "if if/else else block" do
      source = """
      if (:cond1) {
        if (:cond2) {
          print 2;
        } else {
          print 3;
        }
      } else {
        print 4;
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "false")
    end
  end

  describe "run/1: while statement" do
    test "while statement" do
      source = """
      var a = 3 ;
      while (a > 1) {
        print a;
        a = a - 1;
       }
      """

      assert {:ok, output: ["3", "2"]} = run(source)
    end
  end

  describe "run/1: for statement" do
    test "for (var a = 2; a > 0; a = a - 1) print a;" do
      assert {:ok, output: ["2", "1"]} = run("for (var a = 2; a > 0; a = a - 1) print a;")
    end

    test "for (var a = 3; a > 0; a = a - 1) { print a; }" do
      assert {:ok, output: ["3", "2", "1"]} =
               run("for (var a = 3; a > 0; a = a - 1) { print a; }")
    end
  end

  describe "run/2: logical operators" do
    test "print \"hi\" or 2;" do
      assert {:ok, output: ["hi"]} = run("print \"hi\" or 2;")
    end

    test "print nil or \"yes\";" do
      assert {:ok, output: ["yes"]} = run("print nil or \"yes\";")
    end
  end

  defp run(source, replacements \\ []) do
    case Interpreter.run(env(), __replace(source, replacements)) do
      :ok ->
        {:ok, output: Enum.reverse(Process.get(:"$ilox$output", []))}

      {:error, type, errors} ->
        {:error, type, errors, Enum.reverse(Process.get(:"$ilox$output", []))}
    end
  after
    Process.delete(:"$ilox$output")
  end

  defp __replace(source, []), do: source

  defp __replace(source, [{key, value} | rest]),
    do: __replace(String.replace(source, inspect(key), value), rest)

  defp env, do: %{Env.new() | print: &print/1}

  defp print(message) do
    queue = Process.get(:"$ilox$output", [])
    queue = [message | queue]
    Process.put(:"$ilox$output", queue)
  end

  defp number_to_string(value) do
    value
    |> :erlang.float_to_binary([:short, :compact])
    |> String.replace_suffix(".0", "")
  end
end
