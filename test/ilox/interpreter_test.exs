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

  describe "parse/1: block statements" do
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

  defp run(source) do
    case Interpreter.run(env(), source) do
      :ok ->
        {:ok, output: Enum.reverse(Process.get(:"$ilox$output", []))}

      {:error, type, errors} ->
        {:error, type, errors, Enum.reverse(Process.get(:"$ilox$output", []))}
    end
  end

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
