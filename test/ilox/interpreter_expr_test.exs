defmodule Ilox.InterpreterExprTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ilox.Env
  alias Ilox.Interpreter

  import Ilox.StreamData

  describe "eval_expr/2: equality" do
    property "value == value -> true" do
      check all(value <- literal()) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(value)} == #{inspect(value)}")
      end
    end

    property "value != value -> false" do
      check all(value <- literal()) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("#{inspect(value)} != #{inspect(value)}")
      end
    end

    property "left == right -> false" do
      check all(left <- literal(), right <- filter(literal(), neq(left))) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("#{inspect(left)} == (#{inspect(right)})")
      end
    end

    property "left != right -> true" do
      check all(left <- literal(), right <- filter(literal(), neq(left))) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(left)} != (#{inspect(right)})")
      end
    end
  end

  describe "eval_expr/2: comparison" do
    property "left < right -> true" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(left)} < (#{inspect(right)})")
      end
    end

    property "right < left -> false" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("(#{inspect(right)}) < #{inspect(left)}")
      end
    end

    property "left <= right -> true" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(left)} <= (#{inspect(right)})")
      end
    end

    property "right <= left -> false" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("(#{inspect(right)}) <= #{inspect(left)}")
      end
    end

    property "left > right -> true" do
      check all(left <- positive_number(), right <- negative_number()) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(left)} > (#{inspect(right)})")
      end
    end

    property "right > left -> false" do
      check all(left <- positive_number(), right <- negative_number()) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("(#{inspect(right)}) > #{inspect(left)}")
      end
    end

    property "left >= right -> true" do
      check all(left <- positive_number(), right <- negative_number()) do
        assert {:ok, [expr: "true", output: []]} =
                 eval_expr("#{inspect(left)} >= (#{inspect(right)})")
      end
    end

    property "right >= left -> false" do
      check all(left <- positive_number(), right <- negative_number()) do
        assert {:ok, [expr: "false", output: []]} =
                 eval_expr("(#{inspect(right)}) >= #{inspect(left)}")
      end
    end

    for operator <- ["<", "<=", ">", ">="] do
      test "non_number #{operator} non_number -> error" do
        message = "[line 1] Error at '#{unquote(operator)}': Operands must be numbers."

        assert {:error, :runtime, ^message} =
                 eval_expr("nil #{unquote(operator)} true")
      end
    end
  end

  describe "eval_expr/2: binary operations (*/+-)" do
    for operator <- ["*", "/", "+", "-"] do
      test "non_number #{operator} non_number -> error" do
        message =
          case unquote(operator) do
            "+" -> "[line 1] Error at '+': Operands must be two numbers or two strings."
            _ -> "[line 1] Error at '#{unquote(operator)}': Operands must be numbers."
          end

        assert {:error, :runtime, ^message} =
                 eval_expr("nil #{unquote(operator)} true")
      end
    end

    property "left_string + right_string -> concatenation" do
      check all(left <- literal(:string), right <- literal(:string)) do
        expr = inspect(left <> right)

        assert {:ok, [expr: ^expr, output: []]} =
                 eval_expr("#{inspect(left)} + (#{inspect(right)})")
      end
    end

    property "left_number + right_number -> addition" do
      check all(left <- nonzero_number(), right <- nonzero_number()) do
        left = left / 1
        right = right / 1
        expr = number_to_string(left + right)

        assert {:ok, [expr: ^expr, output: []]} =
                 eval_expr("#{inspect(left)} + (#{inspect(right)})")
      end
    end

    property "left_number - right_number -> subtraction" do
      check all(left <- nonzero_number(), right <- nonzero_number()) do
        left = left / 1
        right = right / 1
        expr = number_to_string(left - right)

        assert {:ok, [expr: ^expr, output: []]} =
                 eval_expr("#{inspect(left)} - (#{inspect(right)})")
      end
    end

    property "left_number * right_number -> product" do
      check all(left <- nonzero_number(), right <- nonzero_number()) do
        left = left / 1
        right = right / 1
        expr = number_to_string(left * right)

        assert {:ok, [expr: ^expr, output: []]} =
                 eval_expr("#{inspect(left)} * (#{inspect(right)})")
      end
    end

    property "left_number / right_number -> division" do
      check all(left <- nonzero_number(), right <- nonzero_number()) do
        left = left / 1
        right = right / 1
        expr = number_to_string(left / right)

        assert {:ok, [expr: ^expr, output: []]} =
                 eval_expr("#{inspect(left)} / (#{inspect(right)})")
      end
    end
  end

  describe "eval_expr/2: unary operations (!-)" do
    # We do not need to run tests on literal arithmetic negation (-), as that has been
    # tested extensively through properties above. Here we are doing negative testing on
    # non-numeric unary arithmetic negation (-), e.g., -true.
    for input <- [true, false, nil, "string"] do
      test "-#{inspect(input)} -> error" do
        assert {:error, :runtime, "[line 1] Error at '-': Operand must be a number."} =
                 eval_expr("-#{inspect(unquote(input))}")
      end
    end

    property "!number -> false" do
      check all(value <- number()) do
        assert {:ok, [expr: "false", output: []]} = eval_expr("!#{value}")
      end
    end

    property "!string -> false" do
      check all(value <- literal(:string)) do
        assert {:ok, [expr: "false", output: []]} = eval_expr("!#{inspect(value)}")
      end
    end

    test "!true -> false" do
      assert {:ok, [expr: "false", output: []]} = eval_expr("!true")
    end

    test "!false -> true" do
      assert {:ok, [expr: "true", output: []]} = eval_expr("!false")
    end

    test "!nil -> true" do
      assert {:ok, [expr: "true", output: []]} = eval_expr("!nil")
    end

    property "!!number -> true" do
      check all(value <- number()) do
        assert {:ok, [expr: "true", output: []]} = eval_expr("!!#{value}")
      end
    end

    property "!!string -> true" do
      check all(value <- literal(:string)) do
        assert {:ok, [expr: "true", output: []]} = eval_expr("!!#{inspect(value)}")
      end
    end

    test "!!true -> true" do
      assert {:ok, [expr: "true", output: []]} = eval_expr("!!true")
    end

    test "!!false -> false" do
      assert {:ok, [expr: "false", output: []]} = eval_expr("!!false")
    end

    test "!!nil -> false" do
      assert {:ok, [expr: "false", output: []]} = eval_expr("!!nil")
    end
  end

  defp eval_expr(source) do
    case Interpreter.eval_expr(env(), source) do
      {:ok, expr} -> {:ok, expr: expr, output: Process.get(:"$ilox$output", [])}
      {:error, type, errors} -> {:error, type, errors}
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
