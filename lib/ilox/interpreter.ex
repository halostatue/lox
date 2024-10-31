defmodule Ilox.Interpreter do
  @moduledoc """
  The expression interpreter for ilox.
  """
  alias Ilox.Token

  def interpret(expr) do
    result = stringify(eval(expr))

    {:ok, result}
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  defp stringify(nil), do: "nil"

  defp stringify(value) when is_number(value) do
    value
    |> to_string()
    |> String.replace_suffix(".0", "")
  end

  defp stringify(value), do: to_string(value)

  defp eval({:literal, nil}), do: nil
  defp eval({:literal, %{literal: literal}}), do: literal
  defp eval({:group, expr}), do: eval(expr)

  defp eval({:unary, %Token{} = operator, right}) do
    right = eval(right)

    # is_truthy is not required, as Elixir values are truthy the way Lox requires
    case operator.type do
      :minus when is_number(right) -> -right
      :minus -> raise Ilox.RuntimeError, token: operator, message: "Operand must be a number."
      :bang -> !right
      _ -> nil
    end
  end

  defp eval({:binary, left, %Token{} = operator, right}) do
    left = eval(left)
    right = eval(right)

    numbers? = is_number(left) && is_number(right)
    strings? = is_binary(left) && is_binary(right)

    # Comparison operators *must* be restricted to numbers, because all values in the BEAM
    # are comparables.
    case operator.type do
      :minus when numbers? -> left - right
      :minus -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :slash when numbers? -> left / right
      :slash -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :star when numbers? -> left * right
      :star -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :plus when numbers? -> left + right
      :plus when strings? -> left <> right
      :plus -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :greater when numbers? -> left > right
      :greater -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :greater_equal when numbers? -> left >= right
      :greater_equal -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :less when numbers? -> left < right
      :less -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :less_equal when numbers? -> left <= right
      :less_equal -> raise Ilox.RuntimeError, invalid_operands!(operator)
      :bang_equal -> left != right
      :equal_equal -> left == right
    end
  end

  defp invalid_operands!(%Token{type: type} = token) when type == :plus,
    do: [token: token, message: "Operands must both two numbers or two strings."]

  defp invalid_operands!(token), do: [token: token, message: "Operands must be numbers."]
end
