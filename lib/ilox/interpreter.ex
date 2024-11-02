defmodule Ilox.Interpreter do
  @moduledoc """
  The expression interpreter for ilox.
  """
  alias Ilox.Env
  alias Ilox.Token

  def interpret(statements) when is_list(statements) do
    _env = interpret_statements(Env.new(), statements)
    :ok
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  def interpret_expr(expr) when is_tuple(expr) do
    {_env, value} = eval(Env.new(), expr)
    {:ok, stringify(value)}
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  def interpret_statements(env, []), do: env

  def interpret_statements(env, [current | statements]) do
    {env, _} = eval(env, current)
    interpret_statements(env, statements)
  end

  defp stringify(nil), do: "nil"

  defp stringify(value) when is_number(value) do
    value
    |> to_string()
    |> String.replace_suffix(".0", "")
  end

  defp stringify(value), do: to_string(value)

  defp eval(_env, {:literal_expr, value}) when value in [true, false, nil], do: value
  defp eval(_env, {:literal_expr, %{literal: literal}}), do: literal
  defp eval(env, {:group_expr, expr}), do: eval(env, expr)

  defp eval(env, {:unary_expr, %Token{} = operator, right}) do
    {env, right} = eval(env, right)

    # is_truthy is not required, as Elixir values are truthy the way Lox requires
    value =
      case operator.type do
        :minus when is_number(right) -> -right
        :minus -> raise Ilox.RuntimeError, token: operator, message: "Operand must be a number."
        :bang -> !right
        _ -> nil
      end

    {env, value}
  end

  defp eval(env, {:binary_expr, left, %Token{} = operator, right}) do
    {env, left} = eval(env, left)
    {env, right} = eval(env, right)

    types =
      cond do
        is_number(left) && is_number(right) -> :number
        is_binary(left) && is_binary(right) -> :binary
        true -> nil
      end

    {env, eval_operator(operator, left, right, types)}
  end

  defp eval(env, {:var_expr, name}), do: Env.get(env, name)

  defp eval(env, {:var_decl, name, initializer}) do
    {env, value} =
      if initializer do
        eval(env, initializer)
      else
        {env, nil}
      end

    Env.define(env, name, value)
  end

  defp eval(env, {:assign_expr, name, value}) do
    {env, value} = eval(env, value)
    Env.assign(env, name, value)
  end

  defp eval(env, {:expr_stmt, expr}) do
    {env, _} = eval(env, expr)
    {env, nil}
  end

  defp eval(env, {:print_stmt, expr}) do
    {env, value} = eval(env, expr)
    IO.puts(stringify(value))
    {env, nil}
  end

  # Comparison operators *must* be restricted to numbers, because all values in the BEAM
  # are comparables.
  defp eval_operator(%Token{type: :minus}, left, right, :number), do: left - right
  defp eval_operator(%Token{type: :slash}, left, right, :number), do: left / right
  defp eval_operator(%Token{type: :star}, left, right, :number), do: left * right
  defp eval_operator(%Token{type: :plus}, left, right, :number), do: left + right
  defp eval_operator(%Token{type: :plus}, left, right, :binary), do: left <> right
  defp eval_operator(%Token{type: :greater}, left, right, :number), do: left > right
  defp eval_operator(%Token{type: :greater_equal}, left, right, :number), do: left >= right
  defp eval_operator(%Token{type: :less}, left, right, :number), do: left < right
  defp eval_operator(%Token{type: :less_equal}, left, right, :number), do: left <= right
  defp eval_operator(%Token{type: :bang_equal}, left, right, _), do: left != right
  defp eval_operator(%Token{type: :equal_equal}, left, right, _), do: left == right

  # interpreter test: :minus, :slash, :star, :greater, :greater_equal, :less, and
  # :less_equal must raise errors on non-number in either left or right. :plus must raise
  # errors when both operands aren't both numbers or both strings.
  defp eval_operator(operator, _left, _right, nil),
    do: raise(Ilox.RuntimeError, invalid_operands!(operator))

  defp invalid_operands!(%Token{type: type} = token) when type == :plus,
    do: [token: token, message: "Operands must both two numbers or two strings."]

  defp invalid_operands!(token), do: [token: token, message: "Operands must be numbers."]
end
