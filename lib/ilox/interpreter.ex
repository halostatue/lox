defmodule Ilox.RuntimeError do
  defexception [:token, :message, where: nil]
  defdelegate message(e), to: Ilox.Errors, as: :format
end

defmodule Ilox.Interpreter do
  @moduledoc """
  The expression interpreter for ilox.
  """
  alias Ilox.Env
  alias Ilox.Errors
  alias Ilox.Parser
  alias Ilox.Token

  @spec run(Env.t(), String.t() | list(Token.t()) | Ilox.Parser.program()) ::
          :ok | {:error, atom(), String.t()}
  def run(env \\ Env.new(), input)

  def run(env, source) when is_binary(source) do
    case Parser.parse(source) do
      {:ok, statements} -> run(env, statements)
      {:error, type, errors} -> {:error, type, errors}
    end
  end

  def run(env, [head | _] = tokens) when is_struct(head, Token) do
    case Parser.parse(tokens) do
      {:ok, statements} -> run(env, statements)
      {:error, :parser, errors} -> {:error, :parser, errors}
    end
  end

  def run(env, [head | _] = statements) when is_tuple(head) do
    _env = handle_statements(env, statements)
    :ok
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  @spec eval_expr(Env.t(), String.t() | list(Token.t()) | Ilox.expr()) ::
          {:ok, binary()} | {:error, atom(), String.t()}
  def eval_expr(env \\ Env.new(), input)

  def eval_expr(env, source) when is_binary(source) do
    case Parser.parse_expr(source) do
      {:ok, expr} -> eval_expr(env, expr)
      {:error, type, errors} -> {:error, type, errors}
    end
  end

  def eval_expr(env, [head | _] = tokens) when is_struct(head, Token) do
    case Parser.parse_expr(tokens) do
      {:ok, expr} -> eval_expr(env, expr)
      {:error, type, errors} -> {:error, type, errors}
    end
  end

  def eval_expr(env, expr) when is_tuple(expr) do
    {_env, value} = handle_expression(env, expr)
    {:ok, inspectify(value)}
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  def handle_statements(env, []), do: env

  def handle_statements(env, [current | statements]) do
    {env, _} = handle_expression(env, current)
    handle_statements(env, statements)
  end

  defp inspectify(nil), do: "nil"
  defp inspectify(value) when is_binary(value), do: ~s("#{value}")
  defp inspectify(value), do: stringify(value)

  defp stringify(nil), do: ""

  defp stringify(value) when is_float(value) do
    value
    |> :erlang.float_to_binary([:short, :compact])
    |> String.replace_suffix(".0", "")
  end

  defp stringify(value), do: to_string(value)

  defp handle_expression(env, nil), do: {env, nil}

  defp handle_expression(env, {:literal_expr, value}) when value in [true, false, nil],
    do: {env, value}

  defp handle_expression(env, {:literal_expr, %{literal: literal}}), do: {env, literal}
  defp handle_expression(env, {:group_expr, expr}), do: handle_expression(env, expr)

  defp handle_expression(env, {:unary_expr, %Token{} = operator, right}) do
    {env, right} = handle_expression(env, right)

    # is_truthy is not required, as Elixir values are truthy the way Lox requires
    value =
      case operator.type do
        :minus when is_number(right) ->
          -right

        :minus ->
          raise Ilox.RuntimeError,
            token: operator,
            message: "Operand must be a number.",
            where: Errors.where(operator)

        :bang ->
          !right

        _ ->
          nil
      end

    {env, value}
  end

  defp handle_expression(env, {:binary_expr, left, %Token{} = operator, right}) do
    {env, left} = handle_expression(env, left)
    {env, right} = handle_expression(env, right)

    types =
      cond do
        is_number(left) && is_number(right) -> :number
        is_binary(left) && is_binary(right) -> :binary
        true -> nil
      end

    {env, handle_operator(operator, left, right, types)}
  end

  defp handle_expression(env, {:var_expr, name}), do: Env.get(env, name)

  defp handle_expression(env, {:var_decl, name, initializer}) do
    {env, value} =
      if initializer do
        handle_expression(env, initializer)
      else
        {env, nil}
      end

    Env.define(env, name, value)
  end

  defp handle_expression(env, {:assign_expr, name, value}) do
    {env, value} = handle_expression(env, value)
    Env.assign(env, name, value)
  end

  defp handle_expression(env, {:expr_stmt, expr}) do
    {env, _} = handle_expression(env, expr)
    {env, nil}
  end

  defp handle_expression(env, {:print_stmt, expr}) do
    {env, value} = handle_expression(env, expr)
    env.print.(stringify(value))
    {env, nil}
  end

  defp handle_expression(env, {:block, statements}) do
    %{enclosing: env} = handle_statements(Env.new(env), statements)
    {env, nil}
  end

  defp handle_expression(env, {:if_stmt, condition, then_branch, else_branch}) do
    {env, value} = handle_expression(env, condition)
    {env, _value} = handle_expression(env, if(value, do: then_branch, else: else_branch))
    {env, nil}
  end

  # Comparison operators *must* be restricted to numbers, because all values in the BEAM
  # are comparables.
  defp handle_operator(%Token{type: :minus}, left, right, :number), do: left - right
  defp handle_operator(%Token{type: :slash}, left, right, :number), do: left / right
  defp handle_operator(%Token{type: :star}, left, right, :number), do: left * right
  defp handle_operator(%Token{type: :plus}, left, right, :number), do: left + right
  defp handle_operator(%Token{type: :plus}, left, right, :binary), do: left <> right
  defp handle_operator(%Token{type: :greater}, left, right, :number), do: left > right
  defp handle_operator(%Token{type: :greater_equal}, left, right, :number), do: left >= right
  defp handle_operator(%Token{type: :less}, left, right, :number), do: left < right
  defp handle_operator(%Token{type: :less_equal}, left, right, :number), do: left <= right
  defp handle_operator(%Token{type: :bang_equal}, left, right, _), do: left != right
  defp handle_operator(%Token{type: :equal_equal}, left, right, _), do: left == right

  # interpreter test: :minus, :slash, :star, :greater, :greater_equal, :less, and
  # :less_equal must raise errors on non-number in either left or right. :plus must raise
  # errors when both operands aren't both numbers or both strings.
  defp handle_operator(operator, _left, _right, nil),
    do: raise(Ilox.RuntimeError, invalid_operands!(operator))

  defp invalid_operands!(%Token{type: type} = token) when type == :plus,
    do: [
      token: token,
      message: "Operands must be two numbers or two strings.",
      where: Errors.where(token)
    ]

  defp invalid_operands!(token),
    do: [token: token, message: "Operands must be numbers.", where: Errors.where(token)]
end
