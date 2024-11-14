defmodule Ilox.Interpreter do
  @moduledoc """
  The expression interpreter for ilox.
  """

  alias Ilox.Callable
  alias Ilox.Env
  alias Ilox.Errors
  alias Ilox.Parser
  alias Ilox.Resolver
  alias Ilox.Token

  import Ilox.Guards

  @doc """
  Runs an ilox script.
  """
  @spec run(Env.t() | nil, String.t() | list(Token.t()) | Ilox.Parser.program()) ::
          :ok | {:error, atom(), String.t()}
  def run(env \\ nil, input)

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
    env = create_env(env)

    with {:ok, env} <- Resolver.resolve(env, statements),
         %Env{} <- exec_statements(env, statements) do
      :ok
    end
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  @spec eval_expr(Env.t() | nil, String.t() | list(Token.t()) | Ilox.expr()) ::
          {:ok, binary()} | {:error, atom(), String.t()}
  def eval_expr(env \\ nil, input)

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
    {_env, value} =
      env
      |> create_env()
      |> eval_expression(expr)

    {:ok, inspect_expression_value(value)}
  rescue
    e in Ilox.RuntimeError ->
      {:error, :runtime, Exception.message(e)}
  end

  @doc false
  def eval_function_body(env, {:block, _body} = block) do
    env = exec_statement(env, block)
    {env, nil}
  end

  @spec exec_statements(Env.t(), list(Parser.declaration() | Parser.statement())) :: Env.t()
  defp exec_statements(env, []), do: env

  defp exec_statements(env, [statement | statements]) do
    env
    |> exec_statement(statement)
    |> exec_statements(statements)
  end

  @spec exec_statement(Env.t(), Parser.declaration()) :: Env.t()
  defp exec_statement(env, {:return_stmt, _, value}) do
    {env, value} = if value, do: eval_expression(env, value), else: {env, nil}
    throw({env, value})
  end

  defp exec_statement(env, {:var_decl, name, initializer}) do
    {env, value} =
      if initializer do
        eval_expression(env, initializer)
      else
        {env, nil}
      end

    {env, _value} = Env.define(env, name, value)
    env
  end

  defp exec_statement(env, {:expr_stmt, expr}) do
    {env, _} = eval_expression(env, expr)
    env
  end

  defp exec_statement(env, {:print_stmt, expr}) do
    {env, value} = eval_expression(env, expr)
    env.print.(stringify_expression_value(value))
    env
  end

  defp exec_statement(env, {:block, statements}) do
    {env, scope_id} = Env.push_scope(env)

    try do
      env = exec_statements(env, statements)
      Env.pop_scope(env, scope_id)
    catch
      {env, value} ->
        throw({Env.pop_scope(env, scope_id), value})
    end
  end

  defp exec_statement(env, {:if_stmt, condition, then_branch, else_branch}) do
    {env, condition} = eval_expression(env, condition)

    case condition do
      true -> exec_statement(env, then_branch)
      false when is_nil(else_branch) -> env
      false -> exec_statement(env, else_branch)
    end
  end

  defp exec_statement(env, {:while_stmt, condition, body} = while) do
    {env, condition} = eval_expression(env, condition)

    if condition do
      env
      |> exec_statement(body)
      |> exec_statement(while)
    else
      env
    end
  end

  defp exec_statement(env, {:function, name, _params, arity, _body} = fun) do
    fun =
      Callable.new(
        arity: arity,
        decl: fun,
        to_string: "<fn #{name.lexeme}>",
        closure_id: Env.current_scope(env)
      )

    {env, _} = Env.define(env, name, fun)
    env
  end

  # defp eval_expression(env, nil), do: {env, nil}

  defp eval_expression(env, {:group_expr, expr}), do: eval_expression(env, expr)

  defp eval_expression(env, {:literal_expr, value}) when value in [true, false, nil],
    do: {env, value}

  defp eval_expression(env, {:literal_expr, %{literal: literal}}), do: {env, literal}

  defp eval_expression(env, {:unary_expr, %Token{} = operator, right}) do
    {env, right} = eval_expression(env, right)

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
          raise "Invalid unary expression: #{inspect(operator.type)}(#{inspect(right)})."
      end

    {env, value}
  end

  defp eval_expression(env, {:binary_expr, left, %Token{} = operator, right}) do
    {env, left} = eval_expression(env, left)
    {env, right} = eval_expression(env, right)

    types =
      cond do
        is_number(left) && is_number(right) -> :number
        is_binary(left) && is_binary(right) -> :binary
        true -> nil
      end

    {env, eval_operator(operator, left, right, types)}
  end

  defp eval_expression(env, {:var_expr, name} = expr), do: {env, Env.get(env, expr, name)}

  defp eval_expression(env, {:assign_expr, name, value}) do
    {env, value} = eval_expression(env, value)
    Env.assign(env, name, value)
  end

  defp eval_expression(env, {:logical_expr, left, %Token{type: operator}, right})
       when operator in [:or, :and] do
    {env, left} = eval_expression(env, left)
    truthy? = !!left

    case {operator, truthy?} do
      {:or, true} -> {env, left}
      {:and, false} -> {env, left}
      _ -> eval_expression(env, right)
    end
  end

  defp eval_expression(env, {:call, callee, arguments, argc, closing}) do
    {env, callee} = eval_expression(env, callee)

    if !is_callable(callee) do
      raise Ilox.RuntimeError, token: closing, message: "Can only call functions and classes."
    end

    if callee.arity != argc do
      raise Ilox.RuntimeError,
        token: closing,
        message: "Expected #{callee.arity} arguments but got #{argc}."
    end

    {env, arguments} =
      Enum.reduce(arguments, {env, []}, fn arg, {env, args} ->
        {env, arg} = eval_expression(env, arg)
        {env, [arg | args]}
      end)

    Callable.call(env, callee, Enum.reverse(arguments))
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
    do: [
      token: token,
      message: "Operands must be two numbers or two strings.",
      where: Errors.where(token)
    ]

  defp invalid_operands!(token),
    do: [token: token, message: "Operands must be numbers.", where: Errors.where(token)]

  defp create_env(%Env{} = env), do: env
  defp create_env(nil), do: Env.new()

  defp inspect_expression_value(nil), do: "nil"
  defp inspect_expression_value(value) when is_binary(value), do: ~s("#{value}")
  defp inspect_expression_value(value), do: stringify_expression_value(value)

  defp stringify_expression_value(nil), do: ""

  defp stringify_expression_value(value) when is_float(value) do
    value
    |> :erlang.float_to_binary([:short, :compact])
    |> String.replace_suffix(".0", "")
  end

  defp stringify_expression_value(value), do: to_string(value)
end
