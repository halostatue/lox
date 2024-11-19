defmodule Ilox.Interpreter do
  @moduledoc """
  The expression interpreter for ilox.
  """

  alias Ilox.Callable
  alias Ilox.Class
  alias Ilox.Env
  alias Ilox.Errors
  alias Ilox.Function
  alias Ilox.Instance
  alias Ilox.Parser
  alias Ilox.Resolver
  alias Ilox.Token

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

  @spec eval_expr(Env.t() | nil, String.t() | list(Token.t()) | Lox.expr()) ::
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

    {:ok, inspect_expression_value(env, value)}
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
    env.print.(stringify_expression_value(env, value))
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

    case !!condition do
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

  defp exec_statement(env, {:fun_decl, name, _params, arity, _body} = fun) do
    fun =
      Function.new(
        name: name.lexeme,
        arity: arity,
        decl: fun,
        closure_id: Env.current_scope(env)
      )

    {env, _} = Env.define(env, name, fun)
    env
  end

  defp exec_statement(env, {:class_decl, name, superclass, methods}) do
    {env, superclass} =
      case superclass do
        {:variable, scname} ->
          case eval_expression(env, superclass) do
            {env, %Class{} = superclass} -> {env, superclass}
            _ -> raise Ilox.RuntimeError, token: scname, message: "Superclass must be a class."
          end

        nil ->
          {env, nil}
      end

    {env, _} = Env.define(env, name, nil)

    {env, superclass_scope_id} =
      if superclass do
        {env, scope_id} = Env.push_scope(env)
        env = Env.__define(env, "super", superclass)
        {env, scope_id}
      else
        {env, nil}
      end

    methods =
      methods
      |> Enum.map(fn {:fun_decl, name, _params, arity, _body} = fun ->
        {
          name.lexeme,
          Function.new(
            name: name.lexeme,
            arity: arity,
            decl: fun,
            closure_id: Env.current_scope(env),
            init: name == "init"
          )
        }
      end)
      |> Map.new()

    class = Class.new(name: name.lexeme, methods: methods, superclass: superclass)

    env = if superclass_scope_id, do: Env.pop_scope(env, superclass_scope_id), else: env

    {env, _} = Env.assign(env, name, class)
    env
  end

  # defp eval_expression(env, nil), do: {env, nil}

  defp eval_expression(env, {:group, expr}), do: eval_expression(env, expr)

  defp eval_expression(env, {:literal, value}) when value in [true, false, nil],
    do: {env, value}

  defp eval_expression(env, {:literal, %{literal: literal}}), do: {env, literal}

  defp eval_expression(env, {:unary, %Token{} = operator, right}) do
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
          raise "Invalid unary expr: #{inspect(operator.type)}(#{inspect(right)})."
      end

    {env, value}
  end

  defp eval_expression(env, {:binary, left, %Token{} = operator, right}) do
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

  defp eval_expression(env, {type, name} = expr) when type in [:variable, :this],
    do: {env, Env.get(env, expr, name)}

  defp eval_expression(env, {:super, keyword, name} = expr) do
    distance = Env.__distance(env, expr)
    superclass = Env.__get(env, keyword, distance)
    instance = Env.__get(env, %{keyword | lexeme: "this"}, distance - 1)

    case Class.method(superclass, name.lexeme) do
      {:ok, method} ->
        Function.bind(method, instance, env)

      :error ->
        raise Ilox.RuntimeError, token: name, message: "Undefined property '#{name.lexeme}'."
    end
  end

  defp eval_expression(env, {:assignment, name, value}) do
    {env, value} = eval_expression(env, value)
    Env.assign(env, name, value)
  end

  defp eval_expression(env, {:set, object, name, value}) do
    {env, value} = eval_expression(env, value)

    case eval_expression(env, object) do
      {env, {Instance, _id} = ref} -> Instance.set(ref, env, name, value)
      {env, %Instance{} = instance} -> Instance.set(instance, env, name, value)
      _ -> raise Ilox.RuntimeError, token: name, message: "Only instances have fields."
    end
  end

  defp eval_expression(env, {:logical, left, %Token{type: operator}, right})
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

    if !Callable.impl_for(callee) do
      raise Ilox.RuntimeError, token: closing, message: "Can only call functions and classes."
    end

    arity = Callable.arity(callee)

    if arity != argc do
      raise Ilox.RuntimeError,
        token: closing,
        message: "Expected #{arity} arguments but got #{argc}."
    end

    {env, arguments} =
      Enum.reduce(arguments, {env, []}, fn arg, {env, args} ->
        {env, arg} = eval_expression(env, arg)
        {env, [arg | args]}
      end)

    Callable.call(callee, env, Enum.reverse(arguments))
  end

  defp eval_expression(env, {:get, object, name}) do
    case eval_expression(env, object) do
      {env, {Instance, _id} = ref} -> Instance.get(ref, env, name)
      {env, %Instance{id: id}} -> Instance.get({Instance, id}, env, name)
      _ -> raise Ilox.RuntimeError, token: name, message: "Only instances have properties."
    end
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

  defp inspect_expression_value(_env, nil), do: "nil"
  defp inspect_expression_value(_env, value) when is_binary(value), do: ~s("#{value}")

  defp inspect_expression_value(env, {Instance, _id} = ref),
    do: to_string(Env.get_instance(env, ref))

  defp inspect_expression_value(env, value), do: stringify_expression_value(env, value)

  defp stringify_expression_value(_env, nil), do: ""

  defp stringify_expression_value(_env, value) when is_float(value) do
    value
    |> :erlang.float_to_binary([:short, :compact])
    |> String.replace_suffix(".0", "")
  end

  defp stringify_expression_value(env, {Instance, _id} = ref),
    do: to_string(Env.get_instance(env, ref))

  defp stringify_expression_value(_env, value), do: to_string(value)
end
