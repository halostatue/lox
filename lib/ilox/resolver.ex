defmodule Ilox.Resolver do
  @moduledoc """
  The implementation of a variable resolver for ilox.
  """

  alias Ilox.Env
  alias Ilox.Token

  defmodule Ctx do
    @moduledoc false

    @type t :: %__MODULE__{
            class_type: :none | :class,
            errors: list(keyword()),
            function_type: :none | :function | :initializer | :method,
            locals: %{required(tuple()) => non_neg_integer()},
            scopes: list(%{required(String.t()) => boolean()}),
            test_mode: boolean()
          }

    defstruct class_type: :none,
              errors: [],
              function_type: :none,
              locals: %{},
              scopes: [],
              test_mode: false

    def new(fields \\ []), do: struct!(__MODULE__, fields)

    def finish(%__MODULE__{errors: [_ | _] = errors}),
      do: {:error, :resolver, Enum.reverse(errors)}

    def finish(%__MODULE__{test_mode: true} = ctx) do
      Map.drop(Map.from_struct(ctx), [:test_mode, :scopes, :errors, :function_type, :class_type])
    end

    def finish(%__MODULE__{locals: locals}), do: {:ok, locals}

    def push(%__MODULE__{} = ctx), do: %{ctx | scopes: [%{} | ctx.scopes]}

    def pop(%__MODULE__{scopes: [_popped | scopes]} = ctx), do: %{ctx | scopes: scopes}

    def declare(%__MODULE__{scopes: []} = ctx, _name), do: ctx

    def declare(%__MODULE__{scopes: [scope | scopes]} = ctx, name) do
      ctx =
        maybe_error(
          ctx,
          Map.has_key?(scope, name.lexeme),
          name,
          "Already declared '#{name.lexeme}' in this scope."
        )

      %{ctx | scopes: [Map.put(scope, name.lexeme, false) | scopes]}
    end

    def define(%__MODULE__{scopes: []} = ctx, _name), do: ctx

    def define(%__MODULE__{scopes: [scope | scopes]} = ctx, name),
      do: %{ctx | scopes: [Map.put(scope, name.lexeme, true) | scopes]}

    def __define(%__MODULE__{scopes: [scope | scopes]} = ctx, name),
      do: %{ctx | scopes: [Map.put(scope, name, true) | scopes]}

    def maybe_error(%__MODULE__{} = ctx, true, token, message),
      do: %{ctx | errors: [[token: token, message: message] | ctx.errors]}

    def maybe_error(%__MODULE__{} = ctx, false, _token, _message), do: ctx
  end

  def resolve(%Env{} = env, statements, test_mode \\ false) do
    ctx = Ctx.new(test_mode: test_mode)

    case Ctx.finish(resolve_statements(ctx, statements)) do
      {:ok, locals} -> {:ok, Env.set_locals(env, locals)}
      result -> result
    end
  end

  @spec resolve_statements(Ctx.t(), list(nil | tuple())) :: Ctx.t()
  defp resolve_statements(ctx, []), do: ctx
  defp resolve_statements(ctx, [nil]), do: ctx

  defp resolve_statements(ctx, [statement | statements]) do
    ctx
    |> resolve_statement(statement)
    |> resolve_statements(statements)
  end

  @spec resolve_statement(Ctx.t(), nil | tuple) :: Ctx.t()
  defp resolve_statement(ctx, nil), do: ctx

  defp resolve_statement(ctx, {:block, block}) do
    ctx
    |> Ctx.push()
    |> resolve_statements(block)
    |> Ctx.pop()
  end

  defp resolve_statement(ctx, {:var_decl, name, initializer}) do
    ctx
    |> Ctx.declare(name)
    |> resolve_expression(initializer)
    |> Ctx.define(name)
  end

  defp resolve_statement(ctx, {type, expr})
       when type in [:expr_stmt, :print_stmt],
       do: resolve_expression(ctx, expr)

  defp resolve_statement(ctx, {:return_stmt, token, expr}) do
    ctx
    |> Ctx.maybe_error(ctx.function_type == :none, token, "Can't return from top-level code.")
    |> Ctx.maybe_error(
      ctx.function_type == :initializer && expr != nil,
      token,
      "Can't return a value from an initializer."
    )
    |> resolve_expression(expr)
  end

  defp resolve_statement(ctx, {:if_stmt, condition, then_branch, else_branch}) do
    ctx
    |> resolve_expression(condition)
    |> resolve_statement(then_branch)
    |> resolve_statement(else_branch)
  end

  defp resolve_statement(ctx, {:while_stmt, condition, body}) do
    ctx
    |> resolve_expression(condition)
    |> resolve_statement(body)
  end

  defp resolve_statement(ctx, {:fun_decl, name, _params, _arity, _body} = fun) do
    ctx
    |> Ctx.declare(name)
    |> Ctx.define(name)
    |> resolve_function(fun, :function)
  end

  defp resolve_statement(ctx, {:class_decl, name, methods}) do
    enclosing_class = ctx.class_type

    ctx =
      %{ctx | class_type: :class}
      |> Ctx.declare(name)
      |> Ctx.define(name)
      |> Ctx.push()
      |> Ctx.__define("this")
      |> resolve_methods(methods)
      |> Ctx.pop()

    %{ctx | class_type: enclosing_class}
  end

  @spec resolve_expressions(Ctx.t(), list(nil | tuple())) :: Ctx.t()
  defp resolve_expressions(ctx, exprs) when is_list(exprs) do
    Enum.reduce(exprs, ctx, &resolve_expression(&2, &1))
  end

  @spec resolve_expression(Ctx.t(), list(nil | tuple())) :: Ctx.t()
  defp resolve_expression(ctx, nil), do: ctx

  defp resolve_expression(ctx, {:group, expr}), do: resolve_expression(ctx, expr)

  defp resolve_expression(ctx, {:binary, left, _token, right}) do
    ctx
    |> resolve_expression(left)
    |> resolve_expression(right)
  end

  defp resolve_expression(ctx, {:call, callee, arguments, _argc, _closing}) do
    ctx
    |> resolve_expression(callee)
    |> resolve_expressions(arguments)
  end

  defp resolve_expression(ctx, {:get, object, _name}), do: resolve_expression(ctx, object)

  defp resolve_expression(ctx, {:literal, _}), do: ctx

  defp resolve_expression(%{class_type: :none} = ctx, {:this, keyword}),
    do: Ctx.maybe_error(ctx, true, keyword, "Can't use 'this' outside of a class.")

  defp resolve_expression(ctx, {:this, keyword} = expr), do: resolve_local(ctx, expr, keyword)

  defp resolve_expression(ctx, {:logical, left, _token, right}) do
    ctx
    |> resolve_expression(left)
    |> resolve_expression(right)
  end

  defp resolve_expression(ctx, {:unary, _operator, right}),
    do: resolve_expression(ctx, right)

  defp resolve_expression(ctx, {:variable, name} = expr) do
    ctx
    |> check_read_in_init(name)
    |> resolve_local(expr, name)
  end

  defp resolve_expression(ctx, {:assignment, name, value} = expr) do
    ctx
    |> resolve_expression(value)
    |> resolve_local(expr, name)
  end

  defp resolve_expression(ctx, {:set, object, _name, value}) do
    ctx
    |> resolve_expression(value)
    |> resolve_expression(object)
  end

  @spec check_read_in_init(Ctx.t(), Token.t()) :: Ctx.t()
  def check_read_in_init(%{scopes: []} = ctx, _name), do: ctx

  def check_read_in_init(%{scopes: [scope | _]} = ctx, name) do
    Ctx.maybe_error(
      ctx,
      Map.get(scope, name.lexeme) == false,
      name,
      "Can't read local variable '#{name.lexeme}' in its own initializer."
    )
  end

  @spec resolve_local(Ctx.t(), tuple(), Token.t()) :: Ctx.t()
  defp resolve_local(ctx, expr, %Token{lexeme: name}) do
    if depth = Enum.find_index(ctx.scopes, &Map.has_key?(&1, name)) do
      %{ctx | locals: Map.put(ctx.locals, expr, depth)}
    else
      ctx
    end
  end

  @spec resolve_function(Ctx.t(), tuple(), type :: atom()) :: Ctx.t()
  defp resolve_function(ctx, {:fun_decl, _name, params, _arity, body}, type) do
    enclosing_function = ctx.function_type

    ctx =
      %{ctx | function_type: type}
      |> Ctx.push()
      |> resolve_function_params(params)
      |> resolve_statement(body)
      |> Ctx.pop()

    %{ctx | function_type: enclosing_function}
  end

  @spec resolve_function_params(Ctx.t(), list(tuple())) :: Ctx.t()
  defp resolve_function_params(ctx, params) do
    Enum.reduce(params, ctx, fn param, ctx ->
      ctx
      |> Ctx.declare(param)
      |> Ctx.define(param)
    end)
  end

  @spec resolve_methods(Ctx.t(), list(tuple())) :: Ctx.t()
  defp resolve_methods(ctx, methods) do
    Enum.reduce(methods, ctx, fn {:fun_decl, %Token{lexeme: name}, _params, _arity, _body} =
                                   method,
                                 ctx ->
      resolve_function(ctx, method, if(name == "init", do: :initializer, else: :method))
    end)
  end
end
