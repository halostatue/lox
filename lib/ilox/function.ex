alias Ilox.Env

defmodule Ilox.Function do
  @moduledoc """
  A function is implemented in Lox.
  """

  alias Ilox.Env
  alias Ilox.Instance
  alias Ilox.Scope
  alias Lox.Id

  @type t :: %__MODULE__{
          name: String.t(),
          arity: non_neg_integer(),
          decl: term(),
          closure_id: Scope.id(),
          init: boolean()
        }

  @enforce_keys [:name, :arity, :decl, :closure_id]
  defstruct [:name, :arity, :decl, :closure_id, init: false]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  @spec bind(t, {Instance, Instance.t()} | Instance.t(), Env.t()) :: {Env.t(), t}
  def bind(%__MODULE__{} = fun, {Instance, _instance} = ref, %Env{} = env),
    do: bind(fun, Env.get_instance(env, ref), env)

  def bind(%__MODULE__{} = fun, %Instance{} = instance, %Env{} = env) do
    scope =
      Id.new()
      |> Scope.new(fun.closure_id)
      |> Scope.put("this", {Instance, instance})

    {Env.put_scope(env, scope), %{fun | closure_id: scope.id}}
  end

  defimpl Ilox.Callable do
    def arity(%{arity: arity}), do: arity

    def call(%{decl: decl, closure_id: closure_id, init: init}, %Env{} = env, args) do
      Env.call_with_scope(env, closure_id, fn env ->
        {env, scope_id} = Env.push_scope(env)
        {env, value} = do_call(env, decl, args, init)
        {Env.pop_scope(env, scope_id), value}
      end)
    end

    defp do_call(env, {:fun_decl, name, params, _arity, body}, arguments, init) do
      env =
        params
        |> Enum.zip(arguments)
        |> Enum.reduce(env, fn {k, v}, env -> elem(Env.define(env, k, v), 0) end)

      env
      |> Ilox.Interpreter.eval_function_body(body)
      |> resolve_return_value(name, init)
    catch
      {env, value} -> resolve_return_value({env, value}, name, init)
    end

    defp resolve_return_value({env, _value}, name, true),
      do: {env, Env.get_instance(env, Env.get(env, %{name | lexeme: "this"}))}

    defp resolve_return_value({env, {Instance, _instance} = ref}, _, _),
      do: {env, Env.get_instance(env, ref)}

    defp resolve_return_value({env, value}, _, _), do: {env, value}
  end

  defimpl String.Chars do
    def to_string(%Ilox.Function{name: name, arity: arity}), do: "<fun #{name}/#{arity}>"
  end

  defimpl Inspect do
    def inspect(%mod{name: name, arity: arity, closure_id: closure_id}, _opts) do
      if closure_id == "<globals>" do
        "#<#{mod} #{name}/#{arity}>"
      else
        "#<#{mod} #{name}/#{arity} (#{closure_id})>"
      end
    end
  end
end
