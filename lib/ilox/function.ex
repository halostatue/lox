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

  @spec bind(t, Instance.t(), Env.t()) :: {Env.t(), t}
  def bind(%__MODULE__{} = fun, %Instance{} = instance, %Env{} = env) do
    scope =
      Id.new()
      |> Scope.new(fun.closure_id)
      |> Scope.put("this", instance)

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
        |> Enum.reduce(env, fn {name, value}, env -> elem(Env.define(env, name, value), 0) end)

      Ilox.Interpreter.eval_function_body(env, body)
    catch
      {env, value} ->
        cond do
          init ->
            ref = Env.get(env, %{name | lexeme: "this"})
            {env, Env.get_instance(env, ref)}

          match?({Instance, _id}, value) ->
            {env, Env.get_instance(env, value)}

          true ->
            {env, value}
        end
    end
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
