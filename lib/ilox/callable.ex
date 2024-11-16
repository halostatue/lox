defmodule Ilox.Callable do
  @moduledoc """
  A callable (a class or function).
  """

  alias Ilox.Env
  alias Ilox.Interpreter

  @enforce_keys [:arity, :decl, :to_string, :closure_id]
  defstruct [:arity, :decl, :to_string, :closure_id]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  def call(env, %__MODULE__{decl: decl, closure_id: closure_id}, arguments) do
    Env.call_with_scope(env, closure_id, fn env ->
      {env, scope_id} = Env.push_scope(env)
      {env, value} = do_call(env, decl, arguments)
      {Env.pop_scope(env, scope_id), value}
    end)
  end

  def __native(arity, call),
    do: %__MODULE__{
      arity: arity,
      decl: call,
      to_string: "<native fn>",
      closure_id: "<globals>"
    }

  def __clock,
    do: __native(0, fn env, _ -> {env, System.monotonic_time(:second) / 1} end)

  def __env,
    do: __native(0, fn env, _ -> {env, inspect(env)} end)

  defp do_call(env, native, args) when is_function(native, 2), do: native.(env, args)

  defp do_call(env, {:fun_decl, _name, params, _arity, body}, arguments) do
    env =
      params
      |> Enum.zip(arguments)
      |> Enum.reduce(env, fn {name, value}, env -> elem(Env.define(env, name, value), 0) end)

    Interpreter.eval_function_body(env, body)
  catch
    {env, value} -> {env, value}
  end

  defimpl String.Chars do
    def to_string(%Ilox.Callable{to_string: value}) when is_binary(value), do: value
    def to_string(%Ilox.Callable{to_string: value}) when is_function(value, 0), do: value.()
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(callable, opts) do
      concat([
        "#Callable<",
        to_string(callable),
        " arity: ",
        to_doc(callable.arity, opts),
        " closure: ",
        to_doc(callable.closure_id, opts),
        ">"
      ])
    end
  end
end
