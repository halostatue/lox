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
      {env, value} = do_call(Env.push_scope(env), decl, arguments)
      {Env.pop_scope(env), value}
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

  defp do_call(env, {:function, _name, params, _arity, body}, arguments) do
    env =
      params
      |> Enum.zip(arguments)
      |> Enum.reduce(env, fn {name, value}, env -> elem(Env.define(env, name, value), 0) end)

    Interpreter.execute_function_body(env, body)
  rescue
    e in Ilox.ReturnValue ->
      {e.env, e.value}
  end

  defimpl String.Chars do
    def to_string(%Ilox.Callable{to_string: value}) when is_binary(value), do: value
    def to_string(%Ilox.Callable{to_string: value}) when is_function(value, 0), do: value.()
  end
end
