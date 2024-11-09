defmodule Ilox.Callable do
  @moduledoc """
  A callable (a class or function).
  """

  alias Ilox.Env
  alias Ilox.Interpreter

  @enforce_keys [:arity, :decl, :to_string]
  defstruct [:arity, :decl, :to_string]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  def call(%__MODULE__{decl: decl}, %Env{} = env, arguments) do
    env =
      env
      |> Env.global_env()
      |> Env.new()

    do_call(decl, env, arguments)
  end

  def __native(arity, call), do: %__MODULE__{arity: arity, decl: call, to_string: "<native fn>"}

  defp do_call(native, env, args) when is_function(native, 2) do
    native.(env, args)
  end

  defp do_call({:function, _name, params, _arity, body}, env, arguments) do
    env =
      params
      |> Enum.zip(arguments)
      |> Enum.reduce(env, fn {name, value}, env -> elem(Env.define(env, name, value), 0) end)

    Interpreter.execute_block(env, body)
  end

  defimpl String.Chars do
    def to_string(%Ilox.Callable{to_string: value}) when is_binary(value), do: value
    def to_string(%Ilox.Callable{to_string: value}) when is_function(value, 0), do: value.()
  end
end
