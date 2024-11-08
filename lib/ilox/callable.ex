defmodule Ilox.Callable do
  @moduledoc """
  A callable (a class or function).
  """

  alias Ilox.Env

  @enforce_keys [:arity, :call, :to_string]
  defstruct [:arity, :call, :to_string]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  def call(%__MODULE__{} = callable, %Env{} = env, arguments) do
    if is_function(callable.call, 2) do
      callable.call.(env, arguments)
    else
      raise "Not yet implemented"
    end
  end

  def __native(arity, call), do: %__MODULE__{arity: arity, call: call, to_string: "<native fn>"}

  defimpl String.Chars do
    def to_string(%Ilox.Callable{to_string: value}) when is_binary(value), do: value
    def to_string(%Ilox.Callable{to_string: value}) when is_function(value, 0), do: value.()
  end
end
