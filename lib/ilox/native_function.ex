alias Ilox.Env

defmodule Ilox.NativeFunction do
  @moduledoc """
  A native function is implemented in Elixir for Ilox.
  """

  @type t :: %__MODULE__{
          name: String.t(),
          arity: non_neg_integer(),
          fun: (Env.t(), list(term()) -> {Env.t(), term})
        }

  @enforce_keys [:name, :arity, :fun]
  defstruct [:name, :arity, :fun]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  def __clock do
    new(
      name: "clock",
      arity: 0,
      fun: fn env, [] -> {env, System.monotonic_time(:second) / 1} end
    )
  end

  def __env do
    new(
      name: "env",
      arity: 0,
      fun: fn env, [] -> {env, inspect(env)} end
    )
  end

  defimpl Ilox.Callable do
    def arity(%{arity: arity}), do: arity

    def call(%{fun: fun}, %Env{} = env, args), do: fun.(env, args)
  end

  defimpl String.Chars do
    def to_string(%{name: name, arity: arity}),
      do: "<native fun: #{name}/#{arity}>"
  end

  defimpl Inspect do
    def inspect(%mod{name: name, arity: arity}, _opts), do: "#<#{mod} #{name}/#{arity}>"
  end
end
