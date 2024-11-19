defmodule Ilox.Class do
  alias Ilox.Callable
  alias Ilox.Env
  alias Ilox.Function
  alias Ilox.Instance

  @type t :: %__MODULE__{
          name: String.t(),
          superclass: nil | t(),
          methods: %{required(String.t()) => Function.t()}
        }

  @enforce_keys [:name, :methods]
  defstruct [:name, :methods, superclass: nil]

  def new(opts \\ []), do: struct!(__MODULE__, opts)

  # This will eventually search in superclasses as well
  @spec method(t, String.t()) :: {:ok, Function.t()} | :error
  def method(%__MODULE__{methods: methods, superclass: superclass}, name) do
    case Map.fetch(methods, name) do
      :error when is_nil(superclass) -> :error
      :error -> method(superclass, name)
      {:ok, method} -> {:ok, method}
    end
  end

  @doc false
  def __call(%__MODULE__{} = class, %Env{} = env, args) do
    instance = Instance.new(class: class)

    {env, instance} =
      case method(class, "init") do
        {:ok, init} ->
          {env, init} = Function.bind(init, instance, env)

          Callable.call(init, env, args)
          |> IO.inspect()

        :error ->
          {env, instance}
      end

    env = Env.put_instance(env, instance)
    {env, {Instance, instance.id}}
  end

  @doc false
  def __arity(%__MODULE__{} = class) do
    case method(class, "init") do
      {:ok, init} -> Callable.arity(init)
      :error -> 0
    end
  end

  defimpl Callable do
    def arity(%mod{} = callable), do: mod.__arity(callable)
    def call(%mod{} = callable, env, args), do: mod.__call(callable, env, args)
  end

  defimpl String.Chars do
    def to_string(%{name: name}), do: name
  end

  defimpl Inspect do
    def inspect(%mod{name: name}, _opts) do
      "#<#{mod} #{name}>"
    end
  end
end
