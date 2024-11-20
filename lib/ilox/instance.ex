defmodule Ilox.Instance do
  @moduledoc """
  An instance of a `t:Ilox.Class.t/0`.
  """

  alias Ilox.Class
  alias Ilox.Function
  alias Ilox.Env
  alias Ilox.Token
  alias Lox.Id

  @type id :: Id.t()

  @type t :: %__MODULE__{
          id: id(),
          class: Class.t(),
          fields: %{required(String.t()) => term()},
          generation: non_neg_integer()
        }

  @enforce_keys [:class, :id]
  defstruct [:id, :class, fields: %{}, generation: 0]

  def new(fields \\ []) do
    fields = Keyword.put_new(fields, :id, Id.new())
    struct!(__MODULE__, fields)
  end

  @spec get(t | {__MODULE__, t}, Env.t(), Token.t()) :: {Env.t(), term()}
  def get({__MODULE__, _instance} = ref, env, field),
    do: get(Env.get_instance(env, ref), env, field)

  def get(
        %__MODULE__{fields: fields, class: class} = instance,
        %Env{} = env,
        %Token{type: :identifier, lexeme: name} = field
      ) do
    case Map.fetch(fields, name) do
      {:ok, field} ->
        {env, field}

      :error ->
        case Class.method(class, name) do
          {:ok, method} ->
            Function.bind(method, instance, env)

          :error ->
            raise Ilox.RuntimeError, token: field, message: "Undefined property '#{name}'."
        end
    end
  end

  @spec set(t | {__MODULE__, t}, Env.t(), Token.t(), term()) :: {Env.t(), term()}
  def set({__MODULE__, %__MODULE__{} = instance}, env, field, value),
    do: set(instance, env, field, value)

  def set(
        %__MODULE__{} = instance,
        %Env{} = env,
        %Token{type: :identifier, lexeme: name},
        value
      ) do
    instance = %{
      instance
      | fields: Map.put(instance.fields, name, value),
        generation: instance.generation + 1
    }

    env = Env.put_instance(env, instance)

    env =
      if Env.__defined?(env, "this") do
        Env.__assign(env, "this", {__MODULE__, instance})
      else
        env
      end

    {env, value}
  end

  defimpl String.Chars do
    def to_string(%{class: %{name: name}}), do: "#{name} instance"
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%mod{class: %{name: class_name}} = instance, opts) do
      concat([
        "#<",
        to_doc(mod, opts),
        " ",
        class_name,
        " id: ",
        instance.id,
        " gen: ",
        to_doc(instance.generation, opts),
        " ",
        to_doc(instance.fields, opts),
        ">"
      ])
    end
  end
end
