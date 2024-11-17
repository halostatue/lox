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
          fields: %{required(String.t()) => term()}
        }

  @enforce_keys [:class, :id]
  defstruct [:id, :class, fields: %{}]

  def new(fields \\ []) do
    fields = Keyword.put_new(fields, :id, Id.new())
    struct!(__MODULE__, fields)
  end

  @spec get(t | {__MODULE__, id}, Env.t(), Token.t()) :: {Env.t(), term()}
  def get({__MODULE__, _id} = ref, env, field), do: get(Env.get_instance(env, ref), env, field)

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

  @spec set(t | {__MODULE__, id}, Env.t(), Token.t(), term()) :: {Env.t(), term()}
  def set({__MODULE__, _id} = ref, env, field, value),
    do: set(Env.get_instance(env, ref), env, field, value)

  def set(
        %__MODULE__{} = object,
        %Env{} = env,
        %Token{type: :identifier, lexeme: name},
        value
      ) do
    object = %{object | fields: Map.put(object.fields, name, value)}
    env = Env.put_instance(env, object)
    {env, value}
  end

  defimpl String.Chars do
    def to_string(%{class: %{name: name}}), do: "#{name} instance"
  end

  defimpl Inspect do
    def inspect(%mod{class: %{name: name}}, _opts) do
      "#<#{mod} #{name}>"
    end
  end
end
