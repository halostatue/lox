defmodule Ilox.Scope do
  @moduledoc """
  A scope object in Ilox.
  """

  alias Lox.Id

  @typedoc "The identifier for the scope."
  @type id :: Id.t()

  @typedoc "A variable name."
  @type varname :: String.t()

  @type t :: %__MODULE__{
          id: id,
          values: %{required(varname) => term()},
          enclosing_id: nil | id
        }

  @enforce_keys [:id]
  defstruct [:id, values: %{}, enclosing_id: nil]

  @spec new(id) :: t
  def new(id) when is_binary(id), do: %__MODULE__{id: id}

  @spec new(id, id) :: t
  def new(id, enclosing_id) when is_binary(id) and is_binary(enclosing_id),
    do: %__MODULE__{id: id, enclosing_id: enclosing_id}

  @spec has_key?(t, varname) :: boolean()
  def has_key?(scope, name), do: Map.has_key?(scope.values, name)

  @spec put(t, varname, term()) :: t
  def put(scope, name, value), do: %{scope | values: Map.put(scope.values, name, value)}

  @spec get(t, varname) :: term()
  def get(scope, name), do: Map.get(scope.values, name)

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(scope, opts) do
      variables =
        if scope.id == "<globals>" do
          Map.drop(scope.values, ["clock", "env"])
        else
          scope.values
        end
        |> Map.keys()
        |> to_doc(opts)

      enclosing =
        if is_nil(scope.enclosing_id) do
          ""
        else
          concat([" enclosing: ", to_doc(scope.enclosing_id, opts)])
        end

      concat([
        "#Scope<",
        variables,
        enclosing,
        ">"
      ])
    end
  end
end
