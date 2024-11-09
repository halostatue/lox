defmodule Ilox.Env do
  @moduledoc """
  An environment for storing variables in a scope.
  """

  alias Ilox.Callable
  alias Ilox.Token

  defmodule Scope do
    @moduledoc false

    @type t :: %__MODULE__{
            id: String.t(),
            values: %{required(String.t()) => term()},
            enclosing_id: nil | String.t()
          }

    @enforce_keys [:id]
    defstruct [:id, values: %{}, enclosing_id: nil]

    @spec new(String.t()) :: t
    def new(id) when is_binary(id), do: %__MODULE__{id: id}

    @spec new(String.t(), String.t()) :: t
    def new(id, enclosing_id) when is_binary(id) and is_binary(enclosing_id),
      do: %__MODULE__{id: id, enclosing_id: enclosing_id}

    @spec has_key?(t, String.t()) :: boolean()
    def has_key?(scope, name), do: Map.has_key?(scope.values, name)

    @spec put(t, String.t(), term()) :: t
    def put(scope, name, value), do: %{scope | values: Map.put(scope.values, name, value)}

    @spec get(t, String.t()) :: term()
    def get(scope, name), do: Map.get(scope.values, name)

    defimpl Inspect do
      import Inspect.Algebra

      def inspect(scope, opts) do
        variables =
          if Enum.empty?(scope.values) do
            "[]"
          else
            to_doc(Map.keys(scope.values), opts)
          end

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

  @type t :: %__MODULE__{
          stack: list(String.t()),
          scopes: %{required(String.t()) => term()},
          print: (IO.chardata() | String.Chars.t() -> any())
        }

  @enforce_keys [:print]
  defstruct [:print, scopes: %{}, stack: []]

  @globals "<globals>"

  @doc """
  Initialize a new Ilox interpreter global environment.
  """
  @spec new(keyword()) :: t
  def new(options \\ []) do
    globals =
      Scope.new(@globals)
      |> Scope.put("clock", Callable.__clock())
      |> Scope.put("env", Callable.__env())

    __push_scope(%__MODULE__{print: Keyword.get(options, :print, &IO.puts/1)}, globals)
  end

  @doc """
  Creates a new scope and pushes it in front of the current scope.
  """
  @spec push_scope(t) :: t
  def push_scope(%__MODULE__{stack: [current_scope_id | _]} = env) do
    __push_scope(env, Scope.new(scope_id(), current_scope_id))
  end

  @doc false
  @spec push_scope(t, String.t() | Scope.t()) :: t
  def push_scope(%__MODULE__{stack: [current_scope_id | _]} = env, scope_id)
      when is_binary(scope_id) do
    __push_scope(env, Scope.new(scope_id, current_scope_id))
  end

  @doc """
  Makes the previous scope the current scope.

  If the current scope is the global scope, an exception will be thrown.
  """
  @spec pop_scope(t) :: t
  def pop_scope(%__MODULE__{stack: []}) do
    raise "Invalid environment."
  end

  def pop_scope(%__MODULE__{stack: [@globals]}) do
    raise "Cannot pop root scope."
  end

  def pop_scope(%__MODULE__{stack: [_popped | stack]} = env) do
    %{env | stack: stack}
  end

  @doc """
  Returns the current scope ID.
  """
  @spec current_scope(t) :: String.t()
  def current_scope(%__MODULE__{stack: [current | _]}), do: current

  @doc """
  Define a new variable in the current scope.
  """
  @spec define(t, Token.t(), term()) :: {t, term}
  def define(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name}, value),
    do: {assign_value(env, name, value), value}

  @doc false
  @spec __define(t, String.t(), term()) :: t
  def __define(%__MODULE__{} = env, name, value), do: assign_value(env, name, value)

  @doc """
  Check if a variable is defined in the environment or any enclosing environment.

  If `recursive?` is `false`, only the current environment is searched. The default is `true`.
  """
  @spec defined?(t, Token.t(), boolean()) :: boolean()
  def defined?(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name}, recursive? \\ true),
    do: __defined?(env, name, recursive?)

  @doc false
  @spec __defined?(t, String.t(), boolean()) :: boolean()
  def __defined?(env, name, recursive? \\ true)

  def __defined?(%__MODULE__{stack: [@globals | _], scopes: %{@globals => scope}}, name, true) do
    Scope.has_key?(scope, name)
  end

  def __defined?(%__MODULE__{stack: [current | _], scopes: scopes}, name, false) do
    Map.has_key?(scopes, current) and Scope.has_key?(scopes[current], name)
  end

  def __defined?(%__MODULE__{} = env, name, true) do
    case find_defining_scope(env, name) do
      %Scope{} -> true
      _ -> false
    end
  end

  @doc """
  Assign a value to a defined variable.
  """
  @spec assign(t, Token.t(), term()) :: {t, term()}
  def assign(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id, value) do
    case find_defining_scope(env, name) do
      %Scope{} = scope -> {assign_value(env, scope, name, value), value}
      _ -> raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Retrieve the value from the defined variable.
  """
  @spec get(t, String.t()) :: term()
  def get(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id) do
    case find_defining_scope(env, name) do
      %Scope{} = scope -> Scope.get(scope, name)
      _ -> raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Activate the specified scope with the provided id.

  If a `fun` is passed, it must accept the `env` and return a tuple of `env` and the
  original return value. If there is no return value, `nil` must be the returned value.
  """
  @spec activate_scope(t, String.t()) :: t
  def activate_scope(env, scope_id)

  def activate_scope(%__MODULE__{stack: [scope_id | _]} = env, scope_id), do: env

  def activate_scope(%__MODULE__{} = env, scope_id) do
    if Map.has_key?(env.scopes, scope_id) do
      %{env | stack: [scope_id | env.stack]}
    else
      raise "Invalid scope '#{scope_id}'."
    end
  end

  @doc """
  Deactivates the current scope, if the current matches the provided scope ID.

  An exception is thrown if the scope does not match.
  """
  def deactivate_scope(%__MODULE__{stack: [@globals]}, @globals) do
    raise "Cannot deactivate root scope."
  end

  def deactivate_scope(%__MODULE__{stack: [current | _]} = env, current), do: pop_scope(env)

  def deactivate_scope(%__MODULE__{} = _env, scope_id) do
    raise "Scope '#{scope_id}' is not the active scope."
  end

  @doc """
  Activates the specified scope, calls a function with the activated scope, then
  deactivates the scope.

  The function must expect `t:Env.t/0` and return a tuple of `{t:Env.t/0, term()}`.
  """
  @spec call_with_scope(t, String.t(), (t -> {t, term()})) :: {t, term()}
  def call_with_scope(%__MODULE__{stack: [scope_id | _]} = env, scope_id, fun)
      when is_function(fun, 1) do
    {env, value} = fun.(env)
    {env, value}
  end

  def call_with_scope(%__MODULE__{} = env, scope_id, fun) when is_function(fun, 1) do
    env = activate_scope(env, scope_id)
    {env, value} = fun.(env)
    {deactivate_scope(env, scope_id), value}
  end

  @doc false
  @spec find_defining_scope(t, String.t()) :: Scope.t() | nil
  def find_defining_scope(%__MODULE__{stack: [current | _], scopes: scopes}, name) do
    scopes[current]
    |> Stream.unfold(fn
      nil -> nil
      %Scope{} = scope -> {scope, scopes[scope.enclosing_id]}
    end)
    |> Enum.find(&Scope.has_key?(&1, name))
  end

  @consonants [?b, ?f, ?g, ?k, ?n, ?p, ?r, ?s, ?t, ?v, ?x, ?z]
  @vowels [?a, ?e, ?i, ?o, ?u]

  @spec scope_id :: String.t()
  defp scope_id,
    do: Enum.into(1..3, "", fn _ -> <<Enum.random(@consonants), Enum.random(@vowels)>> end)

  @spec undefined_variable(Token.t()) :: keyword()
  defp undefined_variable(%Token{} = name),
    do: [token: name, message: "Undefined variable '#{name.lexeme}'."]

  @spec assign_value(t, String.t(), term()) :: t
  defp assign_value(%__MODULE__{stack: [current | _], scopes: scopes} = env, name, value),
    do: assign_value(env, scopes[current], name, value)

  @spec assign_value(t, Scope.t(), String.t(), term()) :: t
  defp assign_value(env, scope, name, value) do
    %{env | scopes: %{env.scopes | scope.id => Scope.put(scope, name, value)}}
  end

  defp __push_scope(env, %Scope{} = scope) do
    %{env | stack: [scope.id | env.stack], scopes: Map.put(env.scopes, scope.id, scope)}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(env, opts) do
      concat([
        "#Env<",
        to_doc(env.stack, opts),
        " scopes: ",
        to_doc(env.scopes, opts),
        ">"
      ])
    end
  end
end
