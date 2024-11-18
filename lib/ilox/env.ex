defmodule Ilox.Env do
  @moduledoc """
  An environment for storing variables in a scope.

  TODO: How do we prevent scope leaks? We probably need some sort of compaction for
  clearly unused scopes.

  We will need to mark scopes as held by closures.
  """

  alias Ilox.Instance
  alias Ilox.NativeFunction
  alias Ilox.Scope
  alias Ilox.Token
  alias Lox.Id

  @type t :: %__MODULE__{
          stack: list(Scope.id()),
          scopes: %{required(Scope.id()) => term()},
          locals: %{required(Lox.expr()) => non_neg_integer()},
          instances: %{required(Instance.id()) => Instance.t()},
          print: (IO.chardata() | String.Chars.t() -> any())
        }

  @enforce_keys [:print]
  defstruct [:print, locals: %{}, scopes: %{}, stack: [], instances: %{}]

  @globals "<globals>"

  @doc """
  Initialize a new Ilox interpreter global environment.
  """
  @spec new(keyword()) :: t
  def new(options \\ []) do
    globals =
      Scope.new(@globals)
      |> Scope.put("clock", NativeFunction.__clock())
      |> Scope.put("env", NativeFunction.__env())

    __push_scope(%__MODULE__{print: Keyword.get(options, :print, &IO.puts/1)}, globals)
  end

  @doc """
  Sets the locals for the provided environment.
  """
  @spec set_locals(t, %{required(Lox.expr()) => non_neg_integer()}) :: t
  def set_locals(%__MODULE__{} = env, %{} = locals), do: %{env | locals: locals}

  @doc """
  Creates a new scope and pushes it in front of the current scope.
  """
  @spec push_scope(t) :: {t, Scope.id()}
  def push_scope(%__MODULE__{} = env) do
    new_scope_id = Id.new()
    {__push_scope(env, new_scope_id), new_scope_id}
  end

  @doc false
  @spec push_scope(t, Scope.id()) :: {t, Scope.id()}
  def push_scope(%__MODULE__{} = env, new_scope_id)
      when is_binary(new_scope_id) do
    {__push_scope(env, new_scope_id), new_scope_id}
  end

  @doc false
  @spec __push_scope(t) :: t
  def __push_scope(env), do: __push_scope(env, Id.new())

  @spec __push_scope(t, Scope.id() | Scope.t()) :: t
  def __push_scope(%__MODULE__{stack: [current_scope_id | _]} = env, new_scope_id)
      when is_binary(new_scope_id) do
    __push_scope(env, Scope.new(new_scope_id, current_scope_id))
  end

  def __push_scope(%__MODULE__{} = env, %Scope{} = scope) do
    # print_stack(scope, Process.info(self(), :current_stacktrace))
    %{env | stack: [scope.id | env.stack], scopes: Map.put(env.scopes, scope.id, scope)}
  end

  @doc """
  Makes the previous scope the current scope.

  If the current scope is the global scope, an exception will be thrown.
  """
  @spec pop_scope(t, Scope.id()) :: t
  def pop_scope(%__MODULE__{stack: []}, _scope_id) do
    raise "Invalid environment."
  end

  def pop_scope(%__MODULE__{stack: [@globals]}, _scope_id) do
    raise "Cannot pop root scope."
  end

  def pop_scope(%__MODULE__{stack: [scope_id | stack]} = env, scope_id), do: %{env | stack: stack}

  def pop_scope(%__MODULE__{} = _env, scope_id) do
    raise "Scope '#{scope_id}' is not the active scope."
  end

  @doc """
  Returns the current scope ID.
  """
  @spec current_scope(t) :: Scope.id()
  def current_scope(%__MODULE__{stack: [current | _]}), do: current

  @doc """
  Define a new variable in the current scope.
  """
  @spec define(t, Token.t(), term()) :: {t, term}
  def define(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name}, value),
    do: {assign_value(env, name, value), value}

  @doc false
  @spec __define(t, Scope.varname(), term()) :: t
  def __define(%__MODULE__{} = env, name, value), do: assign_value(env, name, value)

  @doc """
  Check if a variable is defined in the environment or any enclosing environment.

  If `recursive?` is `false`, only the current environment is searched. The default is `true`.
  """
  @spec defined?(t, Token.t(), boolean()) :: boolean()
  def defined?(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name}, recursive? \\ true),
    do: __defined?(env, name, recursive?)

  @doc false
  @spec __defined?(t, Scope.varname(), boolean()) :: boolean()
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

  @doc false
  @spec assign(t, Token.t(), term()) :: {t, term()}
  def assign(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id, value) do
    case find_defining_scope(env, name) do
      %Scope{} = scope -> {assign_value(env, scope, name, value), value}
      _ -> raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Assign a value to a defined variable, consulting the `locals` map for proper scope
  handling.
  """
  @spec assign(t, Lox.expr(), Token.t(), term()) :: {t, term()}
  def assign(%__MODULE__{} = env, expr, %Token{type: :identifier} = id, value),
    do: {do_assign(env, id, value, __distance(env, expr)), value}

  @doc false
  @spec get(t, Token.t()) :: term()
  def get(%__MODULE__{} = env, %Token{type: type, lexeme: name} = id)
      when type in [:identifier, :super, :this] do
    case find_defining_scope(env, name) do
      %Scope{} = scope -> Scope.get(scope, name)
      _ -> raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Retrieve the value from the defined variable, consulting the `locals` map for proper
  scope handling.
  """
  @spec get(t, Lox.expr(), Token.t()) :: term()
  def get(%__MODULE__{} = env, expr, %Token{type: type} = id)
      when type in [:identifier, :super, :this],
      do: __get(env, id, __distance(env, expr))

  @doc """
  Activate the specified scope with the provided id.

  If a `fun` is passed, it must accept the `env` and return a tuple of `env` and the
  original return value. If there is no return value, `nil` must be the returned value.
  """
  @spec activate_scope(t, Scope.id()) :: t
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
  Activates the specified scope, calls a function with the activated scope, then
  deactivates the scope.

  The function must expect `t:Env.t/0` and return a tuple of `{t:Env.t/0, term()}`.
  """
  @spec call_with_scope(t, Scope.id(), (t -> {t, term()})) :: {t, term()}
  def call_with_scope(%__MODULE__{stack: [scope_id | _]} = env, scope_id, fun)
      when is_function(fun, 1) do
    # This ensures that the function returns `{t, term}`.
    {env, value} = fun.(env)
    {env, value}
  end

  def call_with_scope(%__MODULE__{} = env, scope_id, fun) when is_function(fun, 1) do
    env = activate_scope(env, scope_id)
    {env, value} = fun.(env)
    {pop_scope(env, scope_id), value}
  end

  @doc false
  @spec find_defining_scope(t, Scope.varname()) :: Scope.t() | nil
  def find_defining_scope(%__MODULE__{} = env, name) do
    env
    |> scope_chain()
    |> Enum.find(&Scope.has_key?(&1, name))
  end

  @doc false
  @spec get_instance(t, Instance.t() | {Instance, Instance.id()}) :: Instance.t()
  def get_instance(%__MODULE__{} = env, %Instance{id: id}),
    do: get_instance(env, {Instance, id})

  def get_instance(%__MODULE__{instances: instances}, {Instance, id}),
    do: Map.fetch!(instances, id)

  @doc false
  @spec put_instance(t, Instance.t()) :: t
  def put_instance(%__MODULE__{instances: instances} = env, %Instance{id: id} = instance),
    do: %{env | instances: Map.put(instances, id, instance)}

  @doc false
  @spec put_scope(t, Scope.t()) :: t
  def put_scope(%__MODULE__{} = env, %Scope{} = scope),
    do: %{env | scopes: Map.put(env.scopes, scope.id, scope)}

  @doc false
  @spec __get(t, Token.t(), non_neg_integer() | nil) :: term()
  def __get(%{scopes: %{@globals => globals}}, %Token{lexeme: name} = id, nil) do
    if Scope.has_key?(globals, name) do
      Scope.get(globals, name)
    else
      raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  def __get(env, %Token{lexeme: name} = id, distance) do
    case List.pop_at(scope_chain(env), distance) do
      {nil, _scopes} -> raise Ilox.RuntimeError, undefined_variable(id)
      {scope, _scopes} -> Scope.get(scope, name)
    end
  end

  @doc false
  @spec __distance(t, Lox.expr()) :: nil | non_neg_integer()
  def __distance(env, expr), do: Map.get(env.locals, expr)

  @spec scope_chain(t) :: Enumerable.t(Scope.t())
  defp scope_chain(%__MODULE__{stack: [current | _], scopes: scopes}) do
    scopes[current]
    |> Stream.unfold(fn
      nil -> nil
      %Scope{} = scope -> {scope, scopes[scope.enclosing_id]}
    end)
    |> Enum.to_list()
  end

  @spec undefined_variable(Token.t()) :: keyword()
  defp undefined_variable(%Token{} = name),
    do: [token: name, message: "Undefined variable '#{name.lexeme}'."]

  @spec assign_value(t, Scope.varname(), term()) :: t
  defp assign_value(%__MODULE__{stack: [current | _], scopes: scopes} = env, name, value),
    do: assign_value(env, scopes[current], name, value)

  @spec assign_value(t, Scope.t(), Scope.varname(), term()) :: t
  defp assign_value(env, scope, name, %Instance{id: id} = instance) do
    env
    |> put_instance(instance)
    |> assign_value(scope, name, {Instance, id})
  end

  defp assign_value(env, scope, name, value) do
    %{env | scopes: %{env.scopes | scope.id => Scope.put(scope, name, value)}}
  end

  @spec do_assign(t, Token.t(), term(), non_neg_integer() | nil) :: t
  defp do_assign(%{scopes: %{@globals => globals}} = env, %Token{lexeme: name} = id, value, nil) do
    if Scope.has_key?(globals, name) do
      assign_value(env, globals, name, value)
    else
      raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  defp do_assign(env, %Token{lexeme: name} = id, value, distance) do
    case List.pop_at(scope_chain(env), distance) do
      {nil, _scopes} -> raise Ilox.RuntimeError, undefined_variable(id)
      {scope, _scopes} -> assign_value(env, scope, name, value)
    end
  end

  @doc false
  def print_stack(scope, {:current_stacktrace, stack}) do
    IO.puts("==> push_scope #{scope.id} (#{scope.enclosing_id})")

    stack
    |> Enum.map(fn {mod, fun, arity, location} ->
      String.replace_prefix(
        "#{mod}.#{fun}/#{arity} #{location[:file]}:#{location[:line]}",
        "Elixir.",
        ""
      )
    end)
    |> Enum.filter(&String.starts_with?(&1, "Ilox."))
    |> Enum.join("\n")
    |> IO.puts()

    IO.puts("")
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(env, opts) do
      concat([
        "#Env<",
        to_doc(env.stack, opts),
        " scopes: ",
        to_doc(env.scopes, opts),
        " locals: ",
        to_doc(env.locals, opts),
        ">"
      ])
    end
  end
end
