defmodule Ilox.Env do
  @moduledoc """
  An environment for storing variables in a scope.
  """

  alias Ilox.Callable
  alias Ilox.Token

  @type t :: %__MODULE__{
          values: %{required(String.t()) => term()},
          print: (IO.chardata() | String.Chars.t() -> any()),
          enclosing: nil | t
        }

  defstruct values: %{}, print: &IO.puts/1, enclosing: nil

  @doc """
  Create a new Ilox environment.
  """
  def new(fields \\ [])

  def new(%__MODULE__{} = enclosing),
    do: %__MODULE__{enclosing: enclosing, print: enclosing.print}

  def new([]), do: %__MODULE__{}

  def new(fields) do
    env = struct(__MODULE__, fields)

    if env.enclosing && !Keyword.get(fields, :print) do
      %{env | print: env.enclosing.print}
    else
      env
    end
  end

  @doc """
  Define a new variable.
  """
  def define(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name}, value) do
    assign_value(env, name, value)
  end

  @doc """
  Check if a variable is defined in the environment or any enclosing environment.

  If `local?` is true, only the current environment is searched.
  """
  def defined?(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id, local? \\ false) do
    case env do
      %{values: %{^name => _}} -> true
      %{enclosing: %__MODULE__{} = enclosing} when not local? -> defined?(enclosing, id, local?)
      _ -> false
    end
  end

  @doc """
  Assign a value to a defined variable.
  """
  def assign(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id, value) do
    case env do
      %{values: %{^name => _value}} ->
        assign_value(env, name, value)

      %{enclosing: %__MODULE__{} = enclosing} ->
        {enclosing, value} = assign(enclosing, id, value)
        {%{env | enclosing: enclosing}, value}

      _ ->
        raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Retrieve the value from the defined variable.
  """
  def get(%__MODULE__{} = env, %Token{type: :identifier, lexeme: name} = id) do
    case env do
      %{values: %{^name => value}} ->
        {env, value}

      %{enclosing: %__MODULE__{} = enclosing} ->
        {_env, value} = get(enclosing, id)
        {env, value}

      _ ->
        raise Ilox.RuntimeError, undefined_variable(id)
    end
  end

  @doc """
  Returns the root environment, the "globals".
  """
  def globals(%__MODULE__{enclosing: nil} = env), do: env
  def globals(%__MODULE__{enclosing: %__MODULE__{} = parent}), do: globals(parent)

  @doc false
  def __define(%__MODULE__{} = env, name, value) do
    elem(assign_value(env, name, value), 0)
  end

  @doc false
  def __defined?(%__MODULE__{} = env, name, local? \\ false) do
    case env do
      %{values: %{^name => _}} ->
        true

      %{enclosing: %__MODULE__{} = enclosing} when not local? ->
        __defined?(enclosing, name, local?)

      _ ->
        false
    end
  end

  @doc false
  def __prepend_globals, do: define_globals([])
  def __prepend_globals(%__MODULE__{} = env), do: __prepend_globals(env, define_globals([]))

  def __prepend_globals(globals_options) when is_list(globals_options),
    do: define_globals(globals_options)

  def __prepend_globals(%__MODULE__{} = env, globals_options) when is_list(globals_options),
    do: __prepend_globals(env, define_globals(globals_options))

  def __prepend_globals(
        %__MODULE__{} = env,
        %__MODULE__{enclosing: nil} = globals
      ) do
    enclosing =
      if env.enclosing do
        __prepend_globals(env.enclosing, globals)
      else
        globals
      end

    print =
      if env.print != enclosing.print && enclosing.print != (&IO.puts/1) do
        enclosing.print
      else
        env.print
      end

    %{env | enclosing: enclosing, print: print}
  end

  defp define_globals(globals_options) do
    clock = Callable.__native(0, fn env, _ -> {env, System.monotonic_time(:second) / 1} end)
    __define(new(globals_options), "clock", clock)
  end

  defp undefined_variable(%Token{} = name),
    do: [token: name, message: "Undefined variable '#{name.lexeme}'."]

  defp assign_value(env, name, value),
    do: {%{env | values: Map.put(env.values, name, value)}, value}
end
