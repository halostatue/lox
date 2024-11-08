defmodule Ilox.Env do
  @moduledoc """
  An environment for storing variables in a scope.
  """

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
  Retrieve the valuefrom the defined variable.
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

  @doc false
  def __define(%__MODULE__{} = env, name, value) do
    assign_value(env, name, value)
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

  defp undefined_variable(%Token{} = name),
    do: [token: name, message: "Undefined variable '#{name.lexeme}'."]

  defp assign_value(env, name, value),
    do: {%{env | values: Map.put(env.values, name, value)}, value}
end
