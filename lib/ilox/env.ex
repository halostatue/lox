defmodule Ilox.Env do
  @moduledoc """
  An environment for storing variables in a scope.
  """

  alias Ilox.Token

  @type t :: %__MODULE__{
          values: %{required(Token.t()) => term()},
          print: (IO.chardata() | String.Chars.t() -> any())
        }

  defstruct values: %{}, print: &IO.puts/1

  def new, do: %__MODULE__{}

  def define(%__MODULE__{} = env, %Token{type: :identifier} = name, value) do
    {Map.put(env, name, value), nil}
  end

  def assign(%__MODULE__{} = env, %Token{type: :identifier} = name, value) do
    if Map.has_key?(env.values, name) do
      {Map.put(env, name, value), value}
    else
      raise Ilox.RuntimeError, undefined_variable(name)
    end
  end

  def get(%__MODULE__{} = env, %Token{type: :identifier} = name) do
    if Map.has_key?(env.values, name) do
      {env, Map.get(env.values, name)}
    else
      raise Ilox.RuntimeError, undefined_variable(name)
    end
  end

  defp undefined_variable(%Token{} = name),
    do: [token: name, message: "Undefined variable '#{name.lexeme}'."]
end
