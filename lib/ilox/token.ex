defmodule Ilox.Token do
  @moduledoc """
  ilox scanner tokens, used for parsing later.
  """

  use TypeCheck

  @type! lox_standalone ::
           :left_paren
           | :right_paren
           | :left_brace
           | :right_brace
           | :comma
           | :dot
           | :minus
           | :plus
           | :semicolon
           | :slash
           | :star

  @type! lox_compound ::
           :bang
           | :bang_equal
           | :equal
           | :equal_equal
           | :greater
           | :greater_equal
           | :less
           | :less_equal

  @type! lox_literal :: :identifier | :string | :number

  @type! lox_keyword ::
           :and
           | :class
           | :else
           | :Qfalse
           | :fun
           | :for
           | :if
           | :Qnil
           | :or
           | :print
           | :return
           | :super
           | :this
           | :Qtrue
           | :var
           | :while

  @type! lox_terminal :: :eof

  @type! lox_tokens ::
           lox_standalone() | lox_compound() | lox_literal() | lox_keyword() | lox_terminal()

  @enforce_keys [:type, :lexeme, :line]
  defstruct [:type, :lexeme, :literal, :line, :id]

  @type! t :: %__MODULE__{
           id: nil | non_neg_integer(),
           type: lox_tokens(),
           line: non_neg_integer(),
           lexeme: String.t(),
           literal: term()
         }

  @spec! new(
           type :: lox_tokens(),
           line :: non_neg_integer(),
           lexeme :: binary(),
           literal :: term()
         ) :: t()
  def new(type, line, lexeme, literal \\ nil),
    do: %__MODULE__{
      lexeme: lexeme,
      line: line,
      literal: literal,
      type: type
    }

  def eof(line \\ 0), do: new(:eof, line, "")

  def __type_map do
    lox_tokens().type.choices
    |> Enum.map(&Map.from_struct(&1.type))
    |> Enum.flat_map(fn
      %{choices: choices} -> Enum.map(choices, & &1.value)
      %{value: value} -> [value]
    end)
    |> Enum.flat_map(fn t ->
      s =
        t
        |> Atom.to_string()
        |> String.replace_prefix("Q", "")

      [{s, t}, {String.upcase(s), t}]
    end)
    |> Map.new()
  end

  defimpl Inspect do
    def inspect(%{type: :eof, line: line}, _opts), do: "Token.eof(#{line})"

    def inspect(token, _opts) do
      params =
        Enum.map_join([:type, :line, :lexeme, :literal], ", ", &inspect(Map.get(token, &1)))

      "Token.new(#{params})"
    end
  end
end
