defmodule Lox.Interpreter.Token do
  @moduledoc """
  Scanning token types
  """

  @type lx_standalone ::
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

  @type lx_compound ::
          :bang
          | :bang_equal
          | :equal
          | :equal_equal
          | :greater
          | :greater_equal
          | :less
          | :less_equal

  @type lx_literal :: :identifier | :string | :number

  @type lx_keyword ::
          :and
          | :class
          | :else
          | false
          | :fun
          | :for
          | :if
          | nil
          | :or
          | :print
          | :return
          | :super
          | :this
          | true
          | :var
          | :while

  @type lx_terminal :: :eof

  @type t :: %__MODULE__{
          type: lx_standalone | lx_compound | lx_literal | lx_keyword | lx_terminal,
          lexeme: binary(),
          literal: term(),
          line: pos_integer()
        }

  @derive [Inspect]

  defstruct [:type, :lexeme, :literal, :line]

  def new(type, lexeme, literal, line),
    do: %__MODULE__{type: type, lexeme: lexeme, literal: literal, line: line}

  def to_string(%__MODULE__{type: type, lexeme: lexeme, literal: literal, line: line}) do
    "#{line}: #{type} \"#{lexeme}\" #{literal}"
  end

  defimpl String.Chars do
    def to_string(%mod{} = value), do: mod.to_string(value)
  end
end
