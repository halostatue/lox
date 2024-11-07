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
  defstruct [:type, :lexeme, :literal, :line]

  @type! t :: %__MODULE__{
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
    do: %__MODULE__{lexeme: lexeme, line: line, literal: literal, type: type}

  def eof(line \\ 0), do: new(:eof, line, "")

  @lookup %{
    :left_paren => "(",
    :right_paren => ")",
    :left_brace => "{",
    :right_brace => "}",
    :comma => ",",
    :dot => ".",
    :minus => "-",
    :plus => "+",
    :semicolon => ";",
    :slash => "/",
    :star => "*",
    :bang => "!",
    :bang_equal => "!=",
    :equal => "=",
    :equal_equal => "==",
    :greater => ">",
    :greater_equal => ">=",
    :less => "<",
    :less_equal => "<=",
    :and => "and",
    :class => "class",
    :else => "else",
    :Qfalse => "false",
    :fun => "fun",
    :for => "for",
    :if => "if",
    :Qnil => "nil",
    :or => "or",
    :print => "print",
    :return => "return",
    :super => "super",
    :this => "this",
    :Qtrue => "true",
    :var => "var",
    :while => "while"
  }

  def inspect_tokens(input, opts \\ [])

  def inspect_tokens({value, %{tokens: tokens} = ctx}, opts) do
    IO.inspect({value, view(tokens)}, opts)

    {value, ctx}
  end

  def inspect_tokens(%{tokens: tokens} = ctx, opts) do
    tokens
    |> view()
    |> IO.inspect(opts)

    ctx
  end

  def view(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(&view/1)
    |> Enum.join(" ")
  end

  # "Token.eof(#{line})"
  def view(%{type: :eof}), do: "␄"
  def view(%{type: :identifier, lexeme: lexeme}), do: lexeme
  def view(%{type: :string, literal: literal}), do: inspect(literal)
  def view(%{type: :number, literal: literal}), do: to_string(literal)

  def view(%{type: type} = token) do
    @lookup[type] || inspect(token)
  end

  defimpl Inspect do
    # "Token.eof(#{line})"
    def inspect(%{type: :eof, line: line}, _opts), do: "Token.eof(#{line})"

    def inspect(token, _opts) do
      params =
        Enum.map_join([:type, :line, :lexeme, :literal], ", ", &inspect(Map.get(token, &1)))

      "Token.new(#{params})"
    end
  end
end
