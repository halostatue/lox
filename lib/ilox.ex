defmodule Ilox do
  @moduledoc """
  This is an Elixir implementation of [`jlox`][3] using a tree-walk interpreter, called
  `ilox`.

  [3]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
  """

  alias Ilox.Ast
  alias Ilox.Context
  alias Ilox.Parser
  alias Ilox.Scanner
  alias Ilox.Token

  @type expr :: literal | unary_op | binary_op | group
  @type binary_op :: {:binary, left :: expr, operator :: Token.t(), right :: expr}
  @type literal :: {:literal, value :: number() | binary() | true | false | nil}
  @type unary_op :: {:unary, operator :: Token.t(), right :: expr}
  @type group :: {:group, expr :: expr}

  def run, do: run([])

  def run([script]), do: run_script_file(script)

  def run([]) do
    IO.puts("Starting ilox REPL. Use '\\q' to quit.")
    run_repl()
  end

  def run([_ | _]) do
    IO.puts("Usage: ilox [script]")
    System.halt(64)
  end

  def run_string(string), do: run_ilox(string)

  def error(code_context, message, where \\ nil) do
    %{errors: [last_message | _]} = Context.add_error(nil, code_context, message, where)
    IO.puts(:stderr, last_message)
  end

  defp run_script_file(path) do
    case run_ilox(File.read!(path)) do
      :error -> System.halt(65)
      {:ok, _result} -> :ok
    end
  end

  defp run_repl do
    IO.write("> ")

    result =
      case IO.read(:stdio, :line) do
        :eof ->
          :halt

        {:error, reason} ->
          IO.puts(:stderr, "Input error: #{reason}")
          :cont

        "\\q" <> _ ->
          IO.puts("Thanks for flying ilox.")
          :halt

        "\n" ->
          :cont

        data ->
          if ilox_result = run_ilox(data), do: IO.puts("==> #{ilox_result}")

          # Clear the context on each line, for now.
          Context.clear()
          :cont
      end

    if result == :cont, do: run_repl()
  end

  defp run_ilox(source) do
    with {:ok, tokens} <- Scanner.scan_tokens(source),
         {:ok, expr} <- Parser.parse(tokens) do
      Ast.print(expr)
      nil
    else
      :error -> run_ilox_error()
      {:error, _} -> run_ilox_error()
    end
  end

  defp run_ilox_error(message \\ nil) do
    IO.write(:stder, "Error")

    if message, do: IO.puts(:stderr, ": " <> message), else: IO.puts(:stderr, "!")

    :error
  end
end
