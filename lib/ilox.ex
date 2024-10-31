defmodule Ilox do
  @moduledoc """
  This is an Elixir implementation of [`jlox`][3] using a tree-walk interpreter, called
  `ilox`.

  [3]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
  """

  alias Ilox.AstPrinter
  alias Ilox.Context
  alias Ilox.Interpreter
  alias Ilox.Parser
  alias Ilox.Scanner
  alias Ilox.Token

  @type expr :: literal | unary_op | binary_op | group
  @type binary_op :: {:binary, left :: expr, operator :: Token.t(), right :: expr}
  @type literal :: {:literal, value :: number() | binary() | true | false | nil}
  @type unary_op :: {:unary, operator :: Token.t(), right :: expr}
  @type group :: {:group, expr :: expr}

  defdelegate add_error(message), to: Context

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

  def scan(source) when is_binary(source), do: Scanner.scan_tokens(source)

  def parse(tokens) when is_list(tokens), do: Parser.parse(tokens)

  def parse(tokenable) do
    with {:ok, tokens} <- scan(tokenable) do
      parse(tokens)
    end
  end

  def print_ast(expr) when is_tuple(expr), do: AstPrinter.print(expr)

  def print_ast(astable) do
    with {:ok, expr} <- parse(astable) do
      print_ast(expr)
    end
  end

  def interpret(source) when is_binary(source) do
    with {:ok, expr} <- parse(source) do
      Interpreter.interpret(expr)
    end
  end

  defp run_script_file(path) do
    case run_ilox(File.read!(path)) do
      {:error, :scanner} -> System.halt(65)
      {:error, :parser} -> System.halt(66)
      {:error, :interpreter} -> System.halt(70)
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

        source ->
          case run_ilox(source) do
            {:ok, result} -> IO.puts("==> #{result}")
            {:error, _} -> nil
          end

          # Clear the context on each line, for now.
          Context.clear()
          :cont
      end

    if result == :cont, do: run_repl()
  end

  defp run_ilox(source) do
    with {:ok, tokens} <- Scanner.scan_tokens(source),
         {:ok, expr} <- Parser.parse(tokens),
         {:ok, result} <- Interpreter.interpret(expr) do
      {:ok, result}
    else
      e -> show_ilox_error(e)
    end
  end

  defp show_ilox_error({:error, type, nil}) do
    IO.puts(:stderr, "Unknown error!")
    {:error, type}
  end

  defp show_ilox_error({:error, type, []}) do
    IO.puts(:stderr, "Unknown error!")
    {:error, type}
  end

  defp show_ilox_error({:error, type, error}) when is_binary(error) do
    IO.puts(:stderr, error)
    {:error, type}
  end

  defp show_ilox_error({:error, type, [error]}) do
    IO.puts(:stderr, error)
    {:error, type}
  end

  defp show_ilox_error({:error, type, errors}) when is_list(errors) do
    IO.puts(:stderr, Enum.join(errors, "\n"))
    {:error, type}
  end
end
