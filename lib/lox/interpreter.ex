defmodule Lox.Interpreter do
  @moduledoc """
  This is the namespace for implementing the tree-walk interpreter version of Lox,
  nominally called `jlox`.
  """

  alias Lox.Interpreter.Scanner

  def main([script]), do: run_file(script)

  def main([_ | _]) do
    IO.puts("Usage: jlox [script]")
    System.halt(64)
  end

  def main([]) do
    IO.puts("Starting Lox REPL. Use '\\q' to quit.")
    run_prompt()
  end

  def error(context, message), do: report(context, "", message)

  defp run_file(path) do
    case run(File.read!(path)) do
      :error -> System.halt(65)
      {:ok, _result} -> :ok
    end
  end

  defp run_prompt do
    IO.write("> ")

    case IO.read(:stdio, :line) do
      :eof ->
        :ok

      {:error, reason} ->
        IO.puts("Error: #{reason}")
        run_prompt()

      "\\q" <> _ ->
        :ok

      "\n" ->
        run_prompt()

      data ->
        run(data)
        run_prompt()
    end
  end

  defp run(source) do
    # Just print the tokens for now
    case Scanner.scan_tokens(source) do
      {:ok, tokens} ->
        Enum.each(tokens, &IO.puts/1)
        {:ok, nil}

      {:error, _error} ->
        IO.puts("Error!")
        :error
    end
  end

  defp report(context, where, message) do
    IO.puts(:stderr, "[#{context.line}] Error#{where}: #{message}")
    %{context | error: true}
  end
end
