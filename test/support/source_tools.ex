defmodule Ilox.SourceTools do
  @moduledoc false

  alias Ilox.Env
  alias Ilox.Interpreter
  alias Ilox.Parser
  alias Ilox.Resolver

  def run(source, replacements \\ []) do
    case Interpreter.run(Env.new(print: &print/1), replace_in_source(source, replacements)) do
      :ok ->
        {:ok, output: Enum.reverse(Process.get(:"$ilox$output", []))}

      {:error, type, errors} ->
        {:error, type, errors: errors, output: Enum.reverse(Process.get(:"$ilox$output", []))}
    end
  after
    Process.delete(:"$ilox$output")
  end

  def resolve(source, replacements \\ []) do
    with {:ok, statements} <- Parser.parse(replace_in_source(source, replacements)),
         %{} = result <- Resolver.resolve(Env.new(), statements, true) do
      locals =
        result.locals
        |> Map.to_list()
        |> Enum.sort()

      %{result | locals: locals}
    end
  end

  defp replace_in_source(source, []), do: source

  defp replace_in_source(source, [{key, value} | rest]),
    do: replace_in_source(String.replace(source, inspect(key), value), rest)

  defp print(message) do
    queue = Process.get(:"$ilox$output", [])
    queue = [message | queue]
    Process.put(:"$ilox$output", queue)
  end
end
