defmodule Ilox.SourceTools do
  @moduledoc false

  alias Ilox.Env
  alias Ilox.Interpreter
  alias Ilox.Parser
  alias Ilox.Resolver
  alias Ilox.Scanner

  def lox_files(patterns) do
    for {prefix, files} <- patterns do
      files =
        files
        |> List.wrap()
        |> Enum.join(",")

      ["test/upstream", prefix, "{#{files}}.lox"]
      |> Path.join()
      |> Path.wildcard()
    end
    |> List.flatten()
    |> Enum.map(&{&1, String.replace_prefix(&1, "test/upstream/", "")})
  end

  def load_lox(filename) do
    file =
      filename
      |> Path.expand()
      |> File.read!()

    matches =
      ~r{(?://\sexpect:\s(.+))|(?://\sexpect\sruntime\s([Ee]rror.+))}
      |> Regex.scan(file, capture: :all_but_first)
      |> List.flatten()
      |> Enum.map(&String.replace(&1, "<nil>", ""))
      |> Enum.map(&String.replace(&1, "<lf>", "\n"))
      |> Enum.group_by(&(!String.match?(&1, ~r{(?:\[line \d+\] )?[Ee]rror})))

    {file, Map.get(matches, true), Map.get(matches, false)}
  end

  def scan(source, replacements \\ []), do: Scanner.scan(replace_in_source(source, replacements))

  def parse(source, replacements \\ []), do: Parser.parse(replace_in_source(source, replacements))

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
