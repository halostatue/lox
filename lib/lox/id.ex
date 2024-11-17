defmodule Lox.Id do
  @moduledoc """
  A source for randomized string identifiers.
  """

  @type t :: String.t()

  @default_options count: 3, size: 3, separator: "-"

  def new(options \\ []) do
    options = Keyword.merge(@default_options, options)
    generator = generator(Keyword.get(options, :size))
    count = Keyword.get(options, :count)
    separator = Keyword.get(options, :separator)

    Stream.repeatedly(generator)
    |> Enum.take(count)
    |> Enum.join(separator)
  end

  @consonants [?b, ?f, ?g, ?k, ?n, ?p, ?r, ?s, ?t, ?v, ?x, ?z]
  @vowels [?a, ?e, ?i, ?o, ?u]

  defp generator(size) do
    fn ->
      Enum.into(1..size, "", fn _ -> <<Enum.random(@consonants), Enum.random(@vowels)>> end)
    end
  end
end
