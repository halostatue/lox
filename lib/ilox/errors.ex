defmodule Ilox.Errors do
  @moduledoc false

  def format(input) when is_list(input), do: format(Map.new(input))

  def format(%{token: token, message: message, where: nil}),
    do: format(line(token), message, nil)

  def format(%{token: token, message: message, where: where}),
    do: format(line(token), message, where)

  def format(%{token: token, message: message}),
    do: format(line(token), message, nil)

  def format(line, message, where \\ nil)

  def format(%{line: line}, message, where), do: format(line, message, where)

  def format(line, message, nil), do: "[line #{line}] Error: #{message}"

  def format(line, message, where), do: "[line #{line}] Error #{where}: #{message}"

  def where(%{type: :eof}), do: "at end"
  def where(%{lexeme: lexeme}), do: "at '#{lexeme}'"

  defp line(%{type: :eof, line: 1}), do: 1
  defp line(%{type: :eof, line: line}), do: line - 1
  defp line(%{line: line}), do: line
end

defmodule Ilox.RuntimeError do
  defexception [:token, :message, where: nil]
  defdelegate message(e), to: Ilox.Errors, as: :format
end
