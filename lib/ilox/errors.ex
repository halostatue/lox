defmodule Ilox.Errors do
  @moduledoc false

  def format(%{token: %{line: line}, message: message, where: nil}),
    do: format(line, message, nil)

  def format(%{token: %{line: line}, message: message, where: where}),
    do: format(line, message, where)

  def format(%{token: %{line: line}, message: message}),
    do: format(line, message, nil)

  def format(line, message, where \\ nil)

  def format(%{line: line}, message, where), do: format(line, message, where)

  def format(line, message, nil), do: "[line #{line}] Error: #{message}"

  def format(line, message, where), do: "[line #{line}] Error #{where}: #{message}"

  def where(%{type: :eof}), do: "at end"
  def where(%{lexeme: lexeme}), do: "at '#{lexeme}'"
end

defmodule Ilox.RuntimeError do
  defexception [:token, :message, where: nil]
  defdelegate message(e), to: Ilox.Errors, as: :format
end
