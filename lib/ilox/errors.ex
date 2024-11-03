defmodule Ilox.Errors do
  @moduledoc false

  def format(%{token: %{line: line}, message: message, where: nil}),
    do: format(line, message, nil)

  def format(%{token: %{line: line}, message: message, where: where}),
    do: format(line, message, where)

  def format(line, message, where \\ nil)

  def format(%{line: line}, message, where), do: format(line, message, where)

  def format(line, message, nil), do: "[line #{line}] Error: #{message}"

  def format(line, message, where), do: "[line #{line}] Error #{where}: #{message}"
end

defmodule Ilox.RuntimeError do
  defexception [:token, :message]
  defdelegate message(e), to: Ilox.Errors, as: :format
end

defmodule Ilox.ParserError do
  defexception [:token, :message, rest: [], where: nil]
  defdelegate message(e), to: Ilox.Errors, as: :format
end
