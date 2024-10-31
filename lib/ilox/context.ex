defmodule Ilox.Context do
  @moduledoc """
  This is an ilox execution context. This is mostly used to capture scanning, parsing, or
  execution errors.
  """

  @ilox_context :"$ilox_context"

  def clear, do: Process.delete(@ilox_context)

  def add_error(ctx \\ nil, message)

  def add_error(nil, message) do
    get_context()
    |> add_error(message)
    |> put_context()
  end

  def add_error(ctx, exception) when is_struct(exception),
    do:
      add_error(ctx, Ilox.Errors.format(exception))
      |> dbg()

  def add_error(ctx, message) when is_binary(message), do: [message | ctx]

  def errors(ctx \\ nil) do
    case ctx || get_context() do
      nil -> []
      errors -> Enum.reverse(errors)
    end
  end

  defp get_context, do: Process.get(@ilox_context)

  defp put_context(ctx) when not is_nil(ctx) do
    Process.put(@ilox_context, ctx)
    ctx
  end
end
