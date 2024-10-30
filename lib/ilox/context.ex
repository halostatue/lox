defmodule Ilox.Context do
  @moduledoc """
  This is an ilox execution context. This is mostly used to capture scanning, parsing, or
  execution errors.
  """

  defstruct error?: false, errors: []

  @ilox_context :"$ilox_context"

  def new, do: %__MODULE__{}

  def clear, do: Process.delete(@ilox_context)

  def format_error(code_context, message, where \\ nil) do
    where =
      if String.starts_with?(where, " ") do
        where
      else
        " " <> where
      end

    "[#{code_context.line}] Error#{where}: #{message}"
  end

  def all_errors(ctx \\ nil) do
    case ctx || get_context() do
      nil -> []
      %{errors: errors} -> Enum.reverse(errors)
    end
  end

  def add_error(ctx \\ nil, code_context, message, where \\ nil)

  def add_error(nil, code_context, message, where) do
    get_context()
    |> add_error(code_context, message, where)
    |> put_context()
  end

  def add_error(ctx, code_context, message, where), do: report(ctx, code_context, message, where)

  defp report(%__MODULE__{} = ctx, code_context, message, where) do
    report_message = format_error(code_context, message, where)
    %{ctx | error?: true, errors: [report_message | ctx.errors]}
  end

  defp get_context, do: Process.get(@ilox_context)

  defp put_context(%__MODULE__{} = ctx) do
    Process.put(@ilox_context, ctx)
    ctx
  end
end
