defmodule Ilox.AstPrinter do
  alias Ilox.Token

  def print(expr) do
    expr
    |> as_string()
    |> IO.puts()
  end

  @spec as_string(Ilox.expr()) :: binary()
  def as_string({:binary_expr, left, %Token{lexeme: lexeme}, right}),
    do: parenthesize([lexeme, left, right])

  def as_string({:group_expr, expr}), do: parenthesize(["group", expr])

  def as_string({:literal_expr, nil}), do: "nil"
  def as_string({:literal_expr, %{literal: literal}}), do: literal

  def as_string({:unary_expr, %Token{lexeme: lexeme}, right}), do: parenthesize([lexeme, right])

  def as_string({:print_stmt, expr}), do: parenthesize(["print", expr])

  def as_string({:expr_stmt, expr}), do: parenthesize(["expr", expr])

  def as_string(expr) do
    raise "Unsupported expr #{inspect(expr)}"
  end

  defp parenthesize([name | exprs]) do
    exprs =
      exprs
      |> Enum.map(&as_string/1)
      |> Enum.join(" ")

    "(#{name} #{exprs})"
  end
end
