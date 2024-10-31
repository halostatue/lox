defmodule Ilox.Parser do
  @moduledoc """
  We don't need a visitor pattern, we have pattern matching.
  """

  alias Ilox.Token

  def parse(tokens) do
    parse_statements(tokens)
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, Exception.message(e)}
  end

  def parse_expr(tokens) do
    {expr, _tokens} = parse_expression(tokens)
    {:ok, expr}
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, Exception.message(e)}
  end

  @spec parse_statements(tokens :: list(Token.t())) :: list(Ilox.stmt())
  defp parse_statements([]), do: parse_statements([Token.eof()])

  defp parse_statements([%Token{type: :eof} = token | _]) do
    raise Ilox.ParserError, token: token, message: "Expect expression."
  end

  defp parse_statements(tokens) do
    {statements, _tokens} = parse_statement([], tokens)
    {:ok, Enum.reverse(statements)}
  end

  defp parse_statement(statements, []), do: {statements, []}

  defp parse_statement(statements, [%Token{type: :eof} | _]), do: {statements, []}

  defp parse_statement(statements, [%Token{type: :print} | tokens]) do
    {expr, tokens} = parse_expression(tokens)
    tokens = consume_next(tokens, :semicolon, "Expect ';' after value.")
    parse_statement({:print_stmt, expr}, statements, tokens)
  end

  defp parse_statement(statements, tokens) do
    {expr, tokens} = parse_expression(tokens)
    tokens = consume_next(tokens, :semicolon, "Expect ';' after expression.")
    parse_statement({:expr_stmt, expr}, statements, tokens)
  end

  defp parse_statement(stmt, statements, tokens), do: parse_statement([stmt | statements], tokens)

  @spec parse_expression(tokens :: list(Token.t())) :: Ilox.expr()
  defp parse_expression([]), do: parse_expression([Token.eof()])

  defp parse_expression([%Token{type: :eof} = token | _]) do
    raise Ilox.ParserError, token: token, message: "Expect expression."
  end

  defp parse_expression(tokens), do: equality(tokens)

  defp binary_consume_match(expr, [], _types, _consumer), do: {expr, []}

  defp binary_consume_match(expr, [current | _] = tokens, types, consumer) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)

      case consumer.(tokens) |> dbg() do
        {nil, []} ->
          IO.inspect(error_where(current))

          raise Ilox.ParserError,
            token: current,
            message: "Expect right-hand expression.",
            where: error_where(current)

        {right, tokens} ->
          expr = {:binary_expr, expr, operator, right}
          binary_consume_match(expr, tokens, types, consumer)
      end
    else
      {expr, tokens}
    end
  end

  defp equality(tokens) do
    {expr, tokens} = comparison(tokens)
    binary_consume_match(expr, tokens, [:bang_equal, :equal_equal], &comparison/1)
  end

  defp comparison(tokens) do
    {expr, tokens} = term(tokens)
    binary_consume_match(expr, tokens, [:greater, :greater_equal, :less, :less_equal], &term/1)
  end

  defp term(tokens) do
    result = factor(tokens)
    {expr, tokens} = result
    binary_consume_match(expr, tokens, [:minus, :plus], &factor/1)
  end

  defp factor(tokens) do
    {expr, tokens} = unary(tokens)
    binary_consume_match(expr, tokens, [:slash, :star], &unary/1)
  end

  defp unary([]), do: {nil, []}

  defp unary([current | _] = tokens) do
    if type_match(current, [:bang, :minus]) do
      {operator, tokens} = advance(tokens)

      case(unary(tokens)) do
        {nil, []} ->
          raise Ilox.ParserError,
            token: current,
            message: "Expect right-hand expression.",
            where: error_where(current)

        {right, tokens} ->
          {{:unary_expr, operator, right}, tokens}
      end
    else
      primary(tokens)
    end
  end

  defp primary([]), do: {nil, []}
  defp primary([%Token{type: :eof} | _]), do: {nil, []}
  defp primary([%Token{type: :Qfalse} | tokens]), do: {{:literal_expr, false}, tokens}
  defp primary([%Token{type: :Qnil} | tokens]), do: {{:literal_expr, nil}, tokens}
  defp primary([%Token{type: :Qtrue} | tokens]), do: {{:literal_expr, true}, tokens}

  defp primary([%Token{type: type} = current | tokens]) when type in [:number, :string],
    do: {{:literal_expr, current}, tokens}

  defp primary([%Token{type: :left_paren} | tokens]) do
    {expr, tokens} = parse_expression(tokens)
    {{:group_expr, expr}, consume_next(tokens, :right_paren, "Expect ')' after expression.")}
  end

  defp primary([current | _]) do
    raise Ilox.ParserError, token: current, message: "Expect expression."
  end

  defp consume_next([%{type: type} | tokens], type, _message), do: tokens

  defp consume_next([token | _], _type, message) do
    raise Ilox.ParserError, token: token, message: message, where: error_where(token)
  end

  # Call after catching the error from consume_next
  # defp
  def synchronize(tokens) do
    {previous, [current | _] = tokens} = advance(tokens)

    cond do
      current.type == :eof -> tokens
      previous.type == :semicolon -> tokens
      current.type in [:class, :fun, :var, :for, :if, :while, :print, :return] -> tokens
      true -> synchronize(tokens)
    end
  end

  def error_where(%{type: :eof}), do: "at end"
  def error_where(%{lexeme: lexeme}), do: "at '#{lexeme}'"

  defp type_match(%Token{type: :eof}, _types), do: false
  defp type_match(%Token{type: type}, types), do: type in types

  defp advance([%Token{} = previous, %Token{type: :eof} | _]), do: {previous, []}
  defp advance([%Token{type: :eof} | _]), do: {nil, []}
  defp advance([%Token{} = previous | rest]), do: {previous, rest}
end
