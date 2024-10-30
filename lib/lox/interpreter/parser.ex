defmodule Lox.Interpreter.Parser do
  @moduledoc """
  We don't need a visitor pattern, we have pattern matching.
  """

  alias Lox.Interpreter.Token

  def parse(tokens) do
    expr = expression(tokens)

    {:ok, expr}
  rescue
    _e -> {:error, nil}
  end

  # defp
  @spec expression(tokens :: list(Token.t())) :: Lox.expr()
  defp expression(tokens) when is_list(tokens) do
    {expr, _tokens} =
      equality(tokens)

    expr
  end

  defp equality(tokens) do
    {expr, tokens} =
      comparison(tokens)

    equality_consume_match(expr, tokens, [:bang_equal, :equal_equal])
  end

  defp equality_consume_match(expr, [], _types), do: {expr, []}

  defp equality_consume_match(expr, [current | _] = tokens, types) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)
      {right, tokens} = comparison(tokens)
      expr = {:binary, expr, operator, right}
      equality_consume_match(expr, tokens, types)
    else
      {expr, tokens}
    end
  end

  defp comparison(tokens) do
    {expr, tokens} = term(tokens)
    comparison_consume_match(expr, tokens, [:greater, :greater_equal, :less, :less_equal])
  end

  defp comparison_consume_match(expr, [current | _] = tokens, types) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)
      {right, tokens} = term(tokens)
      expr = {:binary, expr, operator, right}
      comparison_consume_match(expr, tokens, types)
    else
      {expr, tokens}
    end
  end

  defp term(tokens) do
    result = factor(tokens)
    {expr, tokens} = result
    term_consume_match(expr, tokens, [:minus, :plus])
  end

  defp term_consume_match(expr, [current | _] = tokens, types) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)
      {right, tokens} = factor(tokens)
      expr = {:binary, expr, operator, right}
      term_consume_match(expr, tokens, types)
    else
      {expr, tokens}
    end
  end

  defp factor(tokens) do
    result = unary(tokens)
    {expr, tokens} = result
    factor_consume_match(expr, tokens, [:slash, :star])
  end

  defp factor_consume_match(expr, [current | _] = tokens, types) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)
      {right, tokens} = unary(tokens)
      expr = {:binary, expr, operator, right}
      factor_consume_match(expr, tokens, types)
    else
      {expr, tokens}
    end
  end

  defp unary([current | _] = tokens) do
    if type_match(current, [:bang, :minus]) do
      {operator, tokens} = advance(tokens)
      {right, tokens} = unary(tokens)
      {{:unary, operator, right}, tokens}
    else
      primary(tokens)
    end
  end

  defp primary([%Token{type: :eof} | _]), do: {nil, []}
  defp primary([%Token{type: :Qfalse} | tokens]), do: {{:literal, false}, tokens}
  defp primary([%Token{type: :Qnil} | tokens]), do: {{:literal, nil}, tokens}
  defp primary([%Token{type: :Qtrue} | tokens]), do: {{:literal, true}, tokens}

  defp primary([%Token{type: type} = current | tokens]) when type in [:number, :string],
    do: {{:literal, current}, tokens}

  defp primary([%Token{type: :left_paren} | tokens]) do
    {expr, tokens} = expression(tokens)
    {{:group, expr}, consume_next(tokens, :right_paren, "Expect ')' after expression.")}
  end

  defp primary([current | _]) do
    raise error(current.line, "", "Expect expression.")
  end

  defp consume_next([%{type: type} | tokens], type, _message), do: tokens

  defp consume_next([%{type: :eof} = token | _], _type, message) do
    raise error(token.line, " at end", message)
  end

  defp consume_next([token | _], _type, message) do
    raise error(token.line, " at '#{token.lexeme}'", message)
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

  defp error(line, where, message) do
    message = "[#{line}] Error#{where}: #{message}"

    IO.puts(:stderr, message)

    raise message
  end

  defp type_match(%Token{type: :eof}, _types), do: false
  defp type_match(%Token{type: type}, types), do: type in types

  defp advance([%Token{} = previous, %Token{type: :eof} | _]), do: {previous, []}
  defp advance([%Token{type: :eof} | _]), do: {nil, []}
  defp advance([%Token{} = previous | rest]), do: {previous, rest}
end
