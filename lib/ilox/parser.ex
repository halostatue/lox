defmodule Ilox.Parser do
  @moduledoc """
  The parser adapts the `m:Ilox#context-free-grammar` into one that encodes its precedence
  rules. The parser produces an AST that is a mix of the context free grammar for
  expressions and the parsing grammar for statements.

  #### Parsing Grammar

  ```
  expression  →  assignment ;
  assignment  →  IDENTIFIER "=" assignment | equality ;
  equality    →  comparison ( ( "!=" | "==" ) comparison )* ;
  comparison  →  term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  term        →  factor ( ( "-" | "+" ) factor )* ;
  factor      →  unary ( ( "/" | "*" ) unary )* ;
  unary       →  ( ( "!" | "-" ) unary ) | primary ;
  variable    →  IDENTIFIER ;
  primary     →  "true" | "false" | "nil"
              |  NUMBER | STRING
              |  "(" expression ")"
              |  IDENTIFIER ;
  program     →  ( declaration )* ;
  declaration →  var_decl | statement ;
  statement   →  expr_stmt | print_stmt ;
  var_decl    →  "var" IDENTIFIER ( "=" expression )? ";" ;
  ```
  """

  alias Ilox.Scanner
  alias Ilox.Token

  @typedoc section: :pgrammar
  @typedoc """
  A (possibly empty) list of `t:declaration/0`.
  """
  @type program :: list(declaration)

  @typedoc section: :pgrammar
  @typedoc """
  A `t:var_decl/0` or `t:statement/0`.
  """
  @type declaration :: var_decl | statement

  @typedoc section: :pgrammar
  @typedoc """
  A variable declaration with an optional initializer.
  """
  @type var_decl :: {:var_decl, identifier :: Token.t(), initializer :: Ilox.expr() | nil}

  @typedoc section: :pgrammar
  @typedoc """
  Supported program statements. All statement types, except blocks, require a terminal
  semicolon (`;`) to be valid.
  """
  @type statement :: expr_stmt | print_stmt

  @typedoc section: :pgrammar
  @typedoc """
  An expression statement. The expression is evaluated, but is essentially side-effect
  free.
  """
  @type expr_stmt :: {:expr_stmt, expr :: Ilox.expr()}

  @typedoc section: :pgrammar
  @typedoc """
  A statement that displays the result of the expression to standard output.
  """
  @type print_stmt :: {:print_stmt, expr :: Ilox.expr()}

  @spec parse(tokens :: binary | list(Token.t())) :: list(program)
  def parse(source) when is_binary(source) do
    case Scanner.scan_tokens(source) do
      {:ok, tokens} -> parse(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse(tokens) do
    parse_declarations(tokens)
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, Exception.message(e)}
  end

  @spec parse(tokens :: binary | list(Token.t())) :: Ilox.expr()
  def parse_expr(source) when is_binary(source) do
    case Scanner.scan_tokens(source) do
      {:ok, tokens} -> parse_expr(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse_expr(tokens) do
    {expr, _tokens} = parse_expression(tokens)
    {:ok, expr}
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, Exception.message(e)}
  end

  defp parse_declarations([]), do: parse_declarations([Token.eof()])

  defp parse_declarations([%Token{type: :eof} = token | rest]) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", rest: rest
  end

  defp parse_declarations(tokens) do
    {statements, _tokens} = parse_declaration([], tokens)
    {:ok, Enum.reverse(statements)}
  end

  defp parse_declaration(statements, []), do: {statements, []}

  defp parse_declaration(statements, [%Token{type: :eof} | _]), do: {statements, []}

  defp parse_declaration(statements, [current | rest] = tokens) do
    if type_match(current, :var) do
      var_declaration(statements, rest)
    else
      parse_statement(statements, tokens)
    end
  rescue
    e in Ilox.ParserError ->
      # This should not be happening *here*.
      IO.puts(:stderr, Exception.message(e))
      parse_declaration(statements, synchronize(e.rest))
  end

  defp var_declaration(statements, tokens) do
    {name, [next | _] = tokens} = consume(tokens, :identifier, "Expect variable name.")

    {initializer, tokens} =
      if type_match(next, :equal) do
        parse_expression(tokens)
      else
        {nil, tokens}
      end

    {_, tokens} = consume(tokens, :semicolon, "Expect ';' after variable declaration.")
    parse_statement({:var_decl, name, initializer}, statements, tokens)
  end

  defp parse_statement(statements, [%Token{type: :eof}]), do: {statements, []}
  defp parse_statement(statements, []), do: {statements, []}

  defp parse_statement(statements, [%Token{type: :print} | tokens]) do
    {expr, tokens} = parse_expression(tokens)
    {_, tokens} = consume(tokens, :semicolon, "Expect ';' after value.")
    parse_statement({:print_stmt, expr}, statements, tokens)
  end

  defp parse_statement(statements, tokens) do
    {expr, tokens} = parse_expression(tokens)
    {_, tokens} = consume(tokens, :semicolon, "Expect ';' after expression.")
    parse_statement({:expr_stmt, expr}, statements, tokens)
  end

  defp parse_statement(stmt, statements, tokens), do: parse_statement([stmt | statements], tokens)

  defp parse_expression([]), do: parse_expression([Token.eof()])

  defp parse_expression([%Token{type: :eof} = token | rest]) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", rest: rest
  end

  defp parse_expression(tokens), do: assignment(tokens)

  defp assignment(tokens) do
    {expr, [head | _] = tokens} = equality(tokens)

    if type_match(head, :equal) do
      {equals, tokens} = advance(tokens)
      {value, tokens} = assignment(tokens)

      case expr do
        {:var_expr, name} ->
          {{:assignment_expr, name, value}, tokens}

        _ ->
          raise Ilox.ParserError,
            token: equals,
            message: "Invalid assignment target.",
            rest: tokens
      end
    else
      {expr, tokens}
    end
  end

  defp binary_consume_match(expr, [], _types, _consumer), do: {expr, []}

  defp binary_consume_match(expr, [current | _] = tokens, types, consumer) do
    if type_match(current, types) do
      {operator, tokens} = advance(tokens)

      case consumer.(tokens) do
        {nil, []} ->
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

  defp primary([%Token{type: :identifier} = current | tokens]), do: {{:var_expr, current}, tokens}

  defp primary([%Token{type: :left_paren} | tokens]) do
    {expr, tokens} = parse_expression(tokens)
    {_, tokens} = consume(tokens, :right_paren, "Expect ')' after expression.")
    {{:group_expr, expr}, tokens}
  end

  defp primary([current | rest]) do
    raise Ilox.ParserError, token: current, message: "Expect expression.", rest: rest
  end

  defp consume([%Token{type: type} = current | tokens], type, _message),
    do: {current, tokens}

  defp consume([token | rest], _type, message) do
    raise Ilox.ParserError, token: token, message: message, where: error_where(token), rest: rest
  end

  defp synchronize(tokens) do
    case advance(tokens) do
      {nil, []} ->
        []

      {previous, [current | _] = tokens} ->
        cond do
          current.type == :eof -> tokens
          previous.type == :semicolon -> tokens
          current.type in [:class, :fun, :var, :for, :if, :while, :print, :return] -> tokens
          true -> synchronize(tokens)
        end
    end
  end

  def error_where(%{type: :eof}), do: "at end"
  def error_where(%{lexeme: lexeme}), do: "at '#{lexeme}'"

  defp type_match(%Token{type: :eof}, _types), do: false
  defp type_match(%Token{type: type}, types) when is_list(types), do: type in types
  defp type_match(%Token{type: type}, type), do: true
  defp type_match(%Token{}, _type), do: false

  defp advance([]), do: {nil, []}
  defp advance([%Token{} = previous, %Token{type: :eof} | _]), do: {previous, []}
  defp advance([%Token{type: :eof} | _]), do: {nil, []}
  defp advance([%Token{} = previous | rest]), do: {previous, rest}
end
