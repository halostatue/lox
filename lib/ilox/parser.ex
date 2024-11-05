defmodule Ilox.ParserError do
  defexception [:token, :message, :ctx, where: nil]
  defdelegate message(e), to: Ilox.Errors, as: :format
end

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

  alias Ilox.Errors
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
  @type statement :: expr_stmt | print_stmt | block

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

  @typedoc section: :pgrammar
  @typedoc """
  A list of statements or declarations wrapped in curly braces.
  """
  @type block :: {:block, list(declaration)}

  @spec parse(tokens :: String.t() | list(Token.t())) :: list(program)
  def parse(source) when is_binary(source) do
    case Scanner.scan(source) do
      {:ok, tokens} -> parse(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse(tokens) when is_list(tokens), do: handle_program(tokens)

  @spec parse_expr(tokens :: String.t() | list(Token.t())) :: Ilox.expr()
  def parse_expr(source) when is_binary(source) do
    case Scanner.scan(source) do
      {:ok, tokens} -> parse_expr(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse_expr(tokens) do
    {expr, _tokens} = handle_expression(%{tokens: tokens})
    {:ok, expr}
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, Exception.message(e)}
  end

  defp handle_program(tokens) when is_list(tokens) do
    case handle_program(%{errors: [], statements: [], tokens: tokens, scopes: []}) do
      %{errors: [], statements: []} ->
        raise "This should be unreachable. No errors and no statements returned."

      %{errors: [_ | _] = errors} ->
        {:error, :parser, Enum.reverse(errors)}

      %{statements: [_ | _] = statements} ->
        {:ok, Enum.reverse(statements)}
    end
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, [Exception.message(e)]}
  end

  defp handle_program(%{tokens: []} = ctx),
    do: handle_program(%{ctx | tokens: [Token.eof()]})

  defp handle_program(%{tokens: [%Token{type: :eof} = token | _]} = ctx) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", ctx: ctx
  end

  defp handle_program(%{} = ctx), do: handle_declaration(ctx)

  defp handle_declaration(%{tokens: []} = ctx), do: ctx

  defp handle_declaration(%{tokens: [%Token{type: :eof} | _]} = ctx), do: ctx

  defp handle_declaration(%{tokens: [current | tokens]} = ctx) do
    if type_match(current, :var) do
      handle_var_declaration(%{ctx | tokens: tokens})
    else
      handle_statement(ctx)
    end
  rescue
    e in Ilox.ParserError ->
      %{e.ctx | errors: [Exception.message(e) | e.ctx.errors]}
      |> synchronize()
      |> handle_declaration()
  end

  defp handle_var_declaration(ctx) do
    {name, %{tokens: [next | tokens]} = ctx} = expect(ctx, :identifier, "Expect variable name.")

    {initializer, ctx} =
      if type_match(next, :equal) do
        handle_expression(%{ctx | tokens: tokens})
      else
        {nil, ctx}
      end

    {_, ctx} = expect_semicolon(ctx, "variable declaration")
    add_statement(ctx, {:var_decl, name, initializer})
  end

  defp handle_statement(%{tokens: []} = ctx), do: ctx
  defp handle_statement(%{tokens: [%Token{type: :eof} | _]} = ctx), do: ctx

  defp handle_statement(%{tokens: [%Token{type: :right_brace} | tokens]} = ctx),
    do: %{ctx | tokens: tokens}

  defp handle_statement(%{tokens: [%Token{type: :print} | tokens]} = ctx) do
    {expr, ctx} = handle_expression(%{ctx | tokens: tokens})
    {_, ctx} = expect_semicolon(ctx, "value")
    add_statement(ctx, {:print_stmt, expr})
  end

  defp handle_statement(%{tokens: [%Token{type: :left_brace} | tokens]} = ctx),
    do: handle_block(%{ctx | tokens: tokens})

  defp handle_statement(ctx) do
    {expr, ctx} = handle_expression(ctx)
    {_, ctx} = expect_semicolon(ctx, "expression")
    add_statement(ctx, {:expr_stmt, expr})
  end

  defp handle_block(ctx) do
    # Block handling is harder in an immutable environments. We need to explicitly
    # recognize the outer and inner contexts, because we have to stack the outer context
    # for recovery. The tokens are being transferred to the inner context, so clear the
    # outer context tokens. The inner context starts with empty statements.
    outer_ctx = %{ctx | tokens: []}
    inner_ctx = %{ctx | statements: [], scopes: [outer_ctx | ctx.scopes]}

    # Process the declarations inside the block.
    inner_ctx = handle_declaration(inner_ctx)

    # Pop the outer context from the scopes stack, and then set the tokens and errors
    # from the inner context to recognize what has been consumed. The existing statements
    # is already valid.
    [outer_ctx | _] = inner_ctx.scopes
    outer_ctx = %{outer_ctx | tokens: inner_ctx.tokens, errors: inner_ctx.errors}

    # Finally, add the block statement to the outer context to the outer context. However,
    # remember to *reverse* the block statements because we collect statements backwards.
    add_statement(outer_ctx, {:block, Enum.reverse(inner_ctx.statements)})
  end

  defp handle_expression(%{tokens: []} = ctx),
    do: handle_expression(%{ctx | tokens: [Token.eof()]})

  defp handle_expression(%{tokens: [%Token{type: :eof} = token | _]} = ctx) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", ctx: ctx
  end

  defp handle_expression(ctx), do: handle_assignment(ctx)

  defp handle_assignment(ctx) do
    {expr, %{tokens: [head | _]} = ctx} = handle_equality(ctx)

    if type_match(head, :equal) do
      {equals, ctx} = handle_advance(ctx)
      {value, ctx} = handle_assignment(ctx)

      case expr do
        {:var_expr, name} ->
          {{:assign_expr, name, value}, ctx}

        _ ->
          raise Ilox.ParserError,
            token: equals,
            message: "Invalid assignment target.",
            ctx: ctx
      end
    else
      {expr, ctx}
    end
  end

  defp handle_equality(ctx) do
    {expr, ctx} = handle_comparison(ctx)
    handle_binary_match(ctx, expr, [:bang_equal, :equal_equal], &handle_comparison/1)
  end

  defp handle_comparison(ctx) do
    {expr, ctx} = handle_term(ctx)

    handle_binary_match(
      ctx,
      expr,
      [:greater, :greater_equal, :less, :less_equal],
      &handle_term/1
    )
  end

  defp handle_term(ctx) do
    {expr, ctx} = handle_factor(ctx)
    handle_binary_match(ctx, expr, [:minus, :plus], &handle_factor/1)
  end

  defp handle_factor(ctx) do
    {expr, ctx} = handle_unary(ctx)
    handle_binary_match(ctx, expr, [:slash, :star], &handle_unary/1)
  end

  defp handle_unary(%{tokens: []} = ctx), do: {nil, ctx}
  defp handle_unary(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp handle_unary(%{tokens: [current | _]} = ctx) do
    if type_match(current, [:bang, :minus]) do
      {operator, ctx} = handle_advance(ctx)

      case handle_unary(ctx) do
        {nil, ctx} ->
          raise Ilox.ParserError,
            token: current,
            message: "Expect right-hand expression.",
            where: Errors.where(current),
            ctx: ctx

        {right, ctx} ->
          {{:unary_expr, operator, right}, ctx}
      end
    else
      handle_primary(ctx)
    end
  end

  defp handle_primary(%{tokens: []} = ctx), do: {nil, ctx}
  defp handle_primary(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp handle_primary(%{tokens: [%Token{type: :Qfalse} | tokens]} = ctx),
    do: {{:literal_expr, false}, %{ctx | tokens: tokens}}

  defp handle_primary(%{tokens: [%Token{type: :Qnil} | tokens]} = ctx),
    do: {{:literal_expr, nil}, %{ctx | tokens: tokens}}

  defp handle_primary(%{tokens: [%Token{type: :Qtrue} | tokens]} = ctx),
    do: {{:literal_expr, true}, %{ctx | tokens: tokens}}

  defp handle_primary(%{tokens: [%Token{type: type} = current | tokens]} = ctx)
       when type in [:number, :string],
       do: {{:literal_expr, current}, %{ctx | tokens: tokens}}

  defp handle_primary(%{tokens: [%Token{type: :identifier} = current | tokens]} = ctx),
    do: {{:var_expr, current}, %{ctx | tokens: tokens}}

  defp handle_primary(%{tokens: [%Token{type: :left_paren} | tokens]} = ctx) do
    {expr, ctx} = handle_expression(%{ctx | tokens: tokens})
    {_, ctx} = expect(ctx, :right_paren, "Expect ')' after expression.")
    {{:group_expr, expr}, ctx}
  end

  defp handle_primary(%{tokens: [current | _]} = ctx) do
    raise Ilox.ParserError, token: current, message: "Expect expression.", ctx: ctx
  end

  defp expect_semicolon(ctx, clause), do: expect(ctx, :semicolon, "Expect ';' after #{clause}.")

  defp expect(%{tokens: [%Token{type: type} = current | tokens]} = ctx, type, _message),
    do: {current, %{ctx | tokens: tokens}}

  defp expect(%{tokens: [token | _]} = ctx, _type, message) do
    raise Ilox.ParserError,
      token: token,
      message: message,
      where: Errors.where(token),
      ctx: ctx
  end

  defp handle_binary_match(%{tokens: []} = ctx, expr, _types, _consumer), do: {expr, ctx}

  defp handle_binary_match(%{tokens: [%Token{type: :eof} | _]} = ctx, expr, _types, _consumer),
    do: {expr, ctx}

  defp handle_binary_match(%{tokens: [current | _]} = ctx, expr, types, consumer) do
    if type_match(current, types) do
      {operator, ctx} = handle_advance(ctx)

      case consumer.(ctx) do
        {nil, ctx} ->
          raise Ilox.ParserError,
            token: current,
            message: "Expect right-hand expression.",
            where: Errors.where(current),
            ctx: ctx

        {right, ctx} ->
          expr = {:binary_expr, expr, operator, right}
          handle_binary_match(ctx, expr, types, consumer)
      end
    else
      {expr, ctx}
    end
  end

  defp add_statement(ctx, statement),
    do: handle_declaration(%{ctx | statements: [statement | ctx.statements]})

  defp synchronize(ctx) do
    case handle_advance(ctx) do
      {nil, ctx} ->
        ctx

      {previous, %{tokens: [current | _]} = ctx} ->
        cond do
          current.type == :eof -> ctx
          previous.type == :semicolon -> ctx
          current.type in [:class, :fun, :var, :for, :if, :while, :print, :return] -> ctx
          true -> synchronize(ctx)
        end
    end
  end

  defp type_match(%Token{type: :eof}, _types), do: false
  defp type_match(%Token{type: type}, types) when is_list(types), do: type in types
  defp type_match(%Token{type: type}, type), do: true
  defp type_match(%Token{}, _type), do: false

  defp handle_advance(%{tokens: []} = ctx), do: {nil, ctx}

  defp handle_advance(%{tokens: [%Token{} = previous, %Token{type: :eof} | _]} = ctx),
    do: {previous, %{ctx | tokens: [Token.eof(previous.line)]}}

  defp handle_advance(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp handle_advance(%{tokens: [%Token{} = previous | tokens]} = ctx),
    do: {previous, %{ctx | tokens: tokens}}
end
