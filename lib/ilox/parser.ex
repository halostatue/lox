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
  assignment  →  IDENTIFIER "=" assignment | logic_or ;
  logic_or    →  logic_and ( "or" logic_and )* ;
  logic_and   →  equality ( "and" equality )* ;
  equality    →  comparison ( ( "!=" | "==" ) comparison )* ;
  comparison  →  term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  term        →  factor ( ( "-" | "+" ) factor )* ;
  factor      →  unary ( ( "/" | "*" ) unary )* ;
  unary       →  ( ( "!" | "-" ) unary ) | call ;
  call        →  primary  ( "(" arguments? ")" )* ;
  variable    →  IDENTIFIER ;
  primary     →  "true" | "false" | "nil"
              |  NUMBER | STRING
              |  "(" expression ")"
              |  IDENTIFIER ;
  arguments   →  expression ( "," expression )* ;
  program     →  ( declaration )* ;
  declaration →  fun_decl | var_decl | statement ;
  statement   →  expr_stmt
              | print_stmt
              | block
              | if_stmt
              | return_stmt
              | while_stmt ;
  fun_decl    →  "fun" function ;
  function    →  IDENTIFIER "(" parameters? ")" block ;
  parameters  →  IDENTIFIER ( "," IDENTIFIER )* ;
  var_decl    →  "var" IDENTIFIER ( "=" expression )? ";" ;
  if_stmt     →  "if" "(" expression ")" statement
                 ( "else" statement )? ;
  block       →  "{" declaration* "}" ;
  print_stmt  →  "print" expression ";" ;
  while_stmt  →  "while" "(" expression ")" statement ;
  for_stmt    →  "for" "(" ( var_decl | expr_stmt | ";" ) expression? ";" expression? ")"
                 statement ;
  return_stmt →  "return" expression? ";" ;
  expr_stmt   →  expression ";" ;
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
  A `t:fun_decl/0`, `t:var_decl/0`, or `t:statement/0`.
  """
  @type declaration :: var_decl | statement

  @typedoc section: :pgrammar
  @typedoc """
  A variable declaration with an optional initializer.
  """
  @type fun_decl ::
          {:function, name :: Token.t(), params :: list(Token.t()), arity :: non_neg_integer(),
           body :: list(statement)}

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
  @type statement :: expr_stmt | print_stmt | block | if_stmt | while_stmt | return_stmt

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

  @typedoc section: :pgrammar
  @typedoc """
  A conditional flow control statement made with an expression and a statement executed
  when the expression is truthy. It may optionally have a else statement executed when the
  expression is falsy.
  """
  @type if_stmt :: {:if_stmt, expr :: Ilox.expr(), truthy :: statement, falsy :: nil | statement}

  @typedoc section: :pgrammar
  @typedoc """
  A looping flow control statement that only executes if the expression is truthy.

  Robert Nystrom has implemented `for` loops as a desugaring into a while loop, so we will
  do the same.
  """
  @type while_stmt :: {:while_stmt, expr :: Ilox.expr(), body :: statement}

  @typedoc section: :pgrammar
  @typedoc """
  A looping flow control statement with three optional clauses inside parentheses,
  separated by semicolons and a body after the closing parenthesis.

  1. The _initializer_, executed exactly once before the loop starts. This may be an
     expression or a variable declaration. If there is a variable declaration, the
     variable is scoped to the for loop clauses and body.

  2. The _condition_, which controls when to exit the loop, evaluated at the beginning of
     each iteration. The body is executed if the condition is true.

  3. The _increment_, an arbitrary expression whose result is discarded. It must have
     a side effect to be useful (usually incrementing a variable).

  This will be desugared into a `while` loop.
  """
  @type for_stmt ::
          {:for_stmt, initializer :: var_decl | expr_stmt | nil, condition :: Ilox.expr() | nil,
           increment :: Ilox.expr() | nil, body :: statement}

  @typedoc section: :pgrammar
  @typedoc """
  Breaks the execution of a function and returns the result of the expression. If the
  expression is omitted, a `nil` value is returned.
  """
  @type return_stmt :: {:return_stmt, keyword :: Token.t(), value :: nil | Ilox.expr()}

  @typedoc section: :pgrammar
  @typedoc """
  A function call expression.

  This stores the closing parenthesis token to use the location to report errors on
  function call.
  """
  @type call ::
          {:call, callee :: Ilox.expr(), arguments :: list(Ilox.expr()),
           argc :: non_neg_integer(), closing :: Token.t()}

  @spec parse(tokens :: String.t() | list(Token.t())) :: list(program)
  def parse(source) when is_binary(source) do
    case Scanner.scan(source) do
      {:ok, tokens} -> parse(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse(tokens) when is_list(tokens), do: parse_program(tokens)

  @spec parse_expr(tokens :: String.t() | list(Token.t())) :: Ilox.expr()
  def parse_expr(source) when is_binary(source) do
    case Scanner.scan(source) do
      {:ok, tokens} -> parse_expr(tokens)
      {:error, :scanner, errors} -> {:error, :scanner, errors}
    end
  end

  def parse_expr(tokens) do
    {expr, ctx} = parse_expression(%{errors: [], tokens: tokens})

    case ctx do
      %{errors: [_ | _]} -> {:error, :parser, Enum.reverse(ctx.errors)}
      _ -> {:ok, expr}
    end
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, [Exception.message(e)]}
  end

  defp parse_program(tokens) when is_list(tokens) do
    ctx = %{errors: [], statements: [], tokens: tokens, scopes: [], break: 0}

    case parse_program(ctx) do
      %{errors: [], statements: []} ->
        raise "Invalid state: no errors and no statements."

      %{errors: [_ | _] = errors} ->
        {:error, :parser, Enum.reverse(errors)}

      %{statements: [_ | _] = statements} ->
        {:ok, Enum.reverse(statements)}
    end
  rescue
    e in Ilox.ParserError ->
      {:error, :parser, [Exception.message(e)]}
  end

  defp parse_program(%{tokens: []} = ctx),
    do: parse_program(%{ctx | tokens: [Token.eof()]})

  defp parse_program(%{tokens: [%Token{type: :eof} = token | _]} = ctx) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", ctx: ctx
  end

  defp parse_program(%{} = ctx), do: parse_declaration(ctx)

  defp parse_declaration(%{tokens: []} = ctx), do: ctx

  defp parse_declaration(%{tokens: [%Token{type: :eof} | _]} = ctx), do: ctx

  defp parse_declaration(%{tokens: [current | tokens]} = ctx) do
    if type_match(current, [:fun, :var]) do
      parse_declaration(current.type, %{ctx | tokens: tokens})
    else
      parse_statement(ctx)
    end
  rescue
    e in Ilox.ParserError ->
      %{e.ctx | errors: [Exception.message(e) | e.ctx.errors]}
      |> synchronize()
      |> parse_declaration()
  end

  defp parse_declaration(:var, ctx) do
    {name, %{tokens: [current | tokens]} = ctx} = expect_identifier(ctx, "variable")

    {initializer, ctx} =
      if type_match(current, :equal) do
        parse_expression(%{ctx | tokens: tokens})
      else
        {nil, ctx}
      end

    {_, ctx} = expect_semicolon(ctx, "variable declaration")
    add_statement(ctx, {:var_decl, name, initializer})
  end

  @callable_types %{
    class: "class",
    fun: "function",
    method: "method"
  }

  @callable_type_names Map.keys(@callable_types)

  defp parse_declaration(type, ctx) when type in @callable_type_names do
    kind = @callable_types[type]

    {name, ctx} = expect_identifier(ctx, kind)

    {_, ctx} = expect_left_paren(ctx, kind)
    {params, %{tokens: [current | _]} = ctx} = parse_decl_params(ctx)

    arity = Enum.count(params)
    ctx = check_arity(ctx, arity, current, "parameters")

    {_, ctx} = expect_right_paren(ctx, "parameters")
    {_, ctx} = expect(ctx, :left_brace, "Expect '{' before #{kind} body.")

    %{statements: [body | statements]} = ctx = parse_block(%{ctx | break: ctx.break + 1})

    add_statement(%{ctx | statements: statements}, {:function, name, params, arity, body})
  end

  defp parse_decl_params(%{tokens: []} = ctx), do: {[], ctx}
  defp parse_decl_params(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {[], ctx}
  defp parse_decl_params(ctx), do: parse_decl_params(ctx, [])

  defp parse_decl_params(%{tokens: [%Token{type: :right_paren} | _]} = ctx, params),
    do: {Enum.reverse(params), ctx}

  defp parse_decl_params(ctx, params) do
    {param, %{tokens: [current | tokens]} = ctx} = expect_identifier(ctx, "parameter")
    params = [param | params]

    if type_match(current, :comma) do
      parse_decl_params(%{ctx | tokens: tokens}, params)
    else
      {Enum.reverse(params), ctx}
    end
  end

  defp parse_statement(%{tokens: []} = ctx), do: ctx
  defp parse_statement(%{tokens: [%Token{type: :eof} | _]} = ctx), do: ctx

  defp parse_statement(%{tokens: [%Token{type: :print} | tokens]} = ctx),
    do: parse_print_statement(%{ctx | tokens: tokens})

  defp parse_statement(%{tokens: [%Token{type: :left_brace} | tokens]} = ctx),
    do: parse_block(%{ctx | tokens: tokens})

  defp parse_statement(%{tokens: [%Token{type: :right_brace} | tokens]} = ctx),
    do: %{ctx | tokens: tokens}

  defp parse_statement(%{tokens: [%Token{type: :if} | tokens]} = ctx),
    do: parse_if_statement(%{ctx | tokens: tokens})

  defp parse_statement(%{tokens: [%Token{type: :while} | tokens]} = ctx),
    do: parse_while_statement(%{ctx | tokens: tokens})

  defp parse_statement(%{tokens: [%Token{type: :for} | tokens]} = ctx),
    do: parse_for_statement(%{ctx | tokens: tokens})

  defp parse_statement(%{tokens: [%Token{type: :return} | _]} = ctx),
    do: parse_return_statment(ctx)

  defp parse_statement(ctx) do
    {expr, ctx} = parse_expression(ctx)
    {_, ctx} = expect_semicolon(ctx, "expression")
    add_statement(ctx, {:expr_stmt, expr})
  end

  defp parse_return_statment(ctx) do
    {keyword, %{tokens: [current | _]} = ctx} = next_token(ctx)

    {value, ctx} =
      if type_match(current, :semicolon) do
        {nil, ctx}
      else
        parse_expression(ctx)
      end

    {_, ctx} = expect_semicolon(ctx, "return value")

    add_statement(ctx, {:return_stmt, keyword, value})
  end

  defp parse_print_statement(ctx) do
    {expr, ctx} = parse_expression(ctx)
    {_, ctx} = expect_semicolon(ctx, "value")
    add_statement(ctx, {:print_stmt, expr})
  end

  defp parse_if_statement(ctx) do
    {_, ctx} = expect_left_paren(ctx, "if")
    {condition, ctx} = parse_expression(ctx)
    {_, ctx} = expect_right_paren(ctx, "'if' condition")

    {then_branch, %{tokens: [current | _]} = ctx} = parse_nested_statement(ctx)

    {else_branch, ctx} =
      if type_match(current, :else) do
        {_, ctx} = next_token(ctx)
        parse_nested_statement(ctx)
      else
        {nil, ctx}
      end

    add_statement(ctx, {:if_stmt, condition, then_branch, else_branch})
  end

  defp parse_while_statement(ctx) do
    {_, ctx} = expect_left_paren(ctx, "while")
    {condition, ctx} = parse_expression(ctx)
    {_, ctx} = expect_right_paren(ctx, "'while' condition")

    {body, ctx} = parse_nested_statement(ctx)

    add_statement(ctx, {:while_stmt, condition, body})
  end

  defp parse_for_statement(ctx) do
    {_, ctx} = expect_left_paren(ctx, "for")
    {initializer, ctx} = parse_for_initializer(ctx)
    {condition, ctx} = parse_for_condition(ctx)
    {_, ctx} = expect_semicolon(ctx, "loop condition")
    {increment, ctx} = parse_for_increment(ctx)
    {_, ctx} = expect_right_paren(ctx, "'for' clauses")
    {body, ctx} = parse_nested_statement(ctx)
    body = {:while_stmt, condition, desugar_for_increment(body, increment)}
    body = desugar_for_initializer(initializer, body)
    add_statement(ctx, body)
  end

  defp parse_for_initializer(%{tokens: [%Token{type: :semicolon} | tokens]} = ctx),
    do: {nil, %{ctx | tokens: tokens}}

  defp parse_for_initializer(%{tokens: [%Token{type: :var} | tokens]} = ctx) do
    %{statements: [current | statements]} =
      ctx =
      parse_declaration(:var, %{ctx | tokens: tokens, break: ctx.break + 1})

    {current, %{ctx | statements: statements}}
  end

  defp parse_for_initializer(ctx), do: parse_expression(ctx)

  defp desugar_for_initializer(nil, body), do: body
  defp desugar_for_initializer(initializer, body), do: {:block, [initializer, body]}

  defp parse_for_condition(%{tokens: [%Token{type: :semicolon} | _]} = ctx),
    do: {{:literal_expr, true}, ctx}

  defp parse_for_condition(ctx), do: parse_expression(%{ctx | break: ctx.break + 1})

  defp parse_for_increment(%{tokens: [%Token{type: :right_paren} | _]} = ctx), do: {nil, ctx}

  defp parse_for_increment(ctx) do
    {increment, ctx} = parse_expression(ctx)
    {{:expr_stmt, increment}, ctx}
  end

  defp desugar_for_increment(body, nil), do: body
  defp desugar_for_increment({:block, body}, increment), do: {:block, body ++ [increment]}
  defp desugar_for_increment(body, increment), do: {:block, [body, increment]}

  # Nested statement handling is required for flow control, where an execution branch may
  # be a single statement or a block of statements. Recursive parsing is greedy (all
  # statements will be processed until end of input or some other terminal condition), so
  # we need to *add* a terminal condition, which we are calling `break: number()`. If
  # `break` is 0, then we process normally. If it is greater than zero, then when
  # `add_statement/2` is called, it drops the break level by 1.
  #
  # Statement handlers only return the context, and the most recent statement added would
  # be what we need to get, so we're not going to stack scope processing like we do
  # in `parse_block/1`, but we will cheat and just pop the most recent statement off the
  # front of the list for processing.
  #
  # We set `break: true` before calling `parse_statement/1` so that we don't recurse back
  defp parse_nested_statement(ctx) do
    %{statements: [current | statements]} = ctx = parse_statement(%{ctx | break: ctx.break + 1})
    {current, %{ctx | statements: statements}}
  end

  # For block handling, We need to explicitly recognize the outer and inner contexts,
  # because we have to stack the outer context for recovery.
  #
  # The tokens are being transferred to the inner context, so clear the outer context
  # tokens so that we are forced to reset it after.
  #
  # This works because the right brace is a breakpoint and is handled by
  # `parse_statement/1`. To prevent issues in processing nested statements, we set
  # `single: false` on the inner context.
  #
  # The inner context starts with empty `statements`, because the result will be wrapped
  # in a block entry.
  defp parse_block(ctx) do
    outer_ctx = %{ctx | tokens: []}
    inner_ctx = %{ctx | statements: [], scopes: [outer_ctx | ctx.scopes], break: 0}

    # Process the declarations inside the block.
    inner_ctx = parse_declaration(inner_ctx)

    # Pop the outer context from the scopes stack, and then set the tokens and errors
    # from the inner context to recognize what has been consumed. The existing statements
    # is already valid.
    [outer_ctx | _] = inner_ctx.scopes
    outer_ctx = %{outer_ctx | tokens: inner_ctx.tokens, errors: inner_ctx.errors}

    # Finally, add the block statement to the outer context to the outer context. However,
    # remember to *reverse* the block statements because we collect statements backwards.
    add_statement(outer_ctx, {:block, Enum.reverse(inner_ctx.statements)})
  end

  defp parse_expression(%{tokens: []} = ctx),
    do: parse_expression(%{ctx | tokens: [Token.eof()]})

  defp parse_expression(%{tokens: [%Token{type: :eof} = token | _]} = ctx) do
    raise Ilox.ParserError, token: token, message: "Expect expression.", ctx: ctx
  end

  defp parse_expression(ctx), do: parse_assignment(ctx)

  defp parse_assignment(ctx) do
    {expr, %{tokens: [current | _]} = ctx} = parse_or(ctx)

    if type_match(current, :equal) do
      {equals, ctx} = next_token(ctx)
      {value, ctx} = parse_assignment(ctx)

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

  defp parse_or(ctx) do
    {expr, ctx} = parse_and(ctx)
    parse_binary_match(ctx, expr, [:or], &parse_and/1, :logical_expr)
  end

  defp parse_and(ctx) do
    {expr, ctx} = parse_equality(ctx)
    parse_binary_match(ctx, expr, [:and], &parse_equality/1, :logical_expr)
  end

  defp parse_equality(ctx) do
    {expr, ctx} = parse_comparison(ctx)
    parse_binary_match(ctx, expr, [:bang_equal, :equal_equal], &parse_comparison/1)
  end

  defp parse_comparison(ctx) do
    {expr, ctx} = parse_term(ctx)

    parse_binary_match(
      ctx,
      expr,
      [:greater, :greater_equal, :less, :less_equal],
      &parse_term/1
    )
  end

  defp parse_term(ctx) do
    {expr, ctx} = parse_factor(ctx)
    parse_binary_match(ctx, expr, [:minus, :plus], &parse_factor/1)
  end

  defp parse_factor(ctx) do
    {expr, ctx} = parse_unary(ctx)
    parse_binary_match(ctx, expr, [:slash, :star], &parse_unary/1)
  end

  defp parse_unary(%{tokens: []} = ctx), do: {nil, ctx}
  defp parse_unary(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp parse_unary(%{tokens: [current | _]} = ctx) do
    if type_match(current, [:bang, :minus]) do
      {operator, ctx} = next_token(ctx)

      case parse_unary(ctx) do
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
      parse_call(ctx)
    end
  end

  defp parse_call(%{tokens: []} = ctx), do: {nil, ctx}
  defp parse_call(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp parse_call(ctx) do
    {expr, %{tokens: [current | tokens]} = ctx} = parse_primary(ctx)

    if type_match(current, :left_paren) do
      parse_call(%{ctx | tokens: tokens}, expr)
    else
      {expr, ctx}
    end
  end

  defp parse_call(ctx, callee) do
    {args, %{tokens: [current | _]} = ctx} = parse_call_args(ctx)
    count = Enum.count(args)
    ctx = check_arity(ctx, count, current, "arguments")
    {closing, %{tokens: [current | tokens]} = ctx} = expect_right_paren(ctx, "arguments")
    call = {:call, callee, args, count, closing}

    if type_match(current, :left_paren) do
      parse_call(%{ctx | tokens: tokens}, call)
    else
      {call, ctx}
    end
  end

  defp check_arity(ctx, count, token, type) when count > 255 do
    error = Errors.format(%{token: token, message: "Can't have more than 255 #{type}."})

    %{ctx | errors: [error | ctx.errors]}
  end

  defp check_arity(ctx, _count, _token, _type), do: ctx

  defp parse_call_args(%{tokens: []} = ctx), do: {[], ctx}
  defp parse_call_args(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {[], ctx}
  defp parse_call_args(ctx), do: parse_call_args(ctx, [])

  defp parse_call_args(%{tokens: [%Token{type: :right_paren} | _]} = ctx, args),
    do: {Enum.reverse(args), ctx}

  defp parse_call_args(ctx, args) do
    {arg, %{tokens: [current | tokens]} = ctx} = parse_expression(ctx)
    args = [arg | args]

    if type_match(current, :comma) do
      parse_call_args(%{ctx | tokens: tokens}, args)
    else
      {Enum.reverse(args), ctx}
    end
  end

  defp parse_primary(%{tokens: []} = ctx), do: {nil, ctx}
  defp parse_primary(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp parse_primary(%{tokens: [%Token{type: :Qfalse} | tokens]} = ctx),
    do: {{:literal_expr, false}, %{ctx | tokens: tokens}}

  defp parse_primary(%{tokens: [%Token{type: :Qnil} | tokens]} = ctx),
    do: {{:literal_expr, nil}, %{ctx | tokens: tokens}}

  defp parse_primary(%{tokens: [%Token{type: :Qtrue} | tokens]} = ctx),
    do: {{:literal_expr, true}, %{ctx | tokens: tokens}}

  defp parse_primary(%{tokens: [%Token{type: type} = current | tokens]} = ctx)
       when type in [:number, :string],
       do: {{:literal_expr, current}, %{ctx | tokens: tokens}}

  defp parse_primary(%{tokens: [%Token{type: :identifier} = current | tokens]} = ctx),
    do: {{:var_expr, current}, %{ctx | tokens: tokens}}

  defp parse_primary(%{tokens: [%Token{type: :left_paren} | tokens]} = ctx) do
    {expr, ctx} = parse_expression(%{ctx | tokens: tokens})
    {_, ctx} = expect_right_paren(ctx, "expression")
    {{:group_expr, expr}, ctx}
  end

  defp parse_primary(%{tokens: [current | _]} = ctx) do
    raise Ilox.ParserError, token: current, message: "Expect expression.", ctx: ctx
  end

  defp expect_identifier(ctx, kind), do: expect(ctx, :identifier, "Expect #{kind} name.")
  defp expect_left_paren(ctx, kind), do: expect(ctx, :left_paren, "Expect '(' after '#{kind}'.")
  defp expect_right_paren(ctx, kind), do: expect(ctx, :right_paren, "Expect ')' after #{kind}.")
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

  defp parse_binary_match(%{tokens: []} = ctx, expr, _types, _consumer), do: {expr, ctx}

  defp parse_binary_match(%{tokens: [%Token{type: :eof} | _]} = ctx, expr, _types, _consumer),
    do: {expr, ctx}

  defp parse_binary_match(
         %{tokens: [current | _]} = ctx,
         expr,
         types,
         consumer,
         expr_type \\ :binary_expr
       ) do
    if type_match(current, types) do
      {operator, ctx} = next_token(ctx)

      case consumer.(ctx) do
        {nil, ctx} ->
          raise Ilox.ParserError,
            token: current,
            message: "Expect right-hand expression.",
            where: Errors.where(current),
            ctx: ctx

        {right, ctx} ->
          expr = {expr_type, expr, operator, right}
          parse_binary_match(ctx, expr, types, consumer, expr_type)
      end
    else
      {expr, ctx}
    end
  end

  defp add_statement(%{break: value} = ctx, statement) when is_integer(value) and value > 0,
    do: %{ctx | statements: [statement | ctx.statements], break: ctx.break - 1}

  defp add_statement(%{break: 0} = ctx, statement),
    do: parse_declaration(%{ctx | statements: [statement | ctx.statements]})

  defp synchronize(ctx) do
    case next_token(ctx) do
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

  defp next_token(%{tokens: []} = ctx), do: {nil, ctx}

  defp next_token(%{tokens: [%Token{} = previous, %Token{type: :eof} | _]} = ctx),
    do: {previous, %{ctx | tokens: [Token.eof(previous.line)]}}

  defp next_token(%{tokens: [%Token{type: :eof} | _]} = ctx), do: {nil, ctx}

  defp next_token(%{tokens: [%Token{} = previous | tokens]} = ctx),
    do: {previous, %{ctx | tokens: tokens}}
end
