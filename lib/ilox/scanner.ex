defmodule Ilox.Scanner do
  @moduledoc """
  Ilox source scanner.
  """

  alias Ilox.Errors
  alias Ilox.Token

  import Ilox.Guards

  def scan(source) when is_binary(source) do
    case scan_next_token(source) do
      %{errors: [], tokens: tokens} ->
        tokens =
          tokens
          |> Enum.with_index()
          |> Enum.map(fn {token, id} -> %{token | id: id} end)

        {:ok, tokens}

      %{errors: errors} ->
        {:error, :scanner, Enum.reverse(errors)}
    end
  end

  defp scan_next_token(source) when is_binary(source),
    do: scan_next_token(%{lexeme: "", errors: [], line: 1, src: source, tokens: []})

  defp scan_next_token(%{src: ""} = ctx) do
    tokens = [Token.eof(ctx.line) | ctx.tokens]
    %{ctx | tokens: Enum.reverse(tokens)}
  end

  defp scan_next_token(%{src: src} = ctx) do
    {codepoint, src} = String.next_codepoint(src)

    codepoint
    |> handle_codepoint(%{ctx | lexeme: ctx.lexeme <> codepoint, src: src})
    |> scan_next_token()
  end

  defp error(ctx, message),
    do: %{ctx | errors: [Errors.format(ctx, message) | ctx.errors]}

  defp add_token(ctx, type, literal \\ nil) do
    token = Token.new(type, ctx.line, ctx.lexeme, literal)

    %{ctx | lexeme: "", tokens: [token | ctx.tokens]}
  end

  defp add_compound_token(ctx, match, left_type, right_type) do
    case String.next_codepoint(ctx.src) do
      {^match, src} ->
        add_token(%{ctx | lexeme: ctx.lexeme <> match, src: src}, left_type)

      _ ->
        add_token(ctx, right_type)
    end
  end

  @keywords %{
    "and" => :and,
    "class" => :class,
    "else" => :else,
    "false" => :Qfalse,
    "for" => :for,
    "fun" => :fun,
    "if" => :if,
    "nil" => :Qnil,
    "or" => :or,
    "print" => :print,
    "return" => :return,
    "super" => :super,
    "this" => :this,
    "true" => :Qtrue,
    "var" => :var,
    "while" => :while
  }

  defp add_identifier_token(ctx) do
    add_token(ctx, Map.get(@keywords, ctx.lexeme, :identifier))
  end

  defp add_number_token(%{lexeme: lexeme} = ctx) do
    {number, _} = Float.parse(lexeme)
    add_token(ctx, :number, number)
  end

  defp handle_codepoint("(", ctx), do: add_token(ctx, :left_paren)
  defp handle_codepoint(")", ctx), do: add_token(ctx, :right_paren)
  defp handle_codepoint("{", ctx), do: add_token(ctx, :left_brace)
  defp handle_codepoint("}", ctx), do: add_token(ctx, :right_brace)
  defp handle_codepoint(",", ctx), do: add_token(ctx, :comma)
  defp handle_codepoint(".", ctx), do: add_token(ctx, :dot)
  defp handle_codepoint("-", ctx), do: add_token(ctx, :minus)
  defp handle_codepoint("+", ctx), do: add_token(ctx, :plus)
  defp handle_codepoint(";", ctx), do: add_token(ctx, :semicolon)
  defp handle_codepoint("*", ctx), do: add_token(ctx, :star)
  defp handle_codepoint("!", ctx), do: add_compound_token(ctx, "=", :bang_equal, :bang)
  defp handle_codepoint("=", ctx), do: add_compound_token(ctx, "=", :equal_equal, :equal)
  defp handle_codepoint("<", ctx), do: add_compound_token(ctx, "=", :less_equal, :less)

  defp handle_codepoint(">", ctx),
    do: add_compound_token(ctx, "=", :greater_equal, :greater)

  defp handle_codepoint("/", ctx), do: handle_slash(ctx)
  defp handle_codepoint("\"", ctx), do: handle_string(ctx)

  defp handle_codepoint("\n", ctx), do: %{ctx | lexeme: "", line: ctx.line + 1}

  defp handle_codepoint(c, ctx) when is_whitespace(c), do: %{ctx | lexeme: ""}
  defp handle_codepoint(c, ctx) when is_digit(c), do: handle_number(%{ctx | lexeme: c})

  defp handle_codepoint(c, ctx) when is_alpha(c),
    do: handle_identifier(%{ctx | lexeme: c})

  defp handle_codepoint(invalid, ctx),
    do: error(ctx, "Unexpected character (`#{invalid}`).")

  defp handle_identifier(%{src: ""} = ctx), do: add_identifier_token(ctx)

  defp handle_identifier(%{lexeme: lexeme, src: src} = ctx) do
    case String.next_codepoint(src) do
      {c, src} when is_alphanumeric(c) ->
        handle_identifier(%{ctx | lexeme: lexeme <> c, src: src})

      _ ->
        add_identifier_token(ctx)
    end
  end

  defp handle_number(%{src: ""} = ctx), do: add_number_token(ctx)

  defp handle_number(ctx) do
    handle_number_part(String.next_codepoint(ctx.src), ctx)
  end

  defp handle_number_part({c, src}, %{lexeme: lexeme} = ctx) when is_digit(c),
    do: handle_number(%{ctx | lexeme: lexeme <> c, src: src})

  defp handle_number_part({".", src}, %{lexeme: lexeme} = ctx) do
    cond do
      String.contains?(lexeme, "e") ->
        error(ctx, "Numbers with scientific notation may only use integer exponents.")

      String.contains?(lexeme, ".") ->
        error(ctx, "Numbers may only contain one decimal.")

      true ->
        case String.next_codepoint(src) do
          {c, src} when is_digit(c) ->
            handle_number(%{ctx | lexeme: "#{lexeme}.#{c}", src: src})

          _ ->
            add_number_token(ctx)
        end
    end
  end

  defp handle_number_part({"e", src}, %{lexeme: lexeme} = ctx) do
    if String.contains?(lexeme, "e") do
      error(ctx, "Numbers may not contain a second scientific notation.")
    else
      handle_number_exponent("e", String.next_codepoint(src), ctx)
    end
  end

  defp handle_number_part(_next_codepoint, ctx),
    do: add_number_token(ctx)

  defp handle_number_exponent(exp, {"-", src}, ctx) do
    if String.contains?(exp, "-") do
      error(ctx, "Scientific notation may only have one negation.")
    else
      handle_number_exponent(exp <> "-", String.next_codepoint(src), ctx)
    end
  end

  defp handle_number_exponent(exp, {c, src}, %{lexeme: lexeme} = ctx) when is_digit(c),
    do: handle_number(%{ctx | lexeme: "#{lexeme}#{exp}#{c}", src: src})

  defp handle_number_exponent(_exp, _codepoint, ctx),
    do: add_number_token(ctx)

  defp handle_slash(ctx) do
    case String.next_codepoint(ctx.src) do
      {"/", src} -> %{ctx | lexeme: "", src: handle_comment(src)}
      _ -> add_token(ctx, :slash)
    end
  end

  defp handle_string(%{src: ""} = ctx), do: error(ctx, "Unterminated string.")

  defp handle_string(%{lexeme: lexeme, src: src} = ctx) do
    case String.next_codepoint(src) do
      {"\"", src} ->
        add_token(
          %{ctx | lexeme: lexeme <> "\"", src: src},
          :string,
          String.replace_prefix(lexeme, "\"", "")
        )

      {"\n", src} ->
        handle_string(%{ctx | lexeme: lexeme <> "\n", src: src})

      {c, src} ->
        handle_string(%{ctx | lexeme: lexeme <> c, src: src})
    end
  end

  defp handle_comment(""), do: ""
  defp handle_comment("\n"), do: "\n"
  defp handle_comment("\n" <> src), do: "\n" <> src
  defp handle_comment(<<_c::utf8, src::binary>>), do: handle_comment(src)
end
