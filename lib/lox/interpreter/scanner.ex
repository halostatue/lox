defmodule Lox.Interpreter.Scanner do
  @moduledoc """
  Lox scanner.
  """

  alias Lox.Interpreter
  alias Lox.Interpreter.Token

  @type t :: %__MODULE__{
          source: binary(),
          tokens: list(Token.t())
        }

  defstruct [:source, tokens: []]

  def new(source), do: %__MODULE__{source: source}

  def scan_tokens(source) when is_binary(source) do
    result =
      source
      |> new()
      |> ctx()
      |> scan_next_token()

    case result do
      %{error: true, scanner: %{tokens: tokens}} -> {:error, tokens}
      %{scanner: %{tokens: tokens}} -> {:ok, tokens}
    end
  end

  defp ctx(%__MODULE__{} = scanner) do
    %{
      error: false,
      # The current token
      current: "",
      # The literal being built
      literal: "",
      # The rest of the source
      rest: scanner.source,
      # The current line
      line: 1,
      scanner: scanner
    }
  end

  defp add_token(context, type, literal \\ nil) do
    token = Token.new(type, context.current, literal, context.line)

    %{
      context
      | current: "",
        scanner: %{context.scanner | tokens: [token | context.scanner.tokens]}
    }
  end

  defp add_compound_token(context, match, left_type, right_type) do
    case String.next_codepoint(context.rest) do
      {^match, rest} ->
        add_token(%{context | current: context.current <> match, rest: rest}, left_type)

      _ ->
        add_token(context, right_type)
    end
  end

  @digits ?0..?9
          |> Range.to_list()
          |> to_string()
          |> String.split("", trim: true)

  defguardp is_digit(c) when c in @digits

  @lower ?a..?z
         |> Range.to_list()
         |> to_string()
         |> String.split("", trim: true)

  @upper ?A..?Z
         |> Range.to_list()
         |> to_string()
         |> String.split("", trim: true)

  @alpha @lower ++ @upper ++ ["_"]

  defguardp is_alpha(c) when c in @alpha

  defguardp is_alphanumeric(c) when is_digit(c) or is_alpha(c)

  defguardp is_whitespace(c) when c in ["\r", "\s", "\t"]

  defp scan_next_token(%{rest: ""} = context) do
    tokens = Enum.reverse([Token.new(:eof, "", nil, context.line) | context.scanner.tokens])
    %{context | scanner: %{context.scanner | tokens: tokens}}
  end

  defp scan_next_token(%{rest: source} = context) do
    {c, rest} = String.next_codepoint(source)
    context = %{context | current: context.current <> c, rest: rest}

    context =
      case c do
        "(" -> add_token(context, :left_paren)
        ")" -> add_token(context, :right_paren)
        "{" -> add_token(context, :left_brace)
        "}" -> add_token(context, :right_brace)
        "," -> add_token(context, :comma)
        "." -> add_token(context, :dot)
        "-" -> add_token(context, :minus)
        "+" -> add_token(context, :plus)
        ";" -> add_token(context, :semicolon)
        "*" -> add_token(context, :star)
        "!" -> add_compound_token(context, "=", :bang_equal, :bang)
        "=" -> add_compound_token(context, "=", :equal_equal, :equal)
        "<" -> add_compound_token(context, "=", :less_equal, :less)
        ">" -> add_compound_token(context, "=", :greater_equal, :greater)
        "/" -> handle_slash(context)
        "\"" -> handle_string(context)
        "\n" -> %{context | current: "", line: context.line + 1}
        c when is_whitespace(c) -> %{context | current: ""}
        c when is_digit(c) -> handle_number(%{context | current: c})
        c when is_alpha(c) -> handle_identifier(%{context | current: c})
        invalid -> error(context, "Unexpected character (`#{invalid}`).")
      end

    scan_next_token(context)
  end

  defp error(context, message) do
    Interpreter.error(context, message)
  end

  @keywords %{
    "and" => :and,
    "class" => :class,
    "else" => :else,
    "false" => false,
    "for" => :for,
    "fun" => :fun,
    "if" => :if,
    "nil" => nil,
    "or" => :or,
    "print" => :print,
    "return" => :return,
    "super" => :super,
    "this" => :this,
    "true" => true,
    "var" => :var,
    "while" => :while
  }

  defp add_identifier_token(context) do
    add_token(context, Map.get(@keywords, context.current, :identifier))
  end

  defp handle_identifier(%{rest: ""} = context), do: add_identifier_token(context)

  defp handle_identifier(%{current: current, rest: rest} = context) do
    case String.next_codepoint(rest) do
      {c, rest} when is_alphanumeric(c) ->
        handle_identifier(%{context | current: current <> c, rest: rest})

      _ ->
        add_identifier_token(context)
    end
  end

  defp handle_number(%{rest: ""} = context), do: add_number_token(context, context.current)

  defp handle_number(%{current: current, rest: rest} = context) do
    case String.next_codepoint(rest) do
      {c, rest} when is_digit(c) ->
        handle_number(%{context | current: current <> c, rest: rest})

      {".", rest} ->
        case String.next_codepoint(rest) do
          {c, rest} when is_digit(c) ->
            handle_number(%{context | current: "#{current}.#{c}", rest: rest})

          _ ->
            add_number_token(context, context.current)
        end

      {c, rest} ->
        add_number_token(%{context | rest: c <> rest}, context.current)
    end
  end

  defp add_number_token(context, value) do
    {number, _} = Float.parse(value)
    add_token(%{context | current: ""}, :number, number)
  end

  defp handle_slash(context) do
    case String.next_codepoint(context.rest) do
      {"/", rest} -> %{context | current: "", rest: consume_comment(rest)}
      _ -> add_token(context, :slash)
    end
  end

  defp handle_string(%{rest: ""} = context) do
    Interpreter.error(context, "Unterminated string.")
  end

  defp handle_string(%{current: current, literal: literal, rest: rest} = context) do
    case String.next_codepoint(rest) do
      {"\"", rest} ->
        add_token(%{context | current: current <> "\"", rest: rest}, :string, literal)

      {"\n", rest} ->
        handle_string(%{
          context
          | current: current <> "\n",
            line: context.line + 1,
            literal: literal <> "\n",
            rest: rest
        })

      {c, rest} ->
        handle_string(%{context | current: current <> c, literal: literal <> c, rest: rest})
    end
  end

  defp consume_comment(""), do: ""
  defp consume_comment("\n"), do: ""
  defp consume_comment("\n" <> rest), do: rest
  defp consume_comment(<<_c::utf8, rest::binary>>), do: consume_comment(rest)
end
