defmodule Ilox.ScannerTest do
  use ExUnit.Case, async: true

  alias Ilox.Scanner
  alias Ilox.Token

  describe "basic token support" do
    @operators %{
      "(" => :left_paren,
      ")" => :right_paren,
      "{" => :left_brace,
      "}" => :right_brace,
      "," => :comma,
      "." => :dot,
      "-" => :minus,
      "+" => :plus,
      ";" => :semicolon,
      "*" => :star,
      "!" => :bang,
      "!=" => :bang_equal,
      "=" => :equal,
      "==" => :equal_equal,
      "<" => :less,
      "<=" => :less_equal,
      ">" => :greater,
      ">=" => :greater_equal,
      "/" => :slash
    }

    for {input, token_type} <- @operators do
      test "input '#{input}' produces a #{token_type} operator" do
        assert {:ok,
                [
                  %Token{
                    type: unquote(token_type),
                    line: 1,
                    lexeme: unquote(input),
                    literal: nil
                  },
                  %Token{type: :eof}
                ]} =
                 Scanner.scan_tokens(unquote(input))
      end
    end

    test "input '//' produces nothing" do
      assert {:ok, [%Token{type: :eof}]} = Scanner.scan_tokens("//")
    end

    test "input '\"hello\"' produces a string literal" do
      assert {:ok,
              [
                %Token{type: :string, line: 1, lexeme: "\"hello\"", literal: "hello"},
                %Token{type: :eof}
              ]} = Scanner.scan_tokens("\"hello\"")
    end

    test "input '123' produces a number literal" do
      assert {:ok,
              [%Token{type: :number, line: 1, lexeme: "", literal: 123.0}, %Token{type: :eof}]} =
               Scanner.scan_tokens("123")
    end

    test "input 'fooBar' produces an identifier" do
      assert {:ok,
              [
                %Token{type: :identifier, line: 1, lexeme: "fooBar", literal: nil},
                %Token{type: :eof}
              ]} =
               Scanner.scan_tokens("fooBar")
    end

    test "whitespace input is ignored" do
      assert {:ok, [%Token{type: :eof}]} = Scanner.scan_tokens(" \t\r")
    end

    test "newlines advance the current line" do
      assert {:ok, [%Token{type: :eof, line: 2}]} = Scanner.scan_tokens("\n")
    end

    test "non-printable ASCII characters in strings are ignored" do
      assert {:ok,
              [
                %Token{type: :string, line: 1, lexeme: "\"hëllö\"", literal: "hëllö"},
                %Token{type: :eof}
              ]} = Scanner.scan_tokens("\"hëllö\"")
    end

    test "non-printable ASCII characters outside of strings produce errors" do
      assert {:error, :scanner,
              [
                "[line 1] Error: Unexpected character (`ë`).",
                "[line 1] Error: Unexpected character (`ö`)."
              ]} = Scanner.scan_tokens("hëllö")
    end

    test "strings do not support quote escaping" do
      assert {:error, :scanner,
              [
                "[line 1] Error: Unexpected character (`\\`).",
                "[line 1] Error: Unterminated string."
              ]} =
               Scanner.scan_tokens("\"I say \\\"hello\\\".")
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

    for {input, token_type} <- @keywords do
      test "input '#{input}' produces a #{token_type} keyword" do
        assert {:ok,
                [
                  %Token{
                    type: unquote(token_type),
                    line: 1,
                    lexeme: unquote(input),
                    literal: nil
                  },
                  %Token{type: :eof}
                ]} =
                 Scanner.scan_tokens(unquote(input))
      end
    end
  end

  describe "compound token support" do
    test "compound token support" do
      assert {:ok,
              [
                Token.new(:print, 1, "print", nil),
                Token.new(:left_paren, 1, "(", nil),
                Token.new(:identifier, 2, "identifier", nil),
                Token.new(:plus, 3, "+", nil),
                Token.new(:string, 4, "\"hello\"", "hello"),
                Token.new(:right_paren, 5, ")", nil),
                Token.eof(7)
              ]} ==
               Scanner.scan_tokens("""
               print ( // multiline support with comments
                   identifier
                   + // and whitespace
                   "hello"
               )

               """)
    end
  end
end
