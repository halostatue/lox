defmodule Ilox.ScannerTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

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
                 Scanner.scan(unquote(input))
      end
    end

    test "input '//' produces nothing" do
      assert {:ok, [%Token{type: :eof}]} = Scanner.scan("//")
    end

    test "input '\"hello\"' produces a string literal" do
      assert {:ok,
              [
                %Token{type: :string, line: 1, lexeme: "\"hello\"", literal: "hello"},
                %Token{type: :eof}
              ]} = Scanner.scan("\"hello\"")
    end

    for input <- ["123", "123e5", "123.5e5", "123.5e-5"] do
      test "input '#{input}' produces a number literal" do
        {literal, ""} = Float.parse(unquote(input))

        assert {:ok,
                [
                  %Token{type: :number, line: 1, lexeme: unquote(input), literal: ^literal},
                  %Token{type: :eof}
                ]} =
                 Scanner.scan(unquote(input))
      end
    end

    test "input '123.3.3' produces an error" do
      assert {:error, :scanner, ["[line 1] Error: Numbers may only contain one decimal."]} ==
               Scanner.scan("123.3.3")
    end

    test "input '123.5e5e5' produces an error" do
      assert {:error, :scanner,
              ["[line 1] Error: Numbers may not contain a second scientific notation."]} ==
               Scanner.scan("123.5e5e5")
    end

    test "input '123.5e5.5' produces an error" do
      assert {:error, :scanner,
              ["[line 1] Error: Numbers with scientific notation may only use integer exponents."]} =
               Scanner.scan("123.5e5.5")
    end

    test "input '123.5e--5' produces an error" do
      assert {:error, :scanner,
              ["[line 1] Error: Scientific notation may only have one negation."]} =
               Scanner.scan("123.5e--5")
    end

    property "input positive float values produce number literals" do
      check all(float <- float(min: 0.0)) do
        assert {:ok, [%Token{type: :number, literal: ^float}, %Token{type: :eof}]} =
                 Scanner.scan(to_string(float))
      end
    end

    test "input 'fooBar' produces an identifier" do
      assert {:ok,
              [
                %Token{type: :identifier, line: 1, lexeme: "fooBar", literal: nil},
                %Token{type: :eof}
              ]} =
               Scanner.scan("fooBar")
    end

    test "whitespace input is ignored" do
      assert {:ok, [%Token{type: :eof}]} = Scanner.scan(" \t\r")
    end

    test "newlines advance the current line" do
      assert {:ok, [%Token{type: :eof, line: 2}]} = Scanner.scan("\n")
    end

    test "non-printable ASCII characters in strings are ignored" do
      assert {:ok,
              [
                %Token{type: :string, line: 1, lexeme: "\"hëllö\"", literal: "hëllö"},
                %Token{type: :eof}
              ]} = Scanner.scan("\"hëllö\"")
    end

    test "non-printable ASCII characters outside of strings produce errors" do
      assert {:error, :scanner,
              [
                "[line 1] Error: Unexpected character (`ë`).",
                "[line 1] Error: Unexpected character (`ö`)."
              ]} = Scanner.scan("hëllö")
    end

    test "strings do not support quote escaping" do
      assert {:error, :scanner,
              [
                "[line 1] Error: Unexpected character (`\\`).",
                "[line 1] Error: Unterminated string."
              ]} =
               Scanner.scan("\"I say \\\"hello\\\".")
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
                 Scanner.scan(unquote(input))
      end
    end
  end

  describe "compound token support" do
    test "compound token support" do
      assert {:ok,
              [
                %{Token.new(:print, 1, "print", nil) | id: 0},
                %{Token.new(:left_paren, 1, "(", nil) | id: 1},
                %{Token.new(:identifier, 2, "identifier", nil) | id: 2},
                %{Token.new(:plus, 3, "+", nil) | id: 3},
                %{Token.new(:string, 4, "\"hello\"", "hello") | id: 4},
                %{Token.new(:right_paren, 5, ")", nil) | id: 5},
                %{Token.eof(7) | id: 6}
              ]} ==
               Scanner.scan("""
               print ( // multiline support with comments
                   identifier
                   + // and whitespace
                   "hello"
               )

               """)
    end
  end

  describe "block scanner" do
    test "scope" do
      assert {:ok, _} =
               Scanner.scan("""
               var a = "global a";
               var b = "global b";
               var c = "global c";
               {
                 var a = "outer a";
                 var b = "outer b";
                 {
                   var a = "inner a";
                   print a;
                   print b;
                   print c;
                 }
                 print a;
                 print b;
                 print c;
               }
               print a;
               print b;
               print c;
               """)
    end
  end
end
