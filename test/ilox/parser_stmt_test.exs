defmodule Ilox.ParserStmtTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ilox.Parser
  alias Ilox.Token

  import Ilox.StreamData

  describe "parse/1: literal expression statements" do
    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "#{inspect(input)};" do
        assert {:ok, [{:expr_stmt, {:literal_expr, unquote(literal)}}]} =
                 Parser.parse(unquote(input) <> ";")
      end

      test "-#{inspect(input)};" do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :minus}, {:literal_expr, unquote(literal)}}}
                ]} =
                 Parser.parse("-#{unquote(input)};")
      end
    end

    property "string;" do
      check all(string <- literal(:string)) do
        assert {:ok, [{:expr_stmt, {:literal_expr, %Token{type: :string, literal: ^string}}}]} =
                 Parser.parse(inspect(string) <> ";")
      end
    end

    property "-string;" do
      check all(string <- literal(:string)) do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :minus},
                    {:literal_expr, %Token{type: :string, literal: ^string}}}}
                ]} =
                 Parser.parse("-\"#{string}\";")
      end
    end

    property "non-negative integer;" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok, [{:expr_stmt, {:literal_expr, %Token{type: :number, literal: ^literal}}}]} =
                 Parser.parse(to_string(int) <> ";")
      end
    end

    property "non-negative float;" do
      check all(float <- float(min: 0.0)) do
        assert {:ok, [{:expr_stmt, {:literal_expr, %Token{type: :number, literal: ^float}}}]} =
                 Parser.parse(to_string(float) <> ";")
      end
    end

    property "negative integer;" do
      check all(int <- positive_integer()) do
        literal = int / 1

        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :minus},
                    {:literal_expr, %Token{type: :number, literal: ^literal}}}}
                ]} =
                 Parser.parse("-#{int};")
      end
    end

    property "negative float;" do
      check all(float <- float(max: -0.0)) do
        literal = abs(float)

        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :minus},
                    {:literal_expr, %Token{type: :number, literal: ^literal}}}}
                ]} =
                 Parser.parse(to_string(float) <> ";")
      end
    end
  end

  describe "parse/1: unary Boolean negation statements" do
    property "!negative integer;" do
      check all(int <- positive_integer()) do
        literal = int / 1

        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang},
                    {:unary_expr, %Token{type: :minus},
                     {:literal_expr, %Token{type: :number, literal: ^literal}}}}}
                ]} =
                 Parser.parse("!-#{int};")
      end
    end

    property "!negative float;" do
      check all(float <- float(max: -0.0)) do
        literal = abs(float)

        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang},
                    {:unary_expr, %Token{type: :minus},
                     {:literal_expr, %Token{type: :number, literal: ^literal}}}}}
                ]} =
                 Parser.parse("!#{float};")
      end
    end

    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "!#{inspect(input)};" do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang}, {:literal_expr, unquote(literal)}}}
                ]} =
                 Parser.parse("!#{unquote(input)};")
      end
    end

    property "!string;" do
      check all(string <- literal(:string)) do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang},
                    {:literal_expr, %Token{type: :string, literal: ^string}}}}
                ]} =
                 Parser.parse("!\"#{string}\";")
      end
    end

    property "!non-negative integer;" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang},
                    {:literal_expr, %Token{type: :number, literal: ^literal}}}}
                ]} =
                 Parser.parse("!#{int};")
      end
    end

    property "!non-negative float;" do
      check all(float <- float(min: 0.0)) do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:unary_expr, %Token{type: :bang},
                    {:literal_expr, %Token{type: :number, literal: ^float}}}}
                ]} =
                 Parser.parse("!#{float};")
      end
    end
  end

  describe "parse/1: group expression statements" do
    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "(#{inspect(input)});" do
        assert {:ok, [{:expr_stmt, {:group_expr, {:literal_expr, unquote(literal)}}}]} =
                 Parser.parse("(" <> inspect(unquote(literal)) <> ");")
      end
    end

    property "(string);" do
      check all(string <- literal(:string)) do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:group_expr, {:literal_expr, %Token{type: :string, literal: ^string}}}}
                ]} =
                 Parser.parse("(\"#{string}\");")
      end
    end

    property "(non-negative integer);" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok,
                [
                  {:expr_stmt,
                   {:group_expr, {:literal_expr, %Token{type: :number, literal: ^literal}}}}
                ]} =
                 Parser.parse("(#{int});")
      end
    end

    property "(non-negative float);" do
      check all(float <- float(min: 0.0)) do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:group_expr, {:literal_expr, %Token{type: :number, literal: ^float}}}}
                ]} =
                 Parser.parse("(#{float});")
      end
    end
  end

  describe "parse/1: binary expression statements" do
    for {operator, token} <- [
          {"!=", :bang_equal},
          {"==", :equal_equal},
          {"<", :less},
          {"<=", :less_equal},
          {">", :greater},
          {">=", :greater_equal},
          {"-", :minus},
          {"+", :plus},
          {"/", :slash},
          {"*", :star}
        ] do
      test "left #{operator} right;" do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:binary_expr, {:var_expr, %Token{type: :identifier, lexeme: "left"}},
                    %Token{type: unquote(token)},
                    {:var_expr, %Token{type: :identifier, lexeme: "right"}}}}
                ]} =
                 Parser.parse("left #{unquote(operator)} right;")
      end

      test "nil #{operator} number;" do
        assert {:ok,
                [
                  {:expr_stmt,
                   {:binary_expr, {:literal_expr, nil}, %Token{type: unquote(token)},
                    {:literal_expr, %Token{type: :number, literal: 5.0}}}}
                ]} =
                 Parser.parse("nil #{unquote(operator)} 5;")
      end
    end
  end

  describe "parse/1: assignment statements" do
    test "left = right;" do
      assert {:ok,
              [
                {:expr_stmt,
                 {:assignment_expr, %Token{type: :identifier, lexeme: "left"},
                  {:var_expr, %Token{type: :identifier, lexeme: "right"}}}}
              ]} = Parser.parse("left = right;")
    end

    test "nil = right; errors on assignment target" do
      assert {:error, :parser, ["[line 1] Error: Invalid assignment target."]} =
               Parser.parse("nil = right;")
    end
  end

  describe "parse/1: variable declaration and initialization" do
    test "var left = right;" do
      assert {:ok,
              [
                {:var_decl, %Token{type: :identifier, lexeme: "left"},
                 {:var_expr, %Token{type: :identifier, lexeme: "right"}}}
              ]} = Parser.parse("var left = right;")
    end

    test "var left;" do
      assert {:ok, [{:var_decl, %Token{type: :identifier, lexeme: "left"}, nil}]} =
               Parser.parse("var left;")
    end

    test "var; errors on missing identifier" do
      assert {:error, :parser, ["[line 1] Error at ';': Expect variable name."]} =
               Parser.parse("var;")
    end
  end
end
