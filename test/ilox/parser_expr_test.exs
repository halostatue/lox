defmodule Ilox.ParserExprTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ilox.Parser
  alias Ilox.Token

  @atomics [
    {"true", true},
    {"false", false},
    {"nil", nil}
  ]

  describe "parse_expr/1: primary literal expressions" do
    for {left, left_literal} <- @atomics do
      test "#{inspect(left)} -> {:literal, #{inspect(left_literal)}}" do
        assert {:ok, {:literal_expr, unquote(left_literal)}} =
                 Parser.parse_expr(unquote(left))
      end

      for {right, right_literal} <- @atomics,
          {operator, token} <- [
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
        test "#{left} #{operator} #{right} -> {:binary_expr, {:literal, #{inspect(left_literal)}}, #{token}, {:literal, #{inspect(right_literal)}}}" do
          assert {:ok,
                  {:binary_expr, {:literal_expr, unquote(left_literal)},
                   %Token{type: unquote(token)},
                   {:literal_expr, unquote(right_literal)}}} =
                   Parser.parse_expr("#{unquote(left)} #{unquote(operator)} #{unquote(right)}")
        end
      end
    end

    property "string -> {:literal, :string}" do
      check all(string <- string(:printable)) do
        assert {:ok, {:literal_expr, %Token{type: :string, literal: ^string}}} =
                 Parser.parse_expr(inspect(string))
      end
    end

    property "non-negative integer -> {:literal, :number}" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok, {:literal_expr, %Token{type: :number, literal: ^literal}}} =
                 Parser.parse_expr(to_string(int))
      end
    end

    property "non-negative float -> {:literal, :number}" do
      check all(float <- float(min: 0.0)) do
        assert {:ok, {:literal_expr, %Token{type: :number, literal: ^float}}} =
                 Parser.parse_expr(to_string(float))
      end
    end
  end

  describe "parse_expr/1: unary arithmetic negation (-expr)" do
    property "negative integer -> {:unary, :minus, {:literal, :number}}" do
      check all(int <- positive_integer()) do
        literal = int / 1

        assert {:ok,
                {:unary_expr, %Token{type: :minus},
                 {:literal_expr, %Token{type: :number, literal: ^literal}}}} =
                 Parser.parse_expr("-#{int}")
      end
    end

    property "negative float -> {:unary, :minus, {:literal, :number}}" do
      check all(float <- float(max: -0.0)) do
        literal = abs(float)

        assert {:ok,
                {:unary_expr, %Token{type: :minus},
                 {:literal_expr, %Token{type: :number, literal: ^literal}}}} =
                 Parser.parse_expr(to_string(float))
      end
    end

    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "-#{inspect(input)} -> {:unary, :minus, {:literal, #{literal}}}" do
        assert {:ok, {:unary_expr, %Token{type: :minus}, {:literal_expr, unquote(literal)}}} =
                 Parser.parse_expr("-#{unquote(input)}")
      end
    end

    property "-string -> {:unary, :minus, {:literal, :string}}" do
      check all(string <- string(:printable)) do
        assert {:ok,
                {
                  :unary_expr,
                  %Token{type: :minus},
                  {:literal_expr, %Token{type: :string, literal: ^string}}
                }} =
                 Parser.parse_expr("-\"#{string}\"")
      end
    end
  end

  describe "parse_expr/1: unary Boolean negation (!expr)" do
    property "negative integer -> {:unary, :bang, {:unary, :minus, {:literal, :number}}}" do
      check all(int <- positive_integer()) do
        literal = int / 1

        assert {:ok,
                {:unary_expr, %Token{type: :bang},
                 {:unary_expr, %Token{type: :minus},
                  {:literal_expr, %Token{type: :number, literal: ^literal}}}}} =
                 Parser.parse_expr("!-#{int}")
      end
    end

    property "negative float -> {:unary, :bang, {:unary, :minus, {:literal, :number}}}" do
      check all(float <- float(max: -0.0)) do
        literal = abs(float)

        assert {:ok,
                {:unary_expr, %Token{type: :bang},
                 {:unary_expr, %Token{type: :minus},
                  {:literal_expr, %Token{type: :number, literal: ^literal}}}}} =
                 Parser.parse_expr("!#{float}")
      end
    end

    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "!#{inspect(input)} -> {:unary, :bang, {:literal, #{inspect(literal)}}}" do
        assert {:ok, {:unary_expr, %Token{type: :bang}, {:literal_expr, unquote(literal)}}} =
                 Parser.parse_expr("!#{unquote(input)}")
      end
    end

    property "string -> {:literal, :string}" do
      check all(string <- string(:printable)) do
        assert {:ok,
                {
                  :unary_expr,
                  %Token{type: :bang},
                  {:literal_expr, %Token{type: :string, literal: ^string}}
                }} =
                 Parser.parse_expr("!\"#{string}\"")
      end
    end

    property "non-negative integer -> {:literal, :number}" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok,
                {:unary_expr, %Token{type: :bang},
                 {:literal_expr, %Token{type: :number, literal: ^literal}}}} =
                 Parser.parse_expr("!#{int}")
      end
    end

    property "non-negative float -> {:literal, :number}" do
      check all(float <- float(min: 0.0)) do
        assert {:ok,
                {:unary_expr, %Token{type: :bang},
                 {:literal_expr, %Token{type: :number, literal: ^float}}}} =
                 Parser.parse_expr("!#{float}")
      end
    end
  end

  describe "parse_expr/1: primary group expressions" do
    for {input, literal} <- [
          {"true", true},
          {"false", false},
          {"nil", nil}
        ] do
      test "#{inspect(input)} -> {:group, {:literal, #{inspect(literal)}}}" do
        assert {:ok, {:group_expr, {:literal_expr, unquote(literal)}}} =
                 Parser.parse_expr("(" <> inspect(unquote(literal)) <> ")")
      end
    end

    property "string -> {:literal, :string}" do
      check all(string <- string(:printable)) do
        assert {:ok, {:group_expr, {:literal_expr, %Token{type: :string, literal: ^string}}}} =
                 Parser.parse_expr("(\"#{string}\")")
      end
    end

    property "non-negative integer -> {:literal, :number}" do
      check all(int <- non_negative_integer()) do
        literal = int / 1

        assert {:ok, {:group_expr, {:literal_expr, %Token{type: :number, literal: ^literal}}}} =
                 Parser.parse_expr("(#{int})")
      end
    end

    property "non-negative float -> {:literal, :number}" do
      check all(float <- float(min: 0.0)) do
        assert {:ok, {:group_expr, {:literal_expr, %Token{type: :number, literal: ^float}}}} =
                 Parser.parse_expr("(#{float})")
      end
    end
  end

  describe "parse_expr/1: binary equality, comparison, term, and factor expressions" do
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
      test "left #{operator} right -> {:binary_expr, left, #{token}, right}" do
        assert {:ok,
                {:binary_expr, {:var_expr, %Token{type: :identifier, lexeme: "left"}},
                 %Token{type: unquote(token)},
                 {:var_expr, %Token{type: :identifier, lexeme: "right"}}}} =
                 Parser.parse_expr("left #{unquote(operator)} right")
      end

      test "nil #{operator} number -> {:binary_expr, {:literal, nil}, #{token}, {:literal, number}}" do
        assert {:ok,
                {:binary_expr, {:literal_expr, nil}, %Token{type: unquote(token)},
                 {:literal_expr, %Token{type: :number, literal: 5.0}}}} =
                 Parser.parse_expr("nil #{unquote(operator)} 5")
      end
    end
  end

  describe "parse_expr/1: logical expressions" do
    test "left or right" do
      assert {:ok, {:logical_expr, {:var_expr, _}, %Token{type: :or}, {:var_expr, _}}} =
               Parser.parse_expr("left or right")
    end

    test "left and right" do
      assert {:ok, {:logical_expr, {:var_expr, _}, %Token{type: :and}, {:var_expr, _}}} =
               Parser.parse_expr("left and right")
    end
  end
end
