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
                 {:assign_expr, %Token{type: :identifier, lexeme: "left"},
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

  describe "parse/1: block statements" do
    test "one block" do
      assert {:ok,
              [
                {:print_stmt, {:literal_expr, %Token{type: :number, literal: 1.0}}},
                {:block,
                 [
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 2.0}}},
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 3.0}}}
                 ]},
                {:print_stmt, {:literal_expr, %Token{type: :number, literal: 4.0}}}
              ]} =
               Parser.parse("print 1; { print 2; print 3; } print 4;")
    end

    test "two blocks" do
      assert {:ok,
              [
                {:print_stmt, {:literal_expr, %Token{type: :number, literal: 1.0}}},
                {:block, [{:print_stmt, {:literal_expr, %Token{type: :number, literal: 2.0}}}]},
                {:block, [{:print_stmt, {:literal_expr, %Token{type: :number, literal: 3.0}}}]},
                {:print_stmt, {:literal_expr, %Token{type: :number, literal: 4.0}}}
              ]} =
               Parser.parse("print 1; { print 2; } { print 3; } print 4;")
    end

    test "nested blocks 1" do
      assert {:ok,
              [
                {:block,
                 [
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 1.0}}},
                   {:block,
                    [
                      {:print_stmt, {:literal_expr, %Token{type: :number, literal: 2.0}}},
                      {:print_stmt, {:literal_expr, %Token{type: :number, literal: 3.0}}}
                    ]},
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 4.0}}}
                 ]}
              ]} =
               Parser.parse("{ print 1; { print 2; print 3; } print 4; }")
    end

    test "nested blocks 2" do
      assert {:ok,
              [
                {:block,
                 [
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 1.0}}},
                   {:block,
                    [
                      {:print_stmt, {:literal_expr, %Token{type: :number, literal: 2.0}}},
                      {:block,
                       [
                         {:print_stmt, {:literal_expr, %Token{type: :number, literal: 3.0}}}
                       ]},
                      {:print_stmt, {:literal_expr, %Token{type: :number, literal: 4.0}}}
                    ]},
                   {:print_stmt, {:literal_expr, %Token{type: :number, literal: 5.0}}}
                 ]}
              ]} =
               Parser.parse("{ print 1; { print 2; { print 3; } print 4; } print 5; }")
    end

    test "scope" do
      assert {:ok,
              [
                {:var_decl, %Token{lexeme: "a"}, {:literal_expr, %Token{type: :string}}},
                {:var_decl, %Token{lexeme: "b"}, {:literal_expr, %Token{type: :string}}},
                {:var_decl, %Token{lexeme: "c"}, {:literal_expr, %Token{type: :string}}},
                {:block,
                 [
                   {:var_decl, %Token{lexeme: "a"}, {:literal_expr, %Token{type: :string}}},
                   {:var_decl, %Token{lexeme: "b"}, {:literal_expr, %Token{type: :string}}},
                   {:block,
                    [
                      {:var_decl, %Token{lexeme: "a"}, {:literal_expr, %Token{type: :string}}},
                      {:print_stmt, {:var_expr, %Token{lexeme: "a"}}},
                      {:print_stmt, {:var_expr, %Token{lexeme: "b"}}},
                      {:print_stmt, {:var_expr, %Token{lexeme: "c"}}}
                    ]},
                   {:print_stmt, {:var_expr, %Token{lexeme: "a"}}},
                   {:print_stmt, {:var_expr, %Token{lexeme: "b"}}},
                   {:print_stmt, {:var_expr, %Token{lexeme: "c"}}}
                 ]},
                {:print_stmt, {:var_expr, %Token{lexeme: "a"}}},
                {:print_stmt, {:var_expr, %Token{lexeme: "b"}}},
                {:print_stmt, {:var_expr, %Token{lexeme: "c"}}}
              ]} =
               Parser.parse("""
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
