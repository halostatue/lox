defmodule Ilox.ParserDeclTest do
  use ExUnit.Case, async: true

  alias Ilox.Token
  alias Ilox.Parser

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

  describe "parse/1: function declaration" do
    test "fun one() { print 1; }" do
      assert {:ok,
              [
                {:function, %Token{lexeme: "one"}, [], 0,
                 {:block, [{:print_stmt, {:literal_expr, %Token{literal: 1.0}}}]}}
              ]} = Parser.parse("fun one() { print 1; }")
    end

    test "fun one() { print 1; } one();" do
      assert {:ok,
              [
                {:function, %Token{lexeme: "one"}, [], 0,
                 {:block, [{:print_stmt, {:literal_expr, %Token{literal: 1.0}}}]}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "one"}}, [], 0, %Token{type: :right_paren}}}
              ]} = Parser.parse("fun one() { print 1; } one();")
    end

    test "fun add(a, b, c) { print a + b + c; } add(1, 2, 3);" do
      assert {:ok,
              [
                {:function, %Token{lexeme: "add"},
                 [%Token{lexeme: "a"}, %Token{lexeme: "b"}, %Token{lexeme: "c"}], 3,
                 {:block,
                  [
                    {:print_stmt,
                     {:binary_expr,
                      {:binary_expr, {:var_expr, %Token{lexeme: "a"}}, %Token{type: :plus},
                       {:var_expr, %Token{lexeme: "b"}}}, %Token{type: :plus},
                      {:var_expr, %Token{lexeme: "c"}}}}
                  ]}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "add"}},
                  [
                    {:literal_expr, %Token{literal: 1.0}},
                    {:literal_expr, %Token{literal: 2.0}},
                    {:literal_expr, %Token{literal: 3.0}}
                  ], 3, %Token{type: :right_paren}}}
              ]} = Parser.parse("fun add(a, b, c) { print a + b + c; } add(1, 2, 3);")
    end
  end
end
