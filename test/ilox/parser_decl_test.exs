defmodule Ilox.ParserDeclTest do
  use ExUnit.Case, async: true

  alias Ilox.Parser
  alias Ilox.Token

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

    test "fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);" do
      assert {:ok,
              [
                {:function, %Token{lexeme: "add"},
                 [%Token{lexeme: "a"}, %Token{lexeme: "b"}, %Token{lexeme: "c"}], 3,
                 {:block,
                  [
                    {:return_stmt, %Token{type: :return},
                     {:binary_expr,
                      {:binary_expr, {:var_expr, %Token{lexeme: "a"}}, %Token{type: :plus},
                       {:var_expr, %Token{lexeme: "b"}}}, %Token{type: :plus},
                      {:var_expr, %Token{lexeme: "c"}}}}
                  ]}},
                {:print_stmt,
                 {:call, {:var_expr, %Token{lexeme: "add"}},
                  [
                    {:literal_expr, %Token{literal: 1.0}},
                    {:literal_expr, %Token{literal: 2.0}},
                    {:literal_expr, %Token{literal: 3.0}}
                  ], 3, %Token{type: :right_paren}}}
              ]} =
               Parser.parse("""
               fun add(a, b, c) {
                 return a + b + c;
               }

               print add(1, 2, 3);
               """)
    end

    test "closures" do
      src = """
      fun makeCounter() {
        var i = 0;

        fun count() {
          i = i + 1;
          print i;
        }
      }

      var counter1 = makeCounter();
      var counter2 = makeCounter();

      counter1();
      counter2();
      counter1();
      counter2();
      """

      assert {:ok,
              [
                {:function, %Token{lexeme: "makeCounter"}, [], 0,
                 {:block,
                  [
                    {:var_decl, %Token{lexeme: "i"}, {:literal_expr, %Token{literal: +0.0}}},
                    {:function, %Token{lexeme: "count"}, [], 0,
                     {:block,
                      [
                        {:expr_stmt,
                         {:assign_expr, %Token{lexeme: "i"},
                          {:binary_expr, {:var_expr, %Token{lexeme: "i"}}, %Token{type: :plus},
                           {:literal_expr, %Token{literal: 1.0}}}}},
                        {:print_stmt, {:var_expr, %Token{lexeme: "i"}}}
                      ]}}
                  ]}},
                {:var_decl, %Token{lexeme: "counter1"},
                 {:call, {:var_expr, %Token{lexeme: "makeCounter"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:var_decl, %Token{lexeme: "counter2"},
                 {:call, {:var_expr, %Token{lexeme: "makeCounter"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "counter1"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "counter2"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "counter1"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:var_expr, %Token{lexeme: "counter2"}}, [], 0,
                  %Token{type: :right_paren}}}
              ]} = Parser.parse(src)
    end
  end
end
