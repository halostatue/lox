defmodule Ilox.ParserDeclTest do
  use ExUnit.Case, async: true

  alias Ilox.Parser
  alias Ilox.Token

  describe "parse/1: variable declaration and initialization" do
    test "var left = right;" do
      assert {:ok,
              [
                {:var_decl, %Token{type: :identifier, lexeme: "left"},
                 {:variable, %Token{type: :identifier, lexeme: "right"}}}
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
                {:fun_decl, %Token{lexeme: "one"}, [], 0,
                 {:block, [{:print_stmt, {:literal, %Token{literal: 1.0}}}]}}
              ]} = Parser.parse("fun one() { print 1; }")
    end

    test "fun one() { print 1; } one();" do
      assert {:ok,
              [
                {:fun_decl, %Token{lexeme: "one"}, [], 0,
                 {:block, [{:print_stmt, {:literal, %Token{literal: 1.0}}}]}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "one"}}, [], 0, %Token{type: :right_paren}}}
              ]} = Parser.parse("fun one() { print 1; } one();")
    end

    test "fun add(a, b, c) { print a + b + c; } add(1, 2, 3);" do
      assert {:ok,
              [
                {:fun_decl, %Token{lexeme: "add"},
                 [%Token{lexeme: "a"}, %Token{lexeme: "b"}, %Token{lexeme: "c"}], 3,
                 {:block,
                  [
                    {:print_stmt,
                     {:binary,
                      {:binary, {:variable, %Token{lexeme: "a"}}, %Token{type: :plus},
                       {:variable, %Token{lexeme: "b"}}}, %Token{type: :plus},
                      {:variable, %Token{lexeme: "c"}}}}
                  ]}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "add"}},
                  [
                    {:literal, %Token{literal: 1.0}},
                    {:literal, %Token{literal: 2.0}},
                    {:literal, %Token{literal: 3.0}}
                  ], 3, %Token{type: :right_paren}}}
              ]} = Parser.parse("fun add(a, b, c) { print a + b + c; } add(1, 2, 3);")
    end

    test "fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);" do
      assert {:ok,
              [
                {:fun_decl, %Token{lexeme: "add"},
                 [%Token{lexeme: "a"}, %Token{lexeme: "b"}, %Token{lexeme: "c"}], 3,
                 {:block,
                  [
                    {:return_stmt, %Token{type: :return},
                     {:binary,
                      {:binary, {:variable, %Token{lexeme: "a"}}, %Token{type: :plus},
                       {:variable, %Token{lexeme: "b"}}}, %Token{type: :plus},
                      {:variable, %Token{lexeme: "c"}}}}
                  ]}},
                {:print_stmt,
                 {:call, {:variable, %Token{lexeme: "add"}},
                  [
                    {:literal, %Token{literal: 1.0}},
                    {:literal, %Token{literal: 2.0}},
                    {:literal, %Token{literal: 3.0}}
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
                {:fun_decl, %Token{lexeme: "makeCounter"}, [], 0,
                 {:block,
                  [
                    {:var_decl, %Token{lexeme: "i"}, {:literal, %Token{literal: +0.0}}},
                    {:fun_decl, %Token{lexeme: "count"}, [], 0,
                     {:block,
                      [
                        {:expr_stmt,
                         {:assignment, %Token{lexeme: "i"},
                          {:binary, {:variable, %Token{lexeme: "i"}}, %Token{type: :plus},
                           {:literal, %Token{literal: 1.0}}}}},
                        {:print_stmt, {:variable, %Token{lexeme: "i"}}}
                      ]}}
                  ]}},
                {:var_decl, %Token{lexeme: "counter1"},
                 {:call, {:variable, %Token{lexeme: "makeCounter"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:var_decl, %Token{lexeme: "counter2"},
                 {:call, {:variable, %Token{lexeme: "makeCounter"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "counter1"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "counter2"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "counter1"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{lexeme: "counter2"}}, [], 0,
                  %Token{type: :right_paren}}}
              ]} = Parser.parse(src)
    end
  end
end
