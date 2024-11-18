defmodule Ilox.ParserClassTest do
  use ExUnit.Case, async: true

  alias Ilox.Parser
  alias Ilox.Token

  @moduletag :focus

  describe "parse/1: class basics" do
    test "declare" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "DevonshireCream"}, nil,
                 [
                   {:fun_decl, %Token{type: :identifier, lexeme: "eat"}, [], 0,
                    {:block, [{:print_stmt, {:literal, %Token{literal: "Yum!"}}}]}}
                 ]}
              ]} =
               Parser.parse("""
               class DevonshireCream {
                 eat() {
                    print "Yum!";
                 }
               }
               """)
    end

    test "print class" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "DevonshireCream"}, nil, []},
                {:print_stmt, {:variable, %Token{type: :identifier, lexeme: "DevonshireCream"}}}
              ]} =
               Parser.parse("""
               class DevonshireCream {}
               print DevonshireCream;
               """)
    end

    test "test basic class" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "DevonshireCream"}, nil, []},
                {:var_decl, %Token{type: :identifier, lexeme: "cream"},
                 {:call, {:variable, %Token{type: :identifier, lexeme: "DevonshireCream"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:print_stmt, {:variable, %Token{type: :identifier, lexeme: "cream"}}}
              ]} =
               Parser.parse("""
               class DevonshireCream {}
               var cream = DevonshireCream();
               print cream;
               """)
    end

    test "test basic class properties" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "DevonshireCream"}, nil, []},
                {:var_decl, %Token{type: :identifier, lexeme: "cream"},
                 {:call, {:variable, %Token{type: :identifier, lexeme: "DevonshireCream"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:set, {:variable, %Token{type: :identifier, lexeme: "cream"}},
                  %Token{type: :identifier, lexeme: "flavour"},
                  {:literal, %Token{literal: "vanilla"}}}},
                {:print_stmt,
                 {:get, {:variable, %Token{type: :identifier, lexeme: "cream"}},
                  %Token{type: :identifier, lexeme: "flavour"}}}
              ]} =
               Parser.parse("""
               class DevonshireCream {}
               var cream = DevonshireCream();
               cream.flavour = "vanilla";
               print cream.flavour;
               """)
    end

    test "test basic class methods" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "DevonshireCream"}, nil,
                 [
                   {:fun_decl, %Token{type: :identifier, lexeme: "eat"}, [], 0,
                    {:block,
                     [
                       {:print_stmt, {:literal, %Token{literal: "Yum, vanilla!"}}}
                     ]}}
                 ]},
                {:var_decl, %Token{type: :identifier, lexeme: "cream"},
                 {:call, {:variable, %Token{type: :identifier, lexeme: "DevonshireCream"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call,
                  {:get, {:variable, %Token{type: :identifier, lexeme: "cream"}},
                   %Token{type: :identifier, lexeme: "eat"}}, [], 0, %Token{type: :right_paren}}}
              ]} =
               Parser.parse("""
               class DevonshireCream {
                 eat() {
                   print "Yum, vanilla!";
                 }
               }

               var cream = DevonshireCream();
               cream.eat();
               """)
    end

    test "test this reference" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "Egotist"}, nil,
                 [
                   {:fun_decl, %Token{type: :identifier, lexeme: "speak"}, [], 0,
                    {:block,
                     [
                       {:print_stmt, {:this, %Token{type: :this}}}
                     ]}}
                 ]},
                {:var_decl, %Token{type: :identifier, lexeme: "method"},
                 {:get,
                  {:call, {:variable, %Token{type: :identifier, lexeme: "Egotist"}}, [], 0,
                   %Token{type: :right_paren}}, %Token{type: :identifier, lexeme: "speak"}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{type: :identifier, lexeme: "method"}}, [], 0,
                  %Token{type: :right_paren}}}
              ]} =
               Parser.parse("""
               class Egotist {
                 speak() {
                   print this;
                 }
               }

               var method = Egotist().speak;
               method();
               """)
    end

    test "test this field reference" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "Cake"}, nil,
                 [
                   {:fun_decl, %Token{type: :identifier, lexeme: "taste"}, [], 0,
                    {
                      :block,
                      [
                        {:var_decl, %Token{type: :identifier, lexeme: "adjective"},
                         {:literal, %Token{literal: "delicious"}}},
                        {:print_stmt,
                         {:binary,
                          {:binary,
                           {:binary,
                            {:binary, {:literal, %Token{literal: "The "}}, %Token{type: :plus},
                             {:get, {:this, %Token{type: :this}},
                              %Token{type: :identifier, lexeme: "flavour"}}}, %Token{type: :plus},
                            {:literal, %Token{literal: " cake is "}}}, %Token{type: :plus},
                           {:variable, %Token{type: :identifier, lexeme: "adjective"}}},
                          %Token{type: :plus}, {:literal, %Token{literal: "!"}}}}
                      ]
                    }}
                 ]},
                {:var_decl, %Token{type: :identifier, lexeme: "cake"},
                 {:call, {:variable, %Token{type: :identifier, lexeme: "Cake"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:set, {:variable, %Token{type: :identifier, lexeme: "cake"}},
                  %Token{type: :identifier, lexeme: "flavour"},
                  {:literal, %Token{literal: "German chocolate"}}}},
                {:expr_stmt,
                 {:call,
                  {:get, {:variable, %Token{type: :identifier, lexeme: "cake"}},
                   %Token{type: :identifier, lexeme: "taste"}}, [], 0,
                  %Token{type: :right_paren}}}
              ]} =
               Parser.parse("""
               class Cake {
                 taste() {
                   var adjective = "delicious";
                   print "The " + this.flavour + " cake is " + adjective + "!";
                 }
               }

               var cake = Cake();
               cake.flavour = "German chocolate";
               cake.taste();
               """)
    end

    test "test this callback" do
      assert {:ok,
              [
                {:class_decl, %Token{type: :identifier, lexeme: "Thing"}, nil,
                 [
                   {:fun_decl, %Token{type: :identifier, lexeme: "getCallback"}, [], 0,
                    {:block,
                     [
                       {:fun_decl, %Token{type: :identifier, lexeme: "localFunction"}, [], 0,
                        {:block, [print_stmt: {:this, %Token{type: :this}}]}},
                       {:return_stmt, %Token{type: :return},
                        {:variable, %Token{type: :identifier, lexeme: "localFunction"}}}
                     ]}}
                 ]},
                {:var_decl, %Token{type: :identifier, lexeme: "callback"},
                 {:call,
                  {:get,
                   {:call, {:variable, %Token{type: :identifier, lexeme: "Thing"}}, [], 0,
                    %Token{type: :right_paren}},
                   %Token{type: :identifier, lexeme: "getCallback"}}, [], 0,
                  %Token{type: :right_paren}}},
                {:expr_stmt,
                 {:call, {:variable, %Token{type: :identifier, lexeme: "callback"}}, [], 0,
                  %Token{type: :right_paren}}}
              ]} =
               Parser.parse("""
               class Thing {
                 getCallback() {
                   fun localFunction() {
                     print this;
                   }

                   return localFunction;
                 }
               }

               var callback = Thing().getCallback();
               callback();
               """)
    end
  end
end
