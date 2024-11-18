defmodule Ilox.ResolverTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ilox.Token

  import Ilox.StreamData
  import Ilox.SourceTools, only: [resolve: 1, resolve: 2]

  @blank %{locals: []}

  describe "resolve/2: expression statements" do
    property "value == value;" do
      check all(value <- literal()) do
        assert @blank == resolve("#{inspect(value)} == #{inspect(value)};")
      end
    end

    property "left < right;" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert @blank == resolve("#{inspect(left)} < (#{inspect(right)});")
      end
    end

    for operator <- ["<", "<=", ">", ">=", "*", "/", "+", "-"] do
      test "non_number #{operator} non_number; // error" do
        assert @blank == resolve("nil #{unquote(operator)} true;")
      end
    end

    for input <- [true, false, nil, "string"] do
      test "-#{inspect(input)}; // error" do
        assert @blank == resolve("-#{inspect(unquote(input))};")
      end
    end
  end

  describe "resolve/2: print statements" do
    test "print 123;" do
      assert @blank == resolve("print 123;")
    end

    test "print \"hello\";" do
      assert @blank == resolve("print \"hello\";")
    end
  end

  describe "resolve/1: block statements" do
    test "one block" do
      assert %{locals: []} =
               resolve("""
               var value = 1;
               print value;
               { value = 2; print value; value = 3; print value; }
               print value;
               """)
    end

    test "two blocks" do
      assert %{locals: []} =
               resolve("""
               var value = 1;
               print value;
               { value = 2; print value; }
               { value = 3; print value; }
               print value;
               """)
    end

    test "nested blocks 1" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 3, lexeme: "value"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 7, lexeme: "value"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 10, lexeme: "value"}}, 0}
               ]
             } =
               resolve("""
               {
                 var value = 1;
                 print value;

                 {
                   var value = 2;
                   print value;
                 }

                 print value;
               }

               print value;
               """)
    end

    test "scope" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 9, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 10, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 13, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 14, lexeme: "b"}}, 0}
               ]
             } =
               resolve("""
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

    test "nested scope" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 10, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 11, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 12, lexeme: "c"}}, 2},
                 {{:variable, %Token{type: :identifier, line: 14, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 15, lexeme: "b"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 16, lexeme: "c"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 18, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 19, lexeme: "b"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 20, lexeme: "c"}}, 0}
               ]
             } =
               resolve("""
               {
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
               }
               """)
    end
  end

  describe "resolve/1: if statements" do
    test "if" do
      source = """
      if (:cond)
        print 2;
      """

      assert @blank == resolve(source, cond: "true")
      assert @blank == resolve(source, cond: "false")
    end

    test "if block" do
      source = """
      if (:cond) {
        print 2;
      }
      """

      assert %{locals: []} == resolve(source, cond: "true")
      assert %{locals: []} == resolve(source, cond: "false")
    end

    test "if/else" do
      source = """
      if (:cond)
        print 2;
      else
        print 3;
      """

      assert @blank == resolve(source, cond: "true")
      assert @blank == resolve(source, cond: "false")
    end

    test "if/else (block)" do
      source = """
      if (:cond) {
        print 2;
      } else {
        print 3;
      }
      """

      assert %{locals: []} == resolve(source, cond: "true")
      assert %{locals: []} == resolve(source, cond: "false")
    end

    test "if if/else" do
      # This is pathological. Do not do this.
      source = """
      if (a) if (b) print c; else print d;
      """

      assert @blank == resolve(source)
    end

    test "if if/else block" do
      source = """
      if (:cond1) {
        if (:cond2) {
          print 2;
        } else {
          print 3;
        }
      }
      """

      result = %{locals: []}

      assert result == resolve(source, cond1: "true", cond2: "true")
      assert result == resolve(source, cond1: "true", cond2: "false")
      assert result == resolve(source, cond1: "false", cond2: "true")
      assert result == resolve(source, cond1: "false", cond2: "false")
    end

    test "if if/else else" do
      # This is pathological. Do not do this.
      source = """
      if (:cond1)
        if (:cond2)
          print 2;
        else
          print 3;
      else
        print 4;
      """

      assert @blank == resolve(source, cond1: "true", cond2: "true")
      assert @blank == resolve(source, cond1: "true", cond2: "false")
      assert @blank == resolve(source, cond1: "false", cond2: "true")
      assert @blank == resolve(source, cond1: "false", cond2: "false")
    end

    test "if if/else else block" do
      source = """
      if (:cond1) {
        if (:cond2) {
          print 2;
        } else {
          print 3;
        }
      } else {
        print 4;
      }
      """

      result = %{locals: []}

      assert result == resolve(source, cond1: "true", cond2: "true")
      assert result == resolve(source, cond1: "true", cond2: "false")
      assert result == resolve(source, cond1: "false", cond2: "true")
      assert result == resolve(source, cond1: "false", cond2: "false")
    end
  end

  describe "resolve/1: while statement" do
    test "while statement" do
      source = """
      var a = 3;
      while (a > 1) {
        print a;
        a = a - 1;
      }
      """

      assert %{locals: []} == resolve(source)
    end

    test "while statement in block" do
      source = """
      var a = 3;

      {
        while (a > 1) {
          print a;
          a = a - 1;
        }
      }
      """

      assert %{locals: []} == resolve(source)
    end
  end

  describe "resolve/1: for statement" do
    test "for (var a = 2; a > 0; a = a - 1) print a;" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 1, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 1, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:assignment, %Token{type: :identifier, line: 1, lexeme: "a"},
                   {:binary, {:variable, %Token{type: :identifier, line: 1, lexeme: "a"}},
                    %Token{type: :minus, line: 1, lexeme: "-"},
                    {:literal, %Token{type: :number, line: 1, lexeme: "1", literal: 1.0}}}}, 1}
               ]
             } =
               resolve("""
               for (var a = 2; a > 0; a = a - 1)
                 print a;
               """)
    end

    test "for (var a = 3; a > 0; a = a - 1) { print a; }" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 1, lexeme: "a"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 1, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:assignment, %Token{type: :identifier, line: 1, lexeme: "a"},
                   {:binary, {:variable, %Token{type: :identifier, line: 1, lexeme: "a"}},
                    %Token{type: :minus, line: 1, lexeme: "-"},
                    {:literal, %Token{type: :number, line: 1, lexeme: "1", literal: 1.0}}}}, 1}
               ]
             } =
               resolve("""
               for (var a = 3; a > 0; a = a - 1) {
                 print a;
               }
               """)
    end
  end

  describe "resolve/2: logical operators" do
    test "print \"hi\" or 2;" do
      assert @blank == resolve("print \"hi\" or 2;")
    end

    test "print nil or \"yes\";" do
      assert @blank == resolve("print nil or \"yes\";")
    end
  end

  describe "resolve/2: function calls" do
    test "print clock;" do
      assert @blank == resolve("print clock;")
    end

    test "print clock();" do
      assert @blank == resolve("print clock();")
    end

    test "fun add(a, b, c) { print a + b + c; } add(1, 2, 3);" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "c"}}, 1}
               ]
             } =
               resolve("""
               fun add(a, b, c) {
                 print a + b + c;
               }

               add(1, 2, 3);
               """)
    end

    test "fun add(a, b, c) { print a + b + c; } print add;" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "c"}}, 1}
               ]
             } =
               resolve("""
               fun add(a, b, c) {
                 print a + b + c;
               }

               print add;
               """)
    end

    test "fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "c"}}, 1}
               ]
             } =
               resolve("""
               fun add(a, b, c) {
                 return a + b + c;
               }

               print add(1, 2, 3);
               """)
    end

    test "fun add(a, b, c) { print a + b + c; } print add(1, 2, 3);" do
      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "a"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "b"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "c"}}, 1}
               ]
             } =
               resolve("""
               fun add(a, b, c) {
                 print a + b + c;
               }

               print add(1, 2, 3);
               """)
    end
  end

  describe "resolve/2: fibonacci" do
    @tag timeout: 3000
    test "fibonacci" do
      src = """
      fun fib(n) {
        if (n < 2) return n;

        return fib(n - 1) + fib(n - 2);
      }

      print fib(7);
      """

      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 4, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 4, lexeme: "n"}}, 1}
               ]
             } = resolve(src)
    end

    @tag timeout: 3000
    test "fibonacci loop" do
      src = """
      fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
      }

      for (var i = 0; i < 20; i = i + 1) {
        print fib(i);
      }
      """

      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 2, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 3, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 3, lexeme: "n"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 6, lexeme: "i"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 6, lexeme: "i"}}, 1},
                 {{:variable, %Token{type: :identifier, line: 7, lexeme: "i"}}, 1},
                 {{:assignment, %Token{type: :identifier, line: 6, lexeme: "i"},
                   {:binary, {:variable, %Token{type: :identifier, line: 6, lexeme: "i"}},
                    %Token{type: :plus, line: 6, lexeme: "+"},
                    {:literal, %Token{type: :number, line: 6, lexeme: "1", literal: 1.0}}}}, 1}
               ]
             } = resolve(src)
    end
  end

  describe "resolve/2: closure" do
    test "counters" do
      src = """
      fun makeCounter() {
        var i = 0;

        fun count() {
          i = i + 1;
          print i;
        }

        return count;
      }

      var counter1 = makeCounter();
      var counter2 = makeCounter();

      counter1();
      counter2();
      counter1();
      counter2();
      """

      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 5, lexeme: "i"}}, 2},
                 {{:variable, %Token{type: :identifier, line: 6, lexeme: "i"}}, 2},
                 {{:variable, %Token{type: :identifier, line: 9, lexeme: "count"}}, 0},
                 {{:assignment, %Token{type: :identifier, line: 5, lexeme: "i"},
                   {:binary, {:variable, %Token{type: :identifier, line: 5, lexeme: "i"}},
                    %Token{type: :plus, line: 5, lexeme: "+"},
                    {:literal, %Token{type: :number, line: 5, lexeme: "1", literal: 1.0}}}}, 2}
               ]
             } = resolve(src)
    end

    test "scope resolution" do
      src = """
      var a = "global";

      {
        fun showA() {
          print a;
        }

        showA();
        var a = "block";
        showA();
      }
      """

      assert %{
               locals: [
                 {{:variable, %Token{type: :identifier, line: 8, lexeme: "showA"}}, 0},
                 {{:variable, %Token{type: :identifier, line: 10, lexeme: "showA"}}, 0}
               ]
             } = resolve(src)
    end
  end

  describe "errors" do
    test "read in initializer" do
      assert {:error, :resolver,
              [
                [
                  token: %Token{lexeme: "a"},
                  message: "Can't read local variable 'a' in its own initializer."
                ]
              ]} =
               resolve("""
               var a = "foo";
               {
                 var a = a;
               }
               """)
    end

    test "redeclaration in local scope" do
      assert {:error, :resolver,
              [
                [token: %Token{lexeme: "a"}, message: "Already declared 'a' in this scope."]
              ]} =
               resolve("""
               {
                 var a;
                 var a = "redeclared";
               }
               """)
    end

    test "redeclaration in function scope" do
      assert {:error, :resolver,
              [
                [token: %Token{lexeme: "a"}, message: "Already declared 'a' in this scope."]
              ]} =
               resolve("""
               fun foo() {
                 var a;
                 var a = "redeclared";
               }
               """)
    end

    test "return from top-level scope" do
      assert {:error, :resolver,
              [
                [token: %Token{type: :return}, message: "Can't return from top-level code."]
              ]} = resolve("return;")
    end

    test "return from nested scope" do
      assert {:error, :resolver,
              [
                [token: %Token{type: :return}, message: "Can't return from top-level code."]
              ]} = resolve("{ return; }")
    end

    test "return value from initializer" do
      assert {:error, :resolver,
              [
                [
                  token: %Token{type: :return},
                  message: "Can't return a value from an initializer."
                ]
              ]} =
               resolve("""
               class Foo {
                 init() {
                   return "invalid";
                 }
               }
               """)
    end

    test "self-inheritance" do
      assert {:error, :resolver,
              [
                [
                  token: %Token{type: :identifier, lexeme: "Foo"},
                  message: "A class can't inherit from itself."
                ]
              ]} = resolve("class Foo < Foo {}")
    end
  end
end
