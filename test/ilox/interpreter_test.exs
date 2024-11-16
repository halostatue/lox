defmodule Ilox.InterpreterTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ilox.StreamData
  import Ilox.SourceTools, only: [run: 1, run: 2]

  @moduletag :focus

  describe "run/2: expression statements" do
    property "value == value;" do
      check all(value <- literal()) do
        assert {:ok, output: []} = run("#{inspect(value)} == #{inspect(value)};")
      end
    end

    property "left < right;" do
      check all(left <- negative_number(), right <- positive_number()) do
        assert {:ok, output: []} = run("#{inspect(left)} < (#{inspect(right)});")
      end
    end

    for operator <- ["<", "<=", ">", ">=", "*", "/", "+", "-"] do
      test "non_number #{operator} non_number; // error" do
        message =
          case unquote(operator) do
            "+" -> "[line 1] Error at '+': Operands must be two numbers or two strings."
            _ -> "[line 1] Error at '#{unquote(operator)}': Operands must be numbers."
          end

        assert {:error, :runtime, errors: ^message, output: []} =
                 run("nil #{unquote(operator)} true;")
      end
    end

    for input <- [true, false, nil, "string"] do
      test "-#{inspect(input)}; // error" do
        assert {:error, :runtime,
                errors: "[line 1] Error at '-': Operand must be a number.", output: []} =
                 run("-#{inspect(unquote(input))};")
      end
    end
  end

  describe "run/2: print statements" do
    test "print 123;" do
      assert {:ok, output: ["123"]} = run("print 123;")
    end

    test "print \"hello\";" do
      assert {:ok, output: ["hello"]} = run("print \"hello\";")
    end
  end

  describe "run/1: block statements" do
    test "one block" do
      assert {:ok, output: ~w[1 2 3 3]} =
               run("""
               var value = 1;
               print value;
               { value = 2; print value; value = 3; print value; }
               print value;
               """)
    end

    test "two blocks" do
      assert {:ok, output: ~w[1 2 3 3]} =
               run("""
               var value = 1;
               print value;
               { value = 2; print value; }
               { value = 3; print value; }
               print value;
               """)
    end

    test "nested blocks 1" do
      assert {:error, :runtime,
              errors: "[line 13] Error: Undefined variable 'value'.", output: ~w[1 2 1]} =
               run("""
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
      assert {:ok,
              output: [
                "inner a",
                "outer b",
                "global c",
                "outer a",
                "outer b",
                "global c",
                "global a",
                "global b",
                "global c"
              ]} =
               run("""
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

  describe "run/1: if statements" do
    test "if" do
      source = """
      if (:cond)
        print 2;
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: []} = run(source, cond: "false")
    end

    test "if block" do
      source = """
      if (:cond) {
        print 2;
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: []} = run(source, cond: "false")
    end

    test "if/else" do
      source = """
      if (:cond)
        print 2;
      else
        print 3;
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: ["3"]} = run(source, cond: "false")
    end

    test "if/else (block)" do
      source = """
      if (:cond) {
        print 2;
      } else {
        print 3;
      }
      """

      assert {:ok, output: ["2"]} = run(source, cond: "true")
      assert {:ok, output: ["3"]} = run(source, cond: "false")
    end

    test "if if/else" do
      # This is pathological. Do not do this.
      source = """
      if (:cond1) if (:cond2) print 2; else print 3;
      """

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "false")
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

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: []} = run(source, cond1: "false", cond2: "false")
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

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "false")
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

      assert {:ok, output: ["2"]} = run(source, cond1: "true", cond2: "true")
      assert {:ok, output: ["3"]} = run(source, cond1: "true", cond2: "false")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "true")
      assert {:ok, output: ["4"]} = run(source, cond1: "false", cond2: "false")
    end
  end

  describe "run/1: while statement" do
    test "while statement" do
      source = """
      var a = 3 ;
      while (a > 1) {
        print a;
        a = a - 1;
       }
      """

      assert {:ok, output: ["3", "2"]} = run(source)
    end
  end

  describe "run/1: for statement" do
    test "for (var a = 2; a > 0; a = a - 1) print a;" do
      assert {:ok, output: ["2", "1"]} =
               run("""
               for (var a = 2; a > 0; a = a - 1)
                 print a;
               """)
    end

    test "for (var a = 3; a > 0; a = a - 1) { print a; }" do
      assert {:ok, output: ["3", "2", "1"]} =
               run("""
               for (var a = 3; a > 0; a = a - 1) {
                 print a;
               }
               """)
    end
  end

  describe "run/2: logical operators" do
    test "print \"hi\" or 2;" do
      assert {:ok, output: ["hi"]} = run("print \"hi\" or 2;")
    end

    test "print nil or \"yes\";" do
      assert {:ok, output: ["yes"]} = run("print nil or \"yes\";")
    end
  end

  describe "run/2: function calls" do
    test "print clock;" do
      assert {:ok, output: ["<native fn>"]} = run("print clock;")
    end

    test "print clock();" do
      assert {:ok, output: [output]} = run("print clock();")
      assert output =~ ~r/^-?\d+(?:\.\d+)?(?:e\d+)?$/
    end

    test "fun add(a, b, c) { print a + b + c; } add(1, 2, 3);" do
      assert {:ok, output: ["6"]} =
               run("fun add(a, b, c) { print a + b + c; } add(1, 2, 3);")
    end

    test "fun add(a, b, c) { print a + b + c; } print add;" do
      assert {:ok, output: ["<fn add>"]} =
               run("fun add(a, b, c) { print a + b + c; } print add;")
    end

    test "fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);" do
      assert {:ok, output: ["6"]} =
               run("fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);")
    end

    test "fun add(a, b, c) { print a + b + c; } print add(1, 2, 3);" do
      assert {:ok, output: ["6", ""]} =
               run("""
               fun add(a, b, c) {
               print a + b + c;
               }

               print add(1, 2, 3);
               """)
    end
  end

  describe "run/2: fibonacci" do
    @tag timeout: 3000
    test "once" do
      src = """
      fun fib(n) {
        if (n < 2) return n;

        return fib(n - 1) + fib(n - 2);
      }

      print fib(7);
      """

      assert {:ok, output: ["13"]} = run(src)
    end

    @tag timeout: 3000
    test "first twenty" do
      src = """
      fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
      }

      for (var i = 0; i < 20; i = i + 1) {
        print fib(i);
      }
      """

      assert {:ok,
              output: [
                "0",
                "1",
                "1",
                "2",
                "3",
                "5",
                "8",
                "13",
                "21",
                "34",
                "55",
                "89",
                "144",
                "233",
                "377",
                "610",
                "987",
                "1597",
                "2584",
                "4181"
              ]} = run(src)
    end
  end

  describe "run/2: closure" do
    test "counter" do
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

      counter1();
      counter1();
      """

      assert {:ok, output: ["1", "2"]} = run(src)
    end

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

      assert {:ok, output: ["1", "1", "2", "2"]} = run(src)
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

      assert {:ok, output: ["global", "global"]} = run(src)
    end
  end
end
