defmodule Ilox.InterpreterClassTest do
  use ExUnit.Case, async: true

  import Ilox.SourceTools, only: [run: 1, run: 2]

  describe "run/2: class basics" do
    test "print class" do
      assert {:ok, output: ["DevonshireCream"]} =
               run("""
               class DevonshireCream {}
               print DevonshireCream;
               """)
    end

    test "test basic class" do
      assert {:ok, output: ["DevonshireCream instance"]} =
               run("""
               class DevonshireCream {}
               var cream = DevonshireCream();
               print cream;
               """)
    end

    test "test basic class properties" do
      assert {:ok, output: ["vanilla"]} =
               run("""
               class DevonshireCream {}
               var cream = DevonshireCream();
               cream.flavour = "vanilla";
               print cream.flavour;
               """)
    end

    test "test basic class methods" do
      assert {:ok, output: ["Yum, vanilla!"]} =
               run("""
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
      assert {:ok, output: ["Egotist instance"]} =
               run("""
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
      assert {:ok, output: ["The German chocolate cake is delicious!"]} =
               run("""
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
      assert {:ok, output: ["Thing instance"]} =
               run("""
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
