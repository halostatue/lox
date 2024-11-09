defmodule Ilox.EnvTest do
  use ExUnit.Case, async: true

  alias Ilox.Callable
  alias Ilox.Env
  alias Ilox.Env.Scope
  alias Ilox.Token

  @globals "<globals>"

  describe "new/1" do
    test "new() returns a default module" do
      print = &IO.puts/1
      env = Env.new()

      assert %Env{
               print: ^print,
               scopes: %{
                 @globals => %Scope{
                   values: %{
                     "clock" => %Callable{}
                   }
                 }
               },
               stack: [@globals]
             } = env

      assert Map.keys(env.scopes) == [@globals]
      assert Map.keys(env.scopes[@globals].values) == ["clock", "env"]
    end

    test "new(print: &fake_print/1) overrides the print hook" do
      print = &fake_print/1

      assert %Env{print: ^print} = Env.new(print: &fake_print/1)
    end
  end

  describe "scope management: push_scope/1 and pop_scope/1" do
    test "push_scope/1: a randomly generated scope is created" do
      env =
        Env.new()
        |> Env.push_scope()
        |> Env.push_scope()

      assert 3 == Enum.count(env.scopes)
      assert 3 == Enum.count(env.stack)
      assert Enum.all?(env.stack, &Map.has_key?(env.scopes, &1))

      [l2, l1, @globals] = env.stack
      assert l1 == env.scopes[l2].enclosing_id
      assert @globals == env.scopes[l1].enclosing_id

      assert l1 =~ ~r/^(?:[bfgknprstvxz][aeiou]){3}$/
      assert l2 =~ ~r/^(?:[bfgknprstvxz][aeiou]){3}$/
    end

    test "pop_scope/1: the scope is removed from activity but not destroyed" do
      env =
        Env.new()
        |> Env.push_scope()
        |> Env.push_scope()
        |> Env.pop_scope()
        |> Env.push_scope()
        |> Env.pop_scope()

      assert 4 == Enum.count(env.scopes)
      assert 2 == Enum.count(env.stack)
      assert Enum.all?(env.stack, &Map.has_key?(env.scopes, &1))
    end

    test "brings variables in and out of scope" do
      one = var("one")

      env =
        Env.new()
        |> Env.define(one, 10)
        |> elem(0)
        |> Env.push_scope()
        |> Env.define(one, 15)
        |> elem(0)
        |> Env.push_scope()
        |> Env.define(one, 25)
        |> elem(0)

      assert 25 == Env.get(env, one)

      env = Env.pop_scope(env)
      assert 15 == Env.get(env, one)

      env = Env.pop_scope(env)
      assert 10 == Env.get(env, one)
    end

    test "raises an exception if trying to pop the root scope" do
      assert_raise RuntimeError, "Cannot pop root scope.", fn ->
        Env.pop_scope(Env.new())
      end
    end

    test "raises an exception if the environment is invalid " do
      assert_raise RuntimeError, "Invalid environment.", fn ->
        Env.pop_scope(%Env{print: &IO.puts/1})
      end
    end
  end

  describe "define/3" do
    test "creates a variable that did not previously exist" do
      env = Env.new()

      refute Map.has_key?(env.scopes[@globals].values, "name")
      assert {env, 5} = Env.define(env, var("name"), 5)
      assert Map.has_key?(env.scopes[@globals].values, "name")
      assert 5 == env.scopes[@globals].values["name"]
    end

    test "updates an already existing variable in the same scope" do
      {env, nil} = Env.define(Env.new(), var("name"), nil)

      assert is_nil(env.scopes[@globals].values["name"])
      assert {env, 5} = Env.define(env, var("name"), 5)
      assert 5 == env.scopes[@globals].values["name"]
    end

    test "creates a variable only in the current scope" do
      env = Env.push_scope(Env.new(), "test-scope")

      refute Map.has_key?(env.scopes[@globals].values, "name")
      assert {env, 5} = Env.define(env, var("name"), 5)
      refute Map.has_key?(env.scopes[@globals].values, "name")
      assert Map.has_key?(env.scopes["test-scope"].values, "name")
      assert 5 == env.scopes["test-scope"].values["name"]
    end

    test "redefines a variable in the current scope over an enclosing scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)
        |> Env.push_scope("test-scope")

      assert Map.has_key?(env.scopes[@globals].values, "name")
      assert 3 == env.scopes[@globals].values["name"]
      assert {env, 5} = Env.define(env, var("name"), 5)
      assert Map.has_key?(env.scopes["test-scope"].values, "name")
      assert 5 == env.scopes["test-scope"].values["name"]
    end
  end

  describe "define?/3" do
    test "returns true if the variable is defined in any scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 5)
        |> elem(0)
        |> Env.push_scope()

      assert Env.defined?(env, var("name"))
    end

    test "shortcuts when the active scope is the global scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 5)
        |> elem(0)

      assert Env.defined?(env, var("name"))
      refute Env.defined?(env, var("not-name"))
    end

    test "returns false if the variable is not defined in any scope" do
      env =
        Env.new()
        |> Env.push_scope()

      refute Env.defined?(env, var("name"))
    end

    test "returns false if the variable is not defined in the current scope with recursive false" do
      env =
        Env.new()
        |> Env.define(var("name"), 5)
        |> elem(0)
        |> Env.push_scope()

      refute Env.defined?(env, var("name"), false)
    end

    test "returns true if the variable is defined in the current scope with recursive false" do
      env =
        Env.new()
        |> Env.define(var("name"), 5)
        |> elem(0)
        |> Env.push_scope()
        |> Env.define(var("name"), 5)
        |> elem(0)

      assert Env.defined?(env, var("name"), false)
    end
  end

  describe "get/2" do
    test "undefined variable in a global scope" do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'name'/, fn ->
        Env.get(Env.new(), var("name"))
      end
    end

    test "undefined variable in any scope" do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'name'/, fn ->
        Env.get(Env.push_scope(Env.new()), var("name"))
      end
    end

    test "global variables" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)

      assert 3 = Env.get(env, var("name"))
    end

    test "variable defined in global but not enclosed scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)
        |> Env.push_scope()

      assert 3 = Env.get(env, var("name"))
    end

    test "variable defined in enclosed but not global scope" do
      env =
        Env.new()
        |> Env.push_scope()
        |> Env.define(var("name"), 3)
        |> elem(0)

      assert 3 = Env.get(env, var("name"))
    end

    test "variable defined in enclosed and global scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 5)
        |> elem(0)
        |> Env.push_scope()
        |> Env.define(var("name"), 3)
        |> elem(0)

      assert 3 = Env.get(env, var("name"))
    end
  end

  describe "assign/2" do
    test "undefined variable in a global scope" do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'name'/, fn ->
        Env.assign(Env.new(), var("name"), 5)
      end
    end

    test "undefined variable in any scope" do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'name'/, fn ->
        Env.assign(Env.push_scope(Env.new()), var("name"), 5)
      end
    end

    test "assignment in global scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)

      assert {env, 5} = Env.assign(env, var("name"), 5)
      assert 5 = Env.get(env, var("name"))
    end

    test "assignment of global variable in enclosed scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)
        |> Env.push_scope("test-scope")

      assert 3 == env.scopes[@globals].values["name"]
      refute Map.has_key?(env.scopes["test-scope"].values, "name")
      assert {env, 5} = Env.assign(env, var("name"), 5)
      assert 5 = Env.get(env, var("name"))
      assert 5 == env.scopes[@globals].values["name"]
    end

    test "assignment of shadowed variable in enclosed scope" do
      env =
        Env.new()
        |> Env.define(var("name"), 3)
        |> elem(0)
        |> Env.push_scope("test-scope")
        |> Env.define(var("name"), 7)
        |> elem(0)

      assert 3 == env.scopes[@globals].values["name"]
      assert 7 == env.scopes["test-scope"].values["name"]
      assert {env, 5} = Env.assign(env, var("name"), 5)
      assert 5 = Env.get(env, var("name"))
      assert 3 == env.scopes[@globals].values["name"]
      assert 5 == env.scopes["test-scope"].values["name"]
    end
  end

  describe "activate_scope/2, deactivate_scope/2, and call_with_scope/3" do
    setup do
      {:ok,
       env:
         Env.new()
         |> Env.define(var("global"), "global")
         |> elem(0)
         |> Env.push_scope("closure")
         |> Env.define(var("closure"), "closure")
         |> elem(0)
         |> Env.pop_scope()
         |> Env.push_scope("inner")
         |> Env.define(var("inner"), "inner")
         |> elem(0)}
    end

    test "activating a scope that does not exist raises an exception" do
      assert_raise RuntimeError, "Invalid scope 'test'.", fn ->
        Env.activate_scope(Env.new(), "test")
      end
    end

    test "calling with a scope that does not exist raises an exception" do
      assert_raise RuntimeError, "Invalid scope 'test'.", fn ->
        Env.call_with_scope(Env.new(), "test", fn _ -> nil end)
      end
    end

    test "activating the current scope is a no-op" do
      env = Env.push_scope(Env.new(), "test-scope")
      assert ^env = Env.activate_scope(env, "test-scope")
    end

    test "calling with the current scope is a scope no-op" do
      env = Env.push_scope(Env.new(), "test-scope")
      assert {^env, true} = Env.call_with_scope(env, "test-scope", fn e -> {e, true} end)
    end

    test "activating a scope puts the activated scope in the stack", %{env: env} do
      assert ["inner", @globals] == env.stack

      activated = Env.activate_scope(env, "closure")

      assert ["closure", "inner", @globals] == activated.stack
      refute Env.defined?(activated, var("inner"))
      assert "closure" == Env.get(activated, var("closure"))
      assert "global" == Env.get(activated, var("global"))
    end

    test "call_with_scope/3 deactivates the activated scope", %{env: env} do
      assert ["inner", @globals] == env.stack

      assert {^env, true} =
               Env.call_with_scope(env, "closure", fn activated ->
                 assert ["closure", "inner", @globals] == activated.stack
                 refute Env.defined?(activated, var("inner"))
                 assert "closure" == Env.get(activated, var("closure"))
                 assert "global" == Env.get(activated, var("global"))

                 {activated, true}
               end)
    end

    test "deactivating a scope restores it to the previous state", %{env: env} do
      assert ^env =
               env
               |> Env.activate_scope("closure")
               |> Env.deactivate_scope("closure")
    end

    test "deactivating the current scope acts the same as pop_scope/2", %{env: env} do
      assert %Env{stack: [@globals]} =
               env
               |> Env.activate_scope("closure")
               |> Env.deactivate_scope("closure")
               |> Env.deactivate_scope("inner")
    end

    test "only the top-level scope can be deactivated", %{
      env: env
    } do
      assert_raise RuntimeError, "Scope 'inner' is not the active scope.", fn ->
        env
        |> Env.activate_scope("closure")
        |> Env.deactivate_scope("inner")
      end
    end

    test "the root scope cannot be deactivated" do
      assert_raise RuntimeError, "Cannot deactivate root scope.", fn ->
        Env.deactivate_scope(Env.new(), @globals)
      end
    end
  end

  describe "internals" do
    setup do
      {:ok,
       env:
         Env.new()
         |> Env.define(var("global"), "global")
         |> elem(0)
         |> Env.push_scope("closure")
         |> Env.define(var("closure"), "closure")
         |> elem(0)
         |> Env.pop_scope()
         |> Env.push_scope("inner")
         |> Env.define(var("inner"), "inner")
         |> elem(0)}
    end

    test "find_defining_scope/2 only looks in the stack", %{env: env} do
      # "closure" scope exists but is not active
      assert is_nil(Env.find_defining_scope(env, "closure"))
      assert %Scope{id: "inner"} = Env.find_defining_scope(env, "inner")
      assert %Scope{id: @globals} = Env.find_defining_scope(env, "global")
    end

    test "__define/3 uses string names and returns only the env" do
      assert %Env{} =
               Env.new()
               |> Env.__define("global", "global")
    end

    test "__defined?/3 uses string names" do
      assert true ==
               Env.new()
               |> Env.__define("global", "global")
               |> Env.__defined?("global")
    end

    test "inspect", %{env: env} do
      assert ~s(#Env<["inner", "<globals>"] scopes: %{"<globals>" => #Scope<["clock", "env", "global"]>, "closure" => #Scope<["closure"] enclosing: "<globals>">, "inner" => #Scope<["inner"] enclosing: "<globals>">}>) ==
               inspect(env)
    end
  end

  defp var(name, line \\ 1), do: Token.new(:identifier, line, name)
  defp fake_print(v), do: v
end
