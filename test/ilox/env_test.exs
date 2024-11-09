defmodule Ilox.EnvTest do
  use ExUnit.Case, async: true

  alias Ilox.Callable
  alias Ilox.Env
  alias Ilox.Token

  setup do
    var = %{outer: var("outer"), inner: var("inner", 2)}
    {outer, _} = Env.define(Env.new(), var.outer, 3)
    env = %{outer: outer, inner: Env.new(outer)}
    {:ok, var: var, env: env}
  end

  describe "new/1" do
    test "new() returns a default module" do
      assert %Env{values: %{}, enclosing: nil, print: &IO.puts/1} == Env.new()
    end

    test "new(print: &fake_print/1) changes the print hook" do
      assert %Env{values: %{}, enclosing: nil, print: &fake_print/1} ==
               Env.new(print: &fake_print/1)
    end

    test "new(%Env{}) encloses and copies the print hook" do
      base = Env.new(print: &fake_print/1)

      assert %Env{values: %{}, enclosing: base, print: &fake_print/1} ==
               Env.new(base)
    end

    test "new(enclosing: %Env{}) still copies the print hook" do
      base = Env.new(print: &fake_print/1)

      assert %Env{values: %{}, enclosing: base, print: &fake_print/1} ==
               Env.new(enclosing: base)
    end

    test "new(enclosing: %Env{}, print: &fun/1) does not copy the print hook" do
      base = Env.new(print: &fake_print/1)

      assert %Env{values: %{}, enclosing: base, print: &IO.puts/1} ==
               Env.new(enclosing: base, print: &IO.puts/1)
    end
  end

  describe "define/3" do
    test "creates a variable that did not previously exist", ctx do
      testenv = ctx.env.outer
      varname = ctx.var.inner

      refute Env.defined?(testenv, varname, true)
      assert {testenv, 5} = Env.define(testenv, varname, 5)
      assert Env.defined?(testenv, varname, true)
      assert %{"inner" => 5, "outer" => 3} == testenv.values
    end

    test "updates an already existing variable in the same scope", ctx do
      testenv = ctx.env.outer
      varname = ctx.var.outer

      assert Env.defined?(testenv, varname, true)
      assert %{"outer" => 3} == testenv.values
      assert {testenv, 5} = Env.define(testenv, varname, 5)
      assert %{"outer" => 5} == testenv.values
    end

    test "creates a variable in an inner scope even if it exists in an outer scope", ctx do
      testenv = ctx.env.inner
      varname = ctx.var.outer

      assert Env.defined?(testenv, varname)
      refute Env.defined?(testenv, varname, true)
      assert %{} == testenv.values
      assert {testenv, 71} = Env.define(testenv, varname, 71)
      assert %{"outer" => 71} == testenv.values
    end
  end

  describe "get/2" do
    test "undefined variable", ctx do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'inner'/, fn ->
        Env.get(ctx.env.outer, ctx.var.inner)
      end
    end

    test "undefined variable inner scope", ctx do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'foo'/, fn ->
        Env.get(ctx.env.inner, var("foo"))
      end
    end

    test "scope defined variable", ctx do
      assert {env, 3} = Env.get(ctx.env.outer, ctx.var.outer)
      assert %{"outer" => 3} == env.values
    end

    test "scope variable matches even if redefined", ctx do
      assert {env, 3} = Env.get(ctx.env.outer, var("outer", 5))
      assert %{"outer" => 3} == env.values
    end

    test "outer scope defined variable", ctx do
      assert {env, 3} = Env.get(ctx.env.inner, ctx.var.outer)
      assert %{} == env.values
      assert %{"outer" => 3} == env.enclosing.values
    end

    test "inner scope overridden variable", ctx do
      {env, 5} = Env.define(ctx.env.inner, ctx.var.outer, 5)
      assert {env, 5} = Env.get(env, ctx.var.outer)
      assert %{"outer" => 5} == env.values
      assert %{"outer" => 3} == env.enclosing.values
    end
  end

  describe "assign/2" do
    test "undefined variable", ctx do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'inner'/, fn ->
        Env.assign(ctx.env.outer, ctx.var.inner, nil)
      end
    end

    test "undefined variable inner scope", ctx do
      assert_raise Ilox.RuntimeError, ~r/Undefined variable 'foo'/, fn ->
        Env.assign(ctx.env.inner, var("foo"), nil)
      end
    end

    test "scope defined variable", ctx do
      assert {env, 3} = Env.get(ctx.env.outer, ctx.var.outer)
      assert {env, 5} = Env.assign(env, ctx.var.outer, 5)
      assert %{"outer" => 5} == env.values
    end

    test "scope variable matches even if redefined", ctx do
      assert {env, 5} = Env.assign(ctx.env.outer, var("outer", 5), 5)
      assert %{"outer" => 5} == env.values
    end

    test "outer scope defined variable", ctx do
      assert {env, 5} = Env.assign(ctx.env.inner, ctx.var.outer, 5)
      assert %{} == env.values
      assert %{"outer" => 5} == env.enclosing.values
    end

    test "inner scope overridden variable", ctx do
      {env, 5} = Env.define(ctx.env.inner, ctx.var.outer, 5)
      assert {env, 71} = Env.assign(env, ctx.var.outer, 71)
      assert %{"outer" => 71} == env.values
      assert %{"outer" => 3} == env.enclosing.values
    end
  end

  describe "global_env/1 and __prepend_global_env/{0-2}" do
    test "__prepend_global_env/0 returns a global environment" do
      assert %Env{values: %{"clock" => %Callable{}}, enclosing: nil} = Env.__prepend_global_env()
    end

    test "__prepend_global_env/1 wraps a global environment around the provided env" do
      base = Env.new(print: &fake_print/1)
      env = Env.__prepend_global_env(base)

      assert %{} == env.values
      assert %{"clock" => %Callable{}} = env.enclosing.values
      assert env.print != env.enclosing.print
      assert env.print == (&fake_print/1)
      assert env.enclosing.print == (&IO.puts/1)
    end

    test "__prepend_global_env/2 with options makes a custom global environment" do
      base = Env.new()
      env = Env.__prepend_global_env(base, print: &fake_print/1)

      assert %{} == env.values
      assert %{"clock" => %Callable{}} = env.enclosing.values
      assert env.print == env.enclosing.print
      assert env.print == (&fake_print/1)
      assert env.enclosing.print == (&fake_print/1)
    end

    test "__prepend_global_env works regardless of nesting depth" do
      fp = &fake_print/1

      assert %Env{
               enclosing: %Env{
                 enclosing: %Env{
                   enclosing: %Env{
                     enclosing: nil,
                     print: ^fp,
                     values: %{"clock" => %Callable{}}
                   },
                   print: ^fp,
                   values: %{"inner" => 1}
                 },
                 print: ^fp,
                 values: %{"middle" => 2}
               },
               print: ^fp,
               values: %{"outer" => 3}
             } =
               Env.new()
               |> Env.__define("inner", 1)
               |> Env.new()
               |> Env.__define("middle", 2)
               |> Env.new()
               |> Env.__define("outer", 3)
               |> Env.__prepend_global_env(print: fp)
    end

    test "global_env/1 returns the root environment" do
      env =
        Env.new()
        |> Env.__define("inner", 1)
        |> Env.new()
        |> Env.__define("middle", 2)
        |> Env.new()
        |> Env.__define("outer", 3)
        |> Env.__prepend_global_env()

      assert env.enclosing.enclosing.enclosing == Env.global_env(env)
    end
  end

  defp var(name, line \\ 1), do: Token.new(:identifier, line, name)
  defp fake_print(v), do: v
end
