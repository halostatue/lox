defmodule LoxTest do
  use ExUnit.Case
  doctest Lox

  test "greets the world" do
    assert Lox.hello() == :world
  end
end
