defmodule Ilox.TokenTest do
  use ExUnit.Case, async: true
  use TypeCheck.ExUnit

  alias Ilox.Token

  spectest Token
end
