defmodule Ilox.Guards do
  @moduledoc false

  defguard is_digit(c) when c in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

  @alpha String.split("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", "", trim: true)

  defguard is_alpha(c) when c in @alpha

  defguard is_alphanumeric(c) when is_digit(c) or is_alpha(c)

  defguard is_whitespace(c) when c in ["\r", "\s", "\t"]

  defguard is_callable(v) when is_struct(v, Ilox.Callable)
end
