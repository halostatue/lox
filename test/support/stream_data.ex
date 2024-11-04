defmodule Ilox.StreamData do
  import StreamData

  @doc """
  Generates a stream of Lox literal values based on `kind`.
  """
  def literal(kind \\ :all)

  def literal(:all), do: one_of([integer(), float(), literal(:string), boolean(), nil])
  def literal(:number), do: number()
  def literal(:non_number), do: one_of([string(:printable), boolean(), nil])

  def literal(:string) do
    filter(
      string(:printable),
      &(!String.contains?(&1, ["\r", "\t", "\v", "\"", "\'", "\n", "\f"]))
    )
  end

  @doc """
  Generates a stream of positive numbers (integers or floats).
  """
  def positive_number, do: one_of([positive_integer(), float(min: 0.001)])

  @doc """
  Generates a stream of non-negative numbers (integers or floats).
  """
  def non_negative_number, do: one_of([non_negative_integer(), float(min: 0.0)])

  @doc """
  Generates a stream of numbers (integers or floats).
  """
  def number, do: one_of([integer(), float()])

  @doc """
  Generates a stream of negative numbers (integers or floats).
  """
  def negative_number, do: map(positive_number(), &(-&1))

  @doc """
  Generates a stream of negative integers.
  """
  def negative_integer, do: map(positive_integer(), &(-&1))

  @doc """
  Generates a stream of numbers excluding zero (integers or floats).
  """
  def nonzero_number, do: filter(number(), neq(0))

  @doc """
  A predicate for filter/3 that excludes `value` from the generated stream.
  """
  def neq(value), do: &(&1 != value)

  @doc """
  A predicate for filter/3 that the generated stream is less than `value`.
  """
  def le(value), do: &(&1 < value)

  @doc """
  A predicate for filter/3 that the generated stream is less than or equal to `value`.
  """
  def leq(value), do: &(&1 <= value)

  @doc """
  A predicate for filter/3 that the generated stream is greater than `value`.
  """
  def ge(value), do: &(&1 > value)

  @doc """
  A predicate for filter/3 that the generated stream is greater than or equal to `value`.
  """
  def geq(value), do: &(&1 >= value)
end
