defprotocol Ilox.Callable do
  @moduledoc """
  A callable (a class or function).
  """

  alias Ilox.Env

  @doc "Returns the number of arguments expected for the callable."
  @spec arity(t) :: non_neg_integer()
  def arity(callable)

  @doc "Executes the callable given the provided env and args."
  @spec call(t, Env.t(), list(term())) :: {Env.t(), term()}
  def call(callable, env, args)
end
