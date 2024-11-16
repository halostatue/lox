defmodule Lox.MixProject do
  use Mix.Project

  def project do
    [
      app: :lox,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
        main: "Lox",
        extras: ["README.md"],
        before_closing_body_tag: %{
          html: """
          <script src="https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"></script>
          <script>mermaid.initialize({startOnLoad: true})</script>
          """,
          epub: ""
        }
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:type_check, "~> 0.13.3"},
      {:dialyxir, "~> 1.4", only: :dev, runtime: false},
      {:credo, "~> 1.5", only: :dev},
      {:ex_doc, "~> 0.34.0", only: :dev},
      {:stream_data, "~> 1.0", only: [:dev, :test]}
    ]
  end

  defp elixirc_paths(:test) do
    ~w(lib support test/support)
  end

  defp elixirc_paths(:dev) do
    ~w(lib support)
  end

  defp elixirc_paths(_) do
    ~w(lib)
  end
end
