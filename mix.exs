defmodule Lox.MixProject do
  use Mix.Project

  def project do
    [
      app: :lox,
      version: "0.1.0",
      elixir: "~> 1.17",
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
        },
        groups_for_docs: [
          "Context-Free Grammar": &(&1[:section] == :cfgrammar),
          "Parsing Grammar": &(&1[:section] == :pgrammar)
        ]
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
      {:ex_doc, "~> 0.34.0", only: :dev}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
