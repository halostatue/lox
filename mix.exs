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
      {:type_check, "~> 0.13.3"},
      {:credo, ">= 0.0.0", only: :dev},
      {:ex_doc, "~> 0.34.0", only: :dev},
      {:stream_data, "~> 0.5.0", only: :test}
    ]
  end
end
