defmodule JSONFilter.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsonfilter,
      version: "0.1.0",
      description: "an otp approach to working with streaming json",
      package: package,
      deps: deps,
      language: :erlang
    ] ++ env_keys(Mix.env)
  end

  # env keys specific to dev, prod, etc
  defp env_keys(:dev) do
    [
      elixirc_paths: ["extras"],
      erlc_paths: ["src", "test"],
      erlc_options: [d: :TEST]
    ]
  end
  defp env_keys(_), do: []

  def application do
    [applications: [:jsx]]
  end

  defp deps do
    [{:jsx, "~> 2.2.0"}]
  end

  defp package do
    [
      files: [
        "LICENSE",
        "mix.exs",
        "README.md",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: %{"github" => "https://github.com/talentdeficit/jsonfilter"},
      licenses: ["MIT"]
    ]
  end
end

