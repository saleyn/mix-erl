defmodule MixErl.MixProject do
  use Mix.Project

  def project do
    [
      app:     :mix_erl,
      version: "0.1.0",
      package: package(),
      deps:    deps(),
      docs:    [
        # The main page in the docs
        main:   "readme",
        extras: ["README.md", "LICENSE"]
      ]
    ]
  end

  defp package do
    [
      name:        "mix_erl",
      description: "Erlang-specific Mix Tasks",
      licenses:    ["MIT"],
      links:       %{"GitHub" => "https://github.com/saleyn/mix-erl"},
      files:       ~w(lib mix.* example Makefile README.md LICENSE .formatter.exs)
    ]
  end

  defp deps do
    [
      {:cth_readable, "~> 1.6.0", runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
