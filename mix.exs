defmodule MixErl.MixProject do
  use Mix.Project

  def project do
    [
      app: :mix_erl,
      version: "0.1.0",
      package: package(),
      deps: deps()
    ]
  end

  defp package do
    [
      name: "mix_erl",
      description: "Erlang-specific Mix Tasks"
    ]
  end

  defp deps do
    [
      {:cth_readable, "~> 1.6.0", only: [:test], runtime: false}
    ]
  end
end
