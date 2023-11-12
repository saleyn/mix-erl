defmodule MixErl.MixProject do
  use Mix.Project

  def project do
    [
      app: :mix_erl,
      version: "0.1.0",
      package: package()
    ]
  end

  defp package do
    [
      name: "mix_erl",
      description: "Erlang-specific Mix Tasks"
    ]
  end
end
