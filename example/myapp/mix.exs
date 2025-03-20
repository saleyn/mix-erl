defmodule MyApp.MixProject do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :myapp,
      version: "0.1.0",
      language: :erlang,
      dialyzer: [plt_add_apps: [:mix]],
      xref_ignores: [:ct, :eunit],
      deps: deps(),
      erlc_options: erlc_options(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      compilers: [:erlydtl, :gpb | Mix.compilers()],
      erlydtl_options: [compiler_options: [:debug_info]],
      gpb_config: [{"proto/x.proto", [:include_as_lib, i: ~c"proto", o: ~c"src"]}]
    ]
  end

  def application do
    [
      #mod: {:myapp_app, []},
      extra_applications: extra_applications(Mix.env())
    ]
  end

  def deps do
    [
      #{:mix_erl, git: "https://github.com/saleyn/mix-erl", branch: "main", runtime: false},
      {:mix_erl, path: "../../", runtime: false},
      {:erlydtl, github: "erlydtl/erlydtl"},
      {:gpb, "~> 4.21"}
    ]
  end

  def cli, do: [preferred_envs: [eunit: :test, eproper: :test, ct: :test, cover: :test, release: :prod]]

  defp extra_applications(:test), do: [:eunit, :common_test]
  defp extra_applications(_),     do: []

  defp erlc_options(:test) do
    [
      :debug_info,
      :warnings_as_errors,
      d: :TEST,
      d: :EUNIT
    ]
  end
  defp erlc_options(_),   do: [:debug_info]

  defp erlc_paths(:test), do: ~w(src test)
  defp erlc_paths(_),     do: ~w(src)
end
