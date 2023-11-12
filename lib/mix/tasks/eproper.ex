defmodule Mix.Tasks.Eproper do
  use Mix.Task

  @compile {:no_warn_undefined, [:proper]}
  @dialyzer {:no_missing_calls, run_proper: 2}

  @shortdoc "Run a project's PropEr Tests"

  @test_dirs ["test"]
  @test_glob "prop_*"

  @moduledoc ~S"""
  Runs the PropEr Tests for a project.

  # Command line example

  ```
  mix eproper
  mix eproper prop_utils
  mix eproper prop_rtb_gateway_*
  mix eproper *_index *_utils
  mix eproper --cover-digest --cover-export
  ```

  # Command line options

    * `--cover-export` - run coverage tool, export cover data
    * `--cover-digest` - run coverage tool, print total coverage
    * `--cover-report` - run coverage tool, save report in html format
  """

  @switches [
    cover_export: :boolean,
    cover_digest: :boolean,
    cover_report: :boolean
  ]

  def run(args) do
    # validate environment
    Mix.env() != :test &&
      Mix.raise(
        "\"mix eproper\" is running in the \"#{Mix.env()}\" environment. " <>
          "Please prefix mix command with MIX_ENV=test, or make sure MIX_ENV is not set."
      )

    {opts, pos_args, _} = OptionParser.parse(args, strict: @switches)

    cover_export = opts[:cover_export]
    cover_digest = opts[:cover_digest]
    cover_report = opts[:cover_report]
    cover_inform = cover_digest || cover_report
    cover_analys = cover_inform || cover_export

    project = Mix.Project.config()

    # compile tests if not done yet
    Mix.Task.run("compile")

    # Find test suites
    tests =
      case pos_args do
        [] ->
          @test_glob <> ".erl"

        _ ->
          "{" <> Enum.join(for(x <- pos_args, do: x <> ".erl"), ",") <> "}"
      end

    Mix.shell().info("Looking for #{inspect(tests)} in #{inspect(Enum.join(@test_dirs, ", "))}")

    cover_analys && Mix.Tasks.Cover.compile(project)

    Mix.Utils.extract_files(@test_dirs, tests)
    |> Enum.reduce(true, fn path, status ->
      path
      |> Path.basename()
      |> Path.rootname()
      |> String.to_atom()
      |> run_proper(status)
    end)
    |> finish

    cover_export && Mix.Tasks.Cover.export(:eproper)
    cover_inform && Mix.Tasks.Cover.report(cover_digest, cover_report)
    cover_analys && Mix.Tasks.Cover.reset()
  end

  defp run_proper(module, status) do
    Mix.shell().info("checking #{module}")
    Code.ensure_loaded!(module)

    module.module_info()
    |> Keyword.get(:exports)
    |> Enum.filter(&property?/1)
    |> Enum.map(fn {name, 0} ->
      :proper.quickcheck(apply(module, name, []))
    end)
    |> add_results(status)
  end

  defp property?({name, 0}) do
    name
    |> Atom.to_string()
    |> String.starts_with?("prop_")
  end

  defp property?({_name, _arity}), do: false

  defp add_results(results, true), do: Enum.all?(results)
  defp add_results(_, _), do: false

  defp finish(false), do: System.at_exit(fn _ -> exit({:shutdown, 1}) end)
  defp finish(_), do: :ok
end
