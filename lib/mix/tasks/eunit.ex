defmodule Mix.Tasks.Eunit do
  use Mix.Task

  @compile {:no_warn_undefined, [:eunit, :cover]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Run a project's EUnit Tests"

  @moduledoc ~S"""
  Runs the EUnit Tests for a project.

  # Command line example

  ```
  mix eunit
  mix eunit --module adgear_testing
  mix eunit --cover-digest --cover-export
  ```

  # Command line options

    * `--module` - run single module unit test
    * `--cover-export` - run coverage tool, export cover data
    * `--cover-digest` - run coverage tool, print total coverage
    * `--cover-report` - run coverage tool, save report in html format
  """

  @switches [
    module: :string,
    cover_export: :boolean,
    cover_digest: :boolean,
    cover_report: :boolean
  ]

  def run(args) do
    # validate environment
    Mix.env() != :test &&
      Mix.raise(
        "\"mix eunit\" is running in the \"#{Mix.env()}\" environment. " <>
          "Please prefix mix command with MIX_ENV=test, or make sure MIX_ENV is not set."
      )

    {opts, _, _} = OptionParser.parse(args, strict: @switches)

    cover_export = opts[:cover_export]
    cover_digest = opts[:cover_digest]
    cover_report = opts[:cover_report]
    cover_inform = cover_digest || cover_report
    cover_analys = cover_inform || cover_export

    project = Mix.Project.config()

    # compile tests if not done yet
    Mix.Task.run("compile")

    # test selected suite(s) or the whole application
    eunit_options = project[:eunit_options] || []

    test_spec =
      case opts[:module] do
        nil ->
          {:application, project[:app]}

        module ->
          {:module, String.to_existing_atom(module)}
      end

    cover_analys && Mix.Tasks.Cover.compile(project)

    :eunit.test(test_spec, eunit_options)

    cover_export && Mix.Tasks.Cover.export(:eunit)
    cover_inform && Mix.Tasks.Cover.report(cover_digest, cover_report)
    cover_analys && Mix.Tasks.Cover.reset()
  end
end
