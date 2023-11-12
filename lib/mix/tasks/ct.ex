defmodule Mix.Tasks.Ct do
  use Mix.Task

  @compile {:no_warn_undefined, [:ct]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Run a project's Common Test suites"

  @test_dirs ["test"]
  @test_glob "*_SUITE"

  @moduledoc ~S"""
  Runs the Common Tests for a project.

  # Command line example

  ```
  mix ct
  mix ct rtb_gateway_utils_SUITE
  mix ct app[ln]*_SUITE
  mix ct app[ln]*_SUITE *match*_SUITE
  mix ct --cover-digest --cover-export
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
        "\"mix ct\" is running in the \"#{Mix.env()}\" environment. " <>
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

    target = Mix.Project.compile_path(project)
    File.mkdir_p!(target)

    {:ok, _} = Application.ensure_all_started(:common_test)
    {:ok, _} = Application.ensure_all_started(:cth_readable)

    # Find test suites
    tests =
      case pos_args do
        [] ->
          @test_glob <> ".erl"

        _ ->
          "{" <> Enum.join(for(x <- pos_args, do: x <> ".erl"), ",") <> "}"
      end

    Mix.shell().info("Looking for #{inspect(tests)} in #{inspect(Enum.join(@test_dirs, ", "))}")

    suites =
      Mix.Utils.extract_files(@test_dirs, tests)
      |> Enum.map(fn path ->
        # test suite name to consider
        base_name = Path.basename(path, ".erl")

        # copy data dir to target build dir
        data_name = base_name <> "_data"
        data_dest = Path.join(target, data_name)
        sync_data(path, data_name, data_dest)

        # return suite as atom
        String.to_atom(base_name)
      end)

    # Ensure that common_test doesn't try to recompile our suites!
    Application.put_env(:common_test, :auto_compile, false)

    # Configure CT settings
    log_dir = project[:log_dir] || Path.join(Mix.Project.build_path(project), "logs")
    verbosity = project[:ct_verbosity] || 100

    File.mkdir_p!(log_dir)

    cover_analys && Mix.Tasks.Cover.compile(project)

    ret =
      :ct.run_testspec([
        {:ct_hooks, [:cth_readable_failonly, :cth_readable_compact_shell]},
        {:logdir, to_charlist(log_dir)},
        {:config, ~c"test/test.config"},
        {:verbosity, [default: verbosity]},
        {:suites, to_charlist(target), suites}
      ])

    case ret do
      {ok, failed, {userSkipped, autoSkipped}} ->
        IO.puts(
          "#{ok} ok, #{failed} failed, #{userSkipped} user-skipped, #{autoSkipped} auto-skipped"
        )

        cover_export && Mix.Tasks.Cover.export(:ct)
        cover_inform && Mix.Tasks.Cover.report(cover_digest, cover_report)
        cover_analys && Mix.Tasks.Cover.reset()

      {:error, reason} ->
        IO.puts("error: #{inspect(reason)}")
        System.stop(1)
    end
  end

  defp sync_data(path, name, dest) do
    case Path.dirname(path) do
      ^path ->
        :ok

      dir ->
        data = Path.join(dir, name)
        File.exists?(data) && sync_files(data, dest)
        sync_files(data, dest)
        sync_data(dir, name, dest)
    end
  end

  defp sync_files(source, target) do
    File.exists?(source) &&
      File.cp_r!(source, target,
        on_conflict: fn orig, dest ->
          {orig_mtime, orig_size} = Mix.Utils.last_modified_and_size(orig)
          {dest_mtime, dest_size} = Mix.Utils.last_modified_and_size(dest)
          orig_mtime > dest_mtime or orig_size != dest_size
        end
      )
  end
end
