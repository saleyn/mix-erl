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
  mix ct --parallel
  mix ct rtb_gateway_utils_SUITE
  mix ct app[ln]*_SUITE
  mix ct app[ln]*_SUITE *match*_SUITE
  mix ct --cover-digest --cover-export
  ```

  # Command line options

    * `--parallel` - run tests in parallel
    * `--cover-export` - run coverage tool, export cover data
    * `--cover-digest` - run coverage tool, print total coverage
    * `--cover-report` - run coverage tool, save report in html format
  """

  @switches [
    parallel: :boolean,
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

    opts[:parallel] && cover_analys &&
      Mix.raise("\"mix ct\" can't use coverage tool in parallel mode!")

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

        # return suite name
        base_name
      end)

    # Ensure that common_test doesn't try to recompile our suites!
    Application.put_env(:common_test, :auto_compile, false)

    # Configure CT settings
    log_dir = project[:log_dir] || Path.join(Mix.Project.build_path(project), "logs")
    verbosity = project[:ct_verbosity] || 100

    File.mkdir_p!(log_dir)

    cover_analys && Mix.Tasks.Cover.compile(project)

    ret =
      if opts[:parallel] do
        Mix.shell().info("run parallel tests")
        run_parallel(suites, target, log_dir, verbosity)
      else
        Mix.shell().info("run sequential tests")
        run_sequence(suites, target, log_dir, verbosity)
      end
      |> check_result

    cover_export && Mix.Tasks.Cover.export(:ct)
    cover_inform && Mix.Tasks.Cover.report(cover_digest, cover_report)
    cover_analys && Mix.Tasks.Cover.reset()

    ret || System.stop(1)
  end

  defp mute_output do
    pid = self()
    stdout = Process.group_leader()
    {:ok, string_io} = StringIO.open("")
    Process.group_leader(pid, string_io)
    fn -> Process.group_leader(pid, stdout) end
  end

  defp run_sequence(suites, target, log_dir, verbosity) do
    suites_atoms = Enum.map(suites, &String.to_atom/1)

    # mute standard output
    mute_off = mute_output()

    try do
      :ct.run_testspec([
        {:ct_hooks, [:cth_readable_failonly, :cth_readable_compact_shell]},
        {:logdir, to_charlist(log_dir)},
        {:config, ~c"test/test.config"},
        {:verbosity, [default: verbosity]},
        {:suites, to_charlist(target), suites_atoms}
      ])
    after
      mute_off.()
    end
  end

  defp run_parallel(suites, target, log_dir, verbosity) do
    case :net_adm.names() do
      {:ok, _} ->
        :ok

      {:error, :address} ->
        epmd = System.find_executable("epmd")
        System.cmd(epmd, ~w(-daemon))
    end

    {:ok, _pid} = Node.start(:fgfghhdd, :shortnames)

    add_args =
      ~w(-kernel connect_all false -common_test auto_compile false)
      |> Enum.map(&String.to_charlist/1)

    suites
    |> Enum.map(
      &Task.async(fn ->
        suite_name = &1
        suite_atom = String.to_atom(suite_name)

        {:ok, _pid, node} =
          :peer.start_link(%{
            name: suite_atom,
            post_process_args: fn args -> add_args ++ args end
          })

        true = :rpc.call(node, :code, :set_path, [:code.get_path()])

        suite_logdir = Path.join(log_dir, suite_name)
        File.mkdir_p!(suite_logdir)

        # mute standard output
        mute_output()

        ret =
          :rpc.call(node, :ct, :run_testspec, [
            [
              {:ct_hooks, [:cth_readable_failonly, :cth_readable_compact_shell]},
              {:logdir, to_charlist(suite_logdir)},
              {:config, ~c"test/test.config"},
              {:verbosity, [default: verbosity]},
              {:suites, to_charlist(target), suite_atom}
            ]
          ])

        {suite_atom, ret}
      end)
    )
    |> Task.await_many(30 * 60 * 1000)
    |> Enum.reduce({0, 0, 0, 0, []}, fn
      {suite, {ok, failed, {uskip, askip}}}, {ok_, failed_, uskip_, askip_, errs} ->
        {ok_ + ok, failed_ + failed, uskip_ + uskip, askip_ + askip,
         if(failed > 0, do: [{suite, failed} | errs], else: errs)}

      {suite, error}, {a, b, c, d, errs} ->
        {a, b, c, d, [{suite, error} | errs]}
    end)
  end

  # sequential tests result
  defp check_result({ok, failed, {userSkipped, autoSkipped}}) do
    IO.puts(
      "#{ok} ok, #{failed} failed, #{userSkipped} user-skipped, #{autoSkipped} auto-skipped"
    )

    failed > 0
  end

  # parallel tests result
  defp check_result({ok, failed, userSkipped, autoSkipped, errs}) do
    IO.puts(
      "#{ok} ok, #{failed} failed, #{userSkipped} user-skipped, #{autoSkipped} auto-skipped"
    )

    errs
    |> Enum.each(fn
      {suite, num} when is_atom(suite) and is_integer(num) ->
        IO.puts("#{num} errors in #{suite}")

      {suite, error} when is_atom(suite) ->
        IO.puts("#{suite} failed: #{inspect(error)}")

      error ->
        IO.puts("error: #{inspect(error)}")
    end)

    failed > 0 or length(errs) > 0
  end

  defp check_result({:error, reason}) do
    IO.puts("error: #{inspect(reason)}")
    false
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
