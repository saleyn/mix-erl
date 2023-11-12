defmodule Mix.Tasks.Cover do
  use Mix.Task

  @compile {:no_warn_undefined, [:cover]}
  @dialyzer {:no_missing_calls,
             analyze: 2,
             compile: 1,
             create_mod_html: 2,
             ensure_started: 0,
             export: 1,
             report: 2,
             reset: 0,
             run: 1}

  @shortdoc "Perform coverage analysis"

  @moduledoc ~S"""
  Generate test coverage report from coverdata files, collected
  by Erlang EUnit, Common Test and PropEr tests run with --cover option.

  # Command line example

  ```
  mix cover
  mix cover --html
  ```

  # Command line options

    * `--html` - save report in html format

  # Configuration

  These configurations can be set in the `def project` section of your `mix.exs`:

    * `:cover_src_paths` - list of directories to look for Erlang source files for
      for covered modules, default: ["src"]

    * `:cover_extra_mods` - extra modules to cover (for example, generated modules,
      or modules compiled from different source directories)

    * `:cover_excl_mods` - what modules we'd like to exclude from the coverage
  """

  def run(args) do
    # validate environment
    Mix.env() != :test &&
      Mix.raise(
        "\"mix cover\" is running in the \"#{Mix.env()}\" environment. " <>
          "Please prefix mix command with MIX_ENV=test, or make sure MIX_ENV is not set."
      )

    {opts, _, _} = OptionParser.parse(args, strict: [html: :boolean])

    # ensure cover is started, mute cover messages
    ensure_started() |> mute_process()

    # reset any existing cover data
    :cover.reset()

    # compile tests if not done yet
    Mix.Task.run("compile")

    # cover-compile eligible modules
    project = Mix.Project.config()
    compile(project)

    # import all coverdata files
    Mix.Utils.extract_files([data_path()], ["coverdata"])
    |> Enum.map(&:cover.import/1)

    # generate report
    report(true, opts[:html])
  end

  def compile(project) do
    ensure_started() |> mute_process()

    cover_src_paths = project[:cover_src_paths] || ["src"]
    compile_path = Mix.Project.compile_path()

    # cover Erlang modules
    erlang_mods =
      Mix.Utils.extract_files(cover_src_paths, ["erl"])
      |> Enum.map(&(Path.basename(&1, ".erl") |> String.to_atom()))
      |> MapSet.new()

    # beam module names as list of string
    beams =
      Mix.Utils.extract_files([compile_path], ["beam"])
      |> Enum.map(&Path.basename(&1, ".beam"))

    # cover Elixir modules
    elixir_mods =
      beams
      |> Enum.reduce(MapSet.new(), fn
        # "Elixir.Mix" <> _, acc -> acc
        "Elixir." <> _ = x, acc -> acc |> MapSet.put(String.to_atom(x))
        _, acc -> acc
      end)

    # cover extra modules
    extra_mods =
      project[:cover_extra_mods] ||
        []
        |> MapSet.new()

    # cover denylist modules
    excl_mods =
      project[:cover_excl_mods] ||
        []
        |> MapSet.new()

    # all compiled modules
    all_mods =
      beams
      |> Enum.map(&String.to_atom/1)
      |> MapSet.new()

    # combined list of modules
    union_mods =
      erlang_mods
      |> MapSet.union(elixir_mods)
      |> MapSet.union(extra_mods)

    # filtered modules
    mods =
      all_mods
      |> MapSet.intersection(union_mods)
      |> MapSet.difference(excl_mods)
      |> MapSet.to_list()

    # cover-compile filtered modules
    :cover.compile_beam(mods)
    |> Enum.all?(&(is_tuple(&1) && elem(&1, 0) === :ok)) ||
      Mix.raise("failed to cover-compile modules")
  end

  def export(name) do
    path = data_path()
    File.mkdir_p!(path)
    file = path |> Path.join("#{name}.coverdata") |> to_charlist

    case :cover.export(file) do
      :ok ->
        :ok

      {:error, reason} ->
        Mix.shell().error("Cover data export failed: #{inspect(reason)}")
    end
  end

  def reset() do
    # dump accumulated coverdata
    :ok = :cover.reset()
  end

  def report(show, save) do
    report(show, save, :cover.modules())
  end

  defp report(show, save, modules) do
    save && File.mkdir_p!(report_path())
    report_data = analyze(modules, save && report_path())
    show && print_digest(report_data)
    save && write_index(report_data)
  end

  defp ensure_started do
    case :cover.start() do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  defp mute_process(pid) do
    {:ok, string_io} = StringIO.open("")
    Process.group_leader(pid, string_io)
  end

  defp data_path do
    Mix.Project.build_path() |> Path.join("cover")
  end

  defp report_path do
    Path.join(data_path(), "report")
  end

  defp analyze(mods, report_path) do
    Enum.map(mods, fn mod ->
      {:ok, coverage} = :cover.analyze(mod, :coverage, :line)
      report_path && create_mod_html(mod, report_path)

      stat =
        Enum.reduce(coverage, {0, 0}, fn
          {{_, 0}, _}, acc -> acc
          {_, {c, n}}, {ac, an} -> {c + ac, n + an}
        end)

      {mod, stat}
    end)
    |> Enum.sort()
    |> aggregate()
  end

  defp create_mod_html(mod, path) do
    fname =
      Path.join(path, to_string(mod) <> ".html")
      |> to_charlist

    :cover.analyze_to_file(mod, fname, [:html])
  end

  defp aggregate(data) do
    {list, {tc, tn, max_len}} =
      data
      |> Enum.map_reduce({0, 0, 0}, fn {mod, {c, n}}, {ac, an, al} ->
        mod_str = to_string(mod)
        {{mod_str, pcnt_str(c, n)}, {ac + c, an + n, max(al, String.length(mod_str))}}
      end)

    {list, pcnt_str(tc, tn), max_len}
  end

  defp pcnt_str(_, 0), do: "100 %"
  defp pcnt_str(c, n), do: "#{trunc(100 * c / (c + n))} %"

  defp print_digest({list, total_str, max_len}) do
    pad = &String.pad_leading(&1, 8)

    IO.puts("coverage : module")
    IO.puts("#{String.pad_leading("", max_len + 13, ".")}")

    list
    |> Enum.each(fn {mod, pcnt} ->
      IO.puts("#{pad.(pcnt)} : #{mod}")
    end)

    IO.puts("#{String.pad_leading("", max_len + 13, ".")}")
    IO.puts("#{pad.(total_str)} : total")
  end

  defp write_index({list, total, _max_len}) do
    {:ok, file} = File.open(Path.join(data_path(), "index.html"), [:write])

    IO.binwrite(
      file,
      """
      <!DOCTYPE HTML><html>
      <head><meta charset="utf-8">
      <style>
      table, th, td {
        border: 1px solid black;
        border-collapse: collapse;
      }
      th, td {
        padding-left: 20px;
        padding-right: 20px;
      }
      </style>
      <title>Coverage Summary</title></head>
      <body>
      """
    )

    IO.binwrite(
      file,
      """
      <table>
      <tr>
        <th><code>coverage %</code></th>
        <th><code>module</code></th>
      </tr>
      """
    )

    list
    |> Enum.each(fn {mod, pcnt} ->
      fname = Path.join("report", mod <> ".html")

      IO.binwrite(
        file,
        """
        <tr>
          <td align='right'><code>#{pcnt}</code></td>
          <td><code><a href='#{fname}'>#{mod}</a></code></td>
        </tr>
        """
      )
    end)

    IO.binwrite(
      file,
      """
      <tr>
        <th align='right'><code>#{total}</code></th>
        <th><code>total</code></th>
      </tr>
      """
    )

    IO.binwrite(file, "</table>\n</body>\n</html>\n")
  end
end
