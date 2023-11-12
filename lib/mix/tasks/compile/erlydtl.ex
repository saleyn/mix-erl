# https://www.reddit.com/r/elixir/comments/4491ty/getting_erlydtl_working_in_elixir/
#
defmodule Mix.Tasks.Compile.Erlydtl do
  @compile {:no_warn_undefined, [:erlydtl]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Compile erlydtl templates"

  @moduledoc """
  Complile erlydtl .dtl files to .beam files.

  This is a "compiler" that converts an input to an output from "mix compile".

  def project do
    [app: :my_app,
     version: "0.1.0",
     compilers: [:erlydtl] ++ Mix.compilers,
     erlydtl_options: [source: "templates", compiler_options: [:debug_info]]
     deps: deps]
  end

  ## Configuration

  The `erlydtl_options` key under project specifies options.

    * `:source` - directory to find source files.
      Defaults to `"templates"`.

    * `:compiler_options` - options for erlydtl:compile.
      Defaults to `[]`.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  """
  use Mix.Task.Compiler
  import Mix.Compilers.Erlang

  @spec run(OptionParser.argv()) ::
          :ok | :noop | {:ok | :noop | :error, [Mix.Task.Compiler.Diagnostic.t()]}
  def run(args) do
    {opts, _argv, _errors} = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.config()
    erlydtl_options = project[:erlydtl_options] || []
    source_path = erlydtl_options[:source] || "templates"
    dest_path = to_erl_file(Mix.Project.compile_path(project))
    force = opts[:force] || false

    erlydtl_options =
      erlydtl_options
      |> Keyword.drop([:source])
      |> Keyword.merge(out_dir: to_erl_file(dest_path), return: true)

    # Mix.shell().info("Compiling erlydtl files from #{source_path} to #{dest_path}")

    manifest = Path.join(Mix.Project.manifest_path(), "compile.dtl")

    ensure_erlydtl()

    callback = fn input, _output ->
      module = String.to_atom(module_from_artifact(input))
      # Mix.shell().info("Compiling #{input}, #{module}, #{output}")
      :erlydtl.compile_file(to_erl_file(input), module, erlydtl_options)
    end

    entries =
      for {src, dest} <- [{source_path, dest_path}],
          target <- extract_entries(src, :dtl, dest, :beam, force),
          do: target

    if preload = entries != [] && opts[:preload] do
      preload.()
    end

    compile(manifest, entries, opts, callback)
  end

  defp extract_entries(src_dir, src_ext, dest_dir, dest_ext, force) do
    files = Mix.Utils.extract_files(List.wrap(src_dir), List.wrap(src_ext))

    for file <- files do
      module = module_from_artifact(file)
      target = Path.join(dest_dir, module <> "." <> to_string(dest_ext))

      if force || Mix.Utils.stale?([file], [target]) do
        {:stale, file, target}
      else
        {:ok, file, target}
      end
    end
  end

  def module_from_artifact(artifact) do
    name = artifact |> Path.basename() |> Path.rootname()
    name <> "_dtl"
  end

  defp ensure_erlydtl do
    with nil <- Application.get_application(:erlydtl),
         {:error, _} <- Application.ensure_all_started(:erlydtl) do
      case Map.get(Mix.Project.deps_paths(), :erlydtl) do
        nil ->
          Mix.shell().error("Can't find erlydtl project in deps")

        _ ->
          {:ok, _} = Application.ensure_all_started(:erlydtl)

          true =
            Mix.Project.build_path()
            |> Path.join(["lib", "erlydtl", "ebin"])
            |> Code.append_path()
      end
    end
  end
end
