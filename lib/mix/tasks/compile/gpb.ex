defmodule Mix.Tasks.Compile.Gpb do
  @compile {:no_warn_undefined, [:gpb_compile]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Compile gpb .proto files"

  @moduledoc """
  Complile gpb .proto files to .erl (and .hrl) files.

  This is a "compiler" that converts an input to an output from "mix compile".

  def project do
    [app: :my_app,
     version: "0.1.0",
     compilers: [:gpb | Mix.compilers()],
     gpb_config: [{["/some/path/file.proto"], [:use_packages, {:o, ~c"/out/dir"}]}]
     deps: deps]
  end

  ## Configuration

  The `gpb_config` key under project specifies list of `{files, options}` pairs.

    * `files` - source file or list of files.

    * `options` - options for gpb_compile:file/2 to compile `files`
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
    force = opts[:force] || false

    project = Mix.Project.config()
    config = project[:gpb_config] || []

    entries =
      Enum.flat_map(config, fn
        {files, opts_} ->
          files = List.wrap(files)
          dest_dir = (opts_[:o] || ".") |> to_string()
          File.exists?(dest_dir) || File.mkdir_p(dest_dir)

          for file <- files do
            base =
              file
              |> Path.basename()
              |> Path.rootname(".proto")

            target = Path.join(dest_dir, base) <> ".erl"

            if force || Mix.Utils.stale?([file], [target]) do
              {:stale, file, target}
            else
              {:ok, file, target}
            end
          end
      end)

    callback = fn input, output ->
      Mix.shell().info("Compiling #{input} to #{output}")

      input
      |> Path.basename()
      |> to_charlist
      |> :gpb_compile.file(options(config, input))
      |> result()
    end

    Path.join(Mix.Project.manifest_path(), "compile.gpb")
    |> compile_entries(entries, :proto, :erl, opts, callback)
  end

  defp result(:ok), do: {:ok, [], []}
  defp result({:ok, warnings}), do: {:ok, [], warnings}
  defp result(:error), do: {:error, [], []}
  defp result({:error, reason}), do: {:error, [reason], []}
  defp result({:error, reason, warnings}), do: {:error, [reason], warnings}

  defp options(config, input) do
    Enum.find_value(config, [], fn {files, opts} ->
      List.wrap(files) |> Enum.member?(input) && opts
    end)
  end
end
