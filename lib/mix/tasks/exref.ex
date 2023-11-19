defmodule Mix.Tasks.Exref do
  use Mix.Task

  @compile {:no_warn_undefined, [:xref]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Run cross reference analysis"

  @moduledoc ~S"""
  Runs cross reference analysis for a project.

  # Command line example

  ```
  mix exref
  mix exref --warnings
  mix exref --verbose
  ```
  """

  def run(args) do
    {opts, _, _} = OptionParser.parse(args, strict: [verbose: :boolean, warnings: :boolean])
    warnings = opts[:warnings] || false
    verbose = opts[:verbose] || false

    project = Mix.Project.config()
    xref_checks = project[:xref_checks] || [:exports_not_used, :undefined_function_calls]
    xref_ignores = project[:xref_ignores] || []

    for x <- Mix.Utils.extract_files([to_string(:code.lib_dir())], "*/ebin") do
      Code.append_path(x)
    end

    {:ok, _pid} = :xref.start(:exref)

    dirs = for x <- :code.get_path(), :filelib.is_dir(x), do: x

    :ok = :xref.set_library_path(:exref, dirs)
    app_dir = Mix.Project.app_path() |> Path.join("ebin") |> to_charlist
    {:ok, _} = :xref.add_directory(:exref, app_dir, [{:warnings, warnings}])

    rets =
      for check <- xref_checks do
        verbose && IO.puts("Checking for #{inspect(check)}...")
        {:ok, results} = :xref.analyze(:exref, check)
        filtered_results = results |> filter_result(check, xref_ignores, verbose)

        filtered_results
        |> Enum.each(report_fun(check))

        filtered_results == []
      end

    case Enum.all?(rets) do
      true ->
        verbose && IO.puts("All checks passed!")

      false ->
        verbose && IO.puts("Some check(s) didn't pass")
        System.stop(1)
    end
  end

  defp mfa({m, f, a}, color), do: IO.ANSI.format([color, "#{m}:#{f}/#{a}"])

  defp report_fun(:undefined_function_calls) do
    fn {src, dst} ->
      IO.puts("#{mfa(src, :cyan)} calls undefined function #{mfa(dst, :red)}")
    end
  end

  defp report_fun(:deprecated_function_calls) do
    fn {src, dst} ->
      IO.puts("#{mfa(src, :cyan)} calls deprecated function #{mfa(dst, :yellow)}")
    end
  end

  defp report_fun(:undefined_functions) do
    fn x ->
      IO.puts("function is not defined: #{mfa(x, :red)}")
    end
  end

  defp report_fun(:deprecated_functions) do
    fn x ->
      IO.puts("function is deprecated: #{mfa(x, :yellow)}")
    end
  end

  defp report_fun(:locals_not_used) do
    fn x ->
      IO.puts("local function is not used: #{mfa(x, :yellow)}")
    end
  end

  defp report_fun(:exports_not_used) do
    fn x ->
      IO.puts("exported function is not used: #{mfa(x, :yellow)}")
    end
  end

  defp report_fun(type) do
    fn x -> IO.puts("#{type}: #{inspect(x)}") end
  end

  defp augment_ignores(ignores, type, result, verbose) do
    result
    |> Enum.reduce(
      MapSet.new(),
      fn
        {m, _, _}, acc -> MapSet.put(acc, m)
        {{m, _, _}, {_, _, _}}, acc -> MapSet.put(acc, m)
        _, acc -> acc
      end
    )
    |> Enum.flat_map(fn mod -> get_ignores(mod, type, verbose) end)
    |> Enum.concat(ignores)
  end

  defp filter_result(result, type, ignores, verbose) do
    ignores = augment_ignores(ignores, type, result, verbose)

    ff = fn
      {{m, _, _} = v1, {m, _, _} = v2} -> fn x -> x in [m, v1, v2] end
      {{m1, _, _} = v1, {m2, _, _} = v2} -> fn x -> x in [m1, m2, v1, v2] end
      {m, _, _} = v -> fn x -> x in [m, v] end
      v -> fn x -> x == v end
    end

    result
    |> Enum.filter(fn x -> not Enum.any?(ignores, ff.(x)) end)
  end

  # ignore behaviour functions, and explicitly marked functions
  #
  # functions can be ignored by using
  # -ignore_xref([{F, A}, {M, F, A}...]).
  defp get_ignores(mod, type, verbose) do
    # get ignore_xref attribute and combine them in one list
    attrs =
      try do
        mod.module_info(:attributes)
      rescue
        _ in UndefinedFunctionError ->
          []
      end

    behaviourCallbacks = get_behaviour_callbacks(type, attrs, verbose)

    # and create a flat {M,F,A} list
    (keyall(:ignore_xref, attrs) ++ behaviourCallbacks)
    |> Enum.map(fn
      {f, a} -> {mod, f, a}
      x -> x
    end)
  end

  defp keyall(key, list) do
    Enum.concat(for {k, v} <- list, k == key, do: v)
  end

  defp get_behaviour_callbacks(:exports_not_used, attrs, verbose) do
    (keyall(:behaviour, attrs) ++ keyall(:behavior, attrs))
    |> Enum.map(fn mod ->
      try do
        mod.behaviour_info(:callbacks)
      rescue
        _ in UndefinedFunctionError ->
          verbose && IO.puts("Behaviour #{mod} is used but cannot be found.")
          []
      end
    end)
  end

  defp get_behaviour_callbacks(_, _, _) do
    []
  end
end
