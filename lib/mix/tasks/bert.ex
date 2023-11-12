defmodule Mix.Tasks.Bert do
  use Mix.Task

  @compile {:no_warn_undefined, [:rtb_lib_bert_convert]}
  @dialyzer {:no_missing_calls, run: 1}

  @shortdoc "Convert *.term to *.bert2"

  @moduledoc ~S"""
  Converts Erlang terms to bert2 format

  # Command line example

  ```
  mix bert priv/bertconfs/development/*.term
  mix test/segments_matcher_SUITE_data/dmp/*.term
  ```
  """

  def run(wildcard) do
    wildcard
    |> Enum.each(fn x -> :rtb_lib_bert_convert.to_bert2(to_charlist(x)) end)
  end
end
