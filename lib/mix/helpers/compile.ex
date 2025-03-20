defmodule Mix.Helpers.Compile do
  # use Mix.Task.Compiler
  import Mix.Compilers.Erlang

  defmacro compile_(manifest, mappings, src_ext, dest_ext, opts, callback) do
    case function_exported?(Mix.Compilers.Erlang, :compile_entries, 6) do
      true ->
        quote do
          compile_entries(
            unquote(manifest),
            unquote(mappings),
            unquote(src_ext),
            unquote(dest_ext),
            unquote(opts),
            unquote(callback)
          )
        end

      false ->
        quote do
          compile(unquote(manifest), unquote(mappings), unquote(opts), unquote(callback))
        end
    end
  end
end
