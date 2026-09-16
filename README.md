# Mix Erl

Mix tasks for compiling Erlang projects.

This project implements tasks for running the following Erlang tools:

* cover - code coverage analysis
* ct - run common tests
* eproper - run PropEr tests
* erlydtl - compile ErlyDTL templates
* eunit - run eunit tests
* exref - perform cross-reference checks

## Installation

The package can be installed by adding `mix_erl` to the list of dependencies
in `mix.exs`:

```elixir
def deps do
  [
    {:mix_erl, "~> 0.2.1"}
  ]
end
```

This will enable the tasks mentioned above to be given to `mix` in order
to compile, lint, and test `src/*.erl` files stored inside the Elixir project
tree.

## License

See the [LICENSE](LICENSE) file.