# Mix Erl

Mix Tasks for compiling Erlang projects.

This project implements targets for running the following Erlang tools:

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

This will enable the targets mentioned above to be given to `mix` in order
to run
