-module(myapp_sup).
-moduledoc "Top level supervisor".

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {
    _SupFlags   = #{strategy => one_for_one, intensity => 1, period => 3},
    _ChildSpecs = []
  }}.
