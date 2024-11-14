-module(myapp_sup).
-moduledoc "Top level supervisor".

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok,
    _SupFlags   = #{strategy => one_for_all, intensity => 0, period => 1},
    _ChildSpecs = []
  }.
