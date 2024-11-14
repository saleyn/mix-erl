-module(myapp_app).
-moduledoc "Test app".

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  myapp_sup:start_link().

stop(_State) ->
  ok.
