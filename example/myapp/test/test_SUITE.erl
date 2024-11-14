-module(test_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0]).
-export([add/1, subtract/1]).

-doc "Returns list of tuples to set default properties for the suite".
suite() ->
  [].

-doc "Returns the list of groups and test cases that are to be executed".
all() ->
  [add, subtract].

%% Test cases

add(_Config) ->
  5 = myapp:add(2, 3).

subtract(_Config) ->
  1 = myapp:subtract(5, 4).
