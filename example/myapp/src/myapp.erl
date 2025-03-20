-module(myapp).

-export([add/2, subtract/2, welcome/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

add(A, B) when is_number(A), is_number(B) ->
  A + B.

subtract(A, B) when is_number(A), is_number(B) ->
  A - B.

welcome() ->
    welcome_dtl:render([
        {name, "Johnny"},
        {friends, [<<"Frankie Lee">>, <<"Judas Priest">>]},
        {primes, [1, 2, "3", <<"5">>]}
    ]).

-ifdef(EUNIT).

add_test_() ->
  [
   ?_assertEqual(add(2,   3),  5),
   ?_assertEqual(add(-2, -3), -5)
  ].

subtract_test_() ->
  [
   ?_assertEqual(subtract(3,   2),  1),
   ?_assertEqual(subtract(-3, -2), -1)
  ].

-endif.
