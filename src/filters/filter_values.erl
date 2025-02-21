
-module(filter_values).

-export([values/2, values/3]).


values(List, _Context) ->
    values1(List, 2, []).

values(undefined, _N, _Context) ->
    [];
values(List, N, _Context) ->
    values1(List, N, []).

values1([], _N, Acc) ->
    lists:reverse(Acc);
values1([Tuple | Rest], N, Acc) when is_tuple(Tuple)->
    values1(Rest, N, [erlang:element(N, Tuple) | Acc]).

