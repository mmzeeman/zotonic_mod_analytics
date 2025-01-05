
-module(filter_values).

-export([values/2]).


values(List, _Context) ->
    values1(List, []).

values1([], Acc) ->
    lists:reverse(Acc);
values1([{_, V}|Rest], Acc) ->
    values1(Rest, [V | Acc]).

