
-module(filter_sum_values).

-export([sum_values/2]).

sum_values(Values, _Context) ->
    lists:sum(Values).
