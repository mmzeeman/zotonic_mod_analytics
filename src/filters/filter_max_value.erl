
-module(filter_max_value).

-export([max_value/2]).

max_value(List, _Context) ->
    lists:max(List).
