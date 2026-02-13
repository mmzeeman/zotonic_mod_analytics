%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Zip two lists together into a list of tuples.
%%
%% This filter combines two lists element-wise into a list of 2-tuples,
%% similar to Python's zip() or Erlang's lists:zip/2.
%%
%% Example:
%%   [1, 2, 3]|zip:[a, b, c] -> [{1, a}, {2, b}, {3, c}]

%% Copyright 2025 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(filter_zip).

-export([zip/3]).

%% @doc Zip two lists together into a list of tuples.
%% Returns a list of 2-tuples where each tuple contains corresponding
%% elements from the two input lists.
%% If lists are of different lengths, stops at the shorter list.
zip(undefined, _List2, _Context) ->
    [];
zip(_List1, undefined, _Context) ->
    [];
zip(List1, List2, _Context) when is_list(List1), is_list(List2) ->
    lists:zip(List1, List2);
zip(_List1, _List2, _Context) ->
    [].
