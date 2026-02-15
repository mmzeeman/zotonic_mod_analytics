%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Absolute vakue filter for template calculations.
%%
%% This filter calculates the absolute value of a number.
%%
%% Example:
%%   {{ 0 | abs }}       -> 0 
%%   {{ -11.5708 | abs }}  -> 11.57080

%% Copyright 2026 Maas-Maarten Zeeman
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

-module(filter_abs).

-export([abs/2]).

%% @doc Calculate the absulute value of a number.
abs(Value, _Context) when is_number(Value) ->
    abs(Value);
abs(_Value, _Context) ->
    undefined.
