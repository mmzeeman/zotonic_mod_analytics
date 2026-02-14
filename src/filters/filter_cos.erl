%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Cosine trigonometric function filter for template calculations.
%%
%% This filter calculates the cosine of an angle (in radians).
%% Used primarily for SVG path calculations in charts.
%%
%% Example:
%%   {{ 0|cos }}       -> 1.0 (cos of 0 radians)
%%   {{ 1.5708|cos }}  -> 0.0 (cos of π/2 radians = 90 degrees)

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

-module(filter_cos).

-export([cos/2]).

%% @doc Calculate the cosine of a value in radians.
%% Returns the cosine of the input angle (in radians).
%% Non-numeric or undefined values return 1 (cos(0) = 1).
cos(undefined, _Context) ->
    1;
cos(Value, _Context) when is_number(Value) ->
    math:cos(Value);
cos(_Value, _Context) ->
    1.
