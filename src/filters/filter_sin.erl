%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Sine trigonometric function filter for template calculations.
%%
%% This filter calculates the sine of an angle (in radians).
%% Used primarily for SVG path calculations in charts.
%%
%% Example:
%%   {{ 1.5708|sin }}  -> 1.0 (sin of π/2 radians = 90 degrees)
%%   {{ 0|sin }}       -> 0.0 (sin of 0 radians)

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

-module(filter_sin).

-export([sin/2]).

%% @doc Calculate the sine of a value in radians.
%% Returns the sine of the input angle (in radians).
%% Non-numeric or undefined values return 0.
sin(undefined, _Context) ->
    0;
sin(Value, _Context) when is_number(Value) ->
    math:sin(Value);
sin(Value, _Context) when is_binary(Value) ->
    try
        math:sin(binary_to_float(Value))
    catch
        _:_ ->
            try
                math:sin(float(binary_to_integer(Value)))
            catch
                _:_ -> 0
            end
    end;
sin(_Value, _Context) ->
    0.
