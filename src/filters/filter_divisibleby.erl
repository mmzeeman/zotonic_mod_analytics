%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Check if a number is divisible by another number.
%%
%% This filter checks if the first value is evenly divisible by the divisor.
%% Returns true if Value mod Divisor == 0, otherwise false.
%% Used in chart templates to determine when to show labels (e.g., every 5th item).
%%
%% Example:
%%   {{ 10|divisibleby:5 }}  -> true
%%   {{ 7|divisibleby:3 }}   -> false
%%   {{ 0|divisibleby:5 }}   -> true

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

-module(filter_divisibleby).

-export([divisibleby/3]).

%% @doc Check if Value is divisible by Divisor.
%% Returns true if Value mod Divisor == 0, false otherwise.
%% Non-numeric or undefined values return false.
%% Division by zero returns false.
divisibleby(_Value, 0, _Context) -> false;
divisibleby(_Value, +0.0, _Context) -> false;
divisibleby(_Value, -0.0, _Context) -> false;
divisibleby(Value, Divisor, _Context) when is_number(Value), is_number(Divisor) ->
    (trunc(Value) rem trunc(Divisor)) == 0;
divisibleby(_Value, _Divisor, _Context) ->
    false.
