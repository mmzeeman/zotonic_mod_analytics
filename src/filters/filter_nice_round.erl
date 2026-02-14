%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Round a number up to a nice rounded value for chart axes.
%%
%% This filter rounds up values to nice numbers suitable for chart axes.
%% For example:
%%   9 -> 10
%%   47 -> 50
%%   3225 -> 3500
%%   850 -> 1000
%%
%% The algorithm finds an appropriate step size based on the magnitude
%% of the number and rounds up to the nearest nice value.

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

-module(filter_nice_round).

-export([nice_round/2]).

%% @doc Round a value up to a nice number for chart axes.
%% Returns a rounded value suitable for displaying on charts.
nice_round(undefined, _Context) ->
    0;
nice_round(Value, _Context) when is_number(Value), Value =< 0 ->
    0;
nice_round(Value, _Context) when is_number(Value) ->
    calculate_nice_ceiling(Value);
nice_round(Value, Context) when is_binary(Value) ->
    try
        NumValue = binary_to_float(Value),
        nice_round(NumValue, Context)
    catch
        _:_ ->
            try
                NumValue = float(binary_to_integer(Value)),
                nice_round(NumValue, Context)
            catch
                _:_ -> 0
            end
    end;
nice_round(_Value, _Context) ->
    0.

%% Internal function to calculate nice ceiling
calculate_nice_ceiling(Value) when Value =< 0 ->
    0;
calculate_nice_ceiling(Value) ->
    % Find the order of magnitude
    Exponent = math:floor(math:log10(Value)),
    Power = math:pow(10, Exponent),
    
    % Normalize to 1-10 range
    Normalized = Value / Power,
    
    % Choose nice step: 1, 2, 5, or 10
    NiceFactor = if
        Normalized =< 1.0 -> 1;
        Normalized =< 2.0 -> 2;
        Normalized =< 5.0 -> 5;
        true -> 10
    end,
    
    % Calculate the nice ceiling
    NiceValue = NiceFactor * Power,
    
    % Return as integer if it's a whole number, otherwise float
    case NiceValue == trunc(NiceValue) of
        true -> trunc(NiceValue);
        false -> NiceValue
    end.
