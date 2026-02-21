%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc
%% Template filter to format numbers with SI-style prefixes (k, M, G, ...).
%% Basic implementation: no precision or unit parameters.
%% Examples:
%%   {{ 20000 | format_si }} -> "20k"
%%   {{ 1500  | format_si }} -> "1.5k"
%%   {{ 950   | format_si }} -> "950"
%%   {{ -1200000 | format_si }} -> "-1.2M"

-module(filter_format_si).

-export([format_si/2]).

-define(T, 1000000000000).
-define(G, 1000000000).
-define(M, 1000000).
-define(k, 1000).

%% Return a compact SI-formatted string for numbers.
format_si(undefined, _Context) ->
    undefined;
format_si(Value, _Context) when is_number(Value) ->
    format_number(Value).

format_number(Value) when is_float(Value) ->
    case is_almost_integer(Value) of
        true -> format_number1(round(Value));
        false -> format_number1(Value)
    end;
format_number(Value) ->
    format_number1(Value).

format_number1(Value) ->
    {Factor, Suffix} = choose_factor(abs(Value)),
    Scaled = Value / Factor,
    Formatted = format(Scaled),
    <<Formatted/binary, Suffix/binary>>.

choose_factor(Abs) when Abs >= ?T -> {?T, <<"T">>};
choose_factor(Abs) when Abs >= ?G -> {?G, <<"G">>};
choose_factor(Abs) when Abs >= ?M -> {?M, <<"M">>};
choose_factor(Abs) when Abs >= ?k -> {?k, <<"k">>};
choose_factor(_Abs) -> {1, <<"">>}.

format(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format(Value) ->
    case is_almost_integer(Value) of
        true ->
            integer_to_binary(round(Value));
        false ->
            z_convert:to_binary(io_lib:format("~.1f", [Value]))
    end.

is_almost_integer(X) when is_integer(X) ->
    true;
is_almost_integer(X) when is_float(X) ->
    Diff = abs(X - round(X)),
    Diff < 0.001;
is_almost_integer(_Other) ->
    false.
