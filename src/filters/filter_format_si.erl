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

%% Return a compact SI-formatted string for numbers.
format_si(Value, _Context) when is_number(Value) ->
    format_number(Value);
format_si(Value, _Context) ->
    % Fallback: return the unchanged value
    Value.

%% Internal: choose suffix and format
format_number(Value) ->
    Abs = abs(Value),
    {Factor, Suffix} = case is_almost_integer(Abs) of
               true -> choose_factor(round(Abs));
               false -> choose_factor(Abs)
           end,
    case Factor of
        1 ->
            format(Value);
        _ ->
            Scaled = Value / Factor,
            Formatted = format(Scaled),
            <<Formatted/binary, Suffix/binary>>
    end.

choose_factor(Abs) when Abs >= 1000000000000 -> {1000000000000, <<"T">>};
choose_factor(Abs) when Abs >= 1000000000 -> {1000000000000, <<"G">>};
choose_factor(Abs) when Abs >= 1000000 -> {1000000, <<"M">>};
choose_factor(Abs) when Abs >= 1000 -> {1000, <<"k">>};
choose_factor(Abs) when Abs >= 1 -> {1, <<"">>}.

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
