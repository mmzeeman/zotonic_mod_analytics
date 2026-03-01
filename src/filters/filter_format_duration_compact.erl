-module(filter_format_duration_compact).
-export([format_duration_compact/2]).

format_duration_compact(Seconds, _Context) when is_float(Seconds) ->
    format_duration_compact(round(Seconds), _Context);
format_duration_compact(Seconds, _Context) when is_integer(Seconds) ->
    TotalMinutes = Seconds div 60,
    RemSeconds   = Seconds rem 60,
    Hours        = TotalMinutes div 60,
    RemMinutes   = TotalMinutes rem 60,
    if
        Hours > 0 ->
            iolist_to_binary(io_lib:format("~bh ~2..0bm", [Hours, RemMinutes]));
        true ->
            iolist_to_binary(io_lib:format("~bm ~2..0bs", [TotalMinutes, RemSeconds]))
    end;
format_duration_compact(_, _Context) ->
    undefined.
