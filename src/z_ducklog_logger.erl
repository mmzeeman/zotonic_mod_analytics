%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022 Maas-Maarten Zeeman
%% @doc An access logger writes log entries to a duckdb database.

%% Copyright 2022 Maas-Maarten Zeeman
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

-module(z_ducklog_logger).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_statem).

%% Api

-export([
    start_link/0,
    log/1,
    get_connection/0,
    database_name/0,

    inet_type/1,
    inet_v4_ntoe/1,
    inet_v6_ntoe/1,
    inet_eton/1
]).

%% gen_statem exports
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4
]).

%% states
-export([
    initialising/3,
    clean/3,
    buffering/3,
    flushing/3
]).

-record(data, {
          database,
          connection,
          appender,

          nr_buffered = 0
}).

-define(MAX_BUFFERED, 250).
-define(DATABASE_NAME, "ducklog.db").

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Api
%%

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Store a log entry in the database
log(#http_log_access{}=Log) ->
    gen_statem:call(?MODULE, {log, Log}).

%% @doc Get a connection to the database.
get_connection() ->
    gen_statem:call(?MODULE, get_connection).

database_name() ->
    case z_config:get(data_dir) of
        undefined ->
            ?DATABASE_NAME;
        DataDir ->
            filename:join([ DataDir, atom_to_list(node()), ?DATABASE_NAME ])
    end.

%%
%% gen_statem callbacks
%%

init([]) ->
    process_flag(trap_exit, true),
    {ok, initialising, #data{}}. 

callback_mode() ->
    [state_functions, state_enter].

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _StateName, #data{ database = Database, connection = Connection } ) ->
    case Connection of
        undefined ->
            ok;
        _ ->
            ok = educkdb:disconnect(Connection),
            ok = educkdb:close(Database)
    end.

%%
%% States
%%

%% Initialise the database and schema
initialising(enter, _OldState, Data) ->
    DatabaseName = database_name(),
    {ok, DB} = educkdb:open(DatabaseName),
    {ok, Conn} = educkdb:connect(DB),
    ensure_log_table(Conn),

    {next_state, initialising, Data#data{database=DB, connection=Conn}, [{state_timeout, 0, initialised}]};
initialising(state_timeout, initialised, Data) ->
    {next_state, clean, Data};
initialising(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, initialising, Data).


%%
%% Clean: Nothing is buffered, waiting for incoming requests
%%

%% Clean, and waiting for input
clean(enter, _OldState, Data) ->
    {next_state, clean, Data};
clean({call, From}, {log, #http_log_access{}=A}, #data{connection=Conn}=Data) ->
    %% Create the appender, and go to buffering state.
    {ok, Appender} = educkdb:appender_create(Conn, undefined, <<"access_log">>),
    
    gen_statem:reply(From, append(Appender, A)),
    {next_state, buffering, Data#data{ appender=Appender, nr_buffered=1 }};

clean(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, clean, Data).

%%
%% Buffering: Requests have arrived... collecting log messages until either a timeout, or
%% the maximum number of buffered log messages is reached.
%%

%% Buffering log messages via the appender.
buffering(enter, _OldState, Data) ->
    %% Create the appender.
    
    %% Set a timeout to flush... 
    {next_state, buffering, Data, [{state_timeout, 2000, flush}]};

buffering({call, From}, {log, #http_log_access{}=A}, #data{appender=Appender, nr_buffered = Count}=Data) ->
    gen_statem:reply(From, append(Appender, A)),
    Count1 = Count + 1,
    case Count1 >= ?MAX_BUFFERED of
       true -> 
            {next_state, flushing, Data#data{ nr_buffered = Count1 }};
       false ->
            {next_state, buffering, Data#data{ nr_buffered = Count1 }}
    end;

buffering(state_timeout, flush, Data) ->
    {next_state, flushing, Data};

buffering(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, buffering, Data).

%%
%% Flushing: Buffered messages are being persisted to the database.
%%

%% Flush the appender and move to clean state.
flushing(enter, _OldState, #data{appender=Appender}=Data) ->
    ok = educkdb:appender_flush(Appender),
    %% Drop the reference to the appender
    {next_state, flushing, Data#data{ appender=undefined }, [{state_timeout, 0, flushed}]};
flushing(state_timeout, flushed, Data) ->
    {next_state, clean, Data};

flushing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, flushing, Data).

%%
%% Helpers
%%

append(Appender, #http_log_access{timestamp=Ts,
                                  status=Status,
                                  status_category=StatusCategory,
                                  method=Method,
                                  metrics=Metrics
                                 }) ->

    M = maps:get(metrics, Metrics, #{}),

    append_value(Appender, maps:get(http_version, Metrics, undefined)),
    append_value(Appender, Method),

    append_value(Appender, maps:get(req_bytes, Metrics, undefined)),

    ok = educkdb:append_uint8(Appender, status_category_to_uint8(StatusCategory)),

    append_value(Appender, Status),
    append_value(Appender, maps:get(resp_bytes, Metrics, undefined)),

    append_value(Appender, maps:get(site, Metrics, undefined)),
    append_value(Appender, maps:get(path, Metrics, undefined)),
    append_value(Appender, maps:get(referer, Metrics, undefined)),

    append_value(Appender, maps:get(controller, M, undefined)),
    append_value(Appender, maps:get(dispatch_rule, M, undefined)),
    append_value(Appender, maps:get(rsc_id, M, undefined)),

    ok = educkdb:append_uint32(Appender, maps:get(duration_process_usec, Metrics, 0)),
    ok = educkdb:append_uint32(Appender, maps:get(duration_total_usec, Metrics, 0)),

    append_value(Appender, maps:get(peer_ip, M, undefined)),

    append_value(Appender, maps:get(session_id, M, undefined)),
    case maps:get(user_id, M, undefined) of
        UserId when is_integer(UserId) ->
            ok = educkdb:append_uint32(Appender, UserId);
        _ ->
            ok = educkdb:append_null(Appender)
    end,

    append_value(Appender, maps:get(language, Metrics, undefined)),
    append_value(Appender, maps:get(timezone, Metrics, undefined)),
    append_value(Appender, maps:get(user_agent, Metrics, undefined)),
    append_value(Appender, Ts),

    ok = educkdb:appender_end_row(Appender).

%%
%%
%%

append_value(Appender, undefined) ->
    ok = educkdb:append_null(Appender);

%% IP Addresses
append_value(Appender, {_,_,_,_}=Ipv4) ->
    ok = educkdb:append_varchar(Appender, inet:ntoa(Ipv4));
append_value(Appender, {_,_,_,_,_,_,_,_}=Ipv6) ->
    ok = educkdb:append_varchar(Appender, inet:ntoa(Ipv6));
append_value(Appender, {M,S,Mi}=Ts) when is_integer(M) andalso is_integer(S) andalso is_integer(Mi) ->
    ok = educkdb:append_timestamp(Appender, Ts);
append_value(Appender, Integer) when is_integer(Integer) ->
    ok = educkdb:append_int32(Appender, Integer);
append_value(Appender, Atom) when is_atom(Atom) ->
    ok = educkdb:append_varchar(Appender, z_convert:to_binary(Atom));
append_value(Appender, Bin) when is_binary(Bin) ->
    ok = educkdb:append_varchar(Appender, Bin).

handle_event({call, From}, get_connection, _, #data{database=Db}=Data) ->
    {ok, Conn} = educkdb:connect(Db),
    gen_statem:reply(From, {ok, Conn}),
    {keep_state, Data};
handle_event({call, From}, CallContent, StateName, Data) ->
    ?LOG_ERROR(#{ text => "Unexpected call in state",
                  content => CallContent,
                  state => StateName} ),
    gen_statem:reply(From, {error, unexpected}),
    {keep_state, Data};
handle_event(EventType, EventContent, StateName, Data) ->
    ?LOG_ERROR(#{ text => "Unexpected event in state",
                  event_type => EventType,
                  content => EventContent,
                  state => StateName} ),
    {keep_state, Data}.


%%
%% Helpers
%%


%%
%% Database Helpers
%%


table_exists(Conn, Table) ->
    case educkdb:squery(Conn, "PRAGMA show_tables;") of
        {ok, [ #{ data := ExistingTables, name := <<"name">> } ]} ->
            lists:member(Table, ExistingTables);
        {ok, _} ->
            false
    end.

ensure_log_table(Conn) ->
    case table_exists(Conn, <<"access_log">>) of
       true ->
            ok;
       false ->
            {ok, _} = educkdb:squery(Conn, "CREATE TABLE access_log (
                                                req_version VARCHAR(10),
                                                req_method VARCHAR(10),

                                                req_bytes UINTEGER,

                                                resp_category UTINYINT,
                                                resp_code USMALLINT,
                                                resp_bytes UINTEGER,

                                                site VARCHAR(128),
                                                path VARCHAR(512),
                                                referer VARCHAR(512),
                                            
                                                controller VARCHAR(128),
                                                dispatch_rule VARCHAR(128),
                                                rsc_id uinteger,

                                                duration_process uinteger,
                                                duration_total uinteger,

                                                peer_ip varchar,
                                                session_id varchar(50),
                                                user_id uinteger,
                                                language varchar(10),
                                                timezone varchar(64), 
                                                user_agent varchar,
                                              
                                                timestamp timestamp
                                         )"),
            ok
    end.

%%
%% Inet helpers.
%%

inet_type({_,_,_,_}) -> 4;
inet_type({_,_,_,_,_,_,_,_}) -> 6.

%% Erlang to network byte order integer
inet_eton({N1, N2, N3, N4}) ->
    (N1 bsl 24) +  (N2 bsl 16) + (N3 bsl 8) + N4;
inet_eton({N1, N2, N3, N4, N5, N6, N7, N8}) ->
    (N1 bsl 112) + (N2 bsl 96) + (N3 bsl 80) + (N4 bsl 64) + (N5 bsl 48) + (N6 bsl 32) (N7 bsl 16) + N8.
                                         

%% Ip-address from integer to erlang
inet_v4_ntoe(Num) ->
    N1 = (Num band 16#FF000000) bsr 24,
    N2 = (Num band 16#00FF0000) bsr 16,
    N3 = (Num band 16#0000FF00) bsr 8,
    N4 =  Num band 16#000000FF,
    {N1, N2, N3, N4}.

inet_v6_ntoe(Num) ->
    N1 = (Num band 16#FFFF_0000_0000_0000_0000_0000_0000_0000) bsr 112,
    N2 = (Num band 16#0000_FFFF_0000_0000_0000_0000_0000_0000) bsr 96,
    N3 = (Num band 16#0000_0000_FFFF_0000_0000_0000_0000_0000) bsr 80,
    N4 = (Num band 16#0000_0000_0000_FFFF_0000_0000_0000_0000) bsr 64,
    N5 = (Num band 16#0000_0000_0000_0000_FFFF_0000_0000_0000) bsr 48,
    N6 = (Num band 16#0000_0000_0000_0000_0000_FFFF_0000_0000) bsr 32,
    N7 = (Num band 16#0000_0000_0000_0000_0000_0000_FFFF_0000) bsr 16,
    N8 =  Num band 16#0000_0000_0000_0000_0000_0000_0000_FFFF,
    {N1, N2, N3, N4, N5, N6, N7, N8}.

%%
%% Helpers
%%

status_category_to_uint8('1xx') -> 1;
status_category_to_uint8('2xx') -> 2;
status_category_to_uint8('3xx') -> 3;
status_category_to_uint8('4xx') -> 4;
status_category_to_uint8('5xx') -> 5;
status_category_to_uint8('xxx') -> 0;
status_category_to_uint8(_) -> 16#FF.


