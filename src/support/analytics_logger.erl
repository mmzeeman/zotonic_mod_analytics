%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022-2025 Maas-Maarten Zeeman
%% @doc An access logger writes log entries to a PostgreSQL database using COPY.

%% Copyright 2022-2025 Maas-Maarten Zeeman
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

-module(analytics_logger).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_statem).

%% Api
-export([
    start_link/0,
    log/1,
    get_pool_name/0,
    get_connection/0
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
    rows = [],
    nr_buffered = 0,
    flush_timer = undefined
}).

-define(MAX_BUFFERED, 500).
-define(FLUSH_TIMEOUT_MS, 3000).
-define(POOL_NAME, analytics_logger_db_pool).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%%
%% Api
%%

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Store a log entry in the database
log(#http_log_access{}=Log) ->
    gen_statem:call(?MODULE, {log, Log}).

%% @doc Get the pool name for direct access
get_pool_name() ->
    ?POOL_NAME.

%% @doc Get a connection from the pool
get_connection() ->
    poolboy:checkout(?POOL_NAME).

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

terminate(_Reason, _StateName, #data{flush_timer=Timer}=_Data) ->
    case Timer of
        undefined -> ok;
        TRef -> catch erlang:cancel_timer(TRef)
    end,
    ok.

%%
%% States
%%

%% Initialise the database schema
initialising(enter, _OldState, Data) ->
    case init_schema() of
        ok ->
            ?LOG_INFO(#{text => "Analytics logger initialized"}),
            {next_state, initialising, Data, [{state_timeout, 0, initialised}]};
        {error, Reason} ->
            ?LOG_ERROR(#{text => "Failed to initialize analytics schema", reason => Reason}),
            {next_state, initialising, Data, [{state_timeout, 5000, retry_init}]}
    end;
initialising(state_timeout, initialised, Data) ->
    {next_state, clean, Data};
initialising(state_timeout, retry_init, Data) ->
    {next_state, initialising, Data, [{state_timeout, 0, initialised}]};
initialising(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, initialising, Data).

%%
%% Clean: Nothing is buffered, waiting for incoming requests
%%

clean(enter, _OldState, Data) ->
    {next_state, clean, Data};
clean({call, From}, {log, #http_log_access{}=A}, Data) ->
    Row = format_for_copy(A),
    Rows = [Row],
    
    gen_statem:reply(From, ok),
    {next_state, buffering, Data#data{rows=Rows, nr_buffered=1}};
clean(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, clean, Data).

%%
%% Buffering: Requests have arrived... collecting log messages until either a timeout, or
%% the maximum number of buffered log messages is reached.
%%

buffering(enter, _OldState, Data) ->
    %% Set a timeout to flush
    Timer = erlang:send_after(?FLUSH_TIMEOUT_MS, self(), {state_timeout, flush}),
    {next_state, buffering, Data#data{flush_timer=Timer}};
buffering({call, From}, {log, #http_log_access{}=A}, #data{rows=Rows, nr_buffered=Count}=Data) ->
    Row = format_for_copy(A),
    Rows1 = [Row | Rows],
    Count1 = Count + 1,
    
    gen_statem:reply(From, ok),
    case Count1 >= ?MAX_BUFFERED of
        true ->
            {next_state, flushing, Data#data{rows=Rows1, nr_buffered=Count1}};
        false ->
            {next_state, buffering, Data#data{rows=Rows1, nr_buffered=Count1}}
    end;
buffering(state_timeout, flush, Data) ->
    {next_state, flushing, Data};
buffering(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, buffering, Data).

%%
%% Flushing: Buffered messages are being persisted to the database.
%%

flushing(enter, _OldState, #data{rows=Rows, nr_buffered=Count, flush_timer=Timer}=Data) ->
    %% Cancel any pending timer
    case Timer of
        undefined -> ok;
        TRef -> catch erlang:cancel_timer(TRef)
    end,
    
    case flush_via_copy(Rows) of
        ok ->
            ?LOG_DEBUG(#{
                text => "Analytics batch flushed to PostgreSQL",
                row_count => Count
            }),
            {next_state, flushing, Data#data{rows=[], nr_buffered=0, flush_timer=undefined},
             [{state_timeout, 0, flushed}]};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => "Analytics COPY flush failed - dropping batch",
                reason => Reason,
                row_count => Count
            }),
            {next_state, flushing, Data#data{rows=[], nr_buffered=0, flush_timer=undefined},
             [{state_timeout, 0, flushed}]}
    end;
flushing(state_timeout, flushed, Data) ->
    {next_state, clean, Data};
flushing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, flushing, Data).

%%
%% Helpers
%%

handle_event({call, From}, _CallContent, _StateName, Data) ->
    gen_statem:reply(From, {error, invalid_state}),
    {keep_state, Data};
handle_event(EventType, EventContent, StateName, Data) ->
    ?LOG_WARNING(#{
        text => "Unexpected event in analytics logger",
        event_type => EventType,
        content => EventContent,
        state => StateName
    }),
    {keep_state, Data}.

%%
%% Schema Initialization
%%

init_schema() ->
    DbOpts = get_db_options(),
    Database = proplists:get_value(dbdatabase, DbOpts),
    Schema = proplists:get_value(dbschema, DbOpts),
    
    case open_connection(Database, DbOpts) of
        {ok, Conn} ->
            try
                Result = case schema_exists(Conn, Schema) of
                    true ->
                        ?LOG_INFO(#{text => "Analytics schema exists", schema => Schema}),
                        ok;
                    false ->
                        ?LOG_NOTICE(#{text => "Creating analytics schema", schema => Schema}),
                        create_schema(Conn, Schema)
                end,
                case Result of
                    ok ->
                        %% Schema exists or was created, now create table
                        create_access_log_table(Conn, Schema);
                    SError ->
                        SError
                end
            catch
                Error:Reason:Stack ->
                    ?LOG_ERROR(#{
                        text => "Schema initialization error",
                        error => Error,
                        reason => Reason,
                        stack => Stack
                    }),
                    {error, {Error, Reason}}
            after
                close_connection(Conn)
            end;
        ConnError ->
            ConnError
    end.

schema_exists(Conn, Schema) ->
    case epgsql:equery(
        Conn,
        "SELECT schema_name FROM information_schema.schemata WHERE schema_name = $1",
        [Schema]
    ) of
        {ok, _, Rows} ->
            length(Rows) > 0;
        Error ->
            ?LOG_ERROR(#{text => "Error checking schema existence", error => Error}),
            false
    end.

create_schema(Conn, Schema) ->
    case epgsql:equery(Conn, "CREATE SCHEMA \"" ++ Schema ++ "\"", []) of
        {ok, _, _} ->
            ?LOG_NOTICE(#{text => "Analytics schema created", schema => Schema}),
            ok;
        {error, #error{codename = duplicate_schema}} ->
            ?LOG_INFO(#{text => "Schema already exists", schema => Schema}),
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => "Failed to create analytics schema",
                schema => Schema,
                error => Reason
            }),
            Error
    end.

create_access_log_table(Conn, Schema) ->
    SchemaTable = Schema ++ ".access_log",
    
    TableSQL = "
        CREATE TABLE IF NOT EXISTS " ++ SchemaTable ++ " (
            id BIGSERIAL PRIMARY KEY,
            req_version VARCHAR(10),
            req_method VARCHAR(10),
            req_bytes INTEGER,
            resp_category SMALLINT,
            resp_code SMALLINT,
            resp_bytes INTEGER,
            site VARCHAR(128),
            path VARCHAR(512),
            qs VARCHAR(512),
            referer VARCHAR(512),
            controller VARCHAR(128),
            dispatch_rule VARCHAR(128),
            rsc_id INTEGER,
            duration_process INTEGER,
            duration_total INTEGER,
            peer_ip INET,
            session_id VARCHAR(50),
            user_id INTEGER,
            language VARCHAR(10),
            timezone VARCHAR(64),
            user_agent TEXT,
            timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )",
    
    case epgsql:squery(Conn, TableSQL) of
        {ok, _} -> 
            create_indexes(Conn, Schema);
        [{ok, _}] -> 
            create_indexes(Conn, Schema);
        {error, {code, <<"42P07">>, _}} -> 
            % Table already exists
            create_indexes(Conn, Schema);
        Error -> 
            ?LOG_ERROR(#{text => "Failed to create access_log table", error => Error}),
            Error
    end.

create_indexes(Conn, Schema) ->
    SchemaTable = Schema ++ ".access_log",
    Indexes = [
        "CREATE INDEX IF NOT EXISTS idx_access_log_timestamp ON " ++ SchemaTable ++ " (timestamp DESC)",
        "CREATE INDEX IF NOT EXISTS idx_access_log_site_timestamp ON " ++ SchemaTable ++ " (site, timestamp DESC)",
        "CREATE INDEX IF NOT EXISTS idx_access_log_rsc_id ON " ++ SchemaTable ++ " (rsc_id)",
        "CREATE INDEX IF NOT EXISTS idx_access_log_user_id ON " ++ SchemaTable ++ " (user_id)",
        "CREATE INDEX IF NOT EXISTS idx_access_log_session_id ON " ++ SchemaTable ++ " (session_id)"
    ],
    create_indexes_loop(Conn, Indexes).

create_indexes_loop(_Conn, []) ->
    ?LOG_INFO(#{text => "Analytics schema verified with all indexes"}),
    ok;
create_indexes_loop(Conn, [IndexSQL | Rest]) ->
    case epgsql:squery(Conn, IndexSQL) of
        {ok, _} ->
            create_indexes_loop(Conn, Rest);
        [{ok, _}] ->
            create_indexes_loop(Conn, Rest);
        {error, {code, <<"42P07">>, _}} ->
            % Index already exists, continue
            create_indexes_loop(Conn, Rest);
        Error ->
            ?LOG_WARNING(#{text => "Failed to create index", error => Error}),
            create_indexes_loop(Conn, Rest)
    end.

%%
%% Connection Management
%%

open_connection(Database, Options) ->
    epgsql:connect(z_db_pgsql:build_connect_options(Database, Options)).

close_connection(Connection) ->
    epgsql:close(Connection).

get_db_options() ->
    [
        {dbhost, z_config:get(analytics_db_host, "localhost")},
        {dbport, z_config:get(analytics_db_port, 5432)},
        {dbuser, z_config:get(analytics_db_user, "postgres")},
        {dbpassword, z_config:get(analytics_db_password, "")},
        {dbdatabase, z_config:get(analytics_db_name, "analytics")},
        {dbschema, z_config:get(analytics_db_schema, "public")}
    ].

%%
%% Format log entry for COPY
%%

format_for_copy(#http_log_access{
    timestamp=Ts,
    status=Status,
    status_category=StatusCategory,
    method=Method,
    metrics=Metrics
}) ->
    M = maps:get(metrics, Metrics, #{}),
    
    {
        null_or_value(maps:get(http_version, Metrics, undefined)),
        atom_to_binary(Method, utf8),
        null_or_value(maps:get(req_bytes, Metrics, undefined)),
        status_category_to_int(StatusCategory),
        Status,
        null_or_value(maps:get(resp_bytes, Metrics, undefined)),
        null_or_value(maps:get(site, Metrics, undefined)),
        null_or_value(maps:get(path, Metrics, undefined)),
        null_or_qs(maps:get(qs, Metrics, undefined)),
        null_or_value(maps:get(referer, Metrics, undefined)),
        null_or_value(maps:get(controller, M, undefined)),
        null_or_value(maps:get(dispatch_rule, M, undefined)),
        null_or_value(maps:get(rsc_id, M, undefined)),
        maps:get(duration_process_usec, Metrics, 0),
        maps:get(duration_total_usec, Metrics, 0),
        format_ip(maps:get(peer_ip, M, undefined)),
        null_or_value(maps:get(session_id, M, undefined)),
        null_or_value(maps:get(user_id, M, undefined)),
        null_or_value(maps:get(language, Metrics, undefined)),
        null_or_value(maps:get(timezone, Metrics, undefined)),
        null_or_value(maps:get(user_agent, Metrics, undefined)),
        format_timestamp(Ts)
    }.

null_or_value(undefined) -> null;
null_or_value(Value) -> Value.

null_or_qs(undefined) -> null;
null_or_qs(<<>>) -> null;
null_or_qs(Qs) -> Qs.

format_ip(undefined) -> null;
format_ip({A,B,C,D}) ->
    iolist_to_binary(io_lib:format("~w.~w.~w.~w", [A,B,C,D]));
format_ip({_,_,_,_,_,_,_,_}=IPv6) ->
    iolist_to_binary(inet:ntoa(IPv6)).

format_timestamp({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Y,Mo,D,H,Mi,S])).

status_category_to_int('1xx') -> 1;
status_category_to_int('2xx') -> 2;
status_category_to_int('3xx') -> 3;
status_category_to_int('4xx') -> 4;
status_category_to_int('5xx') -> 5;
status_category_to_int('xxx') -> 0;
status_category_to_int(_) -> 0.

%%
%% Flush via PostgreSQL COPY
%%

flush_via_copy([]) ->
    ok;
flush_via_copy(Rows) ->
    case get_connection() of
        {ok, Conn} ->
            try
                do_flush_copy(Conn, Rows)
            catch
                Error:Reason ->
                    ?LOG_ERROR(#{text => "COPY exception", error => Error, reason => Reason}),
                    {error, {Error, Reason}}
            after
                poolboy:checkin(?POOL_NAME, Conn)
            end;
        {error, Error} ->
            ?LOG_ERROR(#{text => "Failed to get connection from pool", error => Error}),
            {error, Error}
    end.

do_flush_copy(Conn, Rows) ->
    Schema = z_config:get(analytics_db_schema, "public"),
    SchemaTable = Schema ++ ".access_log",
    
    %% Start COPY command
    CopySQL = <<"COPY ", (iolist_to_binary(SchemaTable))/binary, " (
                    req_version, req_method, req_bytes,
                    resp_category, resp_code, resp_bytes,
                    site, path, qs, referer,
                    controller, dispatch_rule, rsc_id,
                    duration_process, duration_total,
                    peer_ip, session_id, user_id,
                    language, timezone, user_agent, timestamp
                ) FROM STDIN (FORMAT CSV, DELIMITER E'\t', NULL 'null')">>,
    
    case epgsql:squery(Conn, CopySQL) of
        {copy, _} ->
            %% Stream the rows using epgsql's copy protocol
            case stream_copy_rows(Conn, Rows) of
                ok ->
                    %% End COPY with successful marker
                    case epgsql:put_copy_end(Conn) of
                        ok -> ok;
                        {error, _} = Error -> Error
                    end;
                Error ->
                    %% Abort copy on error
                    catch epgsql:put_copy_end(Conn, error),
                    Error
            end;
        SQueryError ->
            SQueryError
    end.

stream_copy_rows(_Conn, []) ->
    ok;
stream_copy_rows(Conn, [Row | Rest]) ->
    Line = format_row_for_copy(Row),
    case epgsql:put_copy_data(Conn, Line) of
        ok -> stream_copy_rows(Conn, Rest);
        Error -> Error
    end.

format_row_for_copy(Row) ->
    %% Convert tuple to tab-separated values with null handling
    Values = tuple_to_list(Row),
    FormattedValues = [format_copy_value(V) || V <- Values],
    Line = string:join(FormattedValues, "\t"),
    iolist_to_binary([Line, "\n"]).

format_copy_value(null) -> "null";
format_copy_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_copy_value(Value) when is_binary(Value) ->
    escape_copy_value(Value);
format_copy_value(Value) when is_atom(Value) ->
    escape_copy_value(atom_to_binary(Value, utf8));
format_copy_value(Value) ->
    escape_copy_value(z_convert:to_binary(Value)).

escape_copy_value(Value) ->
    %% Escape backslash and newline for CSV in COPY
    V1 = binary:replace(Value, <<"\\">>, <<"\\\\">>, [global]),
    V2 = binary:replace(V1, <<"\n">>, <<"\\n">>, [global]),
    V3 = binary:replace(V2, <<"\t">>, <<"\\t">>, [global]),
    V4 = binary:replace(V3, <<"\r">>, <<"\\r">>, [global]),
    V4.
