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
    log/1
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
    appender 
}).


-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Api
%%

start_link() ->
    ?DEBUG(start_link),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Store a log entry in the database
log(#http_log_access{}=Log) ->
    ?DEBUG(log),
    ?DEBUG(gen_statem:call(?MODULE, {log, Log})).


%%
%% gen_statem callbacks
%%

init([]) ->
    ?DEBUG(init),
    {ok, initialising, #data{}}. 

callback_mode() ->
    [state_functions, state_enter].

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(Reason, _StateName, _Data) ->
    ?DEBUG({terminate, Reason}),
    ok.

%%
%% States
%%

%% Initialise the database and schema
initialising(enter, _OldState, Data) ->
    ?DEBUG({enter_initialising, Data}),

    {ok, DB} = educkdb:open("ducklog.db"),
    {ok, Conn} = educkdb:connect(DB),

    case squery(Conn,
    educkdb

    %% [TODO] open the database, check schema and move to the right state.
    {next_state, initialising, Data#data{database=DB, connection=Conn},
     [{state_timeout, 0, initialised}]};
initialising(state_timeout, initialised, Data) ->
    {next_state, clean, Data};
initialising(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, initialising, Data).

%% Clean, and waiting for input
clean(enter, _OldState, Data) ->
    {next_state, clean, Data};
clean(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, clean, Data).

%% Buffering log messages via the appender.
buffering(enter, _OldState, Data) ->
    {next_state, buffering, Data};
buffering(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, buffering, Data).

%% Flush the appender and move to clean state.
flushing(enter, _OldState, Data) ->
    ?DEBUG({enter_flusing, Data}),
    %% [TODO] Flush the appender and reset the count
    {next_state, flushing, Data, [{state_timeout, 0, flushed}]};
flushing(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, flushing, Data).

%%
%% Helpers
%%

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

squery(Conn, Query) ->
    case educkdb:query(Conn, Query) of
        {ok, Result} -> educkdb:extract_result(Result);
        {error, _}=E -> E
    end.

table_exists(Conn, Name) ->
    case squery(Conn, ) of


