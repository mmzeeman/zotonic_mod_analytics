%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022-2024 Maas-Maarten Zeeman
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

-module(z_ducklog).

-include_lib("zotonic_core/include/zotonic.hrl").



-export([
    q/1,
    q/2
]).

q(Query) ->
    with_connection(fun(Conn) ->
                            {ok, Res} = educkdb:query(Conn, Query),
                            educkdb:result_extract(Res)
                    end).

q(Query, Args) ->
    with_connection(fun(Conn) ->
                            {ok, Prepared} = educkdb:prepare(Conn, Query),
                            ok = bind_all(Prepared, Args),
                            educkdb:execute(Prepared)
                    end).

bind_all(PreparedStatement, Args) ->
    bind_all(PreparedStatement, Args, 1).


bind_all(_PreparedStatement, [], _I) ->
    ok;
bind_all(Stmt, [Elt | Rest], I) ->
    ok = bind(Stmt, I, Elt),
    bind_all(Stmt, Rest, I+1).

bind(Stmt, I, Elt) when is_boolean(Elt) ->
    educkdb:bind_boolean(Stmt, I, Elt);
bind(Stmt, I, Elt) when is_float(Elt) ->
    educkdb:bind_float(Stmt, I, Elt).


%%
%% Helpers
%%

with_connection(F) ->
    {ok, Conn} = z_ducklog_logger:get_connection(),
    try
        F(Conn)
    after
        %ok = educkdb:disconnect(Conn)
        ok
    end.
