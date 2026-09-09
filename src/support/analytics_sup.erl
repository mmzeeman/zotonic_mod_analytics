%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022 Maas-Maarren Zeeman
%% @doc Supervisor analytics logger

%% Copyright 2022-2025 Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
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

-module(analytics_sup).
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc API for starting the analytics supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the supervisor tree for analytics.
init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    
    %% Create a database pool for analytics
    PoolSpec = create_pool_spec(),
    
    Children = [
        %% Database pool must start before the logger
        PoolSpec,
        #{
            id => analytics_logger,
            start => {analytics_logger, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.

%%
%% Helper functions
%%

%% @doc Create a poolboy childspec for the analytics database pool
create_pool_spec() ->
    PoolName = analytics_logger:get_pool_name(),
    PoolSize = z_config:get(analytics_db_pool_size, 5),
    
    DbOpts = get_db_options(),
    
    PoolArgs = [
        {name, {local, PoolName}},
        {worker_module, z_db_pgsql},
        {size, PoolSize},
        {max_overflow, 0}
    ],
    
    poolboy:child_spec(PoolName, PoolArgs, DbOpts).

%% @doc Get PostgreSQL connection options from config
get_db_options() ->
    [
        {dbhost, z_config:get(analytics_db_host, "localhost")},
        {dbport, z_config:get(analytics_db_port, 5432)},
        {dbuser, z_config:get(analytics_db_user, "zotonic")},
        {dbpassword, z_config:get(analytics_db_password, "zotonic")},
        {dbdatabase, z_config:get(analytics_db_name, "zotonic")},
        {dbschema, z_config:get(analytics_db_schema, "analytics")}
    ].
