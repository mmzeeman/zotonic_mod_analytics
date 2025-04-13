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

%% @doc API for starting the ducklog server.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the notifier gen_server(s) to be used.
init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    Children = [
        #{
            id => analytics_logger,
            start => {analytics_logger, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.
