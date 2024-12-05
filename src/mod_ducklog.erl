%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022 Maas-Maarten Zeeman 
%% @doc Log http requests to an duckdb olap database.

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

-module(mod_ducklog).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Ducklog").
-mod_description("Log http requests to a duckdb database.").
-mod_depends([]).
-mod_provides([]).

-export([
    observe_http_log_access/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

observe_http_log_access(#http_log_access{} = Log, Context) ->
    z_ducklog_logger:log(Log),
    ok.

