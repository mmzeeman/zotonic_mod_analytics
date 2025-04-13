%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2022-2025 Maas-Maarten Zeeman 
%% @doc Analytics for Zotonic sites.

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

-module(mod_analytics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Analytics").
-mod_description("Analytics for Zotonic sites.").

-mod_depends([admin]).
-mod_provides([]).

-export([
    observe_http_log_access/2,
    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

observe_http_log_access(#http_log_access{} = Log, _Context) ->
    analytics_logger:log(Log),
    ok.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_analytics,
                parent=admin_system,
                label=?__("Analytics", Context),
                url={admin_analytics, []},
                visiblecheck={acl, use, mod_ducklog}}
     |Acc].
