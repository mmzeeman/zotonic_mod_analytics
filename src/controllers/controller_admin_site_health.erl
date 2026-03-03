%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc View zotonic site health 

%% Copyright 2026 Maas-Maarten Zeeman
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

-module(controller_admin_site_health).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_analytics)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Range = z_context:get_q(<<"range">>, Context, <<"28d">>),
    Context1 = z_context:set(active_range, Range, Context),
    Vars = [
        {active_range, Range}
    ],
    Html = z_template:render("admin_site_health.tpl", Vars, Context1),
    z_context:output(Html, Context1).
