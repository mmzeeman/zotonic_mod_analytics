%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc View zotonic site analytics 

%% Copyright 2024-2026 Maas-Maarten Zeeman
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

-module(controller_admin_analytics).
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
    % Get the date range parameter (default to 28d)
    Range = z_context:get_q(<<"range">>, Context, <<"28d">>),

    % View 
    View = z_context:get_q(<<"view">>, Context, <<"unique">>),

    %%
    IsIncludeAdmin = z_convert:to_bool(z_context:get_q(<<"include_admin">>, Context, false)),
    IsIncludeBots = z_convert:to_bool(z_context:get_q(<<"include_bots">>, Context, false)),

    %% Row click filters
    FilterPath = non_empty_binary(z_context:get_q(<<"filter_path">>, Context)),
    FilterRsc = to_integer_or_undefined(z_context:get_q(<<"filter_rsc">>, Context)),
    FilterUser = to_integer_or_undefined(z_context:get_q(<<"filter_user">>, Context)),

    Context1 = z_context:set(active_range, Range, Context),
    Context2 = z_context:set(active_view, View, Context1),
    Context3 = z_context:set(is_include_bots, IsIncludeBots, Context2),
    Context4 = z_context:set(is_include_admin, IsIncludeAdmin, Context3),
    Context5 = z_context:set(filter_path, FilterPath, Context4),
    Context6 = z_context:set(filter_rsc, FilterRsc, Context5),
    Context7 = z_context:set(filter_user, FilterUser, Context6),

    Vars = [
        {page_admin_analytics, true},
        {active_range, Range},
        {active_view, View},
        {is_include_bots, IsIncludeBots},
        {is_include_admin, IsIncludeAdmin},
        {filter_path, FilterPath},
        {filter_rsc, FilterRsc},
        {filter_user, FilterUser}
    ],
    Html = z_template:render("admin_analytics.tpl", Vars, Context7),
    z_context:output(Html, Context7).

non_empty_binary(undefined) -> undefined;
non_empty_binary(<<>>) -> undefined;
non_empty_binary(V) when is_binary(V) -> V;
non_empty_binary(_) -> undefined.

to_integer_or_undefined(undefined) -> undefined;
to_integer_or_undefined(<<>>) -> undefined;
to_integer_or_undefined(V) ->
    try z_convert:to_integer(V)
    catch _:_ -> undefined
    end.

