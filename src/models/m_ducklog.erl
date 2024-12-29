%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2024 Maas-Maarten Zeeman
%% @doc API to view statistics of a site. 

%% Copyright 2024 Maas-Maarten Zeeman
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

-module(m_ducklog).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(zotonic_model).

-export([
    m_get/3
]).

-export([
    unique_visitors/3,
    popular_pages/3,
    popular_resources/3,

    peer_ip_analytics/3,
    controller_health/3,
    dispatch_rule_health/3,
    user_activity/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([<<"unique_visitors">> | Rest], _Msg, Context) ->
    {ok, {unique_visitors(1,2, Context), Rest}};
m_get([<<"dispatch_rule_health">> | Rest], _Msg, Context) ->
    {ok, {dispatch_rule_health(1,2, Context), Rest}};
m_get([<<"popular_pages">> | Rest], _Msg, Context) ->
    {ok, {popular_pages(1,2, Context), Rest}};
m_get([<<"user_activity">> | Rest], _Msg, Context) ->
    {ok, {user_activity(1,2, Context), Rest}};


m_get(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

unique_visitors(From, To, Context) ->
    _Site = z_context:site(Context),

    _Period = <<"select unnest(range(timestamp '2024-12-13', timestamp '2024-12-14', interval 1 hour)) as period">>,

    Q = <<"WITH date_series AS (
               SELECT unnest(range(TIMESTAMP '2024-12-1', TIMESTAMP '2024-12-30', INTERVAL 1 day)) AS period
          )
          SELECT date_trunc('day', ds.period) as day, count(DISTINCT session_id) as unique FROM date_series ds LEFT JOIN access_log
          ON datetrunc('day', timestamp) = datetrunc('day', ds.period)
          WHERE site = 'site_cafe' GROUP BY ds.period ORDER BY ds.period;">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get unique visitors">>, reason => Reason }),
            []
    end.

popular_pages(_From, _To, Context) ->
    Site = z_context:site(Context),

    Q = <<"SELECT
    path, count(*), count(distinct session_id), count(distinct user_id)
FROM
    access_log
WHERE path NOT in ('/zotonic-auth', '/mqtt-transport', '/manifest.json', '/cotonic-service-worker.js')
           AND NOT (path ^@ '/lib/' OR path ^@ '/lib-min' OR path ^@ '/image/')
GROUP BY
    path
ORDER BY
    COUNT(distinct session_id) DESC
LIMIT 10">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get popular pages">>, reason => Reason }),
            []
    end.

popular_resources(_From, _To, Context) ->
    Site = z_context:site(Context),

    Q = <<"SELECT
    rsc_id, count(*)
FROM
    access_log
WHERE
    rsc_id IS NOT NULL
GROUP BY
    rsc_id
ORDER BY
    COUNT(*) DESC
LIMIT 10">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get popular resources">>, reason => Reason }),
            []
    end.

peer_ip_analytics(_From, _To, Context) ->
    _Site = z_context:site(Context),

    Q = <<"SELECT 
    peer_ip,
    COUNT(*) AS request_count,
    COUNT(DISTINCT session_id) AS unique_sessions,
    COUNT(DISTINCT user_id) AS unique_users,
    MIN(timestamp) AS first_access,
    MAX(timestamp) AS last_access,
    AVG(duration_total) AS avg_response_time,
    COUNT(CASE WHEN resp_code >= 400 THEN 1 END) AS error_count,
    COUNT(CASE WHEN resp_code >= 400 THEN 1 END) * 100.0 / COUNT(*) AS error_rate_percentage
FROM 
    access_log
GROUP BY 
    peer_ip
ORDER BY 
    request_count DESC
LIMIT 100">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

controller_health(_From, _To, _Context) ->
    Q = <<"
    SELECT 
    controller,
    COUNT(*) AS total_requests,
    COUNT(CASE WHEN resp_code < 400 THEN 1 END) AS successful_requests,
    COUNT(CASE WHEN resp_code >= 400 THEN 1 END) AS error_requests,
    (COUNT(CASE WHEN resp_code >= 400 THEN 1 END) * 100.0 / COUNT(*)) AS error_rate_percentage,
    AVG(duration_total) AS avg_response_time_ms,
    GEOMEAN(duration_total) AS mean_response_time_ms
FROM 
    access_log
GROUP BY 
    controller
ORDER BY 
    total_requests DESC;">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

dispatch_rule_health(_From, _To, _Context) ->
    Q = <<"
    SELECT 
    dispatch_rule,
    COUNT(*) AS total_requests,
    COUNT(CASE WHEN resp_code < 400 THEN 1 END) AS successful_requests,
    COUNT(CASE WHEN resp_code >= 400 THEN 1 END) AS error_requests,
    (COUNT(CASE WHEN resp_code >= 400 THEN 1 END) * 100.0 / COUNT(*)) AS error_rate_percentage,
    AVG(duration_total) AS avg_response_time_ms,
    GEOMEAN(duration_total) AS mean_response_time_ms
FROM 
    access_log
GROUP BY 
    dispatch_rule 
ORDER BY 
    total_requests DESC;">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

user_activity(_From, _To, Context) ->
    Q = <<"SELECT 
    user_id,
    COUNT(DISTINCT session_id) AS session_count,
    COUNT(*) AS total_requests,
    COUNT(CASE WHEN req_method = 'POST' THEN 1 END) AS post_actions,
    AVG(duration_total) AS avg_response_time_ms,
    GEOMEAN(duration_total) AS mean_response_time_ms,
    MIN(timestamp) AS first_activity,
    MAX(timestamp) AS last_activity,
    ARRAY_AGG(DISTINCT peer_ip) AS ips,
    ARRAY_AGG(DISTINCT rsc_id) FILTER (WHERE rsc_id IS NOT NULL) AS rsc,
FROM 
    access_log
WHERE 
    user_id IS NOT NULL
GROUP BY 
    user_id
ORDER BY 
    total_requests DESC
LIMIT 20; -- Adjust limit as needed">>,

    case z_ducklog:q(Q) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get user activity analytics">>, reason => Reason }),
            []
    end.

