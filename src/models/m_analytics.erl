%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2024-2025 Maas-Maarten Zeeman
%% @doc API to view statistics of a site. 

%% Copyright 2024-2025 Maas-Maarten Zeeman
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

-module(m_analytics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(zotonic_model).

-export([
    m_get/3
]).

-export([
    overview/1,

    stats_overview/1, stats_overview/3,
    rsc_stats_overview/2, rsc_stats_overview/4,
    unique_visitors/1, unique_visitors/3,

    popular_pages/1,
    popular_resources/1,
    hourly_traffic/1,

    popular_rsc_pages/2,
    erroring_pages/1, erroring_pages/3,

    peer_ip_analytics/3,
    controller_health/3,

    page_views/1, sessions/1, 

    dispatch_rule_health/1, dispatch_rule_health/3,
    user_activity/1, user_activity/3,

    response_time_distribution/1, response_time_distribution/3,
    error_breakdown/1, error_breakdown/3,
    traffic_sources/1, traffic_sources/3,
    session_duration_distribution/1, session_duration_distribution/3,
    traffic_by_hour_of_day/1, traffic_by_hour_of_day/3,

    slow_pages/1,
    suspicious_ips/1,

    select/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% TODO add access control to the api.

m_get([<<"overview">> | Rest], _Msg, Context) ->
    {ok, {overview(Context), Rest}};

m_get([<<"stats_overview">> | Rest], _Msg, Context) ->
    {ok, {stats_overview(Context), Rest}};

m_get([<<"rsc_stats_overview">>, Rsc | Rest], _Msg, Context) ->
    {ok, {rsc_stats_overview(Rsc, Context), Rest}};

m_get([<<"unique_visitors">> | Rest], _Msg, Context) ->
    {ok, {unique_visitors(Context), Rest}};
m_get([<<"dispatch_rule_health">> | Rest], _Msg, Context) ->
    {ok, {dispatch_rule_health(Context), Rest}};

m_get([<<"popular_pages">>, Rsc | Rest], _Msg, Context) ->
    {ok, {popular_rsc_pages(Rsc, Context), Rest}};

m_get([<<"popular_pages">> | Rest], _Msg, Context) ->
    {ok, {popular_pages(Context), Rest}};

m_get([<<"popular_resources">> | Rest], _Msg, Context) ->
    {ok, {popular_resources(Context), Rest}};

m_get([<<"popular_referrers">>, Rsc | Rest], _Msg, Context) ->
    {ok, {popular_referrers(Rsc, Context), Rest}};

m_get([<<"slow_pages">> | Rest], _Msg, Context) ->
    {ok, {slow_pages(Context), Rest}};

m_get([<<"broken_links">> | Rest], _Msg, Context) ->
    {ok, {broken_links(Context), Rest}};

m_get([<<"suspicious_ips">> | Rest], _Msg, Context) ->
    {ok, {suspicious_ips(Context), Rest}};

m_get([<<"access_log">>, Rsc | Rest], _Msg, Context) ->
    {ok, {access_log(Rsc, Context), Rest}};

m_get([<<"user_activity">> | Rest], _Msg, Context) ->
    {ok, {user_activity(Context), Rest}};

m_get([<<"page_views">> | Rest], _Msg, Context) ->
    {ok, {page_views(Context), Rest}};
m_get([<<"sessions">> | Rest], _Msg, Context) ->
    {ok, {sessions(Context), Rest}};

m_get([<<"hourly_traffic">> | Rest], _Msg, Context) ->
    {ok, {hourly_traffic(Context), Rest}};
m_get([<<"response_time_distribution">> | Rest], _Msg, Context) ->
    {ok, {response_time_distribution(Context), Rest}};
m_get([<<"error_breakdown">> | Rest], _Msg, Context) ->
    {ok, {error_breakdown(Context), Rest}};
m_get([<<"traffic_sources">> | Rest], _Msg, Context) ->
    {ok, {traffic_sources(Context), Rest}};
m_get([<<"session_duration_distribution">> | Rest], _Msg, Context) ->
    {ok, {session_duration_distribution(Context), Rest}};
m_get([<<"traffic_by_hour_of_day">> | Rest], _Msg, Context) ->
    {ok, {traffic_by_hour_of_day(Context), Rest}};

m_get(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

%% Helper function to get date range based on context
get_date_range(Context) ->
    {Until, _} = z_datetime:next_day(z_datetime:to_datetime(<<"now">>), 1),
    Range = z_context:get(active_range, Context, <<"28d">>),
    Days = case Range of
        <<"7d">> -> 7;
        <<"28d">> -> 28;
        <<"91d">> -> 91;
        _ -> 28  % Default to 28 days
    end,
    {From, _} = z_datetime:prev_day(Until, Days),
    {{From, {0,0,0}}, {Until, {0, 0, 0}}}.

page_views(Context) ->
    {From, Until} = get_date_range(Context),
    page_views(From, Until, Context).

page_views(From, Until, Context) -> 
    Site = z_context:site(Context),

    Q = <<"
SELECT
    count(*)
FROM
    access_log
WHERE
    rsc_id IS NOT NULL
    AND site = $site
    AND timestamp >= $from
    AND timestamp <= $until
">>,

    case z_duckdb:q(Q, #{ from => From,
                           until => Until,
                           site => Site} ) of
        {ok, _, [{Count}]} ->
            Count;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get page view count">>, reason => Reason }),
            []
    end.

sessions(Context) ->
    {From, Until} = get_date_range(Context),
    sessions(From, Until, Context).

sessions(From, Until, Context) -> 
    Site = z_context:site(Context),

    Q = <<"
SELECT
    count(distinct session_id)
FROM
    access_log
WHERE
    session_id IS NOT NULL
    AND site = $site
    AND timestamp >= $from
    AND timestamp <= $until
">>,

    case z_duckdb:q(Q, #{ from => From,
                           until => Until,
                           site => Site} ) of
        {ok, _, [{Count}]} ->
            Count;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get page view count">>, reason => Reason }),
            []
    end.

overview(Context) ->
    SessionStatsCTE = <<"session_stats AS MATERIALIZED (
    SELECT
        window_session_id AS session_id,
        MIN(timestamp) AS session_start,
        MAX(timestamp) AS session_end,
        COUNT(*) AS pageviews,
        COUNT(DISTINCT rsc_id) AS rscs,
        MAX(visitor_id) AS visitor_id,
        MAX(user_id) FILTER (WHERE user_id IS NOT NULL) AS user_id
    FROM
        session_windows
    GROUP BY
        window_session_id
)">>,

    DailyStatsCTE = <<"daily_stats AS (
    SELECT
        date_trunc('day', session_start) AS day,
        COUNT(*) AS visits,
        COUNT(DISTINCT visitor_id) AS visitors,
        COUNT(DISTINCT user_id) FILTER (WHERE user_id IS NOT NULL) AS logged_users,
        SUM(pageviews)::INTEGER AS pageviews,
        COUNT(*) FILTER (WHERE pageviews = 1) AS bounces,
        AVG( epoch_ms(session_end - session_start) / 1000.0) FILTER (WHERE pageviews > 1) AS avg_duration_seconds
    FROM session_stats
    GROUP BY day
)">>,

SessionEventsCTE = <<"session_events AS (
    SELECT
        visitor_id,
        session_id,
        rsc_id,
        user_id,
        timestamp,
        CASE WHEN timestamp - LAG(timestamp) OVER (
                    PARTITION BY visitor_id
                    ORDER BY timestamp
                ) > INTERVAL '30 minutes'
             THEN 1 ELSE 0
        END AS is_new_window
    FROM pageviews
)">>,

SessionWindowsCTE = <<"session_windows AS (
    SELECT
        visitor_id,
        session_id,
        rsc_id,
        user_id,
        timestamp,
        hash(
            visitor_id,
            SUM(is_new_window) OVER (
                PARTITION BY visitor_id
                ORDER BY timestamp
                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            )
        ) AS window_session_id
    FROM session_events
)">>,

    Query = <<"SELECT
    d.day AS day,
    COALESCE(s.visitors, 0) AS visitors,
    COALESCE(s.visits, 0) AS visits,
    COALESCE(s.logged_users, 0) AS logged_users,
    COALESCE(s.pageviews, 0) AS pageviews,
    COALESCE(ROUND(s.pageviews::NUMERIC / NULLIF(s.visits, 0), 2), 0) AS views_per_visit,
    COALESCE(ROUND(s.bounces::NUMERIC / NULLIF(s.visits, 0), 4), 0) AS bounce_rate,
    COALESCE(ROUND(s.avg_duration_seconds, 2), 0)   AS avg_duration_seconds
FROM date_spine d
LEFT JOIN daily_stats s ON s.day = d.day
UNION ALL
SELECT
    NULL,
    COUNT(DISTINCT visitor_id),
    COUNT(*),
    COUNT(DISTINCT user_id) FILTER (WHERE user_id IS NOT NULL),
    SUM(pageviews)::INTEGER,
    ROUND(SUM(pageviews)::NUMERIC / NULLIF(COUNT(*), 0), 2),
    ROUND(COUNT(*) FILTER (WHERE pageviews = 1)::NUMERIC / NULLIF(COUNT(*), 0), 4),
    ROUND(AVG(epoch_ms(session_end - session_start) / 1000.0) FILTER (WHERE pageviews > 1), 2)
FROM session_stats
ORDER BY day NULLS FIRST">>,

    Base = get_base_filters(Context),

    [Totals | Data] = select(Query, Base ++ [
                                             cte(pageviews, pageviews(base)),
                                             date_spine(),
                                             SessionEventsCTE,
                                             SessionWindowsCTE,
                                             SessionStatsCTE,
                                             DailyStatsCTE
                                            ],
                            Context),
    #{ totals => Totals, data => Data}.


get_base_filters(Context) ->
    BaseFilters = [site_filter,
                   time_filter,
                   success_filtered,
                   exclude_controller_authentication,
                   exclude_controller_file,
                   exclude_controller_fileuploader],

    BaseFilters1 = case z_context:get(is_include_admin, Context) of
                       true -> BaseFilters;
                       false -> BaseFilters ++ [exclude_admin]
                   end,

    BaseFilters2 = case z_context:get(is_include_bots, Context) of
                       true -> BaseFilters1;
                       false -> BaseFilters1 ++ [exclude_bots]
                   end,

    build_filter(base, BaseFilters2, access_log, available_filters(), []).


get_base_raw_filters(_Context) ->
    BaseFilters = [site_filter, time_filter],
    build_filter(base_raw, BaseFilters, access_log, available_filters(), []).

build_filter(_Target, [], _Source, _Registry, Acc) ->
    lists:reverse(Acc);
build_filter(Target, [F], Source, Registry, Acc) ->
    #{ F := Fun } = Registry,
    build_filter(done, [], Target, Registry, [cte(Target, Fun(Source)) | Acc]);
build_filter(Target, [F | Rest], Source, Registry, Acc) ->
    #{ F := Fun } = Registry,
    build_filter(Target, Rest, F, Registry, [cte(F, Fun(Source)) | Acc]).

stats_overview(Context) ->
    {From, Until} = get_date_range(Context),
    stats_overview(From, Until, Context).

stats_overview(From, Until, Context) ->
    Site = z_context:site(Context),

    Q1 = <<"
WITH
    date_series AS (
        SELECT unnest(
            generate_series(
                date_trunc('day', $from::timestamp),
                date_trunc('day', $until::timestamp),
                INTERVAL 1 day
            )) AS day
    ),
    unique_sessions AS (
        SELECT
            date_trunc('day', timestamp) AS day,
            count(*) AS requests, 
            count(DISTINCT rsc_id) AS rscs, 
            count(DISTINCT user_id) AS users, 
            count(DISTINCT session_id) AS sessions,
            (sum(resp_bytes) / 1048576)::uinteger AS resp_mbs,
            count(CASE WHEN resp_code >= 400 AND resp_code < 500 THEN 1 END) as client_errors,
            count(CASE WHEN resp_code >= 500 THEN 1 END) as server_errors 
        FROM
            access_log
        WHERE
            site = $site
            AND timestamp >= $from
            AND timestamp <= $until
            AND", (no_bots_clause())/binary,
"        GROUP BY
            day
    )
SELECT
    ds.day,
    COALESCE(us.requests, 0) AS requests,
    COALESCE(us.rscs, 0) AS rscs,
    COALESCE(us.users, 0) AS users,
    COALESCE(us.sessions, 0) AS sessions,
    COALESCE(us.resp_mbs, 0) AS resp_mbs,
    COALESCE(us.client_errors, 0) AS client_errors,
    COALESCE(us.server_errors, 0) AS server_errors 
FROM
    date_series ds
LEFT JOIN
    unique_sessions us
ON
    ds.day = us.day
ORDER BY
    ds.day;
">>,

    case z_duckdb:q(Q1, #{ from => From,
                           until => Until,
                           site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get request overview">>, reason => Reason }),
            []
    end.

rsc_stats_overview(Rsc, Context) ->
    Id = m_rsc:rid(Rsc, Context),
    To = z_datetime:to_datetime(<<"now">>),
    From = z_datetime:prev_day(To, 30),
    rsc_stats_overview(Id, From, To, Context).

rsc_stats_overview(Id, From, Until, Context) ->
    Site = z_context:site(Context),

    Q1 = <<"
WITH
    date_series AS (
        SELECT unnest(
            generate_series(
                date_trunc('day', $from::timestamp),
                date_trunc('day', $until::timestamp),
                INTERVAL 1 day
            )) AS day
    ),
    rsc_stats AS (
        SELECT
            date_trunc('day', timestamp) AS day,
            count(*) AS requests, 
            count(DISTINCT session_id) AS sessions,
            count(DISTINCT user_id) AS users
        FROM
            access_log
        WHERE
            site = $site
            AND resp_code = 200
            AND req_method = 'GET'
            AND rsc_id = $id
            AND timestamp >= $from
            AND timestamp <= $until
            AND ", (no_bots_clause())/binary,
"        GROUP BY
            day
    )
SELECT
    ds.day,
    COALESCE(rs.requests, 0) AS requests,
    COALESCE(rs.sessions, 0) AS sessions,
    COALESCE(rs.users, 0) AS users
FROM
    date_series ds
LEFT JOIN
    rsc_stats rs
ON
    ds.day = rs.day
ORDER BY
    ds.day;
">>,

    case z_duckdb:q(Q1, #{ id => Id, 
                            from => From,
                            until => Until,
                            site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get rsc_stats overview">>,
                            id => Id,
                            reason => Reason }),
            []
    end.

unique_visitors(Context) ->
    {From, Until} = get_date_range(Context),
    unique_visitors(From, Until, Context).


unique_visitors(_From, _Until, Context) ->
    _Site = z_context:site(Context),
    
    Query = <<"
SELECT
    ds.day,
    COALESCE(uvi.ids, 0) AS ids 
FROM
     date_series ds
LEFT JOIN
    unique_visitor_ids uvi 
ON
    ds.day = uvi.day
ORDER BY
    ds.day;
">>,

    select(Query,
           [
            cte(date_series, date_series()), 
            cte(site_filtered, site_filter(access_log)),
            cte(time_filtered, time_filter(site_filtered)),
            cte(successes, success_filtered(time_filtered)),
            cte(visitor_ids, pageviews(successes)),
            <<"unique_visitor_ids AS (SELECT date_trunc('day', timestamp) AS day, count(DISTINCT visitor_id) AS ids FROM visitor_ids GROUP BY day)">>,
            <<"daily_counts AS ( SELECT ds.day, COALESCE(uvi.ids, 0) AS ids FROM date_series ds LEFT JOIN unique_visitor_ids uvi ON ds.day = uvi.day)">>,
            <<"total AS (SELECT count(DISTINCT visitor_id) AS total_visitors FROM visitor_ids)">>
           ],
           Context).


erroring_pages(Context) ->
    {From, Until} = get_date_range(Context),
    erroring_pages(From, Until, Context).

erroring_pages(From, Until, Context) ->
    Site = z_context:site(Context),

    Q = <<"
SELECT
    path,
    count(*),
    count(distinct session_id),
    count(distinct user_id)
FROM
    access_log
WHERE
    resp_code >= 500
    AND site = $site
    AND timestamp >= $from
    AND timestamp <= $until
GROUP BY
    path
ORDER BY
    COUNT(distinct session_id) DESC
LIMIT 10">>,

    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get erroring pages">>, reason => Reason }),
            []
    end.

popular_pages(Context) ->
    Q = <<"
SELECT
    path,
    count(*),
    count(distinct session_id),
    count(distinct user_id)
FROM
    base
GROUP BY
    path
ORDER BY
    COUNT(*) DESC,
    path
LIMIT 10">>,

    Base = get_base_filters(Context),
    select(Q, Base, Context).

popular_rsc_pages(Rsc, Context) ->
    To = z_datetime:to_datetime(<<"now">>),
    From = z_datetime:prev_day(To, 30),
    popular_rsc_pages(Rsc, From, To, Context).

popular_rsc_pages(Rsc, From, Until, Context) ->
    Id = m_rsc:rid(Rsc, Context),
    Site = z_context:site(Context),

    Q = <<"
SELECT
    CASE WHEN strlen(qs) > 0 THEN CONCAT(path, '?', qs) ELSE path END AS url,
    count(*),
    count(distinct session_id),
    count(distinct user_id)
FROM
    access_log
WHERE
    rsc_id = $id
    AND resp_code = 200
    AND req_method = 'GET'
    AND site = $site
    AND timestamp >= $from
    AND timestamp <= $until
    AND ", (no_bots_clause())/binary,
"GROUP BY
    url 
ORDER BY
    COUNT(*) DESC,
    url 
LIMIT 10">>,

    case z_duckdb:q(Q, #{ id => Id, from => From, until => Until, site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get popular rsc pages">>, reason => Reason }),
            []
    end.

popular_referrers(Rsc, Context) ->
    To = z_datetime:to_datetime(<<"now">>),
    From = z_datetime:prev_day(To, 30),
    popular_referrers(Rsc, From, To, Context).

popular_referrers(Rsc, From, Until, Context) ->
    Id = m_rsc:rid(Rsc, Context),
    Site = z_context:site(Context),

    Q = <<"
SELECT
    referer,
    count(*)
FROM
    access_log
WHERE
    rsc_id = $id
    AND referer IS NOT NULL
    AND resp_code = 200
    AND site = $site
    AND timestamp >= $from
    AND timestamp <= $until
GROUP BY
    referer 
ORDER BY
    COUNT(*) DESC,
    referer 
LIMIT 10">>,

    case z_duckdb:q(Q, #{ id => Id, from => From, until => Until, site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get popular referrer">>,
                            id => Id,
                            reason => Reason }),
            []
    end.

slow_pages(Context) ->
    Base = get_base_raw_filters(Context),

    Q = <<"
    SELECT
        path,
        controller,
        dispatch_rule,
        COUNT(*)                                          AS hits,
        ROUND(quantile_cont(duration_total, 0.50)/1000)   AS p50_ms,
        ROUND(quantile_cont(duration_total, 0.95)/1000)   AS p95_ms,
        ROUND(quantile_cont(duration_total, 0.99)/1000)   AS p99_ms,
        MAX(duration_total/1000)                          AS max_ms,
        ROUND(quantile_cont(duration_process, 0.95)/1000) AS p95_process_ms,
        ROUND((quantile_cont(duration_total, 0.95) - quantile_cont(duration_process, 0.95)) / 1000) AS p95_io_gap_ms,
        -- What fraction of hits are slow
        ROUND(100.0 * COUNT(*) FILTER ( WHERE duration_total > ($slow_threshold_ms * 1000)) / NULLIF(COUNT(*), 0), 1) AS pct_slow_hits
    FROM base_raw
    WHERE duration_total IS NOT NULL
    GROUP BY path, controller, dispatch_rule
    HAVING COUNT(*) > 10 AND quantile_cont(duration_total, 0.95) > ($slow_threshold_ms * 1000)
    ORDER BY p95_ms DESC
    LIMIT 50;">>,

    Site = z_context:site(Context),
    {From, Until} = get_date_range(Context),

    select_args(Q, Base, #{ from => From, until => Until, site => Site, slow_threshold_ms => 1000 }).


broken_links(Context) ->
    Base = get_base_raw_filters(Context),

    Q = <<"
SELECT
    path,
    regexp_extract(referer, '^https?://([^/]+)', 1)      AS referer_domain,
    regexp_extract(referer, '^https?://([^/]+)', 1)
        = $hostname AND COUNT(DISTINCT session_id) FILTER (
            WHERE session_id IS NOT NULL
        ) > 0  AS is_internal,
    COUNT(*)                                             AS hits,
    COUNT(DISTINCT hash(peer_ip, user_agent))            AS unique_visitors,
    MIN(timestamp)                                       AS first_seen,
    MAX(timestamp)                                       AS last_seen
FROM base_raw
WHERE resp_code = 404
GROUP BY path, referer_domain
HAVING COUNT(*) > 2
ORDER BY is_internal DESC, hits DESC
LIMIT 100;">>,

    Site = z_context:site(Context),
    {From, Until} = get_date_range(Context),

    select_args(Q, Base, #{ from => From, until => Until, site => Site, hostname => z_context:hostname(Context) }).


suspicious_ips(Context) ->
    Base = get_base_raw_filters(Context),

    Q = <<"SELECT
        peer_ip,
        COUNT(*)                                             AS requests,
        COUNT(DISTINCT path)                                 AS unique_paths,
        COUNT(DISTINCT session_id)                           AS sessions,
        COUNT(*) FILTER (WHERE resp_code = 404)              AS not_founds,
        COUNT(*) FILTER (WHERE resp_code IN (401, 403))      AS auth_errors,
        COUNT(*) FILTER (WHERE resp_code >= 500)             AS server_errors,
        -- Most common user agent for this IP
        max(user_agent)                                      AS user_agent_sample,
        MIN(timestamp)                                       AS first_seen,
        MAX(timestamp)                                       AS last_seen,
        -- Active hours — how many distinct hours did this IP appear
        COUNT(DISTINCT date_trunc('hour', timestamp))        AS active_hours,
        -- Requests in last hour of the window
        COUNT(*) FILTER (
            WHERE timestamp >= $until - INTERVAL '1 hour'
        )                                                    AS req_last_hour,
        -- Severity score
        (COUNT(*) / 100)
        + CASE WHEN COUNT(DISTINCT path)::FLOAT / NULLIF(COUNT(*), 0) > 0.5
               THEN 20 ELSE 0 END
        + (COUNT(*) FILTER (WHERE resp_code = 404) / 10)
        + (COUNT(*) FILTER (WHERE resp_code IN (401, 403)) * 3)
        + CASE WHEN COUNT(*) FILTER (
                    WHERE timestamp >= $until - INTERVAL '1 hour'
                  ) > 50
                AND COUNT(*) FILTER (
                    WHERE timestamp <  $until - INTERVAL '1 hour'
                  ) = 0
               THEN 15 ELSE 0 END                            AS severity_score,
        list_filter([
            CASE WHEN COUNT(*) > 500
                 THEN 'high volume' END,
            CASE WHEN COUNT(DISTINCT path)::FLOAT / NULLIF(COUNT(*), 0) > 0.5
                 THEN 'path scan' END,
            CASE WHEN COUNT(*) FILTER (WHERE resp_code = 404) > 50
                 THEN '404 scan' END,
            CASE WHEN COUNT(*) FILTER ( WHERE resp_code IN (401, 403)) > 5
                 THEN 'auth probe' END
        ], x -> x IS NOT NULL)                               AS reasons
    FROM base_raw
    GROUP BY peer_ip
    HAVING COUNT(*) > 20
        OR COUNT(*) FILTER (WHERE resp_code IN (401, 403)) > 3
    ORDER BY severity_score DESC
    LIMIT 50;">>,

    Q1 = <<"    SELECT
        peer_ip,
        COUNT(*)                                             AS requests,
        COUNT(DISTINCT hash(peer_ip, user_agent))            AS visitor_ids,
        ARRAY_AGG(DISTINCT user_agent)                       AS user_agents,
        -- Requests per visitor_id — low = scanner, high = NAT
        ROUND(COUNT(*)::FLOAT
            / NULLIF(COUNT(DISTINCT
                hash(peer_ip, user_agent)), 0), 1)           AS req_per_visitor,
        approx_count_distinct(path)                          AS unique_paths,
        COUNT(DISTINCT session_id)                           AS sessions,
        COUNT(*) FILTER (WHERE resp_code = 404)              AS not_founds,
        COUNT(*) FILTER (WHERE resp_code IN (401, 403))      AS auth_errors,
        COUNT(*) FILTER (WHERE resp_code >= 500)             AS server_errors,
        entropy(path)                                        AS path_entropy,
        MIN(timestamp)                                       AS first_seen,
        MAX(timestamp)                                       AS last_seen,
        COUNT(DISTINCT date_trunc('hour', timestamp))        AS active_hours,
        COUNT(*) FILTER (
            WHERE timestamp >= $until - INTERVAL '1 hour'
        )                                                    AS req_last_hour,
        -- Revised severity: penalise low visitor_id count relative to requests
        (COUNT(*) / 100)
        + CASE WHEN entropy(path) > 3.0 THEN 25 ELSE 0 END
        + CASE WHEN entropy(path) > 4.0 THEN 15 ELSE 0 END
        + (COUNT(*) FILTER (WHERE resp_code = 404) / 10)
        + (COUNT(*) FILTER (WHERE resp_code IN (401, 403)) * 3)
        -- Single visitor_id with high volume = almost certainly automated
        + CASE WHEN COUNT(DISTINCT hash(peer_ip, user_agent)) = 1
                AND COUNT(*) > 500
               THEN 20 ELSE 0 END                            AS severity_score,
        list_filter([
            CASE WHEN COUNT(*) > 500
                 THEN 'high volume' END,
            CASE WHEN COUNT(DISTINCT hash(peer_ip, user_agent)) = 1
                  AND COUNT(*) > 500
                 THEN 'single agent' END,
            CASE WHEN entropy(path) > 3.0
                 THEN 'path scan (entropy='
                      || ROUND(entropy(path), 1)::VARCHAR
                      || ')' END,
            CASE WHEN COUNT(*) FILTER (WHERE resp_code = 404) > 50
                 THEN '404 scan' END,
            CASE WHEN COUNT(*) FILTER (
                      WHERE resp_code IN (401, 403)) > 5
                 THEN 'auth probe' END
        ], x -> x IS NOT NULL)                               AS reasons
    FROM base_raw
    GROUP BY peer_ip
    HAVING COUNT(*) > 20
        OR COUNT(*) FILTER (WHERE resp_code IN (401, 403)) > 3
    ORDER BY severity_score DESC
    LIMIT 50;">>,

    Site = z_context:site(Context),
    {From, Until} = get_date_range(Context),

    select_args(Q1, Base, #{ from => From, until => Until, site => Site }).


access_log(Rsc, Context) ->
    To = z_datetime:to_datetime(<<"now">>),
    From = z_datetime:prev_day(To, 30),
    access_log(Rsc, From, To, Context).

access_log(Rsc, From, Until, Context) ->
    Id = m_rsc:rid(Rsc, Context),
    Site = z_context:site(Context),

    Q = <<"
WITH log_with_prev_date AS (
    SELECT
        req_version,
        req_method,
        resp_code,
        path,
        qs,
        referer,
        duration_total,
        peer_ip,
        session_id,
        user_id,
        language,
        timezone,
        user_agent,
        timestamp,
        LAG(CAST(timestamp AS DATE)) OVER (ORDER BY timestamp DESC) AS prev_date
    FROM
        access_log
    WHERE
        rsc_id = $id
        AND site = $site
        AND resp_code = 200
        AND req_method = 'GET'
        AND timestamp >= $from
        AND timestamp <= $until
        AND ", (no_bots_clause())/binary,
"
)
SELECT
    req_version,
    req_method,
    resp_code,
    path,
    qs,
    referer,
    duration_total,
    peer_ip,
    session_id,
    user_id,
    language,
    timezone,
    user_agent,
    timestamp,
    CASE 
        WHEN prev_date IS NULL OR CAST(timestamp AS DATE) != prev_date
        THEN true
        ELSE false
    END AS date_changed
FROM
    log_with_prev_date
ORDER BY
    timestamp DESC
LIMIT 100">>,

    case z_duckdb:q(Q, #{ id => Id, from => From, until => Until, site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get access_log">>,
                            id => Id,
                            reason => Reason }),
            []
    end.


popular_resources(Context) ->
    Q = <<"SELECT
    rsc_id,
    count(*),
    count(distinct session_id),
    count(distinct user_id)
FROM
    access_log
WHERE
    rsc_id IS NOT NULL
GROUP BY
    rsc_id
ORDER BY
    COUNT(*) DESC,
    rsc_id
LIMIT 10">>,

    Base = get_base_filters(Context),
    select(Q, Base, Context).

peer_ip_analytics(From, Until, Context) ->
    Site = z_context:site(Context),

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
WHERE
    site = $site
    AND timestamp > $from
    AND timestamp < $until
GROUP BY 
    peer_ip
ORDER BY 
    request_count DESC
LIMIT 100">>,

    case z_duckdb:q(Q, #{ from => From,
                           until => Until,
                           site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

controller_health(From, Until, Context) ->
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
WHERE
    site = $site
    AND timestamp > $from
    AND timestamp < $until
GROUP BY 
    controller
ORDER BY 
    total_requests DESC;">>,

    Site = z_context:site(Context),
    case z_duckdb:q(Q, #{ from => From,
                           until => Until,
                           site => Site} ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

dispatch_rule_health(Context) ->
    {From, Until} = get_date_range(Context),
    dispatch_rule_health(From, Until, Context).

dispatch_rule_health(From, Until, Context) ->
    Site = z_context:site(Context),
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
WHERE
    timestamp <= $until
    AND timestamp >= $from
    AND site = $site
GROUP BY 
    dispatch_rule 
ORDER BY 
    total_requests DESC;">>,

    case z_duckdb:q(Q, #{ site => Site,
                           from => From,
                           until => Until } ) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get peer_ip analytics">>, reason => Reason }),
            []
    end.

user_activity(Context) ->
    {From, Until} = get_date_range(Context),
    user_activity(From, Until, Context).

user_activity(From, Until, Context) ->
    Q = <<"
SELECT 
    user_id,
    COUNT(DISTINCT session_id) AS session_count,
    COUNT(*) AS total_requests,
    COUNT(CASE WHEN req_method = 'POST' THEN 1 END) AS post_actions,
    AVG(duration_total) AS avg_response_time_ms,
    GEOMEAN(duration_total) AS mean_response_time_ms,
    MIN(timestamp) AS first_activity,
    MAX(timestamp) AS last_activity,
    ARRAY_AGG(DISTINCT peer_ip) AS ips,
    ARRAY_AGG(DISTINCT rsc_id) FILTER (WHERE rsc_id IS NOT NULL) AS rsc
FROM 
    access_log
WHERE 
    user_id IS NOT NULL
    AND timestamp <= $until
    AND timestamp >= $from
    AND site = $site
GROUP BY 
    user_id
ORDER BY 
    total_requests DESC
LIMIT 20;">>,

    Site = z_context:site(Context),
    case z_duckdb:q(Q, #{from => From,
                          until => Until,
                          site => Site
                         }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get user activity analytics">>, reason => Reason }),
            []
    end.

no_bots_clause() ->
    <<"((session_id IS NOT NULL)
        OR ( (session_id IS NULL)
             AND user_agent IS NOT NULL
             AND NOT regexp_matches(user_agent, '(bot|crawler|spider|Googlebot|Bingbot|Yahoo! Slurp|Baiduspider|YandexBot|AhrefsBot|MJ12bot|SemrushBot|DotBot|Sogou|Exabot|facebookexternalhit|Twitterbot|Slackbot|GuzzleHttp|HeadlessChrome|python-requests|go-http-client|curl/|wget/)', 'i')
        ))
    ">>.

%% New Analytics Functions

%% @doc Get hourly traffic for the last 24 hours
hourly_traffic(Context) ->
    HoursSpine = <<"
    WITH all_hours AS (
        SELECT unnest(generate_series(0, 23)) as hour_of_day
    )">>,

    HourlyData = <<"hourly_data AS (
        SELECT 
            hour(timestamp) as hour_of_day,
            count(*) as requests
        FROM 
            base
        GROUP BY 
            hour_of_day
    )">>,

    Q = <<"SELECT 
        all_hours.hour_of_day,
        COALESCE(hourly_data.requests, 0) as requests
    FROM 
        all_hours
    LEFT JOIN 
        hourly_data ON all_hours.hour_of_day = hourly_data.hour_of_day
    ORDER BY 
        all_hours.hour_of_day
    ">>,

    Base = get_base_filters(Context),
    select(Q, Base ++ [ HoursSpine, HourlyData ], Context).
    
%% @doc Get response time distribution in buckets
response_time_distribution(Context) ->
    {From, Until} = get_date_range(Context),
    response_time_distribution(From, Until, Context).

response_time_distribution(From, Until, Context) ->
    Site = z_context:site(Context),
    Q = <<"
    SELECT 
        CASE 
            WHEN duration_total / 1000 < 100 THEN '<100ms'
            WHEN duration_total / 1000>= 100 AND duration_total / 1000 < 500 THEN '100-500ms'
            WHEN duration_total /1000  >= 500 AND duration_total / 1000 < 1000 THEN '500ms-1s'
            WHEN duration_total / 1000 >= 1000 AND duration_total / 1000 < 3000 THEN '1s-3s'
            ELSE '>3s'
        END as bucket,
        count(*) as count
    FROM 
        access_log
    WHERE 
        site = $site
        AND timestamp >= $from
        AND timestamp <= $until
        AND duration_total IS NOT NULL
    GROUP BY 
        bucket
    ORDER BY 
        CASE bucket
            WHEN '<100ms' THEN 1
            WHEN '100-500ms' THEN 2
            WHEN '500ms-1s' THEN 3
            WHEN '1s-3s' THEN 4
            ELSE 5
        END
    ">>,
    
    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get response time distribution">>, reason => Reason }),
            []
    end.

%% @doc Get error breakdown by type (4xx vs 5xx)
error_breakdown(Context) ->
    {From, Until} = get_date_range(Context),
    error_breakdown(From, Until, Context).

error_breakdown(From, Until, Context) ->
    Site = z_context:site(Context),
    Q = <<"
    SELECT 
        CASE 
            WHEN resp_code >= 400 AND resp_code < 500 THEN '4xx Client Errors'
            WHEN resp_code >= 500 THEN '5xx Server Errors'
            ELSE 'Other'
        END as error_type,
        count(*) as count
    FROM 
        access_log
    WHERE 
        site = $site
        AND timestamp >= $from
        AND timestamp <= $until
        AND resp_code >= 400
    GROUP BY 
        error_type
    ORDER BY 
        count DESC
    ">>,
    
    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get error breakdown">>, reason => Reason }),
            []
    end.

%% @doc Get top traffic sources/referrers
traffic_sources(Context) ->
    {From, Until} = get_date_range(Context),
    traffic_sources(From, Until, Context).

traffic_sources(From, Until, Context) ->
    Site = z_context:site(Context),
    Q = <<"
    SELECT 
        COALESCE(referer, 'Direct') as source,
        count(*) as requests
    FROM 
        access_log
    WHERE 
        site = $site
        AND timestamp >= $from
        AND timestamp <= $until
        AND ", (no_bots_clause())/binary, "
    GROUP BY 
        source
    ORDER BY 
        requests DESC
    LIMIT 10
    ">>,
    
    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get traffic sources">>, reason => Reason }),
            []
    end.

%% @doc Get session duration distribution
session_duration_distribution(Context) ->
    {From, Until} = get_date_range(Context),
    session_duration_distribution(From, Until, Context).

session_duration_distribution(From, Until, Context) ->
    Site = z_context:site(Context),
    Q = <<"
    WITH session_durations AS (
        SELECT 
            session_id,
            (MAX(timestamp) - MIN(timestamp)) as duration_seconds
        FROM 
            access_log
        WHERE 
            site = $site
            AND timestamp >= $from
            AND timestamp <= $until
            AND session_id IS NOT NULL
        GROUP BY 
            session_id
    )
    SELECT 
        CASE 
            WHEN duration_seconds < 10 THEN '<10s'
            WHEN duration_seconds >= 10 AND duration_seconds < 60 THEN '10s-1m'
            WHEN duration_seconds >= 60 AND duration_seconds < 300 THEN '1m-5m'
            WHEN duration_seconds >= 300 AND duration_seconds < 1800 THEN '5m-30m'
            ELSE '>30m'
        END as bucket,
        count(*) as count
    FROM 
        session_durations
    GROUP BY 
        bucket
    ORDER BY 
        CASE bucket
            WHEN '<10s' THEN 1
            WHEN '10s-1m' THEN 2
            WHEN '1m-5m' THEN 3
            WHEN '5m-30m' THEN 4
            ELSE 5
        END
    ">>,
    
    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get session duration distribution">>, reason => Reason }),
            []
    end.

%% @doc Get traffic pattern by hour of day (aggregate)
traffic_by_hour_of_day(Context) ->
    {From, Until} = get_date_range(Context),
    traffic_by_hour_of_day(From, Until, Context).

traffic_by_hour_of_day(From, Until, Context) ->
    Site = z_context:site(Context),
    Q = <<"
    WITH all_hours AS (
        SELECT unnest(generate_series(0, 23)) as hour_of_day
    ),
    hourly_data AS (
        SELECT 
            hour(timestamp) as hour_of_day,
            count(*) as requests,
            count(DISTINCT session_id) as sessions
        FROM 
            access_log
        WHERE 
            site = $site
            AND timestamp >= $from
            AND timestamp <= $until
            AND ", (no_bots_clause())/binary, "
        GROUP BY 
            hour_of_day
    )
    SELECT 
        all_hours.hour_of_day,
        COALESCE(hourly_data.requests, 0) as requests,
        COALESCE(hourly_data.sessions, 0) as sessions
    FROM 
        all_hours
    LEFT JOIN hourly_data ON all_hours.hour_of_day = hourly_data.hour_of_day
    ORDER BY 
        all_hours.hour_of_day
    ">>,
    
    case z_duckdb:q(Q, #{ from => From, until => Until, site => Site }) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Could not get traffic by hour of day">>, reason => Reason }),
            []
    end.


select(Select, CTEs, Context) ->
    Site = z_context:site(Context),
    {From, Until} = get_date_range(Context),
    IsIncludeBots = z_context:get(is_include_bots, Context),
    IsIncludeAdmin = z_context:get(is_include_admin, Context),

    select_args(Select, CTEs, #{ from => From,
                                 until => Until,
                                 site => Site,
                                 include_admin => IsIncludeAdmin,
                                 include_bots => IsIncludeBots }).

select_args(Select, CTEs, Args) ->
    Query = [<<"WITH ">>, lists:join($,, CTEs), " ", Select],
    case z_duckdb:q(Query, Args) of
        {ok, _, Data} ->
            Data;
        {error, Reason} ->
            ?LOG_WARNING(#{ text => <<"Error">>, reason => Reason }),
            []
    end.

%%
%% Helpers
%%

cte(Name, Query) ->
    <<(z_convert:to_binary(Name))/binary, " AS (", Query/binary, ")">>.

% filter all page like requests, excluding the admin.
site_filter(Source) ->
    star_filter(Source, <<"WHERE site = $site">>).

time_filter(Source) ->
    star_filter(Source, <<"WHERE timestamp >= $from AND timestamp <= $until">>).

success_filtered(Source) ->
    star_filter(Source, <<"WHERE resp_category = 2">>).

exclude_controller_authentication(Source) ->
    star_filter(Source, <<"WHERE controller != 'controller_authentication'">>).

exclude_controller_file(Source) ->
    star_filter(Source, <<"WHERE controller != 'controller_file'">>).

exclude_controller_fileuploader(Source) ->
    star_filter(Source, <<"WHERE controller != 'controller_fileuploader'">>).

exclude_admin(Source) ->
    star_filter(Source, <<"WHERE NOT starts_with(path, '/admin')">>).

exclude_bots(Source) ->
    star_filter(Source, <<"WHERE regexp_matches(user_agent,
    'bot|crawl|spider|slurp|bingpreview|facebook|twitter|linkedinbot|whatsapp|telegram|curl|wget|python|java|go-http|okhttp|axios|postman|libwww|zgrab|nuclei|nmap|masscan|scanbot|dataforseo|semrush|ahrefs|mj12', 'i') = false">>).

pageviews(Source) ->
    <<"SELECT
    hash(peer_ip, user_agent) as visitor_id,
    session_id as session_id,
    rsc_id as rsc_id,
    user_id as user_id,
    timestamp as timestamp
      FROM ", (z_convert:to_binary(Source))/binary>>.

date_spine() ->
    <<"date_spine AS (
    SELECT generate_series::DATE AS day
    FROM generate_series($from::TIMESTAMP, $until::TIMESTAMP, INTERVAL '1 day')
    )">>.

star_filter(Source, WhereClause) ->
    <<"SELECT * FROM ", (z_convert:to_binary(Source))/binary, " ", WhereClause/binary>>.

date_series() ->
    <<"SELECT * FROM generate_series(date_trunc('day', $from), date_trunc('day', $until) - INTERVAL 1 DAY, INTERVAL 1 DAY) AS t(day)">>.

available_filters() ->
    #{
      site_filter => fun site_filter/1,
      time_filter => fun time_filter/1,
      success_filtered => fun success_filtered/1,
      exclude_controller_authentication =>  fun exclude_controller_authentication/1,
      exclude_controller_file => fun exclude_controller_file/1,
      exclude_controller_fileuploader => fun exclude_controller_fileuploader/1,
      exclude_admin => fun exclude_admin/1,
      exclude_bots => fun exclude_bots/1
     }.
