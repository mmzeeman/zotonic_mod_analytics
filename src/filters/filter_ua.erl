%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman 
%% @doc Show morereadable info from a user agent header.

%% Copyright 2025 Maas-Maarten Zeeman 
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

-module(filter_ua).

-export([ua/2]).

ua(UserAgent, _Context) ->
    Browser = find_browser(UserAgent),
    OS = find_os(UserAgent),

    #{ browser => Browser,
       os => OS }.

%% Find browser name + version
find_browser(UA) ->
    Patterns = [
        {"Chrome", "Chrome/([\\d\\.]+)"},
        {"Firefox", "Firefox/([\\d\\.]+)"},
        {"Safari", "Version/([\\d\\.]+).*Safari"},
        {"Edge", "Edg/([\\d\\.]+)"},
        {"Opera", "OPR/([\\d\\.]+)"},
        {"IE", "MSIE ([\\d\\.]+)"},
        {"IE", "Trident/.*rv:([\\d\\.]+)"}
    ],
    find_match(UA, Patterns, "Unknown Browser").

%% Find OS name + version (basic extraction)
find_os(UA) ->
    case re:run(UA, "\\(([^)]+)\\)", [{capture, [1], list}, unicode]) of
        {match, [OsInfo]} ->
            OS = extract_os(OsInfo),
            string:trim(OS);
        nomatch -> "Unknown OS"
    end.

extract_os(OsInfo) ->
    Patterns = [
        {"Windows", "Windows NT ([\\d\\.]+)"},
        {"Mac OS X", "Mac OS X ([\\d_\\.]+)"},
        {"Android", "Android ([\\d\\.]+)"},
        {"iOS", "iPhone OS ([\\d_]+)"}
    ],
    find_match(OsInfo, Patterns, OsInfo).

%% Utility to apply multiple regex patterns and format result
find_match(_UA, [], Default) ->
    Default;
find_match(UA, [{Label, Pattern} | Rest], Default) ->
    case re:run(UA, Pattern, [{capture, [1], list}, unicode]) of
        {match, [Version]} ->
            FormattedVersion = string:replace(Version, "_", ".", all),
            Label ++ " " ++ FormattedVersion;
        nomatch ->
            find_match(UA, Rest, Default)
    end.

