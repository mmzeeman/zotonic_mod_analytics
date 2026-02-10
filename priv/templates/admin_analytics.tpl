{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

<div class="container-fluid">

    {# Stats Overview Section - Responsive Panel Grid #}
    {% with m.analytics.stats_overview as stats_overview %}
    <div class="row" style="margin-bottom: 20px;">
        {# A bit weird.. duckdb retrieves data in columns, it is transposed, and now it transposed back again #}
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Requests" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Resources Visited" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:5) title=_"Sessions" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:6) title=_"Data Out" units=_"Mb" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:7) title=_"Client Errors" %}
                </div>
            </div>
        </div>
        <div class="col-md-3 col-sm-6">
            <div class="panel panel-default">
                <div class="panel-body text-center">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:8) title=_"Server Errors" %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# User Activity Section #}
    {% with m.analytics.user_activity  as user_activity %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ User Activity _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ User _}</th>
                                    <th>{_ # Sessions _}</th>
                                    <th>{_ # Requests _}</th>
                                    <th>{_ # Posts _}</th>
                                    <th>{_ Avg _}</th>
                                    <th>{_ Mean _}</th>
                                    <th>{_ Activity _}</th>
                                    <th>{_ Ips _}</th>
                                    <th>{_ Rscs _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for user, session_count, requests, posts, avg, mean, date_first, date_last, ips, rsc in user_activity %}
                                    <tr>
                                        <td>
                                            <a href="{% url admin_edit_rsc id=user %}">
                                                {{ user.title | default:(m.identity[user].username) | default:user }}
                                            </a>
                                        </td>
                                        <td>{{ session_count }}</td>
                                        <td>{{ requests }}</td>
                                        <td>{{ posts }}</td>
                                        <td>{{ avg | round }}</td>
                                        <td>{{ mean | round }}</td>
                                        <td>{{ date_first }} - {{ date_last }}</td>
                                        <td>{{ ips | length }}</td>
                                        <td>{{ rsc | length }}</td>
                                    </tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# Dispatch Rule Health Section #}
    {% with m.analytics.dispatch_rule_health as health %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Dispatch Rule Health _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ Dispatch _}</th>
                                    <th>{_ Total _}</th>
                                    <th>{_ # Successes _}</th>
                                    <th>{_ # Errors _}</th>
                                    <th>{_ Percentage Faults _}</th>
                                    <th>{_ Avg _}</th>
                                    <th>{_ Mean _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for dispatch, total, success, error, perc, avg, mean in health %}
                                    <tr>
                                        <td>{{ dispatch }}</td>
                                        <td>{{ total }}</td>
                                        <td>{{ success }}</td>
                                        <td>{{ error }}</td>
                                        <td>{{ perc | round }}</td>
                                        <td>{{ avg | round }}</td>
                                        <td>{{ mean | round }}</td>
                                    </tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# Unique Visitors Section #}
    {% with m.analytics.unique_visitors as visitors %}
    {% with visitors | map:"2" | max as max_sessions %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Unique Visitors _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto; padding: 15px;">
                    {% for day, unique_sessions in visitors %}
                    <div style="margin-bottom: 15px;">
                        <div style="display: flex; align-items: center; margin-bottom: 5px;">
                            <div style="width: 120px; font-size: 13px; font-weight: 500;">
                                {{ day | date:"l j" }}
                            </div>
                            <div style="flex: 1; position: relative;">
                                <div class="progress" style="margin-bottom: 0; height: 24px; background-color: #f5f5f5;">
                                    <div class="progress-bar progress-bar-info" role="progressbar" 
                                         aria-valuenow="{{ unique_sessions }}" aria-valuemin="0" aria-valuemax="{{ max_sessions }}"
                                         style="width: {% if max_sessions > 0 %}{{ unique_sessions|mul:100|div:max_sessions }}%{% else %}0%{% endif %}; 
                                                line-height: 24px; font-size: 12px; font-weight: 600;">
                                        {{ unique_sessions }}
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    {% endfor %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}
    {% endwith %}

    {# Popular Pages Section #}
    {% with m.analytics.popular_pages as popular %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Popular Pages _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ Path _}</th>
                                    <th>{_ Views _}</th>
                                    <th>{_ Sessions _}</th>
                                    <th>{_ Users _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for path, views, sessions, users in popular %}
                                <tr>
                                    <td>{{ path | escape }}</td>
                                    <td>{{ views }}</td>
                                    <td>{{ sessions }}</td>
                                    <td>{{ users }}</td>
                                </tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# Popular Resources Section #}
    {% with m.analytics.popular_resources as popular %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Popular Resources _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ Resource _}</th>
                                    <th>{_ Views _}</th>
                                    <th>{_ Sessions _}</th>
                                    <th>{_ Users _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for id, views, sessions, users in popular %}
                                <tr>
                                    <td>
                                        <a href="{% url admin_edit_rsc id=id %}">
                                            {{ id.title | default:id }}
                                        </a>
                                    </td>
                                    <td>{{ views }}</td>
                                    <td>{{ sessions }}</td>
                                    <td>{{ users }}</td>
                                </tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

</div>

{% endblock %}
