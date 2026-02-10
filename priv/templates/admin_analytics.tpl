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
                <div class="panel-body">
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
                                        <td>{{ user.title | default:(m.identity[user].username) | default:user  }}</td>
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
                <div class="panel-body">
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
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Unique Visitors _}</h3>
                </div>
                <div class="panel-body">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ Day _}</th>
                                    <th>{_ Unique Sessions _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for day, unique_sessions in visitors %}
                                    <tr>
                                        <td>{{ day | date:"l j"}}</td>
                                        <td>{{ unique_sessions }}</td>
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

    {# Popular Pages Section #}
    {% with m.analytics.popular_pages as popular %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Popular Pages _}</h3>
                </div>
                <div class="panel-body">
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
                <div class="panel-body">
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
                                    <td>{{ id.title | default:id }}</td>
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
