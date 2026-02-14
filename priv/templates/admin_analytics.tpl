{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

{# Time Range Selector #}
<div class="container-fluid">
    {% include "_time_range_selector.tpl" active_range=active_range %}
</div>

<div class="container-fluid">

    {# Unique Visitors Stat Card #}
    {% with m.analytics.unique_visitors as visitors_data %}
        {% with visitors_data | values:2 as visitor_values %}
            {% if visitor_values %}
                {% with visitor_values | last as current_value %}
                    {% with visitor_values | first as first_value %}
                        {# Calculate percentage change: ((current - first) / first * 100) #}
                        {% with first_value > 0 and ((current_value - first_value) * 100.0 / first_value) | round as change_percent %}
                            <div class="row" style="margin-bottom: 20px;">
                                <div class="col-lg-3 col-md-4 col-sm-6">
                                    {% include "_stat_card.tpl" 
                                        label=_"Unique Visitors"
                                        value=current_value
                                        change_percent=change_percent
                                        trend_data=visitor_values
                                    %}
                                </div>
                            </div>
                        {% endwith %}
                    {% endwith %}
                {% endwith %}
            {% endif %}
        {% endwith %}
    {% endwith %}

</div>

<div class="container-fluid">

    {# Stats Overview Section - Responsive Panel Grid #}
    {% with m.analytics.stats_overview as stats_overview %}
    <div class="panel panel-default">
        <div class="panel-body row align-items-center">
            {# A bit weird.. duckdb retrieves data in columns, it is transposed, and now it transposed back again #}
            <div class="col-lg-3 col-md-4 col-sm-6">
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Requests" %}
                <br>
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Resources Visited" %}
                <br>
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:6) title=_"Data Out" units=_"Mb" %}
            </div>
            <div class="col-lg-3 col-md-4 col-sm-6">
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:5) title=_"Sessions" %}
                <br>
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}
            </div>
            <div class="col-log-3 col-md-4 col-sm-6">
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:7) title=_"Client Errors" %}
                <br>
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:8) title=_"Server Errors" %}
            </div>
        </div>
    </div>
    {% endwith %}

    {# Unique Visitors Section - SVG Bar Chart #}
    {% with m.analytics.unique_visitors as visitors %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">
                        {% if active_range == "7d" %}
                            {_ Unique Visitors (7 days) _}
                        {% elif active_range == "91d" %}
                            {_ Unique Visitors (91 days) _}
                        {% else %}
                            {_ Unique Visitors (28 days) _}
                        {% endif %}
                    </h3>
                </div>
                <div class="panel-body">
                    {% include "_chart_bar.tpl"
                        data=visitors
                        title=""
                        width=1000
                        height=300
                        color="#5bc0de"
                        show_values=true
                        label_format="j M"
                    %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# Main Visualizations Grid #}
    <div class="analytics-grid">
        
        {# Panel: Traffic by Hour of Day #}
        <div class="analytics-grid-full">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">
                        {_ Traffic by Hour of Day _}
                        <small class="text-muted">{_ (aggregated across all days) _}</small>
                    </h3>
                </div>
                <div class="panel-body">
                    {% with m.analytics.traffic_by_hour_of_day as hourly_data %}
                    {% if hourly_data %}
                        {% with hourly_data|element:1 as hours %}
                        {% with hourly_data|element:2 as requests %}
                        {% with hours|zip:requests as chart_data %}
                        {% include "_chart_bar.tpl" 
                            data=chart_data 
                            title=""
                            height=280
                            width=800
                            show_grid=1
                            show_values=1 %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">📊</div>
                            <div class="chart-empty-text">{_ No hourly traffic data available _}</div>
                        </div>
                    {% endif %}
                    {% endwith %}
                </div>
            </div>
        </div>

        {# Error Breakdown Section #}
        <div class="analytics-grid-full">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Error Breakdown _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    {% with m.analytics.error_breakdown as error_data %}
                    {% if error_data %}
                        <div class="table-responsive">
                            <table class="table table-striped table-hover table-condensed">
                                <thead>
                                    <tr>
                                        <th>{_ Error Type _}</th>
                                        <th>{_ Count _}</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {% for error_type, count in error_data %}
                                    <tr>
                                        <td>{{ error_type }}</td>
                                        <td>{{ count }}</td>
                                    </tr>
                                    {% endfor %}
                                </tbody>
                            </table>
                        </div>
                    {% else %}
                        <p class="text-muted">{_ No errors found _}</p>
                    {% endif %}
                    {% endwith %}
                </div>
            </div>
        </div>

    </div>

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
