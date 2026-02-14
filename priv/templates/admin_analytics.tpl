{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
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

    {# Main Visualizations Grid #}
    <div class="analytics-grid">
        
        {# Panel: Traffic by Hour of Day #}
        <div class="analytics-grid-full">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Traffic by Hour of Day _}</h3>
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

        {# Panel: Error Breakdown #}
        <div class="analytics-grid-full">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Error Breakdown _}</h3>
                </div>
                <div class="panel-body">
                    {% with m.analytics.error_breakdown as error_data %}
                    {% if error_data %}
                        {% with error_data|element:1 as labels %}
                        {% with error_data|element:2 as values %}
                        {# Create data with colors for donut chart #}
                        {% with labels|first as label1 %}
                        {% with labels|last as label2 %}
                        {% with values|first as val1 %}
                        {% with values|last as val2 %}
                        {% with [[label1, val1, "#f0ad4e"], [label2, val2, "#d9534f"]] as colored_data %}
                        {% include "_chart_donut.tpl" 
                            data=colored_data 
                            title=""
                            size=300
                            show_legend=1 %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">✅</div>
                            <div class="chart-empty-text">{_ No errors found _}</div>
                        </div>
                    {% endif %}
                    {% endwith %}
                </div>
            </div>
        </div>

    </div>

    {# Unique Visitors Section - SVG Bar Chart #}
    {% with m.analytics.unique_visitors as visitors %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Unique Visitors (30 days) _}</h3>
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

    {# User Activity Section - Collapsible #}
    {% with m.analytics.user_activity  as user_activity %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default panel-collapsible">
                <div class="panel-heading" onclick="this.parentElement.querySelector('.panel-body').classList.toggle('hidden');">
                    <h3 class="panel-title">
                        {_ User Activity _}
                        <span class="panel-collapse-icon">▼</span>
                    </h3>
                </div>
                <div class="panel-body hidden" style="max-height: 400px; overflow-y: auto;">
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

    {# Dispatch Rule Health Section - Collapsible #}
    {% with m.analytics.dispatch_rule_health as health %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default panel-collapsible">
                <div class="panel-heading" onclick="this.parentElement.querySelector('.panel-body').classList.toggle('hidden');">
                    <h3 class="panel-title">
                        {_ Dispatch Rule Health _}
                        <span class="panel-collapse-icon">▼</span>
                    </h3>
                </div>
                <div class="panel-body hidden" style="max-height: 400px; overflow-y: auto;">
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
