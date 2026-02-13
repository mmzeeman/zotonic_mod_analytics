{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

<div class="container-fluid">

    {# Key Metrics Cards - Top Section #}
    {% with m.analytics.stats_overview as stats_overview %}
    <div class="stat-cards-grid">
        <div class="col-lg-3 col-md-6 col-sm-6">
            {% with stats_overview | values:2 as requests_data %}
            {% with requests_data | last as current_requests %}
            {% with requests_data | length as data_length %}
            {% if data_length > 1 %}
                {% with requests_data | slice:[0,-2] | last as prev_requests %}
                {% with prev_requests > 0 as has_prev %}
                {% if has_prev %}
                    {% with ((current_requests - prev_requests) / prev_requests) * 100|round as change_pct %}
                    {% include "_stat_card.tpl" 
                        value=current_requests 
                        label=_"Total Requests"
                        trend_data=requests_data
                        change_percent=change_pct
                        icon="📊" %}
                    {% endwith %}
                {% else %}
                    {% include "_stat_card.tpl" 
                        value=current_requests 
                        label=_"Total Requests"
                        trend_data=requests_data
                        icon="📊" %}
                {% endif %}
                {% endwith %}
                {% endwith %}
            {% else %}
                {% include "_stat_card.tpl" 
                    value=current_requests 
                    label=_"Total Requests"
                    trend_data=requests_data
                    icon="📊" %}
            {% endif %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
        </div>
        
        <div class="col-lg-3 col-md-6 col-sm-6">
            {% with stats_overview | values:5 as sessions_data %}
            {% with sessions_data | last as current_sessions %}
            {% include "_stat_card.tpl" 
                value=current_sessions 
                label=_"Sessions"
                trend_data=sessions_data
                icon="👥" %}
            {% endwith %}
            {% endwith %}
        </div>
        
        <div class="col-lg-3 col-md-6 col-sm-6">
            {% with stats_overview | values:4 as users_data %}
            {% with users_data | last as current_users %}
            {% include "_stat_card.tpl" 
                value=current_users 
                label=_"Unique Visitors"
                trend_data=users_data
                icon="👤" %}
            {% endwith %}
            {% endwith %}
        </div>
        
        <div class="col-lg-3 col-md-6 col-sm-6">
            {% with stats_overview | values:7 as client_errors %}
            {% with stats_overview | values:8 as server_errors %}
            {% with client_errors | last as current_client_errors %}
            {% with server_errors | last as current_server_errors %}
            {% with current_client_errors + current_server_errors as total_errors %}
            {% include "_stat_card.tpl" 
                value=total_errors 
                label=_"Errors"
                icon="⚠️" %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
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

        {# Panel: Response Time Distribution #}
        <div class="analytics-grid-half">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Response Time Distribution _}</h3>
                </div>
                <div class="panel-body">
                    {% with m.analytics.response_time_distribution as rt_data %}
                    {% if rt_data %}
                        {% include "_chart_horizontal_bar.tpl" 
                            data=rt_data 
                            title=""
                            height=300
                            color="#5cb85c" %}
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">⏱️</div>
                            <div class="chart-empty-text">{_ No response time data available _}</div>
                        </div>
                    {% endif %}
                    {% endwith %}
                </div>
            </div>
        </div>

        {# Panel: Error Breakdown #}
        <div class="analytics-grid-half">
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

        {# Panel: Traffic Sources #}
        <div class="analytics-grid-full">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Top Traffic Sources _}</h3>
                </div>
                <div class="panel-body">
                    {% with m.analytics.traffic_sources as sources %}
                    {% if sources %}
                        {% include "_chart_horizontal_bar.tpl" 
                            data=sources 
                            title=""
                            height=400
                            color="#5bc0de" %}
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">🔗</div>
                            <div class="chart-empty-text">{_ No traffic source data available _}</div>
                        </div>
                    {% endif %}
                    {% endwith %}
                </div>
            </div>
        </div>
        
    </div>

    {# Unique Visitors Section - Keep existing visualization #}
    {% with m.analytics.unique_visitors as visitors %}
    {% with visitors | values | max as max_sessions %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Unique Visitors _}</h3>
                </div>
                <div class="panel-body" style="padding: 15px 15px 50px 15px;">
                    <div style="display: flex; align-items: flex-end; justify-content: space-between; height: 280px;">
                        {% for day, unique_sessions in visitors %}
                        <div style="display: flex; flex-direction: column; align-items: center; flex: 1; max-width: 40px; position: relative;">
                            <div style="display: flex; flex-direction: column; justify-content: flex-end; height: 260px; width: 100%;">
                                <div style="background-color: #5bc0de; position: relative; width: 100%; 
                                            height: {% if max_sessions > 0 %}{{ (unique_sessions * 100) / max_sessions }}%{% else %}0%{% endif %}; 
                                            min-height: 20px; border-radius: 3px 3px 0 0;"
                                     role="progressbar" 
                                     aria-valuenow="{{ unique_sessions }}" 
                                     aria-valuemin="0" 
                                     aria-valuemax="{{ max_sessions }}">
                                    <div style="position: absolute; top: 5px; width: 100%; text-align: center; 
                                                font-size: 9px; font-weight: 700; color: #2c5d6f;">
                                        {{ unique_sessions }}
                                    </div>
                                </div>
                            </div>
                            <div style="position: absolute; bottom: -35px; left: 50%; transform: translateX(-50%) rotate(-45deg); 
                                        transform-origin: center center; white-space: nowrap; font-size: 9px; font-weight: 500;">
                                {{ day | date:"j M" }}
                            </div>
                        </div>
                        {% endfor %}
                    </div>
                </div>
            </div>
        </div>
    </div>
    {% endwith %}
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

    {# Popular Pages Section - Enhanced with Chart #}
    {% with m.analytics.popular_pages as popular %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Popular Pages _}</h3>
                </div>
                <div class="panel-body">
                    {% if popular %}
                        {# Extract views (second column) for visualization #}
                        {% with popular|element:1 as paths %}
                        {% with popular|element:2 as views %}
                        {% with paths|zip:views as chart_data %}
                        {% include "_chart_horizontal_bar.tpl" 
                            data=chart_data 
                            title=""
                            height=400
                            color="#5bc0de"
                            show_values=1 %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                        
                        {# Keep table for detailed info - make it collapsible #}
                        <details style="margin-top: 20px;">
                            <summary style="cursor: pointer; font-weight: 600; padding: 10px; background: #f5f5f5; border-radius: 4px;">
                                {_ View detailed table _}
                            </summary>
                            <div class="table-responsive" style="margin-top: 10px; max-height: 400px; overflow-y: auto;">
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
                        </details>
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">📄</div>
                            <div class="chart-empty-text">{_ No popular pages data available _}</div>
                        </div>
                    {% endif %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

    {# Popular Resources Section - Enhanced with Chart #}
    {% with m.analytics.popular_resources as popular %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Popular Resources _}</h3>
                </div>
                <div class="panel-body">
                    {% if popular %}
                        {# Extract resource IDs and views for visualization #}
                        {% with popular|element:1 as ids %}
                        {% with popular|element:2 as views %}
                        {# Create chart data using resource titles #}
                        {% with [] as chart_data_list %}
                        {% for id, view_count in ids|zip:views %}
                            {% with id.title|default:id as label %}
                            {% endwith %}
                        {% endfor %}
                        {% endwith %}
                        {# Simplified approach - use map to get titles directly #}
                        {% include "_chart_horizontal_bar.tpl" 
                            data=ids|zip:views
                            title=""
                            height=400
                            color="#5bc0de"
                            show_values=1 %}
                        {% endwith %}
                        {% endwith %}
                        
                        {# Keep table for detailed info with edit links - make it collapsible #}
                        <details style="margin-top: 20px;">
                            <summary style="cursor: pointer; font-weight: 600; padding: 10px; background: #f5f5f5; border-radius: 4px;">
                                {_ View detailed table _}
                            </summary>
                            <div class="table-responsive" style="margin-top: 10px; max-height: 400px; overflow-y: auto;">
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
                        </details>
                    {% else %}
                        <div class="chart-empty">
                            <div class="chart-empty-icon">📚</div>
                            <div class="chart-empty-text">{_ No popular resources data available _}</div>
                        </div>
                    {% endif %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}

</div>

{% endblock %}
