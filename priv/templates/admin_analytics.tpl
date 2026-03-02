{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

{#
<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

{# Time Range Selector #}

<div class="well">
    {% if is_include_admin %}
        <a href="{% url admin_analytics view=active_view range=active_range include_bots=q.include_bots %}" class="btn btn-default">Include admin 🟢</a>
    {% else %}
        <a href="{% url admin_analytics view=active_view range=active_range include_bots=q.include_bots include_admin=true %}" class="btn btn-default">Include admin ⚪</a>
    {% endif %}

    {% if is_include_bots %}
        <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin %}" class="btn btn-default">Include bots 🟢</a>
    {% else %}
        <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=true %}" class="btn btn-default">Include bots ⚪</a>
    {% endif %}

    {% include "_time_range_selector.tpl" active_range=active_range active_view=active_view %}
</div>

{% with %{ "unique": %{ title: _"Unique Visitors", index: 2, format: "si" },
           "users":  %{ title: _"Users", index: 4, format: "si" },
           "views":  %{ title: _"Total Views", index: 5, format: "si" }
           "visit":  %{ title: _"Views per Visit", index: 6, format: "si" }
           "rate":  %{ title: _"Bounce Rate", index: 7, format: "percent" }
           "duration": %{ title: _"Visit Duration", index: 8, format: "duration" } },
        ["unique", "users", "views", "visit", "rate", "duration"]
   as
       view_map,
       views
%}

<div class="container-fluid">
    {% with m.analytics.overview as overview %}
    <div class="row">
        {% for view in views %}
            <div class="col-md-2 col-sm-4 col-xs-6">
                <a href="{% url admin_analytics view=view range=active_range include_admin=q.include_admin include_bots=q.include_bots %}">
                    {% include "_card_simple_stat.tpl"
                           title=view_map[view].title
                           is_selected=(active_view == view)
                           format=view_map[view].format
                           value=overview.totals | element:view_map[view].index
                           trend_data=(overview.data | values:view_map[view].index ) %}
                </a>
            </div>
        {% endfor %}
    </div>

    {# Unique Visitors Section - SVG Bar Chart #}
    {% with  (overview.data | values:1) | zip:(overview.data | values:view_map[active_view].index) as data %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-body">
                    {% include "_chart_area.tpl"
                        data=data
                        title=""
                        width=1000
                        height=300
                        gradient_colors=["#5bc0de", "#3a9cb8"]
                        y_axis_label=view_map[active_view].title
                        x_axis_label=_"Date"
                    %}
                </div>
            </div>
        </div>
    </div>
    {% endwith %}
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
                        {% with hourly_data | element:1 as hours %}
                        {% with hourly_data | element:2 as requests %}
                        {% with hours | zip:requests as chart_data %}
                        {% include "_chart_bar.tpl" 
                            data=chart_data 
                            title=""
                            height=300
                            width=1000
                            show_grid=1
                            show_values=1
                            y_axis_label=_"Requests"
                            x_axis_label=_"Hour of Day" %}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}

                        {% with hourly_data | element:1 as hours %}
                        {% with hourly_data | element:3 as requests %}
                        {% with hours | zip:requests as chart_data %}
                        {% include "_chart_bar.tpl" 
                            data=chart_data 
                            title=""
                            height=300
                            width=1000
                            show_grid=1
                            show_values=1
                            y_axis_label=_"Sessions"
                            x_axis_label=_"Hour of Day" %}
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

    {# User Activity Section #}

    {#
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
    #}

    {# Dispatch Rule Health Section #}
    {#
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
    #}

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
                                    <td> <a href="{% url admin_edit_rsc id=id %}"> {{ id.title | default:id }} </a> </td>
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
{% endwith %}

{% endblock %}
