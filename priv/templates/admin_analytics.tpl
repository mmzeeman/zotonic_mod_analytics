{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

{# Time Range Selector #}

<div class="well">
    {% if is_include_admin %}
        <a href="{% url admin_analytics view=active_view range=active_range include_bots=q.include_bots filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=q.filter_user %}" class="btn btn-default">Include admin 🟢</a>
    {% else %}
        <a href="{% url admin_analytics view=active_view range=active_range include_bots=q.include_bots include_admin=true filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=q.filter_user %}" class="btn btn-default">Include admin ⚪</a>
    {% endif %}

    {% if is_include_bots %}
        <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=q.filter_user %}" class="btn btn-default">Include bots 🟢</a>
    {% else %}
        <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=true filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=q.filter_user %}" class="btn btn-default">Include bots ⚪</a>
    {% endif %}

    {% include "_time_range_selector.tpl" active_range=active_range active_view=active_view %}

    {% if filter_path or filter_rsc or filter_user %}
    <div style="margin-top: 10px;">
        <strong>{_ Active filters: _}</strong>
        {% if filter_path %}
            <span class="label label-info" style="font-size: 90%; margin-left: 5px;">
                {_ Path _}: {{ filter_path | escape }}
                <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_rsc=q.filter_rsc filter_user=q.filter_user %}" style="color: white; margin-left: 4px;" title="{_ Remove filter _}">&times;</a>
            </span>
        {% endif %}
        {% if filter_rsc %}
            <span class="label label-info" style="font-size: 90%; margin-left: 5px;">
                {_ Resource _}: {{ filter_rsc.title | default:filter_rsc }}
                <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=q.filter_path filter_user=q.filter_user %}" style="color: white; margin-left: 4px;" title="{_ Remove filter _}">&times;</a>
            </span>
        {% endif %}
        {% if filter_user %}
            <span class="label label-info" style="font-size: 90%; margin-left: 5px;">
                {_ User _}: {{ filter_user.title | default:filter_user }}
                <a href="{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=q.filter_path filter_rsc=q.filter_rsc %}" style="color: white; margin-left: 4px;" title="{_ Remove filter _}">&times;</a>
            </span>
        {% endif %}
    </div>
    {% endif %}
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
                <a href="{% url admin_analytics view=view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=q.filter_user %}">
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
                            <tbody data-onclick-topic="model/location/post/redirect-local">
                                {% for path, views, sessions, users in popular %}
                                <tr style="cursor: pointer;" tabindex="0" data-onclick-message='{"href": "{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=path filter_rsc=q.filter_rsc filter_user=q.filter_user %}"}'>
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
                            <tbody data-onclick-topic="model/location/post/redirect-local">
                                {% for id, views, sessions, users in popular %}
                                <tr style="cursor: pointer;" tabindex="0" data-onclick-message='{"href": "{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=q.filter_path filter_rsc=id filter_user=q.filter_user %}"}'>
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

    {# Most Active Users Section #}
    {% with m.analytics.most_active_users as active_users %}
    <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-12">
            <div class="panel panel-default">
                <div class="panel-heading">
                    <h3 class="panel-title">{_ Most Active Users _}</h3>
                </div>
                <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                    <div class="table-responsive">
                        <table class="table table-striped table-hover table-condensed">
                            <thead>
                                <tr>
                                    <th>{_ User _}</th>
                                    <th>{_ Views _}</th>
                                    <th>{_ Sessions _}</th>
                                    <th>{_ Resources _}</th>
                                    <th>{_ Paths _}</th>
                                </tr>
                            </thead>
                            <tbody data-onclick-topic="model/location/post/redirect-local">
                                {% for user_id, views, sessions, resources, paths in active_users %}
                                <tr style="cursor: pointer;" tabindex="0" data-onclick-message='{"href": "{% url admin_analytics view=active_view range=active_range include_admin=q.include_admin include_bots=q.include_bots filter_path=q.filter_path filter_rsc=q.filter_rsc filter_user=user_id %}"}'>
                                    <td>{{ user_id.title | default:user_id }}</td>
                                    <td>{{ views }}</td>
                                    <td>{{ sessions }}</td>
                                    <td>{{ resources }}</td>
                                    <td>{{ paths }}</td>
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
