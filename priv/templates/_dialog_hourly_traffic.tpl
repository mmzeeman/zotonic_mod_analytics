{% with m.analytics[{traffic_by_hour_of_day active_range=active_range
                                            is_include_admin=is_include_admin
                                            is_include_bots=is_include_bots
                                            filter_path=filter_path
                                            filter_rsc=filter_rsc
                                            filter_user=filter_user}] as hourly_data %}
{% if hourly_data %}
    <ul class="nav nav-tabs" role="tablist">
        <li role="presentation" class="active">
            <a href="#hourly-requests" aria-controls="hourly-requests" role="tab" data-toggle="tab">{_ Requests _}</a>
        </li>
        <li role="presentation">
            <a href="#hourly-sessions" aria-controls="hourly-sessions" role="tab" data-toggle="tab">{_ Sessions _}</a>
        </li>
    </ul>

    <div class="tab-content" style="padding-top: 15px;">
        <div role="tabpanel" class="tab-pane active" id="hourly-requests">
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
        </div>

        <div role="tabpanel" class="tab-pane" id="hourly-sessions">
            {% with hourly_data | element:1 as hours %}
            {% with hourly_data | element:3 as sessions %}
            {% with hours | zip:sessions as chart_data %}
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
        </div>
    </div>
{% else %}
    <div class="chart-empty">
        <div class="chart-empty-icon">📊</div>
        <div class="chart-empty-text">{_ No hourly traffic data available _}</div>
    </div>
{% endif %}
{% endwith %}
