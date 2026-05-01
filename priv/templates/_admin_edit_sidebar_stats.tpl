{% extends "admin_edit_widget_std.tpl" %}

{% block widget_content %}
{% with m.analytics.rsc_stats_overview[id] as stats_overview %}
<div class="row">
    <div class="col-md-12">
        <div class="panel panel-default">
            <div class="panel-heading text-center">{_ Total Views _}</div>
            <div class="panel-body text-center">
                <h1 class="text-primary" id="viewCount">{{ (stats_overview | values:2) | sum }}</h1>
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12">
        <div class="form-group">
            {% wire id=#detailed_analytics
                    action={dialog_open title=_"Detailed Analytics"
                                        width="large"
                                        template="_dialog_admin_rsc_stat_details.tpl"
                                        id=id }
            %}
            <button type="button" id="{{ #detailed_analytics }}" class="btn btn-default">
                <span class="glyphicon glyphicon-list-alt"></span> {_ Access Log _}
            </button>

            <a href="{% url admin_analytics filter_rsc=id %}" class="btn btn-default">{_ Analytics _}</a>
        </div>
    </div>
</div>
{% endwith %}
{% endblock %}
