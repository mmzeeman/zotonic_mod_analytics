{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}<span class="glyphicon glyphicon-stats"></span> {_ Analytics _}{% endblock %}

{% block widget_content %}
{% with m.analytics.rsc_stats_overview[id] as stats_overview %}
<div class="row" style="margin-bottom: 15px;">
    <div class="col-md-12">
        <div class="panel panel-info">
            <div class="panel-heading text-center">{_ Total Views _}</div>
            <div class="panel-body text-center">
                <h1 class="text-primary" id="viewCount">{{ (stats_overview | values:2) | sum }}</h1>
            </div>
        </div>
    </div>
</div>

<div class="row" style="margin-bottom: 15px;">
    <div class="col-md-12">
        <div class="panel panel-default">
            <div class="panel-body text-center">
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Views" %}
                <br />
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Sessions" %}
                <br />
                {% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12">
        {% wire id="rsc-stats-details-btn"
                action={dialog_open title=_"Detailed Analytics"
                                    width="large"
                                    template="_dialog_admin_rsc_stat_details.tpl"
                                    id=id }
        %}
        <button id="rsc-stats-details-btn" class="btn btn-primary btn-block">
            <span class="glyphicon glyphicon-list-alt"></span> {_ Show Detailed Analytics _}
        </button>
    </div>
</div>
{% endwith %}
{% endblock %}
