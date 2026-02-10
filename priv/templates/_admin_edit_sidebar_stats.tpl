{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}<span class="glyphicon glyphicon-stats"></span> {_ Analytics _}{% endblock %}

{% block widget_content %}
<style>
.analytics-panel {
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
    margin-bottom: 15px;
}
.analytics-view-count {
    font-size: 48px;
    color: #337ab7;
    transition: color 0.3s ease;
}
.analytics-view-count-panel {
    padding: 20px;
}
.analytics-sparkline-item {
    margin-bottom: 10px;
}
</style>

{% with m.analytics.rsc_stats_overview[id] as stats_overview %}
<div class="row">
    <div class="col-md-12">
        <div class="panel panel-info analytics-panel">
            <div class="panel-heading text-center">{_ Total Views _}</div>
            <div class="panel-body text-center analytics-view-count-panel">
                <div class="analytics-view-count" id="viewCount">{{ (stats_overview | values:2) | sum }}</div>
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12">
        <div class="panel panel-default analytics-panel">
            <div class="panel-body">
                <div class="analytics-sparkline-item">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Views" %}
                </div>
                <div class="analytics-sparkline-item">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Sessions" %}
                </div>
                <div class="analytics-sparkline-item">
                    {% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}
                </div>
            </div>
        </div>
    </div>
</div>

<div class="row">
    <div class="col-md-12 text-center">
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
