{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}Stats {{ id }}{% endblock %}

{% block widget_content %}
{% with m.analytics.rsc_stats_overview[id] as stats_overview %}
<div class="row">
    <div class="col-md-4">
        <div class="panel panel-default">
            <div class="panel-heading text-center">Total&nbsp;Views</div>
            <div class="panel-body text-center">
                <h1><span id="viewCount">{{ (stats_overview | values:2) | sum_values }}</span></h1>
            </div>
        </div>
    </div>

    <div class="col-md-8">
        {% wire id="rsc-stats-details-btn"
                action={dialog_open title=_"Details"
                                    width="large"
                                    template="_dialog_admin_rsc_stat_details.tpl"
                                    id=id }
        %}

        <div>
            {% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Views" %}<br />
            {% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Sessions" %}<br />
            {% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}
            <span class="pull-right">
                <button id="rsc-stats-details-btn" class="btn btn-default btn-xs">
                    {_ Show details _}
                </button>
            </span>
        </div>
    </div>
</div>
{% endwith %}
{% endblock %}
