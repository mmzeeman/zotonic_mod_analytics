{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}Stats {{ id }}{% endblock %}

{% block widget_content %}
{% with m.ducklog.rsc_stats_overview[id] as stats_overview %}
<div class="row">
    <div class="col-md-4">
        <div class="panel panel-default">
            <div class="panel-heading">Total&nbsp;Views</div>
            <div class="panel-body text-center">
                <h1><span id="viewCount">{{ (stats_overview | values:2) | sum_values }}</span></h1>
            </div>
        </div>
    </div>

    <div class="col-md-8">
        {% wire id="rsc-stats-details-btn"
                action={dialog_open title=_"Details"
                                    width=1500
                                    template="_dialog_admin_rsc_stat_details.tpl"
                                    id=id
                       }
        %}
        <button id="rsc-stats-details-btn" class="btn btn-default btn-xs pull-right">
            {_ Show details _}
        </button>

        <ul class="list-unstyled pull-right">
            <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Views" %}</li>
            <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Sessions" %}</li>
            <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}</li>
        </ul>

    </div>
</div>
{% endwith %}
{% endblock %}
