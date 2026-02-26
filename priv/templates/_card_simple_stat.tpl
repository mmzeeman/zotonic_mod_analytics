<div class="panel panel-default">
    <div class="panel-body">
        <div class="text-uppercase text-muted"><span class="text-nowrap">{{ title }}</span></div>
        <div>
            <strong>{{ value }}</strong><strong class="pull-right" style="color: red">{{ trend_data | last }}k</strong>
        </div>
        {% if trend_data %}
            {% include "_sparkline_with_last.tpl" values=trend_data  %}
        {% endif %}
    </div>
</div>

