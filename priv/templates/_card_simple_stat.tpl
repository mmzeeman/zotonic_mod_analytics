<div class="panel {% if is_selected %}panel-primary{% else %}panel-default{% endif %}">
    <div class="panel-body">
        <div class="text-uppercase text-muted">
            {% if is_selected %}
                <u class="text-nowrap">{{ title }}</u>
            {% else %}
                <span class="text-nowrap">{{ title }}</span>
            {% endif %}
        </div>
        <h4 style="position: relative">
            <strong style="color:black">
                {% include "_chart_format_value.tpl" format=format value=value %}
            </strong>
            {% with trend_data | last as last_value %}
            <small style="position: absolute; bottom: 0; right: 0; color: red;">
                <strong>
                    {% include "_chart_format_value.tpl" format=format value=last_value %}
                </strong>
            </small></h4>
            {% endwith %}
        {% if trend_data %}
            {% include "_sparkline_with_last.tpl" values=trend_data format=format %}
        {% endif %}
    </div>
</div>
