{% with m.analytics.requests_per_minute as rpm %}
<div class="container-fluid">
    <div class="row">
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"All" trend_data=rpm|values:2 %}
            </div>
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"1xx" trend_data=rpm|values:3 %}
            </div>
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"2xx" trend_data=rpm|values:4 %}
            </div>
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"3xx" trend_data=rpm|values:5 %}
            </div>
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"4xx" trend_data=rpm|values:6 %}
            </div>
            <div class="col-md-2 col-sm-4 col-xs-6">
                {% include "_card_simple_stat.tpl" format="si" title=_"5xx" trend_data=rpm|values:7 %}
            </div>
    </div>
</div>
{% endwith %}


