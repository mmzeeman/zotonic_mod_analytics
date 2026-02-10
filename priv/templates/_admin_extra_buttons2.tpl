<div class="pull-right">
{% with m.analytics.unique_visitors | values as unique_visitors %}
    {% include "_sparkline.tpl" values=unique_visitors min=0 max=(unique_visitors | max)  show_last=true %}
    <span>{_ Sessions _}</span> <strong style="color: red">{{ unique_visitors | last }}</strong> 
{% endwith %}
</div>
