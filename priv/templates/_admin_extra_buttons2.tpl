<button class="btn btn-default pull-right">
    <span>Stats</span>
{% with m.ducklog.unique_visitors | values as unique_visitors %}
    {% include "_sparkline.tpl" values=unique_visitors min=0 max=(unique_visitors | max_value) show_first=true show_last=true %}
    <span>Unique Visitors</span> <strong style="color: red">{{ unique_visitors | last }}</strong> 
{% endwith %}
</button>
