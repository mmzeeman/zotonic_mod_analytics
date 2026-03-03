{% extends "admin_base.tpl" %}

{% block title %}{_ Site Health _}{% endblock %}

{% block content %}

<div class="btn-group pull-right">
    <a class="btn btn-default" href="{% url home %}">{_ Button _}</a>
</div>
<div class="admin-header">
    <h2>
        {_ Site Health _}
    </h2>
</div>

<div class="well z-button-row">
    <a class="btn btn-default" href="{% url home %}">{_ Another button _}</a>
</div>

<h3>Slow Pages</h3>
{% for slow in m.analytics.slow_pages  %}
    {{ slow | pprint }}<br>
{% endfor %}

<h3>Suspicous IPs</h3>
{% for s in m.analytics.suspicious_ips %}
    {{ s | pprint }}<br>
{% endfor %}


{% endblock %}
