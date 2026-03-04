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
{% with m.analytics.slow_pages as slow_pages %}
    <div class="table-responsive">
        <table class="table table-striped table-hover table-condensed">
            <thead>
                <tr>
                    <th>{_ Path _}</th>
                    <th>{_ Controller _}</th>
                    <th>{_ Rule _}</th>
                    <th>{_ Hits _}</th>
                    <th>{_ P50ms _}</th>
                    <th>{_ P95ms _}</th>
                    <th>{_ P99ms _}</th>
                    <th>{_ Max _}</th>
                    <th>{_ P99 Process _}</th>
                    <th>{_ IO Gap _}</th>
                    <th>{_ % Slow _}</th>
                </tr>
            </thead>
            <tbody>
                {% for path, controller, rule, hits, p50, p95, p99, max, p99_process, io_gap, pct_slow_hits in slow_pages %}
                     <tr>
                         <td>{{ path | escape }}</td>
                         <td>{{ controller }}</td>
                         <td>{{ rule }}</td>
                         <td>{{ hits }}</td>
                         <td>{{ p50 }}</td>
                         <td>{{ p95 }}</td>
                         <td>{{ p99 }}</td>
                         <td>{{ max }}</td>
                         <td>{{ p99_process }}</td>
                         <td>{{ ip_gap }}</td>
                         <td>{{ pct_slow_hits }}</td>
                     </tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
{% endwith %}

<h3>Suspicous IPs</h3>
{% for s in m.analytics.suspicious_ips %}
    {{ s | pprint }}<br>
{% endfor %}


{% endblock %}
