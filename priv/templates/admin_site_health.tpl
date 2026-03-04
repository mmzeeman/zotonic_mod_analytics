{% extends "admin_base.tpl" %}

{% block title %}{_ Site Health _}{% endblock %}

{% block content %}

{% lib "css/analytics.css" %}

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


{% with m.analytics.slow_pages as slow_pages %}
<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {_ Slow Pages _}
            {% if slow_pages %}
                <span class="badge" style="background:#c0392b">{{ slow_pages|length }}</span>
            {% endif %}
        </h3>
    </div>

    {% if slow_pages %}
    <div class="table-responsive">
        <table class="table table-hover table-condensed" style="margin-bottom:0">
            <thead>
                <tr>
                    <th>{_ Path _}</th>
                    <th>{_ Controller _}</th>
                    <th>{_ Rule _}</th>
                    <th class="text-right">{_ Hits _}</th>
                    <th class="text-right">{_ P50 _}</th>
                    <th class="text-right">{_ P95 _}</th>
                    <th class="text-right">{_ P99 _}</th>
                    <th class="text-right">{_ Max _}</th>
                    <th class="text-right">{_ P95 Process _}</th>
                    <th class="text-right">{_ I/O Gap _}</th>
                    <th class="text-right">{_ % Slow _}</th>
                </tr>
            </thead>
            <tbody>
                {% for path, controller, rule, hits, p50, p95, p99, max, p95_process, io_gap, pct_slow_hits in slow_pages %}
                <tr>
                    <td><code>{{ path | escape }}</code></td>
                    <td>{{ controller | escape }}</td>
                    <td>{{ rule | escape }}</td>
                    <td class="text-right"><code>{{ hits }}</code></td>
                    <td class="text-right"><code>{{ p50 }}ms</code></td>
                    <td class="text-right">
                        <code>
                        {% if p95 > 3000 %}
                            <span style="color:#c0392b">{{ p95 }}ms</span>
                        {% elseif p95 > 1000 %}
                            <span style="color:#c47900">{{ p95 }}ms</span>
                        {% else %}
                            <span style="color:#1a7f4b">{{ p95 }}ms</span>
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right"><code>{{ p99 }}ms</code></td>
                    <td class="text-right"><code>{{ max }}ms</code></td>
                    <td class="text-right">
                        {% if p95_process and p95 > 0 %}
                        <code>{{ p95_process }}ms</code>
                        {% else %}
                        <span class="text-muted">—</span>
                        {% endif %}
                    </td>
                    <td class="text-right">
                        {% if io_gap %}
                        {% if io_gap > p95 / 2 %}
                        <code><span style="color:#c47900">{{ io_gap }}ms</span></code>
                        {% else %}
                        <code>{{ io_gap }}ms</code>
                        {% endif %}
                        {% else %}
                        <span class="text-muted">—</span>
                        {% endif %}
                    </td>
<td>
    {% if pct_slow_hits %}
        <div style="display:flex; align-items:center; gap:6px">
            <div style="width:80px; height:12px; background:#e8e5e0; border-radius:3px; overflow:hidden; flex-shrink:0">
                <div style="width:{{ pct_slow_hits }}%; height:100%;
                    {% if pct_slow_hits > 50 %}background:#c0392b;
                    {% elseif pct_slow_hits > 20 %}background:#c47900;
                    {% else %}background:#1a7f4b;
                    {% endif %}">
                </div>
            </div>
            <code>
                {% if pct_slow_hits > 50 %}
                    <span style="color:#c0392b">{{ pct_slow_hits }}%</span>
                {% elseif pct_slow_hits > 20 %}
                    <span style="color:#c47900">{{ pct_slow_hits }}%</span>
                {% else %}
                    {{ pct_slow_hits }}%
                {% endif %}
            </code>
        </div>
    {% else %}
        <span class="text-muted">—</span>
    {% endif %}
</td>

                {% endfor %}
            </tbody>
        </table>
    </div>
    {% else %}
    <div class="panel-body text-center text-muted">
        <span>✓</span> {_ No slow pages detected _}
    </div>
    {% endif %}
</div>
{% endwith %}

{# Broken Links #}
{% with m.analytics.broken_links as broken_links %}
<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {_ 404 Pages _}
            {% if broken_links %}
                <span class="badge" style="background:#c47900">{{ broken_links | length }}</span>
            {% endif %}
        </h3>
    </div>

    {% if broken_links %}
    <div class="table-responsive">
        <table class="table table-hover table-condensed" style="margin-bottom:0">
            <thead>
                <tr>
                    <th>{_ Path _}</th>
                    <th>{_ Referer _}</th>
                    <th class="text-right">{_ Hits _}</th>
                    <th class="text-right">{_ Visitors _}</th>
                    <th>{_ First seen _}</th>
                    <th>{_ Last seen _}</th>
                </tr>
            </thead>
            <tbody>
                {% for path, referer_domain, is_internal, hits, unique_visitors, first_seen, last_seen in broken_links %}
                <tr>
                    <td>
                        <code>{{ path | escape }}</code>
                        {% if is_internal %}
                            <span class="label label-warning">{_ internal _}</span>
                        {% endif %}
                    </td>
                    <td>
                        {% if referer_domain %}
                            <code>{{ referer_domain | escape }}</code>
                        {% else %}
                            <span class="text-muted">—</span>
                        {% endif %}
                    </td>
                    <td class="text-right">
                        <code>
                        {% if hits > 50 %}
                            <span style="color:#c0392b">{{ hits }}</span>
                        {% elseif hits > 10 %}
                            <span style="color:#c47900">{{ hits }}</span>
                        {% else %}
                            {{ hits }}
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right"><code>{{ unique_visitors }}</code></td>
                    <td><code>{{ first_seen }}</code></td>
                    <td><code>{{ last_seen }}</code></td>
                </tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
    {% else %}
    <div class="panel-body text-center text-muted">
        <span>✓</span> {_ No 404 pages detected _}
    </div>
    {% endif %}
</div>
{% endwith %}

{% with m.analytics.suspicious_ips as suspicious_ips %}
<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {_ Suspicious IPs _}
            {% if suspicious_ips %}
                <span class="badge" style="background:#c0392b">{{ suspicious_ips|length }}</span>
            {% endif %}
        </h3>
    </div>

    {% if suspicious_ips %}
    <div class="table-responsive">
        <table class="table table-hover table-condensed" style="margin-bottom:0">
            <thead>
                <tr>
                    <th>{_ IP _}</th>
                    <th class="text-right">{_ Requests _}</th>
                    <th class="text-right">{_ Visitors _}</th>
                    <th class="text-right">{_ Req/Visitor _}</th>
                    <th class="text-right">{_ 404s _}</th>
                    <th class="text-right">{_ Auth _}</th>
                    <th class="text-right">{_ Entropy _}</th>
                    <th class="text-right">{_ Score _}</th>
                    <th>{_ First seen _}</th>
                    <th>{_ Last seen _}</th>
                    <th>{_ Reasons _}</th>
                </tr>
            </thead>
            <tbody>
                {% for peer_ip, requests, visitor_ids, user_agents, req_per_visitor, unique_paths, sessions, not_founds, auth_errors, server_errors, path_entropy, first_seen, last_seen, active_hours, req_last_hour, severity_score, reasons in suspicious_ips %}
                <tr>
                    <td>
                        <code>{{ peer_ip | escape }}</code>
                                            </td>
                    <td class="text-right"><code>{{ requests }}</code></td>
                    <td>
                        {% if user_agents %}
                            <details>
                                <summary style="cursor:pointer">
                                    <code>{{ user_agents|length }} {_ agents _}</code>
                                </summary>
                                <ul style="margin:4px 0 0 0; padding-left:16px; list-style:disc">
                                    {% for ua in user_agents %}
                                        <li><code>{{ ua | escape }}</code></li>
                                    {% endfor %}
                                </ul>
                            </details>
                        {% endif %}
                    <td class="text-right">
                        <code>
                        {% if req_per_visitor > 2000 %}
                            <span style="color:#c0392b">{{ req_per_visitor|round }}</span>
                        {% elseif req_per_visitor > 500 %}
                            <span style="color:#c47900">{{ req_per_visitor|round }}</span>
                        {% else %}
                            {{ req_per_visitor|round }}
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right">
                        <code>
                        {% if not_founds > 100 %}
                            <span style="color:#c0392b">{{ not_founds }}</span>
                        {% elseif not_founds > 20 %}
                            <span style="color:#c47900">{{ not_founds }}</span>
                        {% else %}
                            {{ not_founds }}
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right">
                        <code>
                        {% if auth_errors > 20 %}
                            <span style="color:#c0392b">{{ auth_errors }}</span>
                        {% elseif auth_errors > 5 %}
                            <span style="color:#c47900">{{ auth_errors }}</span>
                        {% else %}
                            {{ auth_errors }}
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right">
                        <code>
                        {% if path_entropy > 4.0 %}
                            <span style="color:#c0392b">{{ path_entropy| format_si }}</span>
                        {% elseif path_entropy > 3.0 %}
                            <span style="color:#c47900">{{ path_entropy| format_si }}</span>
                        {% else %}
                            {{ path_entropy| format_number }}
                        {% endif %}
                        </code>
                    </td>
                    <td class="text-right">
                        <code>
                        {% if severity_score > 100 %}
                            <span style="color:#c0392b">{{ severity_score|round }}</span>
                        {% elseif severity_score > 30 %}
                            <span style="color:#c47900">{{ severity_score|round }}</span>
                        {% else %}
                            {{ severity_score|round }}
                        {% endif %}
                        </code>
                    </td>
                    <td><code>{{ first_seen }}</code></td>
                    <td><code>{{ last_seen }}</code></td>
                    <td>
                        {% for reason in reasons %}
                            <span class="label label-default">{{ reason | escape }}</span>
                        {% endfor %}
                    </td>
                </tr>
                {% endfor %}
            </tbody>
        </table>
    </div>
    {% else %}
    <div class="panel-body text-center text-muted">
        <span>✓</span> {_ No suspicious IPs detected _}
    </div>
    {% endif %}
</div>
{% endwith %}


{% endblock %}
