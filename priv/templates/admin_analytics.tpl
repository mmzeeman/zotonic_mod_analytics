{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

<div class="analytics-panels">

    <div>
    {% with m.ducklog.stats_overview as stats_overview %}
        {# A bit weird.. duckdb retrieves data in columns, it is transposed, and now I transpose it back #}
        {% include "_sparkline.tpl" values = (stats_overview | values:2) show_last=true %} Requests <br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:3) show_last=true %} Resources Visited<br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:4) show_last=true %} Users<br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:5) show_last=true %} Sessions<br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:6) show_last=true %} Data Out<br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:7) show_last=true %} Client Errors (40x)<br/>
        {% include "_sparkline.tpl" values = (stats_overview | values:8) show_last=true %} Server Errors (50x)<br/>
    {% endwith %}
    </div>

    {% with m.ducklog.user_activity  as user_activity %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ User _}</td>
                <td>{_ # Sessions _}</td>
                <td>{_ # Requests _}</td>
                <td>{_ # Posts _}</td>

                <td>{_ Avg _}</td>
                <td>{_ Mean _}</td>

                <td>{_ Activity _}</td>

                <td>{_ Ips _}</td>
                <td>{_ Rscs _}</td>
            </tr>
        </thead>
        {% for user, session_count, requests, posts, avg, mean, date_first, date_last, ips, rsc in user_activity %}
            <tr>
                <td>{{ user.title | default:user  }}</td>
                <td>{{ session_count }}</td>
                <td>{{ requests }}</td>
                <td>{{ posts }}</td>
                <td>{{ avg | round }}</td>
                <td>{{ mean | round }}</td>
                <td>{{ date_first }} - {{ date_last }}</td>
                <td>{% for ip in ips %}{{ ip }}, {% endfor %}</td>
                <td>{% for id in rsc %}{{ id.title | default:id }}, {% endfor %}</td>
            </tr>
        {% endfor %}
    </table>
    {% endwith %}



    {% with m.ducklog.dispatch_rule_health as health %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Dispatch _}</td>
                <td>{_ Total _}</td>
                <td>{_ # Successes _}</td>
                <td>{_ # Errors _}</td>
                <td>{_ Precentage Faults _}</td>
                <td>{_ Avg _}</td>
                <td>{_ Mean _}</td>
            </tr>
        </thead>
        {% for dispatch, total, success, error, perc, avg, mean in health %}
            <tr>
                <td>{{ dispatch }}</td>
                <td>{{ total }}</td>
                <td>{{ success }}</td>
                <td>{{ error }}</td>
                <td>{{ perc | round }}</td>
                <td>{{ avg | round }}</td>
                <td>{{ mean | round }}</td>
            </tr>
        {% endfor %}
    </table>
    {% endwith %}



    {% with m.ducklog.unique_visitors as visitors %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Day _}</td>
                <td>{_ Unique Sessions _}</td>
            </tr>
        </thead>
        {% for day, unique_sessions in visitors %}
            <tr>
                <td>{{ day | date:"l j"}}</td>
                <td>{{ unique_sessions }}</td>
            </tr>
        {% endfor %}
    </table>
    {% endwith %}

    
    {% with m.ducklog.popular_pages as popular %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Path _}</td>
                <td>{_ Hits _}</td>
                <td>{_ Visitors _}</td>
                <td>{_ Users _}</td>
            </tr>
        </thead>
        {% for path, hits, visitors, users in popular %}
        <tr>
            <td>{{ path | escape }}</td>
            <td>{{ hits }}</td>
            <td>{{ visitors }}</td>
            <td>{{ users }}</td>
        </tr>
        {% endfor %}
    </table>
    {% endwith %}
</div>

{% endblock %}
