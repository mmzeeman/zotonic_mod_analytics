{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

<div class="analytics-panels">

    {% with m.analytics.stats_overview as stats_overview %}
    <ul class="list-unstyled">
        {# A bit weird.. duckdb retrieves data in columns, it is transposed, and now it transposed back again #}
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:2) title=_"Requests" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:3) title=_"Resources Visited" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:4) title=_"Users" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:5) title=_"Sessions" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:6) title=_"Data Out" units=_"Mb" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:7) title=_"Client Errors" %}</li>
        <li>{% include "_sparkline_with_title.tpl" values = (stats_overview | values:8) title=_"Server Errors" %}</li>
    </ul>
    {% endwith %}

    {% with m.analytics.user_activity  as user_activity %}
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
                <td>{{ user.title | default:(m.identity[user].username) | default:user  }}</td>
                <td>{{ session_count }}</td>
                <td>{{ requests }}</td>
                <td>{{ posts }}</td>
                <td>{{ avg | round }}</td>
                <td>{{ mean | round }}</td>
                <td>{{ date_first }} - {{ date_last }}</td>
                <td>{{ ips | length }}</td>
                <td>{{ rsc | length }}</td>
            </tr>
        {% endfor %}
    </table>
    {% endwith %}



    {% with m.analytics.dispatch_rule_health as health %}
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

    {% with m.analytics.unique_visitors as visitors %}
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

    {% with m.analytics.popular_pages as popular %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Path _}</td>
                <td>{_ Views _}</td>
                <td>{_ Sessions _}</td>
                <td>{_ Users _}</td>
            </tr>
        </thead>
        {% for path, views, sessions, users in popular %}
        <tr>
            <td>{{ path | escape }}</td>
            <td>{{ views }}</td>
            <td>{{ sessions }}</td>
            <td>{{ users }}</td>
        </tr>
        {% endfor %}
    </table>
    {% endwith %}

    {% with m.analytics.popular_resources as popular %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Resource _}</td>
                <td>{_ Views _}</td>
                <td>{_ Sessions _}</td>
                <td>{_ Users _}</td>
            </tr>
        </thead>
        {% for id, views, sessions, users in popular %}
        <tr>
            <td>{{ id.title | default:id }}</td>
            <td>{{ views }}</td>
            <td>{{ sessions }}</td>
            <td>{{ users }}</td>
        </tr>
        {% endfor %}
    </table>
    {% endwith %}

</div>

{% endblock %}
