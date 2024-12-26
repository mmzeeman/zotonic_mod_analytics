{% extends "admin_base.tpl" %}

{% block title %}{_ Analytics _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Analytics _}</h2>
    <p>{_ This page shows site analytics. _}</p>
</div>

<div class="analytics-panels">
    {% with m.ducklog.dispatch_rule_health as health %}
    <table class="table table-condensed">
        <thead>
            <tr>
                <td>{_ Dispatch _}</td>
                <td>{_ Total _}</td>
                <td>{_ Success _}</td>
                <td>{_ Error _}</td>
                <td>{_ % _}</td>
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
                <td>{{ perc }}</td>
                <td>{{ avg }}</td>
                <td>{{ mean }}</td>
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
