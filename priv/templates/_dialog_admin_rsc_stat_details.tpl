{% with m.analytics.popular_pages[id] as pages %}
<table class="table table-condensed">
    <thead>
        <tr>
            <td>{_ Path _}</td>
            <td>{_ Views _}</td>
            <td>{_ Sessions _}</td>
            <td>{_ Users _}</td>
        </tr>
    </thead>
    {% for page, views, sessions, users in pages %}
        <tr>
            <td>{{ page | escape }}</td>
            <td>{{ views }}</td>
            <td>{{ sessions }}</td>
            <td>{{ users }}</td>
        </tr>
    {% endfor %}
</table>
{% endwith %}

{% with m.analytics.popular_referrers[id] as referrer %}
<table class="table table-condensed">
    <thead>
        <tr>
            <td>{_ Referrer _}</td>
            <td>{_ Visits _}</td>
        </tr>
    </thead>
    {% for referrer, visits in referrer %}
        <tr>
            <td>{{ referrer | escape }}</td>
            <td>{{ visits }}</td>
        </tr>
    {% endfor %}
</table>
{% endwith %}

{% with m.analytics.access_log[id] as log %}
<div class="table-responsive" style="max-height: 300px; overflow: scroll;">
<table class="table table-condensed table-hover" style="white-space:nowrap;">
    <thead>
        <tr>
            <td>{_ Timestamp _}</td>
            <td>{_ Peer _}</td>
            <td>{_ Path _}</td>
            <td>{_ User _}</td>
            <td>{_ Session Id _}</td>
            <td>{_ Referrer _}</td>
            <td>{_ UA _}</td>
        </tr>
    </thead>
    {% for req_version, req_method,
           resp_code, path, qs, referer,
           duration_total,
           peer_ip, session_id, user_id,
           language, timezone, user_agent,
           timestamp in log %}
        <tr>
            <td>{{ timestamp | date:"y-m-d H:i:s" }}</td>
            <td>{{ peer_ip | escape }}</td>

            {% if not qs %}
                <td>{{ path | escape }}</td>
            {% else %}
                <td>{{ path | escape }}?{{ qs | escape }}</td>
            {% endif %}

            <td>{% if user_id %}<a href="{% url admin_edit_rsc id=user_id %}">{% if m.identity[user_id].username as username %}{{ username | escape }}{% else %}{{ user_id }}{% endif %}</a>{% endif %}</td>
            <td>{{ session_id | truncate:8:"" | escape }}</td>
            <td>{{ referer | escape }}</td>
            <td>{% with user_agent | ua as ua %}{{ ua.browser }} {_ on _} {{ ua.os }}{% endwith %}</td>
        </tr>
    {% endfor %}
</table>
</div>
{% endwith %}
