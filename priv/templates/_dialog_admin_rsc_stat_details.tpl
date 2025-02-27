{% with m.ducklog.popular_pages[id] as pages %}
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

{% with m.ducklog.popular_referrers[id] as referrer %}
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
