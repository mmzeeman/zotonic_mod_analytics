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
            <td>{{ page| escape }}</td>
            <td>{{ views }}</td>
            <td>{{ sessions }}</td>
            <td>{{ users }}</td>
        </tr>
    {% endfor %}
</table>
{% endwith %}
