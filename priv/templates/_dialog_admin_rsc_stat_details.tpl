{% with m.ducklog.popular_pages[id] as pages %}
<table class="table table-condensed">
    <thead>
        <tr>
            <td>{_ Path _}</td>
            <td>{_ Hits _}</td>
            <td>{_ Visitors _}</td>
            <td>{_ Users _}</td>
        </tr>
    </thead>
    {% for page, hits, visitors, users in pages %}
        <tr>
            <td>{{ page| escape }}</td>
            <td>{{ hits }}</td>
            <td>{{ visitors }}</td>
            <td>{{ users }}</td>
        </tr>
    {% endfor %}
</table>
{% endwith %}
