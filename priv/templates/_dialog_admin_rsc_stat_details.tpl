{# Two-column layout for Popular Pages and Popular Referrers #}
<div class="row" style="margin-bottom: 20px;">
    {# Popular Pages - Left Column #}
    <div class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Popular Pages _}</h3>
            </div>
            <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                {% with m.analytics.popular_pages[id] as pages %}
                    {% if pages %}
                        <div class="table-responsive">
                            <table class="table table-striped table-hover table-condensed">
                                <thead>
                                    <tr>
                                        <th>{_ Path _}</th>
                                        <th>{_ Views _}</th>
                                        <th>{_ Sessions _}</th>
                                        <th>{_ Users _}</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {% for page, views, sessions, users in pages %}
                                        <tr>
                                            <td>{{ page | escape }}</td>
                                            <td>{{ views }}</td>
                                            <td>{{ sessions }}</td>
                                            <td>{{ users }}</td>
                                        </tr>
                                    {% endfor %}
                                </tbody>
                            </table>
                        </div>
                    {% else %}
                        <p class="text-muted text-center">{_ No data available _}</p>
                    {% endif %}
                {% endwith %}
            </div>
        </div>
    </div>

    {# Popular Referrers - Right Column #}
    <div class="col-md-6">
        <div class="panel panel-default">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Popular Referrers _}</h3>
            </div>
            <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                {% with m.analytics.popular_referrers[id] as referrer %}
                    {% if referrer %}
                        <div class="table-responsive">
                            <table class="table table-striped table-hover table-condensed">
                                <thead>
                                    <tr>
                                        <th>{_ Referrer _}</th>
                                        <th>{_ Visits _}</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {% for referrer, visits in referrer %}
                                        <tr>
                                            <td>{{ referrer | escape }}</td>
                                            <td>{{ visits }}</td>
                                        </tr>
                                    {% endfor %}
                                </tbody>
                            </table>
                        </div>
                    {% else %}
                        <p class="text-muted text-center">{_ No data available _}</p>
                    {% endif %}
                {% endwith %}
            </div>
        </div>
    </div>
</div>

{# Access Log - Full Width #}
<div class="row">
    <div class="col-md-12">
        <div class="panel panel-default">
            <div class="panel-heading">
                <h3 class="panel-title">{_ Access Log _}</h3>
            </div>
            <div class="panel-body" style="max-height: 400px; overflow-y: auto;">
                {% with m.analytics.access_log[id] as log %}
                    {% if log %}
                        <div class="table-responsive">
                            <table class="table table-striped table-hover table-condensed" style="white-space:nowrap;">
                                <thead>
                                    <tr>
                                        <th>{_ Timestamp _}</th>
                                        <th>{_ Peer _}</th>
                                        <th>{_ Path _}</th>
                                        <th>{_ User _}</th>
                                        <th>{_ Session Id _}</th>
                                        <th>{_ Referrer _}</th>
                                        <th>{_ UA _}</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {% for req_version, req_method,
                                           resp_code, path, qs, referer,
                                           duration_total,
                                           peer_ip, session_id, user_id,
                                           language, timezone, user_agent,
                                           timestamp in log %}
                                        
                                        {# Show date separator when date changes #}
                                        {% if forloop.first or not timestamp|eq_day:forloop.previtem.13 %}
                                        <tr class="date-separator">
                                            <td colspan="7" style="background-color: #f5f5f5; font-weight: bold; padding: 8px 12px; border-top: 2px solid #ddd;">
                                                {{ timestamp | date:"l, Y-m-d" }}
                                            </td>
                                        </tr>
                                        {% endif %}
                                        
                                        <tr>
                                            <td>{{ timestamp | date:"H:i:s" }}</td>
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
                                </tbody>
                            </table>
                        </div>
                    {% else %}
                        <p class="text-muted text-center">{_ No data available _}</p>
                    {% endif %}
                {% endwith %}
            </div>
        </div>
    </div>
</div>
