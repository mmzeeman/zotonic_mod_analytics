{# Time Range Selector
   Parameters:
   - active_range: currently active range as a binary/string (e.g., "7d", "28d", "91d")
#}
{% with active_range | default:"28d" as current_range %}
<div class="dropdown pull-right">
    <button class="btn btn-default dropdown-toggle" type="button" id="{{ #range }}" data-toggle="dropdown" aria-haspopup="true" aria-expanded="true">
        {% if current_range == "7d" %}
            {_ Last 7 days _}
        {% elif current_range == "28d" %}
            {_ Last 28 days _}
        {% elif current_range == "91d" %}
            {_ Last 91 days _}
        {% endif %}
    <span class="caret"></span>
  </button>
  <ul class="dropdown-menu" aria-labelledby="{{ #range }}">
      <li {% if current_range == "7d" %}class="active"{% endif %}><a href="{% url admin_analytics range="7d" view=active_view include_admin=q.include_admin include_bots=q.include_bots %}">{_ Last 7 days _}</a></li>
      <li {% if current_range == "28d" %}class="active"{% endif %}><a href="{% url admin_analytics range="28d" view=active_view include_admin=q.include_admin include_bots=q.include_bots %}">{_ Last 28 days _}</a></li>
      <li {% if current_range == "91d" %}class="active"{% endif %}><a href="{% url admin_analytics range="91d" view=active_view include_admin=q.include_admin include_bots=q.include_bots %}">{_ Last 91 days _}</a></li>
  </ul>
</div>
{% endwith %}
