{# Time Range Selector
   Parameters:
   - active_range: currently active range (e.g., "7d", "28d", "91d")
#}
{% with active_range|default:"28d" as current_range %}
<div class="time-range-selector">
    <a href="?range=7d" class="time-range-btn {% if current_range == "7d" %}active{% endif %}" 
       title="{_ Last 7 days _}">
        {_ Last 7 days _}
    </a>
    <a href="?range=28d" class="time-range-btn {% if current_range == "28d" %}active{% endif %}" 
       title="{_ Last 28 days _}">
        {_ Last 28 days _}
    </a>
    <a href="?range=91d" class="time-range-btn {% if current_range == "91d" %}active{% endif %}" 
       title="{_ Last 91 days _}">
        {_ Last 91 days _}
    </a>
</div>
{% endwith %}
