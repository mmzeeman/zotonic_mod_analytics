{# Time Range Selector
   Parameters:
   - active_range: currently active range (e.g., "7d", "30d", "90d")
#}
{% with active_range|default:"30d" as current_range %}
<div class="time-range-selector">
    <button class="time-range-btn {% if current_range == "1d" %}active{% endif %}" 
            disabled
            title="{_ Today _}">
        {_ Today _}
    </button>
    <button class="time-range-btn {% if current_range == "7d" %}active{% endif %}" 
            disabled
            title="{_ Last 7 days _}">
        {_ 7 days _}
    </button>
    <button class="time-range-btn {% if current_range == "30d" %}active{% endif %}" 
            disabled
            title="{_ Last 30 days _}">
        {_ 30 days _}
    </button>
    <button class="time-range-btn {% if current_range == "90d" %}active{% endif %}" 
            disabled
            title="{_ Last 90 days _}">
        {_ 90 days _}
    </button>
    <button class="time-range-btn" 
            disabled
            title="{_ Custom range (Coming soon) _}">
        {_ Custom _}
    </button>
</div>
<p class="text-muted" style="font-size: 12px; margin-top: -10px;">
    <em>{_ Note: Time range selection will be functional in a future update _}</em>
</p>
{% endwith %}
