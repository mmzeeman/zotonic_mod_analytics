{# Stat Card with Mini Chart/Sparkline
   Parameters:
   - value: main number to display
   - label: description text
   - trend_data: array of values for sparkline (optional)
   - change_percent: percentage change (optional, can be positive/negative)
   - icon: icon text/symbol (optional)
#}
<div class="stat-card">
    <div class="stat-card-header">
        <div style="flex: 1;">
            <div class="stat-card-label">{{ label }}</div>
            <div class="stat-card-value">{{ value | default:0 }}</div>
            {% if change_percent %}
                {% if change_percent > 0 %}
                    <span class="stat-card-change positive">
                        ↑ +{{ change_percent }}%
                    </span>
                {% elif change_percent < 0 %}
                    <span class="stat-card-change negative">
                        ↓ {{ change_percent }}%
                    </span>
                {% else %}
                    <span class="stat-card-change neutral">
                        → 0%
                    </span>
                {% endif %}
            {% endif %}
        </div>
        {% if icon %}
        <div class="stat-card-icon">{{ icon }}</div>
        {% endif %}
    </div>
    
    {% if trend_data %}
    <div class="stat-card-trend">
        {% include "_sparkline.tpl" values=trend_data show_last=1 %}
    </div>
    {% endif %}
</div>
