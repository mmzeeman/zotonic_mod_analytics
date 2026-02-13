{# Donut/Pie Chart
   Parameters:
   - data: list of {label, value, color} tuples
   - title: chart title (optional)
   - size: chart size (default: 300)
   - show_legend: whether to show legend (default: true)
#}
{% with size|default:300 as chart_size %}
{% with show_legend|default:1 as display_legend %}
{% with data|length as item_count %}
{% if item_count > 0 %}
    {% with data|map:"1"|sum as total_value %}
    {% if total_value > 0 %}
    {% with chart_size / 2 as center %}
    {% with center * 0.8 as radius %}
    {% with center * 0.5 as inner_radius %}
    
    <div class="chart-container">
        {% if title %}<h4 class="chart-title">{{ title }}</h4>{% endif %}
        
        <svg class="chart-svg" 
             width="{{ chart_size }}" 
             height="{{ chart_size }}"
             viewBox="0 0 {{ chart_size }} {{ chart_size }}"
             xmlns="http://www.w3.org/2000/svg"
             role="img"
             aria-label="{{ title|default:_"Donut chart" }}">
            
            {# Calculate and draw segments #}
            {% with 0 as current_angle %}
            {% for label, val, segment_color in data %}
                {% with (val / total_value) * 360 as angle %}
                {% with current_angle + angle as end_angle %}
                
                {# Calculate arc path - using approximate circle math #}
                {% with current_angle * 0.0174533 as start_rad %}
                {% with end_angle * 0.0174533 as end_rad %}
                {% with (center + radius) * start_rad|sin as start_x %}
                {% with (center - radius) * start_rad|cos as start_y %}
                {% with (center + radius) * end_rad|sin as end_x %}
                {% with (center - radius) * end_rad|cos as end_y %}
                {% with (center + inner_radius) * start_rad|sin as inner_start_x %}
                {% with (center - inner_radius) * start_rad|cos as inner_start_y %}
                {% with (center + inner_radius) * end_rad|sin as inner_end_x %}
                {% with (center - inner_radius) * end_rad|cos as inner_end_y %}
                {% with angle|ge:180 as large_arc %}
                
                <path class="chart-donut-segment"
                      d="M {{ start_x }} {{ start_y }}
                         A {{ radius }} {{ radius }} 0 {% if large_arc %}1{% else %}0{% endif %} 1 {{ end_x }} {{ end_y }}
                         L {{ inner_end_x }} {{ inner_end_y }}
                         A {{ inner_radius }} {{ inner_radius }} 0 {% if large_arc %}1{% else %}0{% endif %} 0 {{ inner_start_x }} {{ inner_start_y }}
                         Z"
                      fill="{{ segment_color|default:"#5bc0de" }}"
                      stroke="white"
                      stroke-width="2">
                    <title>{{ label }}: {{ val }} ({{ (val / total_value) * 100|round }}%)</title>
                </path>
                
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                {% endwith %}
                
                {% with end_angle as current_angle %}
                {% endwith %}
            {% endfor %}
            {% endwith %}
            
            {# Center text showing total #}
            <text x="{{ center }}" 
                  y="{{ center - 5 }}" 
                  class="chart-value-text"
                  text-anchor="middle"
                  font-size="24"
                  font-weight="bold">
                {{ total_value }}
            </text>
            <text x="{{ center }}" 
                  y="{{ center + 15 }}" 
                  class="chart-axis-text"
                  text-anchor="middle"
                  font-size="12">
                {_ Total _}
            </text>
        </svg>
        
        {# Legend #}
        {% if display_legend %}
        <div class="chart-legend">
            {% for label, val, segment_color in data %}
            <div class="chart-legend-item">
                <div class="chart-legend-color" style="background-color: {{ segment_color|default:"#5bc0de" }}"></div>
                <span class="chart-legend-label">{{ label }}</span>
                <span class="chart-legend-value">({{ val }})</span>
            </div>
            {% endfor %}
        </div>
        {% endif %}
    </div>
    
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endif %}
    {% endwith %}
{% else %}
    <div class="chart-empty">
        <div class="chart-empty-icon">🍩</div>
        <div class="chart-empty-text">{_ No data available _}</div>
    </div>
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
