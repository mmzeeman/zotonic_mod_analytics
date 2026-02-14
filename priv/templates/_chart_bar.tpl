{# Vertical Bar Chart
   Parameters:
   - data: list of {label, value} tuples
   - title: chart title (optional)
   - width: chart width (default: 600)
   - height: chart height (default: 300)
   - color: bar color (default: primary color)
   - show_values: whether to show values on top of bars (default: true)
   - show_grid: whether to show grid lines (default: true)
   - x_axis_label: label for x-axis (optional)
   - y_axis_label: label for y-axis (optional)
#}
{% with width|default:600 as chart_width %}
{% with height|default:300 as chart_height %}
{% with color|default:"#5bc0de" as bar_color %}
{% with show_values|default:1 as display_values %}
{% with show_grid|default:1 as display_grid %}
{% with data|length as item_count %}
{% if item_count > 0 %}
    {% with data|element:2|max as max_val %}
    {% with chart_width - 80 as chart_area_width %}
    {% with chart_height - 60 as chart_area_height %}
    {% with 20 as top_padding %}
    {% with chart_area_width / item_count as bar_spacing %}
    {% with bar_spacing * 0.7 as bar_width %}
    
    <div class="chart-container">
        {% if title %}<h4 class="chart-title">{{ title }}</h4>{% endif %}
        
        <svg class="chart-svg" 
             width="{{ chart_width }}" 
             height="{{ chart_height }}"
             viewBox="0 0 {{ chart_width }} {{ chart_height }}"
             xmlns="http://www.w3.org/2000/svg"
             role="img"
             aria-label="{{ title|default:_"Bar chart" }}">
            
            {# Grid lines and Y-axis ticks #}
            {% if display_grid and max_val > 0 %}
                {% for i in "0123" %}
                    {% with ((i + 1) * chart_area_height) / 4 as grid_y %}
                    <line x1="50" 
                          y1="{{ top_padding + grid_y }}" 
                          x2="{{ chart_area_width + 50 }}" 
                          y2="{{ top_padding + grid_y }}"
                          class="chart-grid-line" />
                    {# Y-axis tick mark #}
                    <line x1="45" 
                          y1="{{ top_padding + grid_y }}" 
                          x2="50" 
                          y2="{{ top_padding + grid_y }}"
                          class="chart-axis-line" />
                    <text x="43" 
                          y="{{ top_padding + grid_y + 4 }}" 
                          class="chart-axis-text"
                          text-anchor="end">
                        {{ ((max_val * 3 - max_val) * i) / 3|round }}
                    </text>
                    {% endwith %}
                {% endfor %}
            {% endif %}
            
            {# Bars #}
            {% for label, val in data %}
                {% with forloop.counter0 * bar_spacing + 50 as x_pos %}
                {% with (val / max_val) * chart_area_height as bar_height_calc %}
                {% with top_padding + chart_area_height - bar_height_calc as bar_y %}
                
                <g class="chart-bar-group">
                    {# Bar #}
                    <rect class="chart-bar"
                          x="{{ x_pos + (bar_spacing - bar_width) / 2 }}" 
                          y="{{ bar_y }}"
                          width="{{ bar_width }}"
                          height="{{ bar_height_calc }}"
                          fill="{{ bar_color }}"
                          rx="2">
                        <title>{{ label }}: {{ val }}</title>
                    </rect>
                    
                    {# Value on top #}
                    {% if display_values %}
                    <text x="{{ x_pos + bar_spacing / 2 }}" 
                          y="{{ bar_y - 5 }}" 
                          class="chart-value-text"
                          text-anchor="middle">
                        {{ val }}
                    </text>
                    {% endif %}
                    
                    {# Label at bottom and X-axis tick #}
                    {# X-axis tick mark #}
                    <line x1="{{ x_pos + bar_spacing / 2 }}" 
                          y1="{{ top_padding + chart_area_height }}" 
                          x2="{{ x_pos + bar_spacing / 2 }}" 
                          y2="{{ top_padding + chart_area_height + 5 }}"
                          class="chart-axis-line" />
                    <text x="{{ x_pos + bar_spacing / 2 }}" 
                          y="{{ top_padding + chart_area_height + 25 }}" 
                          class="chart-axis-text"
                          text-anchor="middle"
                          transform="rotate(-45, {{ x_pos + bar_spacing / 2 }}, {{ top_padding + chart_area_height + 25 }})">
                        {% if label_format %}
                            {{ label|date:label_format }}
                        {% else %}
                            {{ label }}
                        {% endif %}
                    </text>
                </g>
                
                {% endwith %}
                {% endwith %}
                {% endwith %}
            {% endfor %}
            
            {# Y-axis #}
            <line x1="50" 
                  y1="{{ top_padding }}" 
                  x2="50" 
                  y2="{{ top_padding + chart_area_height }}"
                  class="chart-axis-line" />
            
            {# X-axis #}
            <line x1="50" 
                  y1="{{ top_padding + chart_area_height }}" 
                  x2="{{ chart_area_width + 50 }}" 
                  y2="{{ top_padding + chart_area_height }}"
                  class="chart-axis-line" />
            
            {# Y-axis label #}
            {% if y_axis_label %}
            <text x="15" 
                  y="{{ top_padding + chart_area_height / 2 }}" 
                  class="chart-axis-label"
                  text-anchor="middle"
                  transform="rotate(-90, 15, {{ top_padding + chart_area_height / 2 }})">
                {{ y_axis_label }}
            </text>
            {% endif %}
            
            {# X-axis label #}
            {% if x_axis_label %}
            <text x="{{ chart_area_width / 2 + 50 }}" 
                  y="{{ top_padding + chart_area_height + 60 }}" 
                  class="chart-axis-label"
                  text-anchor="middle">
                {{ x_axis_label }}
            </text>
            {% endif %}
        </svg>
    </div>
    
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
{% else %}
    <div class="chart-empty">
        <div class="chart-empty-icon">📊</div>
        <div class="chart-empty-text">{_ No data available _}</div>
    </div>
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
