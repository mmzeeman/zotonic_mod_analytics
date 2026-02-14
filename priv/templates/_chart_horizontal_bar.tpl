{# Horizontal Bar Chart
   Parameters:
   - data: list of {label, value} tuples
   - title: chart title (optional)
   - max_value: maximum value for scaling (optional, auto-calculated if not provided)
   - height: chart height in pixels (default: 400)
   - color: bar color (default: primary color)
   - show_values: whether to show values on bars (default: true)
#}
{% with height|default:400 as chart_height %}
{% with color|default:"#5bc0de" as bar_color %}
{% with show_values|default:1 as display_values %}
{% with data|length as item_count %}
{% if item_count > 0 %}
    {% with max_value|default:(data|element:2|max) as max_val %}
    {% with chart_height / item_count as bar_height_calc %}
    {% with bar_height_calc|min:50 as bar_height %}
    {% with bar_height * 0.7 as bar_thickness %}
    
    <div class="chart-container">
        {% if title %}<h4 class="chart-title">{{ title }}</h4>{% endif %}
        
        <svg class="chart-svg" 
             width="100%"
             height="{{ chart_height }}" 
             viewBox="0 0 800 {{ chart_height }}"
             xmlns="http://www.w3.org/2000/svg"
             role="img"
             aria-label="{{ title|default:_"Horizontal bar chart" }}">
            
            {% for label, val in data %}
                {% with forloop.counter0 * bar_height as y_pos %}
                {% with (val / max_val) * 550 as bar_width %}
                
                {# Bar group #}
                <g class="horizontal-bar-group">
                    {# Label #}
                    <text x="10" 
                          y="{{ y_pos + bar_height / 2 + 4 }}" 
                          class="horizontal-bar-label"
                          text-anchor="start"
                          font-size="12">
                        {% if label|length > 45 %}
                            {{ label|slice:[0,45] }}...
                            <title>{{ label }}</title>
                        {% else %}
                            {{ label }}
                        {% endif %}
                    </text>
                    
                    {# Bar #}
                    <rect class="horizontal-bar-rect"
                          x="200" 
                          y="{{ y_pos + (bar_height - bar_thickness) / 2 }}"
                          width="{{ bar_width }}"
                          height="{{ bar_thickness }}"
                          fill="{{ bar_color }}"
                          rx="2">
                        <title>{{ label }}: {{ val }}</title>
                    </rect>
                    
                    {# Value label #}
                    {% if display_values %}
                    <text x="{{ bar_width + 205 }}" 
                          y="{{ y_pos + bar_height / 2 + 4 }}" 
                          class="horizontal-bar-value"
                          text-anchor="start"
                          font-size="12">
                        {{ val }}
                    </text>
                    {% endif %}
                </g>
                
                {% endwith %}
                {% endwith %}
            {% endfor %}
        </svg>
    </div>
    
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
