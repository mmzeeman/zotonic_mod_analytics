{# Line Chart with optional grid and dots
   Parameters:
   - data: list of {label, value} tuples
   - title: chart title (optional)
   - width: chart width (default: 600)
   - height: chart height (default: 300)
   - color: line color (default: primary color)
   - show_dots: whether to show dots at data points (default: false)
   - show_grid: whether to show grid lines (default: true)
#}
{% with width|default:600 as chart_width %}
{% with height|default:300 as chart_height %}
{% with color|default:"#5bc0de" as line_color %}
{% with show_dots|default:0 as display_dots %}
{% with show_grid|default:1 as display_grid %}
{% with data|length as item_count %}
{% if item_count > 0 %}
    {% with data|element:2|max as max_val %}
    {% with data|element:2|min as min_val %}
    {% with chart_width - 80 as chart_area_width %}
    {% with chart_height - 60 as chart_area_height %}
    {% with max_val - min_val as value_range %}
    
    <div class="chart-container">
        {% if title %}<h4 class="chart-title">{{ title }}</h4>{% endif %}
        
        <svg class="chart-svg" 
             width="{{ chart_width }}" 
             height="{{ chart_height }}"
             viewBox="0 0 {{ chart_width }} {{ chart_height }}"
             xmlns="http://www.w3.org/2000/svg"
             role="img"
             aria-label="{{ title|default:_"Line chart" }}">
            
            {# Grid lines #}
            {% if display_grid and value_range > 0 %}
                {% for i in "01234" %}
                    {% with (i * chart_area_height) / 4 as grid_y %}
                    <line x1="50" 
                          y1="{{ grid_y }}" 
                          x2="{{ chart_area_width + 50 }}" 
                          y2="{{ grid_y }}"
                          class="chart-grid-line" />
                    <text x="45" 
                          y="{{ grid_y + 4 }}" 
                          class="chart-axis-text"
                          text-anchor="end">
                        {{ ((max_val - value_range) * i) / 4|round }}
                    </text>
                    {% endwith %}
                {% endfor %}
            {% endif %}
            
            {# Build path data #}
            {% with chart_area_width / item_count - 1 as x_spacing %}
            <path class="chart-line"
                  d="M {% for label, val in data %}{% with forloop.counter0 * x_spacing + 50 as x_pos %}{% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}{% with chart_area_height - y_scaled as y_pos %}{{ x_pos }},{{ y_pos }}{% if not forloop.last %} L {% endif %}{% endwith %}{% endwith %}{% endwith %}{% endfor %}"
                  stroke="{{ line_color }}"
                  stroke-width="2"
                  fill="none">
                <title>{{ title|default:_"Data trend" }}</title>
            </path>
            
            {# Dots at data points #}
            {% if display_dots %}
                {% for label, val in data %}
                    {% with forloop.counter0 * x_spacing + 50 as x_pos %}
                    {% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}
                    {% with chart_area_height - y_scaled as y_pos %}
                    <circle class="chart-dot"
                            cx="{{ x_pos }}"
                            cy="{{ y_pos }}"
                            r="4"
                            fill="{{ line_color }}">
                        <title>{{ label }}: {{ val }}</title>
                    </circle>
                    {% endwith %}
                    {% endwith %}
                    {% endwith %}
                {% endfor %}
            {% endif %}
            
            {# X-axis labels #}
            {% for label, val in data %}
                {% with forloop.counter0 * x_spacing + 50 as x_pos %}
                {% if forloop.counter0|divisibleby:item_count / 5|max:1 %}
                <text x="{{ x_pos }}" 
                      y="{{ chart_area_height + 20 }}" 
                      class="chart-axis-text"
                      text-anchor="middle"
                      transform="rotate(-45, {{ x_pos }}, {{ chart_area_height + 20 }})">
                    {% if label|length > 10 %}
                        {{ label|slice:[0,10] }}...
                    {% else %}
                        {{ label }}
                    {% endif %}
                </text>
                {% endif %}
                {% endwith %}
            {% endfor %}
            {% endwith %}
            
            {# X-axis #}
            <line x1="50" 
                  y1="{{ chart_area_height }}" 
                  x2="{{ chart_area_width + 50 }}" 
                  y2="{{ chart_area_height }}"
                  class="chart-axis-line" />
        </svg>
    </div>
    
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
    {% endwith %}
{% else %}
    <div class="chart-empty">
        <div class="chart-empty-icon">📈</div>
        <div class="chart-empty-text">{_ No data available _}</div>
    </div>
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
