{# Area Chart with gradient fill
   Parameters:
   - data: list of {label, value} tuples
   - title: chart title (optional)
   - width: chart width (default: 600)
   - height: chart height (default: 300)
   - gradient_colors: array of [start_color, end_color] (default: ["#5bc0de", "#5bc0de"])
#}
{% with width|default:600 as chart_width %}
{% with height|default:300 as chart_height %}
{% with gradient_colors|default:"#5bc0de,#3a9cb8"|split:"," as colors %}
{% with colors|first as start_color %}
{% with colors|last as end_color %}
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
             aria-label="{{ title|default:_"Area chart" }}">
            
            {# Gradient definition #}
            <defs>
                <linearGradient id="areaGradient" x1="0%" y1="0%" x2="0%" y2="100%">
                    <stop offset="0%" style="stop-color:{{ start_color }};stop-opacity:0.6" />
                    <stop offset="100%" style="stop-color:{{ end_color }};stop-opacity:0.1" />
                </linearGradient>
            </defs>
            
            {# Grid lines #}
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
            
            {# Build area path #}
            {% with chart_area_width / item_count - 1 as x_spacing %}
            <path class="chart-area"
                  d="M 50,{{ chart_area_height }}
                     {% for label, val in data %}
                        {% with forloop.counter0 * x_spacing + 50 as x_pos %}
                        {% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}
                        {% with chart_area_height - y_scaled as y_pos %}
                        L {{ x_pos }},{{ y_pos }}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                     {% endfor %}
                     L {{ chart_area_width + 50 }},{{ chart_area_height }}
                     Z"
                  fill="url(#areaGradient)">
                <title>{{ title|default:_"Area data" }}</title>
            </path>
            
            {# Line on top of area #}
            <path class="chart-line"
                  d="M {% for label, val in data %}{% with forloop.counter0 * x_spacing + 50 as x_pos %}{% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}{% with chart_area_height - y_scaled as y_pos %}{{ x_pos }},{{ y_pos }}{% if not forloop.last %} L {% endif %}{% endwith %}{% endwith %}{% endwith %}{% endfor %}"
                  stroke="{{ start_color }}"
                  stroke-width="2"
                  fill="none" />
            
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
