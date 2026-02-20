{# Area Chart with gradient fill
   Parameters:
   - data: list of {label, value} tuples
   - title: chart title (optional)
   - width: chart width (default: 600)
   - height: chart height (default: 300)
   - gradient_colors: array of [start_color, end_color] (default: ["#5bc0de", "#3a9cb8"])
   - x_axis_label: label for x-axis (optional)
   - y_axis_label: label for y-axis (optional)
   - y_tick_count: Number of ticks on the y-axis, excluding the origin (optiona)
#}
{% with width | default:600 as chart_width %}
{% with height | default:300 as chart_height %}
{% with gradient_colors | default:["#5bc0de", "#3a9cb8"] as colors %}
{% with colors[1] as start_color %}
{% with colors[2] as end_color %}
{% with y_tick_count | default:5 as y_tick_count  %}
{% if data | length as item_count %}
    {% with data | element:2 | max as max_val %}
    {% with data | element:2 | min as min_val %}
    {# Round max to nice value for better axis labels #}
    {% with max_val | nice_round as nice_max %}
    {% with chart_width - 80 as chart_area_width %}
    {% with chart_height - 60 as chart_area_height %}
    {% with 20 as top_padding %}
    {% with nice_max - min_val as value_range %}

    <div class="chart-container">
        {% if title %}<h4 class="chart-title">{{ title }}</h4>{% endif %}
        
        <svg class="chart-svg" 
             width="{{ chart_width }}" 
             height="{{ chart_height }}"
             viewBox="0 0 {{ chart_width }} {{ chart_height }}"
             xmlns="http://www.w3.org/2000/svg"
             role="img"
             aria-label="{{ title | default:_"Area chart" }}">
            
            {# Gradient definition #}
            <defs>
               <linearGradient id="{{ #gradient }}" x1="0%" y1="0%" x2="0%" y2="100%">
                   <stop offset="0%" style="stop-color:{{ start_color }};stop-opacity:0.6" />
                   <stop offset="100%" style="stop-color:{{ end_color }};stop-opacity:0.1" />
                </linearGradient>
            </defs>
            
            {# Grid lines and Y-axis ticks with better spacing #}
            {% for i in (0 | range:y_tick_count) %}
                {% with (i * chart_area_height) / y_tick_count as grid_y %}
                {# Calculate tick value - nice_max at top (y=0), min at bottom #}
                {% with nice_max - (i * nice_max) / y_tick_count  as tick_value %}
                <line x1="50" y1="{{ top_padding + grid_y }}" 
                      x2="{{ chart_area_width + 50 }}" y2="{{ top_padding + grid_y }}"
                      class="chart-grid-line" 
                      stroke="#d0d0d0" 
                      stroke-width="1"
                      opacity="0.7" />
                {# Y-axis tick mark #}
                <line x1="45" y1="{{ top_padding + grid_y }}" 
                      x2="50" y2="{{ top_padding + grid_y }}"
                      class="chart-axis-line" />
                <text x="43" y="{{ top_padding + grid_y + 4 }}" 
                      class="chart-axis-text"
                      text-anchor="end"
                      font-size="11">
                    {% with tick_value | round as rounded_tick_value %} 
                    {% if ((tick_value - rounded_tick_value) | abs) < 0.01 %}{{ rounded_tick_value }}{% endif %}
                    {% endwith %}
                </text>
                {% endwith %}
                {% endwith %}
            {% endfor %}
            
            {# Build area path #}
            {% with chart_area_width / item_count - 1 as x_spacing %}
            {# Calculate last data point x position #}
            {% with (item_count - 1) * x_spacing + 50 as last_x %}
            <path class="chart-area"
                  d="M 50,{{ top_padding + chart_area_height }}
                     {% for label, val in data %}
                        {% with forloop.counter0 * x_spacing + 50 as x_pos %}
                        {% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}
                        {% with chart_area_height - y_scaled as y_pos %}
                        L {{ x_pos }},{{ top_padding + y_pos }}
                        {% endwith %}
                        {% endwith %}
                        {% endwith %}
                     {% endfor %}
                     L {{ last_x }},{{ top_padding + chart_area_height }}
                     Z"
                  fill="url(#{{ #gradient }})">
                <title>{{ title | default:_"Area data" }}</title>
            </path>
            
            {# Line on top of area #}
            <path class="chart-line"
                  d="M {% for label, val in data %}{% with forloop.counter0 * x_spacing + 50 as x_pos %}{% with ((val - min_val) / value_range) * chart_area_height as y_scaled %}{% with chart_area_height - y_scaled as y_pos %}{{ x_pos }},{{ top_padding + y_pos }}{% if not forloop.last %} L {% endif %}{% endwith %}{% endwith %}{% endwith %}{% endfor %}"
                  stroke="{{ start_color }}"
                  stroke-width="2"
                  fill="none" />
            
            {# X-axis labels and ticks with better spacing #}
            {# Calculate optimal tick interval - aim for 8-10 ticks maximum #}
            {% with (item_count / 8) | max:1 | round as tick_interval %}
            {% for label, val in data %}
                {% with forloop.counter0 * x_spacing + 50 as x_pos %}
                {% if (forloop.counter0 | divisibleby:tick_interval) or forloop.first or forloop.last %}
                {# X-axis tick mark #}
                <line x1="{{ x_pos }}" 
                      y1="{{ top_padding + chart_area_height }}" 
                      x2="{{ x_pos }}" 
                      y2="{{ top_padding + chart_area_height + 5 }}"
                      class="chart-axis-line" />
                 <text x="{{ x_pos }}" 
                       y="{{ top_padding + chart_area_height + 20 }}" 
                       class="chart-axis-text"
                       text-anchor="middle"
                       font-size="10"
                       transform="rotate(-45, {{ x_pos }}, {{ top_padding + chart_area_height + 20 }})">
                     {{ label | date:"j M" }}
                 </text>
                {% endif %}
                {% endwith %}
            {% endfor %}
            {% endwith %}
            {% endwith %}
            {% endwith %}
            
            {# Y-axis #}
            <line x1="50" y1="{{ top_padding }}" 
                  x2="50" y2="{{ top_padding + chart_area_height }}"
                  class="chart-axis-line" />
            
            {# X-axis #}
            <line x1="50" y1="{{ top_padding + chart_area_height }}" 
                  x2="{{ chart_area_width + 50 }}" y2="{{ top_padding + chart_area_height }}"
                  class="chart-axis-line" />
            
            {# Y-axis label #}
            {% if y_axis_label %}
            <text x="15" y="{{ top_padding + chart_area_height / 2 }}" 
                  class="chart-axis-label"
                  text-anchor="middle"
                  transform="rotate(-90, 15, {{ top_padding + chart_area_height / 2 }})">
                {{ y_axis_label }}
            </text>
            {% endif %}
            
            {# X-axis label #}
            {% if x_axis_label %}
            <text x="{{ chart_area_width / 2 + 50 }}" 
                  y="{{ top_padding + chart_area_height + 50 }}" 
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
