{% with values | length as length %}
{% with max | default:(values | max_value) as max %}
<svg style="{% if show_last %}padding-right: 2.4px; {% endif %}{% if show_first %}padding-left: 2.4px; {% endif %}overflow: visible"] 
     xmlns="http://www.w3.org/2000/svg"
     height="25px" width="80px"
     viewBox="0 0 {{ length - 1 }} {{ max }}" preserveAspectRatio="none">
    <path d="M{% for v in values %} {{ forloop.counter0 }} {{ max - v }}{% endfor %}"
          stroke-width="1.2"
          stroke="black"
          stroke-linecap="round"
          fill="transparent"
          vector-effect="non-scaling-stroke" />
    {% if show_first %}
        <line x1="{{ 0 }}" x2="{{ 0 }}.0001"
              y1="{{ max - (values | first) }}" y2="{{ max - (values | first) }}"
              stroke-width="4" stroke="red"
              stroke-linecap="round"
              vector-effect="non-scaling-stroke" />
    {% endif %}
    {% if show_last %}
        <line x1="{{ length - 1 }}" x2="{{ length - 1 }}.0001"
              y1="{{ max - (values | last) }}" y2="{{ max - (values | last) }}"
              stroke-width="4" stroke="red"
              stroke-linecap="round"
              vector-effect="non-scaling-stroke" />
    {% endif %}
</svg>
{% endwith %}
{% endwith %}
