{% with values | length as length %}
<svg xmlns="http://www.w3.org/2000/svg" height="25px" width="80px" viewBox="0 0 {{ length + 1 }} {{ max }}" preserveAspectRatio="none">
  <path d="M{% for v in values %} {{ forloop.counter }} {{ max - v }}{% endfor %}"
        stroke-width="1"
        stroke="black"
        fill="transparent"
        vector-effect="non-scaling-stroke" />
  {% if show_last %}
  <line x1="{{ length  }}"
        x2="{{ length  }}.0001"
        y1="{{ max - (values | last) }}"
        y2="{{ max - (values | last) }}"
        stroke-width="2"
        stroke="red"
        stroke-linecap="round"
        vector-effect="non-scaling-stroke" />
  {% endif %}
    <line x1="{{ 1 }}"
          x2="{{ 1 }}.0001"
          y1="{{ max - (values | first) }}"
          y2="{{ max - (values | first) }}"
          stroke-width="2"
          stroke="red"
          stroke-linecap="round"
          vector-effect="non-scaling-stroke" />
</svg>
{% endwith %}
