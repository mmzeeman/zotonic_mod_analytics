{% include "_sparkline.tpl" values=values show_last=true %}
<span style="vertical-align: middle">{{ title }}</span>
<span style="vertical-align: middle"><strong style="color: red">{{ values | last }}</strong>{% if units %} <small>{{ units }}</small>{% endif %}</span>
