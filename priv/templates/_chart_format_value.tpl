{% if format == "si" %}{{ value | format_si }}{% elif format == "percent" %}{{ (value * 100) | round }}%{% elif format == "duration" %}{{ value | format_duration_compact }}{% else %}{{ value }}{% endif %}

