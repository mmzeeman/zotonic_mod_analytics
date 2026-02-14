# Math Operators Conversion

This document shows the conversion from Zotonic template filter syntax to standard mathematical operators.

## Overview

All math filter expressions have been converted to use standard operators:
- `|add:` → `+`
- `|sub:` → `-`
- `|mul:` → `*`
- `|div:` → `/`

## Conversion Examples

### Simple Operations

**Before:**
```django
{% with chart_width|sub:80 as chart_area_width %}
{% with chart_height|sub:60 as chart_area_height %}
{% with bar_spacing|mul:0.7 as bar_width %}
```

**After:**
```django
{% with chart_width - 80 as chart_area_width %}
{% with chart_height - 60 as chart_area_height %}
{% with bar_spacing * 0.7 as bar_width %}
```

### Filter Chains with Proper Precedence

The pipe operator `|` has high precedence in templates, so filters evaluate left-to-right. When converting to standard operators, parentheses are added to preserve order of operations.

**Before:**
```django
{% with i|add:1|mul:chart_area_height|div:4 as grid_y %}
```

**After:**
```django
{% with ((i + 1) * chart_area_height) / 4 as grid_y %}
```

**Explanation:** 
- Original: `((i + 1) * chart_area_height) / 4`
- Without parens would be: `i + 1 * chart_area_height / 4` = `i + (1 * chart_area_height / 4)` ❌
- With proper parens: `((i + 1) * chart_area_height) / 4` ✓

### Complex Expressions

**Before:**
```django
{% with x_pos|add:bar_spacing|div:2|sub:bar_width|div:2 as x %}
{% with val|div:max_val|mul:chart_area_height as bar_height %}
{% with current_requests|sub:prev_requests|div:prev_requests|mul:100 as change %}
```

**After:**
```django
{% with ((x_pos + bar_spacing) / 2 - bar_width) / 2 as x %}
{% with (val / max_val) * chart_area_height as bar_height %}
{% with ((current_requests - prev_requests) / prev_requests) * 100 as change %}
```

### In Template Variables

**Before:**
```django
y="{{ grid_y|add:4 }}"
x="{{ x_pos|add:bar_spacing|div:2 }}"
```

**After:**
```django
y="{{ grid_y + 4 }}"
x="{{ (x_pos + bar_spacing) / 2 }}"
```

## Operator Precedence Rules

Standard mathematical operator precedence applies:
1. Parentheses `()` - highest precedence
2. Multiplication `*` and Division `/` - evaluated left-to-right
3. Addition `+` and Subtraction `-` - evaluated left-to-right

When converting filter chains, parentheses are added when:
- Addition or subtraction comes before multiplication or division
- Multiple operations need to maintain left-to-right evaluation

## Benefits

1. **More readable** - Standard math notation is universally understood
2. **Fewer characters** - `a + b` vs `a|add:b`
3. **Familiar syntax** - Matches standard programming language syntax
4. **Better tooling support** - IDEs can better parse standard operators

## Files Modified

- `priv/templates/_chart_bar.tpl`
- `priv/templates/_chart_horizontal_bar.tpl`
- `priv/templates/_chart_area.tpl`
- `priv/templates/_chart_line.tpl`
- `priv/templates/_chart_donut.tpl`
- `priv/templates/admin_analytics.tpl`

Total: 158 lines changed (79 insertions, 79 deletions)
