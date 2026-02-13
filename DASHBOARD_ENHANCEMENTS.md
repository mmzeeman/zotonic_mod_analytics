# Enhanced SVG-based Visualization Dashboard

This document describes the enhancements made to the Analytics Module dashboard.

## Overview

The analytics dashboard has been enhanced with beautiful, template-driven SVG visualizations to make data insights more accessible and visually appealing. All charts are generated using Zotonic templates without requiring JavaScript dependencies.

## New Files Added

### Chart Templates (`priv/templates/`)

1. **`_chart_line.tpl`** - Line Chart
   - Displays data trends over time with smooth paths
   - Supports grid lines and axis labels
   - Configurable dimensions and colors
   - Usage: `{% include "_chart_line.tpl" data=chart_data title="My Chart" %}`

2. **`_chart_bar.tpl`** - Vertical Bar Chart
   - Shows data as vertical bars
   - Includes value labels and grid
   - Supports custom colors
   - Usage: `{% include "_chart_bar.tpl" data=chart_data height=300 %}`

3. **`_chart_horizontal_bar.tpl`** - Horizontal Bar Chart
   - Perfect for rankings and comparisons
   - Truncates long labels with tooltips
   - Shows values on bar ends
   - Usage: `{% include "_chart_horizontal_bar.tpl" data=chart_data %}`

4. **`_chart_donut.tpl`** - Donut/Pie Chart
   - SVG path-based circular chart
   - Includes legend with color indicators
   - Center text showing total
   - Usage: `{% include "_chart_donut.tpl" data=colored_data show_legend=1 %}`

5. **`_chart_area.tpl`** - Area Chart
   - Filled area under line
   - Gradient fills for visual appeal
   - Similar to line chart but with fill
   - Usage: `{% include "_chart_area.tpl" data=chart_data %}`

6. **`_stat_card.tpl`** - Stat Card with Mini Chart
   - Large number display
   - Small sparkline trend indicator
   - Up/down arrow for change percentage
   - Usage: `{% include "_stat_card.tpl" value=1234 label="Requests" trend_data=data %}`

7. **`_time_range_selector.tpl`** - Time Range Filter
   - Buttons for different time ranges
   - Active state styling
   - Note: Currently UI-only, backend filtering to be added in future
   - Usage: `{% include "_time_range_selector.tpl" active_range="30d" %}`

### CSS Styling (`priv/lib/css/`)

**`analytics.css`** - Complete styling system
- CSS variables for color palette
- Chart container and SVG styles
- Hover effects and animations
- Responsive breakpoints for mobile/tablet/desktop
- Collapsible panel styles
- Empty state styling

## Enhanced Analytics Model Functions

Added 6 new query functions to `src/models/m_analytics.erl`:

1. **`hourly_traffic/1, hourly_traffic/3`**
   - Returns traffic by hour for the last 24 hours
   - Format: `[{hour, request_count}, ...]`
   - Template usage: `m.analytics.hourly_traffic`

2. **`response_time_distribution/1, response_time_distribution/3`**
   - Returns response time distribution in buckets
   - Buckets: <100ms, 100-500ms, 500ms-1s, 1s-3s, >3s
   - Template usage: `m.analytics.response_time_distribution`

3. **`error_breakdown/1, error_breakdown/3`**
   - Returns error breakdown by type (4xx vs 5xx)
   - Format: `[{"4xx Client Errors", count}, {"5xx Server Errors", count}]`
   - Template usage: `m.analytics.error_breakdown`

4. **`traffic_sources/1, traffic_sources/3`**
   - Returns top 10 traffic sources/referrers
   - Format: `[{source_url, request_count}, ...]`
   - Template usage: `m.analytics.traffic_sources`

5. **`session_duration_distribution/1, session_duration_distribution/3`**
   - Returns session duration distribution in buckets
   - Buckets: <10s, 10s-1m, 1m-5m, 5m-30m, >30m
   - Template usage: `m.analytics.session_duration_distribution`

6. **`traffic_by_hour_of_day/1, traffic_by_hour_of_day/3`**
   - Returns aggregate traffic pattern by hour of day (0-23)
   - Format: `[{hour, request_count, session_count}, ...]`
   - Template usage: `m.analytics.traffic_by_hour_of_day`

All new functions follow the existing pattern:
- `/1` version uses default 30-day range
- `/3` version accepts `From`, `Until`, and `Context` parameters
- Include bot filtering using `no_bots_clause()`

## Enhanced Dashboard Layout

The dashboard (`priv/templates/admin_analytics.tpl`) has been reorganized:

### Top Section - Key Metrics Cards
4 stat cards displaying:
- Total Requests (with 7-day trend)
- Sessions (with trend)
- Unique Visitors (with trend)
- Total Errors (combined client + server)

### Main Visualizations Grid
- **Traffic by Hour of Day** (full width) - Bar chart showing hourly patterns
- **Response Time Distribution** (half width) - Horizontal bar chart
- **Error Breakdown** (half width) - Donut chart showing 4xx vs 5xx
- **Top Traffic Sources** (full width) - Horizontal bar chart of referrers

### Enhanced Data Sections
- **Unique Visitors** - Kept existing bar visualization
- **Popular Pages** - Now shows horizontal bar chart with collapsible detailed table
- **Popular Resources** - Now shows horizontal bar chart with collapsible detailed table

### Collapsible Detail Sections
- **User Activity** - Detailed table (collapsed by default)
- **Dispatch Rule Health** - Detailed table (collapsed by default)

## Technical Details

### Chart Data Format

Most charts expect data in a simple tuple format:

```erlang
% For horizontal/vertical bar charts and line charts
[{Label, Value}, ...]

% Example
[{"Page A", 150}, {"Page B", 120}, {"Page C", 90}]
```

For donut charts, data includes color:

```erlang
% For donut charts
[{Label, Value, Color}, ...]

% Example
[{"4xx Errors", 45, "#f0ad4e"}, {"5xx Errors", 12, "#d9534f"}]
```

### Color Scheme

The CSS defines a consistent color palette:
- Primary: `#5bc0de` (light blue)
- Success: `#5cb85c` (green)
- Warning: `#f0ad4e` (orange)
- Danger: `#d9534f` (red)
- Info: `#5bc0de` (light blue)

### Responsive Breakpoints

- Desktop (≥768px): 2-column grid for charts
- Tablet/Mobile (<768px): Single column stack
- Stat cards: Auto-fit with minimum 200px width

### Browser Compatibility

All visualizations use standard SVG and CSS3:
- Modern browsers: Full support
- Older browsers: Graceful degradation
- No JavaScript required

## Usage Examples

### Basic Stat Card

```django
{% include "_stat_card.tpl" 
    value=12345 
    label=_"Total Views"
    icon="👁️" %}
```

### Stat Card with Trend

```django
{% with m.analytics.stats_overview | values:2 as requests %}
{% include "_stat_card.tpl" 
    value=requests|last
    label=_"Requests"
    trend_data=requests
    change_percent=5.2
    icon="📊" %}
{% endwith %}
```

### Bar Chart

```django
{% with m.analytics.traffic_by_hour_of_day as data %}
{% with data|map:0|zip:data|map:1 as chart_data %}
{% include "_chart_bar.tpl" 
    data=chart_data 
    title=_"Hourly Traffic"
    height=300
    show_grid=1 %}
{% endwith %}
{% endwith %}
```

### Horizontal Bar Chart

```django
{% with m.analytics.popular_pages as pages %}
{% include "_chart_horizontal_bar.tpl" 
    data=pages|map:0|zip:pages|map:1
    title=_"Popular Pages"
    color="#5bc0de" %}
{% endwith %}
```

### Donut Chart

```django
{% with m.analytics.error_breakdown as errors %}
{% with [
    [errors|first|first, errors|first|last, "#f0ad4e"],
    [errors|last|first, errors|last|last, "#d9534f"]
] as colored_data %}
{% include "_chart_donut.tpl" 
    data=colored_data 
    title=_"Error Types"
    show_legend=1 %}
{% endwith %}
{% endwith %}
```

## Backward Compatibility

All existing functionality has been preserved:
- Original sparkline templates still available
- Existing model functions unchanged
- All existing data structures maintained
- Tables still available (collapsible in some cases)

## Performance Considerations

- SVG renders client-side but is generated server-side
- No JavaScript processing overhead
- Efficient DuckDB queries with proper filtering
- Bot traffic excluded from most queries
- Results limited to reasonable sizes (e.g., top 10)

## Future Enhancements

Potential improvements for future versions:
1. Time range selector with backend filtering
2. Drill-down capabilities on chart clicks
3. Export functionality (CSV, PDF)
4. Real-time updates with websockets
5. Custom date range picker
6. Comparison views (this period vs last period)
7. More chart types (scatter, heat map)
8. Dashboard customization/widgets

## Testing

To test the visualizations:
1. Ensure the module is loaded: `mod_analytics` is active
2. Navigate to Admin → Analytics
3. Verify charts render with your data
4. Test responsive behavior by resizing browser
5. Check empty states by filtering to date ranges with no data

## Troubleshooting

**Charts not appearing:**
- Verify CSS is loaded: `{% lib "css/analytics.css" %}`
- Check browser console for errors
- Ensure data format is correct (tuples expected)

**SQL errors in logs:**
- Verify DuckDB connection
- Check query syntax in m_analytics.erl
- Ensure bot filtering clause is properly concatenated

**Empty charts:**
- Check if data exists for the selected range
- Verify model functions are accessible via m.analytics.*
- Look for error logs in Zotonic

## Credits

This enhancement follows Zotonic best practices and maintains the existing analytics module architecture while adding modern visualization capabilities.
