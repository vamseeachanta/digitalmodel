# Agent Health Monitoring Dashboard

Interactive Plotly dashboard for monitoring agent performance metrics across all 54+ agents in the digitalmodel ecosystem.

## Overview

The Agent Dashboard provides comprehensive visualizations for:
- **Success rate gauges** by agent category
- **Load distribution** showing task counts per agent
- **Failure trends** over time with daily aggregation
- **Cost analysis** by agent category

## Features

### Interactive Visualizations
- All charts use **Plotly** for interactive hover, zoom, and pan
- NO static matplotlib exports (follows HTML_REPORTING_STANDARDS.md)
- Responsive HTML reports with embedded CSS styling

### Data Source
- Imports metrics from CSV files with relative paths
- Schema: `timestamp, agent_name, task_type, success, duration_ms, tokens_used, cost_usd, error_type`
- Configurable date range filtering (default: last 30 days)

### Agent Categories
Monitors all 54 agents organized into 9 categories:
- Core Development (5 agents)
- Swarm Coordination (5 agents)
- Consensus & Distributed (7 agents)
- Performance & Optimization (5 agents)
- GitHub & Repository (9 agents)
- SPARC Methodology (6 agents)
- Specialized Development (8 agents)
- Testing & Validation (2 agents)
- Migration & Planning (2 agents)

## Installation

The dashboard module is already integrated into digitalmodel:

```bash
# No additional installation needed
# Dependencies: pandas, plotly, numpy
```

## Usage

### Command Line Interface

```bash
# Generate dashboard from current data
python scripts/agent_dashboard_cli.py generate

# Generate with custom date range
python scripts/agent_dashboard_cli.py generate --start 2026-01-01 --end 2026-01-06

# View dashboard in browser
python scripts/agent_dashboard_cli.py view

# Generate and view in one command
python scripts/agent_dashboard_cli.py generate && python scripts/agent_dashboard_cli.py view
```

### Python API

```python
from digitalmodel.visualization.agent_dashboard import (
    AgentDashboard,
    generate_dashboard,
    view_dashboard
)

# Generate dashboard
output_path = generate_dashboard(
    data_dir="data/agent_metrics",
    output_dir="reports",
    start_date="2025-12-08",  # Optional
    end_date="2026-01-07"      # Optional
)

# View in browser
view_dashboard("reports/agent_health_dashboard.html")

# Advanced usage with class
dashboard = AgentDashboard(
    data_dir=Path("data/agent_metrics"),
    output_dir=Path("reports")
)
dashboard.load_data("agent_metrics.csv")
df_filtered = dashboard.filter_by_date(start_date, end_date)
dashboard.generate_dashboard()
```

## Data Format

### CSV Schema

```csv
timestamp,agent_name,task_type,success,duration_ms,tokens_used,cost_usd,error_type
2026-01-01 10:00:00,coder,code_generation,True,1500.5,1000,0.01,
2026-01-01 11:00:00,reviewer,code_review,False,3000.1,2000,0.02,timeout
```

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `timestamp` | datetime | When the task was executed |
| `agent_name` | string | Name of the agent (must match CLAUDE.md) |
| `task_type` | string | Type of task performed |
| `success` | boolean | Whether task succeeded |
| `duration_ms` | float | Task duration in milliseconds |
| `tokens_used` | int | Number of tokens consumed |
| `cost_usd` | float | Cost in USD |
| `error_type` | string | Error type if failed (empty if successful) |

## Directory Structure

```
data/
└── agent_metrics/
    └── agent_metrics.csv          # Metrics data

reports/
└── agent_health_dashboard.html    # Generated dashboard

scripts/
├── generate_sample_metrics.py     # Sample data generator
└── agent_dashboard_cli.py         # CLI wrapper

src/digitalmodel/modules/visualization/
├── __init__.py
└── agent_dashboard.py             # Main dashboard module

tests/
└── test_agent_dashboard.py        # Comprehensive tests
```

## Dashboard Sections

### 1. Success Rate Gauges
- Gauge chart for each agent category
- Shows percentage success rate
- Color-coded thresholds:
  - Red (0-60%): Poor performance
  - Yellow (60-80%): Below target
  - Light green (80-95%): Good
  - Green (95-100%): Excellent
- Displays total task count per category

### 2. Load Distribution
- Horizontal bar chart of task counts per agent
- Color-coded by agent category
- Sorted by task count (ascending)
- Interactive hover shows category details

### 3. Failure Trends
- Dual-axis line chart over time
- Primary axis: Daily failure count (red line)
- Secondary axis: Failure rate percentage (orange dashed line)
- Aggregated by day for 30-day view

### 4. Cost Analysis
- Donut pie chart showing cost distribution
- Grouped by agent category
- Shows percentage and absolute cost ($USD)
- Interactive hover with detailed breakdown

### 5. Summary Statistics
- Total tasks executed
- Overall success rate
- Total cost
- Average task duration

## Sample Data Generation

Generate realistic sample data for testing:

```bash
python scripts/generate_sample_metrics.py
```

This creates 30 days of sample data for all 54 agents with:
- Realistic success rate distributions (85-98% for most, 60-80% for some)
- Variable task counts (80-200 per day)
- Duration and cost based on agent profiles
- Random error types for failures

## Testing

Run comprehensive test suite:

```bash
# All tests
pytest tests/test_agent_dashboard.py -v

# Specific test class
pytest tests/test_agent_dashboard.py::TestAgentDashboard -v

# With coverage
pytest tests/test_agent_dashboard.py --cov=src/digitalmodel/modules/visualization
```

Test coverage includes:
- Data loading and filtering
- Visualization generation
- HTML report creation
- Error handling
- Data validation
- Convenience functions

## Configuration

### Date Range Filtering

```python
# Default: Last 30 days
dashboard.filter_by_date()

# Custom range
from datetime import datetime
start = datetime(2026, 1, 1)
end = datetime(2026, 1, 7)
filtered_df = dashboard.filter_by_date(start, end)
```

### Output Customization

```python
# Custom output file
dashboard.generate_dashboard(output_file="custom_report.html")

# Custom directories
dashboard = AgentDashboard(
    data_dir=Path("custom/data/path"),
    output_dir=Path("custom/output/path")
)
```

## Performance

- **Data loading**: Fast CSV parsing with pandas
- **Visualization**: Client-side rendering with Plotly.js
- **File size**: ~500KB HTML (includes Plotly CDN)
- **Browser compatibility**: Modern browsers with JavaScript enabled

## Standards Compliance

Follows **HTML_REPORTING_STANDARDS.md**:
- ✅ Interactive plots only (Plotly)
- ✅ NO static matplotlib exports
- ✅ CSV data with relative paths
- ✅ Professional HTML styling
- ✅ Responsive design
- ✅ Embedded CSS (no external dependencies except Plotly CDN)

## Troubleshooting

### "Dashboard not found" Error
```bash
# Generate dashboard first
python scripts/agent_dashboard_cli.py generate
```

### "No data in selected date range"
```bash
# Check CSV file exists
ls data/agent_metrics/agent_metrics.csv

# Generate sample data if needed
python scripts/generate_sample_metrics.py
```

### Encoding Issues on Windows
All emoji characters have been removed from output for Windows compatibility.

## Future Enhancements

Phase 2 features (not included in initial version):
- Real-time auto-refresh
- Drill-down into specific failures
- Export to PDF/CSV
- Agent comparison view
- Predictive analytics
- Alert thresholds
- Email notifications

## API Reference

See docstrings in `agent_dashboard.py` for complete API documentation:

```python
help(AgentDashboard)
help(generate_dashboard)
help(view_dashboard)
```

## License

Part of the digitalmodel project. Same license applies.

## Contact

For issues or feature requests, see main repository documentation.
