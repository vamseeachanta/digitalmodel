# Time Tracking Integration Skill

> **Quick Reference Guide**

## Overview

Time tracking integration patterns with RescueTime and Toggl Track APIs for automated time entry, reporting, analytics, and project/task attribution.

**Category**: Productivity
**Version**: 1.0.0
**Platforms**: REST API, Python

## Quick Start

### Toggl Track Setup

```bash
# Get API token from: https://track.toggl.com/profile
export TOGGL_API_TOKEN="your-api-token"

# Verify
curl -s -u "$TOGGL_API_TOKEN:api_token" \
    "https://api.track.toggl.com/api/v9/me" | jq '.fullname'
```

### RescueTime Setup

```bash
# Get API key from: https://www.rescuetime.com/anapi/manage
export RESCUETIME_API_KEY="your-api-key"

# Verify
curl -s "https://www.rescuetime.com/anapi/data?key=$RESCUETIME_API_KEY&format=json"
```

### Python Dependencies

```bash
pip install requests python-dateutil pandas
```

## Key Features

| Feature | Toggl | RescueTime |
|---------|-------|------------|
| Time Entries | CRUD operations | Read-only |
| Projects | Full management | N/A |
| Reporting | Summary, detailed | Productivity, categories |
| Focus Time | Manual tracking | Automatic tracking |
| Webhooks | No | No |

## Toggl Track Examples

### Start/Stop Timer

```python
import requests
import os

API_TOKEN = os.environ["TOGGL_API_TOKEN"]
AUTH = (API_TOKEN, "api_token")
BASE_URL = "https://api.track.toggl.com/api/v9"

# Start timer
response = requests.post(
    f"{BASE_URL}/workspaces/WORKSPACE_ID/time_entries",
    auth=AUTH,
    json={
        "description": "Working on project",
        "workspace_id": WORKSPACE_ID,
        "start": "2025-01-15T09:00:00.000Z",
        "duration": -1,  # Running timer
        "created_with": "api"
    }
)

# Stop timer
requests.patch(
    f"{BASE_URL}/workspaces/WORKSPACE_ID/time_entries/ENTRY_ID/stop",
    auth=AUTH
)
```

### Get Time Entries

```bash
# Get recent entries
curl -s -u "$TOGGL_API_TOKEN:api_token" \
    "https://api.track.toggl.com/api/v9/me/time_entries"

# With date range
curl -s -u "$TOGGL_API_TOKEN:api_token" \
    "https://api.track.toggl.com/api/v9/me/time_entries?start_date=2025-01-01&end_date=2025-01-31"
```

## RescueTime Examples

### Get Productivity Data

```python
import requests
import os

API_KEY = os.environ["RESCUETIME_API_KEY"]

response = requests.get(
    "https://www.rescuetime.com/anapi/data",
    params={
        "key": API_KEY,
        "format": "json",
        "restrict_kind": "productivity"
    }
)

data = response.json()
```

### Calculate Focus Time

```python
def get_focus_time(api_key, start_date, end_date):
    """Calculate productive focus time."""
    response = requests.get(
        "https://www.rescuetime.com/anapi/data",
        params={
            "key": api_key,
            "format": "json",
            "restrict_kind": "productivity",
            "restrict_begin": start_date,
            "restrict_end": end_date
        }
    )

    data = response.json()
    focus_seconds = sum(
        row[1] for row in data.get("rows", [])
        if row[2] >= 1  # Productivity score >= 1
    )

    return focus_seconds / 3600  # Hours
```

## Common Patterns

### Automated Time Logging

```python
# Log time from git commits
def log_commit_time(workspace_id, commit_msg, duration_minutes):
    requests.post(
        f"{BASE_URL}/workspaces/{workspace_id}/time_entries",
        auth=AUTH,
        json={
            "description": f"Git: {commit_msg}",
            "workspace_id": workspace_id,
            "start": start_time,
            "duration": duration_minutes * 60,
            "tags": ["git", "auto-logged"]
        }
    )
```

### Weekly Report

```python
# Generate weekly summary
entries = get_time_entries(start_of_week, end_of_week)
total_hours = sum(e["duration"] / 3600 for e in entries)
by_project = group_by_project(entries)
```

## Files

```
time-tracking/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **todoist-api** - Task management integration
- **trello-api** - Kanban board automation
- **notion-api** - Database-style tracking

## Resources

- [Toggl Track API](https://engineering.toggl.com/docs/)
- [RescueTime API](https://www.rescuetime.com/anapi/setup/documentation)
- [Toggl Reports API](https://engineering.toggl.com/docs/reports)

---

**See SKILL.md for complete API reference, reporting examples, and integration patterns.**
