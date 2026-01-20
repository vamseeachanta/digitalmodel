# Todoist API Integration Skill

> **Quick Reference Guide**

## Overview

Task management API integration for Todoist with projects, tasks, labels, filters, webhooks, and Python SDK for building automated productivity workflows.

**Version**: 1.0.0
**Category**: productivity
**Platforms**: rest-api, python, web, mobile

## Quick Start

### 1. Get API Token

```bash
# Get token from: https://todoist.com/app/settings/integrations/developer
export TODOIST_API_KEY="your-api-token"
```

### 2. Install Python SDK

```bash
pip install todoist-api-python
```

### 3. Basic Usage

```python
from todoist_api_python import TodoistAPI
import os

api = TodoistAPI(os.environ["TODOIST_API_KEY"])

# Create task
task = api.add_task(
    content="Review pull requests",
    due_string="tomorrow at 10am",
    priority=4
)

# Get today's tasks
today = api.get_tasks(filter="today")
for t in today:
    print(f"- {t.content}")
```

## Key Features

| Feature | Description |
|---------|-------------|
| REST API v2 | CRUD for projects, tasks, labels, sections |
| Sync API v9 | Batch operations, real-time sync |
| Natural Language | "every monday", "tomorrow at 3pm" |
| Filters | Complex queries: `(p1 | p2) & today` |
| Webhooks | Real-time task event notifications |

## Common Patterns

### Create Task with Options
```python
api.add_task(
    content="Complete report",
    project_id="2345678901",
    labels=["work", "urgent"],
    priority=4,  # 1-4, 4=urgent
    due_string="friday at 5pm"
)
```

### Filter Queries
```python
# Today's work tasks
api.get_tasks(filter="today & @work")

# Overdue high priority
api.get_tasks(filter="overdue & (p1 | p2)")

# Project tasks
api.get_tasks(filter="#ProjectName")
```

### cURL Examples
```bash
# List projects
curl -s "https://api.todoist.com/rest/v2/projects" \
    -H "Authorization: Bearer $TODOIST_API_KEY"

# Create task
curl -s -X POST "https://api.todoist.com/rest/v2/tasks" \
    -H "Authorization: Bearer $TODOIST_API_KEY" \
    -H "Content-Type: application/json" \
    -d '{"content": "New task", "due_string": "tomorrow"}'
```

## Files

```
todoist-api/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Related Skills

- **notion-api** - Alternative for database-style tasks
- **obsidian** - Integrate tasks with notes
- **api-integration** - General API patterns

## Resources

- [REST API Docs](https://developer.todoist.com/rest/v2/)
- [Sync API Docs](https://developer.todoist.com/sync/v9/)
- [Python SDK](https://github.com/Doist/todoist-api-python)
- [Filter Syntax](https://todoist.com/help/articles/introduction-to-filters)

---

**See SKILL.md for complete API reference, webhooks, batch operations, and integration examples.**
