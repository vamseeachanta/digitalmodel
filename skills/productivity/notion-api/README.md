# Notion API Integration Skill

> **Quick Reference Guide**

## Overview

Notion API for workspace automation including databases, pages, blocks, query/filter syntax, and integration patterns for building powerful productivity workflows.

**Version**: 1.0.0
**Category**: productivity
**Platforms**: rest-api, python, javascript, web

## Quick Start

### 1. Create Integration

```markdown
1. Go to https://www.notion.so/my-integrations
2. Create new integration
3. Copy the "Internal Integration Token"
4. Connect integration to your pages (... menu > Connections)
```

### 2. Set Environment

```bash
export NOTION_API_KEY="secret_xxxxxxxxxxxxx"
```

### 3. Install Python SDK

```bash
pip install notion-client
```

### 4. Basic Usage

```python
from notion_client import Client
import os

notion = Client(auth=os.environ["NOTION_API_KEY"])

# Query database
results = notion.databases.query(
    database_id="your-database-id",
    filter={
        "property": "Status",
        "select": {"equals": "In Progress"}
    }
)

# Create page
notion.pages.create(
    parent={"database_id": "your-database-id"},
    properties={
        "Name": {"title": [{"text": {"content": "New Task"}}]},
        "Status": {"select": {"name": "To Do"}}
    }
)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Databases | Create, query, filter with complex conditions |
| Pages | CRUD operations with properties |
| Blocks | Paragraphs, headings, lists, code, callouts |
| Relations | Link databases with rollups |
| Search | Full-text search across workspace |

## Common Filter Patterns

```python
# Text contains
{"property": "Name", "title": {"contains": "keyword"}}

# Select equals
{"property": "Status", "select": {"equals": "Done"}}

# Date before
{"property": "Due Date", "date": {"before": "2025-02-01"}}

# Compound AND
{"and": [
    {"property": "Status", "select": {"equals": "Active"}},
    {"property": "Priority", "select": {"equals": "High"}}
]}

# Checkbox
{"property": "Completed", "checkbox": {"equals": True}}
```

## cURL Examples

```bash
# Query database
curl -X POST "https://api.notion.com/v1/databases/DB_ID/query" \
    -H "Authorization: Bearer $NOTION_API_KEY" \
    -H "Notion-Version: 2022-06-28" \
    -H "Content-Type: application/json" \
    -d '{"filter": {"property": "Status", "select": {"equals": "Active"}}}'
```

## Files

```
notion-api/
├── SKILL.md    # Full documentation (950+ lines)
└── README.md   # This quick reference
```

## Related Skills

- **todoist-api** - Alternative for task-focused workflows
- **obsidian** - Local-first alternative
- **api-integration** - General API patterns

## Resources

- [Notion API Docs](https://developers.notion.com/)
- [API Reference](https://developers.notion.com/reference/intro)
- [Python SDK](https://github.com/ramnes/notion-sdk-py)

---

**See SKILL.md for complete database operations, block types, filters, and integration examples.**
