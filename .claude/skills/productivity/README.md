# Productivity Skills Library

> Streamline knowledge management, task tracking, and time optimization workflows
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 5 productivity-focused skills for integrating with popular knowledge management and task tracking platforms. Each skill provides API patterns, automation workflows, and best practices for building efficient personal and team productivity systems.

## Quick Start

```bash
# Browse available skills
ls skills/productivity/

# Read a skill
cat skills/productivity/obsidian/SKILL.md

# Skills are documentation - integrate patterns into your workflows
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [obsidian](./obsidian/SKILL.md) | Local-first knowledge management | Markdown vaults, backlinks, plugins, sync |
| [todoist-api](./todoist-api/SKILL.md) | Task management via REST API | Projects, tasks, labels, filters, webhooks |
| [notion-api](./notion-api/SKILL.md) | All-in-one workspace integration | Databases, pages, blocks, relations |
| [trello-api](./trello-api/SKILL.md) | Kanban board automation | Boards, lists, cards, power-ups |
| [time-tracking](./time-tracking/SKILL.md) | Time management integrations | Toggl, Clockify, Harvest APIs |

## Skill Categories

### Knowledge Management
- **obsidian** - Local-first markdown knowledge base
- **notion-api** - Cloud-based workspace with databases

### Task Management
- **todoist-api** - Personal and team task tracking
- **trello-api** - Visual kanban workflows

### Time Management
- **time-tracking** - Track time across projects and clients

## Skill Selection Guide

| Use Case | Recommended Skill | Why |
|----------|-------------------|-----|
| Personal note-taking with privacy | obsidian | Local-first, markdown, full control |
| Team knowledge base | notion-api | Collaboration, databases, permissions |
| GTD-style task management | todoist-api | Natural language, recurring tasks, filters |
| Visual project tracking | trello-api | Kanban boards, drag-and-drop, integrations |
| Billable hours tracking | time-tracking | Multi-platform, reporting, invoicing |
| Second brain / Zettelkasten | obsidian | Backlinks, graph view, plugins |
| Complex project databases | notion-api | Relations, rollups, formulas |

## Common Patterns Across Skills

### API Authentication
```bash
# Environment-based API keys
export TODOIST_API_KEY="your-api-key"
export NOTION_API_KEY="secret_..."
export TRELLO_API_KEY="your-key"
export TRELLO_TOKEN="your-token"

# Header patterns
curl -H "Authorization: Bearer $TODOIST_API_KEY" \
     https://api.todoist.com/rest/v2/tasks
```

### Webhook Integration
```bash
# Common webhook payload structure
{
    "event_type": "item:completed",
    "event_data": { ... },
    "timestamp": "2026-01-17T10:30:00Z"
}
```

### Rate Limiting
```bash
# Respect API limits
RATE_LIMIT=50  # requests per minute
sleep_between_requests() {
    sleep $(echo "60 / $RATE_LIMIT" | bc -l)
}
```

### Error Handling
```bash
handle_api_error() {
    local status=$1
    case $status in
        429) echo "Rate limited - backing off" >&2; sleep 60 ;;
        401) echo "Authentication failed" >&2; exit 1 ;;
        5*) echo "Server error - retrying" >&2; sleep 5 ;;
    esac
}
```

## Usage Examples

### Create Todoist Task from CLI
```bash
# See todoist-api for complete patterns
create_task() {
    local content="$1"
    local project_id="${2:-}"

    curl -s -X POST "https://api.todoist.com/rest/v2/tasks" \
        -H "Authorization: Bearer $TODOIST_API_KEY" \
        -H "Content-Type: application/json" \
        -d "{\"content\": \"$content\", \"project_id\": \"$project_id\"}"
}

create_task "Review pull requests" "2345678901"
```

### Query Notion Database
```bash
# See notion-api for complete patterns
query_database() {
    local database_id="$1"

    curl -s -X POST "https://api.notion.com/v1/databases/$database_id/query" \
        -H "Authorization: Bearer $NOTION_API_KEY" \
        -H "Notion-Version: 2022-06-28" \
        -H "Content-Type: application/json" \
        -d '{"filter": {"property": "Status", "select": {"equals": "In Progress"}}}'
}
```

### Sync Obsidian Notes
```bash
# See obsidian for complete patterns
VAULT_PATH="$HOME/Documents/ObsidianVault"

# Create daily note
create_daily_note() {
    local date=$(date +%Y-%m-%d)
    local note_path="$VAULT_PATH/Daily Notes/$date.md"

    cat > "$note_path" << EOF
# $date

## Tasks
- [ ] Morning review
- [ ] Evening reflection

## Notes

## Links
[[$(date -d "yesterday" +%Y-%m-%d)]] | [[$(date -d "tomorrow" +%Y-%m-%d)]]
EOF
}
```

### Track Time Entry
```bash
# See time-tracking for complete patterns
start_timer() {
    local description="$1"
    local project_id="$2"

    curl -s -X POST "https://api.track.toggl.com/api/v9/workspaces/$WORKSPACE_ID/time_entries" \
        -u "$TOGGL_API_TOKEN:api_token" \
        -H "Content-Type: application/json" \
        -d "{\"description\": \"$description\", \"project_id\": $project_id, \"start\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\", \"created_with\": \"cli\"}"
}
```

## Integration with Workspace-Hub

These skills integrate with workspace development workflows:

```
workspace-hub/
├── scripts/
│   ├── daily-standup.sh         # Uses: todoist-api, time-tracking
│   ├── project-sync.sh          # Uses: notion-api, trello-api
│   └── knowledge-backup.sh      # Uses: obsidian
├── automation/
│   ├── task-creation/           # Uses: todoist-api, trello-api
│   └── time-reports/            # Uses: time-tracking
└── config/
    └── productivity.conf        # API keys and preferences
```

## Best Practices

### 1. Secure API Keys
```bash
# Use environment variables or secure vaults
# Never commit API keys to repositories
export TODOIST_API_KEY=$(security find-generic-password -s "todoist" -w)
```

### 2. Implement Retry Logic
```bash
retry_request() {
    local max_attempts=3
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if response=$(make_request "$@"); then
            echo "$response"
            return 0
        fi
        ((attempt++))
        sleep $((2 ** attempt))
    done
    return 1
}
```

### 3. Cache Responses
```bash
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/productivity"
CACHE_TTL=300  # 5 minutes

cached_request() {
    local cache_key="$1"
    local cache_file="$CACHE_DIR/$cache_key"

    if [ -f "$cache_file" ] && [ $(($(date +%s) - $(stat -f%m "$cache_file"))) -lt $CACHE_TTL ]; then
        cat "$cache_file"
        return
    fi

    # Make fresh request and cache
    response=$(make_request "${@:2}")
    echo "$response" > "$cache_file"
    echo "$response"
}
```

### 4. Batch Operations
```bash
# Batch multiple operations to reduce API calls
batch_create_tasks() {
    local tasks_json="$1"

    curl -s -X POST "https://api.todoist.com/sync/v9/sync" \
        -H "Authorization: Bearer $TODOIST_API_KEY" \
        -d "commands=$tasks_json"
}
```

### 5. Handle Pagination
```bash
fetch_all_pages() {
    local endpoint="$1"
    local all_results=()
    local cursor=""

    while true; do
        response=$(curl -s "$endpoint?cursor=$cursor")
        results=$(echo "$response" | jq -r '.results[]')
        all_results+=("$results")

        cursor=$(echo "$response" | jq -r '.next_cursor // empty')
        [ -z "$cursor" ] && break
    done

    printf '%s\n' "${all_results[@]}"
}
```

## Testing Skills

Each skill includes testable examples:

```bash
#!/bin/bash
# test_productivity_skill.sh

test_todoist_connection() {
    response=$(curl -s -o /dev/null -w "%{http_code}" \
        -H "Authorization: Bearer $TODOIST_API_KEY" \
        "https://api.todoist.com/rest/v2/projects")

    [ "$response" = "200" ] && echo "PASS: Todoist connected" || echo "FAIL: Todoist auth failed"
}

test_notion_connection() {
    response=$(curl -s -o /dev/null -w "%{http_code}" \
        -H "Authorization: Bearer $NOTION_API_KEY" \
        -H "Notion-Version: 2022-06-28" \
        "https://api.notion.com/v1/users/me")

    [ "$response" = "200" ] && echo "PASS: Notion connected" || echo "FAIL: Notion auth failed"
}

# Run tests
test_todoist_connection
test_notion_connection
```

## Contributing

When adding new skills:

1. **Document real workflows** - Extract patterns from actual usage
2. **Include authentication** - Show secure API key handling
3. **Add error handling** - Cover common failure modes
4. **Provide complete examples** - Patterns should be copy-paste ready
5. **Update this README** - Add to the skills table and categories

## Related Resources

- [Todoist API Documentation](https://developer.todoist.com/)
- [Notion API Reference](https://developers.notion.com/)
- [Trello REST API](https://developer.atlassian.com/cloud/trello/)
- [Toggl Track API](https://developers.track.toggl.com/)
- [Obsidian Help](https://help.obsidian.md/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 5 productivity skills

---

*These skills enable seamless integration between development workflows and productivity tools, reducing context switching and improving focus.*
