# Trello API Integration Skill

> **Quick Reference Guide**

## Overview

Kanban board automation with Trello API including boards, lists, cards, members, webhooks, power-ups, and Python SDK (py-trello) integration for building workflow automations.

**Category**: Productivity
**Version**: 1.0.0
**Platforms**: REST API, Python, Web

## Quick Start

### 1. Get API Credentials

```bash
# Get API Key: https://trello.com/app-key
# Get Token: https://trello.com/1/authorize?expiration=never&scope=read,write&response_type=token&key=YOUR_API_KEY

export TRELLO_API_KEY="your-api-key"
export TRELLO_TOKEN="your-token"
```

### 2. Install Python SDK

```bash
pip install py-trello
```

### 3. Basic Usage

```python
from trello import TrelloClient
import os

client = TrelloClient(
    api_key=os.environ["TRELLO_API_KEY"],
    token=os.environ["TRELLO_TOKEN"]
)

# Get boards
boards = client.list_boards()
for board in boards:
    print(f"{board.name}")

# Create card
board = client.get_board("BOARD_ID")
target_list = board.get_list("LIST_ID")
card = target_list.add_card(
    name="New Task",
    desc="Task description",
    due="2025-02-15"
)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Boards | Create, update, archive boards |
| Lists | CRUD for Kanban columns |
| Cards | Full card management with labels, checklists |
| Webhooks | Real-time event notifications |
| Custom Fields | Extended card metadata |

## Common Patterns

### Create Board with Lists

```python
# Create board
board = client.add_board("Project Board")

# Add lists
board.add_list("Backlog")
board.add_list("In Progress")
board.add_list("Done")
```

### Card Operations

```python
# Create card with options
card = target_list.add_card(
    name="Implement feature",
    desc="Full description",
    due="2025-02-01"
)

# Add checklist
card.add_checklist("Tasks", ["Step 1", "Step 2", "Step 3"])

# Add label
labels = board.get_labels()
card.add_label(labels[0])

# Move card
done_list = board.get_list("DONE_LIST_ID")
card.change_list(done_list.id)
```

### cURL Examples

```bash
# List boards
curl -s "https://api.trello.com/1/members/me/boards?key=$TRELLO_API_KEY&token=$TRELLO_TOKEN"

# Create card
curl -s -X POST "https://api.trello.com/1/cards" \
    -d "key=$TRELLO_API_KEY" \
    -d "token=$TRELLO_TOKEN" \
    -d "idList=LIST_ID" \
    -d "name=New Card"
```

## Webhooks

```python
# Create webhook
curl -s -X POST "https://api.trello.com/1/webhooks" \
    -d "key=$TRELLO_API_KEY" \
    -d "token=$TRELLO_TOKEN" \
    -d "callbackURL=https://your-server.com/webhook" \
    -d "idModel=BOARD_ID"
```

## Files

```
trello-api/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **todoist-api** - Alternative task management
- **notion-api** - Database-style project management
- **github-actions** - CI/CD integration

## Resources

- [Trello REST API](https://developer.atlassian.com/cloud/trello/rest/)
- [py-trello GitHub](https://github.com/sarumont/py-trello)
- [Trello Developer Portal](https://developer.atlassian.com/cloud/trello/)

---

**See SKILL.md for complete API reference, webhooks, custom fields, and integration examples.**
