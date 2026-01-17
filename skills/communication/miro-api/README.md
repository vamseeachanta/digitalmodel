# Miro API Skill

> **Quick Reference Guide**

## Overview

Miro whiteboard automation using REST API v2 and Python SDK for creating boards, frames, shapes, connectors, and collaborative visual workflows.

**Version**: 1.0.0
**Category**: communication
**Platforms**: linux, macos, windows

## Quick Start

### 1. Install Dependencies

```bash
pip install miro-api python-dotenv requests
```

### 2. Set Environment Variables

```bash
export MIRO_ACCESS_TOKEN=your-access-token
export MIRO_TEAM_ID=your-team-id
```

### 3. Basic Usage

```python
from miro_api import Miro
import os

miro = Miro(access_token=os.environ["MIRO_ACCESS_TOKEN"])

# Create a board
board = miro.boards.create(
    name="My Board",
    team_id=os.environ["MIRO_TEAM_ID"]
)

# Create a sticky note
miro.sticky_notes.create(
    board_id=board.id,
    data={"content": "Hello Miro!"},
    style={"fillColor": "yellow"},
    position={"x": 0, "y": 0, "origin": "center"}
)
```

## Key Capabilities

- **Board Management**: Create, copy, update, delete boards
- **Sticky Notes**: Create colored notes in grids or custom layouts
- **Shapes**: Rectangles, circles, flowchart shapes
- **Connectors**: Lines, arrows, ERD relationships
- **Frames**: Organize content with titled sections
- **Text & Images**: Add text boxes, upload images
- **Templates**: Kanban boards, retrospectives, workshops

## Common Patterns

### Create Kanban Board

```python
columns = ["To Do", "In Progress", "Done"]
for i, col in enumerate(columns):
    miro.frames.create(
        board_id=board_id,
        data={"title": col},
        position={"x": i * 400, "y": 0, "origin": "center"},
        geometry={"width": 350, "height": 800}
    )
```

### Create Flowchart Connector

```python
miro.connectors.create(
    board_id=board_id,
    data={
        "startItem": {"id": shape1_id},
        "endItem": {"id": shape2_id}
    },
    style={"endStrokeCap": "stealth"}
)
```

## Files

```
miro-api/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Dependencies

- miro-api >= 2.0.0
- python-dotenv >= 1.0.0
- requests >= 2.31.0

## Related Skills

- **slack-api** - Team notifications
- **notion-api** - Documentation integration
- **github-actions** - CI/CD automation

## Resources

- [Miro REST API Docs](https://developers.miro.com/reference)
- [Miro Python SDK](https://github.com/miroapp/api-clients)
- [Developer Portal](https://developers.miro.com/)

---

**See SKILL.md for complete documentation with 6+ comprehensive examples.**
