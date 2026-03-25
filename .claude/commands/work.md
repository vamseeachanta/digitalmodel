---
name: work
aliases: [work-queue]
description: Maintain a queue of work items across workspace-hub repositories with capture and process pipeline
category: workspace-hub
---

# Work Queue Command

Capture and process work items (features, bugs, tasks) across workspace-hub repositories. Two-phase system: rapidly log items, then triage and delegate by complexity.

## Usage

```
/work [subcommand] [args]
```

## Subcommands

| Subcommand | Description |
|------------|-------------|
| `add <desc>` | Capture one or more work items |
| `run` | Process next pending item (default if no args) |
| `list` | Show all pending/working/blocked items |
| `status WRK-NNN` | Show specific item details |
| `prioritize` | Interactive priority adjustment |
| `archive WRK-NNN` | Manually archive an item |
| `report` | Queue health summary |

## Smart Routing

- **Action verbs** (run, go, start, next) → Process pipeline
- **Descriptive content** → Capture pipeline

## Examples

```bash
# Capture a simple work item
/work add Fix login redirect in aceengineer-website

# Capture multiple items at once
/work add 1. Fix login redirect 2. Add dark mode 3. Update footer

# Process next item in queue
/work run

# Default (no args) also processes
/work

# List all active items
/work list

# Check specific item
/work status WRK-001

# Queue health report
/work report

# Archive completed item
/work archive WRK-001
```

## Complexity Routing

| Route | Complexity | Pipeline |
|-------|-----------|----------|
| A | Simple | Implement → Test → Archive |
| B | Medium | Explore → Implement → Test → Archive |
| C | Complex | Plan → Explore → Implement → Test → Review → Archive |

## Skill Reference

@.claude/skills/coordination/workspace/work-queue/SKILL.md

## Related Commands

- `/reflect` - Daily reflection includes work queue health check
