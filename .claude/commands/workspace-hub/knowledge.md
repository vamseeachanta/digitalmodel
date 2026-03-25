---
name: knowledge
aliases: [k, kc, ka, ks, kr]
description: Capture, organize, and surface institutional knowledge across sessions
category: workspace-hub
---

# Knowledge Manager Command

Manage institutional knowledge across sessions and repositories. Capture decisions, patterns, gotchas, corrections, and tips with confidence tracking and decay.

## Usage

```
/knowledge [subcommand] [options]
```

## Subcommands

| Subcommand | Alias | Description |
|------------|-------|-------------|
| (none) | `/k` | Show knowledge base status summary |
| `capture` | `/kc` | Extract and store knowledge from current session |
| `advise` | `/ka` | Surface relevant knowledge for a task |
| `search` | `/ks` | Search knowledge base |
| `review` | `/kr` | Review stale entries, prune or revalidate |
| `stats` | - | Dashboard with health metrics |

## Examples

```bash
# Show status
/knowledge

# Capture a decision
/knowledge capture --type decision --title "Use Redis for caching"

# Auto-capture from reflect (non-interactive)
/knowledge capture --auto

# Get advice for a task
/knowledge advise "implement authentication in aceengineer-admin"

# Search by tag
/knowledge search --tag orchestrator

# Search by type and category
/knowledge search --type gotcha --category data

# Full-text search
/knowledge search --full-text "submodule foreach"

# Review and prune
/knowledge review --decay      # Apply confidence decay
/knowledge review --prune      # Interactive pruning of low-confidence entries

# Stats dashboard
/knowledge stats
```

## Entry Types

| Type | Prefix | Default Confidence |
|------|--------|--------------------|
| Decision | ADR- | 0.9 |
| Pattern | PAT- | 0.8 |
| Gotcha | GOT- | 0.85 |
| Correction | COR- | 0.7 |
| Tip | TIP- | 0.75 |

## Skill Reference

@.claude/skills/coordination/workspace/knowledge-manager/SKILL.md

## Related Commands

- `/reflect` - Periodic reflection (auto-captures knowledge)
- `/work` - Work queue (pre-task advise integration)
- `/repo-capability-map` - Capability context for advise
