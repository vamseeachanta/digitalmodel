---
name: reflect
aliases: [claude-reflect]
description: Run periodic reflection on git history across all workspace-hub repositories
category: workspace-hub
---

# Claude Reflect Command

Run periodic cross-repository reflection analyzing git history, extracting patterns via RAGS loop, and enhancing/creating skills based on findings.

## Usage

```
/reflect [options]
```

## Options

| Option | Description |
|--------|-------------|
| `--days <n>` | Analysis window in days (default: 30) |
| `--repo <name>` | Analyze single repository |
| `--dry-run` | Preview patterns without creating skills |

## Examples

```bash
# Default 30-day reflection
/reflect

# Quick 7-day reflection
/reflect --days 7

# Extended quarterly reflection
/reflect --days 90

# Single repository analysis
/reflect --repo digitalmodel

# Preview mode
/reflect --dry-run
```

## Skill Reference

@.claude/skills/workspace-hub/claude-reflect/SKILL.md

## Related Commands

- `/repo-sync` - Synchronize repositories
- `/skill-creator` - Manual skill creation
