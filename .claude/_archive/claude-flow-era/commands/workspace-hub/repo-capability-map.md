---
name: repo-capability-map
aliases: [capability-map, capmap]
description: Analyze repository capabilities, classify by domain, assess maturity, and identify strategic gaps
category: workspace-hub
---

# Repo Capability Map

Discover what each repository can do, classify capabilities by domain, assess maturity against a 4-level model, and identify strategic gaps against mission objectives.

## Usage

```
/repo-capability-map [subcommand] [options]
```

## Subcommands

| Subcommand | Description |
|------------|-------------|
| `scan` (default) | Full scan of all active repos in workspace |
| `scan <repo>` | Scan a single repository |
| `domain <name>` | Show capabilities for one domain (ENG, DATA, WEB, BIZ, INFRA, KNOW, FIN) |
| `gaps` | Show gap analysis vs mission objectives |
| `report` | Generate full markdown report to `reports/capability-map/` |
| `compare <r1> <r2>` | Side-by-side capability comparison between two repos |

## Options

| Option | Default | Description |
|--------|---------|-------------|
| `--format` | `console` | Output format: `console`, `markdown`, or `both` |
| `--output` | auto | Override default report output path |
| `--include-dormant` | false | Include dormant/archived repos in scan |
| `--maturity-filter` | none | Filter results by minimum maturity level (1-4) |

## Examples

```bash
# Scan all repos and display console summary
/repo-capability-map

# Scan a single repo
/repo-capability-map scan digitalmodel

# Show only engineering domain capabilities
/repo-capability-map domain ENG

# Run gap analysis
/repo-capability-map gaps

# Generate full markdown report
/repo-capability-map report

# Compare two repos side-by-side
/repo-capability-map compare digitalmodel worldenergydata

# Include dormant repos, filter to mature only
/repo-capability-map scan --include-dormant --maturity-filter=3
```

## Skill Reference

@.claude/skills/coordination/workspace/repo-capability-map/SKILL.md

## Related Commands

- `/health` - Repository health analysis (maintenance perspective)
- `/work` - Work queue management (gaps can feed work items)
- `/reflect` - Periodic reflection (tracks capability changes)
