# Obsidian Knowledge Management Skill

> **Quick Reference Guide**

## Overview

Local-first knowledge management with markdown vaults, bidirectional linking, powerful plugin ecosystem, and flexible sync strategies for building a personal second brain.

**Version**: 1.0.0
**Category**: productivity
**Platforms**: desktop, mobile, linux, macos, windows, ios, android

## Quick Start

### 1. Install Obsidian

```bash
# macOS
brew install --cask obsidian

# Linux (Flatpak)
flatpak install flathub md.obsidian.Obsidian

# Or download from https://obsidian.md/download
```

### 2. Create Vault Structure

```bash
mkdir -p ~/Documents/ObsidianVault
cd ~/Documents/ObsidianVault
mkdir -p "Daily Notes" Templates Inbox Projects Areas Resources Archive
```

### 3. Basic Note with Links

```markdown
# My First Note

This links to [[Another Note]] and [[Projects/Project Alpha]].

## Tags
#learning #obsidian

## Tasks
- [ ] Learn linking
- [ ] Install plugins
```

## Key Features

| Feature | Description |
|---------|-------------|
| Bidirectional Links | `[[Note Name]]` creates two-way connections |
| Graph View | Visualize note relationships |
| Templates | Consistent note structures |
| Dataview | Query notes like a database |
| Local-first | Your data, your control |

## Essential Plugins

```markdown
1. Dataview    - Query and display data from notes
2. Templater   - Advanced templates with logic
3. Calendar    - Visual daily note navigation
4. Tasks       - Task management within notes
```

## Common Patterns

### Daily Note Template
```markdown
# {{date:YYYY-MM-DD}}
## Tasks
- [ ]
## Notes

## Links
[[{{date:YYYY-MM-DD|yesterday}}]] | [[{{date:YYYY-MM-DD|tomorrow}}]]
```

### Dataview Query
```markdown
```dataview
TABLE status, due-date
FROM #project
WHERE status != "completed"
SORT due-date ASC
```
```

## Files

```
obsidian/
├── SKILL.md    # Full documentation (850+ lines)
└── README.md   # This quick reference
```

## Related Skills

- **yaml-configuration** - YAML frontmatter patterns
- **git-sync-manager** - Git-based vault sync
- **todoist-api** - Task integration

## Resources

- [Obsidian Help](https://help.obsidian.md/)
- [Obsidian Forum](https://forum.obsidian.md/)
- [Dataview Docs](https://blacksmithgu.github.io/obsidian-dataview/)

---

**See SKILL.md for vault organization, plugin setup, sync strategies, and backup workflows.**
