# Git Advanced Skill

> **Quick Reference Guide**

## Overview

Advanced git workflows including interactive rebase, worktrees, bisect, rerere, reflog, hooks, and monorepo patterns.

**Version**: 1.0.0 | **Category**: devtools | **Platforms**: Linux, macOS, Windows

## Quick Start

```bash
# Essential configuration
git config --global pull.rebase true
git config --global rerere.enabled true
git config --global rebase.autosquash true
```

## Key Commands

```bash
# Interactive rebase
git rebase -i HEAD~5

# Worktrees
git worktree add ../feature feature-branch

# Bisect
git bisect start && git bisect bad && git bisect good v1.0

# Recovery
git reflog && git reset --hard HEAD@{2}
```

## Essential Aliases

```gitconfig
[alias]
    lg = log --graph --oneline --decorate -20
    undo = reset --soft HEAD~1
    wip = !git add -A && git commit -m 'WIP'
    sync = !git fetch --all && git pull --rebase
```

## Key Features

- Interactive rebase for clean history
- Worktrees for parallel development
- Bisect for automated bug hunting
- Rerere for conflict resolution
- Hooks for workflow automation

## Related Skills

- **docker** - Containerization patterns
- **cli-productivity** - Shell efficiency

---

For complete documentation, see [SKILL.md](./SKILL.md).
