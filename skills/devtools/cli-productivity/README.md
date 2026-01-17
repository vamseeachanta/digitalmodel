# CLI Productivity Skill

> **Quick Reference Guide**

## Overview

Essential CLI tools and shell productivity patterns for efficient terminal workflows including modern replacements for traditional Unix tools.

**Version**: 1.0.0 | **Category**: devtools | **Platforms**: Linux, macOS

## Quick Start

```bash
# macOS
brew install jq fzf ripgrep fd bat exa zoxide

# Initialize tools (add to ~/.bashrc)
eval "$(zoxide init bash)"
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
```

## Key Tools

| Tool | Replaces | Purpose |
|------|----------|---------|
| jq | - | JSON processing |
| fzf | - | Fuzzy finder |
| rg | grep | Fast search |
| fd | find | Fast file find |
| bat | cat | Syntax highlight |
| exa | ls | Better listing |

## Essential Aliases

```bash
alias ls='exa --group-directories-first'
alias cat='bat --paging=never'
alias grep='rg'
alias find='fd'
```

## Useful Functions

```bash
# Fuzzy edit file
fe() { vim $(fzf --preview 'bat --color=always {}'); }

# Search and edit
rge() { rg -n "$1" | fzf --ansi | cut -d: -f1-2 | xargs -o vim; }
```

## Related Skills

- **docker** - Containerization patterns
- **git-advanced** - Version control workflows

---

For complete documentation, see [SKILL.md](./SKILL.md).
