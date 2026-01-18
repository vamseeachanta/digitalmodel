# DevTools Skills Library

> Optimize development environment, containers, CLI productivity, and git workflows
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 7 development tools skills for building efficient local development environments. Each skill provides patterns for containerization, CLI productivity, editor customization, and advanced git workflows used across the workspace-hub ecosystem.

## Quick Start

```bash
# Browse available skills
ls skills/devtools/

# Read a skill
cat skills/devtools/docker/SKILL.md

# Skills are documentation - integrate patterns into your workflow
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [docker](./docker/SKILL.md) | Container development workflows | Compose, multi-stage builds, volumes, networking |
| [cli-productivity](./cli-productivity/SKILL.md) | Shell efficiency patterns | Aliases, functions, fzf, history, completions |
| [vscode-extensions](./vscode-extensions/SKILL.md) | VS Code customization | Extensions, keybindings, tasks, debugging |
| [raycast-alfred](./raycast-alfred/SKILL.md) | macOS launcher automation | Scripts, workflows, clipboard, snippets |
| [git-advanced](./git-advanced/SKILL.md) | Advanced git operations | Worktrees, bisect, reflog, rerere, hooks |
| [uv-package-manager](./uv-package-manager/SKILL.md) | Fast Python package management | venv, sync, lock files, pip replacement |
| [pyproject-toml](./pyproject-toml/SKILL.md) | Python project configuration | Build system, dependencies, tool settings |

## Skill Categories

### Containerization
- **docker** - Container development and orchestration

### Shell Productivity
- **cli-productivity** - Terminal efficiency and automation
- **raycast-alfred** - macOS launcher scripts

### Editor & IDE
- **vscode-extensions** - VS Code configuration and extensions

### Version Control
- **git-advanced** - Advanced git techniques

### Python Tooling
- **uv-package-manager** - Fast Python package management with UV
- **pyproject-toml** - Modern Python project configuration

## Skill Selection Guide

| Use Case | Recommended Skill | Why |
|----------|-------------------|-----|
| Local development environment | docker | Consistent, reproducible, isolated |
| Speed up terminal workflow | cli-productivity | Aliases, fuzzy finding, completions |
| Editor customization | vscode-extensions | Keybindings, snippets, debugging |
| macOS automation | raycast-alfred | Quick scripts, clipboard history |
| Complex git operations | git-advanced | Worktrees, bisect, conflict resolution |
| Multi-container services | docker | Compose, networking, orchestration |
| Cross-repository work | git-advanced | Worktrees, submodules |
| Quick file navigation | cli-productivity | fzf, fd, ripgrep integration |

## Common Patterns Across Skills

### Environment Configuration
```bash
# XDG Base Directory
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"

# Tool-specific
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export HISTFILE="$XDG_DATA_HOME/bash/history"
```

### Dotfile Management
```bash
# Symlink configuration
link_dotfiles() {
    local dotfiles_dir="$HOME/dotfiles"

    ln -sf "$dotfiles_dir/.bashrc" "$HOME/.bashrc"
    ln -sf "$dotfiles_dir/.gitconfig" "$HOME/.gitconfig"
    ln -sf "$dotfiles_dir/config/nvim" "$XDG_CONFIG_HOME/nvim"
}
```

### Tool Version Management
```bash
# Version switching pattern
use_tool_version() {
    local tool="$1"
    local version="$2"

    case "$tool" in
        node) nvm use "$version" ;;
        python) pyenv shell "$version" ;;
        ruby) rbenv shell "$version" ;;
        go) goenv shell "$version" ;;
    esac
}
```

### Path Management
```bash
# Add to PATH safely
add_to_path() {
    local dir="$1"
    [[ ":$PATH:" != *":$dir:"* ]] && export PATH="$dir:$PATH"
}

add_to_path "$HOME/.local/bin"
add_to_path "$HOME/go/bin"
```

## Usage Examples

### Docker Development Environment
```bash
# See docker for complete patterns

# Development Dockerfile with caching
cat > Dockerfile << 'EOF'
# Build stage
FROM node:20-alpine AS builder
WORKDIR /app

# Cache dependencies
COPY package*.json ./
RUN npm ci

# Build application
COPY . .
RUN npm run build

# Production stage
FROM node:20-alpine AS production
WORKDIR /app

COPY --from=builder /app/dist ./dist
COPY --from=builder /app/node_modules ./node_modules

EXPOSE 3000
CMD ["node", "dist/server.js"]
EOF

# Docker Compose for local development
cat > docker-compose.yml << 'EOF'
version: '3.8'
services:
  app:
    build:
      context: .
      target: builder
    volumes:
      - .:/app
      - /app/node_modules
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=development
    command: npm run dev

  db:
    image: postgres:15-alpine
    volumes:
      - postgres_data:/var/lib/postgresql/data
    environment:
      POSTGRES_DB: devdb
      POSTGRES_USER: devuser
      POSTGRES_PASSWORD: devpass

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:
EOF

# Common commands
docker compose up -d
docker compose logs -f app
docker compose exec app sh
docker compose down -v
```

### CLI Productivity Setup
```bash
# See cli-productivity for complete patterns

# Essential aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'
alias g='git'
alias d='docker'
alias dc='docker compose'
alias k='kubectl'

# Git shortcuts
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline -20'
alias gd='git diff'
alias gb='git branch'
alias gco='git checkout'

# FZF integration
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --exclude .git'

# Fuzzy file edit
fe() {
    local file=$(fzf --preview 'bat --color=always {}')
    [[ -n "$file" ]] && ${EDITOR:-vim} "$file"
}

# Fuzzy git branch checkout
fco() {
    local branch=$(git branch -a | fzf | sed 's/remotes\/origin\///' | xargs)
    [[ -n "$branch" ]] && git checkout "$branch"
}

# Fuzzy kill process
fkill() {
    local pid=$(ps aux | fzf | awk '{print $2}')
    [[ -n "$pid" ]] && kill -9 "$pid"
}

# Directory navigation with z
eval "$(zoxide init bash)"
```

### VS Code Configuration
```jsonc
// See vscode-extensions for complete patterns

// settings.json
{
  // Editor
  "editor.fontSize": 14,
  "editor.fontFamily": "'JetBrains Mono', 'Fira Code', monospace",
  "editor.fontLigatures": true,
  "editor.lineNumbers": "relative",
  "editor.minimap.enabled": false,
  "editor.formatOnSave": true,
  "editor.defaultFormatter": "esbenp.prettier-vscode",

  // Files
  "files.autoSave": "onFocusChange",
  "files.trimTrailingWhitespace": true,
  "files.insertFinalNewline": true,

  // Terminal
  "terminal.integrated.fontSize": 13,
  "terminal.integrated.defaultProfile.osx": "zsh",

  // Git
  "git.autofetch": true,
  "git.confirmSync": false,

  // Language-specific
  "[python]": {
    "editor.defaultFormatter": "ms-python.black-formatter"
  },
  "[javascript][typescript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  }
}
```

```jsonc
// keybindings.json
[
  { "key": "cmd+k cmd+e", "command": "workbench.view.explorer" },
  { "key": "cmd+k cmd+g", "command": "workbench.view.scm" },
  { "key": "cmd+k cmd+t", "command": "workbench.action.terminal.toggleTerminal" },
  { "key": "cmd+shift+d", "command": "editor.action.copyLinesDownAction" },
  { "key": "cmd+shift+k", "command": "editor.action.deleteLines" }
]
```

### Git Advanced Workflows
```bash
# See git-advanced for complete patterns

# Git worktrees - work on multiple branches simultaneously
git worktree add ../feature-branch feature/new-feature
git worktree add ../hotfix-branch hotfix/urgent-fix
git worktree list
git worktree remove ../feature-branch

# Git bisect - find the commit that introduced a bug
git bisect start
git bisect bad HEAD
git bisect good v1.0.0
# Git will checkout commits for you to test
git bisect run ./test.sh  # Automate with a script
git bisect reset

# Git reflog - recover lost commits
git reflog
git checkout HEAD@{5}  # Go back to a previous state
git branch recovered-branch HEAD@{5}  # Create branch from lost commit

# Git rerere - reuse recorded resolution
git config rerere.enabled true
# Now git remembers how you resolved conflicts

# Interactive rebase patterns
git rebase -i HEAD~5
# Commands in editor:
#   pick   - use commit
#   reword - edit commit message
#   edit   - stop and amend
#   squash - combine with previous
#   fixup  - combine, discard message
#   drop   - remove commit

# Git hooks
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Run tests before commit
npm test || exit 1

# Run linter
npm run lint || exit 1
EOF
chmod +x .git/hooks/pre-commit

# Shared hooks with husky
npx husky init
echo "npm test" > .husky/pre-commit
```

### Raycast/Alfred Automation
```bash
# See raycast-alfred for complete patterns

# Raycast script command (bash)
#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Open Project
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 📁
# @raycast.argument1 { "type": "text", "placeholder": "project name" }

PROJECT="$1"
PROJECT_DIR="$HOME/projects/$PROJECT"

if [ -d "$PROJECT_DIR" ]; then
    code "$PROJECT_DIR"
    echo "Opened $PROJECT"
else
    echo "Project not found: $PROJECT"
fi
```

## Integration with Workspace-Hub

These skills power development workflows:

```
workspace-hub/
├── docker/
│   ├── Dockerfile           # Uses: docker
│   └── docker-compose.yml   # Uses: docker
├── .vscode/
│   ├── settings.json        # Uses: vscode-extensions
│   ├── extensions.json
│   └── tasks.json
├── scripts/
│   ├── dev-setup.sh         # Uses: cli-productivity
│   └── git-helpers.sh       # Uses: git-advanced
├── .githooks/
│   ├── pre-commit           # Uses: git-advanced
│   └── commit-msg
└── dotfiles/
    ├── .bashrc              # Uses: cli-productivity
    └── .gitconfig           # Uses: git-advanced
```

## Best Practices

### 1. Reproducible Environments
```bash
# Pin versions in Dockerfiles
FROM node:20.10.0-alpine

# Use lockfiles
npm ci --frozen-lockfile
pip install -r requirements.txt --require-hashes

# Document tool versions
cat > .tool-versions << 'EOF'
nodejs 20.10.0
python 3.12.0
EOF
```

### 2. Layer Optimization
```dockerfile
# Order layers by change frequency (least to most)
FROM node:20-alpine

# System dependencies (rarely change)
RUN apk add --no-cache git

# Package manifests (change sometimes)
COPY package*.json ./
RUN npm ci

# Application code (changes often)
COPY . .
```

### 3. Shell Function Library
```bash
# Create reusable functions
source_if_exists() {
    [[ -f "$1" ]] && source "$1"
}

source_if_exists "$HOME/.bashrc.local"
source_if_exists "$HOME/.bashrc.work"

# Lazy loading for slow tools
nvm() {
    unset -f nvm
    source "$NVM_DIR/nvm.sh"
    nvm "$@"
}
```

### 4. Git Configuration
```gitconfig
[alias]
    # Shortcuts
    co = checkout
    br = branch
    ci = commit
    st = status

    # Log formats
    lg = log --graph --oneline --decorate
    ll = log --pretty=format:'%C(yellow)%h%C(reset) %s %C(blue)<%an>%C(reset)'

    # Useful commands
    undo = reset --soft HEAD~1
    amend = commit --amend --no-edit
    wip = !git add -A && git commit -m 'WIP'

[core]
    autocrlf = input
    editor = vim

[pull]
    rebase = true

[push]
    autoSetupRemote = true

[rerere]
    enabled = true
```

### 5. Editor Workspace Settings
```jsonc
// .vscode/settings.json (per-project)
{
  "editor.rulers": [80, 120],
  "files.exclude": {
    "**/.git": true,
    "**/node_modules": true,
    "**/__pycache__": true
  },
  "search.exclude": {
    "**/dist": true,
    "**/coverage": true
  }
}
```

## Testing DevTools Skills

Validate configurations and setups:

```bash
#!/bin/bash
# test_devtools.sh

test_docker() {
    docker --version && echo "PASS: Docker installed" || echo "FAIL: Docker not found"
    docker compose version && echo "PASS: Compose installed" || echo "FAIL: Compose not found"
}

test_cli_tools() {
    command -v fzf && echo "PASS: fzf installed" || echo "FAIL: fzf not found"
    command -v fd && echo "PASS: fd installed" || echo "FAIL: fd not found"
    command -v rg && echo "PASS: ripgrep installed" || echo "FAIL: ripgrep not found"
    command -v bat && echo "PASS: bat installed" || echo "FAIL: bat not found"
}

test_git_config() {
    git config --get rerere.enabled && echo "PASS: rerere enabled" || echo "WARN: rerere disabled"
    git config --get pull.rebase && echo "PASS: pull.rebase set" || echo "WARN: pull.rebase not set"
}

test_vscode() {
    code --version && echo "PASS: VS Code installed" || echo "FAIL: VS Code not found"
    code --list-extensions | grep -q "esbenp.prettier-vscode" && \
        echo "PASS: Prettier extension" || echo "WARN: Prettier not installed"
}

# Run tests
test_docker
test_cli_tools
test_git_config
test_vscode
```

## Contributing

When adding new skills:

1. **Extract working configs** - Real configurations that work
2. **Include installation steps** - How to set up from scratch
3. **Show integration patterns** - How tools work together
4. **Add troubleshooting** - Common issues and fixes
5. **Update this README** - Add to the skills table

## Related Resources

- [Docker Documentation](https://docs.docker.com/)
- [VS Code Documentation](https://code.visualstudio.com/docs)
- [Pro Git Book](https://git-scm.com/book/)
- [Raycast Script Commands](https://github.com/raycast/script-commands)
- [Modern Unix Tools](https://github.com/ibraheemdev/modern-unix)
- [Dotfiles Community](https://dotfiles.github.io/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 5 devtools skills

---

*These skills establish efficient development environments, reducing friction between idea and implementation across the workspace-hub ecosystem.*
