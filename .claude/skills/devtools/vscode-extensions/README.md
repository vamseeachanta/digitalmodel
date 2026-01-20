# VS Code Extensions Skill

> **Quick Reference Guide**

## Overview

VS Code productivity optimization with essential extensions, settings sync, profiles, keybindings, snippets, and workspace configuration.

**Version**: 1.0.0
**Category**: devtools
**Platforms**: linux, macos, windows

## Quick Start

### 1. Install Essential Extensions

```bash
# Core productivity
code --install-extension esbenp.prettier-vscode
code --install-extension dbaeumer.vscode-eslint
code --install-extension eamodio.gitlens
code --install-extension usernamehw.errorlens

# Python
code --install-extension ms-python.python
code --install-extension ms-python.vscode-pylance
code --install-extension ms-python.black-formatter
```

### 2. Apply Core Settings

```jsonc
// settings.json
{
    "editor.fontSize": 14,
    "editor.formatOnSave": true,
    "editor.minimap.enabled": false,
    "files.autoSave": "onFocusChange",
    "git.autofetch": true
}
```

### 3. Export/Import Extensions

```bash
# Export
code --list-extensions > extensions.txt

# Import
cat extensions.txt | xargs -L 1 code --install-extension
```

## Key Capabilities

- **Extension Management**: Install, export, import extensions
- **Settings Configuration**: User, workspace, language-specific
- **Keybindings**: Custom keyboard shortcuts
- **Snippets**: Code templates for rapid development
- **Tasks**: Build, test, lint automation
- **Debug Configurations**: Multi-language debugging
- **Profiles**: Switch between different setups

## Extension Categories

| Category | Key Extensions |
|----------|---------------|
| Formatting | Prettier, ESLint, EditorConfig |
| Git | GitLens, Git Graph |
| Python | Python, Pylance, Black, Ruff |
| TypeScript | ESLint, Prettier, React snippets |
| Docker | Docker, Remote Containers |
| Themes | Material Icon Theme, GitHub Theme |

## Common Keybindings

| Key | Action |
|-----|--------|
| Ctrl+` | Toggle terminal |
| Ctrl+B | Toggle sidebar |
| Ctrl+Shift+P | Command palette |
| Ctrl+P | Quick file open |
| Ctrl+Shift+F | Search in files |
| F12 | Go to definition |
| Alt+Up/Down | Move line |

## Files

```
vscode-extensions/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Configuration Files

```
.vscode/
├── settings.json      # Project settings
├── extensions.json    # Recommended extensions
├── tasks.json         # Build tasks
└── launch.json        # Debug configurations
```

## Related Skills

- **cli-productivity** - Terminal efficiency
- **git-advanced** - Advanced git workflows
- **docker** - Container development

## Resources

- [VS Code Docs](https://code.visualstudio.com/docs)
- [Extension Marketplace](https://marketplace.visualstudio.com/vscode)
- [Keybindings Reference](https://code.visualstudio.com/docs/getstarted/keybindings)

---

**See SKILL.md for complete documentation with settings, snippets, and configurations.**
