# Marp Presentation Skill

> **Quick Reference Guide**

## Overview

Create professional Markdown-based slide presentations with Marp. Export to PDF, HTML, and PPTX with custom themes, speaker notes, and code highlighting.

**Version**: 1.0.0
**Category**: Documentation
**Platforms**: Linux, macOS, Windows

## Quick Start

### 1. Installation

```bash
# Using npm
npm install -g @marp-team/marp-cli

# Using npx (no install)
npx @marp-team/marp-cli --version
```

### 2. Create Slides

```markdown
---
marp: true
theme: default
paginate: true
---

# Slide 1

First slide content

---

# Slide 2

- Point one
- Point two

---

# Thank You!
```

### 3. Build and Preview

```bash
# Preview in browser
marp -s slides.md

# Export to PDF
marp slides.md -o slides.pdf

# Export to PowerPoint
marp slides.md -o slides.pptx
```

## Key Features

- **Markdown-based** - Write slides in familiar syntax
- **Multiple Themes** - Built-in: default, gaia, uncover
- **Custom Themes** - Create CSS themes for branding
- **Speaker Notes** - HTML comments become presenter notes
- **Code Highlighting** - Syntax highlighting for 150+ languages
- **Math Support** - KaTeX for equations
- **Diagrams** - Mermaid integration

## Essential Directives

| Directive | Purpose |
|-----------|---------|
| `marp: true` | Enable Marp |
| `theme` | Set theme |
| `paginate` | Show page numbers |
| `_class` | Slide class |
| `_backgroundColor` | Background color |

## When to Use

**USE for**: Technical presentations, version-controlled decks, CI/CD automation

**AVOID for**: Complex animations, real-time collaboration

## Files

```
marp/
├── SKILL.md      # Full documentation
└── README.md     # This quick reference
```

## Related Skills

- **pandoc** - Document format conversion
- **mkdocs** - Project documentation

## Resources

- [Marp Official](https://marp.app/)
- [Marp CLI](https://github.com/marp-team/marp-cli)
- [VS Code Extension](https://marketplace.visualstudio.com/items?itemName=marp-team.marp-vscode)

---

See `SKILL.md` for complete theme customization, CI/CD workflows, and troubleshooting.
