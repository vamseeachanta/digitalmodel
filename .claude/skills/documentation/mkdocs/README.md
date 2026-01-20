# MkDocs Documentation Skill

> **Quick Reference Guide**

## Overview

Build professional project documentation with MkDocs and Material for MkDocs theme. Fast, searchable static sites with beautiful Material Design.

**Version**: 1.0.0
**Category**: Documentation
**Platforms**: Linux, macOS, Windows

## Quick Start

### 1. Installation

```bash
# Install MkDocs with Material theme
pip install mkdocs mkdocs-material

# With recommended plugins
pip install mkdocs mkdocs-material \
    mkdocs-minify-plugin \
    mkdocs-git-revision-date-localized-plugin
```

### 2. Initialize Project

```bash
mkdocs new my-docs && cd my-docs
```

### 3. Configure (mkdocs.yml)

```yaml
site_name: My Documentation
theme:
  name: material
  features:
    - navigation.tabs
    - search.highlight
    - content.code.copy
plugins:
  - search
nav:
  - Home: index.md
  - Guide: guide.md
```

### 4. Serve Locally

```bash
mkdocs serve
# Open http://127.0.0.1:8000
```

### 5. Build & Deploy

```bash
mkdocs build
mkdocs gh-deploy
```

## Key Features

- **Material Theme** - Beautiful design out of the box
- **Search** - Fast client-side search with highlighting
- **Navigation** - Tabs, sections, breadcrumbs
- **Code Blocks** - Syntax highlighting, copy button, annotations
- **Admonitions** - Notes, warnings, tips, examples
- **Diagrams** - Mermaid integration
- **Versioning** - Multi-version docs with mike
- **Plugins** - Extensible with 100+ plugins

## When to Use

**USE for**: Project docs, API guides, tutorials, internal documentation
**AVOID for**: Auto-generated Python API docs (use Sphinx), React sites (use Docusaurus)

## Essential Plugins

| Plugin | Purpose |
|--------|---------|
| `search` | Built-in search (included) |
| `minify` | Compress HTML/CSS/JS |
| `git-revision-date-localized` | Show last updated dates |
| `glightbox` | Image lightbox |
| `mike` | Documentation versioning |

## Files

```
mkdocs/
├── SKILL.md      # Full documentation (700+ lines)
└── README.md     # This quick reference
```

## Related Skills

- **sphinx** - Python API documentation
- **pandoc** - Document format conversion
- **docusaurus** - React-based documentation

## Resources

- [MkDocs Docs](https://www.mkdocs.org/)
- [Material Theme](https://squidfunk.github.io/mkdocs-material/)
- [Plugin Catalog](https://github.com/mkdocs/catalog)

---

See `SKILL.md` for complete configuration examples, GitHub Actions workflows, and troubleshooting.
