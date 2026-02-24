# Docusaurus Documentation Skill

> **Quick Reference Guide**

## Overview

Build modern documentation websites with Docusaurus. React-based with MDX support, versioning, i18n, and powerful search integration.

**Version**: 1.0.0
**Category**: Documentation
**Platforms**: Linux, macOS, Windows

## Quick Start

### 1. Create Project

```bash
# Create new Docusaurus site
npx create-docusaurus@latest my-website classic

# With TypeScript
npx create-docusaurus@latest my-website classic --typescript

cd my-website
npm start
```

### 2. Project Structure

```
my-website/
├── blog/                # Blog posts
├── docs/                # Documentation
├── src/
│   ├── components/      # React components
│   ├── css/             # Custom styles
│   └── pages/           # Custom pages
├── docusaurus.config.js # Main config
└── sidebars.js          # Sidebar config
```

### 3. Build and Deploy

```bash
npm run build
npm run deploy   # GitHub Pages
```

## Key Features

- **React-based** - Full React/JSX support
- **MDX** - Markdown with embedded React
- **Versioning** - Multi-version documentation
- **i18n** - Internationalization support
- **Blog** - Built-in blog functionality
- **Search** - Algolia DocSearch or local search
- **Themes** - Customizable themes

## Admonitions

```markdown
:::note
This is a note.
:::

:::tip
This is a tip.
:::

:::warning
This is a warning.
:::
```

## Essential Commands

| Command | Description |
|---------|-------------|
| `npm start` | Start dev server |
| `npm run build` | Build static site |
| `npm run deploy` | Deploy to GitHub Pages |
| `npm run clear` | Clear cache |

## When to Use

**USE for**: React projects, interactive docs, versioned APIs, i18n sites

**AVOID for**: Simple docs (use MkDocs), Python autodoc (use Sphinx)

## Files

```
docusaurus/
├── SKILL.md      # Full documentation
└── README.md     # This quick reference
```

## Related Skills

- **mkdocs** - Simple static docs
- **sphinx** - Python API documentation

## Resources

- [Docusaurus Docs](https://docusaurus.io/docs)
- [GitHub](https://github.com/facebook/docusaurus)
- [Showcase](https://docusaurus.io/showcase)

---

See `SKILL.md` for complete configuration, i18n setup, plugins, and troubleshooting.
