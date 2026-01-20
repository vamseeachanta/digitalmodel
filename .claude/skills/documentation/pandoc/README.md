# Pandoc Universal Document Converter Skill

> **Quick Reference Guide**

## Overview

Convert documents between 40+ formats with Pandoc. Transform Markdown to PDF, DOCX, HTML, LaTeX with templates, citations, and automation.

**Version**: 1.0.0
**Category**: Documentation
**Platforms**: Linux, macOS, Windows

## Quick Start

### 1. Installation

```bash
# macOS
brew install pandoc

# Ubuntu/Debian
sudo apt-get install pandoc texlive-xetex

# Windows
choco install pandoc
```

### 2. Basic Conversions

```bash
# Markdown to PDF
pandoc document.md -o document.pdf

# Markdown to DOCX
pandoc document.md -o document.docx

# Markdown to HTML
pandoc document.md -o document.html --standalone
```

### 3. PDF with Options

```bash
pandoc document.md -o document.pdf \
    --pdf-engine=xelatex \
    --toc \
    --number-sections
```

### 4. With Citations

```bash
pandoc document.md -o document.pdf \
    --citeproc \
    --bibliography=refs.bib
```

## Key Features

- **40+ Formats** - Markdown, PDF, DOCX, HTML, LaTeX, ePub, and more
- **Templates** - Custom LaTeX and DOCX templates
- **Citations** - BibTeX/CSL bibliography support
- **Cross-refs** - Figure, table, equation references
- **Filters** - Lua filters for content transformation
- **Batch Processing** - Automate multi-file conversion

## Common Conversions

| From | To | Command |
|------|-----|---------|
| Markdown | PDF | `pandoc doc.md -o doc.pdf` |
| Markdown | DOCX | `pandoc doc.md -o doc.docx` |
| Markdown | HTML | `pandoc doc.md -o doc.html -s` |
| DOCX | Markdown | `pandoc doc.docx -o doc.md` |
| HTML | Markdown | `pandoc page.html -o page.md` |

## When to Use

**USE for**: Format conversion, PDF generation, academic papers, batch processing
**AVOID for**: Documentation sites (use MkDocs), interactive docs (use web frameworks)

## Document Metadata

```markdown
---
title: "My Document"
author: "Your Name"
date: "2026-01-17"
toc: true
bibliography: refs.bib
---
```

## Files

```
pandoc/
├── SKILL.md      # Full documentation (900+ lines)
└── README.md     # This quick reference
```

## Related Skills

- **mkdocs** - Documentation websites
- **sphinx** - Python documentation
- **marp** - Markdown presentations

## Resources

- [Pandoc Manual](https://pandoc.org/MANUAL.html)
- [Lua Filters](https://pandoc.org/lua-filters.html)
- [CSL Styles](https://citationstyles.org/)

---

See `SKILL.md` for templates, citations, Lua filters, and batch conversion scripts.
