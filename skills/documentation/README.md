# Documentation Skills Library

> Generate professional documentation, technical writing, and presentations from code
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 6 documentation-focused skills for building comprehensive documentation systems. Each skill provides patterns for generating, transforming, and publishing technical content across multiple formats and platforms.

## Quick Start

```bash
# Browse available skills
ls skills/documentation/

# Read a skill
cat skills/documentation/mkdocs/SKILL.md

# Skills are documentation - integrate patterns into your build pipelines
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [mkdocs](./mkdocs/SKILL.md) | Python-based static site generator | Material theme, search, plugins, versioning |
| [sphinx](./sphinx/SKILL.md) | Python documentation generator | RST/MyST, autodoc, intersphinx, PDF output |
| [pandoc](./pandoc/SKILL.md) | Universal document converter | 40+ formats, templates, filters, citations |
| [marp](./marp/SKILL.md) | Markdown presentation framework | Slides from markdown, themes, PDF/HTML export |
| [docusaurus](./docusaurus/SKILL.md) | React-based documentation | Versioning, i18n, MDX, blog, search |
| [gitbook](./gitbook/SKILL.md) | Modern documentation platform | Collaborative editing, integrations, analytics |

## Skill Categories

### Static Site Generators
- **mkdocs** - Simple, fast Python-based docs
- **docusaurus** - Feature-rich React-based platform
- **gitbook** - Cloud-hosted documentation

### Document Processing
- **sphinx** - Python ecosystem standard
- **pandoc** - Universal format conversion

### Presentations
- **marp** - Markdown to slides

## Skill Selection Guide

| Use Case | Recommended Skill | Why |
|----------|-------------------|-----|
| API documentation | sphinx | Autodoc, intersphinx, API reference generation |
| Project documentation | mkdocs | Simple setup, Material theme, great search |
| Multi-version docs | docusaurus | Built-in versioning, React components |
| Technical presentations | marp | Markdown slides, themeable, PDF export |
| Format conversion | pandoc | Universal converter, templates, automation |
| Team documentation | gitbook | Collaboration, permissions, analytics |
| Python projects | sphinx | Native Python support, docstring extraction |
| JavaScript projects | docusaurus | MDX support, React integration |

## Common Patterns Across Skills

### Project Structure
```
docs/
├── index.md              # Home page
├── getting-started/      # Quickstart guides
│   ├── installation.md
│   └── configuration.md
├── guides/               # How-to guides
│   ├── basic-usage.md
│   └── advanced.md
├── reference/            # API reference
│   └── api.md
└── assets/               # Images, diagrams
    └── images/
```

### Frontmatter Standards
```yaml
---
title: Page Title
description: Brief description for SEO
sidebar_position: 1
tags:
  - guide
  - configuration
---
```

### Admonitions
```markdown
!!! note "Title"
    This is a note admonition.

!!! warning
    This is a warning without a custom title.

!!! tip "Pro Tip"
    Helpful tips go here.
```

### Code Blocks with Highlighting
```markdown
​```python title="example.py" hl_lines="2 3"
def hello():
    message = "Hello"  # highlighted
    print(message)     # highlighted
​```
```

## Usage Examples

### MkDocs Setup
```bash
# See mkdocs for complete patterns
pip install mkdocs mkdocs-material

# Initialize project
mkdocs new my-docs
cd my-docs

# mkdocs.yml configuration
cat > mkdocs.yml << 'EOF'
site_name: My Documentation
theme:
  name: material
  palette:
    primary: indigo
  features:
    - navigation.tabs
    - search.highlight

plugins:
  - search
  - minify

nav:
  - Home: index.md
  - Getting Started: getting-started.md
  - API Reference: api.md
EOF

# Serve locally
mkdocs serve

# Build for production
mkdocs build
```

### Sphinx Autodoc
```bash
# See sphinx for complete patterns
pip install sphinx sphinx-rtd-theme

# Initialize
sphinx-quickstart docs

# conf.py configuration
cat >> docs/conf.py << 'EOF'
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
    'sphinx.ext.intersphinx',
]

autodoc_default_options = {
    'members': True,
    'undoc-members': True,
    'show-inheritance': True,
}

intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None),
}
EOF

# Generate API docs
sphinx-apidoc -o docs/api src/

# Build HTML
sphinx-build -b html docs docs/_build
```

### Pandoc Conversion
```bash
# See pandoc for complete patterns

# Markdown to PDF
pandoc README.md -o output.pdf \
    --pdf-engine=xelatex \
    --template=template.tex \
    --toc

# Markdown to Word
pandoc doc.md -o output.docx \
    --reference-doc=template.docx

# Multiple files to single document
pandoc chapter1.md chapter2.md chapter3.md \
    -o book.pdf \
    --toc \
    --number-sections

# HTML to Markdown
pandoc https://example.com/page.html -o page.md
```

### Marp Presentation
```bash
# See marp for complete patterns
npm install -g @marp-team/marp-cli

# Create presentation
cat > slides.md << 'EOF'
---
marp: true
theme: default
paginate: true
---

# Presentation Title

Speaker Name
2026-01-17

---

## Agenda

1. Introduction
2. Main Content
3. Conclusion

---

## Code Example

```python
def demo():
    return "Hello, Marp!"
```

---

<!-- _class: lead -->

# Questions?

EOF

# Export to PDF
marp slides.md -o presentation.pdf

# Export to HTML
marp slides.md -o presentation.html

# Watch mode
marp -w slides.md
```

### Docusaurus Setup
```bash
# See docusaurus for complete patterns
npx create-docusaurus@latest my-docs classic

cd my-docs

# docusaurus.config.js customization
cat > docusaurus.config.js << 'EOF'
module.exports = {
  title: 'My Documentation',
  url: 'https://docs.example.com',
  baseUrl: '/',
  themeConfig: {
    navbar: {
      title: 'My Docs',
      items: [
        { type: 'doc', docId: 'intro', label: 'Docs' },
        { to: '/blog', label: 'Blog' },
      ],
    },
  },
};
EOF

# Start development server
npm start

# Build for production
npm run build
```

## Integration with Workspace-Hub

These skills power documentation across the workspace:

```
workspace-hub/
├── docs/
│   ├── mkdocs.yml           # Uses: mkdocs
│   └── src/
│       ├── index.md
│       └── guides/
├── presentations/
│   └── slides.md            # Uses: marp
├── scripts/
│   ├── build-docs.sh        # Uses: mkdocs, sphinx
│   ├── convert-docs.sh      # Uses: pandoc
│   └── generate-api.sh      # Uses: sphinx autodoc
└── .github/workflows/
    └── docs.yml             # CI/CD for documentation
```

## Best Practices

### 1. Docs as Code
```yaml
# .github/workflows/docs.yml
name: Documentation
on:
  push:
    branches: [main]
    paths: ['docs/**']

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: pip install mkdocs-material
      - run: mkdocs build
      - uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./site
```

### 2. Consistent Structure
```bash
# Documentation template
create_doc_template() {
    local name="$1"
    cat > "docs/$name.md" << 'EOF'
# Title

## Overview

Brief description of the topic.

## Prerequisites

- Requirement 1
- Requirement 2

## Steps

### Step 1

Details...

### Step 2

Details...

## Examples

```bash
# Example code
```

## Troubleshooting

### Common Issue 1

Solution...

## Related

- [Related Doc 1](./related1.md)
EOF
}
```

### 3. Automated API Documentation
```python
# Generate API docs from docstrings
"""
Module description.

Example:
    >>> import mymodule
    >>> mymodule.function()
    'result'
"""

def function(arg: str) -> str:
    """
    Brief description.

    Args:
        arg: Description of argument.

    Returns:
        Description of return value.

    Raises:
        ValueError: When arg is invalid.
    """
    pass
```

### 4. Version Documentation
```bash
# Version docs with releases
version_docs() {
    local version="$1"

    # MkDocs with mike
    mike deploy "$version" latest -u
    mike set-default latest

    # Docusaurus
    npm run docusaurus docs:version "$version"
}
```

### 5. Link Validation
```bash
# Check for broken links
check_links() {
    local docs_dir="${1:-docs}"

    # Using markdown-link-check
    find "$docs_dir" -name '*.md' -exec markdown-link-check {} \;

    # Using linkchecker on built site
    linkchecker ./site/index.html
}
```

## Testing Documentation

Validate documentation quality:

```bash
#!/bin/bash
# test_docs.sh

test_markdown_lint() {
    markdownlint docs/**/*.md && echo "PASS: Markdown lint" || echo "FAIL: Markdown lint"
}

test_build() {
    mkdocs build --strict && echo "PASS: Build successful" || echo "FAIL: Build failed"
}

test_links() {
    find docs -name '*.md' -exec markdown-link-check -q {} \; && \
        echo "PASS: Links valid" || echo "FAIL: Broken links found"
}

test_spelling() {
    cspell docs/**/*.md && echo "PASS: Spelling" || echo "FAIL: Spelling errors"
}

# Run all tests
test_markdown_lint
test_build
test_links
test_spelling
```

## Contributing

When adding new skills:

1. **Include real configurations** - Actual config files that work
2. **Show build pipelines** - CI/CD integration examples
3. **Document theming** - Customization patterns
4. **Add migration guides** - Moving between tools
5. **Update this README** - Add to the skills table

## Related Resources

- [MkDocs Documentation](https://www.mkdocs.org/)
- [Sphinx Documentation](https://www.sphinx-doc.org/)
- [Pandoc User's Guide](https://pandoc.org/MANUAL.html)
- [Marp Documentation](https://marp.app/)
- [Docusaurus Documentation](https://docusaurus.io/)
- [Write the Docs](https://www.writethedocs.org/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 6 documentation skills

---

*These skills enable documentation-as-code workflows, ensuring technical content stays synchronized with codebases and deployments.*
