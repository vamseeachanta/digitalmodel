# Skills Library

> **Architecture Note**: This folder contains **tool/technology documentation**. For domain agents, see `agents/`. For framework agents, see `.claude/agents/`. For the full architecture explanation, see [.claude/docs/ARCHITECTURE.md](../.claude/docs/ARCHITECTURE.md).

## Purpose

Skills are reusable knowledge bases documenting tools, technologies, and best practices. They provide reference material for developers and AI agents.

## Structure

```
skills/
├── ai-prompting/      # LangChain, DSPy, prompt engineering
├── automation/        # Airflow, n8n, GitHub Actions
├── communication/     # Slack, Teams, APIs
├── data-analysis/     # Polars, Pandas, Plotly
├── devtools/          # Docker, Git, VS Code
├── documentation/     # Sphinx, MkDocs, Docusaurus
├── office-docs/       # python-docx, openpyxl, pypdf
└── productivity/      # Notion, Todoist, Obsidian
```

## Skill File Format

Each skill has a `SKILL.md` file with:
- YAML frontmatter (name, version, capabilities, tags)
- When to use the skill
- Prerequisites
- Core capabilities with examples
- Complete examples

## NOT for

- Agent implementations (use `agents/`)
- Business logic code (use `src/`)
- Utility scripts (use `scripts/` or `modules/`)
