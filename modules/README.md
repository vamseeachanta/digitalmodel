# Modules (Utilities & Configuration)

> **Architecture Note**: This folder contains **utilities and configuration**. For implementation code, see `src/digitalmodel/modules/`. For the full architecture explanation, see [.claude/docs/ARCHITECTURE.md](../.claude/docs/ARCHITECTURE.md).

## Purpose

This folder provides supporting utilities, configuration, and documentation for complex modules. It is NOT for business logic code.

## Structure

```
modules/
├── orcaflex/      # OrcaFlex file specs, validation utilities
├── automation/    # Agent orchestration scripts
├── config/        # AI agent registry, workflow configs
└── reporting/     # Plotly templates, path utilities
```

## Relationship to Other Folders

| This Folder | vs | That Folder | Difference |
|-------------|-----|-------------|------------|
| `modules/orcaflex/` | vs | `agents/orcaflex/` | Utilities vs AI expertise |
| `modules/orcaflex/` | vs | `src/.../orcaflex/` | Config vs implementation |
| `modules/automation/` | vs | `skills/automation/` | Scripts vs documentation |

## NOT for

- Business logic (use `src/digitalmodel/modules/`)
- Domain expertise (use `agents/`)
- Tool documentation (use `skills/`)
