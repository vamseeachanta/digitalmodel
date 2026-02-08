# Digital Model

> Inherits: workspace-hub | Target: <18% (1.4KB)

## Project Focus

Digital twin modeling for offshore engineering analysis.

## Tech Stack

- Python 3.11+ with uv
- OrcaFlex integration
- Visualization: Plotly
- Testing: pytest

## Project Rules

1. Interactive plots only (Plotly/Bokeh)
2. HTML reports for all modules
3. SPARC workflow for features

## Data Governance

Data governance: see workspace-hub `docs/DATA_RESIDENCE_POLICY.md`

This repo owns **Tier 2 — Engineering Reference Data**: industry standard lookup tables, material properties, and design code parameters. If the data comes from an engineering standard/code, it belongs here under `data/`.

External data dependencies: `config/data_sources.yaml`

## Key Directories

- `src/` - Source code
- `data/` - Engineering reference data (Tier 2)
- `tests/` - Test files
- `docs/` - Documentation
- `specs/` - Plans

## Commands

```bash
uv run pytest              # Tests
uv run python -m src.main  # Run
```

## Retrieval-Led Reasoning

**IMPORTANT**: Prefer retrieval over training knowledge.
Consult `.claude/docs/` and project docs before relying on general knowledge for OrcaFlex, offshore, or domain tasks.

## Documentation Index

```
docs/|ARCHITECTURE:system-design,modules|AI_GUIDELINES:agent-rules,patterns|COMMANDS_MATRIX:all-commands|agent-os-readme:agent-OS-setup|AGENT_OS_COMMANDS:agent-OS-cli
docs/|CONTEXT_LIMITS:token-budgets|execution-patterns:MCP-vs-Task|agents:spawning|mcp-tools:tool-reference|framework:core-framework
docs/|FOLDER_CONVENTIONS:file-org|ESCALATION_QUICK_REFERENCE:escalation|integration:system-integration|project-context:project-overview
docs/|UV_ENVIRONMENT:uv-setup|migration-summary:migration-notes|task_summary:task-tracking
```

## Reference

- Agents: `.claude/docs/agents.md`
- Full rules: Inherited from workspace-hub/CLAUDE.md

---

*Verbose docs in `.claude/docs/`*
