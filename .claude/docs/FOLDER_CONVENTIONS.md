# Repository Folder Conventions

> For detailed architecture explanation, see [ARCHITECTURE.md](ARCHITECTURE.md)

## Root-Level Folders

| Folder | Purpose |
|--------|---------|
| `src/` | Source code (Python packages) |
| `tests/` | Test files (pytest) |
| `docs/` | Documentation (under `docs/domains/`) |
| `scripts/` | Utility scripts (including `scripts/testing/`) |
| `config/` | Configuration files |
| `data/` | Data files |
| `examples/` | Example code and workflows |
| `specs/` | Specifications and plans |
| `benchmarks/` | Performance benchmarks |
| `templates/` | Template files |

## Agent-Related Folders (Key Distinction)

These folders serve **different architectural layers** - not duplicates:

| Folder | Layer | Purpose | Content Type |
|--------|-------|---------|--------------|
| `.claude/agents/` | Framework | Claude Code workflow coordination | Markdown (triggers, tools) |
| `agents/` | Domain | Specialized technical expertise | Python + YAML + tests |
| `skills/` | Knowledge | Tool/technology documentation | Markdown (SKILL.md) |
| `modules/` | Utilities | Configuration & orchestration | Scripts + configs |

### Why Multiple "Agent" Folders?

```
.claude/agents/     = HOW to coordinate (framework level)
agents/             = WHAT domain expertise (domain level)
skills/             = WHAT tools to use (knowledge level)
modules/            = HOW to configure (utilities level)
src/.../modules/    = HOW to implement (code level)
```

**Example flow**:
1. User asks about OrcaFlex analysis
2. `.claude/agents/` routes to appropriate workflow
3. `agents/orcaflex/` provides domain expertise
4. `skills/data-analysis/` provides tool knowledge
5. `modules/orcaflex/` handles file validation
6. `src/digitalmodel/modules/orcaflex/` executes calculations

## Commonly Confused Pairs

| Folder A | Folder B | Relationship |
|----------|----------|--------------|
| `agents/orcaflex/` | `modules/orcaflex/` | Complementary (AI + utilities) |
| `agents/` | `.claude/agents/` | Hierarchical (domain + framework) |
| `skills/automation/` | `scripts/automation/` | Orthogonal (docs + scripts) |
| `modules/orcaflex/` | `src/.../orcaflex/` | Different layers (config + code) |

## Runtime Folders (Not Tracked in Git)

| Folder | Purpose |
|--------|---------|
| `.claude-flow/` | Agent runtime state and memory |
| `logs/` | Application logs |
| `outputs/` | Generated outputs |
| `reports/` | Generated reports |
| `cache/` | Cache files |
| `htmlcov/` | Coverage HTML reports |

## Hidden Configuration Folders

| Folder | Purpose |
|--------|---------|
| `.claude/` | Claude Code configuration and agents |
| `.claude-flow/` | Unified runtime state, memory, and metrics |
| `.agent-os/` | Agent OS standards and tooling (Python implementations) |
| `.ai/` | Project-specific AI configs and guidance (markdown/settings) |

> **Notes**:
> - `.hive-mind/` and `.swarm/` have been consolidated into `.claude-flow/`. Backups in `.claude-flow/archives/`.
> - `.agent-os/` and `.ai/` are intentionally separate - see `.agent-os/integration.md` for details.

## File Naming Conventions

- Test files: `test_*.py` or `*_test.py`
- Configuration: `*.yml`, `*.yaml`, `*.json`
- Documentation: `*.md`
- Specifications: `specs/modules/<module_name>/`
- Skills: `skills/<category>/<tool>/SKILL.md`
- Agent configs: `agents/<domain>/agent.yaml`

## Root Directory Rules

The following should NOT be in the root directory:
- Test files (`test_*.py`) → `tests/` or `scripts/testing/`
- Log files (`*.log`) → `logs/`
- Temporary files (`tmp*`, `temp*`) → auto-ignored
- Generated coverage files → auto-ignored
- Backup files (`*.bak`, `*.old`) → auto-ignored

## Quick Reference: Where to Add Content

| Adding... | Location |
|-----------|----------|
| Claude Code workflow agent | `.claude/agents/<category>/` |
| Domain expertise agent | `agents/<domain>/` |
| Tool/library documentation | `skills/<category>/<tool>/` |
| Utility scripts | `modules/<module>/` or `scripts/` |
| Business logic code | `src/digitalmodel/modules/<module>/` |
| Tests | `tests/` |
| Examples | `examples/` |
| Specifications | `specs/modules/<module>/` |
