# Repository Folder Conventions

## Root-Level Folders

| Folder | Purpose |
|--------|---------|
| `src/` | Source code (Python packages) |
| `tests/` | Test files (pytest) |
| `docs/` | Documentation (under `docs/modules/`) |
| `scripts/` | Utility scripts (including `scripts/testing/`) |
| `config/` | Configuration files |
| `data/` | Data files |
| `examples/` | Example code and workflows |
| `specs/` | Specifications and plans |
| `benchmarks/` | Performance benchmarks |
| `templates/` | Template files |

## Agent-Related Folders

| Folder | Purpose |
|--------|---------|
| `agents/` | Domain-specific agents (orcaflex, aqwa, cad, etc.) |
| `.claude/agents/` | Framework/system agents (core, sparc, github, etc.) |
| `skills/` | Reusable skill definitions |
| `modules/` | Module definitions (separate from src/) |

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
| `.agent-os/` | Agent operating system standards |
| `.ai/` | AI-specific configurations |
| `.hive-mind/` | Collective intelligence runtime |
| `.swarm/` | Swarm coordination runtime |

## File Naming Conventions

- Test files: `test_*.py` or `*_test.py`
- Configuration: `*.yml`, `*.yaml`, `*.json`
- Documentation: `*.md`
- Specifications: `specs/modules/<module_name>/`

## Root Directory Rules

The following should NOT be in the root directory:
- Test files (`test_*.py`) - move to `tests/` or `scripts/testing/`
- Log files (`*.log`) - move to `logs/`
- Temporary files (`tmp*`, `temp*`) - auto-ignored
- Generated coverage files - auto-ignored
- Backup files (`*.bak`, `*.old`) - auto-ignored
