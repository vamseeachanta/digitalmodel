# Modules Directory Reorganization Plan

---
title: Modules Directory Reorganization
description: Relocate files from modules/ to appropriate module-based locations
version: 1.0.0
module: infrastructure
status: completed
session:
  id: idempotent-herding-crescent
  agent: claude-opus-4-5
review:
  status: completed
  cross_review_iterations: 0
completed_at: 2026-01-19T21:55:00Z
---

## Summary

Relocate 13 files from `modules/` directory to their appropriate locations following the repository's 5-layer architecture.

## Current State

```
modules/
├── README.md
├── automation/
│   ├── agent_orchestrator.sh      (411 lines)
│   ├── agent_orchestrator.sh.backup
│   ├── gate_pass_review.sh        (290 lines)
│   └── update_ai_agents_daily.sh  (167 lines)
├── config/
│   ├── ai-agents-registry.json    (19 KB)
│   └── workflow-templates.json    (15 KB)
├── orcaflex/
│   ├── README.md
│   ├── FILE_REQUIREMENTS.md       (9.3 KB)
│   ├── examples/                  (empty)
│   └── utils/
│       ├── __init__.py
│       └── validate_bom.py        (189 lines)
└── reporting/
    ├── templates/
    │   └── plotly_report_template.py (12 KB)
    └── utils/
        └── path_utils.py          (140 lines)
```

## Relocation Plan

### 1. Shell Scripts → `scripts/automation/`

| Source | Destination |
|--------|-------------|
| `modules/automation/agent_orchestrator.sh` | `scripts/automation/agent_orchestrator.sh` |
| `modules/automation/gate_pass_review.sh` | `scripts/automation/gate_pass_review.sh` |
| `modules/automation/update_ai_agents_daily.sh` | `scripts/automation/update_ai_agents_daily.sh` |
| `modules/automation/agent_orchestrator.sh.backup` | **DELETE** (backup shouldn't be in VCS) |

**Path updates required in scripts**:
```bash
# OLD: REGISTRY_FILE="$WORKSPACE_ROOT/modules/config/ai-agents-registry.json"
# NEW: REGISTRY_FILE="$WORKSPACE_ROOT/config/ai-agents-registry.json"
```

### 2. Configuration Files → `config/`

| Source | Destination | Notes |
|--------|-------------|-------|
| `modules/config/ai-agents-registry.json` | `config/ai-agents-registry.json` | Central registry |
| `modules/config/workflow-templates.json` | **MERGE** into `config/workflow-templates-enhanced.yaml` | Consolidate, then delete JSON |

### 3. OrcaFlex Documentation → `docs/modules/orcaflex/`

| Source | Destination |
|--------|-------------|
| `modules/orcaflex/FILE_REQUIREMENTS.md` | `docs/modules/orcaflex/FILE_REQUIREMENTS.md` |
| `modules/orcaflex/README.md` | `docs/modules/orcaflex/utilities/README.md` |

### 4. Python Utilities → `src/digitalmodel/modules/`

| Source | Destination |
|--------|-------------|
| `modules/orcaflex/utils/__init__.py` | `src/digitalmodel/modules/orcaflex/utils/__init__.py` |
| `modules/orcaflex/utils/validate_bom.py` | `src/digitalmodel/modules/orcaflex/utils/validate_bom.py` |
| `modules/reporting/templates/plotly_report_template.py` | `src/digitalmodel/modules/reporting/report_generator.py` |
| `modules/reporting/utils/path_utils.py` | `src/digitalmodel/modules/reporting/path_utils.py` |

**Note**:
- Creates new `src/digitalmodel/modules/reporting/` module
- `path_utils.py` has hardcoded path calculation that needs updating for new location

### 5. Empty Directories & README

| Item | Action |
|------|--------|
| `modules/orcaflex/examples/` | **DELETE** (empty placeholder) |
| `modules/README.md` | **UPDATE** to redirect to new locations, then archive |

## Execution Steps

1. **Create target directories**
   ```bash
   mkdir -p scripts/automation
   mkdir -p docs/modules/orcaflex/utilities
   mkdir -p src/digitalmodel/modules/orcaflex/utils
   mkdir -p src/digitalmodel/modules/reporting
   ```

2. **Move files with git mv** (preserves history)
   ```bash
   # Shell scripts
   git mv modules/automation/agent_orchestrator.sh scripts/automation/
   git mv modules/automation/gate_pass_review.sh scripts/automation/
   git mv modules/automation/update_ai_agents_daily.sh scripts/automation/

   # Config files
   git mv modules/config/ai-agents-registry.json config/

   # OrcaFlex docs
   git mv modules/orcaflex/FILE_REQUIREMENTS.md docs/modules/orcaflex/
   git mv modules/orcaflex/README.md docs/modules/orcaflex/utilities/README.md

   # Python utilities
   git mv modules/orcaflex/utils/validate_bom.py src/digitalmodel/modules/orcaflex/utils/
   git mv modules/reporting/templates/plotly_report_template.py src/digitalmodel/modules/reporting/report_generator.py
   git mv modules/reporting/utils/path_utils.py src/digitalmodel/modules/reporting/
   ```

3. **Merge workflow-templates.json into enhanced YAML**
   - Read `modules/config/workflow-templates.json`
   - Merge content into `config/workflow-templates-enhanced.yaml`
   - Delete the JSON file after merge

4. **Update hardcoded paths**
   - `scripts/automation/agent_orchestrator.sh`: Update REGISTRY_FILE path
   - `src/digitalmodel/modules/reporting/path_utils.py`: Update `get_project_root()` calculation

5. **Delete items**
   ```bash
   rm modules/automation/agent_orchestrator.sh.backup
   rm modules/config/workflow-templates.json  # After merge
   rm -rf modules/orcaflex/examples
   ```

6. **Create __init__.py files for new reporting module**
   - Create `src/digitalmodel/modules/reporting/__init__.py`
   - Add exports to `src/digitalmodel/modules/orcaflex/utils/__init__.py`

7. **Clean up modules/ directory**
   - Update `modules/README.md` with redirection notice
   - Remove empty subdirectories

## Verification

1. **Pre-move baseline**
   ```bash
   uv run pytest tests/ -q
   ```

2. **Post-move checks**
   ```bash
   # Verify Python imports
   uv run python -c "from digitalmodel.orcaflex.utils.validate_bom import BOMValidator"
   uv run python -c "from digitalmodel.modules.reporting.report_generator import PlotlyReportGenerator"

   # Verify scripts execute
   bash scripts/automation/agent_orchestrator.sh --help

   # Search for broken references
   grep -r "modules/config" --include="*.sh" --include="*.py" .
   grep -r "modules/automation" --include="*.sh" --include="*.py" .
   ```

3. **Run tests**
   ```bash
   uv run pytest tests/ -q
   ```

## Critical Files to Modify

1. `scripts/automation/agent_orchestrator.sh` - Update REGISTRY_FILE path
2. `src/digitalmodel/modules/reporting/path_utils.py` - Update path calculation
3. `src/digitalmodel/modules/orcaflex/utils/__init__.py` - Add BOMValidator export
4. `src/digitalmodel/modules/reporting/__init__.py` - Create with exports
5. `config/workflow-templates-enhanced.yaml` - Merge JSON content

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Breaking script references | Update paths before committing |
| Breaking Python imports | Run import tests after each move |
| Lost git history | Use `git mv` for all moves |
| Stale documentation | Global search for "modules/" after completion |

## Post-Reorganization State

```
modules/
└── README.md (redirect notice only)

scripts/automation/
├── agent_orchestrator.sh
├── gate_pass_review.sh
└── update_ai_agents_daily.sh

config/
├── ai-agents-registry.json (NEW)
├── workflow-templates-enhanced.yaml (UPDATED - merged JSON content)
└── ... (existing files)

docs/modules/orcaflex/
├── FILE_REQUIREMENTS.md (NEW)
├── utilities/README.md (NEW)
└── ... (existing files)

src/digitalmodel/modules/
├── orcaflex/
│   └── utils/
│       └── validate_bom.py (NEW)
└── reporting/ (NEW MODULE)
    ├── __init__.py
    ├── report_generator.py
    └── path_utils.py
```
