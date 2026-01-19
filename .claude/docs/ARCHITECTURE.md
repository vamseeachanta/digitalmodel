# Repository Architecture

## Overview

This repository uses a **layered architecture** separating concerns across multiple folders. Understanding this structure is essential for contributing effectively.

## Folder Hierarchy

```
User Request
    │
    ▼
┌─────────────────────────────────────┐
│  .claude/agents/                    │  ← Framework Layer
│  Framework coordination agents      │     (SPARC, GitHub, Swarm, etc.)
│  Natural language triggers          │
│  Tool restrictions & capabilities   │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│  agents/                            │  ← Domain Layer
│  Domain-specific agents             │     (OrcaFlex, AQWA, FreeCAD, GMSH)
│  Full implementations + tests       │
│  YAML configurations               │
└─────────────────────────────────────┘
    │
    │ references
    ▼
┌─────────────────────────────────────┐
│  skills/                            │  ← Knowledge Layer
│  Tool/technology documentation      │     (Polars, Airflow, Docker, etc.)
│  Best practices & patterns          │
│  Integration guides                 │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│  modules/                           │  ← Utilities Layer
│  Configuration & orchestration      │     (File specs, automation scripts)
│  Support documentation              │
│  Utility scripts                    │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│  src/digitalmodel/                  │  ← Implementation Layer
│  Production Python code             │     (Calculations, APIs, services)
│  Business logic                     │
│  Standards implementations          │
└─────────────────────────────────────┘
```

## Folder Purposes

### `.claude/agents/` - Framework Agents

**Purpose**: Claude Code sub-agent definitions for development workflows.

**Contains**:
- Markdown files with YAML frontmatter
- Natural language trigger patterns
- Tool restrictions and capabilities
- Coordination patterns (SPARC, swarm, consensus)

**Categories**:
| Category | Purpose |
|----------|---------|
| `core/` | Basic agents: coder, tester, reviewer, planner, researcher |
| `sparc/` | SPARC methodology: specification, pseudocode, architecture, refinement |
| `github/` | GitHub workflows: PR management, code review, releases |
| `testing/` | Test specialists: unit, integration, e2e, performance |
| `devops/` | Infrastructure: CI/CD, monitoring, deployment |
| `swarm/` | Coordination: mesh, hierarchical, adaptive topologies |

**NOT for**: Domain-specific expertise (use `agents/` instead)

---

### `agents/` - Domain Agents

**Purpose**: Production agents with domain expertise and full implementations.

**Contains**:
- Python implementations
- YAML/JSON configurations
- Tests and documentation
- Platform mappings (Claude, OpenAI, LangChain)

**Examples**:
| Agent | Domain |
|-------|--------|
| `orcaflex/` | Hydrodynamic analysis, offshore modeling |
| `aqwa/` | Wave diffraction, radiation analysis |
| `freecad/` | CAD automation with NLP |
| `gmsh/` | Mesh generation, batch processing |

**NOT for**: Framework coordination (use `.claude/agents/` instead)

---

### `skills/` - Knowledge Base

**Purpose**: Reusable documentation about tools and technologies.

**Contains**:
- SKILL.md files with structured documentation
- Best practices and patterns
- Integration guides and examples

**Categories**:
| Category | Examples |
|----------|----------|
| `ai-prompting/` | LangChain, DSPy, prompt engineering |
| `automation/` | Airflow, n8n, GitHub Actions |
| `data-analysis/` | Polars, Pandas, Plotly |
| `devtools/` | Docker, Git, VS Code |

**NOT for**: Implementations (use `agents/` or `src/`)

---

### `modules/` - Utilities & Configuration

**Purpose**: Support utilities, configuration, and documentation.

**Contains**:
- File format specifications
- Automation orchestration scripts
- Configuration registries
- Utility documentation

**Examples**:
| Module | Purpose |
|--------|---------|
| `orcaflex/` | File encoding specs, validation utilities |
| `automation/` | Agent orchestration scripts |
| `config/` | AI agent registry, workflow configs |
| `reporting/` | Plotly templates, path utilities |

**NOT for**: Business logic (use `src/digitalmodel/modules/`)

---

### `src/digitalmodel/modules/` - Implementation Code

**Purpose**: Production application code with business logic.

**Contains**:
- Python modules for calculations
- Standards implementations (DNV, API, ASME)
- API endpoints and services

**Examples**:
| Module | Purpose |
|--------|---------|
| `aqwa/` | AQWA file parsing and analysis |
| `catenary/` | Riser calculations |
| `mooring/` | Mooring analysis |
| `orcaflex/` | Model generation and post-processing |

---

## Common Confusion Points

### Q: Why both `agents/orcaflex/` and `modules/orcaflex/`?

**Different layers**:
- `agents/orcaflex/` = AI expertise layer (prompts, knowledge, capabilities)
- `modules/orcaflex/` = Utility layer (file specs, validation scripts)
- `src/digitalmodel/modules/orcaflex/` = Implementation layer (Python code)

They work together:
```
User asks about OrcaFlex
    → agents/orcaflex/ provides domain expertise
    → modules/orcaflex/ handles file validation
    → src/digitalmodel/modules/orcaflex/ executes calculations
```

### Q: Why both `agents/` and `.claude/agents/`?

**Different scope**:
- `.claude/agents/` = Framework agents (generic development tasks)
- `agents/` = Domain agents (specialized technical domains)

Framework agents coordinate; domain agents execute.

### Q: Why both `skills/automation/` and `modules/automation/`?

**Different purpose**:
- `skills/automation/` = Documentation (what tools exist, how to use them)
- `modules/automation/` = Execution (actual orchestration scripts)

---

## Where to Add New Content

| You want to... | Add to... |
|----------------|-----------|
| Create a Claude Code workflow agent | `.claude/agents/<category>/` |
| Add domain expertise (new engineering field) | `agents/<domain>/` |
| Document a tool or library | `skills/<category>/<tool>/` |
| Add utility scripts for a module | `modules/<module>/` |
| Implement business logic | `src/digitalmodel/modules/<module>/` |
| Add tests | `tests/` |
| Add examples | `examples/` |
| Write specifications | `specs/modules/<module>/` |
