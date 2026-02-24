# Module: infrastructure

**Path**: `src/digitalmodel/infrastructure/`
**Status**: refactoring
**Last updated**: 2026-02-24

## Purpose

Cross-cutting foundation layer providing configuration management, abstract solver
contracts, database persistence, caching, provenance tracking, and shared utilities
that every domain module depends on. No other digitalmodel module is a dependency
of this package.

## Key Packages

| Package | Responsibility | Key Classes/Functions |
|---------|---------------|-----------------------|
| `config/` | Runtime YAML loading, registry, settings | `ConfigRegistry`, `Settings`, `compat.py` |
| `base_configs/` | Canonical domain YAML configs + schema validation | `ConfigLoader`, `SchemaValidator`, `ConfigManager`; `domains/` (26 YAML bundles) |
| `base_solvers/` | Abstract solver framework, protocols, benchmarks | `BaseSolver`, `ConfigurableSolver`, `AnalysisSolver`, `SolverStatus`, `SolverProtocol`; `structural/` sub-solvers; `benchmarks/` suite |
| `calculations/` | Shared math utilities | `plate_buckling.py` (one module; duplicates `common/plate_buckling.py` — one is dead) |
| `common/` | Mixed engineering solvers, I/O helpers, visualization | 52 files: CP calcs, DNV hydro loads, FEA helpers, fatigue routing, math solvers, VIV components, OrcaFlex helpers, well path geometry |
| `core/` | Persistence, caching, provenance | `DatabaseManager`, `SQLiteDatabase`, `CacheManager`, `ProvenanceTracker` |
| `domains/` | Domain registration and legacy scripts | `platecapacity/` (plate buckling scripts), `reservoir/` (stratigraphic stub) |
| `services/` | File I/O, report rendering, digital twin feed | `digitaltwinfeed/`, `dtf/`, example SPA/blueprint templates (68 files) |
| `templates/` | Jinja2 HTML report templates | 1 Python file + 5 YAML templates |
| `transformation/` | Chained data transformation pipeline | `TransformationPipeline` (1 file) |
| `validation/` | JSON/YAML schema validators | `DataValidator`, `pipeline.py`, `ValidationPipeline` |
| `validators/` | Deprecated domain-specific shims | `ValidationUtils` (2 files — deprecated, shim only) |

## Inputs / Outputs

- **Inputs**: YAML config files (`base_configs/domains/`), environment variables
  (database URIs, API keys), `.env` for local dev (gitignored)
- **Outputs**: validated configuration objects, solver result objects, SQLite/RDBMS
  persisted records, provenance audit entries, rendered HTML reports

## External Dependencies

- `sqlalchemy` — ORM and connection pooling (MSSQL, PostgreSQL, SQLite)
- `pydantic` / `marshmallow` — schema validation
- `jinja2` — HTML report templating
- `scipy`, `numpy` — math utilities in `common/` and `base_solvers/`
- No other `digitalmodel` domain is a dependency (foundation layer)

## Known Gaps / Open Work

- `common/` is a 52-file monolith mixing solvers, I/O, and visualization — the
  primary target of WRK-415 Phase 2 migration
- `calculations/plate_buckling.py` duplicates `common/plate_buckling.py`; dead
  reference must be resolved before WRK-415 migration
- `base_solvers/` stub sub-packages (`fatigue/`, `marine/`, `signal/`,
  `specialized/`, `utils/`) contain only `__init__.py` — reserved placeholders,
  no implementation yet
- `domains/` mixes domain logic with legacy scripts (e.g., raw `plateBucklingCal_*`
  scripts) — these should migrate to `structural/` or be deleted
- `validators/` is a deprecated shim pointing to the main `validation/` package;
  target removal once callers are updated

## Related WRK Items

- WRK-415: Infrastructure refactor — audit complete (Phase 1); migration to
  `config/`, `solvers/`, `persistence/`, `validation/`, `utils/` target dirs pending
  (Phase 2)
- WRK-416: Architecture spec authoring for 5 core domains (this file)
