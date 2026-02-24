# Module: infrastructure

## Purpose
Provides the core solver framework, configuration management, database
connectivity, caching, provenance tracking, and shared utilities that all
domain modules build on; acts as the foundational layer for the entire
digitalmodel package.

## Key Classes / Functions
- `BaseSolver` (`base_solvers/base.py`): Abstract base class for all
  mathematical solvers; enforces `validate_inputs()` and `solve()` interface;
  tracks `SolverStatus` (PENDING, VALIDATING, EXECUTING, COMPLETED, FAILED)
- `SolverStatus`: Enum of execution states used by all solver subclasses
- `SolverInterfaces` (`base_solvers/interfaces.py`): Typed protocol definitions
  for domain-specific solver contracts (structural, hydrodynamic, subsea)
- `DatabaseManager` (`core/database_manager.py`): Connection pool manager
  supporting MSSQL (pool: 15), PostgreSQL (pool: 20), MongoDB (pool: 10),
  and MS Access; exponential backoff retry; HA primary-replica failover;
  SQLAlchemy QueuePool back-end
- `SQLiteDatabase` (`core/sqliteDatabase.py`): Lightweight SQLite wrapper
  for local caching and offline operation
- `CacheManager` (`core/cache.py`): TTL-based in-process result cache with
  LRU eviction; shared across solver instances within a session
- `ProvenanceTracker` (`core/provenance.py`): Records input hash, solver
  version, timestamp, and output digest for audit trails
- `ConfigRegistry` (`config/registry.py`): Loads and merges YAML config files
  from base_configs, project overrides, and environment variables
- `Settings` (`config/settings.py`): Dataclass of validated global settings
  (unit system, solver tolerances, output paths, database URIs)
- `DomainRegistry` (`domains/`): Maps domain names to solver factory
  functions; enables plugin-style domain discovery at runtime
- `ValidationUtils` (`validators/`): Input range checks, unit consistency
  validation, and schema validation against YAML specs
- `TransformationPipeline` (`transformation/`): Chained data transformation
  steps (unit conversion, normalisation, interpolation) applied before
  solving

## Sub-packages
- `base_solvers/` — Abstract solver contracts and benchmarks
- `base_configs/` — Canonical YAML configuration files for all domains
- `calculations/` — Shared mathematical utilities (integration, interpolation,
  matrix operations)
- `common/` — Cross-cutting helpers (logging setup, unit conversion tables)
- `config/` — Runtime configuration loading and registry
- `core/` — Database connections, caching, provenance
- `domains/` — Domain registration and factory lookup
- `services/` — Shared service objects (file I/O, report rendering)
- `templates/` — Jinja2 HTML report templates
- `transformation/` — Data pipeline transformations
- `validation/` — JSON/YAML schema validators
- `validators/` — Domain-specific input range validators

## Data Sources
- YAML config files: `infrastructure/base_configs/` (committed to repo)
- Environment variables: database URIs, API keys (never committed)
- `.env` files: local development only (gitignored)

## Integration Points
- **Depends on**: no other digitalmodel domain (foundation layer)
- **Used by**: every domain module — `hydrodynamics`, `structural`, `subsea`,
  `well`, `marine_ops`, `field_development`, `production_engineering`,
  `asset_integrity`, `signal_processing`, `solvers`, `workflows`, `gis`

## Status
Active
