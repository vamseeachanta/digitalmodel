# Infrastructure Refactor Architecture Spec

**WRK-415** | Phase 1: Audit and Design
**Date**: 2026-02-24
**Status**: Phase 1 complete — audit and target structure defined; migration pending

---

## 1. Current Inventory

`src/digitalmodel/infrastructure/` contains **12 top-level subdirectories**
and **218 Python files** (excluding `__pycache__`). The subdirectories are:

| Subdir | Python files | Classification |
|---|---|---|
| `base_configs/` | 3 (+26 YAML domains in `domains/`) | config |
| `base_solvers/` | 27 | solvers |
| `calculations/` | 2 | solvers |
| `common/` | 52 | mixed (solvers + utils + services) |
| `config/` | 4 | config |
| `core/` | 6 | utils (persistence / cache) |
| `domains/` | 48 | mixed (domain logic + legacy scripts) |
| `services/` | 68 | services |
| `templates/` | 1 (+5 YAML templates) | utils |
| `transformation/` | 1 | solvers |
| `validation/` | 3 | validation |
| `validators/` | 2 | validation (deprecated shim) |

### 1.1 `base_configs/` — config

Provides YAML configuration loading (`ConfigLoader`), schema validation
(`SchemaValidator`), and a registry manager (`ConfigManager`). Contains a
`domains/` subtree of 26 domain-specific YAML configuration bundles (aqwa,
catenary, orcaflex, fatigue_analysis, pipeline, etc.). These are canonical
runtime config files, not Python logic.

- `config_framework.py` — ConfigLoader, SchemaValidator, ConfigManager
- `config_models.py` — ConfigModel SQLAlchemy ORM base
- `domains/` — 26 YAML domain-config directories (data only)

### 1.2 `base_solvers/` — solvers

Core solver framework: abstract base classes, typed protocol interfaces, and
solver benchmarks. Sub-packages:

- `base.py` — BaseSolver, ConfigurableSolver, AnalysisSolver, SolverStatus
- `interfaces.py` — SolverProtocol, ConfigurableSolverProtocol, AnalysisSolverProtocol
- `structural/` — structural FEM elements (beam_element, elastic_buckling,
  von_mises, matrix_operations, post_processing)
- `config/solver_config.py` — per-solver configuration schema
- `benchmarks/` — BenchmarkSuite, ConfigurationBenchmarks, SolverBenchmarks,
  ReportGenerator
- `fatigue/`, `marine/`, `signal/`, `specialized/`, `utils/` — mostly stubs
  (only `__init__.py` present, reserved for future domain solvers)

### 1.3 `calculations/` — solvers

Single module: `plate_buckling.py` containing plate buckling calculations.
Duplicates `common/plate_buckling.py`; one is a dead reference.

### 1.4 `common/` — mixed (largest problem area)

52 files mixing at least four distinct responsibilities:

**Engineering solvers / analysis:**
- `cathodic_protection.py`, `cp_DNV_RP_B401_2021.py`, `cp_DNV_RP_F103_2010.py`,
  `cp_sacrificial_anode_b401.py` — cathodic protection calculations
- `code_dnvrph103_hydrodynamics_circular.py`, `code_dnvrph103_hydrodynamics_rectangular.py`
  — DNV RP H103 hydrodynamic load calculations
- `FEAComponents.py` — finite element analysis helpers
- `fatigue_analysis.py`, `fatigue_analysis_components.py` — fatigue routing/pipeline
- `math_solvers.py` — Polynomial, Scipy_Interpolation, FFT_Methods
- `plate_buckling.py` — plate buckling (duplicate of calculations/)
- `ship_design.py`, `ship_fatigue_analysis.py` — ship structural design
- `typical_riser_stack_up_calculations.py` — riser stack-up
- `viv_analysis_components.py`, `viv_fatigue_analysis_components.py` — VIV analysis
- `shear7_model_components.py` — Shear7 VIV model
- `pipe_properties.py` — pipe cross-section properties
- `orcaflex_model_components.py` — OrcaFlex model construction helpers
- `wellpath3D.py` — 3-D well trajectory geometry

**Data I/O and ETL:**
- `data.py` — ReadData, SaveData, AttributeDict, Transform, DateTimeUtility
- `ETL_components.py` — Excel-to-YAML ETL pipeline
- `excel_utilities.py` — spreadsheet helpers
- `ymlInput.py` — YAML input utilities
- `time_series_components.py` — time-series I/O and processing
- `log_file_analysis_components.py` — log-file parsing
- `data_models_components.py` — data model components
- `ong_fd_components.py` — oil and gas field-development data access
- `compare_tool_components.py` — data comparison helpers

**Visualization:**
- `visualizations.py`, `visualizations_interactive.py`, `visualization_components.py`
  — Matplotlib/Plotly wrappers
- `visualization_unused.py` — dead code pending removal
- `plotDefault.py` — default plot styles
- `utilities/` — `data_extraction.py`, `visualization_plotly.py`,
  `data_compare.py`, `histograms.py`

**Cross-cutting utilities:**
- `application_configuration.py` — application-level config bootstrap
- `basic_statistics.py` — descriptive statistics
- `database.py` — legacy Database class / get_db_connection
- `documentation_components.py`, `finance_components.py`, `front_end_components.py`
  — doc generation, finance helpers, Flask/Dash front-end components
- `engineering_units.py` — re-export bridge to `assetutilities.units`
- `MaterialProperties.py` — material property tables
- `parallel_processing.py` — multiprocessing helpers
- `path_utils.py` — filesystem path utilities
- `send_email.py` — SMTP email dispatch
- `standards_lookup.py` — standards document discovery
- `update_deep.py` — deep-merge utility for nested dicts

### 1.5 `config/` — config

Pydantic-based global settings with environment-variable support and YAML
registry. Well-scoped; already a coherent config module.

- `settings.py` — GlobalSettings (Pydantic BaseSettings)
- `registry.py` — ConfigRegistry, ConfigValidationError
- `compat.py` — load_config backward-compat shim

### 1.6 `core/` — utils (persistence and runtime services)

Database connections, caching, and provenance. Well-scoped.

- `database_manager.py` — DatabaseManager (MSSQL/PG/MongoDB/Access pools)
- `database_legacy.py` — legacy DB wrapper
- `sqliteDatabase.py` — SQLite local cache
- `cache.py` — TTL/LRU in-process result cache
- `provenance.py` — ProvenanceTracker (input hash + audit trail)

### 1.7 `domains/` — domain logic (misplaced)

Contains two domain implementations that do not belong in `infrastructure/`:

- `platecapacity/` — full plate-buckling solver with DataProvision, calculation
  scripts, stiffener buckling, and legacy `z_superseded/` scripts
- `reservoir/` — reservoir analysis scripts (`stratigraphic.py`)

These are domain-level solvers that belong under `src/digitalmodel/structural/`
or a dedicated `src/digitalmodel/structural/plate_capacity/` path.

### 1.8 `services/` — services (web application tier; misplaced)

Contains Flask/Dash web-application blueprints. These are runtime services,
not infrastructure:

- `digitaltwinfeed/` — full Flask web app (routes, HTML templates, CALM/SALM
  buoy pages, digital twin feed)
- `dtf/` — DTF blueprint
- `example_blueprint/` — example Flask blueprint
- `example_SPA/` — example single-page app
- `TemplateCode/` — code template module

### 1.9 `templates/` — utils (config artifacts)

YAML/JSON templates for analysis runs (fatigue, reservoir, stress, plate
capacity). One Python file: `template_validator.py`. These are data artifacts
used by `base_configs/`; the validator belongs in `validation/`.

### 1.10 `transformation/` — solvers

Single file: `Transformation.router()` applies config-driven transformation
groups before solving. Belongs with solver pipeline, not standalone.

### 1.11 `validation/` — validation

Well-scoped validation pipeline:

- `pipeline.py` — ValidationPipeline, BaseValidator, domain validators
  (RangeValidator, MatrixValidator, PhysicalPlausibilityValidator,
  UnitConsistencyValidator, PolarDataValidator, TimeSeriesValidator),
  ValidationCache, generate_html_report
- `data_validator.py` — DataValidator

### 1.12 `validators/` — validation (deprecated)

Backward-compatibility shim that re-exports `DataValidator` from `validation/`
with a `DeprecationWarning`. Will be removed once all callers are migrated.

---

## 2. Responsibility Classification Summary

| Subdir | Responsibility |
|---|---|
| `base_configs/` | config |
| `config/` | config |
| `base_solvers/` | solvers |
| `calculations/` | solvers (duplicate — merge into solvers) |
| `transformation/` | solvers (pipeline step) |
| `common/` (analysis files) | solvers |
| `domains/platecapacity/`, `domains/reservoir/` | solvers (misplaced domain logic) |
| `core/` | utils |
| `common/` (I/O + ETL files) | utils |
| `common/` (viz files) | utils |
| `templates/` | utils (data artifacts) |
| `validation/` | validation |
| `validators/` | validation (deprecated shim) |
| `services/` | services (web layer — misplaced) |

---

## 3. Proposed Target Structure (≤ 5 Domain Modules)

```
src/digitalmodel/infrastructure/
├── config/             # All configuration management
│   ├── __init__.py
│   ├── settings.py         (from config/)
│   ├── registry.py         (from config/)
│   ├── compat.py           (from config/)
│   ├── framework.py        (from base_configs/config_framework.py)
│   ├── models.py           (from base_configs/config_models.py)
│   └── domains/            (from base_configs/domains/ — YAML bundles)
│
├── solvers/            # All numerical solvers and solver framework
│   ├── __init__.py
│   ├── base.py             (from base_solvers/base.py)
│   ├── interfaces.py       (from base_solvers/interfaces.py)
│   ├── structural/         (from base_solvers/structural/)
│   │   ├── buckling/
│   │   ├── elements/
│   │   ├── stress/
│   │   └── utils/
│   ├── marine/             (from base_solvers/marine/ — expand stubs)
│   ├── fatigue/            (from base_solvers/fatigue/ + common/fatigue_analysis*.py)
│   ├── hydrodynamics/      (from common/code_dnvrph103_*.py, common/cathodic_protection*.py)
│   ├── viv/                (from common/viv_analysis_components.py, viv_fatigue*)
│   ├── pipeline_solvers/   (from transformation/ + common/math_solvers.py)
│   ├── plate_capacity/     (from domains/platecapacity/ — move out of domains)
│   ├── config/             (from base_solvers/config/)
│   └── benchmarks/         (from base_solvers/benchmarks/)
│
├── persistence/        # Database, cache, provenance (rename from core/)
│   ├── __init__.py
│   ├── database_manager.py (from core/database_manager.py)
│   ├── database_legacy.py  (from core/database_legacy.py)
│   ├── sqlite.py           (from core/sqliteDatabase.py)
│   ├── cache.py            (from core/cache.py)
│   └── provenance.py       (from core/provenance.py)
│
├── validation/         # All validation (consolidate validation/ + validators/)
│   ├── __init__.py
│   ├── pipeline.py         (from validation/pipeline.py)
│   ├── data_validator.py   (from validation/data_validator.py)
│   ├── template_validator.py (from templates/template_validator.py)
│   └── schema_validator.py (from base_configs/config_framework.SchemaValidator — extract)
│
└── utils/              # Shared utilities that do not fit above
    ├── __init__.py
    ├── data_io.py          (from common/data.py + ETL_components.py + excel_utilities.py)
    ├── engineering_units.py (from common/engineering_units.py)
    ├── material_properties.py (from common/MaterialProperties.py)
    ├── path_utils.py       (from common/path_utils.py)
    ├── parallel_processing.py (from common/parallel_processing.py)
    ├── standards_lookup.py (from common/standards_lookup.py)
    ├── update_deep.py      (from common/update_deep.py)
    ├── basic_statistics.py (from common/basic_statistics.py)
    ├── visualization/      (from common/visualizations*.py + utilities/)
    └── yml_input.py        (from common/ymlInput.py)
```

### Files moved OUT of `infrastructure/` entirely

| Current path | Target path | Reason |
|---|---|---|
| `services/digitaltwinfeed/` | `src/digitalmodel/web/digitaltwinfeed/` | Web app layer; not infrastructure |
| `services/dtf/` | `src/digitalmodel/web/dtf/` | Flask blueprint |
| `services/example_blueprint/` | `src/digitalmodel/web/example_blueprint/` | Flask blueprint |
| `services/example_SPA/` | `src/digitalmodel/web/example_SPA/` | Flask SPA |
| `services/TemplateCode/` | `src/digitalmodel/web/template_code/` | App template |
| `domains/platecapacity/` | `src/digitalmodel/structural/plate_capacity/` | Domain solver |
| `domains/reservoir/` | `src/digitalmodel/reservoir/` | Domain solver |
| `common/front_end_components.py` | `src/digitalmodel/web/components.py` | UI layer |
| `common/finance_components.py` | `src/digitalmodel/finance/components.py` | Finance domain |
| `common/documentation_components.py` | `src/digitalmodel/reporting/docs.py` | Reporting |

### Files to remove (dead code)

| File | Reason |
|---|---|
| `common/visualization_unused.py` | Explicitly marked unused |
| `calculations/plate_buckling.py` | Duplicates `common/plate_buckling.py`; resolve by keeping one in `solvers/` |
| `validators/` | Replace with deprecation-removal once all callers import from `validation/` |

---

## 4. Migration Priority Order

Priority is ordered by: (a) zero import risk, (b) fewest external dependents,
(c) highest ROI in reducing `common/` sprawl.

### Phase 2A — Zero-risk consolidations (no caller changes needed)

1. **Merge `config/` + `base_configs/` → `config/`**
   - Move `base_configs/config_framework.py` → `config/framework.py`
   - Move `base_configs/config_models.py` → `config/models.py`
   - Move `base_configs/domains/` → `config/domains/`
   - Update `base_configs/__init__.py` to re-export from `config/` with deprecation warning
   - Risk: low; `base_configs` has 0 non-infrastructure callers

2. **Rename `core/` → `persistence/`**
   - Files move in-place; add backward-compat `core/__init__.py` shim
   - Risk: low; only `core/` is imported via fully-qualified paths

3. **Move `templates/template_validator.py` → `validation/template_validator.py`**
   - Risk: zero; `templates/` has no Python callers

### Phase 2B — Migrate `validation/` consolidation

4. **Remove `validators/` shim after caller audit**
   - Grep all callers of `digitalmodel.infrastructure.validators`
   - Update callers to import from `digitalmodel.infrastructure.validation`
   - Delete `validators/`

### Phase 2C — Extract `services/` web layer

5. **Move `services/*` → `src/digitalmodel/web/`**
   - Create `src/digitalmodel/web/__init__.py`
   - Move each Flask app/blueprint
   - Risk: medium; Flask routing may use relative imports

### Phase 2D — Migrate `common/` engineering-analysis files → `solvers/`

6. **Move cathodic protection** → `solvers/hydrodynamics/cathodic_protection.py`
   - External callers: `engine.py` (direct import)

7. **Move DNV hydrodynamics** → `solvers/hydrodynamics/dnv_rph103_*.py`
   - External callers: `engine.py` (2 imports)

8. **Move fatigue analysis** → `solvers/fatigue/`
   - Move `common/fatigue_analysis.py`, `common/fatigue_analysis_components.py`
   - External callers: `solvers/orcaflex/orcaflex_fatigue_analysis.py`

9. **Move VIV analysis** → `solvers/viv/`
   - Move `common/viv_analysis_components.py`, `common/viv_fatigue_analysis_components.py`,
     `common/shear7_model_components.py`
   - External callers: `solvers/orcaflex/orcaflex_fatigue_analysis.py`,
     `subsea/vertical_riser/vertical_riser_components.py`

10. **Move pipe/riser analysis** → `solvers/marine/`
    - Move `common/pipe_properties.py`, `common/typical_riser_stack_up_calculations.py`
    - External callers: `subsea/pipeline/pipeline.py`, `subsea/viv_analysis/viv_analysis.py`,
      `subsea/vertical_riser/vertical_riser_components.py`

11. **Move FEA** → `solvers/structural/fea/`
    - Move `common/FEAComponents.py`
    - External callers: `solvers/fea_model/fea_model.py`

12. **Move ship design** → `solvers/marine/ship/`
    - Move `common/ship_design.py`, `common/ship_fatigue_analysis.py`
    - External callers: `engine.py`

### Phase 2E — Migrate `domains/` misplaced domain solvers

13. **Move `domains/platecapacity/`** → `src/digitalmodel/structural/plate_capacity/`
14. **Move `domains/reservoir/`** → `src/digitalmodel/reservoir/`

### Phase 2F — Residual `common/` cleanup → `utils/`

15. Remaining data I/O and utility files in `common/` move to `utils/`
16. Remove `common/` once empty

---

## 5. Import Dependencies Requiring Updates

### Callers outside `infrastructure/` that import from `infrastructure.common`

| Calling file | Imported symbol | Target after refactor |
|---|---|---|
| `solvers/fea_model/fea_model.py` | `FEAComponents` | `infrastructure.solvers.structural.fea` |
| `solvers/orcaflex/opp.py` | `should_use_parallel` | `infrastructure.utils.parallel_processing` |
| `solvers/orcaflex/opp_time_series.py` | `ETL_components` | `infrastructure.utils.data_io` |
| `solvers/orcaflex/opp_time_series_v2.py` | `ETL_components` | `infrastructure.utils.data_io` |
| `solvers/orcaflex/opp_visualization.py` | `should_use_parallel` | `infrastructure.utils.parallel_processing` |
| `solvers/orcaflex/orcaflex_fatigue_analysis.py` | `viv_fatigue_analysis_components` | `infrastructure.solvers.viv` |
| `structural/pipe_capacity/PipeCapacity.py` | `update_deep_dictionary`, `AttributeDict` | `infrastructure.utils.update_deep`, `utils.data_io` |
| `subsea/pipeline/pipeline.py` | `PipeProperties` | `infrastructure.solvers.marine` |
| `subsea/vertical_riser/vertical_riser_components.py` | `typical_riser_stack_up_calculations`, `Shear7ModelComponents`, `Visualization` | `infrastructure.solvers.marine`, `solvers.viv`, `utils.visualization` |
| `subsea/viv_analysis/viv_analysis.py` | `PipeProperties` | `infrastructure.solvers.marine` |
| `engine.py` | `CathodicProtection`, `code_dnvrph103_*`, `ShipDesign`, `Transformation`, `PlateBuckling` | `infrastructure.solvers.hydrodynamics`, `solvers.marine`, `solvers.pipeline_solvers`, `solvers.structural` |
| `digitalmodel/__init__.py` | `Transformation` | `infrastructure.solvers.pipeline_solvers` |

### Internal `infrastructure/` cross-imports to watch

- `common/database.py` uses an unqualified `from common.data import AttributeDict`
  (broken legacy import; must be fixed to absolute path regardless of refactor)
- `common/fatigue_analysis.py` imports from `signal_processing/time_series/`
  (cross-domain; this dependency is acceptable post-refactor)

---

## 6. Acceptance Criteria (Phase 2+ gate)

- `infrastructure/` contains exactly: `config/`, `solvers/`, `persistence/`,
  `validation/`, `utils/` — five directories, no others
- `grep -r "infrastructure.common" src/` returns zero results
- `grep -r "infrastructure.base_configs" src/` returns zero results
- `grep -r "infrastructure.base_solvers" src/` returns zero results
- `grep -r "infrastructure.core" src/` returns zero results
- `grep -r "infrastructure.validators" src/` returns zero results
- All existing tests pass
- No circular imports (`python -c "import digitalmodel"` with no warnings)
