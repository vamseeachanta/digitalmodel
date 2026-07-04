# digitalmodel Operator Map

Updated: 2026-04-27
Purpose: canonical repo-wide routing map for digitalmodel source, tests, docs,
and common issue surfaces.
Traceability: workspace-hub issue #2462 and tier-1 indexing contract #2460.

Historical narrow slice: workspace-hub
`docs/maps/digitalmodel-orcawave-orcaflex-operator-map.md` remains useful for
older OrcaWave/OrcaFlex reconciliation context. This repo-wide map is the
canonical active routing surface for digitalmodel and links that slice instead
of duplicating its detailed issue history.

## Source / Tests / Docs Routing

| Module | Source | Tests | Docs | Issue routing | Key dependencies |
|---|---|---|---|---|---|
| `ansys` | `src/digitalmodel/ansys/` | `tests/ansys/` | `docs/domains/ansys/` | ANSYS automation, APDL/WBJN parsing, pressure-vessel and weld checks | ANSYS file formats, engineering standards |
| `asset_integrity` | `src/digitalmodel/asset_integrity/` | `tests/asset_integrity/` | `docs/domains/README.md` | Fitness-for-service, API 579, BS 7910, RSF, fracture mechanics | NumPy, pandas, matplotlib |
| `benchmarks` | `src/digitalmodel/benchmarks/` | `tests/benchmarks/` | `docs/domains/README.md` | Benchmark inventory, model comparison, reproducibility metrics | pytest-benchmark |
| `cathodic_protection` | `src/digitalmodel/cathodic_protection/` | `tests/cathodic_protection/` | `docs/domains/cathodic_protection/` | CP design, anode sizing, coating/current demand, worked examples | DNV/API/ISO CP standards |
| `citations` | `src/digitalmodel/citations/` | `tests/citations/` | `docs/domains/README.md` | Standards citation schema and registry support | PyYAML |
| `data_models` | `src/digitalmodel/data_models/` | `tests/` | `docs/domains/README.md` | Shared engineering data structures and curves | pydantic/dataclasses |
| `data_systems` | `src/digitalmodel/data_systems/` | `tests/data_systems/` | `docs/domains/data_systems/` | Data catalogs, config loading, procurement/scraping support | pandas, SQL/file formats |
| `drilling_riser` | `src/digitalmodel/drilling_riser/` | `tests/drilling_riser/` | `docs/domains/README.md` | Drilling riser stackup, damping, operability, tool passage | marine engineering standards |
| `fatigue` | `src/digitalmodel/fatigue/` | `tests/fatigue/` | `docs/domains/fatigue/` | S-N curves, rainflow, spectral fatigue, weld classifications | NumPy, fatigue standards |
| `field_development` | `src/digitalmodel/field_development/` | `tests/field_development/` | `docs/domains/README.md` | Concept selection, CAPEX/OPEX, timelines, architecture patterns | pandas, economic models |
| `geotechnical` | `src/digitalmodel/geotechnical/` | `tests/geotechnical/` | `docs/domains/README.md` | Pile capacity, soil/scour/anchor calculations | geotechnical standards |
| `gis` | `src/digitalmodel/gis/` | `tests/gis/` | `docs/domains/README.md` | CRS transforms, spatial queries, GeoJSON/QGIS/Blender export | pyproj, shapely/geospatial formats |
| `hydrodynamics` | `src/digitalmodel/hydrodynamics/` | `tests/hydrodynamics/` | `docs/domains/hydrodynamics/` | Wave spectra, RAOs, diffraction, hull library, solver bridges | NumPy, SciPy, OrcaWave/AQWA context |
| `infrastructure` | `src/digitalmodel/infrastructure/` | `tests/infrastructure/` | `docs/domains/infrastructure/` | Base configs, services, validators, transformations | YAML configs, shared services |
| `marine_ops` | `src/digitalmodel/marine_ops/` | `tests/marine_ops/` | `docs/domains/marine_ops/` | Marine operations, RAO readers, installation and operational analysis | metocean and vessel data |
| `floating_wind` | `src/digitalmodel/floating_wind/` | `tests/floating_wind/` | `docs/domains/README.md` | Floating-wind global sizing & concept screening (semi/spar/TLP/barge); `floating_wind_sizing` workflow | hydrostatics, parametric sweeps, OrcaWave/OrcaFlex (solver tier) |
| `hydrostatics` | `src/digitalmodel/hydrostatics/` | `tests/hydrostatics/` | `docs/domains/README.md` | Fluid-column hydrostatics; seabed/hydrostatic pressure and offshore pressure-unit conversions | NumPy |
| `mooring_resilience` | `src/digitalmodel/mooring_resilience/` | `tests/` | `docs/domains/README.md` | Mooring-system resilience screening (intact/damaged tension, foundation, fatigue) over pre-computed atlases | mooring atlases, API RP 2SK / DNV-OS-E301 |
| `motion_forecast` | `src/digitalmodel/motion_forecast/` | `tests/motion_forecast/` | `docs/domains/README.md` | Short-horizon vessel/structure motion forecast, operability go/no-go (#1358) | NumPy, RAO/hydrodynamics |
| `naval_architecture` | `src/digitalmodel/naval_architecture/` | `tests/naval_architecture/` | `docs/domains/README.md` | Stability, hull form, compliance, gyradius calculations; loading computer (#1035) | naval architecture standards |
| `reporting` | `src/digitalmodel/reporting/` | `tests/reporting/` | `docs/domains/README.md` | Shared report block library / backbone (report-as-backbone, #1018) | pydantic, HTML rendering |
| `nde` | `src/digitalmodel/nde/` | `tests/nde/` | `docs/domains/README.md` | Non-destructive examination workflows and checks | inspection standards |
| `orcaflex` | `src/digitalmodel/orcaflex/` | `tests/orcaflex/` | `docs/domains/orcaflex/` | Public OrcaFlex APIs, reporting, model and post-processing utilities | OrcFxAPI where licensed, YAML |
| `orcawave` | `src/digitalmodel/orcawave/` | `tests/orcawave/` | `docs/domains/orcawave/` | OrcaWave package utilities, reporting, hydrodynamic inputs/outputs | OrcaWave, hydrodynamics |
| `power` | `src/digitalmodel/power/` | `tests/power/` | `docs/domains/README.md` | Generator controls, microgrid EMS, protection relays | IEEE/NFPA power standards |
| `production_chemistry` | `src/digitalmodel/production_chemistry/` | `tests/production_chemistry/` | `docs/domains/README.md` | Mineral-scale saturation indices (Oddo-Tomson), brine mixing/waterflood compatibility | NumPy, pandas |
| `production_engineering` | `src/digitalmodel/production_engineering/` | `tests/production_engineering/` | `docs/domains/README.md` | Well testing, nodal analysis, production quality scoring | petroleum engineering models |
| `reservoir` | `src/digitalmodel/reservoir/` | `tests/reservoir/` | `docs/domains/reservoir/` | Reservoir engineering and production-support calculations | reservoir engineering models |
| `residual` | `src/digitalmodel/residual/` | `tests/residual/` | `docs/domains/README.md` | Transparent, bounded residual-correction spine for the hybrid digital twin (#1374) | NumPy |
| `signal_processing` | `src/digitalmodel/signal_processing/` | `tests/signal_processing/` | `docs/domains/signal_processing/` | Signal analysis, filters, time-series utilities | NumPy, SciPy |
| `solvers` | `src/digitalmodel/solvers/` | `tests/solvers/` | `docs/domains/solvers/` | Solver adapters, OrcaFlex/OrcaWave/AQWA workflows, batch execution | solver-specific APIs |
| `specialized` | `src/digitalmodel/specialized/` | `tests/specialized/` | `docs/domains/specialized/` | Specialized/non-core modules pending routing cleanup | mixed utilities |
| `specs` | `src/digitalmodel/specs/` | `tests/specs/` | `docs/domains/README.md` | Runtime package specs and structured metadata helpers | PyYAML |
| `structural` | `src/digitalmodel/structural/` | `tests/structural/` | `docs/domains/structural/` | Structural analysis, wall thickness, fatigue bridges, pipe capacity | structural standards |
| `subsea` | `src/digitalmodel/subsea/` | `tests/subsea/` | `docs/domains/subsea/` | Pipeline, riser, mooring, on-bottom stability, VIV and free-span work | DNV/API subsea standards |
| `visualization` | `src/digitalmodel/visualization/` | `tests/visualization/` | `docs/domains/visualization/` | Dashboards, design tools, rendering and visual outputs | frontend/CAD/rendering tools |
| `web` | `src/digitalmodel/web/` | `tests/web/` | `docs/domains/README.md` | Web-facing calculator and app support | web framework assets |
| `well` | `src/digitalmodel/well/` | `tests/well/` | `docs/domains/README.md` | Well engineering, drilling hydraulics, tubular design | API well standards |
| `workflows` | `src/digitalmodel/workflows/` | `tests/workflows/` | `docs/domains/workflows/` | Multi-step issue workflows and agent/solver orchestration | repo workflow conventions |
| `base_configs` | `src/digitalmodel/base_configs/` | `tests/` | `docs/domains/README.md` | Shared base configuration scaffolding for module configs | PyYAML |
| `code_checks` | `src/digitalmodel/code_checks/` | `tests/code_checks/` | `docs/domains/README.md` | Engineering code-check helpers and standards verification | engineering standards |
| `common` | `src/digitalmodel/common/` | `tests/common/` | `docs/domains/README.md` | Shared assumption-ledger and spec-authoring helpers | PyYAML |
| `corrosion` | `src/digitalmodel/corrosion/` | `tests/corrosion/` | `docs/domains/README.md` | Galvanic (dissimilar-metal) compatibility screening, MIL-STD-889 anodic index | NumPy, pandas |
| `materials` | `src/digitalmodel/materials/` | `tests/materials/` | `docs/domains/README.md` | Material grades and line-pipe property data | NumPy, pandas |
| `workflow_api` | `src/digitalmodel/workflow_api/` | `tests/workflow_api/` | `docs/domains/README.md` | Workflow runner, provenance and golden-output API | repo workflow conventions |
| `compare_tool` | `src/digitalmodel/compare_tool/` | `tests/compare_tool/` | `docs/domains/README.md` | Result/model comparison utilities across runs and tools | pandas, NumPy |
| `installation` | `src/digitalmodel/installation/` | `tests/` | `docs/domains/installation/` | Offshore installation analysis, lift/lay operability, weather windows | marine engineering standards |
| `lifting_lug` | `src/digitalmodel/lifting_lug/` | `tests/` | `docs/domains/README.md` | Lifting-lug/padeye sizing and closed-form structural checks | structural standards |
| `mooring_fatigue` | `src/digitalmodel/mooring_fatigue/` | `tests/` | `docs/domains/README.md` | Mooring-line fatigue analysis and parametric atlases | fatigue standards, NumPy |
| `parametric` | `src/digitalmodel/parametric/` | `tests/parametric/` | `docs/domains/README.md` | Parametric atlas build and query (sweep-built surrogate serving) | NumPy, pandas |
| `parametrics` | `src/digitalmodel/parametrics/` | `tests/parametrics/` | `docs/domains/README.md` | Parametric sweep bridging into atlases and query handlers | NumPy, pandas |
| `pipelay` | `src/digitalmodel/pipelay/` | `tests/pipelay/` | `docs/domains/README.md` | Pipelay analysis, lay tension and configuration checks | marine/subsea standards |
| `rao_spectral_fatigue` | `src/digitalmodel/rao_spectral_fatigue/` | `tests/` | `docs/domains/README.md` | RAO-driven spectral fatigue workflow (closed-form) | fatigue/hydrodynamics standards |
| `riser_database` | `src/digitalmodel/riser_database/` | `tests/riser_database/` | `docs/domains/README.md` | Public riser reference tables (config catalog, materials, crosswalk) | PyYAML, pandas |
| `riser_fatigue` | `src/digitalmodel/riser_fatigue/` | `tests/riser_fatigue/` | `docs/domains/README.md` | Riser fatigue analysis and automation | fatigue standards, NumPy |
| `spectral_fatigue` | `src/digitalmodel/spectral_fatigue/` | `tests/` | `docs/domains/README.md` | Spectral (frequency-domain) fatigue calculation | fatigue standards, SciPy |
| `synthetic_rope_mooring_fatigue` | `src/digitalmodel/synthetic_rope_mooring_fatigue/` | `tests/` | `docs/domains/README.md` | Synthetic-rope mooring fatigue analysis | fatigue standards, NumPy |
| `tug` | `src/digitalmodel/tug/` | `tests/tug/` | `docs/domains/README.md` | Tug/towage analysis: bollard pull, fendering, girting stability, towline mechanics, emissions | NumPy, engineering standards |
| `usecase_registry` | `src/digitalmodel/usecase_registry/` | `tests/test_usecase_registry.py` | `docs/domains/README.md` | Registry of runnable use cases and routing metadata | PyYAML |
| `vessel_seakeeping` | `src/digitalmodel/vessel_seakeeping/` | `tests/` | `docs/domains/README.md` | Vessel seakeeping response and operability analysis | hydrodynamics standards |
| `weather_window` | `src/digitalmodel/weather_window/` | `tests/` | `docs/domains/README.md` | Weather-window/operability assessment for marine operations | metocean data, statistics |
| `well_access` | `src/digitalmodel/well_access/` | `tests/well_access/` | `docs/domains/README.md` | Well-access/intervention analysis and operability | well engineering standards |

## Common Routing Rules

- Start from this map when an issue names a domain, package, solver, or common
  engineering workflow.
- Use `docs/registry/module-routing.yaml` for machine-readable resolution.
- If a domain docs folder is missing, route through `docs/domains/README.md`
  and the module row here before creating new docs.
- Keep large generated outputs, raw crawls, solver result dumps, caches, and
  binary artifacts out of the repo unless a plan explicitly approves a small
  curated artifact.
