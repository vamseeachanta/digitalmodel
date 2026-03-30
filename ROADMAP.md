# digitalmodel Roadmap

> Tethering timeless engineering to a single source of truth.

## How This Roadmap Works

Modules are tiered by client project demand. Tier 1 = build next, Tier 2 = build when needed, Tier 3 = backlog. Tiers are re-evaluated when new client projects arrive -- this is event-driven, not calendar-driven. aceengineer.com calculator potential is a secondary signal: modules that make good web calculators get a small priority boost but never override client demand.

Module IDs reference `specs/module-registry.yaml`. Maturity levels (production, stable, beta, development, stub) track readiness, not priority.

## Tier 1: Active Development

### OrcaFlex Subsea Structural Analysis

**Current state:** The `solvers/orcaflex/` module contains 80+ files spanning model generation (`model_generator/`, `modular_generator/`), run automation (`universal/`, parallel batch execution), post-processing (`post_process/`, `post_results/`), mooring analysis (`mooring_analysis/`, `mooring_tension_iteration/`), format conversion (`format_converter/`), and reporting (`reporting/`). Maturity is rated production for the core runner and converter. Additional OrcaFlex integration exists in `orcaflex/` (3 files + reporting), `workflows/agents/orcaflex/`, and `hydrodynamics/diffraction/` (OrcaWave-to-OrcaFlex export).

**Target state:** Production-grade subsea structural analysis workflow, defined as:
1. At least 3 reference scenario templates (jacket, pipeline, riser) with YAML configs
2. Integration tests that run without an OrcaFlex license (mock OrcFxAPI calls)
3. Validated output against at least 1 reference project result

**Work remaining:**
- [ ] Define canonical YAML scenario templates for jacket, pipeline, and riser configurations
- [ ] Add license-free integration tests using mocked OrcFxAPI responses
- [ ] Validate model generation output against a reference project dataset
- [ ] Consolidate OrcaWave integration (`solvers/orcawave/`, `hydrodynamics/diffraction/`) into a unified Orca suite entry point
- [ ] Document the full pipeline: YAML config to model generation to run to post-processing

**Calculator potential:** Medium -- complex multi-step workflows do not simplify to single-page web calculators. Individual sub-calculations (e.g., mooring line catenary, VIV screening) are better candidates.

### Cathodic Protection Maturity

**Current state:** 4 implementation files covering 3 international standards plus fuel system CP:
- `dnv_rp_b401.py` -- offshore structure CP design (anode sizing, coating breakdown, zone-based calculations)
- `api_rp_1632.py` -- underground pipeline CP (Dwight resistance, current demand, anode life)
- `iso_15589_2.py` -- offshore pipeline CP (bracelet anodes, attenuation length)
- `fuel_system_cp.py` -- impressed current system design (ground bed, rectifier)

72 passing tests across the 3 standards. Module exports ~30 public functions.

**Target state:** Higher maturity, defined as:
1. All standard clauses within implemented standards have corresponding code
2. Worked-example tests from standard appendices validate each clause implementation
3. ABS Guide for Building and Classing Ships/Offshore Installations full implementation (currently configured only in README, no code)

**Work remaining:**
- [ ] Audit DNV-RP-B401, API RP 1632, and ISO 15589-2 for missing clauses within implemented scope
- [ ] Add worked-example tests from each standard's appendices
- [ ] Implement ABS GN for Building and Classing Ships/Offshore Installations CP requirements
- [ ] Add module entry to `specs/module-registry.yaml` with full capabilities and gaps
- [ ] Create manifest.yaml with clause-level traceability

**Calculator potential:** High -- CP sizing (anode quantity, coating breakdown factor, current demand) is a natural web calculator for aceengineer.com. Similar in complexity to the existing wall-thickness calculator.

## Tier 2: Build When Needed

### Structural Fatigue -- Efthymiou SCF Gap

**Current state:** Production maturity. 221 S-N curves from 17 standards, spectral methods (Dirlik, Tovo-Benasciutti), scatter diagram fatigue damage, 808+ passing tests.
**Key gap:** Efthymiou SCF parametric equations for tubular joints not implemented. Stress transfer function composition missing.
**Calculator potential:** High -- fatigue-life and fatigue-sn-curve calculators already exist on aceengineer.com.

### Subsea Pipeline -- Installation and Free-Span

**Current state:** Stable maturity across `subsea/pipeline/` and `subsea/on_bottom_stability/` (DNV-RP-F109, 5 functions, 20 tests). Pipe capacity (burst, collapse, propagation) in `structural/pipe_capacity/`.
**Key gap:** Installation window weather criteria not implemented. Free-span VIV full response model missing (screening exists in `subsea/viv_analysis/`).
**Calculator potential:** Medium -- on-bottom stability calculator already exists on aceengineer.com.

### Asset Integrity / API 579

**Current state:** Stable maturity. API 579-1/ASME FFS-1 Level 1/2 assessments, remaining strength factor (RSF), BS 7910 fracture mechanics (FAD diagrams, critical flaw size).
**Key gap:** Level 3 assessment (elastic-plastic fracture) not automated. Creep damage (Part 10), fire damage, and dent assessment not implemented.
**Calculator potential:** Medium -- FFS screening could work as a calculator but requires significant input complexity.

### Geotechnical -- Soil Models, Anchors, Scour

**Current state:** 13 functions covering pile axial capacity (API RP 2GEO) and basic geotechnical calculations. 23 tests.
**Key gap:** Soil constitutive models (clay, sand, calcareous), anchor design (DNVGL-RP-E301/E303, API RP 2SK), scour assessment (DNV-RP-F107), and shallow foundation design (DNV-RP-C212) all missing.
**Calculator potential:** Medium -- pile capacity and scour screening are natural calculator candidates.

### Mooring Analysis -- Fatigue and Optimization

**Current state:** Stable maturity in `subsea/mooring_analysis/`. Catenary line analysis, spread mooring, CALM/SALM buoy design, OrcaFlex model generation, safety factor checks per DNV-OS-E301 and API RP 2SK.
**Key gap:** Mooring line fatigue not implemented. Clump weight and buoy optimization not automated.
**Calculator potential:** High -- mooring line preliminary sizing is a good candidate for aceengineer.com.

## Tier 3: Backlog

### Hydrodynamics

**Current state:** 825 functions across 8 sub-modules (production maturity for top-level, AQWA, and diffraction). Wave spectra, RAO analysis, passing ship, hull library, Capytaine BEM solver, planing hull seakeeping. Zero standards mapped in capability register despite extensive code.
**Backlog reason:** Functional and mature but needs capability map population before gap analysis is meaningful.

### Naval Architecture

**Current state:** 20 functions. Basic hull form and stability calculations.
**Backlog reason:** No standards mapped, no active client demand.

### Production Engineering

**Current state:** Production maturity for well test quality scoring, nodal analysis (IPR/VLP), and GIGO detection. 39 functions.
**Backlog reason:** Complete for current use cases. Multiphase flow beyond Hagedorn-Brown is the next gap.

### Well Engineering

**Current state:** Stable maturity for drilling hydraulics (API RP 13D) and tubular design (API 5C3). 18 functions.
**Backlog reason:** Basic modules functional. Casing design integration with API 5CT grades available via assetutilities.

### Drilling Riser

**Current state:** 13 functions. Basic riser analysis.
**Backlog reason:** No standards mapped, no active client demand.

### Candidates for Removal

These stub modules contain no engineering value and clutter the namespace:
- `specialized/digitalmarketing` -- non-engineering, no source files
- `specialized/finance` -- utility/exploratory code, not engineering-grade
- `specialized/project_management` -- minimal implementation, no package init

## Tech Debt

### Category A -- Blocks Current Work

1. **0/150 structural tests runnable.** Three import path issues block all `marine_engineering` tests (see `tests/structural/analysis/TEST_STATUS_DASHBOARD.md`): (a) missing `extract_hydro_coefficients` module, (b) incorrect `RAOPlotter` import path, (c) PYTHONPATH not configured for test discovery. Estimated fix: 1-2 hours.

2. **pyproject.toml version mismatch.** `version = "0.1.1"` in pyproject.toml, but README states "Version: 3.0.0" and CHANGELOG last entry is v2.0.0. Must be resolved before PyPI publishing.

### Category B -- Degrades Developer Experience

3. **Bloated pyproject.toml dependencies.** 170+ dependencies including packages irrelevant to engineering calculations: celery, redis, newrelic, gunicorn, boto3, fastapi, alembic, asyncpg, aiofiles, aiosqlite. These inflate install time and attack surface.

4. **Duplicate module paths.** Catenary solver exists in 4 locations: `subsea/catenary` (stub), `subsea/catenary_riser`, `marine_ops/marine_analysis`, `marine_ops/marine_engineering`. Canonical location unclear.

5. **Stale coverage metrics.** `coverage.json` dates from January 2026. Coverage numbers in documentation may not reflect current state.

### Category C -- Aspirational (not blocking, not in roadmap)

6. **455 standards gaps across 30 disciplines.** Many represent entire disciplines not yet started. Only gaps in Tier 1 and Tier 2 modules are actionable.

7. **No VISION.md at repo root.** CALCULATIONS-VISION.md exists in `docs/vision/` but the ecosystem-level VISION.md it references does not exist.

8. **Stub modules with no engineering value.** `specialized/digitalmarketing`, `specialized/finance`, `specialized/project_management` -- see Candidates for Removal above.

## Document Intelligence Pipeline

The workspace-hub document intelligence platform is the upstream data source for new calculation module development. The established workflow is: extraction (from engineering standards PDFs) to promotion (tables to CSV, equations to Python, procedures to YAML) to implementation (digitalmodel calculation modules).

Three standards currently have promoted data ready for direct code use: DNV-RP-B401 (cathodic protection), DNV-RP-C203 (fatigue), and DNV-RP-F109 (on-bottom stability). The standards-transfer-ledger maps 425 standards to their target repositories and modules. Table extraction yields 69-93% accuracy; equation extraction requires manual reference. As new standards are extracted and promoted, they feed directly into Tier 1 and Tier 2 module development.

## GTM Connection

aceengineer.com calculators consume digitalmodel as their calculation engine. Current alignment:

| aceengineer.com Calculator | digitalmodel Module | Status |
|---|---|---|
| fatigue-life-calculator | structural/fatigue | Live -- S-N curves + damage accumulation |
| fatigue-sn-curve | structural/fatigue | Live -- 221 S-N curves from 17 standards |
| npv-field-development | worldenergydata (cross-repo) | Live -- NPV/MIRR with carbon cost |
| on-bottom-stability | subsea/on_bottom_stability | Live -- DNV-RP-F109 |
| wall-thickness | structural/analysis | Live -- ASME B31.4 wall thickness |

**Gap opportunities for next calculators:**
- Cathodic Protection sizing calculator (cathodic_protection module, Tier 1) -- anode quantity, coating breakdown, current demand
- Mooring line preliminary design (subsea/mooring_analysis, Tier 2) -- catenary geometry, tension, material selection
- VIV screening (subsea/viv_analysis, Tier 2) -- reduced velocity, lock-in assessment, fatigue damage estimate
