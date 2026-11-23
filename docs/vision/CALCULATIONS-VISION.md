# Calculations Vision — ACE Engineering Ecosystem

> Tethering timeless engineering to a single source of truth.
>
> *Every calculation traces to its standard. Every standard traces to its implementation.
> From clause to code to validated report.*

## Vision Direction

digitalmodel is a **Python calculation library**: importable functions with CLI convenience wrappers, each traceable to an international engineering standard. It is library-first, not a platform or SaaS product.

### What digitalmodel IS
- A Python package of engineering calculations (`pip install digitalmodel`)
- Each function maps to a specific standard clause (dual traceability: docstrings + YAML manifests)
- 27 CLI entry points for common workflows (registered in pyproject.toml)
- The calculation engine behind aceengineer.com interactive calculators

### What digitalmodel is NOT
- Not a web platform or SaaS product (no web server, no user management, no database)
- Not a web API service (FastAPI is in dependencies but unused for core calculations)
- Not a monolithic application — it is a library consumed by other tools

### How it feeds downstream
1. **Client project analysis** — direct Python usage producing calculation reports
2. **aceengineer.com calculators** — JavaScript reimplementations of digitalmodel modules for web interactivity
3. **Document intelligence pipeline** — workspace-hub extraction/promotion workflow feeds reference data and test fixtures into modules

### Development priorities
See [ROADMAP.md](../../ROADMAP.md) for the tiered module prioritization driven by client project demand.

## Current State

### digitalmodel
- Disciplines: 30 (structural, subsea, hydrodynamics, cathodic protection, asset integrity, geotechnical, marine ops, naval architecture, production engineering, well engineering, drilling riser, signal processing, solvers, visualization, gis, power, field development, and more)
- Source files with public API: 1,077
- Public functions: 7,355
- Public classes: 2,027
- Standards in capability map: 17,799 mapped across 27 modules
- Standards marked done: 42 (after reconciliation)
- Standards marked gap: 455

### assethold
- Public functions: ~130
- Key calculations: VaR, CVaR, Sharpe, Sortino, Calmar, Black-Scholes Greeks, portfolio optimization, beta, covered call analysis, technical indicators (SMA, EMA, RSI, MACD, Bollinger, OBV), trend detection, insider tracking, multifamily analysis
- Tests passing: 996

### assetutilities
- Calculation modules: 12
- Public calculation functions: 80
- Shared constants: 11 steel grades (API 5L + ASTM) + 10 casing grades (API 5CT)
- Unit conversion: 27 direct conversion functions + Pint registry (energy + metocean domains)
- Seawater properties: 3 functions (UNESCO 1980 / ITTC 2011)
- Tests passing: 855

### worldenergydata
- Calculation modules: 42
- Data loader modules: ~548
- Key calculations: Arps decline (3 models), SEPD, NPV/MIRR with carbon cost, pore pressure, learning curve economics, decommissioning cost estimation, safety risk scoring
- Tests passing: 230

## Coverage by Discipline

| Discipline | Module | Functions | Standards Mapped | Done | Gap | Coverage % |
|-----------|--------|-----------|:---:|:---:|:---:|:---:|
| Structural | structural | 739 | 126 | 12 | 126 | 9.5 |
| Subsea | subsea | 325 | 152 | 6 | 152 | 3.9 |
| Hydrodynamics | hydrodynamics | 825 | 0 | 0 | 0 | N/A (map gap) |
| Asset Integrity | asset_integrity | 393 | 100 | 4 | 100 | 4.0 |
| Marine Ops | marine_ops | 510 | 64 | 3 | 64 | 4.7 |
| Cathodic Protection | cathodic_protection | 30 | 0 | 3 | 0 | N/A (not in map) |
| Signal Processing | signal_processing | 140 | 0 | 2 | 0 | N/A (not in map) |
| Production Engineering | production_engineering | 39 | 0 | 0 | 0 | N/A (not in map) |
| Geotechnical | geotechnical | 13 | 10 | 2 | 8 | 20.0 |
| Well Engineering | well | 18 | 0 | 0 | 0 | N/A (not in map) |
| Solvers | solvers | 1,351 | 0 | 0 | 0 | N/A (infra) |
| Drilling Riser | drilling_riser | 13 | 0 | 0 | 0 | N/A (not in map) |
| Naval Architecture | naval_architecture | 20 | 0 | 0 | 0 | N/A (not in map) |
| Visualization | visualization | 342 | 0 | 0 | 0 | N/A (infra) |
| GIS | gis | 124 | 0 | 0 | 0 | N/A (infra) |
| Power | power | 42 | 0 | 0 | 0 | N/A (not in map) |
| Field Development | field_development | 18 | 3 | 0 | 3 | 0.0 |
| **assethold** | all | ~130 | — | — | 8 | — |
| **assetutilities** | calculations | 80 | — | — | 12 | — |
| **worldenergydata** | calculations | 42 modules | — | — | 7 | — |

**Note:** "Done" counts reflect the reconciliation completed during WRK-1179 Sprint. Prior to this sprint, the capability map showed 0 standards marked done across all 17,799 entries.

## Tier Progression

Per `specs/architecture/agent-vision.md`:

### Tier 1 — Engineering Calculator (Current for most modules)
- Function exists with typed inputs/outputs
- Standard referenced in code/docstring
- At least one test with worked example from the standard
- Results reproducible: same inputs always produce same outputs

### Tier 2 — Engineering Assistant (Target: end 2026)
- Agent API layer wraps Tier 1 modules (`ace.calc(module_id, inputs)`)
- Natural language routing to correct module via module registry
- Calculation report template (YAML/JSON to HTML/PDF)
- Gap detection emits WRK item when module cannot satisfy request

### Tier 3 — Autonomous Engineering Agent (Target: end 2027)
- Project brief produces calculation package without step-by-step prompting
- Cross-repo orchestration (digitalmodel + worldenergydata + assethold in one workflow)
- Iterative design: varies parameters until all code checks pass
- Gap register maintained automatically; WRK items auto-generated

### Current Tier by Discipline

| Discipline | Current Tier | What is Needed for Next Tier |
|-----------|:---:|------|
| Structural / Fatigue | 1 | Agent API layer; Efthymiou SCF parametric equations; spectral fatigue validation complete (35 new tests this sprint) |
| Subsea / Pipeline | 1 | On-bottom stability done (this sprint); need installation window criteria, free-span VIV full model |
| Hydrodynamics | 1 | Capability map population (825 functions, 0 mapped); agent-queryable wave spectra interface |
| Asset Integrity | 1 | API 579 Level 3 (creep, fire, dents); BS 7910 fracture mechanics completion |
| Cathodic Protection | 1 | Module exists (30 funcs, DNV RP B401); needs module registry entry and report template |
| Geotechnical | 1 (partial) | Pile capacity done (this sprint); needs soil models, anchors, scour, foundations |
| Marine Ops | 1 | Artificial lift standards coverage; agent interface for installation analysis |
| Production Engineering | 1 | IPR/VLP models exist; needs agent API and structured output |
| Signal Processing | 1 | Rainflow + spectral analysis mature; needs capability map entry and report template |
| Well Engineering | 0-1 | Basic hydraulics exist; needs casing design integration with API 5CT grades (added this sprint) |
| Economics (worldenergydata) | 1 | NPV/MIRR/carbon sensitivity complete; needs cross-source synthesis layer |
| Decline Curves (worldenergydata) | 1 | Arps + SEPD (this sprint) complete; needs type curve matching and P10/P50/P90 |
| Portfolio Risk (assethold) | 1 | VaR/CVaR/Sharpe/beta complete; needs Fama-French factor model and correlation matrix |

## Priority Framework

Which calculations unlock which workflow patterns:

### Pattern 1: Subsea Fatigue Assessment
- S-N curves (221 curves, 17 standards across DNV RP C203, API, BS 7608)
- Palmgren-Miner damage accumulation
- Spectral fatigue (narrow-band, Dirlik, Wirsching-Light, Zhao-Baker, Tovo-Benasciutti) — validated this sprint with 35 new tests, 808 total fatigue tests passing
- Efthymiou SCF parametric equations — **gap**
- Stress transfer function composition — **gap**

### Pattern 2: Deepwater Pipeline Feasibility
- DNV-ST-F101 wall thickness (assetutilities: 5 functions in `pipeline_dnv.py`)
- On-bottom stability (DNV-RP-F109) — implemented this sprint (8 functions, 14 tests)
- Pipe capacity: burst, collapse, propagation buckling, combined loading
- Catenary riser: simple catenary, lazy-wave, buoyancy module design
- Catenary riser dynamic analysis enhancement — **gap**
- Installation window weather criteria — **gap**

### Pattern 3: Field Development Screening
- Arps decline curves: exponential, hyperbolic, harmonic (worldenergydata)
- SEPD decline for unconventionals — implemented this sprint (2 functions, 10 tests)
- NPV/MIRR with carbon cost sensitivity (worldenergydata `economics/dcf.py` + `economics/carbon.py`)
- Cross-basin production comparison (worldenergydata `production/unified/cross_basin.py`)
- Type curve matching (Blasingame/Fetkovich) — **gap**
- P10/P50/P90 resource estimation (Monte Carlo) — **gap**

### Pattern 4: Portfolio Risk Analysis
- VaR, CVaR (historical + parametric) — assethold `risk_metrics.py`
- Sharpe, Sortino, Calmar ratios — assethold `risk_metrics.py`
- Black-Scholes covered call analysis — assethold `options/covered_call.py`
- Technical indicators: SMA, EMA, RSI, MACD, Bollinger, OBV — assethold `stocks/indicators.py`
- Portfolio beta vs benchmarks — implemented this sprint (1 function, 6 tests)
- Fama-French factor model — **gap**
- Dividend yield forecasting — **gap**

## Gap Register

### Total Remaining Gaps

| Category | Count |
|----------|------:|
| Capability map standards gaps (digitalmodel) | 413 (455 original minus 42 reconciled) |
| Document index standards gaps | 235 (162 uncategorized "other") |
| Calculation gaps identified across 4 repos | 29 |
| Public data sources not yet ingested | 17 of 38 catalogued |

### By Discipline

| Discipline | Gap Count | Top Priorities |
|-----------|:---------:|----------------|
| Structural / Fatigue | 48 | BS 7608 editions, DNV RP F204, NORSOK N-001, ABS FPSO fatigue guide |
| Structural / Analysis | 28 | API RP 2A-LRFD, AISC, NORSOK N-004 |
| Structural / Pipe Capacity | 50 | API 5L editions, API 17E, DNV OS F101 editions |
| Subsea / Pipeline | 48 | API RP 1111 editions, DNV ST F101 editions |
| Subsea / Catenary Riser | 49 | API RP 2RD editions, DNV OS F201 |
| Subsea / Mooring | 49 | API RP 2SK editions, DNV OS E301, BV NR 493 |
| Subsea / VIV | 6 | DNV RP F105 full response model |
| Asset Integrity / API 579 | 50 | API 579 Part 9-13 (creep, fire, dents) |
| Asset Integrity / Fracture | 50 | BS 7910:2013, R6, SINTAP/FITNET |
| Marine Ops / Artificial Lift | 50 | Standards coverage for ESP, gas lift, plunger lift |
| Marine Ops / Marine Engineering | 14 | Installation and lifting standards |
| Geotechnical / Remaining | 8 | Soil models, anchors (DNVGL RP E301/E303), scour (DNV RP F107), foundations (DNV RP C212) |
| Hydrodynamics (map gap) | 0 mapped | 825 functions exist; 0 standards mapped — capability map needs population |

### By Priority

**HIGH** (no implementation exists or entire discipline missing):
- Geotechnical soil models (API RP 2GEO, DNV RP C212) — foundation design blocked
- Geotechnical anchors (DNVGL RP E301, DNV RP E303, API RP 2SK) — anchor design blocked
- Geotechnical scour (DNV RP F107) — scour assessment blocked
- Type curve matching / Blasingame / Fetkovich (worldenergydata)
- P10/P50/P90 probabilistic resource estimation (worldenergydata)

**MEDIUM** (partial implementation, key standards missing):
- Spectral fatigue: Efthymiou SCF parametric equations
- Fracture mechanics BS 7910 (basic framework only)
- API 579 Level 3 (RSF + Level 1/2 exist; creep/fire/dents missing)
- Mooring API RP 2SK full checks (catenary solver exists)
- Fama-French 3/5-factor model (assethold)
- Portfolio correlation matrix (assethold)
- Dividend yield forecasting (assethold)

**LOW** (extensive code exists, incremental standard coverage):
- Pipeline DNV ST F101 latest edition alignment
- Structural API RP 2A-LRFD (WSD implemented; LRFD missing)
- VIV DNV RP F105 full response model (screening exists)
- Hydrodynamics capability map population
- Duplicate decline/NPV consolidation in worldenergydata
- Confidence intervals on decline curves (fields exist but unpopulated)

## Calculation Report Standard

All calculations follow the calculation-report skill pattern:
- YAML schema: `config/reporting/calculation-report-schema.yaml`
- Generator: `scripts/reporting/generate-calc-report.py`
- Examples: `examples/reporting/` (3 new this sprint: pile capacity, on-bottom stability, SEPD decline)

Report structure:
1. Cover sheet (project, date, revision, engineer)
2. Design basis (inputs, applicable standards, environmental conditions)
3. Calculation sheets (one per module invoked, with clause references)
4. Results tables (pass/fail against code limits)
5. Code check summary (standards applied, clauses checked, verdict)
6. Gap register (what was not checked and why)

## Sprint Impact (WRK-1179)

| Metric | Before Sprint | After Sprint | Change |
|--------|:---:|:---:|:---:|
| Geotechnical functions | 0 | 13 | +13 |
| Geotechnical tests | 0 | 23 | +23 |
| Fatigue validation tests | 771 | 808 | +37 |
| Cap map "done" standards | 0 | 42 | +42 |
| Shared math helpers (assetutilities) | 0 | 5 | +5 |
| Math helper tests | 0 | 12 | +12 |
| Casing grades (API 5CT) | 0 | 10 | +10 |
| Pipe geometry functions | 0 | 3 | +3 |
| SEPD decline functions | 0 | 2 | +2 |
| SEPD decline tests | 0 | 10 | +10 |
| Portfolio beta function | 0 | 1 | +1 |
| Portfolio beta tests | 0 | 6 | +6 |
| Calc reports | 2 | 5 | +3 |

### What was delivered

1. **Geotechnical: pile axial capacity** (API RP 2GEO) — 5 functions, 9 tests. First geotechnical code in digitalmodel.
2. **Geotechnical: on-bottom stability** (DNV-RP-F109) — 8 functions, 14 tests. Unblocks Pattern 2.
3. **Spectral fatigue validation + NumPy fix** — 35 new tests, 808 total fatigue tests passing. Validates Dirlik, narrow-band, Wirsching-Light, Zhao-Baker, Tovo-Benasciutti methods.
4. **SEPD decline curve** (worldenergydata) — 2 functions, 10 tests. Extends decline analysis beyond Arps for unconventional wells.
5. **Math helpers: interpolation + integration** (assetutilities) — 5 functions, 12 tests. Shared linear/spline interpolation and trapezoidal/Simpson wrappers.
6. **API 5CT casing grades + pipe geometry** (assetutilities) — 10 grades (J55, K55, N80, L80, C90, T95, P110, Q125, S135, V150) + 3 pipe geometry functions, 10 tests.
7. **Portfolio beta** (assethold) — 1 function, 6 tests. Enables energy benchmark context (XLE, XOP).
8. **Cap map reconciliation** — 42 standards moved from gap to done across structural, subsea, cathodic protection, signal processing, and geotechnical.

---

*Updated: 2026-03-26 — Vision direction added, roadmap link added*
*See also: [ROADMAP.md](../../ROADMAP.md) for tiered development priorities*
