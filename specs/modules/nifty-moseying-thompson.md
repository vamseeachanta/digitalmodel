# Plan: Consolidate docs/eng into docs/modules

## Context

The project has two documentation directories under `docs/`:
- **`docs/eng/`** — 374 files, general engineering reference library (PDFs, markdown, images). **Zero code references** anywhere in the codebase.
- **`docs/modules/`** — 7,406 files, project-specific modules with specs, examples, and tools. **Extensively referenced** in source code, scripts, tests, and skills.

Having two directories creates confusion about where to find or place documentation. Since `docs/modules/` is the active, code-referenced directory and `docs/eng/` is unreferenced, we merge eng INTO modules and delete `docs/eng/`.

## Approach

**Direction**: Move all `docs/eng/` content into `docs/modules/`, then delete `docs/eng/`.

Three categories of moves:
1. **9 overlapping directories** — merge eng files into existing modules subdirectories
2. **35 non-overlapping eng subdirectories** — map to existing or new modules subdirectories
3. **Root-level eng files** (~95 files) — distribute by topic into modules subdirectories
4. **`docs/eng/ref/`** (168 files) — distribute by topic per user preference

## File Move Map

### 1. Overlapping Directories (eng → modules)

| eng/ directory | Action | Notes |
|---|---|---|
| `cathodic_protection/` (1 file) | Merge into `modules/cathodic_protection/` | modules has 40 files already |
| `metocean/` (4 files) | Merge into `modules/metocean/` | modules has 13 files |
| `pipelines/` (7 files) | Merge into `modules/pipelines/` | modules has 34 files |
| `reservoir/` (11 files) | Merge into `modules/reservoir/` | eng has more depth here |
| `signal_processing/` (1 file) | Merge into `modules/signal_processing/` | both minimal |
| `structural/` (10 files, 2 subdirs) | Merge into `modules/structural/` | complementary content |
| `subsea/` (2 files) | Merge into `modules/subsea/` | modules has 5 files |
| `viv/` (1 file) | Merge into `modules/viv/` | modules has 3 files |
| `wind/` (21 files) | Merge into `modules/wind/` | eng actually has more here |

### 2. Non-Overlapping Directories (eng → modules)

| eng/ directory | Target in modules/ | Rationale |
|---|---|---|
| `abaqus/` (1) | `modules/ansys/` | FEA solvers group |
| `accidents/` (2) | `modules/standards/accidents/` | Incident case studies |
| `animation/` (3) | `modules/visualization/` | 3D visualization tools |
| `casing_design/` (3) | `modules/drilling/casing_design/` | Well construction |
| `co2_storage/` (1) | `modules/production/` | Carbon capture/storage |
| `connectors/` (4) | `modules/subsea/connectors/` | Subsea connectors |
| `data_driven/` (4) | `modules/data_systems/` | Data-driven methods |
| `decomm/` (1) | `modules/offshore_installation/` | Decommissioning |
| `energy/` (1) | `modules/references/` | Energy transition |
| `environment/` (3) | `modules/metocean/` | Environmental data |
| `fea/` (4, incl nastran/) | `modules/fem/` | FEA reference (modules has `fem/`) |
| `ffs/` (6) | `modules/structural/ffs/` | Fitness for service |
| `field_development/` (11) | `modules/references/field_development/` | Field planning |
| `fluid_machanics/` (4) | `modules/hydrodynamics/` | Fluid mechanics |
| `geotech/` (3) | `modules/references/geotech/` | Geotechnical (no existing module) |
| `geothermal/` (3) | `modules/references/geothermal/` | No existing module |
| `hpht/` (2) | `modules/drilling/hpht/` | HPHT well design |
| `hydrogen/` (1) | `modules/references/hydrogen/` | No existing module |
| `interfaces/` (2) | `modules/structural/` | Lug/interface design |
| `intervention/` (1) | `modules/interventions/` | modules has `interventions/` |
| `manufacturing/` (1) | `modules/references/manufacturing/` | No existing module |
| `materials/` (2) | `modules/structural/materials/` | Material properties |
| `moorings/` (3) | `modules/mooring/` | modules has `mooring/` |
| `navigation/` (1) | `modules/marine_ops/` | Navigation/positioning |
| `offshore_installation/` (18) | `modules/offshore_installation/` | Direct match |
| `process/` (1) | `modules/references/process/` | Process engineering |
| `ref/` (168) | Distributed — see Section 3 | By topic |
| `rigid_jumper/` (1) | `modules/risers/` | Riser-adjacent |
| `simulation/` (1) | `modules/references/` | General simulation |
| `trees/` (2) | `modules/subsea/` | Subsea trees |
| `umbilicals/` (4) | `modules/umbilical/` | modules has `umbilical/` |

### 3. ref/ Distribution (168 files by topic)

| Topic prefix/pattern | Target in modules/ | Count |
|---|---|---|
| Drilling, well construction, MWD, casing | `drilling/references/` | ~28 |
| Wind, FOWT, turbine, offshore wind | `wind/references/` | ~13 |
| Structural, FFS, FAD, buckling, FEA | `structural/references/` | ~12 |
| Standards (API, DNV, BS, BSEE, Macondo) | `standards/` | ~12 |
| Fatigue, ECA, GoM fatigue | `fatigue/references/` | ~10 |
| Reservoir, petrophysics, well testing | `reservoir/references/` | ~8 |
| Data engineering, SQL, analytics | `data_systems/references/` | ~8 |
| Marine ops, vessels, tankers, positioning | `marine_ops/references/` | ~8 |
| Signal processing, DSP, vibration, seismic | `signal_processing/references/` | ~7 |
| Visualization (D3, charts) | `visualization/references/` | ~7 |
| Artificial lift, ESP, SRP | `artificial_lift/references/` | ~5 |
| Subsea production, intervention | `subsea/references/` | ~6 |
| Welding, pipe joining | `welding/references/` | ~4 |
| Mooring, FLNG | `mooring/references/` | ~2 |
| VIV | `viv/references/` | ~1 |
| Cathodic protection, corrosion | `cathodic_protection/references/` | ~1 |
| Non-engineering (SDEV, FIN, CLD, MAN) | `references/misc/` | ~24 |
| Math, general, uncategorized | `references/` | ~12 |

### 4. Root-Level eng/ Files (~95 files)

Same topic-based distribution as ref/. Key mappings:
- `*drilling*`, `*well*` → `modules/drilling/`
- `*fatigue*` → `modules/fatigue/`
- `*riser*`, `*catenary*` → `modules/risers/`
- `*metocean*`, `*waves*` → `modules/metocean/`
- `*wind*` → `modules/wind/`
- `*installation*`, `*pipelay*` → `modules/offshore_installation/`
- General O&G guides → `modules/guides/`
- Standards/regulatory → `modules/standards/`

## Execution Steps

1. **Create a move script** (`scripts/consolidate_docs.py`) that:
   - Reads the mapping (hardcoded dict or YAML)
   - Copies each file from `docs/eng/` to its target in `docs/modules/`
   - Creates `references/` subdirectories where needed
   - Logs every move for auditability
   - Handles filename collisions (prefix with `eng_` if duplicate names exist)

2. **Run the script** and verify file counts match (374 files moved)

3. **Delete `docs/eng/`** after verification

4. **Update docs/modules index files** — add entries for newly populated directories:
   - `docs/modules/_index.md`
   - `docs/modules/README.md`
   - `docs/modules/domains_index.md`

5. **No code changes needed** — zero references to `docs/eng` exist in the codebase

## Files to Create/Modify

| File | Action |
|---|---|
| `scripts/consolidate_docs.py` | **Create** — migration script with full move mapping |
| `docs/modules/_index.md` | **Edit** — add new module entries |
| `docs/modules/README.md` | **Edit** — update organization description |
| `docs/modules/domains_index.md` | **Edit** — add new domain entries |
| `docs/eng/` (entire directory) | **Delete** after migration |

## Verification

1. Count files before: `docs/eng/` should have 374 files
2. Run migration script
3. Count files after: `docs/modules/` should gain ~374 files
4. Verify `docs/eng/` is empty, then delete it
5. Run existing tests: `uv run pytest tests/ -x --timeout=30` — should have zero impact (no code references docs/eng)
6. Spot-check 5-10 files in their new locations
