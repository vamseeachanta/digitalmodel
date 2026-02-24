# Plan: WRK-127 — Sanitize and Categorize Ideal spec.yml Templates

## Metadata
- **version**: 1
- **module**: orcaflex/templates
- **session.id**: twinkling-roaming-graham
- **session.agent**: claude-opus-4-6
- **review**: pending

## Context

The digitalmodel repo has **75 spec.yml files** across the OrcaFlex library, created by different processes (hand-crafted, auto-extracted) with wildly varying quality:

- **tier2_fast/** (4 risers): Excellent — inline comments, validated at 0.0% diff, 107-192 lines
- **pipeline/** (4): Good — documented, ~100 lines
- **model_library/** (53): Auto-extracted via WRK-121. Minimal docs, all `structure: generic`, 685-255K lines
- **jumper/installation/mooring/training/** (10): Mixed quality, 23-258K lines

**Problem**: No curated "start here" entry point exists. A user needing a mooring template has to dig through 53 model_library specs to find the right one.

**Goal**: Create `library/templates/` with 6 curated templates (one per major structure type), an audit script that classifies all 75 specs, and a machine-readable catalog.

## Inventory Summary

| Source | Count | Line Range | Quality | Model Schema |
|--------|-------|-----------|---------|-------------|
| tier2_fast/ | 4 | 107-192 | Excellent | `riser:` |
| pipeline/ | 4 | ~100 | Good | `pipeline:` |
| model_library/ | 53 | 685-255K | Minimal | `generic:` |
| jumper/ | 4 | 23-68K | Mixed | `generic:` |
| installation/ | 4 | 176+ | Mixed | `generic:` |
| mooring/ | 1 | 258K | Minimal | `generic:` |
| training/ | 3 | 371-148K | Minimal | `generic:` |
| reference/regional/ | 2 | varies | Minimal | varies |

## Implementation

### Phase 1: Audit Script

**File**: `scripts/audit_spec_library.py` (~300 lines)

Walks all spec.yml under `docs/modules/orcaflex/`, validates each against `ProjectInputSpec`, computes quality scores, classifies by structure type, and outputs structured results.

```bash
uv run python scripts/audit_spec_library.py
uv run python scripts/audit_spec_library.py --html audit_report.html
```

**Quality scoring** (0-100):
- Schema valid: +30
- Has inline comments: +20
- Meaningful name (not temp): +15
- Meaningful description (not "Extracted from..."): +10
- No raw_properties bloat: +15
- File size < 1000 lines: +10

**Category inference** from path + content signals:

| Signal | Category |
|--------|----------|
| tier2_fast/ or `riser:` key | `riser` |
| pipeline/ or `pipeline:` key | `pipeline` |
| C-series prefix | `mooring` |
| D/E/F/G-series prefix | `installation` |
| jumper/ in path or A03 | `jumper` |
| K-series prefix | `wind_turbine` |
| H-series prefix | `heavy_lift` |
| L-series prefix | `vessel` |

**Reuses**: `ProjectInputSpec` from `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py`

**Output**: `docs/modules/orcaflex/library/templates/audit_results.yaml`

### Phase 2: Create 6 Templates

Templates live in `docs/modules/orcaflex/library/templates/{name}/spec.yml`. Each uses real engineering defaults (not placeholders) so they validate through the existing benchmark.

**Template source selection** (prefer smallest, cleanest spec per category):

| Template | Source | Lines | Transform |
|----------|--------|-------|-----------|
| `riser_catenary/` | tier2_fast/a01_catenary_riser | 107 | Copy, add template header |
| `riser_lazy_wave/` | tier2_fast/a01_lazy_wave_riser | 192 | Copy, add template header |
| `pipeline_installation/` | pipeline/installation/floating/24in_pipeline | ~100 | Copy, add template header |
| `mooring_buoy/` | model_library/c07_metocean_buoy | 886 | Strip raw_properties, add comments, fix metadata |
| `installation_subsea/` | installation/mudmat | 176 | Add comments, fix metadata |
| `installation_pull_in/` | model_library/d02_pull_in_analysis | 722 | Strip raw_properties, add comments, fix metadata |

**Why these 6** (not 8):
- Wind turbine (k02: 50K lines) and vessel/FPSO (l01: 22K lines) are too large for useful templates — stripping properties loses the engineering content that makes them useful. These are better served by the existing model_library entries with improved metadata (Phase 1 audit output).
- 6 templates cover the most common use cases: riser (2 configs), pipeline, mooring, installation (2 scenarios).

**Template comment style** (matching tier2_fast gold standard):
```yaml
# OrcaFlex Modular Model Generator - Template Specification
# Mooring - Metocean Data Buoy
#
# Model Type: Generic (mooring/buoy configuration)
# Source: OrcaFlex Examples - C07 Metocean buoy
# Template Version: 1.0
#
# Usage:
#   Copy this directory, modify values, then generate:
#   uv run python -m digitalmodel.solvers.orcaflex.modular_generator generate \
#       --input spec.yml --output modular/

metadata:
  name: "mooring_buoy_template"
  description: "Metocean data buoy with mooring legs"
  structure: mooring              # riser | pipeline | mooring | installation | vessel
  operation: in-place             # in-place | installation | production
  project: "OrcaFlex Templates"
  version: "1.0"
```

**Transform steps for generic specs** (mooring_buoy, installation_pull_in):
1. Load YAML, validate against `ProjectInputSpec`
2. Fix `metadata.name` → template name, `metadata.structure` → actual type
3. Remove `environment.raw_properties` (duplicates structured fields)
4. Keep `generic.*` sections with their `properties:` bags (needed for round-trip)
5. Add header comment block
6. Add inline comments on metadata and environment fields
7. Re-validate against `ProjectInputSpec`

### Phase 3: Catalog and Validation

**Catalog file**: `docs/modules/orcaflex/library/templates/catalog.yaml`

```yaml
version: "1.0"
templates:
  - name: riser_catenary
    path: riser_catenary/spec.yml
    schema_type: riser
    structure: riser
    description: "Simple catenary riser connected to FPSO"
    source: library/tier2_fast/a01_catenary_riser/spec.yml
    water_depth_m: 100
    benchmark_validated: true
    complexity: minimal
    tags: [riser, catenary, fpso, production]
  # ... one entry per template
```

**Validation**:
- **Schema validation** (all 6): `ProjectInputSpec(**yaml.safe_load(spec))` — fast, no license needed
- **Statics validation** (riser + pipeline): Already proven at 0.0% diff in 3-way benchmark
- **Statics validation** (generic templates): Run via `benchmark_model_library.py --library-only` on templates dir

## Directory Structure (final)

```
docs/modules/orcaflex/library/templates/
├── catalog.yaml
├── audit_results.yaml
├── riser_catenary/
│   └── spec.yml                  # 107 lines, riser: schema
├── riser_lazy_wave/
│   └── spec.yml                  # 192 lines, riser: schema
├── pipeline_installation/
│   └── spec.yml                  # ~100 lines, pipeline: schema
├── mooring_buoy/
│   └── spec.yml                  # ~200 lines (trimmed from 886), generic: schema
├── installation_subsea/
│   └── spec.yml                  # ~176 lines, generic: schema
└── installation_pull_in/
    └── spec.yml                  # ~200 lines (trimmed from 722), generic: schema
```

## Critical Files

| File | Action | Purpose |
|------|--------|---------|
| `scripts/audit_spec_library.py` | CREATE | Audit + classify all 75 specs |
| `docs/modules/orcaflex/library/templates/catalog.yaml` | CREATE | Machine-readable index |
| `docs/modules/orcaflex/library/templates/*/spec.yml` | CREATE | 6 curated templates |
| `src/.../modular_generator/schema/root.py` | READ | ProjectInputSpec for validation |
| `src/.../modular_generator/schema/generic.py` | READ | GenericModel schema reference |
| `src/.../modular_generator/extractor.py` | READ | Extraction patterns to reuse |
| `docs/modules/orcaflex/library/tier2_fast/a01_*/spec.yml` | READ | Gold standard template sources |

## Task Sequence

1. **Write audit test stubs** (TDD) → `tests/scripts/test_audit_spec_library.py`
2. **Implement audit script** → `scripts/audit_spec_library.py`
3. **Run audit**, review results, validate category inference
4. **Copy clean templates** (riser x2, pipeline) — trivial, add headers
5. **Transform generic templates** (mooring, installation x2) — strip raw_properties, add comments
6. **Create catalog.yaml** from audit + template metadata
7. **Schema-validate all 6 templates**
8. **Statics-validate** where OrcFxAPI available

## Scope Limits

- Does NOT create templates for wind_turbine (50K lines) or vessel/FPSO (22K lines) — too large, better served by model_library with improved metadata
- Does NOT modify existing model_library specs — templates are curated copies
- Does NOT create README files per template — inline YAML comments serve as docs
- Does NOT create a template generation CLI — manual curation is intentional for quality

## Verification

```bash
# 1. Run audit
uv run python scripts/audit_spec_library.py --html benchmark_output/spec_audit.html

# 2. Schema-validate all templates
uv run python -c "
import yaml
from pathlib import Path
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
for p in sorted(Path('docs/modules/orcaflex/library/templates').rglob('spec.yml')):
    data = yaml.safe_load(p.read_text())
    spec = ProjectInputSpec(**data)
    print(f'PASS: {spec.metadata.name} ({p.parent.name})')
"

# 3. Statics validation (requires OrcFxAPI)
uv run python scripts/benchmark_model_library.py \
    --library-only --three-way --skip-mesh \
    --library-root docs/modules/orcaflex/library/templates
```
