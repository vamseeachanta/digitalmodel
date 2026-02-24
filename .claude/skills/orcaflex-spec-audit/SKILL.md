---
name: orcaflex-spec-audit
description: Audit, classify, and score OrcaFlex spec.yml files across the model library for quality, schema validity, and structure type categorization.
version: 1.0.0
updated: 2026-02-11
category: offshore-engineering
triggers:
- audit spec library
- classify OrcaFlex specs
- spec quality score
- categorize models
- template catalog
---
# OrcaFlex Spec Library Audit

Audit and classify all `spec.yml` files in the OrcaFlex library. Scores quality (0-100), validates against `ProjectInputSpec` schema, and infers structural category from path and content signals.

## Usage

```bash
# Basic audit (YAML output)
uv run python scripts/audit_spec_library.py

# With HTML report
uv run python scripts/audit_spec_library.py --html benchmark_output/spec_audit.html

# Custom root directory
uv run python scripts/audit_spec_library.py --root docs/modules/orcaflex/library/templates
```

## Quality Scoring (0-100)

| Criterion | Points | Check |
|-----------|--------|-------|
| Schema valid | +30 | `ProjectInputSpec(**data)` passes |
| Has comments | +20 | Lines starting with `#` |
| Meaningful name | +15 | Not temp/single-char/digits-only |
| Meaningful description | +10 | Not "Extracted from...", not "generic", len >= 10 |
| No raw_properties | +15 | `environment.raw_properties` absent |
| Compact file | +10 | < 1000 lines |

## Category Inference

Priority order:
1. **Schema key**: `riser:` key -> riser, `pipeline:` key -> pipeline
2. **Path parts**: `tier2_fast` -> riser, `jumper` -> jumper, `training` -> training
3. **Model library prefix**: `a`=riser, `b`=drilling, `c`=mooring, `d-g`=installation, `h`=heavy_lift, `k`=wind_turbine, `l`=vessel, `m`=pipeline
4. **Metadata fallback**: `metadata.structure` field value

## Output

### YAML (`audit_results.yaml`)
```yaml
summary:
  total: 81
  avg_quality: 53.0
  schema_valid_count: 73
  by_category: {riser: 19, installation: 18, mooring: 10, ...}
specs:
  - path: library/templates/riser_catenary/spec.yml
    name: riser_catenary_template
    quality_score: 100
    schema_valid: true
    category: riser
    model_type: riser
    line_count: 108
```

### HTML Report
Single-page report with summary cards, per-category tables, color-coded quality scores (green >= 70, yellow 40-69, red < 40).

## Template Library

6 curated templates at `docs/modules/orcaflex/library/templates/`:

| Template | Source | Lines |
|----------|--------|-------|
| riser_catenary | tier2_fast/a01_catenary_riser | 108 |
| riser_lazy_wave | tier2_fast/a01_lazy_wave_riser | 188 |
| pipeline_installation | pipeline/.../24in_pipeline | 101 |
| mooring_buoy | model_library/c07_metocean_buoy | 763 |
| installation_subsea | installation/mudmat | 196 |
| installation_pull_in | model_library/d02_pull_in_analysis | 645 |

All templates score 100/100 quality and pass schema validation.

## References

- Audit script: `scripts/audit_spec_library.py`
- Tests: `tests/scripts/test_audit_spec_library.py`
- Template catalog: `docs/modules/orcaflex/library/templates/catalog.yaml`
- Audit results: `docs/modules/orcaflex/library/templates/audit_results.yaml`
- Schema: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/root.py`
