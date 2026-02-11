---
name: orcaflex-monolithic-to-modular
description: Convert monolithic OrcaFlex models (.dat/.yml) to spec-driven modular format with semantic validation for round-trip fidelity.
version: 2.0.0
updated: 2026-02-10
category: offshore-engineering
triggers:
- convert monolithic to modular
- extract OrcaFlex spec
- modularize OrcaFlex model
- create OrcaFlex includes
- reverse engineer OrcaFlex YAML
- monolithic to spec.yml
- semantic validation OrcaFlex
---
# OrcaFlex Monolithic to Modular Converter

Convert monolithic OrcaFlex models into spec-driven modular format using the `MonolithicExtractor` → `ProjectInputSpec` → `ModularModelGenerator` pipeline.

## When to Use

- Converting `.dat` / `.yml` OrcaFlex models to portable `spec.yml` format
- Creating reusable component libraries from existing models
- Validating that modular output is semantically equivalent to monolithic source
- Preparing models for parametric studies or automated benchmarking
- Building a spec.yml foundation for any new OrcaFlex model

## Architecture

```
┌──────────────┐    ┌────────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│ .dat / .yml  │───>│ MonolithicExtractor │───>│ ProjectInputSpec │───>│ ModularModel     │
│ (monolithic) │    │   (extractor.py)    │    │   (spec.yml)     │    │ Generator        │
└──────────────┘    └────────────────────┘    └──────────────────┘    └────────┬────────┘
                                                                               │
                                                                    ┌──────────▼──────────┐
                                                                    │  modular/           │
                                                                    │  ├── master.yml     │
                                                                    │  ├── includes/      │
                                                                    │  │   ├── 01_general │
                                                                    │  │   ├── 03_env     │
                                                                    │  │   └── 20_generic │
                                                                    │  └── inputs/        │
                                                                    │      └── params.yml │
                                                                    └─────────────────────┘
```

## Pipeline Steps

### Step 1: Convert .dat to .yml (if needed)

```python
import OrcFxAPI

model = OrcFxAPI.Model("model.dat")
model.SaveData("model.yml")  # OrcaFlex YAML export
```

### Step 2: Extract spec from monolithic YAML

```python
from digitalmodel.solvers.orcaflex.modular_generator.extractor import MonolithicExtractor

ext = MonolithicExtractor(Path("model.yml"))
spec_dict = ext.extract()
# Returns: {"metadata": {...}, "environment": {...}, "simulation": {...}, "generic": {...}}
```

The extractor:
- Reads multi-document YAML (handles `---` separators)
- Maps OrcaFlex keys to spec schema (typed fields + properties bag)
- Handles section name aliases (Groups/BrowserGroups, FrictionCoefficients/SolidFrictionCoefficients)
- Extracts current profiles from multi-column keys
- Captures `raw_properties` for diagnostic use

### Step 3: Validate and create spec

```python
from digitalmodel.solvers.orcaflex.modular_generator.schema.root import ProjectInputSpec

spec = ProjectInputSpec(**spec_dict)
# Pydantic validates all fields, applies defaults
```

### Step 4: Generate modular output

```python
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

gen = ModularModelGenerator.from_spec(spec)
gen.generate(Path("output/modular"))
```

### Step 5: Semantic validation

```python
from scripts.semantic_validate import load_monolithic, load_modular, validate, summarize

mono = load_monolithic(Path("model.yml"))
mod = load_modular(Path("output/modular"))
results = validate(mono, mod)
summary = summarize(results)

print(f"Match: {summary['total_sections'] - summary['sections_with_diffs']}/{summary['total_sections']}")
print(f"Significant diffs: {summary['significant_diffs']}")
```

## Extractor Details

### Section Mapping

The extractor uses three mapping dicts from `schema/generic.py`:

| Mapping | Purpose | Example |
|---------|---------|---------|
| `FIELD_TO_SECTION` | spec field → OrcaFlex YAML key | `"line_types"` → `"LineTypes"` |
| `SINGLETON_SECTIONS` | singleton section → field | `"FrictionCoefficients"` → `"friction_coefficients"` |
| `TYPED_FIELD_MAP` | typed field → OrcaFlex prop | `"mass"` → `"Mass"` |

### Section Name Aliases

OrcaFlex `SaveData()` exports may use different section names than the API:

| YAML Export Name | API/Internal Name |
|-----------------|-------------------|
| `Groups` | `BrowserGroups` |
| `FrictionCoefficients` | `SolidFrictionCoefficients` |

The extractor handles both via `_SECTION_ALIASES` fallback in `_extract_singleton()`.

### Object Extraction Strategy

For each object in a list section:
1. Keys in `TYPED_FIELD_MAP` → extracted as typed Pydantic fields
2. All other keys → placed in `properties` dict (pass-through bag)
3. Both recombined by builder's `_merge_object()` during generation

## Semantic Validation

### Significance Levels

| Level | Meaning | Action |
|-------|---------|--------|
| `match` | Values identical (within tolerance) | None |
| `cosmetic` | < 0.01% numeric diff | Safe to ignore |
| `minor` | 0.01-1% numeric diff | Review if important |
| `significant` | > 1% numeric diff | Must investigate |
| `missing` | Present in mono, absent in mod | Fix extractor/builder |
| `extra` | Absent in mono, present in mod | Fix builder defaults |
| `type_mismatch` | Different types (bool vs string) | Fix type handling |

### Running Semantic Validation

```bash
# Single model
uv run python scripts/semantic_validate.py model.yml modular_dir/

# With HTML report
uv run python scripts/semantic_validate.py model.yml modular_dir/ --html report.html

# Batch mode
uv run python scripts/semantic_validate.py model.yml modular_dir/ --batch output_dir/
```

### Integrated in Benchmark

The benchmark pipeline (`scripts/benchmark_model_library.py`) runs semantic validation as a pre-statics gate:
```
.dat → YAML → extract → spec → generate modular → [SEMANTIC CHECK] → [statics] → compare
```

## Output Structure

```
modular/
├── master.yml              # Entry point with include directives
├── includes/
│   ├── 01_general.yml      # General section (simulation, solver settings)
│   ├── 03_environment.yml  # Environment (water, waves, current, wind)
│   └── 20_generic_objects.yml  # All object sections (types, instances, singletons)
└── inputs/
    └── parameters.yml      # Extracted key parameters
```

## Common Issues and Fixes

| Issue | Root Cause | Fix |
|-------|-----------|-----|
| Missing section in modular | Extractor doesn't map the YAML key | Add to `FIELD_TO_SECTION` or `SINGLETON_SECTIONS` |
| Property value lost | GenericObject subclass lacks typed field | Add field to schema class (Pydantic silent drop) |
| `None` vs missing diff | Builder skips None values | Use `model_fields_set` in `_merge_object()` |
| Boolean mismatch | SaveData() exports `True`/`False`, builder uses `"Yes"`/`"No"` | Use Python booleans in builder defaults |
| "Change not allowed" error | Dormant properties in re-loaded YAML | Use hardcoded safe defaults, not raw pass-through |

## Benchmark Results (2026-02-10)

All 5 library models at **100% semantic match** (0 significant diffs):

| Model | Sections | Significant | Statics |
|-------|----------|-------------|---------|
| Catenary | 8/8 | 0 | CONVERGED |
| Lazy Wave | 9/9 | 0 | CONVERGED |
| Pliant Wave | 11/11 | 0 | CONVERGED |
| Steep Wave | 11/11 | 0 | CONVERGED |
| Lazy S | 12/12 | 0 | CONVERGED |

## Related Skills

- [orcaflex-model-generator](../orcaflex-model-generator/SKILL.md) - Builder registry and generation architecture
- [orcaflex-yaml-gotchas](../orcaflex-yaml-gotchas/SKILL.md) - Production OrcaFlex YAML traps
- [orcaflex-environment-config](../orcaflex-environment-config/SKILL.md) - Environment configuration

## References

- Extractor: `src/digitalmodel/solvers/orcaflex/modular_generator/extractor.py`
- Schema: `src/digitalmodel/solvers/orcaflex/modular_generator/schema/generic.py`
- Semantic validator: `scripts/semantic_validate.py`
- Benchmark: `scripts/benchmark_model_library.py`
- Spec library: `docs/modules/orcaflex/library/tier2_fast/`

---

## Version History

- **2.0.0** (2026-02-10): Complete rewrite. Documents actual MonolithicExtractor pipeline, section name aliases, semantic validation, Pydantic integration, and benchmark results.
- **1.0.0** (2026-01-21): Initial release with manual splitting approach.
