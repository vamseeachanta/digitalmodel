# Plan: Fix 5 Remaining B&C Failures

## Metadata
- **version**: 3
- **module**: orcaflex/modular_generator
- **session.id**: twinkling-roaming-graham
- **session.agent**: claude-opus-4-6

## Context

51-model library benchmark shows 44/49 passing. 5 models fail on Path B and C:
- **B01**: PyModel dependency (unfixable — external Python soil plugin)
- **K02**: `VerticalWindVariationFactor=~` (Change not allowed)
- **K03, L04, L05**: `RayleighDampingCoefficients=Tower damping` (Invalid value)

## Fix 1: K02 — VerticalWindVariationFactor (dormant wind property)

**Root cause**: `environment_builder.py` line 79 emits `VerticalWindVariationFactor: None` in `_DEFAULTS`. This property is dormant when `WindType: Full field` — only valid for spectrum-based wind types (API/NPD/ESDU).

**Fix**: Remove `VerticalWindVariationFactor` from `_DEFAULTS` dict. Only emit it when wind type is spectrum-based, via the existing `_WIND_TYPE_PROPS` gate pattern (lines 124-149).

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py`

## Fix 2: K03/L04/L05 — RayleighDampingCoefficients section ordering

**Root cause**: In `generic_builder.py` `_SECTION_ORDER`, `RayleighDampingCoefficients` is at line 76 — AFTER `Lines` (line 62). But LineTypes reference named RayleighDamping sets, so the singleton must be loaded BEFORE LineTypes.

**Dependency chain**: RayleighDampingCoefficients → LineTypes → Lines

**Fix**: Move `"RayleighDampingCoefficients"` in `_SECTION_ORDER` from after object instances to BEFORE `LineTypes` (after `ExpansionTables`). Also move `"FrictionCoefficients"` for the same reason (referenced by LineTypes).

**File**: `src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py`

## Verification

```bash
uv run python scripts/benchmark_model_library.py --library-only --three-way --skip-mesh
```

Target: K02, K03, L04, L05 should now pass Path B and C (48/49, only B01 remaining).
