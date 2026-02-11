# Plan: Semantic Equivalence for 5 Library Models

## Metadata
- **version**: 2
- **module**: orcaflex/modular_generator
- **session.id**: twinkling-roaming-graham
- **session.agent**: claude-opus-4-6
- **review**: codex=REQUEST_CHANGES, gemini=APPROVE → revised

## Context

The 5 library models (4 risers + 1 generic) currently show:
- **Path B (spec-driven)**: 4/5 pass at 0.00% tension diff, 1 fails (steep wave PenetratingLine ref)
- **Path C (modular-direct)**: 5/5 pass at 0.00%

Semantic validation of the **input YAML** shows 0/5 fully matching. Many diffs are stale artifacts (modular files pre-date user's environment builder edits). The goal is to get generated YAML semantically identical to monolithic, with analysis results (tension AND bending moment) within 1%.

**YAML format equivalence**: `yaml.safe_load()` normalizes flow arrays vs block lists, inline vs multi-line dicts, quoting variations — all transparent.

## Acceptance Criteria (Codex P1 fix)

### Semantic Diff Policy

**Significant diffs** = any property difference that is NOT in the "allowed" list below:

| Classification | Rule | Action |
|---------------|------|--------|
| **Allowed (ignore)** | Cosmetic/view props: `DefaultView*`, `DrawNodes*`, `DrawShaded*`, `ContactPen`, `State` | Skip in comparison |
| **Allowed (ignore)** | Dormant/mode-dependent props that OrcaFlex fills automatically | Skip in comparison |
| **Cosmetic (<0.01%)** | Floating-point precision differences | Report but pass |
| **Minor (0.01-1%)** | Small numeric differences | Report but pass |
| **Significant (>1%)** | Property values differ materially | FAIL |
| **Type mismatch** | True/False vs "Yes"/"No" | FAIL (must fix) |
| **Missing** | Property in mono, not in modular | Evaluate per-property |
| **Extra** | Property in modular, not in mono | Evaluate per-property |

### Target Metrics (per model)

- **Tension**: <1% difference vs monolithic
- **Bending moment**: <1% difference vs monolithic
- **Semantic equivalence**: 0 significant diffs, 0 type mismatches

### Benchmark Behavior on Semantic Failure (Codex P3 fix)

Semantic validation is **report-only** (warn but proceed). Statics always runs regardless of semantic result. This is because:
- Models with semantic diffs may still produce correct analysis results
- The purpose is diagnostic, not gating

## Approach: Regenerate → Measure → Fix → Verify

### Step 1: Regenerate modular files for all 5 models

Regenerate the `modular/` directory for each library model using current code (with user's True/False booleans, "Wave 1" name, SeabedSlopeDirection=0 edits). This eliminates stale-file artifacts.

**Models:**
- `docs/modules/orcaflex/library/tier2_fast/a01_catenary_riser/`
- `docs/modules/orcaflex/library/tier2_fast/a01_lazy_wave_riser/`
- `docs/modules/orcaflex/library/tier2_fast/a01_pliant_wave_riser/`
- `docs/modules/orcaflex/library/tier2_fast/a01_steep_wave_riser/`
- `docs/modules/orcaflex/library/model_library/a02_lazy_s_detailed/`

**Rollback** (Codex P2 fix): `git stash` before regeneration; `git stash pop` to restore if regressions detected.

### Step 2: Run semantic validation to measure actual diff count

```bash
uv run python scripts/semantic_validate.py --batch docs/modules/orcaflex/library/tier2_fast \
    --batch-report benchmark_output/semantic_5models.html
```

### Step 3: Fix remaining differences (prioritized)

Replace the 3-agent parallel approach (Codex/Gemini feedback) with a **prioritized fix-and-verify loop**:

**Priority 1 — HIGH impact (affect analysis results):**

| Fix | File | Details |
|-----|------|---------|
| 6DBuoy Mass/Volume roundtrip | `schema/generic.py`, `generic_builder.py` | Verify `mass`/`volume` fields survive extraction → spec → generation |
| CurrentProfile depth-varying | `extractor.py`, `environment_builder.py` | Ensure multi-level profiles survive roundtrip (Lazy S: `[[0,1,0],[100,0.2,0]]`) |
| Steep wave PenetratingLine | `extractor.py` or `generic_builder.py` | Cross-reference validation: ensure LineContactData references match generated line names |

**Priority 2 — MEDIUM impact (may affect convergence):**

| Fix | File | Details |
|-----|------|---------|
| Line default props | riser builders | `AsLaidTension`, `PreBendSpecifiedBy`, `StaticsSeabedFrictionPolicy` — missing in riser output |
| StageDuration mismatch | `general_builder.py` | Simulation duration extraction |

**Priority 3 — LOW impact (cosmetic, don't affect analysis):**

| Fix | File | Details |
|-----|------|---------|
| NorthDirection extra | `general_builder.py` | Only emit if explicitly set in spec |
| DefaultView* missing | Ignore | Add to allowed-diff exclusion list in `semantic_validate.py` |
| Groups State diff | Ignore | Add to allowed-diff exclusion list |

**Unit tests for each fix** (Gemini suggestion):
- Test 6DBuoy mass roundtrip: extract → build → verify mass field present
- Test current profile roundtrip: extract multi-level profile → build → verify all levels present
- Test cross-reference consistency: extract model with LineContactData → verify all referenced names exist

### Step 4: Integrate semantic validation into benchmark pipeline

Modify `scripts/benchmark_model_library.py`:

1. **Import** `semantic_validate.load_monolithic`, `load_modular`, `validate`, `Significance`
2. **In `run_spec_driven()`**: After `gen.generate(mod_dir)`, before `OrcFxAPI.Model(str(master))`:
   ```python
   mono_yaml = load_monolithic(yml_path)
   mod_yaml = load_modular(mod_dir)
   sem_results = validate(mono_yaml, mod_yaml)
   sem_summary = summarize(sem_results)  # compact dict for JSON
   ```
3. **Return type**: Add `sem_summary` to return tuple
4. **Add to `ModelBenchmark`**: `semantic_total_sections`, `semantic_sections_with_diffs`, `semantic_significant_count`, `semantic_sections`
5. **Console output**: `Semantic check: N/M sections match (K significant diffs)`
6. **HTML report**: Add "Semantic" column to executive summary + per-model collapsible section

### Step 5: Verify

```bash
uv run python scripts/benchmark_model_library.py --library-only --three-way --skip-mesh
```

**Must achieve for all 5 models:**
- Tension: <1% diff vs monolithic
- Bending moment: <1% diff vs monolithic (bending extracted from range_data)
- Semantic: 0 significant diffs, 0 type mismatches

## Files Modified

| File | Change |
|------|--------|
| `scripts/benchmark_model_library.py` | Semantic validation pre-statics, ModelBenchmark fields, HTML report |
| `scripts/semantic_validate.py` | `summarize()` helper, cosmetic-props exclusion list |
| `src/.../extractor.py` | Current profile extraction fix |
| `src/.../builders/general_builder.py` | Conditional NorthDirection, StageDuration |
| `src/.../builders/generic_builder.py` | Cross-reference validation |
| `src/.../schema/generic.py` | Verify 6DBuoy mass/volume roundtrip |
| `docs/.../tier2_fast/*/modular/` | Regenerated modular files |
| `docs/.../model_library/a02_lazy_s_detailed/modular/` | Regenerated |
| `tests/.../test_semantic_roundtrip.py` | New: unit tests for roundtrip fidelity |

## Cross-Review Results

| Reviewer | Verdict | Action Taken |
|----------|---------|-------------|
| Codex | REQUEST_CHANGES | P1: Added acceptance criteria table. P2: Replaced 3-agent with prioritized loop. P2: Added rollback strategy. P3: Defined failure behavior (warn, don't gate). |
| Gemini | APPROVE | Simplified Step 3. Added unit test suggestion. |

## Work Item

Track as: **WRK-108 — Semantic equivalence for 5 library models (tension + bending <1%)**
