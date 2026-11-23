# Plan: digitalmodel #537 — manifest.yml not written when all runs skipped

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/537
**Status:** plan-review
**Tier:** T3

## Context

`CampaignGenerator.generate(spec_only=True)` at `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` writes `manifest.yml` only when `manifest_runs` is non-empty:

```python
if spec_only and manifest_runs:
    manifest_path = output_dir / "manifest.yml"
```

A `--resume` re-run where every combo's output dir already exists produces `manifest_runs == []` → no manifest → `result.manifest_path is None`. The current docstring claims "manifest_path is set only in spec_only mode" without qualifying the empty-runs case. Callers that assume `spec_only=True` ⇒ manifest exists will hit a silent `None`.

Code-review follow-up from #511 / PR #533, defect ID **m7**, DEFERRED in commit `1d96aa63`. Sibling defects: #534, #535, #536. Issue presents three resolution options (A: always write; B: document caveat; C: track all attempted runs). Issue recommends **B** for minimal disruption — this plan adopts that recommendation.

## Plan

1. **Confirm current behavior.** Read `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` around the `manifest_path` assignment — locate the exact docstring and the `if spec_only and manifest_runs:` guard. Run `git grep -n "manifest_path\\|manifest_runs" src/digitalmodel/solvers/orcaflex/modular_generator/` to confirm there is no second write path.

2. **Update the `generate()` docstring.** Replace the existing manifest sentence with:
   ```
   manifest_path is set in spec_only mode when at least one run is generated;
   it is None if all combos were skipped (e.g., --resume against fully-existing dirs).
   ```
   Keep the rest of the docstring untouched. This is the minimum-disruption Option B.

3. **Add explicit assertion test.** New test in `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py`:
   `test_spec_only_resume_all_skipped_manifest_is_none` — builds a small campaign, calls `generate(spec_only=True)` once to materialize all output dirs, calls again with the same args, asserts `result.manifest_path is None` and `result.runs` is empty (or whatever the existing skip-marker is). Locks the documented behavior.

4. **Audit callers for unguarded access.** Run `git grep -n "result\\.manifest_path\\|manifest_path\\." src/ tests/ examples/` to find consumers. Any code that treats `manifest_path` as non-`None` in `spec_only` mode without a `None` check is a latent bug — file follow-up issues for those, do NOT fix in this plan (out of scope).

5. **Smoke check.** `uv run pytest tests/solvers/orcaflex/modular_generator/schema/test_campaign.py -v -k "manifest or spec_only"` then full module run.

## Acceptance Criteria

- [ ] Updated docstring explicitly names the all-skipped → `None` case
- [ ] New `test_spec_only_resume_all_skipped_manifest_is_none` test passes
- [ ] No change to existing `generate()` semantics (behavior preservation, doc-only fix)
- [ ] Caller-audit findings recorded as issue comment (zero or N follow-up issues filed)
- [ ] All pre-existing `test_campaign.py` tests continue to pass
