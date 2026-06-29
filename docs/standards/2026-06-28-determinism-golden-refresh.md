# Determinism golden refresh / re-sanction procedure (workspace-hub#3283)

> Scope: the `ResultEnvelope` determinism goldens consumed by
> `digitalmodel.workflow_api.golden.golden_workflow_test` (e.g.
> `tests/workflow_api/goldens/buckling_parametric.json`). This is the control-plane
> procedure for the determinism guarantee the whole determinism epic
> ([workspace-hub#3281](https://github.com/vamseeachanta/workspace-hub/issues/3281))
> rests on.

## What a determinism golden is

A committed JSON snapshot pinning the `determinism.result_hash` (and, optionally, the
volatile-pruned envelope) emitted by a `run_workflow` call. The test verdict is
**string equality of `result_hash`** — the #3282-owned, content-based hash. It is
NOT a value-tolerance comparison: if a single emitted output byte changes, the
per-file `sha256` (and therefore the overall `result_hash`) flips and the golden
fails. That is the point — drift must be loud.

## When a golden legitimately changes

A `result_hash` change is EXPECTED only when the producer's emitted artifact
genuinely changes: a calc formula fix, a rounding-convention change, a new/removed
output file, or a deliberate input-default change. It is NOT expected on:

- a new commit (the `git_sha` is volatile — pruned by NAME, never hashed),
- a release bump (`package_version` is volatile — pruned by NAME),
- a data refresh (`data_as_of` is volatile by default),
- a `reproducible` measurement flip (volatile by name).

Those four live in `GOLDEN_VOLATILE_KEYS` (a KEY-ALLOWLIST applied by dotted
key-name only — never a "looks like a date/path" value heuristic). If a golden
breaks for one of those reasons, the bug is in the harness wiring, not the golden —
fix the wiring, do NOT refresh the golden.

## How to refresh (regen)

```bash
REGEN_GOLDENS=1 uv run pytest tests/workflow_api/test_determinism_harness.py -q
```

`REGEN_GOLDENS=1` causes `golden_workflow_test` to **rewrite** the golden from the
current emission and then **`pytest.skip`** the test — it does NOT pass. A refreshed
golden therefore can never be laundered into a green run; the skip forces a human
to look at the diff.

## Re-sanction (owner sign-off) — REQUIRED before commit

A refreshed golden is an unverified baseline until re-sanctioned. Per the BSEE
golden-baseline lesson ("golden baseline needs RE-SANCTIONING after refresh"), a
golden change MUST NOT be committed without:

1. **A documented reason** for the `result_hash` change (which calc/formula/default
   moved, and why the new value is correct) in the PR description.
2. **A diff review** of the old vs new golden (`git diff` on the `*.json`), confirming
   only the intended fields moved.
3. **Owner sign-off** on the PR that carries the refreshed golden. The agent that
   regenerates the golden may NOT self-sanction it.

If you cannot explain why the hash changed, the change is a regression — investigate
the producer, do not refresh.

## `data_as_of` pinning (opt-in)

`provenance.data_as_of` is volatile by default so a data refresh does not break a
pure-calc golden. A workflow whose determinism legitimately depends on a frozen data
vintage may PIN it by passing `extra_volatile_keys` that EXCLUDES `data_as_of` and
asserting `pin_structural=True` against a golden captured at that vintage. Default
posture is record-don't-pin.

## Related

- Plan: `workspace-hub/docs/plans/2026-06-28-issue-3283-determinism-harness.md`
- Harness: `src/digitalmodel/workflow_api/golden.py`, `.../provenance.py`
- Upstream contract (#3282): `assetutilities.workflow_api.envelope`
  (`result_hash`, `make_provenance`, `code_version`)
- BSEE re-sanction lesson: `project_julia_field_economics_demo`,
  `project_bsee_ogor_refresh_mechanics`
