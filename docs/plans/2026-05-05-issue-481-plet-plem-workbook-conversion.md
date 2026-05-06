# Plan: digitalmodel #481 — Convert PLET-PLEM workbook via Windows cowork

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/481
**Status:** plan-review
**Tier:** T3 (Excel→Python conversion across two repos)
**Machine:** ws014 (Windows, Claude cowork)

## Context

Convert `client_projects/engineering_workbooks/ballymore/jumper_manifold_to_plet/jumper_PLET to PLEM/SZ_Ballymore_Jumper_MF.xlsm` to a Python module + pytest suite using the established `excel-workbook-to-python-cowork` skill. Template precedent: MF-PLET converter at `src/digitalmodel/marine_ops/installation/jumper_lift.py` (1,200+ lines, 24 functions, 81 tests, a `KNOWN_JUMPER_CONFIGS` registry at line 117).

Current state of PLEM consumer: `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml` exists but warns "NOTE: Segment lengths need verification from SZ_Ballymore_Jumper_MF.xlsm workbook. Current values use MF-PLET baseline (same pipe, geometry TBD from workbook conversion)." This issue closes that gap. Cross-repo: workbook lives in `client_projects` (private), Python module lands in `digitalmodel`, segment lengths flow into `KNOWN_JUMPER_CONFIGS`.

## Plan

1. **Open workbook with Claude cowork on ws014.** Pre-flight: confirm ws014 is reachable, Excel is licensed, and the cowork harness is installed. Open `SZ_Ballymore_Jumper_MF.xlsm`. List sheets, formulas, cross-sheet refs. Document any external links or VBA macros (these block deterministic conversion — flag immediately).

2. **Run `excel-workbook-to-python-cowork` skill.** Follow the skill's standard sequence: extract all sheet data, transcribe formulas, identify input/output boundaries, generate Python module skeleton, add pytest suite. Target the same shape as `jumper_lift.py`: ≥20 functions and ≥80 pytest tests. New module lands as `client_projects/.../plet_plem_jumper_lift.py` (private) — **NOT** in digitalmodel; only the test file and `KNOWN_JUMPER_CONFIGS` entry land in this public repo.

3. **Diff against MF-PLET baseline.** Confirm: same pipe (10.75" OD, 1.79" WT, 50" bend) — assert in tests. Different segment lengths and jumper geometry — capture the new lengths. Same crane configs (SZ + DZ) — verify the crane parameter blocks. Verify buoyancy/strake layout differences vs MF-PLET (this is the geometry that justifies the new spec). Capture the diff in a markdown report under `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/conversion_diff.md`.

4. **Wire findings into digitalmodel.** Edit `src/digitalmodel/marine_ops/installation/jumper_lift.py`: add a `KNOWN_JUMPER_CONFIGS["ballymore_plet_plem"]` entry mirroring the workbook geometry. Update `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`: replace the "TBD from workbook conversion" segment lengths with the verified values, and remove the warning comment block. Add `tests/marine_ops/installation/test_plet_plem_jumper.py` mirroring the structure of `test_jumper_lift.py`.

5. **Smoke + close BUG and finalize.** Run on Linux: `uv run pytest tests/marine_ops/installation/test_plet_plem_jumper.py -xvs` and the broader `tests/marine_ops/installation/` suite. Commit the workbook→Python module to `client_projects` (separate repo), the spec/test/registry updates to `digitalmodel`. Reference both PRs in this issue. Close the segment-verification BUG issue (search "ballymore plet plem segment" in `gh issue list`).

## Acceptance Criteria

- [ ] Python module produced from workbook on ws014 with ≥20 functions and ≥80 tests, all passing on ws014
- [ ] `KNOWN_JUMPER_CONFIGS["ballymore_plet_plem"]` entry exists in `src/digitalmodel/marine_ops/installation/jumper_lift.py` with verified segment lengths
- [ ] `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml` no longer contains the "TBD from workbook conversion" warning; segment lengths match the registry entry
- [ ] `tests/marine_ops/installation/test_plet_plem_jumper.py` exists and passes on Linux via `uv run pytest`
- [ ] `conversion_diff.md` captures the geometry/buoyancy differences vs MF-PLET
- [ ] BUG issue for ballymore PLET-PLEM segment verification is closed and references this PR

## Open questions

1. Is ws014 currently online and licensed for Excel + Claude cowork? Confirm before dispatch.
2. Does the workbook contain VBA macros or external data links? If yes, document and decide whether to convert them or freeze cached values — block on user input.
3. The workbook lives in `client_projects` (private). Confirm the executor has push rights to that repo before starting; otherwise plan reduces to an extract-and-handoff.
