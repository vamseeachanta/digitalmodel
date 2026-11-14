# Plan: digitalmodel #481 — Convert PLET-PLEM workbook via Windows cowork

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/481
**Status:** plan-review
**Tier:** T3 (Excel→Python conversion across two repos)
**Machine:** Windows workstation (Windows, Claude cowork)

## Context

Convert the approved jumper workbook from the private project archive to a Python module + pytest suite using the established `excel-workbook-to-python-cowork` skill. Template precedent: MF-PLET converter at `src/digitalmodel/marine_ops/installation/jumper_lift.py` (1,200+ lines, 24 functions, 81 tests, a `KNOWN_JUMPER_CONFIGS` registry at line 117).

Current state of PLEM consumer: the approved PLET-PLEM installation spec exists but warns "NOTE: Segment lengths need verification from approved jumper workbook. Current values use MF-PLET baseline (same pipe, geometry TBD from workbook conversion)." This issue closes that gap. Cross-repo: workbook lives in private project archive (private), Python module lands in `digitalmodel`, segment lengths flow into `KNOWN_JUMPER_CONFIGS`. Public discovery: installation specs are under `docs/domains/orcaflex/subsea/jumper/installation/`; registry entries are defined in `src/digitalmodel/marine_ops/installation/jumper_lift.py:127-153`.

## Plan

1. **Open workbook with Claude cowork on the Windows workstation.** Pre-flight: confirm the Windows workstation is reachable, Excel is licensed, and the cowork harness is installed. Open the approved jumper workbook. List sheets, formulas, cross-sheet refs. Document any external links or VBA macros (these block deterministic conversion — flag immediately).

2. **Run `excel-workbook-to-python-cowork` skill.** Follow the skill's standard sequence: extract all sheet data, transcribe formulas, identify input/output boundaries, generate Python module skeleton, add pytest suite. Target the same shape as `jumper_lift.py`: ≥20 functions and ≥80 pytest tests. New module lands as `plet_plem_jumper_lift.py` in the private project archive — **NOT** in digitalmodel; only the test file and `KNOWN_JUMPER_CONFIGS` entry land in this public repo.

3. **Diff against MF-PLET baseline.** Confirm: same pipe (10.75" OD, 1.79" WT, 50" bend) — assert in tests. Different segment lengths and jumper geometry — capture the new lengths. Same crane configs (SZ + DZ) — verify the crane parameter blocks. Verify buoyancy/strake layout differences vs MF-PLET (this is the geometry that justifies the new spec). Capture the diff in a markdown report beside the approved PLET-PLEM installation spec as `conversion_diff.md`.

4. **Wire findings into digitalmodel.** Edit `src/digitalmodel/marine_ops/installation/jumper_lift.py`: add the approved PLET-PLEM entry in `KNOWN_JUMPER_CONFIGS` mirroring the workbook geometry. Update the approved PLET-PLEM installation spec: replace the "TBD from workbook conversion" segment lengths with the verified values, and remove the warning comment block. Add `tests/marine_ops/installation/test_plet_plem_jumper.py` mirroring the structure of `test_jumper_lift.py`.

5. **Smoke + close BUG and finalize.** Run on Linux: `uv run pytest tests/marine_ops/installation/test_plet_plem_jumper.py -xvs` and the broader `tests/marine_ops/installation/` suite. Commit the workbook→Python module to the private project archive (separate repo), the spec/test/registry updates to `digitalmodel`. Reference both PRs in this issue. Close the segment-verification BUG issue (search "plet plem segment" in `gh issue list`).

## Acceptance Criteria

- [ ] Python module produced from workbook on the Windows workstation with ≥20 functions and ≥80 tests, all passing on the Windows workstation
- [ ] The approved PLET-PLEM entry in `KNOWN_JUMPER_CONFIGS` exists in `src/digitalmodel/marine_ops/installation/jumper_lift.py` with verified segment lengths
- [ ] The approved PLET-PLEM installation spec no longer contains the "TBD from workbook conversion" warning; segment lengths match the registry entry
- [ ] `tests/marine_ops/installation/test_plet_plem_jumper.py` exists and passes on Linux via `uv run pytest`
- [ ] `conversion_diff.md` captures the geometry/buoyancy differences vs MF-PLET
- [ ] BUG issue for the approved PLET-PLEM jumper segment verification is closed and references this PR

## Open questions

1. Is the Windows workstation currently online and licensed for Excel + Claude cowork? Confirm before dispatch.
2. Does the workbook contain VBA macros or external data links? If yes, document and decide whether to convert them or freeze cached values — block on user input.
3. The workbook lives in private project archive (private). Confirm the executor has push rights to that repo before starting; otherwise plan reduces to an extract-and-handoff.
