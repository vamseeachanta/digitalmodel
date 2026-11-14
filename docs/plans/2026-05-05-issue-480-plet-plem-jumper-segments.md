# Plan: digitalmodel #480 — Verify PLET-PLEM jumper segment lengths from approved jumper workbook

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/480
**Status:** plan-review
**Tier:** T1 (single-site data correction)
**Parent:** #471

## Context

The approved PLET-PLEM entry in `KNOWN_JUMPER_CONFIGS` in `src/digitalmodel/marine_ops/installation/jumper_lift.py` (lines 139-158) carries placeholder segment lengths copied wholesale from MF-PLET. The TODO at line 144 documents this: "Verify these from the approved jumper workbook." Identical seg_a-g (336/160/525/1046.3/370/160/352) produce a misleading 71.64 m total — the same value MF-PLET computes — so any analysis run on the PLET-PLEM config currently re-derives MF-PLET geometry and silently ships wrong results downstream (line sections, weights, COG, OrcaFlex sections).

The approved jumper workbook is held in the private project archive and accessed on the Windows workstation with Claude cowork. The GA sheet rows C5:C11 hold seg_a through seg_g in inches per the Excel-conversion convention used for the MF-PLET ingest.

The approved companion PLET-PLEM installation spec carries the same notice ("Segment lengths need verification") and currently mirrors MF-PLET; it must move in lockstep with the Python config. Public discovery: installation specs are under `docs/domains/orcaflex/subsea/jumper/installation/`; registry entries are defined in `src/digitalmodel/marine_ops/installation/jumper_lift.py:127-153`.

## Plan

1. **Extract on the Windows workstation.** The executor will open the approved jumper workbook on the Windows workstation with Claude cowork, read GA!C5:C11, and capture the seven segment values plus the Excel cell trace as comments. No code modifications happen on Linux until extracted values are pasted back into the issue thread for review.

2. **Patch the approved PLET-PLEM entry in `KNOWN_JUMPER_CONFIGS`.** Edit `src/digitalmodel/marine_ops/installation/jumper_lift.py` lines 146-152 to set seg_a_inch through seg_g_inch to the extracted values. Replace the TODO comment at lines 142-145 with a `Source: GA!Cxx -- description` cell-trace comment matching the MF-PLET convention (lines 121-128). Leave AHC-related fields (crane_sz/dz_swl_te, crane_sz/dz_radius_m) untouched — those are correct for PLET-PLEM.

3. **Patch `spec.yml`.** Edit the approved PLET-PLEM installation spec to remove the "NOTE: Segment lengths need verification" preamble and update any segment fields that mirror MF-PLET. Match the schema shape of the approved MF-PLET spec. Export `PLET_PLEM_SPEC` to the approved PLET-PLEM spec path. Smoke command: `uv run python -c "import os; from digitalmodel.infrastructure.spec_loader import load_spec; load_spec(os.environ['PLET_PLEM_SPEC'])"` — must parse without exception.

4. **Add divergence-asserting test.** New test in `tests/marine_ops/installation/test_jumper_lift.py` (or new file `test_plet_plem_geometry.py` if the existing file is treated as fixture-locked), with the approved MF-PLET and PLET-PLEM registry keys supplied as fixtures:
   ```python
   def test_plet_plem_total_length_differs_from_mf_plet(mf_plet_key, plet_plem_key):
       from digitalmodel.marine_ops.installation.jumper_lift import KNOWN_JUMPER_CONFIGS, run_jumper_analysis
       mf = run_jumper_analysis(KNOWN_JUMPER_CONFIGS[mf_plet_key])
       pp = run_jumper_analysis(KNOWN_JUMPER_CONFIGS[plet_plem_key])
       assert abs(mf["total_length_m"] - pp["total_length_m"]) > 0.5  # m
   ```
   This is a regression sentinel: the same defect (placeholder copy) cannot reland silently.

5. **Smoke check.** Run `uv run pytest tests/marine_ops/installation/test_jumper_lift.py -v -k plet_plem` and `uv run python -c "import os; from digitalmodel.marine_ops.installation.jumper_lift import KNOWN_JUMPER_CONFIGS, run_jumper_analysis; r=run_jumper_analysis(KNOWN_JUMPER_CONFIGS[os.environ['PLET_PLEM_KEY']]); print(r['total_length_m'])"` after exporting `PLET_PLEM_KEY` to the approved PLET-PLEM registry key. Confirm total length is non-71.64 m.

## Acceptance Criteria

- [ ] The approved PLET-PLEM entry in `KNOWN_JUMPER_CONFIGS` carries extracted seg_a-g values with `Source: GA!Cxx` cell-trace comments
- [ ] The approved PLET-PLEM installation spec reflects the same values, "needs verification" notice removed
- [ ] Divergence test passes: PLET-PLEM total length ≠ MF-PLET total length (Δ > 0.5 m)
- [ ] No changes to MF-PLET config, AHC fields, or pipe properties
- [ ] `uv run pytest tests/marine_ops/installation/test_jumper_lift.py` still green

## Open Questions

- Confirm with parent #471 owner whether seg_e/seg_f composition (e.g. "210+160" comment style) should be preserved if the PLET-PLEM workbook uses different sub-segment composition.
