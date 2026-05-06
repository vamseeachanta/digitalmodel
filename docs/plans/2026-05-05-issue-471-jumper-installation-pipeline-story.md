# Plan: digitalmodel #471 — STORY: Jumper Installation Analysis Pipeline (spec.yml → OrcaFlex)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/471
**Status:** plan-review
**Tier:** T3 (story / coordination)
**Children:** #475, #472, #470 (related), #479, #478, #480

## Context

#471 is the parent story coordinating the eight-step pipeline `spec.yml → JumperConfig → run_jumper_analysis → generate_orcaflex_line_sections_yaml → model.yml → OrcaFlex → post-processing → Go/No-Go report`. Code already in place in `src/digitalmodel/marine_ops/installation/`:

| File | Lines | Status |
|------|------:|--------|
| `jumper_lift.py` | 1200 | ✅ landed; 24 funcs; both KNOWN_JUMPER_CONFIGS keys present |
| `jumper_installation.py` | 346 | ✅ pipeline scaffolded (`PipelineConfig`, `run_pipeline`, `PipelineOutput`) |
| `go_no_go.py` | 360 | ✅ landed; 6 criteria covered (#472 adds splash-zone Hs) |
| `splash_zone.py` | — | ✅ landed; integration pending in #472 |
| `crane_tip_motion.py` | — | ✅ landed |
| `solvers/orcaflex/reporting/renderers/jumper.py` | 20 | stub; #479 builds out |
| `solvers/orcaflex/modular_generator/` | — | exists; #478 wires to jumper |
| `tests/marine_ops/installation/test_jumper_lift.py` | 599 | 81 tests claimed; #475 finalizes |

The two `KNOWN_JUMPER_CONFIGS` entries: `ballymore_mf_plet` (correct), `ballymore_plet_plem` (placeholder geometry mirrors MF-PLET — #480 fixes). Specs at `docs/domains/orcaflex/subsea/jumper/installation/ballymore_{mf_plet,plet_plem}/spec.yml`.

This issue itself does **not** ship code — it's the coordination tracker. The plan here is the close-out-checklist for the parent.

## Plan

1. **Track child issues to resolution.** No code edits in #471. Confirm each child plan lands `status:plan-review` then `status:plan-approved`, then merges:
   - #480 — verify PLET-PLEM segments (T1, blocker for #475/#479 fidelity)
   - #475 — pytest suite finalize (T2)
   - #472 — Go/No-Go splash-zone criterion + aggregator audit (T2)
   - #478 — modular generator integration (T3)
   - #479 — HTML/PDF report renderer (T3)

2. **Add to `marine_ops/installation/__init__.py` exports.** Once the children stabilize the public API, append `jumper_lift`, `jumper_installation`, `go_no_go`, and (after #479) the report-rendering helper to the `__all__` block. The `__init__.py` already exports `crane_tip_motion`, `splash_zone`, `operability` family — extend that list.

3. **Author end-to-end smoke script.** New file `examples/marine_ops/jumper_installation_pipeline.py`:
   - Loads both jumper specs
   - Calls `run_pipeline(...)` for each
   - Asserts `GoNoGoDecision.overall_state` returned
   - Writes a `pipeline_smoke_report.html` index linking the two per-jumper reports
   This is the user-facing demonstration the GTM team will reference.

4. **Update parent issue body.** After all children close, edit issue #471's TODO block to check off completed items and link to the merged PRs. (No file edit — `gh issue edit`.)

5. **Smoke check (post-children-merged).** `uv run python examples/marine_ops/jumper_installation_pipeline.py` — both jumpers produce report HTML, both produce model.yml, total runtime < 60 s on Linux.

## Acceptance Criteria

- [ ] Children #480, #475, #472, #478, #479 all closed and their PRs merged
- [ ] `marine_ops/installation/__init__.py` exports `jumper_lift`, `jumper_installation`, `go_no_go` (and report helper from #479)
- [ ] `examples/marine_ops/jumper_installation_pipeline.py` runs end-to-end against both spec.yml inputs
- [ ] Parent issue TODO block in #471 reflects child closures
- [ ] No regression in `tests/marine_ops/installation/`

## Open Questions

- Sequencing for batch agents: #480 must land before #475's "PLET-PLEM differs" assertion is meaningful; otherwise #475's matrix tests on PLET-PLEM are testing-the-bug. Recommend the executor merge children in order #480 → #472 → #475 → #478 → #479.
- The reporting renderer (#479) and modular generator (#478) are independent and can run in parallel once #480 lands.
