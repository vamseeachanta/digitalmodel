# Codex code review record — pressure preparation

Scope: [issue 2094](https://github.com/vamseeachanta/digitalmodel/issues/2094). Independent reviewer: Codex child session `solver_evidence`. This record preserves the reviewer's returned findings; no new review is implied.

## R1 — MAJOR

`verify_pressure_mesh` accepted altered frozen geometry. Moving every left-boundary node from x=0 to −10 mm, or top-boundary node from y=300 to 310 mm, still returned `verified_preparation`.

`assess_shape_output` accepted an earlier default-limit statement followed by a modified-limit statement and returned `shape_gate_passed:true`.

Both defects were reproduced offline while all 71 existing focused tests passed.

## R2 — MAJOR

The domain defect was closed by the separate complete-domain verifier and mutation regressions. The status correction introduced a native-format regression: legitimate `ALREADY USING DEFAULT LIMITS` and `ALREADY ON` initialization messages were rejected despite the later definitive `ON WITH DEFAULT LIMITS`.

The actual corrected coarse CDB passed parsing, domain verification and pressure integration, but the composed audit failed at the shape gate. All 114 focused tests passed, exposing the missing native-status fixture.

The parent subsequently applied an inline correction and verified the composed native audit. That correction was not independently reviewed in a third round. Neither historical MAJOR verdict is relabelled as independent approval. See the [inline disposition](2026-09-12-code-2094-pressure-disposition.md).
