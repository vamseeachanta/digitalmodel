# Review packet: 2617 revised plan and final code review

Generated 2026-10-16T15:41:24+00:00 · git HEAD `c5e4ac3390942e326243093a9a6175716f0e7459`
Digest domain: raw bytes as stored on disk (line endings included).
Line numbers are prefixed; cite findings as `path:line`. The packet is the complete review target.

## docs/plans/issue-2617-qtf-variance.html

sha256 `212b79019cd516d7b3d3790694c1f46260f231fddb84ad83417aa97c7a6d2b20` · 7448 bytes · 73 lines

```
   1 | <!doctype html>
   2 | <html lang="en"><meta charset="utf-8"><title>Issue 2617: QTF variance plan</title>
   3 | <style>body{font:16px/1.5 sans-serif;max-width:960px;margin:40px auto;padding:0 20px}code{background:#eee}td,th{border:1px solid #bbb;padding:8px}table{border-collapse:collapse}</style>
   4 | <h1>Issue 2617: difference-frequency force variance</h1>
   5 | <p>The change will address <a href="https://github.com/vamseeachanta/digitalmodel/issues/2617">issue 2617</a>
   6 | on a fresh worktree from <code>c5e4ac33</code>. Complexity will be T2; execution will use one writer and parallel read-only plan/code reviews.
   7 | The user request will authorize implementation, one public draft PR and its issue summary; merge and deployment will remain outside scope.</p>
   8 | <h2>Resource intelligence and reproduction</h2>
   9 | <p>The implementation will be checked against <code>src/digitalmodel/orcawave/drift_forces.py</code>,
  10 | the existing tests in <code>tests/orcawave/test_drift_forces.py</code>, the issue criteria, and
  11 | <a href="https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Wavedriftandsumfrequencyloads.htm">Orcina's second-order load theory</a>.
  12 | The derivation will distinguish conjugate-paired terms and the constant diagonal from fluctuation variance.</p>
  13 | <p>The pre-change runtime evidence will be retained in the verification report:</p>
  14 | <pre>seed=2617; narrow-band JONSWAP Hs=2 m, Tp=2*pi/0.8 s, gamma=10
  15 | omega=linspace(0.2,1.224,2049) rad/s; Q=10 kN/m^2
  16 | 2048 random-phase records; 8192 samples/record; 12566.370614 s/record
  17 | repository full-QTF variance = 50.005839732175204 kN^2
  18 | Monte Carlo force variance   = 24.926308458002424 kN^2
  19 | repository / Monte Carlo    = 2.0061470320175374
  20 | 95% CI relative half-width  = 0.0029835655035290666</pre>
  21 | <p>Registry discovery will remain bounded to the hub's standards-transfer ledger, code registry and online-resource registry.
  22 | No standards-derived coefficients will be introduced. The available online registry will supply context for Orcina L07;
  23 | no licensed/client files will be consumed. The absent sibling wiki catalogs and claim helper will be recorded as coverage gaps;
  24 | the isolated worktree will avoid shared implementation paths.</p>
  25 | <h2>Proposed calculation and scope</h2>
  26 | <p>For fixed-amplitude random-phase waves, <code>a_i=sqrt(2 S_i dw)</code> and a Hermitian difference-frequency QTF,
  27 | the two ordered terms will combine into a real sinusoid of amplitude <code>2 a_i a_j |Q_ij|</code>.
  28 | Its phase-ensemble variance will be <code>2 a_i^2 a_j^2 |Q_ij|^2 = 8 S_i S_j dw^2 |Q_ij|^2</code>.
  29 | Summation will therefore visit only <code>i&lt;j</code>. The diagonal will contribute mean load, not slowly-varying variance.</p>
  30 | <p><code>full_qtf_slowly_varying</code> will use the unique-pair sum, retain its scalar standard-deviation API and document the derivation.
  31 | <code>newman_approximation</code> will retain its array API and positive-lag values, with its zero-lag bin set to zero to exclude mean load.
  32 | Its documentation will specify <code>mu=k*dw</code> and discrete variance <code>sum(S_sv)*dw</code>, including full endpoint-bin weights.
  33 | This discrete interpretation will be explicit; the positive-frequency continuum limit will remain the usual factor-eight convolution.</p>
  34 | <p>Uniform increasing frequency grids and Hermitian full QTF matrices will remain the stated supported physical case.
  35 | Nonuniform quadrature, directional seas, QTF interpolation and nonlinear/mooring qualification will remain outside this correction.
  36 | Input validation will not be expanded; unsupported non-Hermitian/triangular-only matrices and nonuniform integration grids will be identified as limitations.
  37 | A single component will return zero fluctuation. The existing positive-std fixture will use a complete QTF on a uniform grid.</p>
  38 | <h2>TDD and acceptance</h2>
  39 | <ol>
  40 | <li>A reproducible synthetic reference harness will call the actual public APIs before any production edit.
  41 | The recorded baseline will precede implementation; random-phase force histories will be formed independently from wave amplitudes.</li>
  42 | <li>Failing tests will cover a bichromatic known variance (including a complex Hermitian QTF), a single component/diagonal-only zero fluctuation,
  43 | and Newman/full variance agreement within 1%. These will fail against the old implementation.</li>
  44 | <li>A narrow-band JONSWAP ensemble will use fixed seed 2617 and independent records. The 95% confidence half-width of the mean
  45 | time-history variance will be below 1%; full-QTF and Monte Carlo variances will agree within 1%.</li>
  46 | <li>A linear surge oscillator will use synthetic mass, stiffness and damping. A time-domain ODE response will be compared to the repository full-QTF
  47 | standard deviation using a QTF weighted by the oscillator transfer magnitude at each difference frequency.
  48 | The case will use a bichromatic force with mass 10000 kN*s^2/m, damping 200 kN*s/m and stiffness 100 kN/m
  49 | (natural frequency 0.1 rad/s, damping ratio 0.1). Four random-phase records will use 256 samples per beat cycle,
  50 | discard 64 cycles and measure 128 complete cycles. Standard deviation will remove the static offset.
  51 | Whole-cycle averaging will eliminate phase sampling error in this one-beat case, and standard deviations will agree within 5%.</li>
  52 | <li>The existing drift, spectrum, motion-statistics and relevant package tests will be run. Every tracked reference/import/call to both functions
  53 | will be inspected, and the PR will list every full-QTF caller plus the package re-export and new tests.</li>
  54 | <li>Adversarial plan and code reviews will use frozen SHA-256 packets with verification before verdict acceptance.
  55 | Any unavailable provider will be recorded without treating it as approval. No-MAJOR findings will precede publication.</li>
  56 | </ol>
  57 | <h2>Artifacts and retention</h2>
  58 | <table><tr><th>Purpose</th><th>Proposed artifact</th></tr>
  59 | <tr><td>Implementation</td><td>src/digitalmodel/orcawave/drift_forces.py</td></tr>
  60 | <tr><td>Regression/reference harness</td><td>tests/orcawave/test_drift_force_variance.py</td></tr>
  61 | <tr><td>Calculation evidence</td><td>docs/verification/issue-2617-qtf-variance.html</td></tr>
  62 | <tr><td>Frozen review evidence</td><td>scripts/review/results/2617/</td></tr></table>
  63 | <p>Only synthetic inputs, compact metrics and reproducible generation code will be retained in Git. Full time arrays and QTF matrices will be generated
  64 | in memory and discarded. Input units and criteria will be pinned in the harness/report; no external dataset ownership will be implied.</p>
  65 | <p>Newman/full equality on identical grids will be checked at relative tolerance 1e-12. A mean-force test will bind the QTF convention
  66 | to <code>compute_mean_drift_force</code>. Exact bichromatic and diagonal tests will guard mean-load exclusion independently of the fine-grid ensemble tolerance.
  67 | NumPy 1.26.4 will match the repository's declared NumPy &lt;2 dependency.</p>
  68 | <h2>Review, publication and closeout</h2>
  69 | <p>Claude and Gemini/Agy reviews will be requested alongside the main-session Codex adversarial check at both stages.
  70 | Changed reviewed bytes will invalidate the corresponding verdict. Review transcripts will record actual provider availability.
  71 | The public outgoing bundle will be checked for secrets, client data and source rights. The cleanup audit will classify retained worktree/evidence as expected.
  72 | The verified changes will be committed and pushed without force or hook bypass; one draft PR will reference the issue and carry the result and caller audit.</p>
  73 | </html>
```

## docs/plans/README.md

sha256 `6fcf939bb576c11b88e83aff601681a3698828ad9287a56ca99198194e645b9f` · 15663 bytes · 40 lines

```
   1 | # Issue Plans
   2 | 
   3 | This is the active issue-plan index for `digitalmodel`. Historical plans in
   4 | this directory predate this index and are not backfilled here.
   5 | 
   6 | | Issue # | Title / Slug | Plan File | Date | Status | Complexity | Notes |
   7 | |---|---|---|---|---|---|---|
   8 | | [#2617](https://github.com/vamseeachanta/digitalmodel/issues/2617) | QTF force variance | [plan](issue-2617-qtf-variance.html) | 2026-10-16 | draft; reviews recorded in scripts/review/results/2617 | T2 | Synthetic Monte Carlo reproduction before implementation. |
   9 | | [#2241 item 2](https://github.com/vamseeachanta/digitalmodel/issues/2241) | Boundary-preserving BRep study | [proposal](2026-10-02-issue-2241-brep-boundary-study.html) | 2026-10-02 | revised plan; cross-provider review pending | T2 | Codex approved for consideration; Claude/Gemini unavailable. No implementation authorized. |
  10 | | [#2241 item 2](https://github.com/vamseeachanta/digitalmodel/issues/2241) | Opt-in BRep validity investigation | [plan](2026-10-01-issue-2241-brep-validity.html) | 2026-10-01 | bounded experiment executed; candidate rejected | T2 | Owner continuation authorized the published r3 scope. Geometry counterexamples prevent qualification; item 2 remains open. |
  11 | | [#2281](https://github.com/vamseeachanta/digitalmodel/issues/2281) | CP benchmark evidence research | [research plan](2026-10-06-issue-2281-cp-benchmark-evidence-research.html) | 2026-10-06 | reviewed research draft | T2 | Shared generic methods and private A-E ledgers; no new calculations; 11/16 incomplete. |
  12 | | [#2281](https://github.com/vamseeachanta/digitalmodel/issues/2281) | CP structure and benchmark review reports | [plan](2026-10-03-issue-2281-cp-review-report-portfolio.md) | 2026-10-03 | implementation authorized; partial draft | T2 | Eleven structure/mode packs available; A–E remain incomplete; comprehensive owner visual review deferred; Claude/Gemini unavailable, documented Codex fallback. |
  13 | | [#2261](https://github.com/vamseeachanta/digitalmodel/issues/2261) | Multi-component riser CP | [plan](2026-09-29-issue-2261-multi-component-risers.html) | 2026-09-29 | adversarial-reviewed r3 | T3 | r2 Agy APPROVE / Codex MAJOR; r3 inline patches resolve zero/coverage, life-filtered paths, reciprocal allocations, kernel placement, field-joint schema, exact reconciliation, deterministic governing, and partial private totals. |
  14 | | [#2263](https://github.com/vamseeachanta/digitalmodel/issues/2263) | Riser-base compositions and phased CP assessments | [plan](2026-09-29-issue-2263-riser-base-phases.html) | 2026-09-29 | local adversarial re-review | T3 | User-authorized branch implementation; no external mutation; private rank-6/7 evidence remains comparison-only. |
  15 | | [#2260](https://github.com/vamseeachanta/digitalmodel/issues/2260) | pipeline-anode-bank | `docs/plans/2026-09-29-issue-2260-pipeline-anode-bank.md` | 2026-09-29 | implemented | T3 | Remote terminal-bank route using F103 Sec. 6.7 Eq. (15)/(16)/(18)/(20), not the issue's incorrect Appendix B locator; plan r2 and code-stage standards/API reviews APPROVE; private regression remains outside Git. |
  16 | | [#2264](https://github.com/vamseeachanta/digitalmodel/issues/2264) | CP evidence application | [plan](2026-09-29-issue-2264-evidence-application.html) | 2026-09-29 | application implemented; qualification deferred | T2 | User-directed scope; experimental gates preserved; evidence classes, EN band provenance and graphite measured mass. |
  17 | | [#2214](https://github.com/vamseeachanta/digitalmodel/issues/2214) | cp-docs-examples-index-brochure | `docs/plans/2026-09-25-issue-2214-cp-docs-examples-index-brochure.md` | 2026-09-25 | implemented | T2 | Epic #2206, owner decisions D11/D12. Worked-example router keys mapped to the four accepted keys, duplicates merged, B401 offshore example added, `_index.md` rewritten, brochure removed, `test_worked_examples.py` executes every example block. |
  18 | | [#1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) | ffs-offering-program | `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md` | 2026-09-25 | plan-approved (r1.4) | T4 | Program plan for the FFS offering (Phase 4 Level 3 / Part 9 tracks, readiness demonstrators, new asset classes #2197-#2203, #2205); offering catalog data + lookup tables + catalog test shipped with the plan. Claude r1 MAJOR, Codex r1 MAJOR, Codex r1.1 MAJOR applied (r1.2); Approved by owner 2026-09-26 (approval gate G1-G6); two-provider review exception accepted; Wave 0 = #1094, #2160, #2171, #2180, #2197, #2205 via plan-lites. |
  19 | | [#2121](https://github.com/vamseeachanta/digitalmodel/issues/2121) | N8 predecessor replay prerequisite | [HTML plan](issue-2121-n8-predecessor-replay.html) | 2026-09-16 | exact implementation review | T2 | Read-only pinned N4 replay; native ordinal-3 admission remains separate. Standing analysis lifecycle authority applies. |
  20 | | [#2121](https://github.com/vamseeachanta/digitalmodel/issues/2121) | ANSYS open-cylinder verification campaign | [HTML plan](2026-09-13-issue-2121-ansys-cylinder-campaign.html) | 2026-09-13 | historical plan; superseded by native/recovery checkpoints | T2 | One physical point at three meshes plus zero control; standing lifecycle authority recorded in issue checkpoints. Campaign incomplete; current prerequisite is linked above. |
  21 | | [#2119](https://github.com/vamseeachanta/digitalmodel/issues/2119) | ANSYS analysis lifecycle | [HTML plan](2026-09-13-issue-2119-ansys-analysis-lifecycle.html) | 2026-09-13 | Stage A implemented; subsequent campaign separately gated | T2 | Offline evidence adapter published at `97a152c3`; 175 tests pass, two native tests skipped. Component-specific campaign: issue 2121. |
  22 | | [#2117](https://github.com/vamseeachanta/digitalmodel/issues/2117) | welded-padeye parametric go-by | [HTML plan](2026-09-13-issue-2117-padeye-parametric.html) | 2026-09-13 | plan-review; exact-current reciprocal review and implementation approval pending | T2 | Proposed 225 historical analytical cases; exact-response lookup, explicit source/geometry gates, zero native invocations. |
  23 | | [#2094](https://github.com/vamseeachanta/digitalmodel/issues/2094) | padeye numerical-response proposal | `docs/plans/2026-09-13-issue-2094-padeye-response-proposal.html` | 2026-09-13 | adversarial-reviewed; new-scope approval pending | T2 | Compliance and recovered section-equilibrium diagnostic; at most two preparations and three response solves after explicit approval. Local-stress sizing remains unresolved. |
  24 | | [#2094](https://github.com/vamseeachanta/digitalmodel/issues/2094) | padeye prescribed-pressure addendum | `docs/plans/2026-09-12-issue-2094-padeye-load-model-addendum.html` | 2026-09-12 | user-approved preparation implemented; stress gate pending | T2 | [Approval record](https://github.com/vamseeachanta/digitalmodel/issues/2094#issuecomment-5650114708). Corrected coarse native preparation verified; fine levels offline only. [Technical supplement](../reports/2026-09-12-padeye-pressure-preparation.html). |
  25 | | [#2089](https://github.com/vamseeachanta/digitalmodel/issues/2089) | Windows sentinel containment addendum | docs/plans/2026-09-11-issue-2089-windows-containment.html | 2026-09-11 | plan-review | T2 | Actual Claude/Codex MAJOR drafts and inline corrections retained; revised addendum approval pending; no GHS launch. |
  26 | | [#2089](https://github.com/vamseeachanta/digitalmodel/issues/2089) | neutral GHS hydrostatics canary | `docs/plans/2026-09-11-issue-2089-ghs-canary.html` | 2026-09-11 | M1 implemented | T2 | User approval is recorded in issue evidence; 86 tests pass on Linux and Windows. Licensed capture and native-format qualification remain separately gated. |
  27 | | [#2082](https://github.com/vamseeachanta/digitalmodel/issues/2082) | orcaflex-smoke-proof | `docs/plans/2026-09-10-issue-2082-orcaflex-smoke.html` | 2026-09-10 | implementation | T2 | Owner continuation recorded; finite/completed results, explicit one-thread models and saved-simulation readback; no deployment changes. |
  28 | | [#2051](https://github.com/vamseeachanta/digitalmodel/issues/2051) / [#1564](https://github.com/vamseeachanta/digitalmodel/issues/1564) | batch-reliability | `docs/plans/2026-09-09-issue-2051-batch-reliability.md` | 2026-09-09 | plan review | T2 | First execution slice: native YAML defaults, actual engine configuration and central failure verdict. |
  29 | | [#1977](https://github.com/vamseeachanta/digitalmodel/issues/1977) | norecursedirs-bare-basenames | `docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html` | 2026-08-05 | plan-approved | T2 | Owner approved plan `efc491c4` on 2026-08-06; merged `cac95abd`, issue closed. Severity corrected: `norecursedirs` never filters a directory passed as an explicit argument and every CI workflow passes explicit paths, so the 218 hidden tests had been running in CI all along — the real defect was that the documented developer command covered 218 fewer tests than CI did. The issue's suggested fix is not implementable: no portable pattern anchors to the repo root (32 checked), and `./scripts` reads as anchored while matching nothing at all. Fixed with root-anchored `collect_ignore` in a new root conftest, plus `conftest.py` added to `FULL_MATRIX_PATHS` so the new surface stays CI-sensitive. Both review rounds independently found the plan's own guard test RED-after-fix and vacuously satisfiable. Node-ID diff 0 removed / 218 added, all passing. |
  30 | | [#1970](https://github.com/vamseeachanta/digitalmodel/issues/1970) | artifact-generation-commit | `docs/plans/2026-08-05-issue-1970-artifact-generation-commit.md` | 2026-08-05 | plan-review | T3 | Wider than filed: the whole `artifact_index` layer has zero production consumers, not just the two domain constants. Option A wires the layer, option B removes it; option C (constants only) named as the trap. r1 Claude inline; owner approval pending. |
  31 | | [#1969](https://github.com/vamseeachanta/digitalmodel/issues/1969) | prebuilt-case-schema-arm | `docs/plans/2026-08-05-issue-1969-prebuilt-case-schema-arm.md` | 2026-08-05 | plan-approved | T2 | Owner approved option B on 2026-08-05; implemented, issue closed. Premise refuted, and the issue was filed backwards: prebuilt staging is fully reachable via `OpenFOAMRunner` and covered by 14 tests — only the YAML schema refuses it, with a message that is false as written. |
  32 | | [#1633](https://github.com/vamseeachanta/digitalmodel/issues/1633) | ship-benchmark-verdict | `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` | 2026-08-03 | plan-approved | T3 | Four adversarial rounds, three MAJOR verdicts, before closeout on the licensed Windows host: 20,465 passing tests, zero attributable regressions. The fitted-constant objection recurred three times in different forms — threshold derived from the data it judges, inputs derived from the threshold, and data derived from the threshold. Zero-variance is checked before Pearson r, and an absent diagonal is refused rather than scored. Review artifacts under `docs/plans/evidence/`. T3 code-stage review still owed. |
  33 | | [#1961](https://github.com/vamseeachanta/digitalmodel/issues/1961) | protected-identifier-scanner | `docs/plans/2026-08-04-issue-1961-protected-identifier-scanner.md` | 2026-08-04 | plan-approved | T3 | Owner approved plan `69ab90fd` on 2026-08-05, Stage 1 only. Stage 2 stays blocked on workspace-hub #3522 Phase B, confirmed unprovisioned by live readback. Every premise re-measured and reproduced to the digit: the unbounded matcher finds 30 genuine occurrences across 10 files at the pinned pre-fix tree where word bounding finds 17, and the two-name class collides with an OS release codename. Implemented with TDD (50 RED committed before GREEN). Three verifications do not route through pytest. |
  34 | | [#1631](https://github.com/vamseeachanta/digitalmodel/issues/1631) | licensed-lane-fail-closed | `docs/plans/2026-08-05-issue-1631-licensed-lane-fail-closed.md` | 2026-08-05 | plan-approved | T3 | Owner approved plan `6a670b71` on 2026-08-05. Premise verification refuted the issue's causal story: none of the 12 drained queue runs went through any OrcaFlex mock path, and the one OrcaFlex workflow present (`orcaflex-strength-post`) routes through none of the three modules the issue names — working the issue as written would have shipped green and turned zero of the twelve red. Root cause: `__main__.main()` discarded `engine()`'s return and never called `sys.exit`, so the engine contract had no failure exit path on any host. Implemented with TDD (36 RED committed before GREEN). Audit of all 12 results at `docs/plans/2026-08-05-issue-1631-licensed-lane-result-audit.html` found THREE runs with no `returned_files`, not one. |
  35 | | [#1968](https://github.com/vamseeachanta/digitalmodel/issues/1968) | mpi-resume-decomposition-validation | `docs/plans/2026-08-05-issue-1968-mpi-resume-decomposition-validation.md` | 2026-08-05 | plan-approved | T2 | Owner approved D2 option 1 (validated resume) on 2026-08-05. The filed question — which of two artifacts to amend — was the wrong question: both were wrong. The #1576 plan's blanket-reject rests on a refuted premise (no serial or pool resume exists to inherit the capability), and the merged green test asserted a 4-rank resume from a 1-rank decomposition. Implemented with TDD: a four-check decomposition gate that refuses before `system/controlDict` is touched. |
  36 | | [#1965](https://github.com/vamseeachanta/digitalmodel/issues/1965) | published-provenance-paths | `docs/plans/2026-08-04-issue-1965-published-provenance-paths.md` | 2026-08-04 | plan-review | T2 | Premise verification refuted the generated-page remedy (the page is a PAGE_EXCLUSION the drift gate never regenerates) and the issue's own claim that no in-repo producer exists. Leak is 3.25x larger than filed: a second published page, covered by no gate, carries the same 8 occurrences. The intended legal-scan verification is fail-open per workspace-hub #3804 and was observed PASSing over the live leak, so a repo-local detector replaces it. r1 Claude MAJOR (6 findings) folded in. Owner approval pending. |
  37 | | [#1959](https://github.com/vamseeachanta/digitalmodel/issues/1959) | interfoam-runnable-case | `docs/plans/2026-08-04-issue-1959-interfoam-runnable-case.md` | 2026-08-04 | plan-approved | T2 | Owner approved plan `9b781052` on 2026-08-04. Implemented with TDD; verified by a real interFoam start on the CFD node (10 advancing timesteps, sha256 manifest proving the tree was unpatched). Solver start found 3 defects beyond the 3 identified by inspection. |
  38 | | [#1898](https://github.com/vamseeachanta/digitalmodel/issues/1898) | equipment-catalog-reference-data | `docs/plans/2026-07-29-issue-1898-equipment-catalog-reference-data.md` | 2026-07-29 | adversarial-reviewed | T3 | Catalog preflight complete; tubing source unavailable; Codex lanes APPROVE, Claude/Agy UNAVAILABLE; user approval pending. |
  39 | | [#1602](https://github.com/vamseeachanta/digitalmodel/issues/1602) | solver-neutral-riser-host-diffraction-orcaflex-hf-program | `docs/plans/2026-07-18-issue-1602-riser-host-diffraction-plan.html` | 2026-07-18 | plan-approved | T3 | User approved the reviewed parent plan on 2026-07-18. Parent-scope implementation may proceed with TDD; every child retains its independent plan/review/approval gate. |
  40 | | [#1470](https://github.com/vamseeachanta/digitalmodel/issues/1470) | registry-batch-coverage-wave-6 | `docs/plans/2026-07-08-issue-1470-registry-batch-coverage.md` | 2026-07-08 | plan-approved | T3 | Provider CLIs unavailable; two subagent reviews returned MAJOR, plan was revised, focused re-reviews returned APPROVE. User approved; implementation authorized with TDD. |
```

## src/digitalmodel/orcawave/drift_forces.py

sha256 `d30b17b9d7071d252dd037de1a4d9b412736025ca190a68d130c7f81328858c6` · 11354 bytes · 309 lines

```
   1 | """
   2 | ABOUTME: Mean drift and slowly-varying drift force calculations for OrcaWave.
   3 | Implements Newman's approximation for QTF, full QTF double-sum integration,
   4 | mean drift force coefficient handling, and wind/current drift estimation.
   5 | """
   6 | 
   7 | from typing import Dict, List, Optional, Tuple
   8 | 
   9 | import numpy as np
  10 | from pydantic import BaseModel, Field
  11 | 
  12 | 
  13 | # ---------------------------------------------------------------------------
  14 | # Pydantic models
  15 | # ---------------------------------------------------------------------------
  16 | 
  17 | class MeanDriftCoefficients(BaseModel):
  18 |     """Frequency-dependent mean drift force coefficients for one heading."""
  19 | 
  20 |     heading_deg: float = Field(..., description="Wave heading in degrees")
  21 |     frequencies_rad_s: List[float] = Field(..., description="Frequency vector")
  22 |     surge_kn_m2: List[float] = Field(
  23 |         ...,
  24 |         description="Mean drift force coefficient in surge (kN/m^2)",
  25 |     )
  26 |     sway_kn_m2: List[float] = Field(
  27 |         ...,
  28 |         description="Mean drift force coefficient in sway (kN/m^2)",
  29 |     )
  30 |     yaw_knm_m2: List[float] = Field(
  31 |         ...,
  32 |         description="Mean drift moment coefficient in yaw (kN·m/m^2)",
  33 |     )
  34 | 
  35 | 
  36 | class QTFMatrix(BaseModel):
  37 |     """Quadratic Transfer Function matrix for one DOF and heading."""
  38 | 
  39 |     dof: str = Field(..., description="Degree of freedom (surge, sway, yaw)")
  40 |     heading_deg: float
  41 |     frequencies_rad_s: List[float] = Field(
  42 |         ...,
  43 |         description="Frequency vector (same for both axes)",
  44 |     )
  45 |     real_part: List[List[float]] = Field(
  46 |         ...,
  47 |         description="Real part of QTF matrix [n_freq x n_freq]",
  48 |     )
  49 |     imag_part: List[List[float]] = Field(
  50 |         ...,
  51 |         description="Imaginary part of QTF matrix [n_freq x n_freq]",
  52 |     )
  53 | 
  54 | 
  55 | class DriftForceResult(BaseModel):
  56 |     """Mean and slowly-varying drift force results."""
  57 | 
  58 |     heading_deg: float
  59 |     mean_surge_kn: float = Field(..., description="Mean drift force in surge (kN)")
  60 |     mean_sway_kn: float = Field(..., description="Mean drift force in sway (kN)")
  61 |     mean_yaw_knm: float = Field(..., description="Mean drift moment in yaw (kN·m)")
  62 |     sv_surge_std_kn: float = Field(
  63 |         default=0.0,
  64 |         description="Standard deviation of slowly-varying surge force (kN)",
  65 |     )
  66 |     sv_sway_std_kn: float = Field(
  67 |         default=0.0,
  68 |         description="Standard deviation of slowly-varying sway force (kN)",
  69 |     )
  70 | 
  71 | 
  72 | class WindCurrentDrift(BaseModel):
  73 |     """Steady drift force from wind and current."""
  74 | 
  75 |     wind_speed_m_s: float
  76 |     current_speed_m_s: float
  77 |     heading_deg: float
  78 |     wind_force_surge_kn: float
  79 |     wind_force_sway_kn: float
  80 |     wind_moment_yaw_knm: float
  81 |     current_force_surge_kn: float
  82 |     current_force_sway_kn: float
  83 |     current_moment_yaw_knm: float
  84 |     total_surge_kn: float
  85 |     total_sway_kn: float
  86 |     total_yaw_knm: float
  87 | 
  88 | 
  89 | # ---------------------------------------------------------------------------
  90 | # Mean drift forces
  91 | # ---------------------------------------------------------------------------
  92 | 
  93 | def compute_mean_drift_force(
  94 |     drift_coeffs: MeanDriftCoefficients,
  95 |     wave_spectrum: np.ndarray,
  96 |     omega: np.ndarray,
  97 | ) -> Tuple[float, float, float]:
  98 |     """Compute mean drift forces from coefficients and wave spectrum.
  99 | 
 100 |     F_drift = integral( 2 * D(omega) * S(omega) d_omega )
 101 | 
 102 |     where D(omega) is the mean drift coefficient and S(omega) the wave spectrum.
 103 | 
 104 |     Args:
 105 |         drift_coeffs: Frequency-dependent drift coefficients for one heading.
 106 |         wave_spectrum: Wave spectral density S(omega).
 107 |         omega: Frequency vector (rad/s).
 108 | 
 109 |     Returns:
 110 |         (surge_kN, sway_kN, yaw_kNm) mean drift forces.
 111 |     """
 112 |     freqs = np.array(drift_coeffs.frequencies_rad_s)
 113 |     d_surge = np.interp(omega, freqs, drift_coeffs.surge_kn_m2)
 114 |     d_sway = np.interp(omega, freqs, drift_coeffs.sway_kn_m2)
 115 |     d_yaw = np.interp(omega, freqs, drift_coeffs.yaw_knm_m2)
 116 | 
 117 |     f_surge = float(np.trapz(2.0 * d_surge * wave_spectrum, omega))
 118 |     f_sway = float(np.trapz(2.0 * d_sway * wave_spectrum, omega))
 119 |     m_yaw = float(np.trapz(2.0 * d_yaw * wave_spectrum, omega))
 120 | 
 121 |     return f_surge, f_sway, m_yaw
 122 | 
 123 | 
 124 | # ---------------------------------------------------------------------------
 125 | # Newman's approximation
 126 | # ---------------------------------------------------------------------------
 127 | 
 128 | def newman_approximation(
 129 |     drift_coeffs: MeanDriftCoefficients,
 130 |     wave_spectrum: np.ndarray,
 131 |     omega: np.ndarray,
 132 |     dof: str = "surge",
 133 | ) -> np.ndarray:
 134 |     """Newman's approximation for the slowly-varying QTF.
 135 | 
 136 |     QTF_ij ~ 0.5 * (D(omega_i) + D(omega_j))
 137 | 
 138 |     where D(omega) is the diagonal (mean drift) coefficient.
 139 | 
 140 |     Args:
 141 |         drift_coeffs: Mean drift coefficients.
 142 |         wave_spectrum: Wave spectral density.
 143 |         omega: Frequency vector.
 144 |         dof: Degree of freedom ('surge', 'sway', or 'yaw').
 145 | 
 146 |     Returns:
 147 |         Discrete one-sided slowly-varying force PSD at mu[k] = k * d_omega.
 148 |         For a uniform increasing omega grid, variance is sum(result) * d_omega
 149 |         (full bin weights, including the last bin). The zero-lag bin is zero:
 150 |         equal-frequency pairs give constant mean load, not fluctuation variance.
 151 |         This excludes the mean-load bin previously included in this function.
 152 |     """
 153 |     freqs = np.array(drift_coeffs.frequencies_rad_s)
 154 |     if dof == "surge":
 155 |         d_vals = np.array(drift_coeffs.surge_kn_m2)
 156 |     elif dof == "sway":
 157 |         d_vals = np.array(drift_coeffs.sway_kn_m2)
 158 |     else:
 159 |         d_vals = np.array(drift_coeffs.yaw_knm_m2)
 160 | 
 161 |     d_interp = np.interp(omega, freqs, d_vals)
 162 |     n = len(omega)
 163 |     # Build Newman QTF matrix
 164 |     qtf_newman = 0.5 * (d_interp[:, None] + d_interp[None, :])
 165 |     # Slowly-varying force spectrum (difference-frequency)
 166 |     # S_sv(mu) = 8 * integral( QTF(omega, omega+mu)^2 * S(omega) * S(omega+mu) d_omega )
 167 |     d_omega = omega[1] - omega[0] if len(omega) > 1 else 1.0
 168 |     sv_spectrum = np.zeros(n)
 169 |     # k=0 contains only the constant diagonal loads for fixed wave amplitudes.
 170 |     for k in range(1, n):
 171 |         total = 0.0
 172 |         for i in range(n - k):
 173 |             j = i + k
 174 |             total += qtf_newman[i, j] ** 2 * wave_spectrum[i] * wave_spectrum[j]
 175 |         sv_spectrum[k] = 8.0 * total * d_omega
 176 | 
 177 |     return sv_spectrum
 178 | 
 179 | 
 180 | def full_qtf_slowly_varying(
 181 |     qtf: QTFMatrix,
 182 |     wave_spectrum: np.ndarray,
 183 |     omega: np.ndarray,
 184 | ) -> float:
 185 |     """Compute slowly-varying drift force std using full QTF integration.
 186 | 
 187 |     sigma_sv^2 = 8 * sum_{i<j}( |QTF(omega_i, omega_j)|^2
 188 |                                * S(omega_i) * S(omega_j) * d_omega^2 )
 189 | 
 190 |     Uses a complete Hermitian difference-frequency QTF on a uniform increasing
 191 |     frequency grid and fixed wave-component amplitudes sqrt(2*S*d_omega).
 192 |     Diagonal terms give the mean drift load and are excluded from this std.
 193 |     Compared to the former all-pairs sum, variance is approximately halved on
 194 |     fine grids; a diagonal-only QTF now gives zero slowly-varying force.
 195 | 
 196 |     Args:
 197 |         qtf: Complete Hermitian QTF matrix (both triangles, same frequency grid).
 198 |         wave_spectrum: Wave spectral density.
 199 |         omega: Frequency vector.
 200 | 
 201 |     Returns:
 202 |         Standard deviation of slowly-varying drift force.
 203 |     """
 204 |     freqs = np.array(qtf.frequencies_rad_s)
 205 |     qr = np.array(qtf.real_part)
 206 |     qi = np.array(qtf.imag_part)
 207 |     qtf_mag2 = qr**2 + qi**2
 208 | 
 209 |     # Interpolate wave spectrum to QTF frequency grid
 210 |     s_interp = np.interp(freqs, omega, wave_spectrum)
 211 | 
 212 |     d_omega = freqs[1] - freqs[0] if len(freqs) > 1 else 1.0
 213 | 
 214 |     # With a_i = sqrt(2*S_i*d_omega), Hermitian ordered terms (i,j) and (j,i)
 215 |     # combine into ONE real sinusoid of amplitude 2*a_i*a_j*|Q_ij|. Its phase
 216 |     # variance is amplitude^2/2 = 8*S_i*S_j*d_omega^2*|Q_ij|^2. Distinct
 217 |     # unordered pairs have zero phase-ensemble covariance, so visit i<j once.
 218 |     # i=j is constant in time and belongs to mean load, not this variance.
 219 |     # Convention: Orcina, "Vessel theory: Wave drift and sum frequency loads",
 220 |     # "Second order load theory" (F = Re sum_ij Q_ij a_i a_j exp(i*delta_w*t)).
 221 |     n = len(freqs)
 222 |     variance = 0.0
 223 |     for i in range(n):
 224 |         for j in range(i + 1, n):
 225 |             variance += qtf_mag2[i, j] * s_interp[i] * s_interp[j]
 226 |     variance *= 8.0 * d_omega**2
 227 | 
 228 |     return float(np.sqrt(max(variance, 0.0)))
 229 | 
 230 | 
 231 | # ---------------------------------------------------------------------------
 232 | # Wind and current drift
 233 | # ---------------------------------------------------------------------------
 234 | 
 235 | def compute_wind_current_drift(
 236 |     wind_speed_m_s: float,
 237 |     current_speed_m_s: float,
 238 |     heading_deg: float,
 239 |     projected_wind_area_m2: float = 3000.0,
 240 |     projected_current_area_m2: float = 1500.0,
 241 |     vessel_length_m: float = 250.0,
 242 |     cx_wind: float = 1.0,
 243 |     cy_wind: float = 0.8,
 244 |     cx_current: float = 0.08,
 245 |     cy_current: float = 1.2,
 246 |     rho_air: float = 1.225,
 247 |     rho_water: float = 1025.0,
 248 | ) -> WindCurrentDrift:
 249 |     """Estimate steady drift forces from wind and current.
 250 | 
 251 |     Uses standard drag-type formulations:
 252 |         F = 0.5 * rho * C * A * V^2
 253 | 
 254 |     Args:
 255 |         wind_speed_m_s: Wind speed (m/s).
 256 |         current_speed_m_s: Current speed (m/s).
 257 |         heading_deg: Environmental heading (deg, 0=head, 90=beam).
 258 |         projected_wind_area_m2: Projected above-water area (m^2).
 259 |         projected_current_area_m2: Projected below-water area (m^2).
 260 |         vessel_length_m: Vessel length for yaw moment arm.
 261 |         cx_wind: Wind drag coefficient in surge direction.
 262 |         cy_wind: Wind drag coefficient in sway direction.
 263 |         cx_current: Current drag coefficient in surge direction.
 264 |         cy_current: Current drag coefficient in sway direction.
 265 |         rho_air: Air density (kg/m^3).
 266 |         rho_water: Water density (kg/m^3).
 267 | 
 268 |     Returns:
 269 |         WindCurrentDrift with all force components.
 270 |     """
 271 |     theta = np.deg2rad(heading_deg)
 272 |     cos_t = np.cos(theta)
 273 |     sin_t = np.sin(theta)
 274 | 
 275 |     # Wind forces
 276 |     q_wind = 0.5 * rho_air * wind_speed_m_s**2
 277 |     fw_surge = q_wind * cx_wind * projected_wind_area_m2 * abs(cos_t) * np.sign(cos_t)
 278 |     fw_sway = q_wind * cy_wind * projected_wind_area_m2 * abs(sin_t) * np.sign(sin_t)
 279 |     # Yaw moment: assume moment arm ~ 0.1 * vessel length
 280 |     mw_yaw = fw_sway * 0.1 * vessel_length_m
 281 | 
 282 |     # Current forces
 283 |     q_curr = 0.5 * rho_water * current_speed_m_s**2
 284 |     fc_surge = q_curr * cx_current * projected_current_area_m2 * abs(cos_t) * np.sign(cos_t)
 285 |     fc_sway = q_curr * cy_current * projected_current_area_m2 * abs(sin_t) * np.sign(sin_t)
 286 |     mc_yaw = fc_sway * 0.1 * vessel_length_m
 287 | 
 288 |     # Convert to kN
 289 |     fw_surge_kn = float(fw_surge / 1000.0)
 290 |     fw_sway_kn = float(fw_sway / 1000.0)
 291 |     mw_yaw_knm = float(mw_yaw / 1000.0)
 292 |     fc_surge_kn = float(fc_surge / 1000.0)
 293 |     fc_sway_kn = float(fc_sway / 1000.0)
 294 |     mc_yaw_knm = float(mc_yaw / 1000.0)
 295 | 
 296 |     return WindCurrentDrift(
 297 |         wind_speed_m_s=wind_speed_m_s,
 298 |         current_speed_m_s=current_speed_m_s,
 299 |         heading_deg=heading_deg,
 300 |         wind_force_surge_kn=fw_surge_kn,
 301 |         wind_force_sway_kn=fw_sway_kn,
 302 |         wind_moment_yaw_knm=mw_yaw_knm,
 303 |         current_force_surge_kn=fc_surge_kn,
 304 |         current_force_sway_kn=fc_sway_kn,
 305 |         current_moment_yaw_knm=mc_yaw_knm,
 306 |         total_surge_kn=fw_surge_kn + fc_surge_kn,
 307 |         total_sway_kn=fw_sway_kn + fc_sway_kn,
 308 |         total_yaw_knm=mw_yaw_knm + mc_yaw_knm,
 309 |     )
```

## tests/orcawave/test_drift_forces.py

sha256 `6d1e8db422b4dab0aa9ee6d7e4c3a0c077577cdaa8dd33c314b9e257c8c96352` · 4881 bytes · 141 lines

```
   1 | """Tests for orcawave.drift_forces module."""
   2 | 
   3 | import numpy as np
   4 | import pytest
   5 | 
   6 | from digitalmodel.orcawave.drift_forces import (
   7 |     MeanDriftCoefficients,
   8 |     QTFMatrix,
   9 |     compute_mean_drift_force,
  10 |     compute_wind_current_drift,
  11 |     full_qtf_slowly_varying,
  12 |     newman_approximation,
  13 | )
  14 | from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz
  15 | 
  16 | 
  17 | def _make_drift_coeffs() -> MeanDriftCoefficients:
  18 |     """Helper: create realistic drift coefficients."""
  19 |     freqs = [0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.5]
  20 |     # Typical mean drift: increases with frequency squared (short waves)
  21 |     surge = [10 * f**2 for f in freqs]
  22 |     sway = [8 * f**2 for f in freqs]
  23 |     yaw = [50 * f**2 for f in freqs]
  24 |     return MeanDriftCoefficients(
  25 |         heading_deg=0.0,
  26 |         frequencies_rad_s=freqs,
  27 |         surge_kn_m2=surge,
  28 |         sway_kn_m2=sway,
  29 |         yaw_knm_m2=yaw,
  30 |     )
  31 | 
  32 | 
  33 | class TestMeanDriftForce:
  34 |     """Test mean drift force computation."""
  35 | 
  36 |     def test_positive_drift(self):
  37 |         coeffs = _make_drift_coeffs()
  38 |         omega = np.linspace(0.1, 2.0, 200)
  39 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  40 |         surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
  41 |         assert surge > 0  # positive drift coefficients -> positive force
  42 |         assert sway > 0
  43 | 
  44 |     def test_zero_spectrum_zero_drift(self):
  45 |         coeffs = _make_drift_coeffs()
  46 |         omega = np.linspace(0.1, 2.0, 200)
  47 |         s_wave = np.zeros_like(omega)
  48 |         surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
  49 |         np.testing.assert_allclose(surge, 0.0, atol=1e-15)
  50 | 
  51 |     def test_larger_hs_larger_drift(self):
  52 |         coeffs = _make_drift_coeffs()
  53 |         omega = np.linspace(0.1, 2.0, 200)
  54 |         s1 = pierson_moskowitz(omega, hs=2.0, tp=10.0)
  55 |         s2 = pierson_moskowitz(omega, hs=5.0, tp=10.0)
  56 |         surge1, _, _ = compute_mean_drift_force(coeffs, s1, omega)
  57 |         surge2, _, _ = compute_mean_drift_force(coeffs, s2, omega)
  58 |         assert surge2 > surge1
  59 | 
  60 | 
  61 | class TestNewmanApproximation:
  62 |     """Test Newman's approximation for slowly-varying forces."""
  63 | 
  64 |     def test_sv_spectrum_positive(self):
  65 |         coeffs = _make_drift_coeffs()
  66 |         omega = np.linspace(0.1, 2.0, 50)
  67 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  68 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  69 |         assert np.all(sv >= 0)
  70 | 
  71 |     def test_sv_spectrum_length(self):
  72 |         coeffs = _make_drift_coeffs()
  73 |         omega = np.linspace(0.1, 2.0, 50)
  74 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  75 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  76 |         assert len(sv) == len(omega)
  77 | 
  78 |     def test_sv_zero_spectrum(self):
  79 |         coeffs = _make_drift_coeffs()
  80 |         omega = np.linspace(0.1, 2.0, 50)
  81 |         s_wave = np.zeros_like(omega)
  82 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  83 |         np.testing.assert_allclose(sv, 0.0, atol=1e-15)
  84 | 
  85 | 
  86 | class TestFullQTF:
  87 |     """Test full QTF slowly-varying computation."""
  88 | 
  89 |     def test_full_qtf_positive_std(self):
  90 |         freqs = [0.3, 0.5, 0.7, 0.9]
  91 |         n = len(freqs)
  92 |         # A constant complete Newman QTF has nonzero off-diagonal beat loads.
  93 |         real_part = np.full((n, n), 10.0)
  94 |         imag_part = np.zeros((n, n))
  95 |         qtf = QTFMatrix(
  96 |             dof="surge",
  97 |             heading_deg=0.0,
  98 |             frequencies_rad_s=freqs,
  99 |             real_part=real_part.tolist(),
 100 |             imag_part=imag_part.tolist(),
 101 |         )
 102 |         omega = np.array(freqs)
 103 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
 104 |         sigma = full_qtf_slowly_varying(qtf, s_wave, omega)
 105 |         assert sigma > 0
 106 | 
 107 | 
 108 | class TestWindCurrentDrift:
 109 |     """Test wind and current drift estimation."""
 110 | 
 111 |     def test_head_wind(self):
 112 |         result = compute_wind_current_drift(
 113 |             wind_speed_m_s=20.0,
 114 |             current_speed_m_s=1.0,
 115 |             heading_deg=0.0,
 116 |         )
 117 |         assert result.wind_force_surge_kn != 0
 118 |         assert result.total_surge_kn != 0
 119 | 
 120 |     def test_beam_wind_sway(self):
 121 |         result = compute_wind_current_drift(
 122 |             wind_speed_m_s=20.0,
 123 |             current_speed_m_s=0.0,
 124 |             heading_deg=90.0,
 125 |         )
 126 |         # At 90 deg, sway should be dominant
 127 |         assert abs(result.wind_force_sway_kn) > abs(result.wind_force_surge_kn)
 128 | 
 129 |     def test_zero_wind_zero_force(self):
 130 |         result = compute_wind_current_drift(
 131 |             wind_speed_m_s=0.0,
 132 |             current_speed_m_s=0.0,
 133 |             heading_deg=0.0,
 134 |         )
 135 |         np.testing.assert_allclose(result.total_surge_kn, 0.0)
 136 |         np.testing.assert_allclose(result.total_sway_kn, 0.0)
 137 | 
 138 |     def test_higher_wind_higher_force(self):
 139 |         r1 = compute_wind_current_drift(10.0, 0.0, 0.0)
 140 |         r2 = compute_wind_current_drift(30.0, 0.0, 0.0)
 141 |         assert abs(r2.wind_force_surge_kn) > abs(r1.wind_force_surge_kn)
```

## tests/orcawave/test_drift_force_variance.py

sha256 `3f120a1b2f0b063b6d646bd64e3b3e319fbe14d64de1c29000606996765b4744` · 7493 bytes · 187 lines

```
   1 | """Synthetic QTF regression and independent time-domain references for issue 2617.
   2 | 
   3 | Reference convention: Orcina, 'Vessel theory: Wave drift and sum frequency loads',
   4 | Second order load theory: F(t) = Re sum_ij Q_ij a_i a_j exp(i(w_i-w_j)t).
   5 | All coefficients, sea states and oscillator parameters below are synthetic.
   6 | Run this file directly with PYTHONPATH=src to print the reproducible metrics.
   7 | """
   8 | 
   9 | import json
  10 | 
  11 | import numpy as np
  12 | import pytest
  13 | from scipy.signal import lsim
  14 | 
  15 | from digitalmodel.orcawave.drift_forces import (
  16 |     MeanDriftCoefficients,
  17 |     QTFMatrix,
  18 |     compute_mean_drift_force,
  19 |     full_qtf_slowly_varying,
  20 |     newman_approximation,
  21 | )
  22 | from digitalmodel.orcawave.wave_spectrum import jonswap
  23 | 
  24 | 
  25 | def _qtf(omega, values):
  26 |     """Construct a physical Hermitian QTF, in kN/m^2."""
  27 |     values = np.asarray(values, dtype=complex)
  28 |     return QTFMatrix(
  29 |         dof="surge", heading_deg=0.0, frequencies_rad_s=omega.tolist(),
  30 |         real_part=values.real.tolist(), imag_part=values.imag.tolist(),
  31 |     )
  32 | 
  33 | 
  34 | def _coefficients(omega, values):
  35 |     return MeanDriftCoefficients(
  36 |         heading_deg=0.0, frequencies_rad_s=omega.tolist(),
  37 |         surge_kn_m2=np.asarray(values).tolist(),
  38 |         sway_kn_m2=np.zeros(len(omega)).tolist(),
  39 |         yaw_knm_m2=np.zeros(len(omega)).tolist(),
  40 |     )
  41 | 
  42 | 
  43 | def narrow_band_reference():
  44 |     """Average independent record variances; do not treat correlated samples as iid.
  45 | 
  46 |     Each record spans exactly 2*pi/dw seconds. The FFT evaluates the complex
  47 |     wave envelope, then F=Q*|z|^2 independently of the production pair sum.
  48 |     Nyquist=2.048 rad/s exceeds the largest difference frequency, 1.024 rad/s.
  49 |     """
  50 |     omega = np.linspace(0.2, 1.224, 2049)
  51 |     dw = omega[1] - omega[0]
  52 |     spectrum = jonswap(omega, hs=2.0, tp=2 * np.pi / 0.8, gamma=10.0)
  53 |     coefficient = 10.0  # kN/m^2; constant Newman QTF
  54 |     amplitudes = np.sqrt(2 * spectrum * dw)
  55 |     rng = np.random.default_rng(2617)
  56 |     variances = []
  57 |     for _ in range(32):
  58 |         phases = rng.uniform(0.0, 2 * np.pi, (64, len(omega)))
  59 |         envelope = np.fft.ifft(
  60 |             amplitudes * np.exp(1j * phases), n=8192, axis=1,
  61 |         ) * 8192
  62 |         force = coefficient * np.abs(envelope)**2
  63 |         variances.extend(np.var(force, axis=1).tolist())
  64 |     variances = np.asarray(variances)
  65 |     qtf = _qtf(omega, np.full((len(omega), len(omega)), coefficient))
  66 |     full_variance = full_qtf_slowly_varying(qtf, spectrum, omega)**2
  67 |     newman = newman_approximation(
  68 |         _coefficients(omega, np.full(len(omega), coefficient)), spectrum, omega,
  69 |     )
  70 |     return {
  71 |         "seed": 2617, "records": len(variances), "samples_per_record": 8192,
  72 |         "duration_per_record_s": float(2 * np.pi / dw),
  73 |         "monte_carlo_variance_kn2": float(variances.mean()),
  74 |         "relative_95pct_halfwidth": float(
  75 |             1.96 * variances.std(ddof=1) / np.sqrt(len(variances)) / variances.mean()
  76 |         ),
  77 |         "full_variance_kn2": full_variance,
  78 |         "newman_variance_kn2": float(newman.sum() * dw),
  79 |     }
  80 | 
  81 | 
  82 | def surge_oscillator_reference():
  83 |     """Time-integrate m*x''+c*x'+k*x=F for a bichromatic random-phase force.
  84 | 
  85 |     A complete number of beat cycles removes finite-window phase error. Four
  86 |     independent phase records check phase invariance; 64 cycles are discarded
  87 |     before measuring 128 cycles (exp(-zeta*wn*t) < 4e-18 at the cutoff).
  88 |     lsim integrates a piecewise-linear forcing with 256 samples per beat cycle.
  89 |     """
  90 |     omega = np.array([0.7, 0.8])
  91 |     dw = omega[1] - omega[0]
  92 |     spectrum = np.array([5.0, 1.25])  # amplitudes 1 m and 0.5 m
  93 |     mass, damping, stiffness = 10000.0, 200.0, 100.0  # kN*s^2/m, kN*s/m, kN/m
  94 |     delta = omega[:, None] - omega[None, :]
  95 |     transfer = 1 / (stiffness - mass * delta**2 + 1j * damping * delta)
  96 |     response_qtf = _qtf(omega, 10.0 * np.abs(transfer))
  97 |     predicted = full_qtf_slowly_varying(response_qtf, spectrum, omega)
  98 |     period = 2 * np.pi / dw
  99 |     time = np.arange(192 * 256) * period / 256
 100 |     rng = np.random.default_rng(2617)
 101 |     response_stds = []
 102 |     force_variances = []
 103 |     for _ in range(4):
 104 |         phases = rng.uniform(0.0, 2 * np.pi, 2)
 105 |         envelope = np.sum(
 106 |             np.sqrt(2 * spectrum * dw)[:, None]
 107 |             * np.exp(1j * ((omega - omega[0])[:, None] * time + phases[:, None])),
 108 |             axis=0,
 109 |         )
 110 |         force = 10.0 * np.abs(envelope)**2
 111 |         _, response, _ = lsim(([1.0], [mass, damping, stiffness]), force, time)
 112 |         response_stds.append(float(np.std(response[64 * 256:])))
 113 |         force_variances.append(float(np.var(force[64 * 256:])))
 114 |     return {
 115 |         "predicted_std_m": predicted,
 116 |         "monte_carlo_std_m": float(np.mean(response_stds)),
 117 |         "phase_record_stds_m": response_stds,
 118 |         "force_variance_kn2": float(np.mean(force_variances)),
 119 |         "samples_per_cycle": 256, "retained_cycles": 128, "discarded_cycles": 64,
 120 |     }
 121 | 
 122 | 
 123 | @pytest.mark.parametrize("pair", [10.0 + 0j, 3.0 + 4j])
 124 | def test_bichromatic_pair_variance_excludes_double_count(pair):
 125 |     omega = np.array([0.7, 0.8])
 126 |     spectrum = np.array([5.0, 1.25])
 127 |     qtf = _qtf(omega, [[10.0, pair], [pair.conjugate(), 10.0]])
 128 |     # One real beat force: amplitude=2*a1*a2*|Q12|; variance=amplitude^2/2.
 129 |     expected_variance = (2 * 1.0 * 0.5 * abs(pair))**2 / 2
 130 |     actual = full_qtf_slowly_varying(qtf, spectrum, omega)**2
 131 |     assert actual == pytest.approx(expected_variance, rel=1e-12)
 132 | 
 133 | 
 134 | @pytest.mark.parametrize("count", [1, 4])
 135 | def test_diagonal_qtf_has_mean_load_but_zero_fluctuation(count):
 136 |     omega = 0.7 + 0.1 * np.arange(count)
 137 |     qtf = _qtf(omega, 10.0 * np.eye(count))
 138 |     assert full_qtf_slowly_varying(qtf, np.ones(count), omega) == 0.0
 139 | 
 140 | 
 141 | @pytest.mark.parametrize("diagonal", [[10., 10.], [10., 20.], [-10., -20.]])
 142 | def test_newman_full_agree_with_full_endpoint_bin_weights(diagonal):
 143 |     omega = np.array([0.7, 0.8])
 144 |     spectrum = np.array([5.0, 1.25])
 145 |     diagonal = np.asarray(diagonal)
 146 |     qtf = _qtf(omega, 0.5 * (diagonal[:, None] + diagonal[None, :]))
 147 |     newman = newman_approximation(_coefficients(omega, diagonal), spectrum, omega)
 148 |     assert newman[0] == 0.0
 149 |     assert newman.sum() * (omega[1] - omega[0]) == pytest.approx(
 150 |         full_qtf_slowly_varying(qtf, spectrum, omega)**2, rel=1e-12,
 151 |     )
 152 | 
 153 | 
 154 | def test_mean_drift_qtf_normalization_matches_wave_amplitudes():
 155 |     omega = np.array([0.6, 0.7, 0.8, 0.9])
 156 |     spectrum = np.array([0.0, 5.0, 1.25, 0.0])
 157 |     coeffs = _coefficients(omega, np.full(4, 10.0))
 158 |     mean_force, _, _ = compute_mean_drift_force(coeffs, spectrum, omega)
 159 |     assert mean_force == pytest.approx(10.0 * (1.0**2 + 0.5**2), rel=1e-12)
 160 | 
 161 | 
 162 | @pytest.mark.slow
 163 | def test_narrow_band_monte_carlo_force_variance():
 164 |     result = narrow_band_reference()
 165 |     assert result["relative_95pct_halfwidth"] < 0.01
 166 |     assert result["full_variance_kn2"] == pytest.approx(
 167 |         result["monte_carlo_variance_kn2"], rel=0.01,
 168 |     )
 169 |     assert result["full_variance_kn2"] == pytest.approx(
 170 |         result["newman_variance_kn2"], rel=1e-12,
 171 |     )
 172 | 
 173 | 
 174 | def test_linear_surge_oscillator_std_matches_time_domain():
 175 |     result = surge_oscillator_reference()
 176 |     assert result["force_variance_kn2"] == pytest.approx(50.0, rel=1e-12)
 177 |     assert result["predicted_std_m"] == pytest.approx(
 178 |         result["monte_carlo_std_m"], rel=0.05,
 179 |     )
 180 |     assert np.ptp(result["phase_record_stds_m"]) < 1e-10
 181 | 
 182 | 
 183 | if __name__ == "__main__":
 184 |     print(json.dumps({
 185 |         "narrow_band": narrow_band_reference(),
 186 |         "surge_oscillator": surge_oscillator_reference(),
 187 |     }, indent=2))
```

## docs/verification/issue-2617-qtf-variance.html

sha256 `efa075cce2846e9955145be817df68433761ee740f0c6ae3851b20951eb2b313` · 8786 bytes · 88 lines

```
   1 | <!doctype html>
   2 | <html lang="en"><meta charset="utf-8"><title>Issue 2617: QTF variance verification</title>
   3 | <style>body{font:16px/1.5 sans-serif;max-width:1060px;margin:40px auto;padding:0 20px}code,pre{background:#eee}pre{padding:12px;white-space:pre-wrap}td,th{border:1px solid #bbb;padding:8px}table{border-collapse:collapse}caption{caption-side:bottom;text-align:left;padding:8px}</style>
   4 | <h1>Difference-frequency QTF variance: confirmed and corrected</h1>
   5 | <p><a href="https://github.com/vamseeachanta/digitalmodel/issues/2617">Issue 2617</a>.
   6 | The unmodified repository function at <code>c5e4ac33</code> returns 50.005840 kN² against
   7 | a fixed-seed Monte Carlo variance of 24.926308 kN²: a ratio of 2.006147.
   8 | The corrected function returns 24.952483 kN², within 0.1051% of Monte Carlo against the 1% criterion.
   9 | All inputs are synthetic. This comparison qualifies the documented numerical cases, not a vessel or mooring design.</p>
  10 | <h2>Physical convention and correction</h2>
  11 | <p>The reference convention is the full difference-frequency load in
  12 | <a href="https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Wavedriftandsumfrequencyloads.htm">Orcina's second-order load theory</a>:
  13 | <code>F(t) = Re sum_ij Q_ij a_i a_j exp(i[(omega_i-omega_j)t+phi_i-phi_j])</code>.
  14 | The complete difference-frequency QTF is Hermitian; <code>a_i = sqrt(2 S_i dw)</code>.</p>
  15 | <p>Two ordered terms form one real sinusoid with amplitude <code>2 a_i a_j |Q_ij|</code>.
  16 | Its variance is <code>(2 a_i a_j |Q_ij|)^2/2 = 8 S_i S_j dw^2 |Q_ij|^2</code>.
  17 | Different unordered pairs have zero covariance under independent uniform phases, including pairs sharing one component.
  18 | The variance sum therefore visits only <code>i&lt;j</code>. The old loop visits each off-diagonal pair twice
  19 | and includes constant <code>i=j</code> mean loads as fluctuating variance.</p>
  20 | <p>The implementation retains factor eight and visits each unique pair once. Newman's positive-lag calculation is retained;
  21 | lag zero is set to zero to exclude mean load for this discrete fixed-amplitude sea. Discrete variance is
  22 | <code>sum(S_sv)*dw</code> with full bin weights, including the final lag. A trapezoidal integral of these discrete bins
  23 | would incorrectly halve an occupied final bin, as demonstrated by the bichromatic test.</p>
  24 | <h2>Narrow-band Monte Carlo reference</h2>
  25 | <p>The generated sea uses the repository JONSWAP function: Hs=2 m, Tp=7.853982 s, gamma=10,
  26 | <code>omega=linspace(0.2,1.224,2049)</code> rad/s, <code>dw=0.0005</code> rad/s.
  27 | The synthetic diagonal drift coefficient and every Newman QTF entry equal 10 kN/m².
  28 | The initial code is called directly before production edits; its source SHA-256 is retained in
  29 | <a href="issue-2617-metrics.json">the compact metrics record</a>.</p>
  30 | <p>With seed 2617, 2048 independent phase vectors are drawn uniformly on [0,2π).
  31 | Each record spans 12566.370614 s at 8192 equally spaced times. The complex wave envelope is evaluated by FFT,
  32 | and <code>F(t)=10*|z(t)|^2</code> is formed in the time domain; this reference never evaluates a pair-variance sum.
  33 | The carrier frequency cancels in the squared magnitude. Difference frequencies lie on exact DFT bins;
  34 | Nyquist=2.048 rad/s exceeds the largest difference frequency of 1.024 rad/s.</p>
  35 | <p>The record force mean is removed by <code>np.var</code>. Sampling uncertainty is estimated from the 2048 independent record variances,
  36 | not the correlated time samples: <code>1.96*std(record_variances)/sqrt(2048)/mean(record_variances)</code>.
  37 | The resulting 95% confidence half-width is 0.29836%, below the requested 1% sampling error.
  38 | Exact sparse tests independently guard the diagonal correction, which is small on this fine grid.</p>
  39 | <table><tr><th>Calculation</th><th>Variance (kN²)</th><th>Difference from Monte Carlo (%)</th></tr>
  40 | <tr><td>Unmodified repository full QTF</td><td>50.005839732175204</td><td>+100.6147</td></tr>
  41 | <tr><td>Monte Carlo</td><td>24.926308458002424</td><td>0</td></tr>
  42 | <tr><td>Corrected repository full QTF</td><td>24.95248308621409</td><td>+0.105008</td></tr>
  43 | <tr><td>Corrected Newman, full discrete bin weights</td><td>24.95248308621378</td><td>+0.105008</td></tr>
  44 | <caption>Table 1. Narrow-band force variances; corrected Newman/full relative difference is below 1e-12.</caption></table>
  45 | <h2>Bichromatic and linear surge checks</h2>
  46 | <p>The two-component case uses frequencies 0.7 and 0.8 rad/s, amplitudes 1 m and 0.5 m,
  47 | and a constant QTF of 10 kN/m². Its single beat force has amplitude 10 kN and exact variance 50 kN².
  48 | The unmodified function returns 312.5 kN²; the time history returns 50.0 kN².
  49 | An additional Hermitian complex-pair case, Q12=3+4i, has exact variance 12.5 kN².
  50 | Single-component and diagonal-only QTFs give zero fluctuation; their mean load remains separate.</p>
  51 | <p>The linear surge equation is <code>10000*x'' + 200*x' + 100*x = F(t)</code>,
  52 | with force in kN and displacement in m. Natural frequency is 0.1 rad/s and damping ratio is 0.1.
  53 | The frequency-domain prediction calls the actual full-QTF function after weighting each QTF entry by
  54 | <code>|1/(k-m*mu²+i*c*mu)|</code>. This is equivalent to filtering the force variance by |H|².</p>
  55 | <p>Four fixed-seed random-phase bichromatic histories are integrated using SciPy <code>signal.lsim</code>
  56 | with piecewise-linear force interpolation, 256 samples per beat cycle, 64 cycles discarded and 128 complete cycles retained.
  57 | The discarded interval reduces homogeneous transient amplitude below 4e-18 of its initial amplitude.
  58 | Standard deviation removes the static offset. Complete-cycle averaging eliminates random-phase window error for this one-beat case;
  59 | the four measured standard deviations differ by less than 1e-10 m.</p>
  60 | <table><tr><th>Prediction</th><th>Surge standard deviation (m)</th></tr>
  61 | <tr><td>Corrected repository function with oscillator weighting</td><td>0.3535533905932738</td></tr>
  62 | <tr><td>Time-domain oscillator, four phase records</td><td>0.3535356427801949</td></tr>
  63 | <caption>Table 2. Surge standard deviation difference is 0.005020%, below the 5% criterion.</caption></table>
  64 | <h2>Caller audit and behavior change</h2>
  65 | <p>A repository-wide text search, including tracked documentation, examples and notebooks, finds no production call site.
  66 | The package re-exports the symbol in <code>src/digitalmodel/orcawave/__init__.py</code> (import and __all__).
  67 | The sole pre-existing direct caller is <code>tests/orcawave/test_drift_forces.py::TestFullQTF.test_full_qtf_positive_std</code>;
  68 | its fixture is updated to a complete QTF on a uniform grid and its assertion requires positive fluctuation.</p>
  69 | <p>New direct callers in <code>tests/orcawave/test_drift_force_variance.py</code> are
  70 | <code>narrow_band_reference</code>, <code>surge_oscillator_reference</code>,
  71 | <code>test_bichromatic_pair_variance_excludes_double_count</code>,
  72 | <code>test_diagonal_qtf_has_mean_load_but_zero_fluctuation</code>, and
  73 | <code>test_newman_full_agree_with_full_endpoint_bin_weights</code>.
  74 | The Monte Carlo and oscillator tests reach the function through the first two helpers.</p>
  75 | <p>External consumers are not inventoried. Their fine-grid full-QTF standard deviations will decrease by approximately 1/sqrt(2);
  76 | sparse/diagonal cases can change more. Newman's zero-lag bin changes from a positive value to zero.
  77 | Both public signatures and return types are preserved. Complete Hermitian matrices and uniform increasing integration grids are prerequisites;
  78 | validation of malformed, triangular-only or nonuniform inputs is unchanged. Nonlinear mooring response, directional seas and licensed solvers are not evaluated.</p>
  79 | <h2>TDD, reproducibility and retained evidence</h2>
  80 | <p>The pre-change regression run records seven failures and one pass, including force variance and surge response.
  81 | The original 38 drift/spectrum/motion tests pass before the fix. Additional signed, frequency-varying Newman cases are included after reproduction.
  82 | All 357 tests in <code>tests/orcawave</code> pass after correction. A public-package import smoke check resolves the function to the real repository module.</p>
  83 | <pre>PYTHONPATH=src uv run --no-project --with numpy==1.26.4 --with pydantic --with scipy --with pytest python tests/orcawave/test_drift_force_variance.py
  84 | PYTHONPATH=src uv run --no-project --with numpy==1.26.4 --with pydantic --with scipy --with pytest --with pytest-asyncio --with pyyaml --with pandas --with jinja2 --with plotly --with loguru python -m pytest tests/orcawave -q</pre>
  85 | <p>Only generation code, compact numeric metrics, this report and frozen review evidence are retained.
  86 | Time histories and full matrices are temporary in-memory arrays. No client data or licensed standards are used.
  87 | Baseline and corrected observations are retained together in the metrics record; saved values are read back before publication.</p>
  88 | </html>
```

## docs/verification/issue-2617-metrics.json

sha256 `c37784e0766d4c20f274df6f813f1698fc3a1aeca9dd195393c40f990948c9ed` · 1299 bytes · 40 lines

```
   1 | {
   2 |   "narrow_band": {
   3 |     "seed": 2617,
   4 |     "records": 2048,
   5 |     "samples_per_record": 8192,
   6 |     "duration_per_record_s": 12566.37061435916,
   7 |     "monte_carlo_variance_kn2": 24.926308458002424,
   8 |     "relative_95pct_halfwidth": 0.0029835655035290666,
   9 |     "full_variance_kn2": 24.95248308621409,
  10 |     "newman_variance_kn2": 24.95248308621378
  11 |   },
  12 |   "surge_oscillator": {
  13 |     "predicted_std_m": 0.3535533905932738,
  14 |     "monte_carlo_std_m": 0.3535356427801949,
  15 |     "phase_record_stds_m": [
  16 |       0.35353564278019506,
  17 |       0.3535356427801948,
  18 |       0.3535356427801951,
  19 |       0.35353564278019484
  20 |     ],
  21 |     "force_variance_kn2": 50.0000000000001,
  22 |     "samples_per_cycle": 256,
  23 |     "retained_cycles": 128,
  24 |     "discarded_cycles": 64
  25 |   },
  26 |   "baseline": {
  27 |     "seed": 2617,
  28 |     "records": 2048,
  29 |     "samples_per_record": 8192,
  30 |     "duration_per_record_s": 12566.37061435916,
  31 |     "force_variance_kn2": 24.926308458002424,
  32 |     "relative_95pct_halfwidth": 0.0029835655035290666,
  33 |     "repository_variance_kn2": 50.005839732175204,
  34 |     "repository_to_mc": 2.0061470320175374,
  35 |     "newman_rectangle_variance_kn2": 25.053356645979488,
  36 |     "newman_trapezoid_variance_kn2": 25.002919866096626,
  37 |     "git_revision": "c5e4ac33",
  38 |     "source_sha256": "b33d7cc8f677d4f61a42f2f5d9b96c95c57943d3d600c58baf721548b4872fa6"
  39 |   }
  40 | }
```
