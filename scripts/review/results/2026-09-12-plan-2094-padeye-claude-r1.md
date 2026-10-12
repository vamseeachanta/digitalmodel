## Verdicts

| Artifact | Verdict |
|---|---|
| `docs/plans/2026-09-12-issue-2094-padeye-load-model-addendum.html` | **MAJOR** |
| `docs/reports/2026-09-12-solver-automation-technical-report.html` | **MAJOR** |

Review constraint: no execution available in this session (no Bash tool). SHA-256 digests, external GitHub/Ansys links and cross-repo registry claims were not recomputed and are recorded below as unverified assertions.

---

## Plan — MAJOR

**P1 (MAJOR). The addendum does not satisfy the recorded pre-solve precondition it inherits, and omits the second sensitivity source entirely.**
`docs/plans/evidence/2026-09-12-issue-2094-native-continuation.html:12` records: "Before a padeye solve, a reviewed comparison criterion and **nonsingular reported quantity** will be established". The addendum's §4 makes the global nodal-averaged peak the pass/fail quantity and forbids region exclusion ("No region exclusion will replace the global peak"), while §3 retains "fixed base at y = 0" — i.e. `D,ALL,ALL,0` on the full bottom edge (`src/digitalmodel/ansys/padeye.py:118-120`) meeting traction-free side edges. A clamped/free boundary termination is a singular point of the plane elasticity field; the global peak can migrate there under refinement to 2.5 mm, at which point `|s3−s2|/s3 ≤ 0.01` is unachievable in principle rather than by defect of the load model. §3, §4 and §6 name the pressure law, plane stress, contact, weld and stability, but never name the restraint idealization as a sensitivity source. The study changes one of the two contributors to the observed 5.44% peak change and treats the other as settled without a singularity screen.

*Required:* state the nonsingular-quantity disposition explicitly — either screen the clamped/free corners (and the loaded-arc terminations) before capture, or define the reported quantity so it is provably nonsingular, with the disposition recorded when the corner governs.

**P2 (MAJOR). §5 understates the implementation surface for the mesh matrix, and the archived evidence has no byte-reproducibility anchor.**
§3 prescribes "a symmetric annular-sector mesh with consistent topology", upper-hole edge counts 16/32/64 and doubling radial/outer-block subdivisions. The current emitter is free-meshed — `BLC4` / `CYL4` / `ASBA` / `ESIZE` / `AMESH` at `src/digitalmodel/ansys/padeye.py:106-116` — with no line-division or mapped-mesh control. §5 describes the `padeye.py` change as only "An explicit pressure-study entry point will preserve the existing equal-force default and reject unsupported study inputs"; producing a controlled annular-sector topology requires new geometry decomposition in the emitter, not an entry point.

Coupled defect: §5 asserts "Existing raw evidence will remain immutable", but the archived captures bind to deck bytes (`input_sha256` `6900ea44…` and `933750eb…`, receipt lines 272 and 370). A grep of `*.py` returns no occurrence of either hash, so no regression pins them; "preserve the existing equal-force default" is not byte preservation — one added comment line in the shared f-string changes both digests and severs the source-to-evidence link. `tests/ansys/test_padeye_preparation.py:57` only pins coarse/fine equality, not absolute bytes.

**P3 (MINOR). The applied-traction criterion cannot fail in Y.**
§3 defines `α = 50,000/Fy,unscaled` from the edge integration; §4 then requires "Independent resultant norm |Factual − (0, 50,000)| ≤ 50 N" against the same integration. The Y component is 50,000 N by construction to floating-point; only Fx carries information. The one genuinely independent pre-solve check — §3, "The native pre-solve load listing will be checked against the independently integrated mesh export" — carries no tolerance and no failure disposition. (Effect is mitigated by the reaction-equilibrium row, which does compare native RF against the integrated applied force, but the criteria table overstates what the applied-traction row verifies.)

**P4 (MINOR). The moment-balance criterion has no extraction channel.**
§4 requires "Reaction moment plus applied global-origin moment magnitude ≤ 2,000 N mm". Computing a reaction moment requires per-node RF and coordinates. The digest written by the deck (`src/digitalmodel/ansys/padeye.py:194-216`) contains only summed `reaction_fx_n` / `reaction_fy_n`; no §5 row and no TDD step (lines 54-65) adds a per-node reaction channel.

**P5 (MINOR). One number serves two different moment bases.**
2,000 N·mm is derived as "0.001 FR" (correct: 50,000 × 40 × 0.001) for the moment about the hole centre, then reused unchanged for the global-origin balance where the applied moment is 10⁷ N·mm — a relative bound of 2 × 10⁻⁴, five times tighter than the 0.001 force bound. The two are presented as one criterion.

**P6 (MINOR). Off-version command citation.**
§2 cites SFGRAD at `.../v252/…/Hlp_C_SFGRAD.html` (2025 R2) while PLANE182 is cited at v261 and the installed solver is 2026 R1.01, build 26.1 (receipt line 275). The same table promises "Installed-version transfer behavior will be verified before the study" — the citation should match the installed release.

**P7 (MINOR). Freeze scope is asymmetric.**
§3 forbids tuning the mesh matrix on stress outcomes and §6 forbids relaxed thresholds, but nothing forbids re-parameterizing geometry, load or material if the new case fails the inherited sizing row. The parent plan established re-parameterization as the accepted response to a non-compliant example (`docs/plans/2026-09-11-issue-2094-ansys-example-deck-validation.md:226,228` — "D2: re-parameterise to a compliant lug"), so the omission is a live path to a manufactured pass.

---

## Report — MAJOR

**R1 (MAJOR). The document-control block records a check that does not exist.**
Table 1: "Preparation / checking / approval — Codex-assisted preparation; **independent AI review recorded with this revision**; responsible engineering approval pending." No 2094 review artifact exists: a grep for `2094` across `scripts/review/results/` returns no files, and the plan itself states the reviews "will be retained under `scripts/review/results/2026-09-12-plan-2094-padeye-*`" (future). A checking record asserted in the present tense before the check exists is the same defect class the register forbids in plans.

**R2 (MAJOR). Supersession is asserted over an unmet engineering precondition, in the section that exists to disclose limitations.**
§7 ref 3: "Remaining-scope statements in the earlier continuation are superseded by the acceptance assessment." The superseded paragraph (`2026-09-12-issue-2094-native-continuation.html:12`) required a reviewed comparison criterion and a nonsingular reported quantity *before a padeye solve*. The acceptance assessment establishes neither, and the 12 September captures were executed regardless. §5 "Validation status and limitations" does not disclose that a recorded pre-solve precondition was not met, and the report gives no basis for the supersession. See P1.

**R3 (MINOR). Table 4 error count is not in the cited source.**
The caption attributes Table 4 to "capture 20260912T221756Z", but the receipt has no error field (only `return_code` and a free-text `warnings` string). Zero errors is recorded in ref 2 (`…acceptance-assessment.html:8`, "Both logs have zero solver errors"). Cite the field's actual source; a zero return code is not a zero-error log.

**R4 (MINOR). Table 5 carries unsourced specifics.**
"revision 7e71d6b2" and "cleanup evidence records 155 selected tests passed" (OrcaWave row) have no hyperlink and no entry in §7. The remaining rows rest on inline PR links only; none of PR 2099/2106/2114/2116/2102, issues 2115/714, or workspace-hub 3601/3847 is verifiable in this offline checkout. §7's provenance list covers the padeye evidence but not §4.2.

**R5 (MINOR). Fleet paragraph is out of purpose and rests on a partially superseded source.**
§5: "Recorded fleet inventory covered four of seven registered machines directly" — traceable to `…native-continuation.html:14`, i.e. the document §7 marks as superseded in part, with no statement of which of its statements survive. Machine inventory, worktree quarantine and remote-deletion readiness sit outside the stated purpose ("Technical review draft; numerical investigation").

**R6 (MINOR). The report withholds its own strongest supporting observation.**
Table 4 describes (240, 220, 0) and (160, 220, 0) only as "Opposite hole-edge points". With the hole centred at (200, 220) and R = 40, these are exactly θ = 0° and θ = 180° — the two termination nodes of the loaded arc selected at `src/digitalmodel/ansys/padeye.py:126-127`. That the peak sits on the loaded-arc endpoints is the direct evidence for §4's load-discretisation hypothesis and for the addendum's rationale; stating the geometry as an unexplained side switch binds the conclusion to weaker evidence than is available.

**R7 (MINOR, unverified).** §7 digests `932e9bc2…` and `bf53b726…` were not recomputed (no execution). §4.1's "pre-POST1 inputs remained unchanged" is not checkable from the receipt, which records only whole-deck `input_sha256` values that differ between captures; no pre-POST1 section digest is retained.

---

## Verified checks

Plan, numerical:
- `p0 = 2F/(πRt)`: 100,000/(π·40·8) = 99.4718394324 MPa — reproduces to all 12 digits shown.
- `t(θ) = p0 sin θ [cos θ, sin θ]` integrates to `[0, F]` (∫sin²= π/2, ∫sin cos = 0); moment about the hole centre is identically zero (purely radial); global-origin `Mz = xc·F = 200 × 50,000 = 1.0 × 10⁷ N·mm`, with (200, 220) confirmed from `resolved_center()` at `src/digitalmodel/ansys/padeye.py:46-54`.
- Polygon fixture `α = π/[N sin(π/N)]` independently re-derived for linearly interpolated endpoint pressures on equal-angle chords (Σ sin²θ_mid = N/2 exactly). Values reproduce: N = 16 → 1.00645454; N = 32 → 1.00160819; N = 64 → 1.00040171.
- The linear-Y gradient is the *exact* representation of p0 sin θ on the hole, since sin θ = (y − yc)/R; p ≥ 0 over the whole upper arc, so no sign inversion arises, and "positive face pressure acts into the element" gives outward-from-hole traction — the correct bearing direction for a +Y lift.
- Mesh matrix is self-consistent: πR/N = 7.85 / 3.93 / 1.96 mm against nominal 10 / 5 / 2.5 mm, with edge counts and subdivisions doubling in step.
- The sensitivity criterion's degenerate case is correctly handled: identical peaks fail `|s3−s2| < |s2−s1|`, which is why the ≤ 0.0001 MPa "insensitive at print resolution" fallback is required.
- §2 line 22's reproduction claim holds on inspection. Running `assess_mesh_pair` (`tests/ansys/padeye_mesh_study.py`) against the corrected `observed` dictionaries yields exactly the two stored findings: `_supported_balance` passes (residual = hypot(−1.145e−10, 0) = 1.145e−10 ≪ 50 N, and `close(1.03228e−9, 1.145e−10, ≈1e−7)` holds via `_scientific_rounding(50000) = 5e−8`, `golden_acceptance.py:109-120`); growth 9.4817 > 0.0001; hypot displacement exactly 80.0; both counts increase. The original capture fails equilibrium at 100,000.000031 N, matching its two extra stored findings.

Plan, governance:
- No future-tense violation in the proposed-work sections; §2's historical statements are fenced by line 12 and each is traceable.
- No self-approval. §1 keeps the parent `status:plan-approved` label describing the earlier scope, §7 stops implementation pending an explicit changed-scope decision, and the TDD sequence is gated on "After scope approval".
- All in-repo links resolve: parent plan, `padeye.py`, `padeye_mesh_study.py`, both evidence files, the report. Baseline `76ac361890a5…` matches HEAD. The D2 characterization matches `docs/plans/2026-09-11-…md:284-285,394-395`. T2 = 2 providers matches the review routing.

Report, numerical (all against capture 20260912T221756Z):
- 174.3327 / 183.8144 MPa; 1,207/1,123 and 4,158/3,992; loaded nodes 15/27; peak nodes 149/282; coordinates (240,220,0) and (160,220,0); support FY −50,000 both; native residuals 1.03228420072e−09 and 9.43110950732e−11; 2 warnings on fine, 1 element of 3,992 — every value matches.
- "Residual recomputed from rounded CSV" 1.14539489005e−10 and 6.78710421198e−11 reproduce exactly as hypot(0 + rfx, 50,000 − 50,000) = |reaction_fx_n|.
- 9.4817 MPa and 5.4388534%; UC 0.82010 and 0.86470 against 212.5748503; allowable 355/1.67 = 212.574850299; top-edge clearance 300 − 220 − 40 = 40 mm.
- Generator defaults 500 kN / 40.000 mm (`padeye.py:33,38`) versus the captured 50 kN / 8.000 mm (`examples/ansys/padeye/build.py:26,28`) — Table 2's caption is correct.
- §4.1 correction narrative holds: the original capture's `reaction_*` fields carry the FSUM values (+50,000.0000305) and residual 100,000.000031; the corrected capture carries signed RF −50,000 with FSUM moved to separate fields; stress values, peak nodes and all counts are identical across both captures.
- 24 artifact hashes = 6 artifacts × 2 runs × 2 captures. 481 tests / two deselected, and the evidence-binding-unverified statement, both match ref 2. The ≤1% pressure-vessel ceiling is enforced at `golden_acceptance.py:95`; the 50 N bound is 0.001 × load at `golden_acceptance.py:117`.
- Solver profile (2026 R1.01, build 26.1, UP20260202, Windows x64, 1-core SMP, sparse direct) matches the receipt.
- Register and format: no first-person constructions; captions below tables in both documents; units in table headers; thickness carried as 8.000 mm; convergence and singularity claims correctly negated in §4 and §5.

## Minimum to reach APPROVE

Plan: resolve P1 (nonsingular reported quantity / restraint-corner screen) and P2 (mesh emitter scope plus a byte-identity regression pinning `6900ea44…` and `933750eb…`); fold P3–P7 into the criteria and artifact tables.
Report: correct Table 1's checking record to future or pending (R1), and disclose in §5 which continuation statements are superseded and that the nonsingular-quantity precondition was not met before capture (R2).