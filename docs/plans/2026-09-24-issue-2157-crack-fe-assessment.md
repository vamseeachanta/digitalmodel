# Plan for #2157: Crack-like flaw assessment from own FE crack parameters (API 579-1 Part 9)

- **Issue:** [digitalmodel#2157](https://github.com/vamseeachanta/digitalmodel/issues/2157) · parent epic [#1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) · follows [#1270](https://github.com/vamseeachanta/digitalmodel/issues/1270) (closed)
- **Client:** N/A. The design basis is assumed and no client data is used.
- **Project:** N/A
- **Plan revision:** r2.2 (2026-09-24; Codex r2.1 confirming pass = MINOR, its three stale-wording findings patched inline), status `plan-review`. Owner cards G10, G11 and G12 were decided at 2026-09-24T10:56Z. r2.1 applied the Codex r2 findings as inline patches. As G12 requested, Codex then ran one confirming pass on r2.1 (MINOR: three stale-wording lines, patched in r2.2). The plan awaits owner approval; the owner applies `status:plan-approved`.
- **Complexity:** T3, delivered in five phases (P0–P4) with a code review per phase

## Revision r2: what changed and why

r1 received **MAJOR** from Codex (10 findings) and from Claude (11 findings). The owner then decided seven scope cards on the decision board: save `crack-fe-2157-decisions (2).json`, 2026-09-24T10:39Z, all seven matching the recommendations. The earlier save (09:55Z) also recorded the owner's direction on B02 and B05–B08.

| Card | Owner decision | Effect on this plan |
|---|---|---|
| S01 | Own FE model: ANSYS MAPDL batch with CINT SIF/J at discrete crack depths | New phase P0: FE model generator, K/J extraction, verification |
| S04 | Originality and comparator guard | Every number is regenerated from our model and cited laws. Tests assert closed-form or conservation comparators only. No values from the published case appear in the repository or the report. |
| B01 | Option 1 / API Level 2 curve now; FE-derived Option 3 later | P1 implements material-based curves. Option 3 is a follow-on issue. |
| B02 | σ_ref = FE-linearised stress from our own model | σ_ref basis recorded; limit-load Lr is a sensitivity (card S02, decided at P0) |
| G07 | Five phases: P0 FE · P1 FAD + growth · P2 secondary stress · P3 coordinator/workflow/citations · P4 report | Phase structure below |
| G08 → G11 | One consumer contract (G08); refined by G11 to a sibling `CrackAssessmentResult` behind a shared Protocol | `FFSAssessmentResult` stays unchanged; consumers type against the Protocol |
| G05 | Standards-derived constants are user inputs with a basis; the repository ships no standard-derived tables | Growth-law constants, Φ/ρ, PSFs, residual profiles and E(T) enter as inputs |
| G04 | Local review, then commit after approval | The report goes to `D:\ws\output\crack-fe-weldolet\` first |
| Owner notes B02, B05–B08 | Self-contained analysis; design data stated as assumed; the source article is not referenced | The report carries an assumed design basis only |

The cards gated at later phases (P0: S02, S03, S05, B12; P1: B05–B09, B14, B15, R01, R02; P2: B10, B11, B13, G01–G03; P3: E01–E04, R04, R05, G09; P4: R06) are open. Each phase starts only after its cards are decided. The board is at `D:\ws\output\crack-fe-weldolet\decisions\crack-fe-2157-decisions-local.html`.

## Resource Intelligence Summary

### Existing repo code
- `asset_integrity/assessment/crack_fad.py`
  - Contains `lr_max` (L45), `fad_curve_option1` (L50), `newman_raju_k` (L73), `reference_stress_surface_flaw` (L135), `kmat_from_charpy` (L161) and `assess_crack_like_flaw` / `CrackFlawAssessment` (L179–198).
  - `fad_curve_option1` implements the (1+0.5Lr²)^-0.5 (0.3+0.7e^(−μLr⁶)) form, while its docstrings (L2, L8, L43) state equivalence with API 579 Part 9 Level 2. The Level 2 form (1−0.14Lr²)(0.3+0.7e^(−0.65Lr⁶)) is not implemented. The two differ numerically: 0.1684 against 0.1756 at Lr = 1.721.
  - Codex r1 holds that the curves are the same. **Resolved on secondary sources (2026-09-24, board card R01):**
    - An open-access, peer-reviewed paper (*Materials* 19 (2026) 465, PMC12897954, CC BY 4.0) cites API 579-1/ASME FFS-1 2016 and states that its Level 2 uses the BS 7910 Level 2A generic curve (1−0.14Lr²)(0.3+0.7e^(−0.65Lr⁶)). The paper typesets it as a division, which is physically impossible (Kr > 1 at Lr = 1).
    - The licensed BS 7910:2013 text confirms that Option 1 is the (1+0.5Lr²)^-0.5 form.
    - So API 579 2016 Level 2 matches the *older* BS Level 2A curve, not the 2013 Option 1 form that `crack_fad` implements. Codex was right about the API/Level 2A identity, and the `crack_fad` docstring is wrong.
    - The primary API text is DRM-protected and unread. Docstrings will name editions on this secondary-source basis and say so.
- `fatigue/crack_growth.py` `paris_law_life` (L68)
  - Forward Euler. At the default step it gives 0.045 % error on a √a case (Claude r1).
  - It truncates silently when ΔK ≤ 0 (L122–123).
  - The GPL header (L1–3) is stale.
  - The new growth code will **not** call it.
- `ansys/runner.py` (fail-closed MAPDL subprocess runner, #940) and the APDL generators `ansys/padeye.py` (#948) and `ansys/mudmat.py`. P0 follows this generator + runner pattern.
- `ansys/weld_assessment.py` `calculate_linearized_stresses` (L312) and the `pressure_vessel.py` SCL linearisation (L759) cover through-thickness linearisation.
- `ansys/results_extractor.py` `ResultsExtractor` (L162) parses PRNSOL and element tables. **Nothing in the repository parses CINT (SIF/J) output.**
- `asset_integrity/assessment/ffs_coordinator.py` `FFSAssessmentResult`
  - Its metal-loss fields (`t_nominal_in` … `rerated_pressure_psi`) are required floats.
  - Its verdicts are ACCEPT/MONITOR/RE_RATE/REPAIR/REPLACE.
- `citations/schema.py` (fail-closed `validate_citation` L141, exact revision match) and `citations/registry.py`. The CI overlay `digitalmodel/knowledge/` has no API 579 or BS 7910 stub.
- `reporting/calc_report.py` `CalcReport` (L456): the house style, with no CDN.
- Workflow wiring:
  - `engine.py` L790–796;
  - `docs/registry/workflows.yaml` L1243;
  - `tests/workflows/test_durable_workflows.py`.
- Legacy `common/BS7910_critical_flaw_limits.py` (1,223 lines) is broken under pandas ≥ 2 and its tests assert `{} == {}`. It is not used.

### Standards (rights-gated per G05)
- API 579-1/ASME FFS-1 (2016) Part 9 is the procedure the report follows. Ledger rows `SECT-09` and `APPEND-C` are catalogued (`modules: []`). code-registry has no entry (card E04).
- BS 7910 is the source family for the growth law and ρ. code-registry lists our edition as 2019 with `status: check`, and the wiki records no 2019 edition (card E02).
- No clause text, table or worked example enters the repository. Standard-derived numbers enter only as user inputs carrying a basis string.

### LLM Wiki pages consulted
- `llm-wiki/wikis/engineering-standards/wiki/standards/api-std-579.md`: `api-std-579-asme-ffs-1`, 2016, private. This is the candidate citation target (card E01).
- `llm-wiki/wikis/engineering-standards/wiki/standards/bs-7910.md`: `bs-7910`, multi-edition. Three conflicting pages exist (card E02).
- `llm-wiki/wikis/engineering-standards/wiki/concepts/fatigue-crack-growth.md`: Paris law, threshold, R-ratio.
- `llm-wiki/wikis/engineering/wiki/concepts/engineering-report-house-style.md`: the report skeleton and register.

### Documents consulted
- `docs/domains/asset-integrity/ffs-architecture.md`: the canonical path is `assessment/`, extended additively.
- `docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md`: covers metal loss and excludes crack-like flaws.
- FAD theory (answer to the owner's board note on B01), from public sources:
  - Option 1 needs σy (plus σu for the cut-off, and E); Option 2 needs the full stress–strain curve.
  - Option 3 is derived from FE of the cracked component as Kr = √(Je/J) against Lr = P/P_L.
  - API 579 Level 3 uses elastic-plastic FE with J.
  - Sources: ScienceDirect "Failure Assessment Diagrams" topic page; University of Plymouth FAD tutorial; OSTI ETDEWEB 21170575.
- Drive-index search (run 2026-09-24): all five indexes were unreachable from the Windows host. Card E05: a repeat from ace-linux-1 was blocked, because that checkout lacks the search script and the master index. No drive files were retrieved.

### Gaps identified
1. An FE crack-model generator (weldolet + root flaw), a CINT extraction parser, and a verification case with a published solution.
2. Edition-named FAD curves; a ray margin over primary loads only; an explicit Lr_max basis.
3. Growth from a tabulated ΔK(a): accurate quadrature, explicit threshold arrest, an explicit threshold temperature rule, and sensitivities.
4. Secondary stress with separate Φ and ρ methods, each fed by user input.
5. A `CrackAssessmentResult` behind the shared FFS consumer Protocol, with an evidence-completeness status (G11).
6. Citation overlay stubs (metadata only) and a durable-workflow route.
7. A self-contained house-style report.

### Evidence (embedded verification)
The frozen closed-form values below were computed in the main session on 2026-09-24 with `D:\ws\digitalmodel\.venv\Scripts\python.exe`. The inputs are generic, not taken from the published case.

## Artifact Map

| Phase | Artifact | Location |
|---|---|---|
| P0 | APDL generator: weldolet + root crack, CINT, linearisation paths | `src/digitalmodel/ansys/weldolet_crack.py` |
| P0 | APDL generator: verification plate with a semi-elliptical surface crack | `src/digitalmodel/ansys/crack_verification.py` |
| P0 | CINT output parser | `src/digitalmodel/ansys/cint_parser.py` |
| P0 | FE crack-state records (parsed, host-free) + run configuration | `examples/workflows/crack-fe-weldolet/fe_states/*.csv`, `run_config.json` |
| P1 | FAD curves, cut-off, margin | `src/digitalmodel/asset_integrity/assessment/fad_curves.py` |
| P1 | Growth law (user-supplied, basis-carrying), quadrature, threshold, sensitivities, history checks | `src/digitalmodel/fatigue/crack_growth_history.py` |
| P1 | Consistency checks (implied stress, SSY, shakedown, Lr > 1 coupling) | `src/digitalmodel/asset_integrity/assessment/crack_checks.py` |
| P2 | Secondary stress, Φ and ρ methods | `src/digitalmodel/asset_integrity/assessment/secondary_stress.py` |
| P3 | Coordinator → `CrackAssessmentResult` + shared FFS result Protocol (G11) | `src/digitalmodel/asset_integrity/assessment/crack_fe_assessment.py` |
| P3 | Citation getters + overlay stubs | `citations/registry.py`, `digitalmodel/knowledge/...` stubs |
| P3 | Workflow route | `engine.py`, `docs/registry/workflows.yaml`, `examples/workflows/crack-fe-weldolet/input.yml` |
| P4 | Report builder | `src/digitalmodel/asset_integrity/assessment/crack_fe_report.py` |
| P4 | Validation record | `docs/domains/asset-integrity/crack-fe-validation-record.md` |

## Deliverable
A phased `crack_fe_ffs` capability. It will build and solve our own FE crack model, extract K and J along the crack front at several depths, and assess each state on the API 579 Part 9 FAD. It will also integrate fatigue growth to the limit state and report sensitivities, consistency findings and evidence completeness. The output is a self-contained house-style report on an assumed design basis.

## Phase plan

### P0: Own FE model and K/J extraction
Gated by cards S02, S03, S05 and B12, plus follow-up cards listing the assumed geometry and material values. P0 is split into **P0a** (the verification deck, step 1) and **P0b** (the weldolet, steps 2–5). P0b starts only after P0a's receipt validates.

**Artifact gate (Codex r2 #1).**
- Every solved P0 state produces a committed, sanitised **verification receipt**, `examples/workflows/crack-fe-weldolet/fe_states/<state>.receipt.json`. It records:
  - the SHA-256 of the generated deck;
  - the producing commit;
  - the MAPDL version, argv, cores and platform;
  - the parsed K/J per front node and contour;
  - the solved reaction sums;
  - the result of each guard.
- A CI test that needs no licence shall:
  - validate every receipt against its schema;
  - regenerate each deck and compare its SHA-256 with the receipt, so a stale receipt fails;
  - fail if any guard is not `pass`.
- A missing receipt for a declared state **fails** the test; it does not skip. Only the solve itself needs the licensed host. Evidence of the solve is always checked.

1. **Verification before use.** `crack_verification.py` will generate a flat plate with a semi-elliptical surface crack under uniform tension (a = 2, c = 4, t = 10 mm, σ = 100 MPa). The CINT K at the deepest and surface points shall match the Newman–Raju (1981, NASA TM-83200, public domain) solution: 7.2896 and 5.7422 MPa√m from `crack_fad.newman_raju_k`, **within ±5 %**, which is the stated accuracy of the Newman–Raju fit. The weldolet model is built only after this passes.
2. **Weldolet model.** `weldolet_crack.py` will generate:
   - a 3-D SOLID187 model of the run pipe segment, weldolet body and weld, with a planar root flaw defined as a crack-front component;
   - CINT (SIF and J, contours 1–6) for each depth in a declared set (for example 2.35, 2.8, 3.2, 3.6 and 4.0 mm, up to ligament exhaustion);
   - internal pressure with the closed-end thrust applied as an equivalent end pressure, and crack-face pressure as a declared option;
   - PRSECT linearisation paths for σ_ref (B02).

   All geometry comes from the assumed basis (S03).
3. **Conservation guards, checked on solved results (Codex r2 #2).** The deck writes solved reactions (PRRSOL/FSUM) and CINT tables. The parser evaluates each guard independently from the solved output, never from prescribed loads:
   - (a) **Equilibrium:** the solved axial reaction sum on the constrained end equals p·π·r_i² within 0.5 %.
   - (b) **Mesh independence of load:** the solved reaction sums from two mesh densities agree within 0.1 %. This is checked **before** any cross-mesh K comparison.
   - (c) **Contour independence:** the spread across the last three contours is ≤ 3 % at every front node.
   - (d) **Extraction completeness:** every declared front node has K_I, K_II, K_III and J.
   - (e) **Sanitisation:** no host or user tokens appear.
   - (f) **Units:** declared mm–N–MPa, with K converted to MPa√m exactly once.

   Each guard has a **negative parser fixture** (field missing, value out of tolerance, stale mesh pair, host token present, wrong unit tag) that shall make that guard, and only that guard, fail.
4. **Runs** will go through the fail-closed `ansys/runner.py` on the licensed host. The run configuration is recorded with every state: MAPDL version, argv, cores, platform and producing commit.
   - Parsed CSVs shall carry no host or user fields, because MAPDL `.out` headers embed the host name.
   - `.rst` and `.out` files stay on the host.
**Stop rule and fallback gate (Codex r2 #6).** If the weldolet tet mesh fails guard (c) after two refinement steps (each halving the crack-front element size), P0b stops, and a board card goes to the owner. The owner decides between switching to a structured crack-block submodel and stopping. A submodel may proceed only with:
- displacement-driven boundaries from the solved global model;
- a transfer check: submodel uncracked stresses within 3 % of the global model at the cut-boundary sampling points;
- its own run of guards (a)–(f).

The report states the modelling route that produced every state.

5. **Optional within P0**, if S02 is chosen: one elastic-perfectly-plastic run to obtain the limit load P_L, used for the limit-load Lr sensitivity. The Option 3 curve (B01) goes to a follow-on issue.

### P1: FAD curves and crack growth
Gated by cards B05–B09, B14, B15, R01 and R02.
- `fad_curves.py`:
  - `api579_level2(Lr)` and a re-export of `crack_fad.fad_curve_option1` under an edition-neutral name until E06 confirms the edition.
  - `lr_max(rule="flow"|"fixed", basis=...)`, where a fixed value requires a basis string.
  - `envelope_margin` scales **primary terms only** (Lr, K_P). K_S is held constant, which resolves Claude r1 #5.
- `crack_growth_history.py`:
  - `GrowthLaw` objects are built from user inputs (A, m, units, basis, citation), with no built-in standard constants (G05).
  - An E-ratio correction, plus `threshold_temperature_rule` ∈ {"none", "e_ratio"}, which is **required** and has no default (B14).
  - Life is computed as a composite-Simpson quadrature of dN/da = 1/(A·ΔK(a)^m), with the number of intervals fixed so that the error against the closed form is ≤ 1e-7 relative.
  - A tabulated ΔK(a) interpolator whose extrapolation policy raises by default.
  - `ARRESTED(a_arrest)` is returned whenever ΔK ≤ ΔK_th at any depth, including a crossing inside the interval; a finite life is never truncated silently.
  - Sensitivities: the closed-form multiplier (N/N_d)^(1/m) only when there is no threshold; otherwise a numerical root find.
  - `back_calculate_dk(history)` and `cycles_to_extension(history)` for automated-growth histories.
- `crack_checks.py`:
  - the implied crack-opening stress against σ_ref, with the tolerance band fixed here at 0.5–2.0;
  - SSY r_p against the ligament;
  - shakedown, as elastic range against 2σy, with the range taken from our own FE;
  - **Lr > 1 at any state marks the growth result CONDITIONAL** (B15).
- `crack_fad.py`: the docstring correction (R01) and the level3_escalation pointer are text-only changes.

### P2: Secondary and residual stress
Gated by cards B10, B11, B13, G01, G02 and G03.
- Two separate, named methods resolve Claude r1 #4 and Codex r1 #3:
  - `kr_api_phi(K_P, K_S, Kmat, phi)`, with Φ ≥ 1 supplied by the user with a basis;
  - `kr_bs7910_rho(K_P, K_S, Kmat, rho)`, with ρ ≥ 0 supplied by the user with a basis.
- Neither method has a built-in default, and no computed Φ or ρ ships (G05).
- Residual profiles are uniform or user polynomial. A relaxation factor is a user input.
- Screening bounds are labelled `screening` and are never presented as a benchmark verdict.

### P3: Coordinator, workflow and citations
Gated by cards E01–E04, R04, R05 and G09.
- **The result record is a shared API migration (Codex r2 #4). Owner card G11 was decided on 2026-09-24 (sibling_protocol).** The shape is a sibling `CrackAssessmentResult` that implements the consumer contract `FFSAssessmentResult` already exposes: `component_id`, `assessment_type`, `verdict`, `passes`, `to_dict()`, `code_reference`.
  - `FFSAssessmentResult` and its required metal-loss fields stay **unchanged**.
  - Consumers that accept either type are typed against a `Protocol`.
  - Before any change, characterisation tests shall instantiate and serialise the existing GML, LML and PITTING paths.
- **Required for `evidence_status = COMPLETE` (Codex r1 #10, Codex r2 #5):**
  - a Kmat basis;
  - an Lr_max basis (σu or a fixed-value basis);
  - a residual-stress basis;
  - a PSF basis;
  - geometry validity (declared limits met);
  - **for every FE state, a validated receipt whose guards (a)–(f) all pass and whose deck hash matches the current generator at the producing commit.**
- `passes` is `False` whenever `evidence_status != COMPLETE`. A test asserts this for `assessment_type = "CRACK"`.
- Citation overlay stubs will carry metadata only (code_id, publisher, revision) after E01/E02 settle the identifiers and revision strings. Nothing in `asset_integrity` emits a citation before then.
- The engine branch `crack_fe_ffs`, the registry row with declared outputs, and `examples/workflows/crack-fe-weldolet/input.yml`, whose design basis is labelled *assumed*.

### P4: Report
Gated by card R06 (recommended: CalcReport).
- The report is built on `CalcReport` with the house-style skeleton:
  - executive summary with the governing numbers;
  - design basis (assumed) and its assumptions register;
  - acceptance criteria;
  - methodology;
  - FE model and verification;
  - results by state;
  - conclusions separated from recommendations;
  - references to cited laws and procedures only.
- **No reference to the published case.**
- FAD and growth figures are inline SVG without clipPath, patterns or filters (per `svg-pdf-portability.md`). Captions go below each object.
- Table values are emitted as `data-src` attributes that point into the result record. The trace test checks those cells only, which resolves Codex r1 #7.
- The report shall pass the register lint and include no external resources. It is generated into `D:\ws\output\crack-fe-weldolet\` and committed only after owner approval (G04).

## Rights matrix (Codex r2 #3)

Every function the plan commits to the public repository falls into one of four classes:
- **PD:** public domain, or general textbook fracture mechanics;
- **RC:** already existing repository code;
- **UI:** a user-input-only wrapper that carries no standard-derived values;
- **PS:** derived from a private standard and not committable.

Owner card G10 was decided on 2026-09-24 (commit_both): both owner rows are committable.

| Function / content | Class | Basis |
|---|---|---|
| Paris–Erdogan integration, √a closed form, Simpson quadrature | PD | Textbook fracture mechanics |
| Irwin plastic-zone size; energy-equivalent Keq | PD | Textbook LEFM |
| Newman–Raju surface-flaw K (verification comparator) | RC + PD | `crack_fad.newman_raju_k`; NASA TM-83200 (US Government work) |
| Centre-crack secant (Feddersen) K | PD | Textbook handbook solution |
| BS 7910 Option 1 curve | RC | Already in `crack_fad.fad_curve_option1` |
| API 579 Level 2 curve form (1−0.14Lr²)(0.3+0.7e^(−0.65Lr⁶)) | **committable (owner G10, 2026-09-24)** | Also published as the original R6 Option 1 curve in the open literature |
| Φ and ρ | UI | Values supplied by the user with a basis; no computed Φ/ρ procedure ships |
| Residual-stress profiles | UI | Uniform or polynomial user input; no Annex 9D content |
| Threshold temperature rule (E-ratio) | UI + PD | The rule is a user choice; the E-ratio scaling is textbook |
| Growth-law constants, PSFs, E(T), σy, σu, Kmat | UI | `input.yml` only, each with a basis string |
| Citation metadata (code_id, publisher, revision) | **committable (owner G10, 2026-09-24)** | Bibliographic facts only; no clause text |
| APDL decks for the weldolet and verification plate | PD | Our own model; assumed geometry |

**Checklist criterion.** Each phase's code review ticks every committed function against this matrix. It also greps the diff for clause-numbered text blocks and for numeric tables of more than 3 rows that carry no `input.yml` provenance.

## Pseudocode (P1 growth core)

```text
life(law, dk_of_a, a0, af, dKth, rule):
  dKth_eff = dKth * (E_T/E_ref) if rule == "e_ratio" else dKth    # rule required
  grid = composite_simpson_nodes(a0, af, n)                        # n fixed for <=1e-7 rel
  if any(dk_of_a(a) <= dKth_eff for a in dense_scan(a0, af)):
      return ARRESTED(first_crossing_depth)
  return simpson(lambda a: 1/(law.A_eff * dk_of_a(a)**law.m), grid)
```

## Files to Change

| Phase | Action | File |
|---|---|---|
| P0 | Create | `src/digitalmodel/ansys/crack_verification.py`, `weldolet_crack.py`, `cint_parser.py` |
| P0 | Create | `tests/ansys/test_crack_verification.py`, `test_weldolet_crack.py`, `test_cint_parser.py` |
| P0 | Create | `examples/workflows/crack-fe-weldolet/fe_states/` (parsed CSV), `run_config.json` |
| P1 | Create | `src/digitalmodel/asset_integrity/assessment/fad_curves.py`, `crack_checks.py`; `src/digitalmodel/fatigue/crack_growth_history.py` |
| P1 | Create | `tests/asset_integrity/test_fad_curves.py`, `test_crack_checks.py`; `tests/fatigue/test_crack_growth_history.py` |
| P1 | Modify | `crack_fad.py` (docstring), `level3_escalation.py` (text), `assessment/__init__.py` (exports) |
| P2 | Create | `src/digitalmodel/asset_integrity/assessment/secondary_stress.py`, `tests/asset_integrity/test_secondary_stress.py` |
| P3 | Create/Modify | `crack_fe_assessment.py` (`CrackAssessmentResult` + Protocol, per G11); `ffs_coordinator.py` unchanged except the Protocol type; `citations/registry.py`; overlay stubs; `engine.py`; `docs/registry/workflows.yaml`; `examples/workflows/crack-fe-weldolet/input.yml`; tests incl. consumer characterisation |
| P4 | Create/Modify | `crack_fe_report.py`; `reporting/calc_report.py` (optional `ResultBlock.figure_svg`); `tests/asset_integrity/test_crack_fe_report.py`; validation record |

## TDD Test List (frozen values; comparator class in brackets)

**P0**
- `test_verification_receipt_newman_raju_deepest` [published closed-form fit, CI, never skips]: the committed P0a receipt shows the deepest-point K = 7.2896 MPa√m ± 5 %, using a = 2, c = 4, t = 10 and σ_m = 100. The test **fails** when the receipt is absent, when its deck hash is stale, or when any guard failed. The licensed solve that produces the receipt is a separate `@licensed` run (`test_solve_verification_deck`), and it is the only test that may skip on an unlicensed host.
- `test_verification_receipt_newman_raju_surface` [published closed-form fit, CI, never skips]: surface-point K = 5.7422 ± 5 % from the same receipt.
- `test_centre_crack_secant` [closed-form, used for CINT parser sanity]: K = 12.6110 MPa√m, from a = 5 mm, W = 100 mm and σ = 100 MPa.
- `test_reaction_sum_equals_end_thrust` [conservation]: within 0.5 %.
- `test_solved_reaction_mesh_independent` [conservation]: the two solved reaction sums (parsed PRRSOL/FSUM output) from two mesh densities agree within 0.1 %. The stale-mesh-pair negative fixture fails this guard alone.
- `test_contour_independence` [invariant]: the last three contours agree within 3 %.
- `test_cint_parser_roundtrip` and `test_parsed_csv_has_no_host_fields`.

**P1**
- `test_api579_level2_values` [closed-form evaluation]: 1.0 at Lr = 0; 0.9581741742, 0.5722715576 and 0.2057919530 at Lr = 0.5, 1.0 and 1.5 (each ± 1e-9).
- `test_implemented_option1_differs_from_level2` [closed-form comparison of the two implemented formulas]: this stays valid whatever the edition attribution turns out to be.
- `test_lr_max_fixed_requires_basis` and `test_lr_max_flow_rule` [closed-form]: (300 + 500)/600 = 1.3333333333.
- `test_envelope_margin_curve_contact` [closed-form]: at (0.8, 0.5) with cut-off 1.8, F = 1.2174353 ± 1e-6 at a curve contact; the contact point lies on the curve to 1e-9.
- `test_envelope_margin_cutoff` [closed-form]: at (1.2, 0.05) with cut-off 1.5, F = 1.25 exactly.
- `test_envelope_margin_holds_secondary_constant` [invariant]: with K_S > 0 the ray does not pass through the origin, and F is computed on primary terms only.
- `test_life_sqrt_closed_form` [closed-form]: A = 2e-8, m = 3, ΔK₀ = 5, a₀ = 1, a_f = 3 → N = 338,119.784648 ± 1e-7 relative.
- `test_life_constant_dk` [closed-form]: N = 800,000 exactly (to 1e-9).
- `test_life_scales_as_k_power_minus_m` [invariant, no threshold].
- `test_threshold_crossing_inside_interval_returns_arrested` and `test_below_threshold_everywhere_returns_arrested` [invariant].
- `test_threshold_rule_required` [contract].
- `test_multiplier_closed_form_only_without_threshold` [contract + closed-form].
- `test_tabulated_extrapolation_raises_by_default` [contract].
- `test_back_calculated_dk` and `test_cycles_to_extension` [closed-form on a synthetic history built from a known law].
- `test_implied_stress_ratio`, `test_ssy_ratio`, `test_shakedown_ratio` [closed-form on generic inputs].
- `test_lr_above_one_marks_growth_conditional` [invariant].
- `test_unit_conversion_n_mm_to_mpa_sqrt_m` [closed-form]: 5.21e-13 · 1000^1.5 = 1.647547e-8. This tests the conversion only; the constant is not shipped.

**P2**
- `test_phi_method_requires_basis`, `test_phi_at_least_one`, `test_rho_non_negative`, `test_methods_not_interchangeable` [contract].
- `test_uniform_secondary_k` [closed-form]: 1.12·σ·√(πa) on generic inputs.
- `test_kr_monotone_in_residual` [invariant].
- `test_screening_bound_labelled` [contract].

**P3**
- `test_existing_consumers_unchanged` [characterisation, written first].
- `test_crack_incomplete_evidence_never_passes` [invariant].
- `test_complete_evidence_requires_all_bases` [contract].
- `test_citations_resolve_or_fail_closed` [contract].
- The durable-workflow test covers `examples/workflows/crack-fe-weldolet/input.yml`, and the declared outputs exist.

**P4**
- `test_report_table_cells_trace_to_result` [conservation of values].
- `test_report_register_lint` (skipped with a reason when the workspace-hub sibling checkout is absent).
- `test_report_no_external_resources`, `test_report_svg_portable` (no clipPath, pattern, filter or mask).
- `test_report_captions_below`, `test_report_skeleton_sections`, `test_report_does_not_reference_source`.

## Acceptance Criteria
- [ ] Each phase passes `uv run pytest` on its test folders with no regression, and passes the CI lint toolchain at the `uv.lock` versions.
- [ ] P0: the verification deck matches Newman–Raju within ±5 % on a licensed run. Every declared state has a committed receipt whose guards (a)–(f) pass and whose deck hash matches the generator. The receipt-validation test fails, rather than skips, when a receipt is missing or stale. Each guard's negative fixture fails that guard alone.
- [ ] P1: all frozen closed-form values reproduce within the stated tolerances. No finite life is returned through a threshold crossing.
- [ ] P3: a crack result with any missing basis carries `evidence_status = INCOMPLETE` and `passes == False`.
- [ ] P4: one command regenerates the report from `input.yml` and the FE state records. It passes the register lint and the SVG portability test, contains no reference to the published case, and states its disposition with criterion, comparator and governing case.
- [ ] Rights: the repository contains no standard-derived table, clause text or worked example. Every standard-derived number appears only in `input.yml` with a basis string. Each phase's code review checks this against a checklist.
- [ ] Every validation claim names its comparator class, and none rests on `archived-run` alone.

## Adversarial Review Summary (r1 → r2)

| # | Finding | Resolution in r2 |
|---|---|---|
| Codex 1 | Option 1 vs Level 2 premise disputed | The test compares the two implemented formulas; the edition attribution waits on E06 |
| Codex 2 / Claude 1 | Level 2 expected value wrong | Values frozen at generic Lr with 1e-9 tolerance |
| Codex 3 / Claude 4 | Φ/ρ mixed and not controlled | Two named methods, both user-supplied with a basis (G05) |
| Codex 4 / Claude 4 | Residual stress over-claimed | Screening-labelled bounds, relaxation input, P2 gated by B11/G02 |
| Codex 5 / Claude 6 | Threshold and integrator under-specified | Simpson quadrature, ARRESTED on crossing, required temperature rule, closed-form multiplier only without threshold |
| Codex 6 | Constant-ΔK value ambiguous | Generic frozen inputs; exact value 800,000 |
| Codex 7 / Claude 3 | Comparator labels wrong; trace test infeasible | S04: closed-form and conservation only; trace via `data-src` cells |
| Codex 8 / Claude 9 | Licensing in a public repo | G05: standard-derived values only as basis-carrying inputs; checklist criterion |
| Codex 9 | Duplication | Re-export only; the consolidation issue is filed at P1 start (R02/R03) |
| Codex 10 | Missing required inputs before ACCEPT | P3 evidence-completeness rules |
| Claude 2 | σu back-solved | No derived σu anywhere; Lr_max requires σu or a fixed-value basis |
| Claude 5 | Ray margin wrong with secondary stress | Primary-only scaling plus a dedicated test |
| Claude 7 | Citations fail closed in CI | Metadata-only overlay stubs after E01/E02; no emission before P3 |
| Claude 8 | Architecture departure | G08, superseded by G11: sibling `CrackAssessmentResult` behind a shared Protocol; `FFSAssessmentResult` unchanged; consumer characterisation first |
| Claude 10 | Factual and minor gaps | Line count corrected (1,223); tolerances frozen; registry outputs declared in P3 |
| Claude 11 | Phasing | G07: P0–P4 with a code review per phase |

**Codex r2 (MAJOR: 6 major, 2 minor), resolved in r2.1 as inline patches.** Artifact: `workspace-hub/scripts/review/results/2026-09-24-digitalmodel-2157-codex-plan-review-r2.md`.

| # | Finding | Resolution in r2.1 |
|---|---|---|
| r2-1 | FE verification bypassable by skips | Committed receipts; a CI schema and deck-hash check; a missing receipt fails |
| r2-2 | Guards not on solved results; no negative fixtures | Guards (a)–(f) on solved output, each with a negative fixture |
| r2-3 | Rights for equations and metadata | Rights matrix, owner card G10, checklist criterion |
| r2-4 | Result change not additive | Sibling `CrackAssessmentResult` behind a Protocol; `FFSAssessmentResult` unchanged; owner card G11 |
| r2-5 | COMPLETE omits guard outcomes | COMPLETE requires validated receipts with all guards passing at the producing commit |
| r2-6 | No stop rule for the FE fallback | Two-refinement stop rule, owner card, submodel transfer check |
| r2-7 | Consolidation only deferred | Filed as [digitalmodel#2160](https://github.com/vamseeachanta/digitalmodel/issues/2160) |
| r2-8 | Frozen arithmetic | Confirmed correct by Codex |

**Codex r2.1 confirming pass (MINOR), patched in r2.2.** Artifact: `…/2026-09-24-digitalmodel-2157-codex-plan-review-r2.1.md`. All r2 fixes were confirmed concrete, testable and able to fail. Three stale lines were rewritten:
- the P0 Newman–Raju tests now validate receipts in CI and never skip;
- the mesh test now compares solved reaction sums;
- the r1-era `FFSAssessmentResult` wording is replaced by G11 (artifact map, the Claude-8 row, and risk R4).

Gemini remains unavailable on this host, so the review stays at T2.

**Licensed-source checks (housekeeping card E06, 2026-09-24, read-only on ace-linux-1):**
- The API 579-1/ASME FFS-1 2016 PDF is present at its licensed location. It is protected by FileOpen DRM, so text extraction is refused; the Level 2 equation needs an owner check in an authorised viewer.
- BS 7910:2013 is present, and its Option 1 is confirmed to be the (1 + 0.5Lr²)^-0.5 form that `crack_fad` implements.
- No BS 7910:2019 copy is on record.
- The recorded RP 579 (2000) path is missing.
- E05, the drive-index search, is blocked: the ace-linux-1 `workspace-hub` checkout lacks the search script and the master index.

## Risks and Open Questions
- **R1 – CINT on a weldolet tet mesh.** Crack-front meshing in pure APDL may prove difficult. Mitigation: the verification deck proves the extraction first. If the weldolet mesh fails the contour guard, a structured crack-block submodel will be used instead, and the report will state the change.
- **R2 – live solver tree.** Implementation will run in a worktree off `origin/main`, because the local `digitalmodel` checkout is the licensed-run agent's editable install.
- **R3 – host leakage.** MAPDL `.out`/`.inp` echoes can embed host and user names. The parser strips them, and a test asserts their absence.
- **R4 – Protocol compatibility.** Consumers typed against the new shared Protocol could rely on metal-loss attributes that `CrackAssessmentResult` lacks. The characterisation tests of existing consumers are written first, and any consumer that reads metal-loss fields keeps its `FFSAssessmentResult` type.
- **R5 – assumed basis.** Every result is only as good as the assumed geometry and material (S03/S05). The report states the assumptions beside the inputs they affect and repeats the governing ones in the summary.
- **Q-P0.** Assumed geometry and material values will be listed on follow-up cards for owner confirmation before P0 starts.

## Phase card decisions (owner save 2026-09-25T11:04Z, `crack-fe-2157-decisions (4).json`)

**P0 cards**
- S02 (both): the FE-linearised σ_ref governs, and a limit-load Lr is reported as a sensitivity.
- S03 / V02 (catalogue_plus): public manufacturer catalogue geometry. For a 6 × ½ STD weldolet, A = 19.05, B = 34.93 and C = 23.81 mm. The branch is NPS ½ Sch 40S. Bore = branch ID 15.80 mm, bevel 45°, root gap 1.6 mm and cover-fillet leg 3 mm are engineering assumptions.
- V01 (s40s): the run pipe is NPS 6 Sch 40S (OD 168.3 mm, wall 7.11 mm).
- V03 (both): a full-circumference root flaw is the base case, with a crotch arc (a/2c = 0.25) as a sensitivity, both from a₀ = 2.35 mm.
- S05 / V04 (datasheet), taken from public datasheets:
  - Rp0.2 = 127 MPa (EN 10088-3 minimum via the Acidur 4404 datasheet);
  - E(250 °C) = 182.5 GPa (interpolated);
  - ν = 0.3;
  - tensile strength at 250 °C ≈ 385 MPa (interpolated);
  - Ramberg–Osgood n = 7.

  Kmat = 132 MPa√m has no public source and is listed as missing evidence.
- B12 (assume_basis): geometry is an assumed design basis.

**P1 cards**
- R01 (fix): edition-named curves, with the `crack_fad` docstring correction.
- R02 (closed_form): closed-form comparators.
- B09 (cite): E(T) is taken from a cited datasheet.
- B14 (state_both): the threshold temperature rule is explicit, and both margins are reported.
- B15 (flag): Lr > 1 marks the growth result CONDITIONAL.
- **B05–B08 (drop; this differs from the recommendation):** none of the published case's result values are used anywhere, including an internal cross-check. The analysis and report are self-contained, built on our own FE work and cited laws only.

**Design-data convention (owner note on S05).** Every assumed input carries the riser-report convention. The input is `status_label: "ASSUMED - to be confirmed"`, with a note in the form "ASSUMED - to be confirmed: \<reason\>. Confirm with \<evidence\>." The inputs are held in a design-data register, `examples/workflows/crack-fe-weldolet/design-data-register.json`, which mirrors the riser register schema: id, parameter, value, unit, source_class, reference_ids, note, status_label.

**Databases (owner note on V04 and B09).** Reusable inputs and outputs are proposed as tables in the existing `llm-wiki/data/domain-database-index.yml` domains (`materials-standards`, `structural-ffs`), on board cards D01–D04. Standard-derived values stay out of the public repository, per G05.

Public sources for the P0 values are backed up at `/mnt/ace/docs/literature/materials/stainless-steel-1.4404/` and `/mnt/ace/docs/literature/piping/fittings-and-pipe-dimensions/`, each with a `SOURCES.md`.

## Database decisions (owner save 2026-09-25T14:02Z, `crack-fe-2157-decisions (5).json`)

- **D01 (public; this differs from the recommendation):** elevated-temperature material properties are published in `data/materials_database/`, each row cited to the public manufacturer datasheet it was read from. Only the values used are included, and no standard table is reproduced. The owner's rights decision is recorded in the manifest. This was implemented in `38ebad0a`: 22 rows for 1.4404, with a hash-checked manifest and temperature-monotonic invariants.
- **D02 (awaiting confirmation):** the owner selected the private catalogue but noted "why not public then". The recommended option is public with per-row citation, the same rule as D01. The table is built after confirmation.
- **D03 (repo ecosystem only, from the owner's note "no HF flow"):** crack-front FE state tables live in `digitalmodel` `data/` with a manifest, registered in the domain-database index. They are not published to Hugging Face.
- **D04 (extend structural-ffs):** result lookups are generated from `CrackAssessmentResult` in P3.

## P0b stop-rule decisions (owner save 2026-09-25T16:30Z, `crack-fe-2157-decisions (6).json`)

P0b stopped at a₀ = 2.35 mm on the full-circumference fusion-face flaw. Evidence is merged in `08176d3e` under `fe_states/stop_rule_evidence/`. The definitions below are fixed **before** any further FE run.

- **G13, guard (c) redefined.** Contour independence is evaluated as:
  - (i) the K spread over the last three contours at each front node, normalised by the maximum |K| along the whole front, ≤ 3 %; **and**
  - (ii) the J spread over the last three contours at each front node, normalised by that node's mean J, ≤ 3 %.

  The tolerance is unchanged. The reason for the change is that the previous metric, (max − min)/|mean| per node, is undefined where K_I passes through zero on the front, which is a physical feature that no refinement or submodel removes. The previous definition and the failing evidence stay in the record.
- **G14, governing K.** The governing crack driving force is K = √(E′ J), with E′ = E/(1 − ν²) and J from the converged contours. The mode mix (K_I, K_II, K_III) comes from the interaction integral and is reported alongside. A new mesh-convergence guard (g) is added: J at the governing node changes by ≤ 1 % between the two finest meshes.
- **G15, second crack plane.** A root flaw in the radial–axial plane at the crotch, normal to the run-pipe hoop stress, is added as a second base case, with the same a₀ and depth states. The governing case at each depth is the worse of the two planes. The fusion-face plane gives a crotch K of about 1.39 MPa√m, below ΔK_th, so it predicts no growth. Treating that as the answer without the hoop-opened plane could be non-conservative. That the hoop-opened plane governs is a hypothesis until it is modelled.
- **D02:** the owner kept the private-catalogue selection in two saves. The fitting-dimension table is not built as public and waits for the owner.

## Guard (g) at free-surface end nodes (owner card G16, save 2026-09-25T18:47Z)

Fixed before any further FE run. For fronts that end on a free surface:
- guard (g) (J mesh convergence ≤ 1 % between the two finest meshes) is evaluated over the interior front nodes only;
- the governing K_gov is still the maximum over **all** front nodes, including the free-surface end nodes.

The reason is that free-surface end nodes carry a non-square-root singularity, so their extracted J does not converge like the interior field. On the crotch plane at a₀ it oscillated +1.28 % then −1.10 % while every interior node converged within 0.04 %. Taking the end-node value as governing keeps the result conservative: 7.57 MPa√m against an interior maximum of 7.16 MPa√m. Each receipt reports the end-node J spread for the record. Closed fronts (the fusion-face ring) have no end nodes and are unaffected.

## Complexity: T3
