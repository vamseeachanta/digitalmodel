# Plan: digitalmodel #581 — API RP 2A WSD (structural)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/581
**Status:** plan-review
**Tier:** T3 (large standard, but partial implementation already exists; this PR closes the highest-value gaps)

## Context
- **Standard / publisher / revision.** API RP 2A-WSD, 22nd Edition (Nov 2014), Reaffirmed 2025. Title: *Planning, Designing, and Constructing Fixed Offshore Platforms — Working Stress Design*. Wiki page already present at `knowledge/wikis/engineering-standards/wiki/standards/api-rp-2a-wsd.md` (workspace-hub).
- **Why it matters.** Partial implementation exists today: `src/digitalmodel/structural/jacket_topside/joint_checks.py` (Section 4.3 Tubular Joints — punching shear) and `member_checks.py` (Sections 3.2/3.3/3.4/3.5 tubular member capacity). Citation contract is **not** wired (the docstring names API RP 2A-WSD without a `Citation` emission). Several headline calc surfaces remain GAPs: §2.3 environmental loading factors, §6 fatigue (cross-link to RP-2A-WSD §17 / RP 2A-FAD), §6.2 foundations, §17 in-service inspection (handed off to API RP 2SIM in #582 territory). This PR closes the **citation gap** plus adds the **highest-leverage missing calc surface: Section 5 Strength of Tubular Joints (Hot-Spot SCF + the SCF cross-walk to fatigue)**.
- **Dependencies on other issues.** **Resolves duplicate ambiguity with `OGManufacturing#35` (= old WRK-540) which was filed against `OGManufacturing/structural` for the same standard.** Recommendation: **digitalmodel is the canonical home** because (i) `structural/jacket_topside/` already implements the WSD member + joint checks, (ii) `structural/fatigue/` already implements RP-C203 / RP 2A-FAD-style S-N machinery, and (iii) OGManufacturing's historical scope is fabrication / manufacturing, not in-service structural design. Action required outside this PR: post a comment on `OGManufacturing#35` proposing closure as duplicate-of-this. Do **not** transfer or close in this plan; that is execution.
- Soft dependency: a `Citation` plumbing already exists via `src/digitalmodel/citations/schema.py`; the wiki page is already present.

## Scope
- **Citation plumbing for the existing module surface:**
  - `src/digitalmodel/structural/jacket_topside/member_checks.py` — emit `Citation(code_id="api-rp-2a-wsd", clause="§3.2|§3.3|§3.4|§3.5", revision="22e-2014-r2025", ...)` from each unity-check function.
  - `src/digitalmodel/structural/jacket_topside/joint_checks.py` — emit `Citation` for §4.3 punching-shear formula.
- **New calc surface: Section 5 Hot-Spot SCF and fatigue SCF cross-walk:**
  - `src/digitalmodel/structural/jacket_topside/joint_scf.py` — Stress Concentration Factor formulas for T/Y, K, X joints using the **Efthymiou parametric equations** referenced by API RP 2A-WSD Section 5.5 (commentary). Returns SCF separately for chord saddle, chord crown, brace saddle, brace crown — the four hot-spots required for RP-C203 / RP 2A-FAD fatigue cross-walk.
  - `src/digitalmodel/structural/jacket_topside/fatigue_crosswalk.py` — `joint_fatigue_design_check(joint, sea_state_history)` composes the new SCF module with `src/digitalmodel/structural/fatigue/sn_curves.py` to produce a §17 fatigue check.
- **API §6 Fatigue (S-N curves cross-walk):** No re-implementation; `src/digitalmodel/structural/fatigue/sn_curves.py` already covers RP-C203 curves which RP 2A §17 references. Add a mapping table `API_RP_2A_TO_RP_C203_SN_MAPPING` in `fatigue_crosswalk.py`.
- `tests/structural/jacket_topside/test_joint_scf.py` — Efthymiou validation against the canonical T-joint example in the OTC paper.
- `tests/structural/jacket_topside/test_fatigue_crosswalk.py` — joint-level fatigue test with synthetic sea-state.
- `tests/structural/jacket_topside/test_citation_emission.py` — asserts every public function in member_checks.py and joint_checks.py emits a resolvable Citation.
- **Non-goals.** §2 Loads (already covered by `orcaflex/environment.py` + OCIMF wind loading; cross-reference only). §6 Foundation design (separate module — pile capacity is `geotechnical/`, out of scope here). §17 in-service inspection (deferred to #582 API RP 2I). §16 Reuse (out of scope). LRFD edition (RP 2A-LRFD) — explicitly WSD only this PR.

## Deliverables
- `src/digitalmodel/structural/jacket_topside/member_checks.py` — Citation emission added
- `src/digitalmodel/structural/jacket_topside/joint_checks.py` — Citation emission added
- `src/digitalmodel/structural/jacket_topside/joint_scf.py` — new module (Efthymiou SCF)
- `src/digitalmodel/structural/jacket_topside/fatigue_crosswalk.py` — new module (RP 2A § 17 → RP-C203 mapping)
- `src/digitalmodel/structural/jacket_topside/__init__.py` — export new symbols
- `tests/structural/jacket_topside/test_joint_scf.py`
- `tests/structural/jacket_topside/test_fatigue_crosswalk.py`
- `tests/structural/jacket_topside/test_citation_emission.py`
- `docs/domains/articles/api_rp_2a_wsd_coverage_map.md` — explicit coverage map: which clauses are now in code, which are still gaps, where each gap is tracked.

## Approach
1. **Survey existing implementation.** `grep -n 'Section\|§' src/digitalmodel/structural/jacket_topside/*.py` to lock the existing clause-coverage map. Smoke check: existing tests still pass via `uv run pytest tests/structural/jacket_topside/ -q`.
2. **Citation plumbing.** Wire `Citation` into every public function in `member_checks.py` and `joint_checks.py`. Smoke check: `cd digitalmodel && uv run pytest tests/structural/jacket_topside/test_citation_emission.py -q`.
3. **Efthymiou SCF.** Implement chord/brace saddle/crown formulas with the parametric ranges (β, γ, τ, α) from the OTC 5666 paper. Smoke check: `cd digitalmodel && uv run pytest tests/structural/jacket_topside/test_joint_scf.py -q`.
4. **Fatigue cross-walk.** Compose SCF + RP-C203 curves; the mapping table for §17 → RP-C203 categories should be a Python dict. Smoke check: `cd digitalmodel && uv run pytest tests/structural/jacket_topside/test_fatigue_crosswalk.py -q`.
5. **Coverage map doc.** Write `api_rp_2a_wsd_coverage_map.md` with three columns: clause / module / status (DONE / PARTIAL / GAP / out-of-scope). This is the deliverable that an executor can check off against.
6. **Cross-repo coordination.** Comment on `OGManufacturing#35` recommending closure as duplicate-of-#581 (do not close — that is operator action).

## Open questions
- **Efthymiou vs Lloyd's-Register SCF.** RP 2A §5.5 cites Efthymiou as the primary set; some operators specify the Lloyd's Register formulation. Default: Efthymiou with hooks for an alternative. **Needs user input** if Lloyd's is contractually required.
- **Reaffirmation status.** Wiki frontmatter says revision `22e-2014-r2025`. Confirm we should target the 2014 22nd Ed. text rather than waiting for a 23rd Ed. release.
- **OGManufacturing duplicate resolution.** Confirm digitalmodel is canonical and OGManufacturing#35 should be closed as duplicate. **Needs user input.**
- **Test fixture license.** Efthymiou OTC paper Examples 1–4 are publicly distributed in the proceedings; CSV transcription is acceptable. Confirm.

## Acceptance Criteria
- [ ] Every public function in `member_checks.py` and `joint_checks.py` emits a `Citation` that resolves against `api-rp-2a-wsd.md`.
- [ ] `joint_scf.py` implements Efthymiou SCF for T/Y, K, X joints with chord/brace × saddle/crown breakdown; validation test passes against ≥1 published example within ±10%.
- [ ] `fatigue_crosswalk.py` produces a fatigue damage scalar from a synthetic sea-state for at least one joint type.
- [ ] `docs/domains/articles/api_rp_2a_wsd_coverage_map.md` enumerates every Section of RP 2A-WSD and labels DONE / PARTIAL / GAP.
- [ ] Comment on `OGManufacturing#35` proposing duplicate closure (manual operator action; flagged in plan).
- [ ] Full module suite green: `cd digitalmodel && uv run pytest tests/structural/jacket_topside/ -q`.
