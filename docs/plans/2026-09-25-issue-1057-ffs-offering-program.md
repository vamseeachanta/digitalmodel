# Plan for #1057: Fitness-For-Service Offering Program

**Issue:** [#1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) (epic), Phase 4 and readiness demonstrators
**Date:** 2026-09-25 · **Revision:** r1.3 (r1.2 plus the owner's decisions D1, D2, D4, D5, D6 recorded on #1057; D3 resolved on secondary sources, see *Owner Decisions*)
**Status:** owner chose *Approve r1.2 as is* on the decision page, and also chose D6 = wait for the Gemini artifact. The label `status:plan-approved` is not applied until the owner resolves that conflict; this plan never self-applies it.
**Evidence locality:** the program note and readiness review cited below are in this branch (cherry-picked from PR #2186, which is superseded by this PR). The #2157 crack FE plan is external (PR #2194, open). Deckhand `report_url` is a target contract (deckhand #498/#499), not current behaviour.
**Objective (owner's words):** be able to offer fitness-for-service (FFS) services for a wide variety of assets via available codes and standards.

## Resource Intelligence Summary

### Existing Repo Code

| Area | What exists | Where |
|---|---|---|
| Metal-loss FFS | Canonical L1/L2 coordinator (GML/LML/pitting router, decision, report, sufficiency), legacy API579 engine | `src/digitalmodel/asset_integrity/assessment/*`, `API579.py` |
| Pipeline strength | B31G / Mod B31G / RSTRENG, RSTRENG-2D, DNV-RP-F101, circumferential | `corroded_pipe.py`, `rstreng_2d.py`, `dnv_rp_f101.py`, `circumferential_defect.py` |
| Other mechanisms | Pitting (Pt 6), dents (Pt 12), crack FAD (BS 7910 Opt 1), Paris growth, Level 3 handoff | `assessment/pitting.py`, `dent_assessment.py`, `assessment/crack_fad.py`, `fatigue/crack_growth.py`, `assessment/level3_escalation.py` |
| Decision layer | `ffs_decision` (pressure-equipment vocabulary: ACCEPT / MONITOR / RE_RATE / REPAIR / REPLACE, inch/psi), RBI screen, composite repair selector, inspection planning, acceptance curves + lookup | `assessment/ffs_decision.py`, `rbi_screening.py`, `composite_repair.py`, `inspection_planning.py`, `ffs_acceptance_curves.py`, `ffs_lookup.py` |
| Asset engines reusable for FFS | Plate/panel buckling (DNV-RP-C201), hull girder screening, jacket member/joint checks, mooring screening (GREEN/AMBER/RED) + fatigue, synthetic-rope fatigue, riser fatigue, S-N library (221 curves), scour, SHM templates | `infrastructure/base_solvers/structural`, `hull_girder_screening`, `structural/jacket_topside`, `mooring_resilience`, `mooring_fatigue`, `synthetic_rope_mooring_fatigue`, `riser_fatigue`, `structural/fatigue`, `geotechnical/scour.py`, `structural/offshore_resilience` |
| Solvers | gmsh meshing, CalculiX linear static chain, ANSYS APDL generators (licensed host), in-house beam eigen-buckling | `solvers/gmsh_meshing`, `solvers/calculix`, `ansys/`, `structural/fe` |
| Delivery | Workflow registry + examples + durable test + Deckhand route + report URL; capability pages | `docs/registry/workflows.yaml`, `examples/workflows/`, `tests/workflows/test_durable_workflows.py`, deckhand repo `config/deckhand/routing/paths.yaml` and `domain-workflows.yaml` (sibling checkout `../deckhand`), `docs/api/ffs/` |
| Real data | Anonymized riser UT C-scan grids + registers (#1293) | `tests/asset_integrity/test_data/real_inspection/` |

### Standards

Held as full text in llm-wiki: API 579-1 2007 (all parts and Annexes B1, C, D, E, F), API 650 2007, BS 7910:2013. Metadata-only pages: API 653, API 510, API 570, API RP 571, API RP 580/581, API RP 2SIM, ISO 19902, API RP 2MIM, DNV-OS-E301, API RP 17B, API RP 2A, DNV-RP-C210, DNV-ST-F201. Rights posture: publisher identifiers and public overviews only; no clause text, tables, figures, formulas or licensed numeric thresholds in this repo (enforced for the catalog by `tests/asset_integrity/test_offering_catalog.py`); standards-derived constants are user inputs with public defaults recorded on the implementing issue (per #2157 owner card G10).

### Private Evidence Consulted

None. The #2157 weldolet MAPDL model lives on ACMA-HOU-RDS02 and was not accessible from this session; its plan is now public as PR #2194.

### Documents Consulted

- `docs/domains/asset-integrity/ffs-architecture.md`, `ffs-validation-record-2026-06-27.md`, `b31g-validation-2026-06-27.md`, `docs/domains/circumferential-defect-validation-2026-06-29.md`, `docs/domains/rstreng-2d-validation-2026-06-29.md`
- `docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md`, `ffs-readiness-review-2026-09-25.md` (in this branch; originally PR #2186)
- Issues #1057, #1094, #1274, #2157, #2160, #2171–#2178, #2180–#2185, #2197–#2203, #2205, llm-wiki #913
- Public overviews listed in `docs/domains/asset-integrity/ffs-offering-catalog.md`

### Gaps Identified

From the offering catalog (`src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml`, 78 asset-defect rows after the D5 split, counts enforced by test; design screens live in `ffs_design_screen_catalog.yml`):

| Status | Rows | Meaning for the offering |
|---|---|---|
| live | 0 | nothing is both validated and routed today |
| routed | 3 | run link exists, no validation record: inspection interval, riser fatigue, plate buckling (the legacy API 579 pipe engine is also routed but no row depends on it; the two subsea design screens moved to `ffs_design_screen_catalog.yml` per D5) |
| validated | 6 | defensible numbers, no run link |
| engine | 31 | code exists, no validation record or run link |
| planned | 25 | issue filed |
| none | 13 | roadmap candidates with no issue |

Structural gaps: no Level 3 numerical path in the unlicensed chain; three duplicate FAD implementations; no API 579 Level 2 curve; no material (Annex F) library; no storage-tank or API 653 logic; a pressure-only decision vocabulary (#2205); `ffs: unknown` in the capability map.

### Evidence

- Readiness matrix and per-module test counts: `docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md` (this branch).
- Catalog validation: `tests/asset_integrity/test_offering_catalog.py` (7 tests: shape, engine entries incl. module import and workflow ids, Deckhand routes when the sibling checkout exists, row references, status semantics (none ⇒ no engines; row ≤ strongest engine; live ⇒ only live/validated engines), no threshold-like numerics, page summary equals data). Run locally with the repo venv (`.venv/Scripts/python.exe -m pytest tests/asset_integrity/test_offering_catalog.py`) because `uv run` re-syncs the lock and fails building `cx-oracle` / `pyyaml` wheels on this Windows host; the CI job `tests-asset-integrity` on PR #2204 is the arbiter.
- #2157 review trail: Codex r1 MAJOR, Claude r1 MAJOR, Codex r2 MAJOR, Codex r2.1 MINOR; r2.2 pending owner approval.

## Offering Model

Three service tiers, every asset class offered at the highest tier its engines and validation support:

| Tier | Deliverable | Who consumes | Gate |
|---|---|---|---|
| T1 field screen | ACCEPT / MONITOR / take-more-measurements / ESCALATE within the mobilization | inspector, MI coordinator | Level 1 rule or code screening rule, validation record, workflow + route |
| T2 office Level 2 | closed-form report with RSF / MAWP / FAD point / remaining life, clause-cited | integrity engineer, owner-user | Level 2 engine, published-example golden test, cross-review |
| T3 Level 3 | numerical assessment package (strain limit, buckling, collapse, crack driving force) | specialist, class / regulator | FEA chain (#2173) or licensed host, verification receipts, engineer sign-off |

Industries in scope (catalog sections 1–8): pipelines, refining and petrochemical fixed equipment (vessels, piping, exchangers, heaters, tanks), fixed offshore platforms, floating systems with moorings and risers, wells, offshore wind support structures, ships and floating hulls, power boilers.

## Owner Decisions

Recorded from the decision page (`docs/plans/assets/2026-09-25-issue-1057-owner-decisions.html`) and posted on #1057 on 2026-09-25.

| # | Decision | Owner's choice | Effect in this plan |
|---|---|---|---|
| D1 | Verdict vocabulary across asset classes | **A** one shared vocabulary with a per-class action map | #2205 is a Wave 0 serialization point ahead of every Wave 2 verdict |
| D2 | Unlicensed Level 3 solver | **A** spike first (#2178), then choose | #2178 runs before #2173 scales; #2174 waits on its verdict |
| D3 | API 579 2016 Level 2 FAD equation | **Resolved on secondary sources** (owner asked for a check; result below) | #2175 and #2157 implement the Level 2A form; the 2016 viewer check becomes a verification item, not a blocker |
| D4 | Wave 2 asset-class order | **#2198 platforms → #2202 hulls → #2201 casing → #2172 tanks → #2199 moorings → #2200 flexible pipe** | Wave 2 list reordered; platforms and hulls start first in parallel |
| D5 | Subsea design screens in the FFS catalog | **B** move to a separate design-screen catalog | `ffs_design_screen_catalog.yml` created; FFS catalog is 78 rows; #2197 renders both |
| D6 | Review coverage | **A** re-run Gemini before approval | Re-run done 2026-09-25: UNAVAILABLE (empty provider output). Owner now chooses: run Gemini on ace-linux-1 from their own shell, or accept the two-provider review |
| Gate | Plan approval | **Approve r1.2 as is** | conflicts with D6; label withheld until the owner says which wins |

**D3 check (2026-09-25).** The #2157 plan (PR #2194, board card R01, 2026-09-24) already settled the equation form on secondary sources: an open-access peer-reviewed paper (*Materials* 19 (2026) 465, CC BY 4.0) states that API 579-1/ASME FFS-1 2016 Level 2 uses the BS 7910 Level 2A generic curve, (1 − 0.14 Lr²)(0.3 + 0.7 e^(−0.65 Lr⁶)); the licensed BS 7910:2013 text confirms that its Option 1 is the different (1 + 0.5 Lr²)^−½ form that `crack_fad` implements today; and owner card G10 declared the Level 2 curve form committable (it is also the original R6 Option 1 curve in the open literature). The 2007 API 579 text held in llm-wiki refers to Figure 9.20 for the curve, and that figure is an image the extraction did not capture, so it neither confirms nor contradicts. What remains open is only a primary-source read of the 2016 PDF, which is FileOpen-protected; treat it as verification, not a gate. Recommendation for D3: record as *resolved by literature; viewer check optional*.

## Artifact Map

```
plan  docs/plans/2026-09-25-issue-1057-ffs-offering-program.md          (this file)
data  src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml    (single source: industries x assets x defects x codes x status)
test  tests/asset_integrity/test_offering_catalog.py                    (schema, references, status semantics, rights, page counts)
page  docs/domains/asset-integrity/ffs-offering-catalog.md              (lookup tables; generator lands in #2197, counts already enforced)
notes docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md   (this branch)
      docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md       (this branch)
epic  #1057  Phase 4 (Level 3 / Part 9) + readiness demonstrators + offering asset classes
wiki  llm-wiki #913 (API 653 pointers, 2007->2021 crosswalk, Part 9 dataset audit)
```

## Deliverable

**End state.** Every catalog row is `live`, or `validated` with a documented reason it is not a workflow, or `planned` with a scheduled issue, or `none` with a dated deferral note in the catalog. Milestones below state the row counts each wave must reach; M5 closes the API 579 parts, and the remaining `none` rows carry a deferral note rather than silence.

1. **Wave 0 — foundations (no new physics).** #2160 FAD consolidation · #2171 Annex F material library · #1094 applicability-limit layer · #2205 decision-vocabulary generalization · #2197 catalog loader/renderer + API RP 571 crosswalk + capability map · #2180 routes for the canonical workflows (cross-repo: deckhand PR).
2. **Wave 1 — make the existing engines sellable.** #2181 corroded-defect method comparison · #2182 pitting + dent screens · #2183 riser-joint FFS on real data · #2184 crack FAD + Level 3 handoff · #2185 campaign demo + readiness page · #2175 Part 9 Level 2 from inspection data · #2157 weldolet FE benchmark (licensed host) · validation records for the five `routed` rows.
3. **Wave 2 — new asset classes at T1/T2, in the owner's order (D4).** #2198 platform damaged members · #2202 ship wastage · #2201 casing · #2172 tank settlement screen · #2199 mooring chain · #2200 flexible pipe. All emit verdicts through the generalized `ffs_decision` (#2205, D1).
4. **Wave 3 — Level 3 in the open chain.** #2173 CalculiX nonlinear/shell/buckling · #2174 tank Level 3 · #2178 crack driving-force spike.
5. **Wave 4 — part-by-part completion.** #2176 Part 9 Level 1 · #2177 growth / LBB / intervals · #1274 Parts 3/7/10 · #2203 Parts 8/11/13/14 · triage of the 14 `none` rows into issues or dated deferrals.

### Milestones

| Milestone | Definition of done | Catalog effect (rows) |
|---|---|---|
| M1 Foundations | Wave 0 merged; one FAD; material library; decision engine generalized; catalog page generated in CI; canonical routes live | live 0 → 2 (metal-loss coordinator, inspection planning) |
| M2 Sellable core | Wave 1 merged; every existing engine has a validation record and a run link | engine 31 → ≤ 10; routed 5 → 0 |
| M3 Multi-asset T1/T2 | Wave 2 merged in D4 order; platforms, hulls, casing, tanks, moorings, flexible pipe each have a routed, validated screen | planned 25 → ≤ 12 |
| M4 Level 3 | Wave 3 merged; tank Level 3 runs on the solver host; crack Level 3 go/no-go recorded | T3 offered for tanks |
| M5 Part completion | Wave 4 merged; every API 579 part has an engine or a dated plan; every remaining `none` row has a deferral note | none 13 → 0 undocumented |

## Execution Protocol (per issue)

1. **Plan-lite** on the issue (scope, sources, files, tests, acceptance); adversarial review per `../workspace-hub/docs/standards/AI_REVIEW_ROUTING_POLICY.md` (workspace-hub repo; Claude + Codex + Gemini by default, any provider shortfall recorded with its artifact); owner applies `status:plan-approved`.
2. **TDD**: golden test from a published example first; applicability-limit tests (flags, never silent extrapolation); negative fixtures for guards.
3. **Implement** in an isolated worktree on the lane's host (`machine:dev-primary` for closed-form work; `machine:licensed-win-1` / RDS02 for ANSYS; Linux host for CalculiX). Use `uv run`.
4. **Wire**: registry row + `examples/workflows/<id>/` with committed results + durable test + Deckhand route (deckhand repo PR) + `ffs_report` output + catalog status update; `test_offering_catalog.py` fails if a `live`/`routed` row lacks a registry id or route, or a `live`/`validated` row lacks a validation record.
5. **Validate**: validation record under `docs/domains/asset-integrity/` with reference case, our value, difference, limits; the record's numbers asserted by a test.
6. **Land** via PR with cross-review; never `git merge` on main in-session (use `gh pr merge`); write a handoff before stopping.

Parallelism: Wave 0 items are disjoint and can run concurrently on separate worktrees; Wave 2 asset classes are disjoint from each other and from Wave 3 solver work. Serialization points: #2160 before #2175/#2176/#2184; #2171 before #2173/#2174/#2175; #2178 (D2 spike) before #2173 scales and before #2174; #2205 (D1) before #2172/#2198/#2199/#2202 emit verdicts; #2180 requires a deckhand-repo PR and re-rendered API catalog before any `live` claim.

## Files to Change (program level; each issue lists its own)

- This PR: `src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml`, `ffs_design_screen_catalog.yml` (D5), `docs/plans/assets/2026-09-25-issue-1057-owner-decisions.html`, `tests/asset_integrity/test_offering_catalog.py`, `docs/domains/asset-integrity/ffs-offering-catalog.md`, `pyproject.toml` package-data entry, `docs/plans/README.md`
- `src/digitalmodel/asset_integrity/offering_catalog.py` (#2197)
- `src/digitalmodel/asset_integrity/assessment/ffs_decision.py` (#2205)
- `src/digitalmodel/asset_integrity/{tank_settlement,mooring_chain,platform_member,hull_wastage,casing_capacity,flexible_pipe}.py` (Wave 2)
- `src/digitalmodel/solvers/calculix/{inp_writer,result_parser,fem_chain}.py` (#2173)
- `src/digitalmodel/materials/ffs_material_library.py` (#2171)
- `docs/registry/workflows.yaml`, `examples/workflows/<id>/`, deckhand `config/deckhand/routing/paths.yaml` and `domain-workflows.yaml` (every wired issue)
- `docs/domains/asset-integrity/*-validation-*.md`, `docs/api/ffs/ffs-readiness.html` (#2185), `docs/capability-map/capabilities-added.yml` (#2197)

## TDD Test List (program level)

- `tests/asset_integrity/test_offering_catalog.py` (in this PR): shape; engine entries import, workflow ids exist, `live`/`routed` ⇒ route, `live`/`validated` ⇒ existing validation record, `planned` ⇒ issue; Deckhand route presence when `../deckhand` exists; row references resolve; row status ≤ engines' status; no threshold-like numerics; page summary counts equal data.
- Per engine: published-example golden test; applicability flag tests; negative guard fixtures; `test_durable_workflows[<id>]` parametrized row.
- Per validation record: the numbers in the record are asserted by a test (no record without a test).

## Acceptance Criteria (program)

1. Catalog is the single source; the page is generated (#2197) and its counts are enforced now; CI enforces status semantics.
2. Every `live` row has a registered workflow whose durable test passes and a `digitalmodel:<id>` Deckhand route. Evidence per row: the durable test id and the route line in deckhand `config/deckhand/routing/paths.yaml`. A report URL from `POST /api/run` becomes part of the evidence only once deckhand #498/#499 land (today `docs/deckhand/API.md` calls `report_url` a target contract); until then the report is the committed `examples/workflows/<id>/results/` output.
3. No standards clause text, table, figure, formula or licensed threshold in the repo; rights card recorded per issue.
4. Every engine that reaches `validated` has a validation record with a published comparator and a test asserting the record's numbers.
5. Level 3 offerings state the solver, receipts and sign-off requirement in the report.
6. Readiness page reflects the catalog within one release of any change.

## Adversarial Review Summary

| Round | Provider | Verdict | Disposition |
|---|---|---|---|
| r1 | Claude (subagent, read-only) | MAJOR, 9 findings | all applied in r1.1 (below) |
| r1 | Codex (`plan-review-fanout.sh`, workspace-hub) | MAJOR, 9 findings | all applied in r1.1 (below) |
| r1.1 | Codex confirming pass | MAJOR, 7 findings | all applied in r1.2 (below) |
| r1.1 | Gemini (`plan-review-fanout.sh --providers=gemini`, CLI 0.61.0 installed) | no verdict: the run was stopped by the host under memory pressure after producing empty artifacts (`2026-09-25-plan-1057-gemini.md`, `.err`, 0 bytes) | owner chose D6 = A; re-run requested 2026-09-25 |
| r1.3 | owner decisions | D1 A, D2 A, D4 order, D5 B, D6 A applied; D3 resolved on secondary sources | catalog split, Wave 2 reordered, serialization updated |
| r1.3 | Gemini re-run (D6 A, `plan-review-fanout.sh --providers=gemini`, 2026-09-25 11:19) | UNAVAILABLE: the CLI exited rc=0 with empty provider output; fanout wrote a no-signal artifact (`review-gemini2/2026-09-25-plan-1057-gemini.md`) | Gemini has now failed twice on this host (memory kill, then empty output); a third attempt needs a different host (ace-linux-1) from the owner's shell, or the owner accepts the two-provider review |

**r1.1 → r1.2 (Codex confirming pass):**

1. Status validator was vacuous for `none` rows and used the wrong comparison: rewritten as `test_status_semantics` (none ⇒ no engines; row ≤ strongest engine; planned ⇒ planned engine or issue; live ⇒ only live/validated engines) and the catalog header now states the same rule.
2. Offshore-wind lifetime-extension row listed an engine while `none`: engines cleared, note kept.
3. Acceptance #2 relied on Deckhand `report_url`, which `docs/deckhand/API.md` calls a target contract: evidence is now the durable test id + route line, with `report_url` gated on deckhand #498/#499.
4. Gemini downgrade was unsupported: attempted; artifact recorded above; owner decision D6 added.
5. PR #2186 evidence was outside this PR: its commit is cherry-picked into this branch; #2186 is superseded.
6. Routed narrative named six engines for five rows: fixed (the legacy pipe engine is routed but no row depends on it).
7. `uv run pytest` fails on this host before tests: the plan now names the venv command actually used and the CI job that arbitrates.

**r1 → r1.1** findings and how r1.1 answered them:

1. Catalog over-stated `live` and `validated` (both reviewers): added `routed` status; legacy API 579 pipe, inspection planning, riser fatigue, plate buckling and the two subsea screens are `routed`; boiler tube thinning and measurement sufficiency downgraded to `engine`; live count is 0 and the plan says so.
2. "Every engine reference resolves" was false: `structural-health` and `scour` engine entries added; seven code identifiers added (`asme-b31-8`, `dnv-rp-f105/f109/f110`, `dnv-rp-c201`, `api-rp-2fsim`, `chain-fears-jip`); `plate-panel-buckling` now points at the real `plate-buckling` registry id; the catalog test enforces all of this in this PR.
3. Rights: IACS percentage limits and chain pitting dimensions removed from catalog and page; a test rejects threshold-like numerics.
4. Misattributions: B31.8 Appendix R (not B31.8S); API STD 2RD supersedes RP 2RD; ISO 19902 note; unused DNV-RP-E303 replaced by DNV-OS-E303 for fibre rope.
5. Paths: review policy lives in workspace-hub; Deckhand routing files live in the deckhand repo; companion notes were on PR #2186 and are now cherry-picked into this branch.
6. End state vs milestones contradiction: end state now admits `none` rows with dated deferral notes, and M5 requires none to be undocumented.
7. Missing enforcement test: `tests/asset_integrity/test_offering_catalog.py` ships in this PR; 7 tests pass.
8. Decision vocabulary for non-pressure assets: filed #2205 (Wave 0) and owner decision D1.
9. Coverage gaps: added exchanger and fired-heater tubes, conductors/caissons, wellhead fatigue, wire and fibre rope, anchors, geohazards, ECDA/ICDA, wind lifetime extension as rows.
10. Acceptance #2 now names its evidence (durable test id + route line + Deckhand API contract) instead of asserting behaviour the repo cannot show.

Next: owner confirms the D3 reading, decides how to close D6 (Gemini on another host, or accept two providers), and the approval gate follows; then `status:plan-approved` and Wave 0 starts.

## Risks and Open Questions

| Risk | Mitigation |
|---|---|
| Rights: reduction rules and thresholds for new asset classes come from licensed texts | Thresholds are inputs with documented public defaults; goldens from published papers; rights card per issue; catalog test rejects numerics |
| Solver host: no CalculiX on Windows hosts; SSH to hosts blocked from agent sessions | Solver work scheduled on ace-linux-1 by the user's own shell; workflows fail soft to a Level 3 handoff |
| API 579 Level 2 curve confirmed only on secondary sources (2016 copy DRM-protected) | D3: implement the Level 2A form with the open-literature citation; primary-source read is a verification item |
| Scope breadth: 14 `none` rows after triage | Waves ordered by demand; catalog makes deferrals explicit and dated |
| Duplicate implementations recur | #2160 first; catalog CI checks one engine per (code, mechanism) once #2197 lands |
| #2157 depends on a licensed machine | Its plan is public (PR #2194); receipts are committed before #2178 compares against them; program does not block on it |
| Deckhand `report_url` not implemented (deckhand #498/#499) | Acceptance #2 uses committed results + route line until it lands; `live` never claims a report URL before then |

## Complexity: T4 (program of ~27 issues across five waves; individual issues T2–T3)
