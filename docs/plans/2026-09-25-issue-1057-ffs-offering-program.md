# Plan for #1057: Fitness-For-Service Offering Program

**Issue:** [#1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) (epic), Phase 4 and readiness demonstrators
**Date:** 2026-09-25 · **Revision:** r1.1 (after Claude r1 MAJOR and Codex r1 MAJOR; findings and dispositions in *Adversarial Review Summary*)
**Status:** under adversarial review. `status:plan-review` on the epic means "a plan exists and is being reviewed"; approval is the owner's `status:plan-approved`, which this plan never self-applies.
**Depends on:** PR #2186 (program note and readiness review, cited below) and PR #2194 (#2157 crack FE plan) being merged, or their content being read from those branches.
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
- PR #2186: `docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md`, `ffs-readiness-review-2026-09-25.md`
- Issues #1057, #1094, #1274, #2157, #2160, #2171–#2178, #2180–#2185, #2197–#2203, #2205, llm-wiki #913
- Public overviews listed in `docs/domains/asset-integrity/ffs-offering-catalog.md`

### Gaps Identified

From the offering catalog (`src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml`, 81 asset-defect rows, counts enforced by test):

| Status | Rows | Meaning for the offering |
|---|---|---|
| live | 0 | nothing is both validated and routed today |
| routed | 5 | run link exists, no validation record (legacy API 579 pipe, inspection planning, riser fatigue, plate buckling, two subsea design screens) |
| validated | 6 | defensible numbers, no run link |
| engine | 31 | code exists, no validation record or run link |
| planned | 25 | issue filed |
| none | 14 | roadmap candidates with no issue |

Structural gaps: no Level 3 numerical path in the unlicensed chain; three duplicate FAD implementations; no API 579 Level 2 curve; no material (Annex F) library; no storage-tank or API 653 logic; a pressure-only decision vocabulary (#2205); `ffs: unknown` in the capability map.

### Evidence

- Readiness matrix and per-module test counts: `ffs-readiness-review-2026-09-25.md` (PR #2186).
- Catalog validation: `tests/asset_integrity/test_offering_catalog.py` (7 tests: shape, engine entries incl. module import and workflow ids, Deckhand routes when the sibling checkout exists, row references, status not stronger than engines, no threshold-like numerics, page summary equals data).
- #2157 review trail: Codex r1 MAJOR, Claude r1 MAJOR, Codex r2 MAJOR, Codex r2.1 MINOR; r2.2 pending owner approval.

## Offering Model

Three service tiers, every asset class offered at the highest tier its engines and validation support:

| Tier | Deliverable | Who consumes | Gate |
|---|---|---|---|
| T1 field screen | ACCEPT / MONITOR / take-more-measurements / ESCALATE within the mobilization | inspector, MI coordinator | Level 1 rule or code screening rule, validation record, workflow + route |
| T2 office Level 2 | closed-form report with RSF / MAWP / FAD point / remaining life, clause-cited | integrity engineer, owner-user | Level 2 engine, published-example golden test, cross-review |
| T3 Level 3 | numerical assessment package (strain limit, buckling, collapse, crack driving force) | specialist, class / regulator | FEA chain (#2173) or licensed host, verification receipts, engineer sign-off |

Industries in scope (catalog sections 1–8): pipelines, refining and petrochemical fixed equipment (vessels, piping, exchangers, heaters, tanks), fixed offshore platforms, floating systems with moorings and risers, wells, offshore wind support structures, ships and floating hulls, power boilers.

## Owner Decisions Required

| # | Decision | Default if not decided |
|---|---|---|
| D1 | One verdict vocabulary across asset classes (#2205 generalizes `ffs_decision`) or per-class vocabularies with a shared severity ordering | one vocabulary with per-class action map (#2205) |
| D2 | CalculiX or Code_Aster (#253) for the unlicensed Level 3 chain | spike in #2178 decides before #2173 scales |
| D3 | API 579 2016 Level 2 FAD equation confirmed in an authorised viewer (#2157 card R01) | curves stay labelled BS 7910 Option 1 |
| D4 | Wave 2 asset-class order (tanks, moorings, platforms, hulls, casing, flexible pipe) by prospect demand | order as listed |
| D5 | Whether the two subsea design screens (free span, on-bottom stability) stay in the FFS catalog as `routed` rows or move to a design-screen catalog | stay, flagged "not an FFS verdict" |

## Artifact Map

```
plan  docs/plans/2026-09-25-issue-1057-ffs-offering-program.md          (this file)
data  src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml    (single source: industries x assets x defects x codes x status)
test  tests/asset_integrity/test_offering_catalog.py                    (schema, references, status semantics, rights, page counts)
page  docs/domains/asset-integrity/ffs-offering-catalog.md              (lookup tables; generator lands in #2197, counts already enforced)
notes docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md   (PR #2186)
      docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md       (PR #2186)
epic  #1057  Phase 4 (Level 3 / Part 9) + readiness demonstrators + offering asset classes
wiki  llm-wiki #913 (API 653 pointers, 2007->2021 crosswalk, Part 9 dataset audit)
```

## Deliverable

**End state.** Every catalog row is `live`, or `validated` with a documented reason it is not a workflow, or `planned` with a scheduled issue, or `none` with a dated deferral note in the catalog. Milestones below state the row counts each wave must reach; M5 closes the API 579 parts, and the remaining `none` rows carry a deferral note rather than silence.

1. **Wave 0 — foundations (no new physics).** #2160 FAD consolidation · #2171 Annex F material library · #1094 applicability-limit layer · #2205 decision-vocabulary generalization · #2197 catalog loader/renderer + API RP 571 crosswalk + capability map · #2180 routes for the canonical workflows (cross-repo: deckhand PR).
2. **Wave 1 — make the existing engines sellable.** #2181 corroded-defect method comparison · #2182 pitting + dent screens · #2183 riser-joint FFS on real data · #2184 crack FAD + Level 3 handoff · #2185 campaign demo + readiness page · #2175 Part 9 Level 2 from inspection data · #2157 weldolet FE benchmark (licensed host) · validation records for the five `routed` rows.
3. **Wave 2 — new asset classes at T1/T2.** #2172 tank settlement screen · #2199 mooring chain · #2198 platform damaged members · #2202 ship wastage · #2201 casing · #2200 flexible pipe.
4. **Wave 3 — Level 3 in the open chain.** #2173 CalculiX nonlinear/shell/buckling · #2174 tank Level 3 · #2178 crack driving-force spike.
5. **Wave 4 — part-by-part completion.** #2176 Part 9 Level 1 · #2177 growth / LBB / intervals · #1274 Parts 3/7/10 · #2203 Parts 8/11/13/14 · triage of the 14 `none` rows into issues or dated deferrals.

### Milestones

| Milestone | Definition of done | Catalog effect (rows) |
|---|---|---|
| M1 Foundations | Wave 0 merged; one FAD; material library; decision engine generalized; catalog page generated in CI; canonical routes live | live 0 → 2 (metal-loss coordinator, inspection planning) |
| M2 Sellable core | Wave 1 merged; every existing engine has a validation record and a run link | engine 31 → ≤ 10; routed 5 → 0 |
| M3 Multi-asset T1/T2 | Wave 2 merged; tanks, moorings, platforms, hulls, casing, flexible pipe each have a routed, validated screen | planned 25 → ≤ 12 |
| M4 Level 3 | Wave 3 merged; tank Level 3 runs on the solver host; crack Level 3 go/no-go recorded | T3 offered for tanks |
| M5 Part completion | Wave 4 merged; every API 579 part has an engine or a dated plan; every remaining `none` row has a deferral note | none 14 → 0 undocumented |

## Execution Protocol (per issue)

1. **Plan-lite** on the issue (scope, sources, files, tests, acceptance); adversarial review per `../workspace-hub/docs/standards/AI_REVIEW_ROUTING_POLICY.md` (workspace-hub repo; Codex + Claude, Gemini unavailable on Windows hosts); owner applies `status:plan-approved`.
2. **TDD**: golden test from a published example first; applicability-limit tests (flags, never silent extrapolation); negative fixtures for guards.
3. **Implement** in an isolated worktree on the lane's host (`machine:dev-primary` for closed-form work; `machine:licensed-win-1` / RDS02 for ANSYS; Linux host for CalculiX). Use `uv run`.
4. **Wire**: registry row + `examples/workflows/<id>/` with committed results + durable test + Deckhand route (deckhand repo PR) + `ffs_report` output + catalog status update; `test_offering_catalog.py` fails if a `live`/`routed` row lacks a registry id or route, or a `live`/`validated` row lacks a validation record.
5. **Validate**: validation record under `docs/domains/asset-integrity/` with reference case, our value, difference, limits; the record's numbers asserted by a test.
6. **Land** via PR with cross-review; never `git merge` on main in-session (use `gh pr merge`); write a handoff before stopping.

Parallelism: Wave 0 items are disjoint and can run concurrently on separate worktrees; Wave 2 asset classes are disjoint from each other and from Wave 3 solver work. Serialization points: #2160 before #2175/#2176/#2184; #2171 before #2173/#2174/#2175; #2173 before #2174/#2178; #2205 before #2172/#2198/#2199/#2202 emit verdicts; #2180 requires a deckhand-repo PR and re-rendered API catalog before any `live` claim.

## Files to Change (program level; each issue lists its own)

- This PR: `src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml`, `tests/asset_integrity/test_offering_catalog.py`, `docs/domains/asset-integrity/ffs-offering-catalog.md`, `pyproject.toml` package-data entry, `docs/plans/README.md`
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
2. Every `live` row has a registered workflow whose durable test passes and a `digitalmodel:<id>` Deckhand route; the Deckhand `POST /api/run` contract (deckhand `docs/deckhand/API.md`) returns its report URL. Evidence per row: the durable test id and the route line.
3. No standards clause text, table, figure, formula or licensed threshold in the repo; rights card recorded per issue.
4. Every engine that reaches `validated` has a validation record with a published comparator and a test asserting the record's numbers.
5. Level 3 offerings state the solver, receipts and sign-off requirement in the report.
6. Readiness page reflects the catalog within one release of any change.

## Adversarial Review Summary

| Round | Provider | Verdict | Disposition |
|---|---|---|---|
| r1 | Claude (subagent, read-only) | MAJOR, 9 findings | all applied in r1.1 (below) |
| r1 | Codex (`plan-review-fanout.sh`, workspace-hub) | MAJOR, 9 findings | all applied in r1.1 (below) |
| r1 | Gemini | unavailable on Windows host | T2 review |

Findings and how r1.1 answers them:

1. Catalog over-stated `live` and `validated` (both reviewers): added `routed` status; legacy API 579 pipe, inspection planning, riser fatigue, plate buckling and the two subsea screens are `routed`; boiler tube thinning and measurement sufficiency downgraded to `engine`; live count is 0 and the plan says so.
2. "Every engine reference resolves" was false: `structural-health` and `scour` engine entries added; seven code identifiers added (`asme-b31-8`, `dnv-rp-f105/f109/f110`, `dnv-rp-c201`, `api-rp-2fsim`, `chain-fears-jip`); `plate-panel-buckling` now points at the real `plate-buckling` registry id; the catalog test enforces all of this in this PR.
3. Rights: IACS percentage limits and chain pitting dimensions removed from catalog and page; a test rejects threshold-like numerics.
4. Misattributions: B31.8 Appendix R (not B31.8S); API STD 2RD supersedes RP 2RD; ISO 19902 note; unused DNV-RP-E303 replaced by DNV-OS-E303 for fibre rope.
5. Paths: review policy lives in workspace-hub; Deckhand routing files live in the deckhand repo; companion notes are on PR #2186 (declared dependency).
6. End state vs milestones contradiction: end state now admits `none` rows with dated deferral notes, and M5 requires none to be undocumented.
7. Missing enforcement test: `tests/asset_integrity/test_offering_catalog.py` ships in this PR; 7 tests pass.
8. Decision vocabulary for non-pressure assets: filed #2205 (Wave 0) and owner decision D1.
9. Coverage gaps: added exchanger and fired-heater tubes, conductors/caissons, wellhead fatigue, wire and fibre rope, anchors, geohazards, ECDA/ICDA, wind lifetime extension as rows.
10. Acceptance #2 now names its evidence (durable test id + route line + Deckhand API contract) instead of asserting behaviour the repo cannot show.

Next: Codex confirming pass on r1.1, then owner approval.

## Risks and Open Questions

| Risk | Mitigation |
|---|---|
| Rights: reduction rules and thresholds for new asset classes come from licensed texts | Thresholds are inputs with documented public defaults; goldens from published papers; rights card per issue; catalog test rejects numerics |
| Solver host: no CalculiX on Windows hosts; SSH to hosts blocked from agent sessions | Solver work scheduled on ace-linux-1 by the user's own shell; workflows fail soft to a Level 3 handoff |
| API 579 Level 2 curve unconfirmed (2016 copy DRM-protected) | Owner decision D3 |
| Scope breadth: 14 `none` rows after triage | Waves ordered by demand; catalog makes deferrals explicit and dated |
| Duplicate implementations recur | #2160 first; catalog CI checks one engine per (code, mechanism) once #2197 lands |
| #2157 depends on a licensed machine | Its plan is public (PR #2194); receipts are committed before #2178 compares against them; program does not block on it |
| Companion notes unmerged (PR #2186) | Declared dependency; merge #2186 before or with this PR |

## Complexity: T4 (program of ~27 issues across five waves; individual issues T2–T3)
