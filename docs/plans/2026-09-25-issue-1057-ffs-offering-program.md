# Plan for #1057: Fitness-For-Service Offering Program

**Issue:** [#1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) (epic), Phase 4 and readiness demonstrators
**Date:** 2026-09-25
**Status:** plan-review (owner applies `status:plan-approved`; not self-labelled)
**Objective (owner's words):** be able to offer fitness-for-service (FFS) services for a wide variety of assets via available codes and standards.

## Resource Intelligence Summary

### Existing Repo Code

| Area | What exists | Where |
|---|---|---|
| Metal-loss FFS | Canonical L1/L2 coordinator (GML/LML/pitting router, decision, report, sufficiency), legacy API579 engine | `src/digitalmodel/asset_integrity/assessment/*`, `API579.py` |
| Pipeline strength | B31G / Mod B31G / RSTRENG, RSTRENG-2D, DNV-RP-F101, circumferential | `corroded_pipe.py`, `rstreng_2d.py`, `dnv_rp_f101.py`, `circumferential_defect.py` |
| Other mechanisms | Pitting (Pt 6), dents (Pt 12), crack FAD (BS 7910 Opt 1), Paris growth, Level 3 handoff | `assessment/pitting.py`, `dent_assessment.py`, `assessment/crack_fad.py`, `fatigue/crack_growth.py`, `assessment/level3_escalation.py` |
| Decision layer | RBI screen, composite repair selector, inspection planning, acceptance curves + lookup | `rbi_screening.py`, `composite_repair.py`, `inspection_planning.py`, `ffs_acceptance_curves.py`, `ffs_lookup.py` |
| Asset engines reusable for FFS | Plate/panel buckling, hull girder screening, jacket member/joint checks, mooring screening + fatigue, riser fatigue, S-N library (221 curves), scour, SHM templates | `infrastructure/base_solvers/structural`, `hull_girder_screening`, `structural/jacket_topside`, `mooring_resilience`, `mooring_fatigue`, `riser_fatigue`, `structural/fatigue`, `geotechnical/scour.py`, `structural/offshore_resilience` |
| Solvers | gmsh meshing, CalculiX linear static chain, ANSYS APDL generators (licensed host), in-house beam eigen-buckling | `solvers/gmsh_meshing`, `solvers/calculix`, `ansys/`, `structural/fe` |
| Delivery | Workflow registry + examples + durable test + Deckhand route + report URL; capability pages | `docs/registry/workflows.yaml`, `examples/workflows/`, `tests/workflows/test_durable_workflows.py`, deckhand `config/deckhand/routing/domain-workflows.yaml`, `docs/api/ffs/` |
| Real data | Anonymized riser UT C-scan grids + registers (#1293) | `tests/asset_integrity/test_data/real_inspection/` |

### Standards

Held as full text in llm-wiki: API 579-1 2007 (all parts and Annexes B1, C, D, E, F), API 650 2007, BS 7910:2013. Metadata-only pages: API 653, API 510, API 570, API RP 571, API RP 580/581, API RP 2SIM, ISO 19902, API RP 2MIM, DNV-OS-E301, API RP 17B, API RP 2A, DNV-RP-C210, DNV-ST-F201. Rights posture: publisher facts and clause pointers only; no clause text, tables or figures in this repo; standards-derived constants are user inputs or regenerated from published examples (per #2157 owner card G10).

### Private Evidence Consulted

None. The #2157 weldolet plan and MAPDL model live on ACMA-HOU-RDS02 and were not accessible from this session; their state is taken from the #2157 comment trail.

### Documents Consulted

- `docs/domains/asset-integrity/ffs-architecture.md`, `ffs-validation-record-2026-06-27.md`, `b31g-validation-2026-06-27.md`, `docs/domains/circumferential-defect-validation-2026-06-29.md`, `docs/domains/rstreng-2d-validation-2026-06-29.md`
- `docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md`, `ffs-readiness-review-2026-09-25.md` (PR #2186)
- Issues #1057, #1094, #1274, #2157, #2160, #2171–#2178, #2180–#2185, llm-wiki #913
- Public overviews of API 579 2021 parts, API RP 571, PDAM, ASME B31.8S, API 570, API 653, API RP 2SIM, API RP 2MIM, API RP 17B, IACS CSR Ch 13, DNV-ST-0126 literature, API TR 5C3 literature (source list in `docs/domains/asset-integrity/ffs-offering-catalog.md`)

### Gaps Identified

From the offering catalog (`src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml`, 71 asset-defect rows):

| Status | Rows | Meaning for the offering |
|---|---|---|
| live | 4 | sellable today with a run link |
| validated | 8 | defensible numbers, no run link |
| engine | 30 | code exists, no validation record or run link |
| planned | 6 | issue filed (tank settlement, Part 9, Parts 3/7/10) |
| none | 23 | asset classes with no engine: platform damaged members, mooring chain, flexible pipe, casing, ship wastage, API 579 Parts 8/11/13/14, API RP 571 routing |

Structural gaps: one live Deckhand route (legacy); no Level 3 numerical path in the unlicensed chain; three duplicate FAD implementations; no API 579 Level 2 curve; no material (Annex F) library; no storage-tank or API 653 logic; `ffs: unknown` in the capability map.

### Evidence

- Readiness matrix and per-module test counts: `ffs-readiness-review-2026-09-25.md` (subagent inventory 2026-09-25, 19 engines).
- Catalog validation: YAML parses; every engine reference resolves; 71 rows (4/8/30/6/23).
- #2157 review trail: Codex r1 MAJOR, Claude r1 MAJOR, Codex r2 MAJOR, Codex r2.1 MINOR; r2.2 pending owner approval.

## Offering Model

Three service tiers, every asset class offered at the highest tier its engines and validation support:

| Tier | Deliverable | Who consumes | Gate |
|---|---|---|---|
| T1 field screen | ACCEPT / MONITOR / take-more-measurements / ESCALATE within the mobilization | inspector, MI coordinator | Level 1 rule or code screening rule, validation record, workflow + route |
| T2 office Level 2 | closed-form report with RSF / MAWP / FAD point / remaining life, clause-cited | integrity engineer, owner-user | Level 2 engine, published-example golden test, cross-review |
| T3 Level 3 | numerical assessment package (strain limit, buckling, collapse, crack driving force) | specialist, class / regulator | FEA chain (#2173) or licensed host, verification receipts, engineer sign-off |

Industries in scope (catalog sections 1–8): pipelines, refining and petrochemical fixed equipment (vessels, piping, tanks), fixed offshore platforms, floating systems with moorings and risers, wells, offshore wind support structures, ships and floating hulls, power boilers.

## Artifact Map

```
plan  docs/plans/2026-09-25-issue-1057-ffs-offering-program.md          (this file)
data  src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml    (single source: industries x assets x defects x codes x status)
page  docs/domains/asset-integrity/ffs-offering-catalog.md              (rendered lookup tables; generator lands in #2197)
notes docs/domains/asset-integrity/level3-and-part9-program-2026-09-25.md
      docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md
epic  #1057  Phase 4 (Level 3 / Part 9) + readiness demonstrators + offering asset classes
wiki  llm-wiki #913 (API 653 pointers, 2007->2021 crosswalk, Part 9 dataset audit)
```

## Deliverable

At program end every catalog row is `live`, `validated` with a documented reason it is not a workflow, or `planned` with a scheduled issue. Concretely:

1. **Wave 0 — foundations (no new physics).** #2160 FAD consolidation · #2171 Annex F material library · #1094 applicability-limit layer · #2197 catalog loader/renderer + API RP 571 crosswalk + capability map · #2180 routes for the canonical workflows.
2. **Wave 1 — make the existing engines sellable.** #2181 corroded-defect method comparison · #2182 pitting + dent screens · #2183 riser-joint FFS on real data · #2184 crack FAD + Level 3 handoff · #2185 campaign demo + readiness page · #2175 Part 9 Level 2 from inspection data · #2157 weldolet FE benchmark (licensed host).
3. **Wave 2 — new asset classes at T1/T2.** #2172 tank settlement screen · #2199 mooring chain · #2198 platform damaged members · #2202 ship wastage · #2201 casing · #2200 flexible pipe.
4. **Wave 3 — Level 3 in the open chain.** #2173 CalculiX nonlinear/shell/buckling · #2174 tank Level 3 · #2178 crack driving-force spike.
5. **Wave 4 — part-by-part completion.** #2176 Part 9 Level 1 · #2177 growth / LBB / intervals · #1274 Parts 3/7/10 · #2203 Parts 8/11/13/14.

### Milestones

| Milestone | Definition of done | Catalog effect |
|---|---|---|
| M1 Foundations | Wave 0 merged; one FAD; material library; catalog page generated in CI; canonical routes live | live rows 4 → 6 |
| M2 Sellable core | Wave 1 merged; every existing engine has a validation record and a run link; readiness page public | engine rows 30 → ≤ 10 |
| M3 Multi-asset T1/T2 | Wave 2 merged; tanks, moorings, platforms, hulls, casing, flexible pipe each have a routed screen | none rows 23 → ≤ 8 |
| M4 Level 3 | Wave 3 merged; tank Level 3 runs on the solver host; crack Level 3 verdict recorded | T3 offered for tanks; crack T3 go/no-go |
| M5 Part completion | Wave 4 merged | every API 579 part has an engine or a dated plan |

## Execution Protocol (per issue)

1. **Plan-lite** on the issue (scope, sources, files, tests, acceptance), adversarial review per `docs/standards/AI_REVIEW_ROUTING_POLICY.md`; owner applies `status:plan-approved`.
2. **TDD**: golden test from a published example first; applicability-limit tests (flags, never silent extrapolation); negative fixtures for guards.
3. **Implement** in an isolated worktree on the lane's host (`machine:dev-primary` for closed-form work; `machine:licensed-win-1` / RDS02 for ANSYS; Linux host for CalculiX). Use `uv run`.
4. **Wire**: registry row + `examples/workflows/<id>/` with committed results + durable test + Deckhand route + `ffs_report` output + catalog status update (the catalog CI test fails if a `live` row lacks a route).
5. **Validate**: validation record under `docs/domains/asset-integrity/` with reference case, our value, difference, limits.
6. **Land** via PR with cross-review; never `git merge` on main in-session (use `gh pr merge`); write a handoff before stopping.

Parallelism: Wave 0 items are disjoint and can run concurrently on separate worktrees; Wave 2 asset classes are disjoint from each other and from Wave 3 solver work. Serialization points: #2160 before #2175/#2176/#2184; #2171 before #2173/#2174/#2175; #2173 before #2174/#2178.

## Files to Change (program level; each issue lists its own)

- `src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml` (new, this PR) and `pyproject.toml` package-data entry (this PR)
- `src/digitalmodel/asset_integrity/offering_catalog.py` (#2197)
- `src/digitalmodel/asset_integrity/{tank_settlement,mooring_chain,platform_member,hull_wastage,casing_capacity,flexible_pipe}.py` (Wave 2)
- `src/digitalmodel/solvers/calculix/{inp_writer,result_parser,fem_chain}.py` (#2173)
- `src/digitalmodel/materials/ffs_material_library.py` (#2171)
- `docs/registry/workflows.yaml`, `examples/workflows/<id>/`, deckhand `config/deckhand/routing/domain-workflows.yaml` (every wired issue)
- `docs/domains/asset-integrity/*-validation-*.md`, `docs/api/ffs/ffs-readiness.html`, `docs/capability-map/capabilities-added.yml`

## TDD Test List (program level)

- `tests/asset_integrity/test_offering_catalog.py`: schema valid; engine refs resolve; `live` ⇒ registry row and route; `planned` ⇒ open issue; rendered page equals committed page.
- Per engine: published-example golden test; applicability flag tests; negative guard fixtures; `test_durable_workflows[<id>]` parametrized row.
- Per validation record: the numbers in the record are asserted by a test (no record without a test).

## Acceptance Criteria (program)

1. Catalog is the single source; the page is generated; CI enforces status semantics.
2. Every `live` row runs through `POST /api/run` and returns a report URL with clause citations for the held edition and the current edition.
3. No standards clause text, table or figure in the repo; rights gate recorded per issue.
4. Every engine that reaches `validated` has a validation record with a published comparator.
5. Level 3 offerings state the solver, receipts and sign-off requirement in the report.
6. Readiness page reflects the catalog within one release of any change.

## Adversarial Review Summary

Not yet run on this plan. Required before `status:plan-approved`: one Codex and one Claude pass per `AI_REVIEW_ROUTING_POLICY.md` (Gemini unavailable on Windows hosts). Known items reviewers should probe: whether Wave 2 asset classes have citable public sources for their reduction rules (mooring pitting classes, dented tubular capacity, IACS renewal thresholds) or must be modelled as user inputs; whether the CalculiX path is worth pursuing versus Code_Aster (#253) before #2173 starts; and whether T1 verdicts for non-pressure assets (moorings, hulls, platforms) need a different decision vocabulary than `ffs_decision`'s ACCEPT / MONITOR / RE_RATE / REPAIR / REPLACE.

## Risks and Open Questions

| Risk | Mitigation |
|---|---|
| Rights: reduction rules and thresholds for new asset classes come from licensed texts | Thresholds are inputs with documented public defaults; goldens from published papers; rights card per issue |
| Solver host: no CalculiX on Windows hosts; SSH to hosts blocked from agent sessions | Solver work scheduled on ace-linux-1 by the user's own shell; workflows fail soft to a Level 3 handoff |
| API 579 Level 2 curve unconfirmed (2016 copy DRM-protected) | Owner confirms in an authorised viewer (#2157 card R01); until then curves are labelled BS 7910 Option 1 |
| Scope breadth: 23 `none` rows | Waves ordered by demand (pipelines and fixed equipment first, then offshore, then ships/wells); catalog makes deferrals explicit |
| Duplicate implementations recur | #2160 first; catalog CI checks one engine per (code, mechanism) |
| #2157 depends on a licensed machine and an uncommitted plan | Its receipts are committed before #2178 compares against them; program does not block on it |

## Complexity: T4 (program of ~25 issues across five waves; individual issues T2–T3)
