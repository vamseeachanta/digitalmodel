# FFS Level 3 and Part 9 Program — 2026-09-25

**Epic:** #1057 Phase 4
**Module:** `digitalmodel.asset_integrity` (+ `solvers/calculix`, `materials`)
**Standards:** API 579-1/ASME FFS-1 (2007 text held; 2016/2021 numbering in reports), API 653 Annex B, BS 7910:2013
**Knowledge companion:** llm-wiki #913

## Why this note exists

Two external prompts were reviewed on 2026-09-24:

1. A LinkedIn post (S. Laonittharakul) describing an API 579 fitness-for-service
   assessment of a settled large storage tank by two checks: elastic-plastic
   total strain against a permissible strain limit, and a buckling factor
   against an allowable. A commenter (T. Seipp) challenged the failure-mode
   assumption and the stress-strain curve. Both points shape the design below:
   settlement is displacement-controlled, so strain-limit and buckling checks
   lead and plastic collapse is reported as secondary.
2. The inspection-for-industry.com Part 9 page, a practitioner intake checklist
   for crack-like flaws (flaw geometry and position, weld/HAZ/base location,
   loads, toughness basis, PWHT and residual stress, hydrogen service, growth
   and leak-before-break). It carries no method; its value is as the input
   schema for a Part 9 workflow.

Both map onto the same gap: `level3_escalation.py` deliberately stops at a
handoff because no FEA and no Level 3 acceptance post-processing exist in the
open-source chain.

## What the ecosystem already holds

| Layer | Have | Do not have |
|---|---|---|
| Knowledge (llm-wiki) | API 579-1 2007 full text incl. Annex B1 (stress analysis), C (K solutions), D (reference stress), E (residual stress), F (material); API 650 2007 full text; BS 7910:2013 full text; 606 extracted API 579 tables | API 653 clause pointers (page is metadata-only, 3rd ed 2001 copy); 2007→2016/2021 annex crosswalk; verification status of the Annex C/E tables |
| Code (digitalmodel) | Level 1/2 metal loss (Parts 4/5/6), Part 12 dents, B31G/ModB31G/RSTRENG, DNV-RP-F101, Level 2 FAD tracer (`crack_fad.py`), legacy BS 7910 Annex M/P module, Paris-law growth, riser-joint Level 1 envelopes, Level 3 handoff, `ffs_decision`/`ffs_report`, two registered API 579 pipe workflows with Deckhand routes | API 579 Level 2 curve + PSFs, Part 9 Level 1 curves, Annex C cylinder K, Annex E residual, Annex F material model, growth threshold/LBB, any storage-tank model, API 653 Annex B, nonlinear/shell/buckling CalculiX, Level 3 acceptance checks |
| Solvers | gmsh (venv), CalculiX writer/parser (linear static, solids), ANSYS APDL generators (licensed host), in-house beam eigen-buckling | ccx on Windows hosts; shell/plastic/buckle/Riks in the INP writer; crack driving force in any unlicensed solver |
| Delivery | `docs/registry/workflows.yaml` + `examples/workflows/<id>/` + durable test + Deckhand `digitalmodel:<id>` route returning a report URL | — |

## Program (child issues of #1057)

| Wave | Issue | Work | Depends on |
|---|---|---|---|
| 0 | #2160 | Consolidate three FAD implementations; real BS 7910 tests | — |
| 0 | #2171 | Annex F material library: toughness estimation + true stress-strain curve | — |
| 0 | #1094 | Applicability-limit layer (flags, not silent numbers) | — |
| 1 | #2175 | Part 9 Level 2 workflow from inspection data: intake schema, API 579 curve + PSFs, Annex C cylinder K, Annex E residual | #2160 |
| 1 | #2157 | Part 9 from FE crack parameters: weldolet benchmark (ANSYS MAPDL, licensed host), growth with threshold, house-style report | plan r2.2 awaiting owner approval |
| 2 | #2172 | API 653 Annex B tank-settlement screen (planar tilt, out-of-plane, edge, bottom) | llm-wiki #913 task 1 |
| 3 | #2173 | CalculiX: shell sections, plasticity, NLGEOM, buckling, Riks, strain/eigen parsing | #2171 |
| 3 | #2174 | API 579 Level 3 tank-settlement workflow: strain limit, buckling margin, collapse, weld strain | #2172, #2173 |
| 4 | #2176 | Part 9 Level 1 screening curves | #2175 |
| 4 | #2177 | Growth with threshold, remaining life, LBB → `inspection_planning` | #2175 |
| 4 | #2178 | Spike: Level 3 crack driving force in CalculiX vs Code_Aster (#253) | #2173 |
| 4 | #1274 | Parts 3/7/10; pull Part 3 forward as the Part 9 toughness prerequisite | — |

Shared infrastructure (#2171, #2173) is filed separately so neither
application track buries it.

## Design decisions recorded

- **Settlement is displacement-controlled.** The Level 3 tank workflow applies
  the survey-derived bottom displacement profile and reports strain-limit and
  buckling margins first; the LRFD plastic-collapse check is reported but
  secondary; shell-to-bottom weld strain is a service criterion.
- **Edition citations.** Code cites the 2007 numbering we hold and the current
  edition via the crosswalk page (llm-wiki #913). No clause text in this repo.
- **Solver host.** CalculiX runs on a Linux host (candidate ace-linux-1); the
  workflow fails soft to a Level 3 handoff package when ccx is absent.
  Code_Aster (#253) is re-evaluated before committing to CalculiX only.
- **Insufficient data is a verdict.** The Part 9 intake returns a
  `measurement_sufficiency`-style list of missing items rather than guessing.

## Access notes

- The weldolet analysis (#2157) lives on the licensed Windows host (ace-win-1); its
  plan (`docs/plans/2026-09-24-issue-2157-crack-fe-assessment.md`) is
  uncommitted there. Review trail: Codex r1 MAJOR, Claude r1 MAJOR, Codex r2
  MAJOR, Codex r2.1 MINOR; r2.2 awaits `status:plan-approved`.
- The API 579-1 2016 licensed copy is FileOpen-protected; the Level 2 curve
  equation is to be confirmed by the owner in an authorised viewer (board card
  R01 on #2157).
