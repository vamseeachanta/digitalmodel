# FFS Readiness Review — 2026-09-25

**Epic:** #1057 (Phase 4 adds the Level 3 / Part 9 program; this review adds the readiness demonstrators)
**Scope:** every fitness-for-service engine in `digitalmodel.asset_integrity` plus `fatigue/crack_growth`, scored on what a prospect can *run and see*, not on what exists in source.
**Companion notes:** `ffs-architecture.md`, `ffs-validation-record-2026-06-27.md`, `level3-and-part9-program-2026-09-25.md`

## Readiness scale

| Score | Meaning |
|---|---|
| R4 | Engine + tests + validation record + registered workflow with example + Deckhand route + public page |
| R3 | Engine + tests + validation record + workflow, but no route or page |
| R2 | Engine + tests + validation record, no workflow |
| R1 | Engine + tests, no validation record |
| R0 | Engine only, or tests that cannot fail |

A capability is *demonstrable* at R3 or better. Today only the legacy API 579 pipe path reaches R4, and it is the path the architecture note supersedes.

## Matrix

| Capability | Module | Standard | Tests | Workflow id | Route | Validation | Page | Score |
|---|---|---|---|---|---|---|---|---|
| Metal-loss coordinator (L1/L2, GML/LML, decision, report) | `assessment/ffs_coordinator.py` + 8 siblings | API 579 Pt 4/5, Fig 2.1 | ~90 | `ffs-metal-loss` (results not committed) | **no** | VR 2026-06-27 | showcase | R3 |
| Measurement sufficiency | `assessment/measurement_sufficiency.py` | API 579 Phase 1 | 15 | via `ffs-metal-loss` | no | VR (mention) | showcase, dashboard | R3 |
| Level 3 escalation handoff | `assessment/level3_escalation.py` | API 579 L3 | 14 | none | no | design doc only | showcase, dashboard | R1 |
| Legacy API 579 GML/LML | `API579.py` | API 579 | 20 | `api579-pipe-ffs-b314`, `-b318` | **yes (b314)** | none | none | R4* |
| Corroded pipe B31G / Mod B31G / RSTRENG | `corroded_pipe.py` | ASME B31G-2012 | 27 | none | no | B31G-VR + VR | showcase | R2 |
| RSTRENG 2D river-bottom | `rstreng_2d.py` | RSTRENG | 16 | none | no | R2D-VR | none | R2 |
| DNV-RP-F101 single / PSF / interacting | `dnv_rp_f101.py` | DNV-RP-F101 Pt B | 30 | none | no | VR (#1094 open findings) | showcase | R2 |
| Circumferential defect | `circumferential_defect.py` | API 579 Pt 5 Table 5.2 | 17 | none | no | CIRC-VR | none | R2 |
| Pitting | `assessment/pitting.py` | API 579 Pt 6 (charts not transcribed) | 36 | none | no | none | build-wave | R1 |
| Dents / dent-gouge | `dent_assessment.py` | API 579 Pt 12, B31.8 App R | 23 | none | no | none | build-wave | R1 |
| Crack-like flaw Level 2 FAD | `assessment/crack_fad.py` | BS 7910 Opt 1 (labelled API 579 L2, see #2157) | 11 | none | no | none | riser-joint explorer | R1 |
| Legacy BS 7910 flaw limits | `common/BS7910_critical_flaw_limits.py`, `common/fad.py` | BS 7910:2013 | 6 vacuous + 25 | none | no | none | none | R0 (#2160) |
| Crack growth (Paris) | `fatigue/crack_growth.py` | BS 7910 Cl 8 | 20 | none | no | none | none | R1 |
| Riser-joint FFS on real C-scans | `riser_joint_ffs.py` | B31G / F101 / BS 7910 | 12 | engine basename only | no | none | riser-joint explorer, build-wave | R1 |
| RBI screening | `rbi_screening.py` | API RP 580 qualitative | 27 | none | no | none | showcase, dashboard | R1 |
| Composite repair selector | `composite_repair.py` | ISO 24817, ASME PCC-2 | 8 | none | no | none | none | R1 |
| Inspection planning | `inspection_planning.py` | API 510/570/653 half-life | via registry | `inspection-planning`, `-atlas-query` | **no** (comment only) | none | none | R3- |
| Acceptance curves + lookup | `ffs_acceptance_curves.py`, `ffs_lookup.py` | inverts engines | 17 | none | no | none | none | R1 |
| RSF calculations | `rsf_calculations.py` | API 579 Table 4-4 | 28 | none | no | none | none | R1 |

\* R4 on the legacy path only. The canonical `ffs-metal-loss` path that the architecture note designates is unrouted.

**Fixtures.** Four anonymized riser UT C-scan grids with GML and flaw registers (#1293) live under `tests/asset_integrity/test_data/real_inspection/`. They are the strongest readiness asset in the module and are used by tests only.

**Weldolet / Part 9 FE work.** Nothing on any digitalmodel branch. The work is planned on #2157 (plan r2.2, ANSYS MAPDL CINT, licensed Windows host (ace-win-1), plan document uncommitted there). It is not yet part of the demonstrable surface.

## Findings

1. **The demonstrable surface is one legacy route.** Nineteen engines, one Deckhand entry point, and it is the path the canonical coordinator replaced in #1075.
2. **Validation records stop at metal loss.** Pitting, dents, cracks, RBI, composite repair, riser joints and crack growth have tests but no record against a published example, which the epic's validation gate requires before release.
3. **Real data is not shown.** The #1293 C-scan fixtures never reach a workflow, example or page.
4. **The field-to-office story is not wired end to end.** Sufficiency, RBI, repair selection and interval exist as separate engines with no single example that chains them.
5. **Capability map says `ffs: unknown`.** `docs/capability-map/capabilities-added.yml` has no per-module FFS entries.

## Readiness demonstrators (filed as children of #1057)

| Issue | Demonstrator | Lifts to |
|---|---|---|
| #2180 | Route the canonical `ffs-metal-loss` and `inspection-planning` workflows in Deckhand; commit the `ffs-metal-loss` results; retire b314 as entry point | R4 for the coordinator and interval |
| #2181 | `pipeline-corroded-defect-screen`: one river-bottom UT grid through B31G / Mod B31G / RSTRENG / RSTRENG-2D / DNV-F101 / circumferential, side by side, with the existing validation records as goldens | R4 for five engines |
| #2182 | `api579-pitting-screen` and `api579-dent-screen` workflows with validation records against published Part 6 / Part 12 examples | R1 → R4 |
| #2183 | `riser-joint-ffs` registered on the #1293 real C-scan fixtures: acceptance envelopes, string placement, fleet roll-up, report | R1 → R4, real data shown |
| #2184 | `crack-fad-level2-handoff`: flaw register → Level 2 FAD (curve labelled correctly as BS 7910 Option 1 until #2175 lands) → Level 3 handoff package; validation against a BS 7910 worked example | R1 → R3 honestly |
| #2185 | `ffs-campaign-demo`: sufficiency → RBI rank → repair selector → next interval on one campaign input; readiness matrix published under `docs/api/ffs/` and `ffs` entries in the capability map | field-to-office chain demonstrable |

Each demonstrator carries its own validation record as an acceptance criterion; no separate "write validation records" issue exists.

## Sequencing with the Level 3 / Part 9 program

Demonstrators #2180 to #2185 are Wave 0/1 work: they wire what exists and need no new physics. They should land before the tank and crack tracks so that the readiness story is true when those tracks add to it.
