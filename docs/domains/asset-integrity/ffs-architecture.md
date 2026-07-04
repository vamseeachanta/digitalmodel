# FFS Architecture — Canonical Path and Data Model

**Date:** 2026-06-27 · **Issue:** #1058 (FFS epic #1057)

## Decision
The modular **Phase-1 assessment pipeline** (`asset_integrity/assessment/`) is
the **canonical** Fitness-For-Service path. New code builds on it through the
single coordinator `assess_component()`. The legacy
`asset_integrity/common/API579_components.py` (driven by the `API579` engine
basename) is retained for backward compatibility but is **not** extended.

## The two paths

| | Canonical (Phase 1) | Legacy |
|---|---|---|
| Location | `assessment/` (`grid_parser`, `ffs_router`, `level1_screener`, `level2_engine`, `ffs_decision`, `measurement_sufficiency`, `ffs_coordinator`) | `common/API579_components.py` |
| Style | small, single-responsibility, unit-tested modules | one ~1000-line class, config/YAML-driven |
| Entry | `assess_component(component, grid)` → `FFSAssessmentResult` | `engine` basename `API579` → GML/LML methods |
| Tests | ~100 focused tests | legacy integration tests |
| Status | **canonical — build here** | frozen; do not extend |

## Canonical data model
`FFSComponent` (input) and `FFSAssessmentResult` (output) in
`assessment/ffs_coordinator.py`. `assess_component()` chains, in order:

```
GridParser -> FFSRouter -> Level1Screener -> Level2Engine
            -> FFSDecision -> MeasurementSufficiency
```

and returns one `FFSAssessmentResult` with: assessment_type (GML/LML),
level_reached, nominal/min/measured thickness, FCA, RSF/RSFa, Folias factor,
remaining life, verdict (ACCEPT/MONITOR/RE_RATE/REPAIR/REPLACE), a screening
re-rated pressure, the measurement-sufficiency status, and the raw stage dicts.
`FFSComponent`/`FFSAssessmentResult` are the single record shape that all
downstream consumers (reports, the design showcase, parametric acceptance
curves #1065, the indexed lookup / Deckhand API #1066, the field dashboard
#1067) share — no consumer re-wires the six modules or re-invents field names.

**No physics here.** The coordinator orchestrates and normalises only; every
value originates in a module with golden tests. Pipeline/structural method
modules (`corroded_pipe`, `dnv_rp_f101`, `plate_metal_loss_ffs`,
`assessment/level3_escalation`) are independent calculators consumed alongside
this model.

## Why additive, not a rewrite
Consolidation here is **adding** the coordinator + data model on top of the
validated modules — the existing ~100 Phase-1 tests and the legacy path are
untouched, so there is zero behaviour-change risk. Migrating the `API579`
engine basename onto the Phase-1 path (or deleting the legacy class) is a
separate, optional follow-up that would need its own characterization tests.
