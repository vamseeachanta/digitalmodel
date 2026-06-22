# Parametrics plan (preparedness → systematic sweeps → atlases)

> digitalmodel #943, under the solver-run preparedness epic #938 (c).
> Follows (a) deployable programs — OrcaWave/OrcaFlex/AQWA/ANSYS, #939/#940 — and
> (b) the use-case registry + templates (#941/#942).

## 1. Goal

For each prepared use-case (`src/digitalmodel/usecase_registry/registry.yaml`),
run systematic parameter sweeps to produce **atlases** — pre-computed grids /
surrogates that answer common questions instantly — and to explore design space.
Out-of-range queries fall back to a **24h-SLA licensed run** (the #794 pattern).

## 2. Reuse — do NOT add a 4th parametric engine

Parametric/batch machinery already exists per solver. The plan **delegates** to
these; it does not replace them:

| Solver | Existing parametric/batch primitive |
|---|---|
| OrcaFlex | `orcaflex/batch_parametric.py` — `ParametricStudy` (`ParameterSweep` → `generate_case_matrix()` / `generate_case_configs()`), `solvers/orcaflex/*_parallel.py` |
| OrcaWave / AQWA | `hydrodynamics/diffraction/{orcawave,aqwa}_batch_runner.py` (`BatchJobConfig` job lists, parallel) + `fleet_decks.emit_orcawave_batch_config` (#929) |
| ANSYS | `ansys/batch_runner.py` — `ParameterSweep`/`BatchConfig` (APDL template + `{param}` substitution, full-factorial / zip) |

> **Consolidation opportunity (not this issue):** these are parallel sweep
> abstractions. A future thin `ParametricStudy` interface could front all three
> (one `ParameterSweep` model → per-solver case generation), but only once each
> is exercised. Flagged so we don't grow a 5th.

## 3. The parametric study, end to end

```
usecase_registry entry  (solver, basename, operation, base template)
        │
        ▼  parametric spec: sweep variables + grids over the base template
   case-matrix expansion  (per-solver: ParametricStudy / BatchConfig / job list)
        │
        ▼
   execution:
     - analytical / offline cases  → run on Linux now
     - licensed solver cases       → dispatch through the licensed-run lane in
                                      bulk (metadata-only results; #489 returns
                                      the numeric bundle)
        │
        ▼
   collect  →  ATLAS (grid or surrogate, per #794/#870)
        │           └─ out-of-range query → 24h-SLA licensed run
        ▼
   post-process: Linux postproc orchestrator (#928) + envelope generator (#930)
```

## 4. Per-use-case sweep variables (starting set)

| Use-case (registry id) | Swept parameters | Atlas output |
|---|---|---|
| `orcawave-fpso-parametric-deck` | LOA × beam × draft × heading × period; QTF on/off | RAO / added-mass / damping atlas |
| `riser-scr` | water depth × top tension × hang-off angle × current | tension / stress envelope |
| `mooring-fatigue` | line type × pretension × Hs/Tp scatter | fatigue-life surface |
| `free-span-f105` | span length × current × gap ratio | VIV fatigue-damage surface |
| `jumper-installation` | Hs × Tp × water depth × payload | operability / DAF surface |
| `ansys-padeye` | pad thickness × hole dia × sling angle × load | UC / stress surface |

(Each maps to a registry entry; ready entries can sweep now, partial/planned
after their template lands.)

## 5. Phasing

- **P0 — spec + one pilot per solver (offline/dry-run).** Define the parametric
  spec (sweep vars + grid) over a `ready` registry template; expand via the
  solver's existing case generator; run offline/dry-run. Pilots: one OrcaFlex
  (`free-span-f105`) + one diffraction (`orcawave-fpso-parametric-deck`).
- **P1 — lane-dispatched licensed sweeps.** Bulk-dispatch the case matrix through
  the licensed-run lane; collect numeric bundles (#489) and postprocess (#928).
- **P2 — atlases + bot wiring.** Build grid/surrogate atlases (#794/#870);
  interpolate for instant answers; out-of-range → SLA run.

## 6. Dependencies / sequencing

- Registry (`usecase_registry`, this PR) — the enumeration parametrics iterate.
- Template gap-fill (#941/#942) — `partial`/`planned` use-cases need a `ready`
  template before they can be swept.
- Lane result-return (deckhand #489) — required for P1 (numeric bundles back).
- Atlas infra (#794/#870) — the P2 surrogate/grid store.

## 7. Non-goals

- No new parametric engine (reuse §2).
- No licensed solves in CI (dry-run / offline only; real solves on the host).
- Atlas surrogate modelling choices are deferred to P2.
