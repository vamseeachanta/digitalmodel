# GTM Parametric Demos

The `examples/demos/gtm/` deliverables turn one-off engineering demo reports (demo_01–05) into
repeatable, parameter-driven client deliverables. This glossary fixes the language used across the
parametric-runs work (#591 umbrella; #632–#636 per demo).

## Language

**Sweep Config**:
The analyst-facing input that declares *what matrix of cases to compute* for a demo — the
cross-product of engineering axes (pipe sizes, span lengths, current velocities, gap ratios) plus
the physical constants and screening thresholds. Lives at `inputs/demo_0N_*.yml`.
_Avoid_: parameter file, config, run yaml (when the sweep matrix is meant).

**Sweep Axis**:
A single varied dimension in a Sweep Config, expressed as a **list**. A length-1 list is a frozen
axis (held at the user's most-likely value); a length-N list is swept. The Run computes the
cross-product of all axes — so the user expresses one-factor-at-a-time or full-factorial purely by
list lengths, with no study-method modes in the engine.
_Avoid_: parameter, variable, dimension (when the list-valued sweep input is meant).

**Prospect Intake**:
The client-facing input that declares *who a deliverable is for and the single design point it
concerns* — company/contact, NDA, target demo, one structure body, environment, and delivery/URL
gating. Validated and materialized by `prospect_adapter.py` (`prospect-*.yaml` schema).
_Avoid_: client config, intake yaml (be specific), sweep (a Prospect Intake is not a Sweep Config).

**Screening**:
A pass/flag classification that identifies cases *warranting* detailed analysis — not an acceptance
check. Demo_01 is a DNV-RP-F105 *screening* tool, explicitly not a full assessment (no axial force,
soil, wave, or fatigue).
_Avoid_: assessment, acceptance check, qualification (these imply a rigor the screening does not have).

**Run**:
A single execution of a demo over one Sweep Config, identified by a `run_id`, producing one set of
parametric results (tabular cases + manifest + report).
_Avoid_: job, batch, analysis (when a single identified execution is meant).

**Baseline Run**:
The default Run whose Sweep Config reproduces the original hardcoded matrix exactly; the golden
regression oracle and the run seeded into the Results Store for the live demo.
_Avoid_: default run, non-parametric run (the Baseline is itself a parametric Run, just the default one).

**Results Store**:
The queryable SQLite cache of all Runs' inputs and outputs (`runs` + `cases` tables) that powers
instant case lookup during a demo. A rebuildable derived index, not the source of truth — git holds
the yaml inputs and per-run CSV/manifest; the store is regenerated from them.
_Avoid_: database, results db (be specific — it is a rebuildable cache, not the canonical record).
