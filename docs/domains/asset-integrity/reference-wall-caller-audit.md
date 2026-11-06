# Equivalent-LTA reference-wall caller audit

## Summary

The simplified local-loss engine uses required wall as its outside-flaw
reference. The dedicated pitting workflow checks measured background after
FCA; the other callers do not establish that prerequisite. Local RSF acceptance
and an all-clear Folias applicability record do not qualify whole-component
acceptance. This is discovery for issue #2345: assessment algorithms, catalog
status and published-case qualification are unchanged.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-19 | Inventory reference-wall evidence and reproduce caller gaps to bound subsequent safeguards. |

Table 1. Audit issue history.

## Scope and sources

Slice 1 inventories the direct engine and equivalent-LTA helper callers.
Slice 2 adds synthetic missing-background, sub-reference-background, FCA and
reference-equality regressions. Slice 3 follows the coordinator through its
final decision and distinguishes the existing Level 1 repair gate from a
reference-wall gate. No shared-engine modification is authorized by this audit.

Sources read for this audit:

- Issue #2345, *Audit reference-wall preconditions across equivalent-LTA
  callers* (2026): scope and synthetic counterexample.
- Issue #2182 and PR #2341: workflow scope, existing background gates and
  separation of synthetic formula checks from published-case qualification.
- [Pitting qualification record](pitting-validation-2026-10-09.md),
  *Formula anchor* and *Published cases and applicability* (2026): synthetic
  geometry and the existing background-after-FCA prerequisite.
- [Uniform-loss source audit](uniform-loss-source-audit-2026-10-06.md) (2026):
  unresolved Folias source binding and incomplete Part 5 qualification.
- API 579-1/ASME FFS-1, *Fitness-For-Service*, 2021, Part 5 §5.4, local metal
  loss assessment, and Part 6, pitting assessment: declared method references
  in the implementation, not independently checked licensed clause content.

No licensed coefficient, table or worked-example value is reproduced here.
The background check is the existing workflow prerequisite, not a newly
verified transcription of a standard. Full Part 5 and Part 6 qualification
remain **NOT_EVALUATED**.

## Caller inventory

Names below are relative to `src/digitalmodel/asset_integrity/`.
The search covers Python sources in `src/` and `scripts/`, including public
re-exports; no additional constructor or helper call sites were found by name.
Dynamic external callers and aliases outside that search are not qualified.

| Call site | Reference and FCA handling | Outside-flaw evidence | Disposition implication |
|---|---|---|---|
| `assessment/level2_engine.py`, `_evaluate_lml` | Sets `t_c` to supplied `t_min_in`; no FCA input. | No outside-flaw wall input or background check; nominal wall segments the flaw. | `ACCEPT` checks local RSF only; Folias applicability is not a reference-wall check. |
| `assessment/level2_engine.py`, `_evaluate_gml` | Uses the same required-wall reference in the mean-wall ratio; no FCA input. | General loss has no distinct outside-flaw population in this calculation. | Inventory context only; do not apply the pitting population mask to GML. General-loss qualification is separate. |
| `assessment/pitting.py`, `assess_pitting_level2_equivalent_lta` → `Level2Engine` | Supplies `t_min_in` unchanged; synthetic LTA uses mean pit wall. No FCA input. | Characterization has background median, but no minimum or explicit background count; the engine sees only the LTA grid. | Passing local RSF cannot establish missing or thin background. |
| `assessment/pitting.py`, `assess_pitting` → equivalent-LTA helper | FCA reaches Level 1 only; Level 2 receives the original characterization. | Original grid is available but no reference-wall gate is applied. | Returns stage results without a qualified component disposition. |
| `assessment/ffs_coordinator.py`, `assess_component`, GML/LML → `Level2Engine` | Computes required wall from the design-code screener. Raw grid goes to both levels; FCA is retained as metadata. | Original grid is available; no background-after-FCA gate is applied. | Existing Level 1 failure can prevent acceptance, but positive FCA can leave a passing disposition without net-wall evidence. |
| `assessment/ffs_coordinator.py`, `assess_component`, PITTING → equivalent-LTA helper | Required wall comes from the screener. FCA reaches the pitting Level 1 screen only; Level 2 receives the original characterization. | No background-after-FCA gate is applied; characterization and raw grid are both available. | The synthetic counterexample ends in `REPAIR` because Level 1 fails, despite local Level 2 `ACCEPT` and all-clear applicability. |
| `assessment/pitting_workflow.py`, `_levels` → equivalent-LTA helper; `_assess` → `_applicability` | Required wall is explicit input; FCA is deducted once from the LTA characterization before Level 2. | `_applicability` selects non-pitted grid readings and compares their minimum minus FCA with required wall. Missing background raises `pitting.no_background`; thin background raises `pitting.background_below_tmin`. | Either flag overrides local RSF with `ESCALATE`; equality at the reference wall does not raise these flags. Synthetic formula qualification only. |

Table 2. Direct engine/helper calls and the prerequisite each establishes.

`assessment/ffs_workflow.py`, `FFSWorkflow.router`, delegates to
`assess_component` and serializes its result without adding a reference check.
`uniform_loss_diagnostic.py`, `diagnose_case`, also delegates with forced LML;
it already labels output `UNQUALIFIED_BASELINE` and
`NO_SOUND_REGION_IN_GRID`. `assessment/pitting.py`, `router`, delegates to the
dedicated workflow and inherits its gates. Public re-exports in
`assessment/__init__.py` add no checks. The frozen legacy
`common/API579_components.py` does not call this engine and is not a qualified
replacement.

## Preconditions and independent calculations

For the dedicated workflow, let B be finite readings outside the existing pit
population and let f be FCA. If B is empty, the reference is
**NOT_EVALUATED**. Otherwise the existing prerequisite is
`min(B) - f >= t_min`. Its source is the pitting qualification record's
*Published cases and applicability* section, not nominal wall or the median.
The pit threshold only defines the population; it does not establish that the
remaining background meets the required wall.

The new test module cites issue #2345's synthetic inputs: nominal wall
0.500 in, required wall 0.470 in, pit wall 0.440 in and pitch 1.000 in.
Its diameter input is 24.000 in from the existing qualification record's
synthetic anchor. The selected lattice preserves separated corner pits when
background is present; the missing-background case contains only pit readings.
The test oracle gives:

| Case | Independent background calculation | Reference status |
|---|---|---|
| Missing | No non-pitted readings; no nominal substitution | NOT_EVALUATED |
| Thin | 0.460 in minus zero FCA is below 0.470 in | FAIL |
| Thin after FCA | 0.500 in minus 0.040 in is 0.460 in, below 0.470 in | FAIL |
| Equality control | 0.470 in minus zero FCA equals 0.470 in | PASS |

Table 3. Synthetic prerequisite oracles, independent of the strength engine.

No Folias coefficient is needed to predict the local acceptance witness.
The existing implementation documents `RSF = Rt / (1 - (1 - Rt) / Mt)`.
For `0 < Rt < 1` and `Mt >= 1`, algebra gives `Rt <= RSF <= 1`.
Here `Rt = 44/47 > 9/10`, since `440 > 423`. Consequently the unadjusted
local RSF exceeds the issue's input RSFa even when the reference is not
established. This is an independent bound on the implemented equation, not
source qualification of that equation.

The coordinator is checked with required wall supplied directly to its
screener in the test. This isolates the prerequisite from pressure-code
coefficients. In the issue counterexample, both forced LML and PITTING end in
`REPAIR`: raw Level 1 fails, so numeric Level 2 acceptance is insufficient for
a final passing disposition. The tests retain that distinction.

A further hand-constructed LML case uses required wall 0.400 in and FCA
0.100 in with the same measured pit/background walls. Background after FCA is
0.360 in and pit wall after FCA is 0.340 in, both below the supplied required
wall. Raw pit wall exceeds required wall, so raw Level 1 passes; unadjusted
`Rt >= 1` caps local RSF at unity. The coordinator returns `ACCEPT` with
all-clear applicability. This reproduces omitted FCA handling; it is not a
qualified fitness verdict. The new regression documents the gap without
changing production behavior.

## Verification and interpretation

`tests/asset_integrity/test_reference_wall_audit.py` contains 25 discovery
cases: four cases for each of the direct LML engine, equivalent-LTA helper,
pitting convenience wrapper and dedicated workflow; eight coordinator cases
across forced LML/PITTING; and one positive-FCA LML disposition witness.
Expected values are hand calculations in the test docstrings. The required
wall is a test input, not an inferred code-qualified design value.

These tests record present behavior, including gaps. A later safeguard change
must revise the corresponding witness and inventory row together. A passing
test that reproduces numeric `ACCEPT` is evidence of the limitation, never
component acceptance or method qualification. The tests do not authorize
catalog promotion; readiness remains separate from formula checks.

## Later slices

Further implementation requires a separately scoped change:

- Define explicit outside-flaw evidence and missing-evidence semantics for
  direct LML callers; a background median alone cannot prove a minimum.
- Define consistent FCA ownership for the convenience helper and coordinator
  before adding caller gates, including net-wall and no-background behavior.
- Replace characterization witnesses when those safeguards are implemented;
  preserve the existing Level 1 final-decision distinction.
- Obtain edition-matched public examples and verify method applicability
  before claiming Part 5/Part 6 qualification or changing catalog readiness.
