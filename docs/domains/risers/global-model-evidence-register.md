# Riser global-model qualification evidence register

## Summary

Production qualification is **NOT_EVALUATED**. Native execution is **NOT_RUN**.
Independent quality-manager and deputy appointments are **UNCONFIRMED**.
No target-version eligibility, design acceptance basis, model fingerprint or
signed evidence appendix is supplied by this register. Passing offline tests
cannot clear these gaps. The requirement basis is issue #2530, Detection and
Acceptance, and the [global-model knowledge pack](../../../src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md),
revision 2 (2026), sections 1, 5, 8 and 10.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-22 | Recorded missing qualification evidence, retained test coverage and a scalar example to prevent offline results being read as production clearance. |

## Scope and source authority

The [machine-readable register](global-model-evidence-register.json) records a
historical snapshot at `3554b14fde67f44b1c52b81b6c72106282eaff29`.
Every cited source and test file has a SHA-256 digest of its retained Git blob.
These are source fingerprints, distinct from the absent production model
fingerprint. Later source changes do not update or invalidate this historical
inventory; they require a separately reviewed reconciliation and new snapshot.
This register does not describe a current production package.

AQG r0, *Analysis QA gates*, revision 0, sections 3–10, is **REFERENCED_ONLY**:
its primary text is not present in the inspected repository. Gate labels and
proposed applicability below are traced to the knowledge pack, section 5.
No AQG numerical limit is copied, inferred or approved here. The quality manager
must confirm the primary revision, clause mapping and acceptance criteria before
using them. A checklist of required records is not an evidence record.

The scalar [gate evaluator](../../../src/digitalmodel/drilling_riser/global_model/qualification.py)
compares one demand/reference pair. It does not verify appointments, native
versions, applicability, model identity, component coverage or signatures.
The [native wrapper](../../../src/digitalmodel/drilling_riser/global_model/orcaflex_run.py)
requests a version in its docstring; that request does not establish eligibility.
Its `as_analysed_sections` routine reads line sections, not every native object.
These boundaries explain why a scalar PASS is insufficient.

## Appointments and clearance authority

| Required appointment | Status | Evidence needed |
| --- | --- | --- |
| Independent riser and mooring quality manager | UNCONFIRMED | Maintainer-approved appointment reference, competence and scope, independence from the producer, clearance authority and signed acceptance. |
| Independent deputy quality manager | UNCONFIRMED | Separate appointment reference, competence and scope, independence record, delegated authority and signed acceptance. |

Table A. Role labels in the knowledge pack section 8 do not establish appointments.

Public records should use opaque appointment and review references. Authorized
reviewers must be able to resolve and check those records. No appointment or
independence is asserted by this document. The maintainer supplies the appointment
records; the confirmed quality manager reconciles gates and clears evidence.

## Gate and requirement reconciliation

All rows remain **NOT_EVALUATED**, including rows requiring native evidence.
Native records themselves remain **NOT_RUN**. `acceptance_basis` is null and
`evidence_records` is empty in every register row. Each row names its responsible
role, cited sources, missing evidence and required record fields.

| Requirement | Proposed gates | Missing evidence and next record |
| --- | --- | --- |
| `appointments` | G0, G5 | Confirm both independent appointments and their clearance scope. |
| `version` | G0, G1 | Declare eligible target program and binding versions, intended use and limitations; retain actual native version readback. |
| `aqg` | G0–G5 | Obtain the primary basis; map every applicable clause to a criterion, record and independent disposition. |
| `inputs` | G0 | Retain master and includes, units, geometry, selector/null checks and input review against the production model fingerprint. |
| `native_load` | G1 | Retain version-bound load log, warnings and expected-versus-observed object counts. |
| `hand_comparison` | G2 | Independently derive reference values, reconcile signs and units, compare native responses and resolve reference applicability. |
| `offset_sweep` | G2 | Declare headings, offset sequence and design maximum; retain convergence, reactions, first-step hand comparison, monotonicity and compression dispositions. |
| `all_objects` | G2 | Reconcile every expected and native object, properties, connections and response ranges; a section-only table cannot close this row. |
| `short_dynamics` | G3 | Retain production time step, requested/actual stop time, logs, finite response checks, static/dynamic comparison, compression disposition, final image and timing. |
| `diffraction` | G4 | Establish RAO origin and upstream qualification, or document an independently accepted inapplicability reason. Applicability remains UNDETERMINED. |
| `appendix` | G5 | Bind gate records, criteria, images, evidence digests and signatures to the same target version and production fingerprint. |
| `directional_stroke` | G2, G3 | Declare datum, extension and collapse travel, tensioner travel, centering assumption, sign, units and demand history. |
| `joint_angle` | G2, G3 | Declare joint-specific limits, axes, signs, operating condition and channel definition. Connection-relative rotation does not establish global inclination. |
| `clearance` | G2, G3 | Declare each obstruction, opening geometry, pivot, swept envelope, motions and clearance criterion. |

Table B. Proposed gate membership follows the knowledge pack section 5; missing
criteria remain required inputs rather than guessed acceptance limits.

G4 cannot become NOT_APPLICABLE merely because RAO provenance is absent. A new
diffraction-derived dataset requires its upstream gate record. Existing-data
applicability also requires an explicit basis and review. The appendix must carry
that decision, including any limitations, for the same model fingerprint.

### Complete component coverage

A future evidence record must compare the expected generated object set with the
native object set for each configuration. Each object needs an identifier, type,
properties and units, connections, applicable response/range checks and a reviewed
disposition. Both missing expected objects and unexpected native objects require
disposition; equal counts alone do not establish matching sets.

The [builder](../../../src/digitalmodel/drilling_riser/global_model/build.py)
and [specification](../../../src/digitalmodel/drilling_riser/global_model/spec.py)
define the configuration-dependent expected set: line types and lines, vessel and
vessel type, tension ring, slip joint, tensioner winches and flex-joint connections.
Released-body lines, string-top/tensioner objects, conductor and foundation links,
hang-off springs, variable stiffness and damping must be included when generated.
Open-water configurations need their own expected topology, including the frame,
rotary and release interface. These are inventory categories, not an assertion
that native objects have been enumerated. No all-object extractor is added here.

### Design-basis boundaries

The [stroke checks](../../../src/digitalmodel/drilling_riser/postprocess/checks.py)
`telescopic-joint` implementation (`tj_stroke`) uses travel from the datum in the
direction of excursion. `tensioner_stroke` assumes centered travel. The design basis
must justify these conventions or supply the applicable directional criteria.
The [channel extractor](../../../src/digitalmodel/drilling_riser/global_model/w5_channels.py)
uses different drilling and open-water stroke definitions; global elevation alone
is not an interchangeable stroke demand.

The [clearance helper](../../../src/digitalmodel/drilling_riser/postprocess/clearance.py)
uses idealized straight-member geometry. It does not establish actual opening
geometry, obstructions, swept envelopes or design allowables. Joint-angle and
clearance records must identify what physical quantity each channel represents.
Missing design data leaves the corresponding requirement NOT_EVALUATED.

The [hand-reference interface](../../../src/digitalmodel/drilling_riser/global_model/hand_checks.py)
is limited by its boundary conditions and contents conventions. The positive-bore
drilling stack discrepancy tracked by #2532 must be reconciled before that
reference is used for gate clearance. External consumer provenance remains separate
under #2531. No claim is made here that either related issue remains unresolved
in all later revisions; evidence must identify the convention at its own pin.

## Retained offline and native test coverage

The following inventory reads test syntax at the retained revision. Named
functions, explicit solver markers and direct `available()` calls are separate
counts. **named functions are not parametrized case counts**. MIXED means helper
functions share a file with explicitly solver-marked functions. A direct
availability probe can run at collection before marker deselection; selecting
`-m 'not solver'` alone does not avoid that query.

| Retained test file | Named functions | Solver-marked functions | Direct availability calls | Classification | Execution |
| --- | --- | --- | --- | --- | --- |
| `test_batch_params.py` | 8 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_campaign.py` | 12 | 3 | 4 | MIXED | NOT_RUN |
| `test_damping_offset_current.py` | 15 | 2 | 2 | MIXED | NOT_RUN |
| `test_event_models.py` | 8 | 2 | 2 | MIXED | NOT_RUN |
| `test_events.py` | 7 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_fatigue_channels.py` | 7 | 1 | 1 | MIXED | NOT_RUN |
| `test_hand_checks.py` | 10 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_hang_off.py` | 13 | 3 | 3 | MIXED | NOT_RUN |
| `test_irregular_wave.py` | 5 | 1 | 1 | MIXED | NOT_RUN |
| `test_nonlinear_vessel_foundation.py` | 16 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_open_water.py` | 24 | 8 | 9 | MIXED | NOT_RUN |
| `test_open_water_campaign.py` | 9 | 1 | 1 | MIXED | NOT_RUN |
| `test_orcaflex_dynamics.py` | 4 | 4 | 1 | NATIVE_ONLY | NOT_RUN |
| `test_orcaflex_solver.py` | 2 | 2 | 1 | NATIVE_ONLY | NOT_RUN |
| `test_parallel_runner.py` | 17 | 2 | 1 | MIXED | NOT_RUN |
| `test_qualification.py` | 4 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_spec_and_build.py` | 17 | 0 | 0 | NO_DIRECT_NATIVE_MARKER | NOT_RUN |
| `test_statics_path.py` | 21 | 2 | 2 | MIXED | NOT_RUN |
| `test_tensioner_representation.py` | 9 | 2 | 2 | MIXED | NOT_RUN |
| `test_w5_channels.py` | 7 | 1 | 1 | MIXED | NOT_RUN |

Table C. Historical syntax inventory; NOT_RUN is the execution status of the
inventoried tests in this contribution, not a statement about earlier runs.
The earlier knowledge-pack execution record remains historical evidence only.

NO_DIRECT_NATIVE_MARKER describes syntax, not transitive import or fixture safety.
The JSON retains named functions, explicit marker membership and probe line
numbers so mixed coverage is visible without importing the files. No global-model
test module is collected by the documentation contracts. Splitting mixed files
and replacing collection-time availability queries remains follow-up work; no
solver check, marker or fixture is weakened by this documentation change.

Run the independent documentation contracts with the repository's normal pytest
configuration and a writable external scratch directory:

```sh
uv run --no-project --with pytest python -m pytest tests/docs/test_riser_global_evidence_register.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/riser-evidence-tests"
```

The tests use only standard-library parsing, JSON, decimal arithmetic and read-only
Git blob access, together with pytest. They do not import or query a licensed
binding. They check register completeness, unresolved states, retained source
fingerprints, complete test-file membership, the displayed inventory, local links
and the worked example. Passing these contracts establishes documentation
consistency, with no native qualification credit.

## Worked example: scalar comparison with missing qualification

Inputs are constructed arithmetic values: demand **104 N**, reference **100 N**,
and caller-selected tolerance **5.0 %**. They are illustrative required inputs,
not a design acceptance limit or a value from a standard. The method is the signed
deviation equation documented by `qualification.py::evaluate_gate`:

```text
deviation = (demand - reference) / abs(reference)
          = (104 - 100) / 100
          = 4 / 100 = 0.04 = 4.0 %
```

The scalar arithmetic meets the caller-selected tolerance and gives scalar PASS.
Target-version evidence, appointments, native gate records, the all-object
inventory and the fingerprint-linked appendix remain absent. The qualification
answer is therefore **NOT_EVALUATED** and native execution remains **NOT_RUN**.
A missing or zero reference cannot establish even this scalar comparison, as
specified by the evaluator docstring. The independent calculation is also written
out in the documentation test's docstring; its expected answer is not obtained
from the implementation under test.

## Evidence needed before clearance

A future appendix must retain the model fingerprint and eligible target version,
the actual binding version, design-basis references, applicability and criteria
for each gate, native logs and observations, independent calculations, complete
object reconciliation, images, evidence digests, appointment references and
review dispositions/signatures. Native execution requires separate authorization;
none is requested or performed in this documentation change.

Every missing applicable record prevents qualified PASS. A future not-applicable
record requires its reason and independent disposition. Failed checks retain
their failure and corrective action. Review of the documentation contracts alone
cannot substitute for independent engineering clearance.
