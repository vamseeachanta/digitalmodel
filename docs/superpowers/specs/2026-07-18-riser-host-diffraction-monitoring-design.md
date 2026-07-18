# Riser Host, Diffraction, Envelope, and Monitoring Design

**Decision date:** 2026-07-18  
**Program:** [digitalmodel #1602](https://github.com/vamseeachanta/digitalmodel/issues/1602)  
**Status:** user-approved issue decomposition; revised parent implementation plan still requires adversarial review and fresh approval

## Objective

Extend the riser program with one public, evidence-bound tracer:

```text
riser configuration
  -> drilling rig
  -> physical vessel and loading condition
  -> diffraction run and hydrodynamic response
  -> normalized six-DOF RAO
  -> vessel-motion boundary
  -> OrcaFlex riser execution
  -> strength and operability result
  -> versioned operating envelope
  -> telemetry-bound monitoring state
  -> validated alarm and risk evidence
```

## Architecture

The normalized engineering records will remain solver-neutral. Stable IDs and
canonical hashes—not names or solver object identifiers—will join the rig,
vessel, loading condition, hydrodynamic response, RAO, riser case, execution,
result, release, envelope, and monitoring records.

Existing engines will be reused:

- worldenergydata rig source and riser-handling records;
- digitalmodel hull catalogs, diffraction backends, RAO readers, and motion reconstruction;
- the OrcaFlex modular adapter and licensed Deckhand execution boundary;
- the existing strength, engineering-oracle, atlas, and envelope engines;
- the delivered digital-twin ingestion and monitoring capabilities.

OrcaFlex and the selected diffraction backend will be execution targets. Their
native files, defaults, and object names will not become the source of truth.

## Solver-Neutral Contracts

The revised parent interface will add three explicit envelopes:

1. `FloatingHostIdentityEnvelope`: rig identity, physical vessel/hull,
   loading condition, dimensions, mass properties, aliases, provenance, and
   public-eligibility evidence.
2. `HydrodynamicResponseEnvelope`: host/loading identity, six-DOF complex
   RAOs, frequency and heading grids, units, phase/sign conventions, reference
   point/frame, water properties, hydrodynamic matrices, source hashes, solver
   projection, validation evidence, usable range, and fidelity class.
3. `VesselMotionBoundaryEnvelope`: RAO and wave-case hashes, top-connection
   frame/reference, rigid-body transform, phase/time basis, imposed-versus-
   coupled intent, coverage, and unsupported disposition.

Every downstream bundle, request, receipt, engineering result, envelope, and
monitoring state will preserve these bindings.

## Issue Decomposition

### Naval-architecture and diffraction tracers

1. A child of #1203 will bind one public drilling-rig record to one physical
   vessel/hull and loading condition. Ambiguous or name-only joins will fail.
2. A child of #1203 will run one reviewed diffraction backend and emit one
   normalized, provenance-safe MODU hydrodynamic/RAO record. Proxy, benchmark,
   and actual-vessel fidelity will remain distinct.

### Monitoring tracers

3. A child of #1617 will bind one immutable HF release to one exact versioned
   operating envelope.
4. A child of #1617 will map public-safe offline telemetry to canonical rig,
   vessel, loading, riser, case, and envelope identities.
5. A child of #1617 will prove deterministic batch-replay and incremental
   monitoring-state equivalence.
6. A HITL child of #1617 will freeze and activate alarm persistence,
   hysteresis, acknowledgement, staleness, and escalation policy.
7. A HITL child of #1617 will validate any risk-reduction claim against a
   frozen baseline and measured detection/error evidence.

## Existing-Issue Amendments

- #1602 will receive a revised parent plan/interface and fresh approval gate.
- #1603 will reference producer-owned host/hydrodynamic/RAO records.
- #811 will select the rig/host pairing and preserve its identity.
- #138 will own generic vessel-motion-to-riser boundary semantics.
- #1609 will preserve the complete host/hydrodynamic/riser chain through the
  deterministic OrcaFlex bundle.
- Deckhand #568 will preserve hashes without interpreting hydrodynamics.
- #1611 will verify identity continuity before oracle evaluation.
- #1612/#1613 will retain the full chain and version sparse envelope coverage.
- #1604 will publish approved lineage, fidelity, and immutable release evidence.
- #1607 will re-audit the revised parent and expanded native hierarchy.

## Fail-Closed Safety Rules

- Bind every RAO to a specific physical host and loading condition.
- Declare coordinate frame, heading, rotational units, reference point, and
  phase convention; missing or contradictory conventions are terminal.
- Never present representative or proxy hydrodynamics as actual-vessel evidence.
- Validate rigid-body motion transfer analytically before riser coupling.
- Preserve sparse/failed/uncomputed envelope cells; never interpolate them into
  asserted coverage.
- Missing, stale, contradictory, or out-of-domain telemetry cannot produce a
  green monitoring state.
- Alarm outputs remain advisory and cannot emit automatic vessel-control,
  disconnect, or DP commands.
- No risk-reduction claim is allowed before reviewed detection, excursion,
  missed-event, false-positive, and false-negative evidence exists.

## Approval Boundary

The user approved this architecture and seven-issue decomposition on
2026-07-18. That approval authorizes creation and planning of the issue tree.
It does not approve the revised #1602 implementation plan or any child
implementation. Each issue will start at `status:needs-plan` and follow the
mandatory plan, adversarial review, user approval, TDD, artifact review, and
closeout workflow.
