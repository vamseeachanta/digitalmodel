# S-lay Table B reference basis

## Summary

Touchdown position, sag-bend strain and stiff closure remain `NOT_EVALUATED`
under #2608. The inspected public cable source supplies background equations,
but does not establish stiff-pipe reference answers or comparison criteria.
Static QA now retains the missing evidence for each row as `reference_basis`.
The way forward is a published stiff-pipe method, reproducible case inputs and
an applicable published comparison criterion for each row.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Record the inspected source, missing evidence and worked reporting example so background equations cannot imply acceptance. |

## Source review

The institutional open copy of
[*Dynamics Modeling and Loads Analysis of an Offshore Floating Wind Turbine*](https://www.osti.gov/servlets/purl/921803),
NREL/TP-500-41958, November 2007, was retrieved on 2026-10-22 with HTTP 200.
Section 2.5, printed page 36, explicitly neglects individual-line bending
stiffness. Equation (2-38), printed page 39, gives grounded cable length;
equations (2-43a,b), printed page 41, give elastic cable coordinates.
These methods describe a cable. They do not supply a stiff-pipe Table B
solution or any of the missing comparison bands.

The source is already used for the repository's
[independent cable benchmark](../../../benchmarks/catenary_closed_form/README.md).
That benchmark's arithmetic checks and its chosen comparison tolerances do not
qualify these S-lay rows. An attempted public line-stress help URL returned a
help shell rather than inspectable technical content and is not used as evidence.
No standard allowable is reproduced or inferred. This review covers the inspected
material; it does not assert that a suitable reference cannot exist elsewhere.

## Evidence required before evaluation

| Row | Quantity and units | Missing basis | Why the cable source is insufficient |
| --- | --- | --- | --- |
| `touchdown_x` | Pipe-axis touchdown global X, m | Published stiff-pipe reference inputs and answer; common coordinate frame; contact and coated-radius definition; criterion with edition and clause or table | Cable coordinates omit the pipe's bending stiffness and do not establish its touchdown position. |
| `sag_bend_strain` | Maximum longitudinal strain, dimensionless | Published stiff-pipe reference inputs and answer; sag-bend region; bending-only or total strain and fibre definition; strain criterion with edition and clause or table | A cable model without bending stiffness does not supply pipe bending strain. |
| `stiff_closure` | Stiff-pipe length-closure residual, m | Published closure method and reference inputs; residual sign; unstretched length, axial stretch and end-motion definitions; residual criterion with edition and clause or table | Grounded cable length does not qualify stiff-pipe closure or tensioner travel. |

The implementation deliberately leaves `acceptance_source` and
`numeric_tolerance` null for each row. A reference value, a source-shaped mapping
or a finite reading supplied by a caller cannot change this policy. No proposed
numerical band is activated. Existing layout inputs and the evaluated workflow
checks retain their current meanings; their design-basis follow-up is #2625.
A future implementation needs a cited method, independent expected answers and
boundary tests before any pending row can return `PASS` or `FAIL`.

## Worked reporting example

All inputs below are synthetic, **assumed, based on previous experience**.
They are selected to exercise reporting, not to represent a solved installation.
The existing workflow checks are documented in the static-QA table in #2560 and
implemented in `slay_probe.evaluate_static_acceptance`. Their bands are existing
workflow choices; this example does not attribute them to a published standard.

```python
from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import (
    evaluate_static_acceptance,
)

# Synthetic arithmetic inputs; the independent enumeration follows this block.
inputs = dict(
    wire_length=10, end_a_reaction=1, last_support_force=0,
    grounded_tension=[0], overbend_strain=0, lift_off_arc=0,
    departure_angle=0, reference=dict(lift_off_arc=0, departure_angle=0),
)
result = evaluate_static_acceptance(**inputs)
assert result["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}
assert result["status"] == "accepted with 3 rows not evaluated"
assert result["fully_accepted"] is False
assert result["reference_basis"]["rows"]["touchdown_x"]["acceptance_source"] is None

# A negative grounded tension fails the existing sign check despite pending rows.
inputs["grounded_tension"] = [-1]
rejected = evaluate_static_acceptance(**inputs)
assert rejected["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
assert rejected["status"] == "rejected"
```

Independent hand enumeration: wire length lies inside the existing wire band;
End A reaction is positive; zero overbend strain and zero last-support force
satisfy S1; matching lift-off and departure references give zero differences;
and grounded tension is nonnegative. These are six passing checks. The three
pending rows have no published criteria, so the answer is six passes, no failures
and three unevaluated rows, with full acceptance false. Replacing grounded
tension with a negative value produces five passes, one failure and the same
three unevaluated rows. The test executes this exact example and compares its
counts with this hand enumeration.

## Known limits and next steps

This change establishes evidence-gap reporting only. It does not qualify native
readback, mesh convergence, S1 design adequacy or installation acceptance. No
licensed solver is started, imported or queried. The three rows remain open
until the evidence above is available or the maintainers decide to drop them.
