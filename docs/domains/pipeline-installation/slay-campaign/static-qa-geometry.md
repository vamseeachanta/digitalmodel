# S-lay static QA geometry

`evaluate_static_acceptance` requires the steel outer diameter and stinger
radius as keyword arguments. The report records both inputs and the S1 strain
limit so that a result can be traced to its geometry. This addresses #2715.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Require explicit geometry and show its effect on the S1 result to prevent silent reuse of a campaign limit. |

## Inputs and method

`diameter` is the steel outer diameter, excluding coatings. `radius` is the
stinger radius. Both are positive finite values in metres. Supply them on every
call to `evaluate_static_acceptance` or `assert_static_acceptance`; omitting
either raises `TypeError`. Invalid values raise `ValueError`.

The bending strain magnitude is the distance from the neutral axis multiplied
by curvature. Taking that distance as D/2 and circular curvature as 1/R gives
`epsilon_b = D/(2R)`. The strain–curvature relation is given in
[Stresses in Beams (2000), Eq. 2, page 2](https://ocw.mit.edu/courses/3-11-mechanics-of-materials-fall-1999/96d839b02e4a6c63cf8031800e89cccd_MIT3_11F99_bstress.pdf).
This is a bending screen, without axial strain or an installation allowable.

The existing S1 criterion is `s1_limit = 1.10*D/(2R)` together with zero force on
the last stinger support. The factor 1.10 is the adopted campaign criterion in
[criteria.json](criteria.json), revision execution-3 (2026),
`criteria.stinger_conformity`; it is not a published standard allowable. This
change retains that criterion and all existing QA bands. The original campaign
geometry comes from [case-table.yml](case-table.yml) (2026),
`basis.pipeline.dimensions.outer_diameter` and `basis.equipment.stinger.radius`.

## Worked example

The alternate geometry and strain are the synthetic counterexample in #2715:
D = 0.508 m, R = 120 m and measured overbend strain = 0.0026. Other readings
below are synthetic inputs within the existing campaign QA bands; they are not
measured solver results or qualified design references.

An independent hand calculation gives `1.10*0.508/(2*120) = 0.5588/240 =
1397/600000`, or **2328.3 microstrain**. The supplied strain is **2600.0
microstrain**, exceeding the unrounded limit by **11.7 %**, so S1 fails. For the
original D = 0.762 m and R = 150 m, the same calculation gives `0.8382/300`, or
**2794.0 microstrain**, which would incorrectly pass this alternate case if
silently reused.

Run this example with the source package on the Python path; it requires no
solver binding:

```python
from math import isclose

from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import (
    evaluate_static_acceptance,
)

# Inputs: #2715 counterexample; other readings are synthetic QA inputs.
# Bands: criteria.json, execution-3 (2026), static_qa.
result = evaluate_static_acceptance(
    diameter=0.508, radius=120,
    wire_length=10, end_a_reaction=1, last_support_force=0,
    grounded_tension=[0, 1], overbend_strain=0.0026,
    lift_off_arc=55, departure_angle=21,
    reference={"lift_off_arc": 55, "departure_angle": 21},
)

# Independent hand result above; the tolerance tests arithmetic only.
assert isclose(result["s1_limit"], 1397 / 600000)
assert result["diameter"] == 0.508
assert result["radius"] == 120
assert result["rows"]["stinger_conformity"]["status"] == "FAIL"
assert result["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
assert result["status"] == "rejected"
assert result["fully_accepted"] is False
print(f"S1 limit: {result['s1_limit'] * 1e6:.1f} microstrain")
print(result["status"])
```

Expected output:

```text
S1 limit: 2328.3 microstrain
rejected
```

## Report fields and limits

| Field | Meaning | Unit |
| --- | --- | --- |
| `diameter` | Supplied steel outer diameter | m |
| `radius` | Supplied stinger radius | m |
| `s1_limit` | Computed limit used by the S1 row | dimensionless strain |

Existing `status`, `fully_accepted`, `counts` and `rows` fields retain their
meaning. `assert_static_acceptance` returns the same report when no evaluated
row fails and raises `ValueError` for a failed row. S1 evaluates the supplied
overbend strain; it does not reconstruct that reading or establish convergence.

Touchdown, sag-bend and stiff-closure rows remain `NOT_EVALUATED` pending the
published references requested in #2608. A passing S1 row does not establish
installation design acceptance. No native solver results are produced here.
