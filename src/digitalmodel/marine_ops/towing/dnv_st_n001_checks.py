"""Plan-criterion towing screening checks; no standard compliance verdict.

The historical module name is retained for imports. Factors are prescribed by
#487's plan; their attribution to a standard has not been confirmed. Fitting
capacities and bridle limits are caller inputs. Full tug-pull strength, dynamics
and operational acceptance require a separate assessment.
"""
from dataclasses import dataclass
import math

from .towline_analysis import _positive


@dataclass(frozen=True)
class CriterionReference:
    """Local design-basis reference, distinct from a maintained standard citation."""

    code_id: str
    document: str
    section: str
    note: str


@dataclass(frozen=True)
class Result:
    passed: bool
    reason: str
    margin: float  # capacity minus demand in N, or distance to angle limit in deg
    citations: tuple[CriterionReference, ...]


def _reference() -> tuple[CriterionReference, ...]:
    return (CriterionReference(
        code_id='towing-plan-criteria',
        document='docs/plans/2026-05-05-issue-487-towing-marine-operations.md',
        section='Context and Plan item 4; plan factors and caller-supplied limits only',
        note=('Plan-criterion screening only; the plan is not evidence of standard '
              'compliance or operational acceptance.'),
    ),)


def check_towline(*, mbl_n: float, static_tension_n: float,
                  dynamic_tension_n: float | None) -> Result:
    """Check both plan factors; None explicitly requests a static-only assessment."""
    _positive(mbl_n=mbl_n, static_tension_n=static_tension_n)
    demands = [('static', 1.67 * static_tension_n)]
    if dynamic_tension_n is not None:
        _positive(dynamic_tension_n=dynamic_tension_n)
        if dynamic_tension_n < static_tension_n:
            raise ValueError('dynamic tension must be >= static tension')
        demands.append(('dynamic', 3.0 * dynamic_tension_n))
    governing, demand = max(demands, key=lambda item: item[1])
    margin = mbl_n - demand
    return Result(margin >= 0,
                  f'plan-criterion screening: {governing}: MBL {mbl_n:.3f} N versus required {demand:.3f} N',
                  margin, _reference())


def check_fairlead(*, reaction_n: float, capacity_n: float) -> Result:
    """Compare the supplied fitting reaction with its rated reaction capacity (N)."""
    _positive(capacity_n=capacity_n)
    if not math.isfinite(reaction_n) or reaction_n < 0:
        raise ValueError('reaction_n must be finite and nonnegative')
    margin = capacity_n - reaction_n
    return Result(margin >= 0,
                  f'plan-criterion screening: fairlead reaction {reaction_n:.3f} N versus capacity {capacity_n:.3f} N',
                  margin, _reference())


def check_bridle_angle(angle_deg: float, *, minimum_deg: float,
                       maximum_deg: float) -> Result:
    """Check the included angle against caller-declared bounds (degrees)."""
    if not all(math.isfinite(v) for v in (angle_deg, minimum_deg, maximum_deg)):
        raise ValueError('angles must be finite')
    if not 0 <= minimum_deg <= maximum_deg < 180:
        raise ValueError('bounds must satisfy 0 <= minimum <= maximum < 180')
    margin = min(angle_deg - minimum_deg, maximum_deg - angle_deg)
    return Result(margin >= 0,
                  f'plan-criterion screening: bridle angle {angle_deg:.3f} deg versus [{minimum_deg}, {maximum_deg}] deg',
                  margin, _reference())
