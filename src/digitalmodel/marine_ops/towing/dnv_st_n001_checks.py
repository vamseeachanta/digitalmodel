"""Bounded towing checks from the issue 487 approved design basis.

The plan prescribes MBL/static tension >=1.67 and MBL/dynamic tension >=3.0.
Those plan factors are implemented verbatim, not inferred from a full N001
assessment. Fairlead capacities and bridle limits are explicit project inputs.
The citation binds the available 2016-06 reference; no claim of certification,
current-edition equivalence or complete marine-warranty compliance is made.
"""
from dataclasses import dataclass
import math

from digitalmodel.citations.schema import Citation, validate_citation

from .towline_analysis import _positive


@dataclass(frozen=True)
class Result:
    passed: bool
    reason: str
    margin: float  # capacity minus demand in N, or distance to angle limit in deg
    citations: tuple[Citation, ...]


def _reference() -> tuple[Citation, ...]:
    citation = Citation(
        code_id='dnvgl-st-n001', publisher='DNV GL (now DNV)', revision='2016-06',
        section='Section 11 (sea voyages and towing equipment)',
        wiki_path='wikis/engineering-standards/wiki/standards/dnvgl-st-n001.md',
        source_sibling='generic',
        note=('DNV-ST-N001 towing reference; SF 3.0 dynamic and 1.67 static are '
              'the issue 487 approved-plan criteria, not a full compliance assessment.'),
    )
    validate_citation(citation)
    return (citation,)


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
                  f'{governing}: MBL {mbl_n:.3f} N versus required {demand:.3f} N',
                  margin, _reference())


def check_fairlead(*, reaction_n: float, capacity_n: float) -> Result:
    """Compare the supplied fitting reaction with its rated reaction capacity (N)."""
    _positive(capacity_n=capacity_n)
    if not math.isfinite(reaction_n) or reaction_n < 0:
        raise ValueError('reaction_n must be finite and nonnegative')
    margin = capacity_n - reaction_n
    return Result(margin >= 0,
                  f'fairlead reaction {reaction_n:.3f} N versus capacity {capacity_n:.3f} N',
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
                  f'bridle angle {angle_deg:.3f} deg versus [{minimum_deg}, {maximum_deg}] deg',
                  margin, _reference())
