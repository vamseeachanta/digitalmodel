"""Plan-criterion screening checks; historical module name retained for callers.

The plan prescribes MBL/static tension >=1.67 and MBL/dynamic tension >=3.0.
Those factors are attributed only to the plan, not to DNV-ST-N001. Fairlead
capacities and bridle limits are explicit project inputs. A screening PASS
does not establish towline compatibility with full tug bollard pull or
operational acceptance. No DNV compliance verdict is calculated here.
"""
from dataclasses import dataclass
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation

from .towline_analysis import _positive

_BASIS_ROOT = Path(__file__).with_name('basis')


@dataclass(frozen=True)
class Result:
    passed: bool
    reason: str
    margin: float  # capacity minus demand in N, or distance to angle limit in deg
    citations: tuple[Citation, ...]
    assessment: str = 'plan-criterion screening'


def _reference() -> tuple[Citation, ...]:
    citation = Citation(
        code_id='digitalmodel-towing-plan-487', publisher='digitalmodel', revision='2026-05-05',
        section='Issue 487 plan: design basis and step 4, screening factors only',
        wiki_path='wikis/towing-plan-487.md',
        source_sibling='generic',
        note=('Plan-criterion screening: 3.0 dynamic and 1.67 static. '
              'No standard clause has been established for these factors.'),
    )
    validate_citation(citation, repo_root=_BASIS_ROOT)
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
