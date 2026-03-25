"""
Cathodic disbonding calculations per ASTM G80 (1998).

ASTM G80: Standard Test Method for Specific Cathodic Disbonding of
Pipeline Coatings (ambient temperature).

A holiday (defect) is drilled in the coating and the specimen is held at
a cathodic protection potential for a defined test duration.  After the
test, the disbonded coating is peeled back and the disbonding radius is
measured from the centre of the holiday.

Key equations
-------------
Disbonding area (Section 10.1):
    A_d = pi * (r_d^2 - r_h^2)   [mm^2]

Net disbonding radius (Section 10.2):
    r_net = r_d - r_h             [mm]

Pass/fail check:
    r_net <= acceptance_radius_mm
"""

import math
from dataclasses import dataclass


@dataclass
class G80TestResult:
    """Result from an ASTM G80 cathodic disbonding evaluation."""

    holiday_radius_mm: float
    disbonding_radius_mm: float
    disbonding_area_mm2: float
    net_disbonding_mm: float
    passes: bool
    acceptance_radius_mm: float  # None when no acceptance criterion is specified


# ---------------------------------------------------------------------------
# Standalone calculation functions
# ---------------------------------------------------------------------------

def disbonding_area(holiday_radius_mm, disbonding_radius_mm):
    """Cathodic disbonding area per ASTM G80 Section 10.1.

    A_d = pi * (r_d^2 - r_h^2)

    Parameters
    ----------
    holiday_radius_mm : float
        Radius of the drilled holiday (defect) [mm].  Must be > 0.
    disbonding_radius_mm : float
        Total radius of disbonded area measured from the holiday centre [mm].
        Must be >= holiday_radius_mm.

    Returns
    -------
    float
        Disbonding area [mm^2].
    """
    if holiday_radius_mm <= 0.0:
        raise ValueError(
            f"holiday_radius_mm must be > 0, got {holiday_radius_mm}"
        )
    if disbonding_radius_mm < holiday_radius_mm:
        raise ValueError(
            "disbonding_radius_mm must be >= holiday_radius_mm, "
            f"got {disbonding_radius_mm} < {holiday_radius_mm}"
        )
    return math.pi * (disbonding_radius_mm**2 - holiday_radius_mm**2)


def net_disbonding_radius(holiday_radius_mm, disbonding_radius_mm):
    """Net disbonding distance from the holiday edge per ASTM G80 Section 10.2.

    r_net = r_d - r_h

    Parameters
    ----------
    holiday_radius_mm : float
        Holiday radius [mm].  Must be > 0.
    disbonding_radius_mm : float
        Total disbonding radius from holiday centre [mm].
        Must be >= holiday_radius_mm.

    Returns
    -------
    float
        Net disbonding radius [mm].
    """
    if holiday_radius_mm <= 0.0:
        raise ValueError(
            f"holiday_radius_mm must be > 0, got {holiday_radius_mm}"
        )
    if disbonding_radius_mm < holiday_radius_mm:
        raise ValueError(
            "disbonding_radius_mm must be >= holiday_radius_mm, "
            f"got {disbonding_radius_mm} < {holiday_radius_mm}"
        )
    return disbonding_radius_mm - holiday_radius_mm


def evaluate_g80(holiday_radius_mm, disbonding_radius_mm, acceptance_radius_mm=None):
    """Evaluate an ASTM G80 cathodic disbonding test result.

    Parameters
    ----------
    holiday_radius_mm : float
        Holiday radius [mm].
    disbonding_radius_mm : float
        Measured total disbonding radius from the holiday centre [mm].
    acceptance_radius_mm : float or None
        Maximum allowable net disbonding radius [mm].
        When None, no pass/fail check is applied and passes is set to True.

    Returns
    -------
    G80TestResult
    """
    area = disbonding_area(holiday_radius_mm, disbonding_radius_mm)
    net_r = net_disbonding_radius(holiday_radius_mm, disbonding_radius_mm)

    if acceptance_radius_mm is not None and acceptance_radius_mm <= 0.0:
        raise ValueError(
            f"acceptance_radius_mm must be > 0, got {acceptance_radius_mm}"
        )

    if acceptance_radius_mm is None:
        passes = True
    else:
        passes = net_r <= acceptance_radius_mm

    return G80TestResult(
        holiday_radius_mm=holiday_radius_mm,
        disbonding_radius_mm=disbonding_radius_mm,
        disbonding_area_mm2=round(area, 3),
        net_disbonding_mm=round(net_r, 3),
        passes=passes,
        acceptance_radius_mm=acceptance_radius_mm,
    )
