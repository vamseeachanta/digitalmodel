"""Exhaustive static tow-configuration search with freely separating level supports.

Diameter-squared scaling assumes identical rope construction/grade for weight
and MBL; it is a synthetic sizing relation, not a substitute for rope certificates.
The included bridle angle divides main-line tension between symmetric legs.
Fairlead reaction is 2*T_leg*sin(deflection/2), using the dynamic leg tension.
Depth is vertical room below the support elevation; prescribed line weight is
unchanged even if a physical line would enter water (approved air-weight model).
"""
from dataclasses import dataclass, replace
from itertools import product
import math

from .dnv_st_n001_checks import Result, check_bridle_angle, check_fairlead, check_towline
from .towline_analysis import _positive, catenary_from_pull


@dataclass(frozen=True)
class ConfigurationResult:
    bridle_angle_deg: float
    line_length_m: float
    line_diameter_m: float
    span_m: float
    sag_m: float
    scope: float
    clearance_m: float
    peak_tension_n: float
    bridle_leg_tension_n: float
    mbl_n: float
    checks: dict[str, Result]
    feasible_count: int = 0


@dataclass(frozen=True)
class _Basis:
    horizontal_load_n: float
    reference_diameter_m: float
    reference_weight_n_m: float
    reference_mbl_n: float
    available_depth_m: float
    minimum_clearance_m: float
    scope_bounds: tuple[float, float]
    bridle_angle_bounds_deg: tuple[float, float]
    fairlead_capacity_n: float
    dynamic_amplification: float
    fairlead_deflection_deg: float

    def validate(self):
        _positive(horizontal_load_n=self.horizontal_load_n,
                  diameter=self.reference_diameter_m, weight=self.reference_weight_n_m,
                  mbl=self.reference_mbl_n, depth=self.available_depth_m,
                  fairlead_capacity_n=self.fairlead_capacity_n,
                  dynamic_amplification=self.dynamic_amplification)
        if not math.isfinite(self.minimum_clearance_m) or self.minimum_clearance_m < 0:
            raise ValueError('minimum clearance must be finite and nonnegative')
        if self.dynamic_amplification < 1:
            raise ValueError('dynamic amplification must be >= 1')
        if not math.isfinite(self.fairlead_deflection_deg) or not 0 <= self.fairlead_deflection_deg <= 180:
            raise ValueError('fairlead deflection must be in [0,180] degrees')
        lo, hi = self.scope_bounds
        if not math.isfinite(lo) or not math.isfinite(hi) or not 1 <= lo <= hi:
            raise ValueError('scope bounds must be finite with 1 <= lower <= upper')
        lo, hi = self.bridle_angle_bounds_deg
        check_bridle_angle(lo, minimum_deg=lo, maximum_deg=hi)


def _candidate(angle, length, diameter, basis):
    ratio = (diameter / basis.reference_diameter_m)**2
    line = catenary_from_pull(horizontal_tension_n=basis.horizontal_load_n,
                              length_m=length, weight_n_m=basis.reference_weight_n_m * ratio)
    clearance = basis.available_depth_m - line.sag_m
    if not basis.scope_bounds[0] <= line.scope <= basis.scope_bounds[1]:
        return None
    if clearance < basis.minimum_clearance_m:
        return None
    angle_check = check_bridle_angle(angle, minimum_deg=basis.bridle_angle_bounds_deg[0],
                                     maximum_deg=basis.bridle_angle_bounds_deg[1])
    if not angle_check.passed:
        return None
    dynamic = line.top_tension_n * basis.dynamic_amplification
    leg = dynamic / (2 * math.cos(math.radians(angle / 2)))
    reaction = 2 * leg * math.sin(math.radians(basis.fairlead_deflection_deg / 2))
    mbl = basis.reference_mbl_n * ratio
    checks = {
        'towline': check_towline(mbl_n=mbl, static_tension_n=line.top_tension_n,
                                 dynamic_tension_n=dynamic),
        'fairlead': check_fairlead(reaction_n=reaction, capacity_n=basis.fairlead_capacity_n),
        'bridle_angle': angle_check,
    }
    if not all(check.passed for check in checks.values()):
        return None
    return ConfigurationResult(angle, length, diameter, line.span_m, line.sag_m,
                               line.scope, clearance, dynamic, leg, mbl, checks)


def optimize_configuration(*, horizontal_load_n: float, bridle_angles_deg,
                           line_lengths_m, line_diameters_m, reference_diameter_m: float,
                           reference_weight_n_m: float, reference_mbl_n: float,
                           available_depth_m: float, minimum_clearance_m: float,
                           scope_bounds: tuple[float, float],
                           bridle_angle_bounds_deg: tuple[float, float],
                           fairlead_capacity_n: float, dynamic_amplification: float,
                           fairlead_deflection_deg: float = 180) -> ConfigurationResult:
    """Minimize dynamic main-line tension; break ties by leg tension and grid order.

    A supplied amplification factor is a screening assumption, not a time-domain
    tug-barge response simulation. Raise ValueError for invalid or infeasible grids.
    """
    basis = _Basis(horizontal_load_n, reference_diameter_m, reference_weight_n_m,
                   reference_mbl_n, available_depth_m, minimum_clearance_m, scope_bounds,
                   bridle_angle_bounds_deg, fairlead_capacity_n, dynamic_amplification,
                   fairlead_deflection_deg)
    basis.validate()
    grids = tuple(tuple(values) for values in (bridle_angles_deg, line_lengths_m, line_diameters_m))
    if any(not values for values in grids):
        raise ValueError('configuration grids must be nonempty')
    for angle in grids[0]:
        if not math.isfinite(angle) or not 0 <= angle < 180:
            raise ValueError('grid angles must be finite and in [0,180)')
    for value in grids[1] + grids[2]:
        _positive(grid_value=value)
    feasible = []
    for angle, length, diameter in product(*grids):
        candidate = _candidate(angle, length, diameter, basis)
        if candidate is not None:
            feasible.append(candidate)
    if not feasible:
        raise ValueError('no feasible towing configuration in supplied grid')
    best = min(feasible, key=lambda item: (item.peak_tension_n, item.bridle_leg_tension_n))
    return replace(best, feasible_count=len(feasible))
