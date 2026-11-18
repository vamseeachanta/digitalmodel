# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exact inextensible static installation screens, independent of native solvers.

Basis: Catenary Curve (2019), equations (1), (4), (9), (10), (14), extended
by signed constant-weight section integration; see docs/route-qualification.md.
The seabed is flat and frictionless, the lower tangent horizontal. No stiffness,
stretch, current, dynamics, local module bend stiffness or safety factors enter.
"""

from dataclasses import dataclass
import math
from typing import Callable, Iterable


def _positive(name: str, value: float) -> None:
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


@dataclass(frozen=True)
class PlanningState:
    """SI output, z upward from touchdown; top angle measured from vertical."""

    control: str
    control_value: float
    water_depth: float
    horizontal_span: float
    suspended_length: float
    bottom_tension: float
    vertical_top_force: float
    top_tension: float
    hang_off_angle: float
    max_curvature: float
    sag_height: float | None = None
    hog_height: float | None = None

    @property
    def touchdown_point(self) -> tuple[float, float]:
        """Touchdown displacement FROM hang-off in the same x,z axes (m)."""
        return -self.horizontal_span, -self.water_depth


@dataclass(frozen=True)
class RejectedCase:
    control: str
    value: float
    suspended_length: float | None
    reason: str


@dataclass(frozen=True)
class SweepResult:
    cases: tuple[PlanningState, ...]
    rejected: tuple[RejectedCase, ...]


@dataclass(frozen=True)
class PlanningLimits:
    """All limits are supplied by the caller; none implies code acceptance."""

    min_depth: float
    max_depth: float
    max_top_tension: float
    min_bottom_tension: float
    max_curvature: float
    max_span: float
    max_suspended_length: float
    max_hang_off_angle: float

    def __post_init__(self):
        for name, value in vars(self).items():
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{name} must be finite and nonnegative")
        if self.min_depth > self.max_depth:
            raise ValueError("min_depth must not exceed max_depth")
        if self.max_hang_off_angle > 90:
            raise ValueError("max_hang_off_angle must not exceed 90 degrees")

    def violations(self, state: PlanningState) -> list[str]:
        pairs = {
            "min_depth": state.water_depth >= self.min_depth,
            "max_depth": state.water_depth <= self.max_depth,
            "max_top_tension": state.top_tension <= self.max_top_tension,
            "min_bottom_tension": state.bottom_tension >= self.min_bottom_tension,
            "max_curvature": state.max_curvature <= self.max_curvature,
            "max_span": state.horizontal_span <= self.max_span,
            "max_suspended_length": state.suspended_length <= self.max_suspended_length,
            "max_hang_off_angle": state.hang_off_angle <= self.max_hang_off_angle,
        }
        return [name for name, passed in pairs.items() if not passed]


@dataclass(frozen=True)
class OptimizationResult:
    best: PlanningState | None
    objective_value: float | None
    rejected: tuple[RejectedCase, ...]

    @property
    def feasible(self) -> bool:
        return self.best is not None


def sweep_simple(
    *, water_depth: float, weight: float,
    hang_off_angles: Iterable[float], top_tensions: Iterable[float],
    suspended_lengths: Iterable[float],
) -> SweepResult:
    """Sweep alternative controls at FIXED depth; they are not independent DOFs.

    E1-E3 give a=d*sin(theta)/(1-sin(theta)), H=T-w*d,
    or a=(s²-d²)/(2d). Each control contributes one independently solved case.
    Invalid candidates are retained with a reason rather than dropped.
    """
    _positive("water_depth", water_depth)
    _positive("weight", weight)
    cases, rejected = [], []
    for control, values in (("angle", hang_off_angles), ("tension", top_tensions),
                            ("length", suspended_lengths)):
        for value in values:
            try:
                _positive(control, value)
                if control == "angle":
                    if value >= 90:
                        raise ValueError("angle must be less than 90 degrees from vertical")
                    q = math.radians(value)
                    a = water_depth * math.sin(q) / (2 * math.sin(math.radians(90-value)/2)**2)
                    h = weight * a
                elif control == "tension":
                    h = value - weight * water_depth
                else:
                    h = weight * (value - water_depth) * (value + water_depth) / (2 * water_depth)
                _positive("horizontal tension", h)
                a = h / weight
                length = math.sqrt(water_depth * (water_depth + 2 * a))
                # E1 half-angle identity conditions nearly horizontal spans.
                span = 2 * a * math.asinh(math.sqrt(water_depth / a / 2))
                state = PlanningState(
                    control, value, water_depth, span, length, h, weight * length,
                    h + weight * water_depth,
                    math.degrees(math.atan2(h, weight * length)), weight / h,
                )
                _finite_state(state)
                cases.append(state)
            except (ValueError, OverflowError) as exc:
                rejected.append(RejectedCase(control, value, None, str(exc)))
    return SweepResult(tuple(cases), tuple(rejected))


def _section(h: float, v: float, w: float, length: float) -> tuple[float, float, float, float]:
    """Integrate E1/E2 using signed V: dV/ds=w, dx/ds=H/T, dz/ds=V/T.

    Rationalising T1-T0 avoids loss of vertical displacement when w is small.
    Curvature k=|d atan(V/H)/ds|=|w|H/(H²+V²) peaks at smallest |V|.
    """
    end = v + w * length
    t0, t1 = math.hypot(h, v), math.hypot(h, end)
    dx = h / w * (math.asinh(end / h) - math.asinh(v / h))
    dz = length * (end + v) / (t1 + t0)
    vmin = 0 if v * end <= 0 else min(abs(v), abs(end))
    curvature = abs(w) / h * (h / math.hypot(h, vmin))**2
    return dx, dz, end, curvature


def _finite_state(state: PlanningState) -> None:
    for name, value in vars(state).items():
        if isinstance(value, (float, int)) and not math.isfinite(value):
            raise ValueError(f"{name} is non-finite; rescale the inputs")


def sweep_lazy_wave(
    *, weight: float, buoyancy_start: float, buoyancy_length: float,
    net_buoyancy: float, suspended_lengths: Iterable[float],
    hang_off_angles: Iterable[float], top_tensions: Iterable[float],
) -> SweepResult:
    """Sweep total suspended length crossed with alternative angle/T controls.

    Start and length are arc distances from touchdown. net_buoyancy is the
    positive NET upward force per metre, including the bare pipe weight.
    Water depth is an OUTPUT: filter it with explicit PlanningLimits. Angles
    and tensions are alternative controls, not a simultaneous overconstraint.
    Only a genuine hog/sag configuration entirely above seabed and below
    hang-off is admitted. No general boundary-value solution is claimed.
    """
    controls = [("angle", v) for v in hang_off_angles] + [("tension", v) for v in top_tensions]
    cases, rejected = [], []
    for total in suspended_lengths:
        for control, value in controls:
            try:
                for name, number in (("weight", weight), ("buoyancy_start", buoyancy_start),
                                     ("buoyancy_length", buoyancy_length),
                                     ("net_buoyancy", net_buoyancy), ("length", total),
                                     (control, value)):
                    _positive(name, number)
                upper = total - buoyancy_start - buoyancy_length
                _positive("upper suspended length", upper)
                v1 = weight * buoyancy_start
                v2 = v1 - net_buoyancy * buoyancy_length
                vt = v2 + weight * upper
                if v2 >= 0 or vt <= 0:
                    raise ValueError("lazy wave requires hog and sag tangent reversals")
                if control == "angle":
                    if value >= 90:
                        raise ValueError("angle must be less than 90 degrees from vertical")
                    h = vt * math.tan(math.radians(value))
                else:
                    if value <= vt:
                        raise ValueError("top tension must exceed vertical top force")
                    h = math.sqrt((value - vt) * (value + vt))
                _positive("horizontal tension", h)
                x = z = v = curvature = 0.0
                for length, w in ((buoyancy_start, weight), (buoyancy_length, -net_buoyancy),
                                  (upper, weight)):
                    dx, dz, v, k = _section(h, v, w, length)
                    x, z, curvature = x + dx, z + dz, max(curvature, k)
                # The two V=0 extrema are found analytically inside their sections.
                _, z1, _, _ = _section(h, 0, weight, buoyancy_start)
                _, to_hog, _, _ = _section(h, v1, -net_buoyancy, v1 / net_buoyancy)
                _, buoy_dz, _, _ = _section(h, v1, -net_buoyancy, buoyancy_length)
                _, to_sag, _, _ = _section(h, v2, weight, -v2 / weight)
                hog, sag = z1 + to_hog, z1 + buoy_dz + to_sag
                if sag <= 0 or z <= hog:
                    raise ValueError("lazy wave crosses seabed or rises above hang-off")
                state = PlanningState(
                    control, value, z, x, total, h, vt, math.hypot(h, vt),
                    math.degrees(math.atan2(h, vt)), curvature, sag, hog,
                )
                _finite_state(state)
                cases.append(state)
            except (ValueError, OverflowError) as exc:
                rejected.append(RejectedCase(control, value, total, str(exc)))
    return SweepResult(tuple(cases), tuple(rejected))


def select_optimal(
    sweep: SweepResult, *, objective: Callable[[PlanningState], float],
    limits: PlanningLimits,
) -> OptimizationResult:
    """Select the least finite objective on the FEASIBLE supplied grid.

    Rejections include invalid inputs and every violated named limit. Ties keep
    the first candidate. An empty feasible set returns best=None explicitly.
    """
    best = score = None
    rejected = list(sweep.rejected)
    for state in sweep.cases:
        reasons = limits.violations(state)
        value = None
        if not reasons:
            value = objective(state)
            if not math.isfinite(value):
                reasons.append("objective must be finite")
        if reasons:
            rejected.append(RejectedCase(state.control, state.control_value,
                                         state.suspended_length, "; ".join(reasons)))
        elif score is None or value < score:
            best, score = state, value
    return OptimizationResult(best, score, tuple(rejected))
