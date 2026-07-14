# ABOUTME: Hull-girder longitudinal strength SCREENING — still-water SF/BM from a
# ABOUTME: loading condition, utilisation vs allowable curves, section-modulus check.
"""Hull-girder longitudinal strength screening (still-water SF/BM vs allowables).

Given a loading condition (lightship distribution + tank/point weights) and a
hull buoyancy description, this module builds the weight and buoyancy
distributions along the hull, integrates the still-water shear force and
bending moment, and reports utilisation against user-supplied allowable SF/BM
curves interpolated at frame positions. An optional hull-girder
section-modulus check accepts either simple midship scantlings or a
class-approved section modulus.

SCREENING TIER ONLY. This ranks and gates loading conditions; any
class-compliance conclusion still requires the approved loading instrument
(loadicator) or a class-endorsed calculation. Allowable SF/BM curves and
approved section-modulus values are user inputs (supplied per project from the
approved loading manual) — nothing here substitutes for them.

Conventions and units (SI, marine practice):

* ``x`` runs from the aft perpendicular / aft end (``x = 0``) forward.
* Weights in tonnes (t), distributed weights in t/m; shear force in
  tonnes-force (t), bending moment in tonne-metres (t·m); kN / kN·m
  equivalents are reported with ``g = 9.81``.
* Load curve ``q(x) = w(x) - b(x)`` (weight minus buoyancy);
  ``SF(x) = ∫ q dx``; ``BM(x) = ∫ SF dx``. With this convention
  **positive BM = hogging**, negative = sagging (weight excess at the ends
  and buoyancy excess amidships integrates to a positive midship moment).
* In equilibrium the total weight equals the total buoyancy and their
  longitudinal first moments match, so SF and BM vanish at both free ends
  (the classical closure check).

Buoyancy input modes:

* ``box`` — rectangular barge: a linear (trimmed-waterplane) buoyancy
  distribution pinned so the *discrete* force and moment exactly balance the
  discretised weight curve (same closure device as
  :mod:`digitalmodel.naval_architecture.loading_computer`).
* ``hydrostatics_table`` — Bonjean-style table of immersed sectional area vs
  draft at stations along the hull; the even-keel/trimmed waterline is solved
  so buoyancy balances weight and LCB aligns with LCG, then an optional linear
  closure correction absorbs the residual discretisation error.
* ``direct`` — a user-supplied buoyancy-per-metre curve (e.g. exported from a
  hydrostatics program), with the same optional closure correction.

References (public):

- E. V. Lewis (ed.), *Principles of Naval Architecture*, Vol. I, SNAME —
  longitudinal strength: weight/buoyancy load curve and double integration.
- O. F. Hughes & J. K. Paik, *Ship Structural Analysis and Design*, SNAME —
  hull-girder still-water shear/bending and section-modulus fundamentals.
- C. B. Barrass & D. R. Derrett, *Ship Stability for Masters and Mates* —
  box-barge shear/bending worked-example form used by the test fixture.
- IACS UR S11 permissible-stress framework via
  :mod:`digitalmodel.naval_architecture.hull_girder_strength` (reused).
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np

# REUSE: IACS UR S11 permissible stress / section-modulus yield check and the
# S11 wave bending moment already validated in this package.
from digitalmodel.naval_architecture.hull_girder_strength import (
    SectionModulusCheck,
    section_modulus_check,
    wave_bending_moment,
)

G_M_S2 = 9.81  # m/s^2 (tonnes-force -> kN)
RHO_SEAWATER_T_M3 = 1.025

#: fraction of displacement (SF) / displacement*length (BM) tolerated as a raw
#: end-closure residual before the condition is flagged.
DEFAULT_CLOSURE_TOLERANCE = 0.005


# --------------------------------------------------------------------------- #
# Inputs
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class DistributedWeight:
    """A weight spread over a longitudinal extent (lightship block, tank, ...).

    The density is linear over ``[x_start_m, x_end_m]`` with total
    ``weight_t`` and centroid ``lcg_m`` (default: mid-extent). A linear
    non-negative density can only place the centroid within the middle third
    of the extent (``|lcg - mid| <= span/6``); outside that the block must be
    subdivided, and a ``ValueError`` is raised.
    """

    name: str
    weight_t: float
    x_start_m: float
    x_end_m: float
    lcg_m: float | None = None

    def __post_init__(self) -> None:
        if self.weight_t < 0.0:
            raise ValueError(f"weight_t must be non-negative for {self.name!r}")
        if self.x_end_m <= self.x_start_m:
            raise ValueError(f"x_end_m must exceed x_start_m for {self.name!r}")
        lcg = self.mid_m if self.lcg_m is None else self.lcg_m
        span = self.x_end_m - self.x_start_m
        if abs(lcg - self.mid_m) > span / 6.0 + 1e-12:
            raise ValueError(
                f"lcg_m of {self.name!r} is outside the middle third of its "
                "extent; a non-negative linear density cannot represent it — "
                "subdivide the block"
            )

    @property
    def mid_m(self) -> float:
        return 0.5 * (self.x_start_m + self.x_end_m)

    def density_at(self, x: float) -> float:
        """Linear density (t/m) at ``x`` (0 outside the extent)."""
        if not (self.x_start_m <= x <= self.x_end_m):
            return 0.0
        span = self.x_end_m - self.x_start_m
        lcg = self.mid_m if self.lcg_m is None else self.lcg_m
        c0 = self.weight_t / span
        c1 = 12.0 * self.weight_t * (lcg - self.mid_m) / span**3
        return c0 + c1 * (x - self.mid_m)


@dataclass(frozen=True)
class PointWeight:
    """A concentrated weight, smeared over a short extent for the load curve."""

    name: str
    weight_t: float
    x_m: float
    extent_m: float | None = None  # default: one grid interval

    def __post_init__(self) -> None:
        if self.weight_t < 0.0:
            raise ValueError(f"weight_t must be non-negative for {self.name!r}")


@dataclass(frozen=True)
class HydroStation:
    """Bonjean-style immersed sectional area vs draft at one station.

    ``drafts_m`` must be ascending and should start at ``(0.0, 0.0)``;
    areas are interpolated linearly and clamped at the table ends.
    """

    x_m: float
    drafts_m: tuple[float, ...]
    areas_m2: tuple[float, ...]

    def __post_init__(self) -> None:
        if len(self.drafts_m) != len(self.areas_m2) or len(self.drafts_m) < 2:
            raise ValueError(
                f"station at x={self.x_m}: drafts_m/areas_m2 must be equal-"
                "length lists with at least 2 points"
            )
        if any(b <= a for a, b in zip(self.drafts_m, self.drafts_m[1:])):
            raise ValueError(f"station at x={self.x_m}: drafts_m must ascend")

    def area_at(self, draft_m: float) -> float:
        if draft_m > self.drafts_m[-1] + 1e-9:
            raise ValueError(
                f"station at x={self.x_m}: draft {draft_m:.3f} m exceeds the "
                f"hydrostatics table maximum {self.drafts_m[-1]:.3f} m"
            )
        return float(np.interp(draft_m, self.drafts_m, self.areas_m2))


@dataclass(frozen=True)
class AllowableCurve:
    """Two-sided allowable magnitudes vs position (linear interpolation).

    ``positive`` limits positive values (hogging BM / positive SF),
    ``negative`` limits the magnitude of negative values. Positions outside
    the knot range clamp to the end values.
    """

    x_m: tuple[float, ...]
    positive: tuple[float, ...]
    negative: tuple[float, ...]

    def __post_init__(self) -> None:
        n = len(self.x_m)
        if n < 2 or len(self.positive) != n or len(self.negative) != n:
            raise ValueError(
                "allowable curve needs >= 2 knots with matching x/positive/"
                "negative lengths"
            )
        if any(b <= a for a, b in zip(self.x_m, self.x_m[1:])):
            raise ValueError("allowable curve x_m must ascend")
        if min(min(self.positive), min(self.negative)) <= 0.0:
            raise ValueError("allowable values must be positive magnitudes")

    def limit_for(self, x: float, value: float) -> float:
        table = self.positive if value >= 0.0 else self.negative
        return float(np.interp(x, self.x_m, table))


@dataclass(frozen=True)
class SectionElement:
    """One lumped element of a midship section (area about the keel axis).

    Provide ``area_m2`` directly (with optional own inertia ``i_self_m4``), or
    a horizontal strake ``breadth_m x thickness_m`` (own inertia b*t^3/12), or
    a vertical strake ``height_m x thickness_m`` (own inertia t*h^3/12).
    ``z_m`` is the element-centroid height above the keel.
    """

    name: str
    z_m: float
    area_m2: float
    i_self_m4: float = 0.0

    @staticmethod
    def horizontal(name: str, breadth_m: float, thickness_m: float, z_m: float) -> "SectionElement":
        return SectionElement(
            name=name, z_m=z_m, area_m2=breadth_m * thickness_m,
            i_self_m4=breadth_m * thickness_m**3 / 12.0,
        )

    @staticmethod
    def vertical(name: str, height_m: float, thickness_m: float, z_m: float) -> "SectionElement":
        return SectionElement(
            name=name, z_m=z_m, area_m2=height_m * thickness_m,
            i_self_m4=thickness_m * height_m**3 / 12.0,
        )


# --------------------------------------------------------------------------- #
# Results
# --------------------------------------------------------------------------- #
@dataclass(frozen=True)
class StillWaterResult:
    """Still-water SF/BM distribution and closure diagnostics."""

    x_m: tuple[float, ...]
    weight_per_m_t: tuple[float, ...]
    buoyancy_per_m_t: tuple[float, ...]
    shear_force_t: tuple[float, ...]
    bending_moment_t_m: tuple[float, ...]
    displacement_t: float
    lcg_m: float
    max_shear_t: float
    x_max_shear_m: float
    max_hogging_t_m: float  # >= 0 (0 if the hull never hogs)
    x_max_hogging_m: float
    max_sagging_t_m: float  # <= 0 (0 if the hull never sags)
    x_max_sagging_m: float
    # Raw end residuals of the *uncorrected* buoyancy balance, normalised by
    # displacement (SF) and displacement*length (BM).
    closure_shear_fraction: float
    closure_moment_fraction: float
    closure_correction_applied: bool
    closure_ok: bool
    equilibrium: dict = field(default_factory=dict)  # mode-specific extras


@dataclass(frozen=True)
class FrameUtilization:
    """SF/BM utilisation at one reporting frame."""

    frame: str
    x_m: float
    shear_force_t: float
    shear_allowable_t: float | None
    shear_utilization: float | None
    bending_moment_t_m: float
    bending_allowable_t_m: float | None
    bending_utilization: float | None
    status: str  # pass / fail / no-allowable


@dataclass(frozen=True)
class SectionModulusScreen:
    """Hull-girder section-modulus screening result (one flange, one sign)."""

    location: str  # deck / keel
    condition: str  # hogging / sagging
    moment_kn_m: float
    still_water_kn_m: float
    wave_kn_m: float
    section_modulus_m3: float
    sm_source: str  # approved / scantlings
    check: SectionModulusCheck


# --------------------------------------------------------------------------- #
# Discrete helpers
# --------------------------------------------------------------------------- #
def _cumtrapz(y: np.ndarray, x: np.ndarray) -> np.ndarray:
    """Cumulative trapezoidal integral, same length, starting at 0."""
    out = np.zeros_like(y, dtype=float)
    out[1:] = np.cumsum(0.5 * (y[1:] + y[:-1]) * np.diff(x))
    return out


def _linear_closure(
    x: np.ndarray, force_target: float, moment_target: float
) -> np.ndarray:
    """Linear curve ``a + c*(x - L/2)`` whose *discrete* trapezoidal integral
    and double integral equal the targets (loading_computer closure device)."""
    phi = x - 0.5 * (x[0] + x[-1])
    ones = np.ones_like(x)
    a1 = _cumtrapz(ones, x)[-1]
    p1 = _cumtrapz(phi, x)[-1]
    a2 = _cumtrapz(_cumtrapz(ones, x), x)[-1]
    p2 = _cumtrapz(_cumtrapz(phi, x), x)[-1]
    det = a1 * p2 - p1 * a2
    if abs(det) < 1e-12:
        raise ValueError("degenerate station grid for closure correction")
    a = (force_target * p2 - p1 * moment_target) / det
    c = (a1 * moment_target - force_target * a2) / det
    return a + c * phi


# --------------------------------------------------------------------------- #
# Weight distribution builder
# --------------------------------------------------------------------------- #
def build_weight_curve(
    x: np.ndarray,
    distributed: list[DistributedWeight],
    points: list[PointWeight] | None = None,
) -> np.ndarray:
    """Weight per unit length (t/m) sampled at the stations ``x``.

    Each station carries the *cell-averaged* density over its tributary cell
    (``x_k ± dx/2``, clipped to the hull), so uniform blocks whose edges fall
    on the grid integrate exactly under the trapezoidal rule and density
    jumps are smeared over at most one grid interval. Point weights are
    smeared uniformly over ``extent_m`` (default one grid interval) centred
    on ``x_m`` and clipped to the hull.
    """
    length = float(x[-1] - x[0])
    dx = float(x[1] - x[0])
    w = np.zeros_like(x, dtype=float)

    def add(density_at, x_start: float, x_end: float) -> None:
        for k in range(len(x)):
            cell_lo = max(float(x[0]), float(x[k]) - dx / 2.0)
            cell_hi = min(float(x[-1]), float(x[k]) + dx / 2.0)
            ov_lo = max(cell_lo, x_start)
            ov_hi = min(cell_hi, x_end)
            if ov_hi <= ov_lo:
                continue
            mid = 0.5 * (ov_lo + ov_hi)
            w[k] += density_at(mid) * (ov_hi - ov_lo) / (cell_hi - cell_lo)

    for item in distributed:
        if item.x_start_m < x[0] - 1e-9 or item.x_end_m > x[-1] + 1e-9:
            raise ValueError(f"{item.name!r} extends beyond the hull [0, {length}]")
        add(item.density_at, item.x_start_m, item.x_end_m)
    for item in points or []:
        extent = item.extent_m if item.extent_m else dx
        x0 = max(float(x[0]), item.x_m - extent / 2.0)
        x1 = min(float(x[-1]), item.x_m + extent / 2.0)
        if x1 <= x0:
            raise ValueError(f"point weight {item.name!r} lies outside the hull")
        density = item.weight_t / (x1 - x0)
        add(lambda _xi, _d=density: _d, x0, x1)
    return w


def condition_totals(
    distributed: list[DistributedWeight], points: list[PointWeight] | None = None
) -> tuple[float, float]:
    """Exact (continuous) total weight and LCG of the loading condition."""
    total = sum(it.weight_t for it in distributed)
    moment = sum(
        it.weight_t * (it.mid_m if it.lcg_m is None else it.lcg_m)
        for it in distributed
    )
    for it in points or []:
        total += it.weight_t
        moment += it.weight_t * it.x_m
    if total <= 0.0:
        raise ValueError("total condition weight must be positive")
    return total, moment / total


# --------------------------------------------------------------------------- #
# Buoyancy builders
# --------------------------------------------------------------------------- #
def buoyancy_box(x: np.ndarray, w_per_m: np.ndarray) -> tuple[np.ndarray, dict]:
    """Linear (trimmed box waterplane) buoyancy pinned to discrete closure."""
    force = _cumtrapz(w_per_m, x)[-1]
    moment = _cumtrapz(_cumtrapz(w_per_m, x), x)[-1]
    b = _linear_closure(x, force, moment)
    if float(np.min(b)) < 0.0:
        raise ValueError(
            "box-mode buoyancy goes negative — trim too extreme for a linear "
            "waterplane; use a hydrostatics_table"
        )
    return b, {"method": "box"}


def solve_waterline(
    stations: list[HydroStation],
    displacement_t: float,
    lcg_m: float,
    x: np.ndarray,
    rho_t_m3: float = RHO_SEAWATER_T_M3,
    max_iter: int = 60,
    tolerance: float = 1e-10,
) -> tuple[np.ndarray, float, float]:
    """Solve the trimmed waterline against a Bonjean hydrostatics table.

    Finds end drafts ``(T_aft, T_fwd)`` (linear waterline) such that the
    integrated buoyancy equals the displacement and the LCB aligns with the
    LCG. Returns ``(buoyancy_per_m_t, t_aft, t_fwd)``.
    """
    stations = sorted(stations, key=lambda s: s.x_m)
    xs = np.array([s.x_m for s in stations])
    if xs[0] > x[0] + 1e-9 or xs[-1] < x[-1] - 1e-9:
        raise ValueError("hydrostatics stations must cover the full hull length")
    length = float(x[-1] - x[0])

    def buoyancy(t_aft: float, t_fwd: float) -> np.ndarray:
        drafts = t_aft + (t_fwd - t_aft) * (x - x[0]) / length
        areas_lo = np.empty_like(x)
        for k, xi in enumerate(x):
            j = int(np.searchsorted(xs, xi, side="right")) - 1
            j = min(max(j, 0), len(stations) - 2)
            s0, s1 = stations[j], stations[j + 1]
            t = max(float(drafts[k]), 0.0)
            frac = (float(xi) - s0.x_m) / (s1.x_m - s0.x_m)
            areas_lo[k] = (1.0 - frac) * s0.area_at(t) + frac * s1.area_at(t)
        return rho_t_m3 * areas_lo

    def residuals(t_aft: float, t_fwd: float) -> tuple[float, float]:
        b = buoyancy(t_aft, t_fwd)
        total = _cumtrapz(b, x)[-1]
        lcb = _cumtrapz(b * x, x)[-1] / total if total > 0 else 0.0
        return total - displacement_t, total * lcb - displacement_t * lcg_m

    # Initial guess: even-keel draft by bisection on the mean immersed area.
    t_max = min(s.drafts_m[-1] for s in stations)
    lo, hi = 0.0, t_max
    if _cumtrapz(buoyancy(hi, hi), x)[-1] < displacement_t:
        raise ValueError(
            "displacement exceeds the buoyancy available in the hydrostatics "
            "table — vessel floats deeper than the table covers"
        )
    for _ in range(80):
        mid = 0.5 * (lo + hi)
        if _cumtrapz(buoyancy(mid, mid), x)[-1] < displacement_t:
            lo = mid
        else:
            hi = mid
    t_aft = t_fwd = 0.5 * (lo + hi)

    scale_f = displacement_t
    scale_m = displacement_t * length
    for _ in range(max_iter):
        r1, r2 = residuals(t_aft, t_fwd)
        if abs(r1) <= tolerance * scale_f and abs(r2) <= tolerance * scale_m:
            return buoyancy(t_aft, t_fwd), t_aft, t_fwd
        step = max(1e-6, 1e-6 * max(t_aft, t_fwd, 1.0))
        r1a, r2a = residuals(t_aft + step, t_fwd)
        r1f, r2f = residuals(t_aft, t_fwd + step)
        j11, j12 = (r1a - r1) / step, (r1f - r1) / step
        j21, j22 = (r2a - r2) / step, (r2f - r2) / step
        det = j11 * j22 - j12 * j21
        if abs(det) < 1e-14:
            raise ValueError("waterline solve Jacobian is singular")
        d_aft = (-r1 * j22 + r2 * j12) / det
        d_fwd = (-r2 * j11 + r1 * j21) / det
        # Damp to keep drafts inside the table.
        damp = 1.0
        while damp > 1e-4:
            na, nf = t_aft + damp * d_aft, t_fwd + damp * d_fwd
            if max(na, nf) <= t_max and min(na, nf) > -t_max:
                break
            damp *= 0.5
        t_aft += damp * d_aft
        t_fwd += damp * d_fwd
    raise ValueError("waterline solve did not converge — check the hydrostatics table")


def buoyancy_hydrostatics_table(
    x: np.ndarray,
    w_per_m: np.ndarray,
    stations: list[HydroStation],
    rho_t_m3: float = RHO_SEAWATER_T_M3,
) -> tuple[np.ndarray, dict]:
    displacement = _cumtrapz(w_per_m, x)[-1]
    lcg = _cumtrapz(w_per_m * x, x)[-1] / displacement
    b, t_aft, t_fwd = solve_waterline(stations, displacement, lcg, x, rho_t_m3)
    return b, {
        "method": "hydrostatics_table",
        "draft_aft_m": float(t_aft),
        "draft_fwd_m": float(t_fwd),
        "draft_mean_m": float(0.5 * (t_aft + t_fwd)),
        "trim_m": float(t_fwd - t_aft),  # positive = by the bow
    }


def buoyancy_direct(
    x: np.ndarray, x_b: np.ndarray, b_per_m: np.ndarray
) -> tuple[np.ndarray, dict]:
    if len(x_b) != len(b_per_m) or len(x_b) < 2:
        raise ValueError("direct buoyancy needs matching x/value lists (>= 2 points)")
    return np.interp(x, x_b, b_per_m), {"method": "direct"}


# --------------------------------------------------------------------------- #
# Still-water SF/BM
# --------------------------------------------------------------------------- #
def still_water_sf_bm(
    x: np.ndarray,
    w_per_m: np.ndarray,
    b_per_m: np.ndarray,
    closure_correction: bool = True,
    closure_tolerance: float = DEFAULT_CLOSURE_TOLERANCE,
    equilibrium: dict | None = None,
) -> StillWaterResult:
    """Integrate the load curve and report SF/BM with closure diagnostics.

    The *raw* end residuals (before any correction) are reported as fractions
    of displacement / displacement*length; ``closure_ok`` compares them to
    ``closure_tolerance``. If ``closure_correction`` is set, a linear
    correction is added to the buoyancy so the discrete integrals close
    exactly (standard loadicator practice); the correction never hides a bad
    balance because the raw residuals are what is checked.
    """
    x = np.asarray(x, dtype=float)
    w = np.asarray(w_per_m, dtype=float)
    b = np.asarray(b_per_m, dtype=float)
    length = float(x[-1] - x[0])
    displacement = float(_cumtrapz(w, x)[-1])
    lcg = float(_cumtrapz(w * x, x)[-1] / displacement)

    q_raw = w - b
    sf_raw = _cumtrapz(q_raw, x)
    bm_raw = _cumtrapz(sf_raw, x)
    frac_sf = float(sf_raw[-1] / displacement)
    frac_bm = float(bm_raw[-1] / (displacement * length))
    closure_ok = abs(frac_sf) <= closure_tolerance and abs(frac_bm) <= closure_tolerance

    if closure_correction:
        b = b + _linear_closure(x, sf_raw[-1], bm_raw[-1])
        sf = _cumtrapz(w - b, x)
        bm = _cumtrapz(sf, x)
    else:
        sf, bm = sf_raw, bm_raw

    i_sf = int(np.argmax(np.abs(sf)))
    i_hog = int(np.argmax(bm))
    i_sag = int(np.argmin(bm))
    return StillWaterResult(
        x_m=tuple(float(v) for v in x),
        weight_per_m_t=tuple(float(v) for v in w),
        buoyancy_per_m_t=tuple(float(v) for v in b),
        shear_force_t=tuple(float(v) for v in sf),
        bending_moment_t_m=tuple(float(v) for v in bm),
        displacement_t=displacement,
        lcg_m=lcg,
        max_shear_t=float(sf[i_sf]),
        x_max_shear_m=float(x[i_sf]),
        max_hogging_t_m=max(float(bm[i_hog]), 0.0),
        x_max_hogging_m=float(x[i_hog]),
        max_sagging_t_m=min(float(bm[i_sag]), 0.0),
        x_max_sagging_m=float(x[i_sag]),
        closure_shear_fraction=frac_sf,
        closure_moment_fraction=frac_bm,
        closure_correction_applied=bool(closure_correction),
        closure_ok=bool(closure_ok),
        equilibrium=dict(equilibrium or {}),
    )


# --------------------------------------------------------------------------- #
# Utilisation vs allowables at frames
# --------------------------------------------------------------------------- #
def utilization_at_frames(
    result: StillWaterResult,
    frames: list[tuple[str, float]],
    allowable_sf: AllowableCurve | None = None,
    allowable_bm: AllowableCurve | None = None,
    utilization_limit: float = 1.0,
) -> list[FrameUtilization]:
    """Interpolate SF/BM and the allowable curves at the reporting frames."""
    x = np.asarray(result.x_m)
    sf = np.asarray(result.shear_force_t)
    bm = np.asarray(result.bending_moment_t_m)
    rows: list[FrameUtilization] = []
    for name, xf in frames:
        if not (x[0] - 1e-9 <= xf <= x[-1] + 1e-9):
            raise ValueError(f"frame {name!r} at x={xf} lies outside the hull")
        sf_f = float(np.interp(xf, x, sf))
        bm_f = float(np.interp(xf, x, bm))
        sf_allow = allowable_sf.limit_for(xf, sf_f) if allowable_sf else None
        bm_allow = allowable_bm.limit_for(xf, bm_f) if allowable_bm else None
        sf_util = abs(sf_f) / sf_allow if sf_allow else None
        bm_util = abs(bm_f) / bm_allow if bm_allow else None
        utils = [u for u in (sf_util, bm_util) if u is not None]
        if not utils:
            status = "no-allowable"
        else:
            status = "pass" if max(utils) <= utilization_limit else "fail"
        rows.append(
            FrameUtilization(
                frame=name, x_m=float(xf),
                shear_force_t=sf_f, shear_allowable_t=sf_allow,
                shear_utilization=sf_util,
                bending_moment_t_m=bm_f, bending_allowable_t_m=bm_allow,
                bending_utilization=bm_util, status=status,
            )
        )
    return rows


# --------------------------------------------------------------------------- #
# Section modulus
# --------------------------------------------------------------------------- #
def section_properties(
    elements: list[SectionElement], depth_m: float | None = None
) -> dict:
    """Neutral axis, inertia and deck/keel section moduli of a midship section."""
    if not elements:
        raise ValueError("section needs at least one element")
    area = sum(e.area_m2 for e in elements)
    if area <= 0.0:
        raise ValueError("section area must be positive")
    z_na = sum(e.area_m2 * e.z_m for e in elements) / area
    inertia = sum(
        e.i_self_m4 + e.area_m2 * (e.z_m - z_na) ** 2 for e in elements
    )
    z_top = depth_m if depth_m is not None else max(e.z_m for e in elements)
    if z_top <= z_na or z_na <= 0.0:
        raise ValueError(
            "neutral axis outside (keel, depth) — check element z_m values"
        )
    return {
        "area_m2": area,
        "neutral_axis_m": z_na,
        "inertia_m4": inertia,
        "sm_deck_m3": inertia / (z_top - z_na),
        "sm_keel_m3": inertia / z_na,
    }


def section_modulus_screen(
    result: StillWaterResult,
    sm_deck_m3: float,
    sm_keel_m3: float | None,
    yield_mpa: float,
    sm_source: str,
    wave: dict | None = None,
) -> list[SectionModulusScreen]:
    """Screen deck/keel bending stress at the still-water hog/sag extremes.

    If ``wave`` supplies ``{"beam_m": ..., "block_coefficient": ...}`` (and
    optionally ``length_m``), the IACS UR S11 wave bending moment is combined
    with the still-water extreme per condition; otherwise the still-water
    moment alone is screened.
    """
    length = result.x_m[-1] - result.x_m[0]
    msw = {
        "hogging": result.max_hogging_t_m * G_M_S2,   # kN·m
        "sagging": result.max_sagging_t_m * G_M_S2,
    }
    mwv = {"hogging": 0.0, "sagging": 0.0}
    if wave:
        wbm = wave_bending_moment(
            float(wave.get("length_m", length)),
            float(wave["beam_m"]),
            float(wave["block_coefficient"]),
        )
        mwv = {"hogging": wbm.hogging_kn_m, "sagging": wbm.sagging_kn_m}

    screens: list[SectionModulusScreen] = []
    moduli = [("deck", sm_deck_m3)]
    if sm_keel_m3 is not None:
        moduli.append(("keel", sm_keel_m3))
    for condition in ("hogging", "sagging"):
        total = msw[condition] + mwv[condition]
        for location, sm in moduli:
            screens.append(
                SectionModulusScreen(
                    location=location,
                    condition=condition,
                    moment_kn_m=total,
                    still_water_kn_m=msw[condition],
                    wave_kn_m=mwv[condition],
                    section_modulus_m3=sm,
                    sm_source=sm_source,
                    check=section_modulus_check(total, sm, yield_mpa),
                )
            )
    return screens
