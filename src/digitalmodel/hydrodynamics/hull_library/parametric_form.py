"""Dense synthetic monohulls with analytic sections and volume-calibrated ends.

Coordinates are metres, x is forward from AP, and z is upward from the keel.
See docs/domains/hull_library/parametric-form.md for the SAC feasibility contract.
"""

from dataclasses import dataclass
from functools import lru_cache
from itertools import product

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, model_validator
from scipy.integrate import simpson
from scipy.optimize import brentq
from scipy.special import betainc

from .parametric_hull import ParametricRange
from .profile_schema import HullProfile, HullStation, HullType


class MonohullFormParameters(BaseModel):
    """Underwater form targets; box and Wigley are explicit analytic controls."""

    model_config = ConfigDict(frozen=True, extra="forbid", allow_inf_nan=False)
    length_bp: float = Field(gt=0)
    beam: float = Field(gt=0)
    draft: float = Field(gt=0)
    depth: float = Field(gt=0)
    cb: float = Field(default=0.7, ge=0.35, le=1)
    lcb_fraction: float = Field(default=0, gt=-0.5, lt=0.5)
    parallel_midbody_fraction: float = Field(default=0.4, ge=0, le=0.8)
    bilge_radius_fraction: float = Field(default=0.2, ge=0, le=1)
    deadrise_deg: float = Field(default=0, ge=0, le=30)
    flare_deg: float = Field(default=0, ge=-15, le=30)
    bow_fullness: float = Field(default=2, ge=1, le=4)
    stern_fullness: float = Field(default=2, ge=1, le=4)
    transom_fraction: float = Field(default=0, ge=0, le=0.95)
    n_stations: int = Field(default=41, ge=9)
    n_waterlines: int = Field(default=21, ge=5)
    box: bool = False
    wigley: bool = False

    @model_validator(mode="after")
    def _validate_geometry(self):
        if self.depth < self.draft:
            raise ValueError("depth must be at least draft")
        for name in ("n_stations", "n_waterlines"):
            if getattr(self, name) % 2 != 1:
                raise ValueError(f"{name} must be odd for Simpson integration")
        if self.box or self.wigley:
            expected = 1 if self.box else 4 / 9
            if self.box and self.wigley or not np.isclose(self.cb, expected):
                raise ValueError("box/wigley requires its analytic cb target")
            if any(
                (
                    self.bilge_radius_fraction,
                    self.deadrise_deg,
                    self.flare_deg,
                    self.lcb_fraction,
                    self.transom_fraction,
                )
            ):
                raise ValueError(
                    "box/wigley requires zero section angles, bilge, LCB and transom"
                )
        elif self.cb > 0.95:
            raise ValueError("cb must not exceed 0.95 except for box=True")
        if self.cb > midship_area(self) / (self.beam * self.draft) + 1e-12:
            raise ValueError(
                "cb exceeds cm implied by bilge_radius_fraction/deadrise_deg/flare_deg"
            )
        return self


def _section_geometry(p):
    h, r = p.beam / 2, p.bilge_radius_fraction * p.beam / 2
    m, k = np.tan(np.deg2rad([p.deadrise_deg, p.flare_deg]))
    yc = (h - k * p.draft - r * np.hypot(1, k) + k * r * np.hypot(1, m)) / (1 - k * m)
    zc = m * yc + r * np.hypot(1, m)
    zb, zs = zc - r / np.hypot(1, m), zc - r * k / np.hypot(1, k)
    if zb < -1e-10 or zs > p.draft + 1e-10 or yc + r * m / np.hypot(1, m) < -1e-10:
        raise ValueError(
            "bilge_radius_fraction/deadrise_deg/flare_deg cannot fit beam and draft"
        )
    if max(h, yc + r) > 1.05 * h + 1e-10:
        raise ValueError(
            "flare_deg creates tumblehome wider than profile beam tolerance"
        )
    return r, m, k, yc, zc, max(zb, 0), min(zs, p.draft)


def midship_section(params, z):
    """Closed-form half-breadth: deadrise line, tangent circular bilge, side."""
    p, z = params, np.asarray(z, dtype=float)
    if not np.isfinite(z).all() or np.any((z < 0) | (z > p.draft)):
        raise ValueError("z must be finite and within [0, draft]")
    if p.box:
        return np.full_like(z, p.beam / 2)
    if p.wigley:
        return p.beam / 2 * (1 - (1 - z / p.draft) ** 2)
    r, m, k, yc, zc, zb, zs = _section_geometry(p)
    arc = yc + np.sqrt(np.maximum(0, r * r - (z - zc) ** 2))
    bottom = z / m if m > 0 else np.full_like(z, yc)
    side = p.beam / 2 + k * (z - p.draft)
    return np.where(z < zb, bottom, np.where(z < zs, arc, side))


def midship_area(params):
    """Analytic full section area; a semicircular bilge gives cm=pi/4."""
    p = params
    if p.box or p.wigley:
        return p.beam * p.draft * (1 if p.box else 2 / 3)
    r, m, k, yc, zc, zb, zs = _section_geometry(p)
    area = zb * zb / (2 * m) if m > 0 else 0
    if r:
        u = np.clip(np.array([zb - zc, zs - zc]) / r, -1, 1)
        primitive = r * r / 2 * (u * np.sqrt(np.maximum(0, 1 - u * u)) + np.arcsin(u))
        area += yc * (zs - zb) + primitive[1] - primitive[0]
    area += p.beam / 2 * (p.draft - zs) - k / 2 * (p.draft - zs) ** 2
    return 2 * area


def _end_parameters(p, run):
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    cp = p.cb * p.beam * p.draft / midship_area(p)
    mean = (cp - middle - run * transom) / (1 - middle - run * transom)
    if not 0 < mean < 1:
        raise ValueError(
            "cb unreachable with parallel_midbody_fraction and transom_fraction"
        )
    b = np.array([p.stern_fullness, p.bow_fullness]) + 1
    a = b * (1 - mean) / mean
    first = (1 - a * (a + 1) / ((a + b) * (a + b + 1))) / 2
    return mean, a, b, first


def _sac_centroid(p, run):
    mean, _, _, first = _end_parameters(p, run)
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    entrance = 1 - middle - run
    moment = run**2 * (transom / 2 + (1 - transom) * first[0])
    moment += middle * (run + middle / 2) + entrance * mean - entrance**2 * first[1]
    cp = p.cb * p.beam * p.draft / midship_area(p)
    return moment / cp - 0.5


@lru_cache(maxsize=128)
def _solve_sac(p):
    """Solve only the run length; enforce a full section at geometric midships."""
    middle, transom = p.parallel_midbody_fraction, p.transom_fraction**2
    cp = p.cb * p.beam * p.draft / midship_area(p)
    low, high = max(1e-8, 0.5 - middle), min(0.5, 1 - middle - 1e-8)
    if transom:
        high = min(high, (cp - middle) / transom - 1e-8)
    if high < low or cp <= middle or cp >= 1:
        raise ValueError(
            "cb unreachable with parallel_midbody_fraction/transom_fraction"
        )

    def residual(run):
        return _sac_centroid(p, run) - p.lcb_fraction

    if abs(residual(low)) < 1e-12:
        return low
    if high == low or residual(low) * residual(high) > 0:
        raise ValueError(
            "lcb_fraction unreachable with cb, fullness and parallel_midbody_fraction"
        )
    return brentq(residual, low, high, xtol=1e-12)


def sectional_area_curve(params, x):
    """Three-segment SAC; beta ends have exactly calibrated volume and centroid."""
    p, x = params, np.asarray(x, dtype=float)
    if not np.isfinite(x).all() or np.any((x < 0) | (x > p.length_bp)):
        raise ValueError("x must be finite and within [0, length_bp]")
    u, area = x / p.length_bp, midship_area(p)
    if p.box:
        return np.full_like(u, area)
    if p.wigley:
        return area * (1 - (2 * u - 1) ** 2)
    run = _solve_sac(p)
    _, a, b, _ = _end_parameters(p, run)
    entrance = 1 - p.parallel_midbody_fraction - run
    aft = p.transom_fraction**2 + (1 - p.transom_fraction**2) * betainc(
        a[0], b[0], np.clip(u / run, 0, 1)
    )
    fore = betainc(a[1], b[1], np.clip((1 - u) / entrance, 0, 1))
    return area * np.where(u < run, aft, np.where(u > 1 - entrance, fore, 1))


def _grid(count, scale):
    return scale * (1 - np.cos(np.linspace(0, np.pi, count))) / 2


def station_offsets(params, x):
    """Area-preserving sections on a cosine grid; pointed ends retain 0.5% breadth."""
    p = params
    area = float(sectional_area_curve(p, x))
    z = _grid(p.n_waterlines, p.draft)
    base = midship_section(p, z)
    if p.box or p.wigley:
        y = base * area / midship_area(p)
    else:
        ratio = area / midship_area(p)
        run = _solve_sac(p)
        if x / p.length_bp < run and p.transom_fraction:
            width = np.sqrt(ratio)
            if area < width * p.beam / 2 * (z[-1] - z[-2]):
                raise ValueError("transom_fraction unresolved: increase n_waterlines")
            power = 2 / max(width, 0.005)
            narrow = base * (z / p.draft) ** power
            base_area, narrow_area = simpson(base, x=z), simpson(narrow, x=z)
            blend = (1 - width) * base_area / (base_area - narrow_area)
            y = width * ((1 - blend) * base + blend * narrow)
        else:
            distance = max(
                0,
                (x / p.length_bp - run - p.parallel_midbody_fraction)
                / (1 - run - p.parallel_midbody_fraction),
            )
            blend = distance**2 * (3 - 2 * distance) * (1 - ratio)
            shape = base * ((1 - blend) + blend * z / p.draft)
            y = shape * area / (2 * simpson(shape, x=z)) if area else shape * 0
        if ratio > 1 - 1e-12:
            y = base
    if not p.box and (not p.transom_fraction or x >= p.length_bp / 2):
        y = np.sqrt((0.005 * base) ** 2 + (1 - 0.005**2) * y**2)
    if np.any(y < -1e-10) or np.max(y) > 1.05 * p.beam / 2 + 1e-10:
        raise ValueError("cb/section blend produces invalid half-breadths")
    return list(zip(z.tolist(), np.maximum(y, 0).tolist()))


@dataclass(frozen=True)
class FormReport:
    """Achieved coefficients from actual offsets; targets are absent for bare profiles."""

    cb: float
    cp: float
    cm: float
    cwp: float
    lcb_fraction: float
    displaced_volume: float
    hydrostatic_volume: float
    targets: dict | None


def _profile_integrals(profile):
    stations = sorted(profile.stations, key=lambda s: s.x_position)
    x, areas, waterline = [], [], []
    for station in stations:
        offsets = np.asarray(sorted(station.waterline_offsets))
        z = np.unique(
            np.r_[
                0,
                offsets[(offsets[:, 0] > 0) & (offsets[:, 0] < profile.draft), 0],
                profile.draft,
            ]
        )
        y = np.interp(z, offsets[:, 0], offsets[:, 1])
        x.append(station.x_position)
        areas.append(2 * simpson(y, x=z))
        waterline.append(2 * y[-1])
    volume = float(simpson(areas, x=x))
    mid_area = float(np.interp(profile.length_bp / 2, x, areas))
    moment = float(simpson(np.asarray(x) * areas, x=x))
    return volume, mid_area, float(simpson(waterline, x=x)), moment


def _check_resolution(p, profile):
    from digitalmodel.visualization.design_tools.hull_hydrostatics import (
        HullHydrostatics,
    )

    volume, _, _, moment = _profile_integrals(profile)
    cb = volume / (p.length_bp * p.beam * p.draft)
    lcb = moment / volume / p.length_bp - 0.5
    crosscheck = HullHydrostatics(profile).compute_displaced_volume()
    if (
        abs(cb / p.cb - 1) > 0.005
        or abs(lcb - p.lcb_fraction) > 0.002
        or abs(crosscheck / volume - 1) > 0.01
    ):
        raise ValueError(
            "cb/lcb_fraction or volume unresolved: increase n_stations/n_waterlines"
        )
    return cb


def generate_profile(params, name="parametric_monohull") -> HullProfile:
    """Generate wetted offsets only (0 <= z <= draft), with achieved Cb stored."""
    p = params
    profile = HullProfile(
        name=name,
        hull_type=HullType.SHIP,
        source="parametric_form",
        length_bp=p.length_bp,
        beam=p.beam,
        draft=p.draft,
        depth=p.depth,
        stations=[
            HullStation(
                x_position=float(x), waterline_offsets=station_offsets(p, float(x))
            )
            for x in _grid(p.n_stations, p.length_bp)
        ],
    )
    profile.block_coefficient = _check_resolution(p, profile)
    return HullProfile.model_validate(profile.model_dump())


def form_report(profile_or_params) -> FormReport:
    """Compare Simpson volume with the independent trapezoidal hydrostatics consumer."""
    from digitalmodel.visualization.design_tools.hull_hydrostatics import (
        HullHydrostatics,
    )

    p = profile_or_params
    targets = p.model_dump() if isinstance(p, MonohullFormParameters) else None
    profile = generate_profile(p) if targets is not None else p
    volume, middle, waterplane, moment = _profile_integrals(profile)
    cb = volume / (profile.length_bp * profile.beam * profile.draft)
    cm = middle / (profile.beam * profile.draft)
    return FormReport(
        cb,
        cb / cm,
        cm,
        waterplane / (profile.length_bp * profile.beam),
        moment / volume / profile.length_bp - 0.5,
        volume,
        HullHydrostatics(profile).compute_displaced_volume(),
        targets,
    )


def _form_combinations(base, ranges):
    unknown = set(ranges) - MonohullFormParameters.model_fields.keys()
    if unknown:
        raise ValueError(f"unknown form parameters: {sorted(unknown)}")
    keys = list(ranges)
    for values in product(*(ranges[key].values() for key in keys)):
        combo = dict(zip(keys, values))
        yield combo, MonohullFormParameters.model_validate(base.model_dump() | combo)


def sweep_forms(
    base: MonohullFormParameters,
    ranges: dict[str, ParametricRange],
    *,
    screen=False,
    mesh_config=None,
) -> list[dict]:
    """Generate the Cartesian product; unavailable optional screening returns None."""
    from .curvature_screen import hullprod_available, screen_profile

    rows = []
    for combo, params in _form_combinations(base, ranges):
        profile = generate_profile(params)
        report = form_report(profile)
        report = FormReport(**{**report.__dict__, "targets": params.model_dump()})
        signature = (
            screen_profile(profile, mesh_config).signature
            if screen and hullprod_available()
            else None
        )
        rows.append({"parameters": combo, "report": report, "signature": signature})
    return rows
