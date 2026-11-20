"""Steady wind/current screening, reusing the existing OCIMF approximation.

A scalar current is uniform. A callable takes height above bed (m); water depth
is then required. Signed velocity-square is integrated over the occupied draft.
Issue 493 exposes profile functions, not a RiverCurrentProfile class.
Directions follow OCIMFLoading's relative axes: positive force follows positive
flow. Explicit signed wind coefficients are used as supplied without reflection;
the caller must match their direction and reference areas to the source table.
The opt-in OCIMF 2020 VLCC model interpolates a published table and requires
explicit Lpp; it does not replace the barge's default approximation.
No wave drift, yaw response, shallow-water correction or tug resistance is added.
"""
from collections.abc import Callable
from dataclasses import dataclass, field
import math

from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.hydrodynamics.models import VesselProperties
from digitalmodel.hydrodynamics.ocimf_loading import OCIMFLoading

from .towline_analysis import _positive
from . import ocimf_vlcc


@dataclass(frozen=True)
class WindProfile:
    speed_m_s: float
    direction_deg: float = 0
    air_density_kg_m3: float = 1.225
    coefficients: tuple[float, float, float] | None = None  # Cx, Cy, Cmz
    coefficient_model: str = 'approximate'
    moment_reference_length_m: float | None = None  # Lpp for the published VLCC table


@dataclass(frozen=True)
class DriftForceResult:
    current_x_n: float
    current_y_n: float
    wind_x_n: float
    wind_y_n: float
    yaw_moment_nm: float
    citations: tuple[Citation, ...] = field(default_factory=tuple)

    @property
    def force_x_n(self) -> float:
        return self.current_x_n + self.wind_x_n

    @property
    def force_y_n(self) -> float:
        return self.current_y_n + self.wind_y_n

    @property
    def resultant_n(self) -> float:
        return math.hypot(self.force_x_n, self.force_y_n)


def _on_axis(loads: dict, direction: float) -> dict:
    """Remove cardinal-angle sign artefacts in the existing current approximation."""
    theta = math.radians(direction)
    if abs(math.cos(theta)) < 1e-12:
        loads['Fx_surge_N'] = 0.0
    if abs(math.sin(theta)) < 1e-12:
        loads['Fy_sway_N'] = 0.0
        loads['Mz_yaw_Nm'] = 0.0
    return loads


def _mean_signed_square(profile, vessel, water_depth, n_slices):
    if not callable(profile):
        if not math.isfinite(profile):
            raise ValueError("current velocity must be finite")
        return profile * abs(profile)
    if water_depth is None or not math.isfinite(water_depth) or water_depth <= vessel.draft:
        raise ValueError("water_depth_m must exceed draft for a callable profile")
    if isinstance(n_slices, bool) or not isinstance(n_slices, int) or n_slices < 1:
        raise ValueError("n_slices must be a positive integer")
    values = []
    for i in range(n_slices):
        height = water_depth - vessel.draft + vessel.draft * (i + 0.5) / n_slices
        speed = float(profile(height))
        if not math.isfinite(speed):
            raise ValueError("current profile must return finite velocities")
        values.append(speed * abs(speed))
    return math.fsum(values) / n_slices


def _wind_load(loader, vessel, wind):
    if wind.coefficient_model not in ('approximate', 'ocimf_2020_vlcc'):
        raise ValueError('unknown wind coefficient model')
    length = wind.moment_reference_length_m
    if length is not None and (not math.isfinite(length) or length <= 0):
        raise ValueError('moment reference length must be finite and positive')
    coefficients, citations = wind.coefficients, ()
    if wind.coefficient_model == 'ocimf_2020_vlcc':
        if coefficients is not None:
            raise ValueError('coefficient model and explicit coefficients are mutually exclusive')
        if length is None:
            raise ValueError('VLCC table requires explicit moment reference length Lpp')
        coefficients = ocimf_vlcc.wind_coefficients(wind.direction_deg)
        citations = (ocimf_vlcc.reference(),)
    if coefficients is None:
        return (loader.wind_load(vessel, wind.speed_m_s, wind.direction_deg,
                                 wind.air_density_kg_m3), citations)
    if len(coefficients) != 3 or not all(math.isfinite(v) for v in coefficients):
        raise ValueError("wind coefficients must be a finite (Cx, Cy, Cmz) triple")
    if vessel.frontal_wind_area is None or vessel.lateral_wind_area is None:
        raise ValueError("tabulated coefficients require explicit wind reference areas")
    cx, cy, cm = coefficients
    q = 0.5 * wind.air_density_kg_m3 * wind.speed_m_s**2
    loads = {'Fx_surge_N': q * vessel.frontal_wind_area * cx,
             'Fy_sway_N': q * vessel.lateral_wind_area * cy,
             'Mz_yaw_Nm': q * vessel.lateral_wind_area * (length or vessel.length_overall) * cm}
    return loads, citations


def _citation() -> Citation:
    citation = Citation(
        code_id='OCIMF-MEG4', publisher='OCIMF', revision='4th-Edition-2018',
        section='Environmental loading; existing OCIMFLoading approximation',
        wiki_path='wikis/engineering/wiki/standards/ocimf-meg4.md',
        source_sibling='generic',
        note='Approximate coefficients are not digitized tables. Overrides are caller-supplied.',
    )
    validate_citation(citation)
    return citation


def compute_drift_force(vessel: VesselProperties,
                        current_profile: float | Callable[[float], float],
                        wind_profile: WindProfile, *, current_direction_deg: float = 0,
                        water_density_kg_m3: float = 1025,
                        water_depth_m: float | None = None,
                        n_slices: int = 1000) -> DriftForceResult:
    """Return force components (N), yaw moment (N m) and a citation sidecar."""
    _positive(length=vessel.length_overall, beam=vessel.beam, draft=vessel.draft,
              water_density=water_density_kg_m3, air_density=wind_profile.air_density_kg_m3)
    if not all(math.isfinite(v) for v in (current_direction_deg, wind_profile.speed_m_s,
                                         wind_profile.direction_deg)):
        raise ValueError("wind velocity and directions must be finite")
    if wind_profile.speed_m_s < 0:
        raise ValueError("wind speed must be nonnegative")
    for area in (vessel.frontal_wind_area, vessel.lateral_wind_area, vessel.freeboard):
        if area is not None and (not math.isfinite(area) or area < 0):
            raise ValueError("wind reference areas and freeboard must be finite and nonnegative")
    citation = _citation()
    pressure_ratio = _mean_signed_square(current_profile, vessel, water_depth_m, n_slices)
    loader = OCIMFLoading()
    direction = (current_direction_deg + 180) % 360 - 180
    current = _on_axis(loader.current_load(vessel, 1, direction, water_density_kg_m3), direction)
    wind, wind_citations = _wind_load(loader, vessel, wind_profile)
    return DriftForceResult(pressure_ratio * current['Fx_surge_N'],
                           pressure_ratio * current['Fy_sway_N'],
                           wind['Fx_surge_N'], wind['Fy_sway_N'],
                           pressure_ratio * current['Mz_yaw_Nm'] + wind['Mz_yaw_Nm'],
                           (citation, *wind_citations))


compute = compute_drift_force
