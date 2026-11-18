# Copyright (c) 2026 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Optional constant-forcing surface drift; no OpenDrift source is included.

API and velocity relation checked against OpenDrift v1.14.7, commit
4cfbbbf813e0642175f8dff840d721859fb21174 (2026-11-13):
https://github.com/OpenDrift/opendrift/tree/4cfbbbf813e0642175f8dff840d721859fb21174
``models/physics_methods.py``: ``advect_ocean_current`` and ``advect_wind``.
This is a synthetic advection check, not oil weathering or operational prediction.
"""

from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from math import isclose, isfinite
from numbers import Real


@dataclass(frozen=True)
class ConstantDriftCase:
    """One surface release with constant east/north forcing in SI units.

    Coordinates are degrees; times are seconds; velocities are m/s. The wind
    drift factor is dimensionless and supplied by the caller. Naive release
    times mean UTC; aware times are converted to UTC for the backend.
    """

    longitude_deg: float
    latitude_deg: float
    release_time: datetime
    duration_s: float
    time_step_s: float
    current_east_m_s: float
    current_north_m_s: float
    wind_east_m_s: float
    wind_north_m_s: float
    wind_drift_factor: float

    def __post_init__(self):
        for field, value in vars(self).items():
            if field == "release_time":
                continue
            if isinstance(value, bool) or not isinstance(value, Real):
                raise ValueError(f"{field} must be finite numeric")
            try:
                number = float(value)
            except (OverflowError, ValueError) as exc:
                raise ValueError(f"{field} must be finite numeric") from exc
            if not isfinite(number):
                raise ValueError(f"{field} must be finite numeric")
            object.__setattr__(self, field, number)
        if not isinstance(self.release_time, datetime) or self.release_time != self.release_time:
            raise ValueError("release_time must be a valid datetime")
        if not -180 <= self.longitude_deg <= 180:
            raise ValueError("longitude_deg must be within [-180, 180]")
        if not -90 <= self.latitude_deg <= 90:
            raise ValueError("latitude_deg must be within [-90, 90]")
        if self.duration_s <= 0:
            raise ValueError("duration_s must be positive")
        if self.time_step_s <= 0:
            raise ValueError("time_step_s must be positive")
        steps = self.duration_s / self.time_step_s
        if not isfinite(steps) or not isclose(steps, round(steps), rel_tol=0, abs_tol=1e-9):
            raise ValueError("time_step_s must divide duration_s")
        if self.time_step_s > self.duration_s:
            raise ValueError("time_step_s must not exceed duration_s")
        if self.wind_drift_factor < 0:
            raise ValueError("wind_drift_factor must be nonnegative")


def advective_displacement_m(case: ConstantDriftCase) -> tuple[float, float]:
    """Return east and north displacement in metres: (current + factor*wind)*T."""
    return (
        (case.current_east_m_s + case.wind_drift_factor * case.wind_east_m_s) * case.duration_s,
        (case.current_north_m_s + case.wind_drift_factor * case.wind_north_m_s) * case.duration_s,
    )


def run_constant_drift(case: ConstantDriftCase) -> tuple[float, float]:
    """Run one particle without diffusion; return final longitude/latitude degrees.

    OpenDrift is installed separately. A missing transitive dependency retains
    its original ModuleNotFoundError. Constant forcing needs no network reader.
    """
    try:
        from opendrift.models.oceandrift import OceanDrift
        from opendrift.readers.reader_constant import Reader
    except ModuleNotFoundError as exc:
        if exc.name != "opendrift":
            raise
        raise ImportError(
            "Install OpenDrift (opendrift==1.14.7, GPL-2.0) separately for drift modelling"
        ) from exc

    model = OceanDrift(loglevel=50)
    for key, value in {
        "general:use_auto_landmask": False,
        "drift:horizontal_diffusivity": 0,
        "drift:current_uncertainty": 0,
        "drift:current_uncertainty_uniform": 0,
        "drift:wind_uncertainty": 0,
        "drift:relative_wind": False,
        "drift:stokes_drift": False,
        "drift:vertical_mixing": False,
        "drift:vertical_advection": False,
    }.items():
        model.set_config(key, value)
    model.add_reader(Reader({
        "x_sea_water_velocity": case.current_east_m_s,
        "y_sea_water_velocity": case.current_north_m_s,
        "x_wind": case.wind_east_m_s,
        "y_wind": case.wind_north_m_s,
        "land_binary_mask": 0,
    }))
    time = case.release_time
    if time.utcoffset() is not None:
        time = time.astimezone(timezone.utc).replace(tzinfo=None)
    model.seed_elements(lon=case.longitude_deg, lat=case.latitude_deg, time=time,
                        number=1, z=0, wind_drift_factor=case.wind_drift_factor,
                        current_drift_factor=1, terminal_velocity=0)
    model.run(duration=timedelta(seconds=case.duration_s),
              time_step=case.time_step_s, time_step_output=case.time_step_s)
    return _final_coordinates(model)


def _final_coordinates(model) -> tuple[float, float]:
    # OpenDrift can return normally after an error beyond the first time step.
    if model.steps_calculation != model.expected_steps_calculation:
        raise RuntimeError("OpenDrift did not complete the requested duration")
    final = model.result.isel(trajectory=0, time=-1)
    coordinates = float(final.lon), float(final.lat)
    if not all(isfinite(value) for value in coordinates):
        raise RuntimeError("OpenDrift final coordinates must be finite")
    return coordinates
