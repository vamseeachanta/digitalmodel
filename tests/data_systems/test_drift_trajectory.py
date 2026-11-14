# Copyright (c) 2026 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic constant-forcing cases; no measured or design data are used."""

import builtins
import importlib
from dataclasses import FrozenInstanceError
from datetime import datetime
from math import atan2, degrees, hypot
from types import SimpleNamespace
from unittest.mock import Mock

import pytest


def case(**overrides):
    from digitalmodel.data_systems.drift_trajectory import ConstantDriftCase

    values = dict(
        longitude_deg=4.0, latitude_deg=60.0, release_time=datetime(2026, 1, 1),
        duration_s=21600, time_step_s=900, current_east_m_s=0.5,
        current_north_m_s=0.0, wind_east_m_s=0.0, wind_north_m_s=10.0,
        wind_drift_factor=0.02,
    )
    return ConstantDriftCase(**(values | overrides))


def test_hand_displacement_combines_current_and_wind_drift():
    from digitalmodel.data_systems.drift_trajectory import advective_displacement_m

    assert advective_displacement_m(case()) == pytest.approx((10800, 4320))


def test_zero_forcing_gives_zero_displacement():
    from digitalmodel.data_systems.drift_trajectory import advective_displacement_m

    assert advective_displacement_m(
        case(current_east_m_s=0, wind_north_m_s=0)
    ) == (0, 0)


@pytest.mark.parametrize("field", [
    "current_east_m_s", "current_north_m_s", "wind_east_m_s", "wind_north_m_s",
    "wind_drift_factor", "duration_s", "time_step_s", "latitude_deg", "longitude_deg",
])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_rejects_nonfinite_case(field, value):
    with pytest.raises(ValueError, match=field):
        case(**{field: value})


@pytest.mark.parametrize(("field", "value"), [
    ("latitude_deg", -90.1), ("latitude_deg", 90.1),
    ("longitude_deg", -180.1), ("longitude_deg", 180.1),
    ("duration_s", 0), ("duration_s", -1),
    ("time_step_s", 0), ("time_step_s", -1), ("time_step_s", 901),
    ("time_step_s", 43200), ("wind_drift_factor", -0.01),
    ("release_time", "2026-01-01"), ("release_time", None),
    ("current_east_m_s", True), ("wind_drift_factor", "0.02"),
])
def test_rejects_invalid_case(field, value):
    with pytest.raises(ValueError, match=field):
        case(**{field: value})


def test_case_is_frozen_and_factor_is_required():
    from digitalmodel.data_systems.drift_trajectory import ConstantDriftCase

    sample = case()
    with pytest.raises(FrozenInstanceError):
        sample.wind_drift_factor = 0
    values = vars(sample).copy()
    del values["wind_drift_factor"]
    with pytest.raises(TypeError, match="wind_drift_factor"):
        ConstantDriftCase(**values)


def block_backend(monkeypatch, missing):
    original_import = builtins.__import__

    def unavailable(name, *args, **kwargs):
        if name.startswith("opendrift"):
            raise ModuleNotFoundError("unavailable", name=missing)
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", unavailable)


def test_adapter_import_does_not_require_backend(monkeypatch):
    block_backend(monkeypatch, "opendrift")
    from digitalmodel.data_systems import drift_trajectory

    importlib.reload(drift_trajectory)
    assert drift_trajectory.advective_displacement_m(case()) == (10800, 4320)


def test_missing_backend_names_opendrift(monkeypatch):
    from digitalmodel.data_systems.drift_trajectory import run_constant_drift

    block_backend(monkeypatch, "opendrift")
    with pytest.raises(ImportError, match=r"OpenDrift.*GPL-2.0.*separately"):
        run_constant_drift(case())


def test_missing_transitive_dependency_is_not_misreported(monkeypatch):
    from digitalmodel.data_systems.drift_trajectory import run_constant_drift

    block_backend(monkeypatch, "netCDF4")
    with pytest.raises(ModuleNotFoundError) as error:
        run_constant_drift(case())
    assert error.value.name == "netCDF4"


@pytest.mark.parametrize(("completed", "longitude"), [(23, 4.0), (24, float("nan"))])
def test_incomplete_or_nonfinite_backend_result_is_rejected(monkeypatch, completed, longitude):
    from digitalmodel.data_systems.drift_trajectory import run_constant_drift

    model = Mock(steps_calculation=completed, expected_steps_calculation=24)
    model.result.isel.return_value = SimpleNamespace(lon=longitude, lat=60.0)
    original_import = builtins.__import__

    def backend(name, *args, **kwargs):
        if name == "opendrift.models.oceandrift":
            return SimpleNamespace(OceanDrift=lambda **kw: model)
        if name == "opendrift.readers.reader_constant":
            return SimpleNamespace(Reader=dict)
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", backend)
    with pytest.raises(RuntimeError, match="complete|finite"):
        run_constant_drift(case())


def test_opendrift_constant_forcing_matches_hand_displacement(record_property):
    pytest.importorskip("opendrift")
    from pyproj import Geod
    from digitalmodel.data_systems.drift_trajectory import (
        advective_displacement_m, run_constant_drift,
    )

    sample = case()
    longitude, latitude = run_constant_drift(sample)
    bearing, _, distance = Geod(ellps="WGS84").inv(
        sample.longitude_deg, sample.latitude_deg, longitude, latitude,
    )
    east, north = advective_displacement_m(sample)
    for name, value in {"longitude_deg": longitude, "latitude_deg": latitude,
                        "distance_m": distance, "bearing_deg": bearing}.items():
        record_property(name, value)
    assert distance == pytest.approx(hypot(east, north), rel=0.01)
    assert bearing == pytest.approx(degrees(atan2(east, north)), abs=0.5)
