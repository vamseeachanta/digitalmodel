# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Contracts for solver-free configuration and excursion screening."""

import importlib
import math

import pytest


def kernel():
    """Load the configuration adapter after asserting that it exists."""
    spec = importlib.util.find_spec("digitalmodel.riser_configuration_screen")
    assert spec is not None, "configuration screening adapter is missing"
    return importlib.import_module("digitalmodel.riser_configuration_screen.static")


def case(buoyancy=2.0):
    """Return assumed synthetic values, independent of earlier results."""
    return {
        "water_depth_m": 1500.0,
        "submerged_weight_n_m": 1000.0,
        "grounded_length_m": 500.0,
        "hangoff_angle_deg": 10.0,
        "sag_fraction": 0.1,
        "hog_fraction": 0.4,
        "buoyancy_factor": buoyancy,
    }


def test_buoyancy_changes_excursion_curvature():
    api = kernel()
    low = api.lazy_wave_offsets(case(1.5), [-50.0, 0.0, 50.0])
    high = api.lazy_wave_offsets(case(3.5), [-50.0, 0.0, 50.0])
    def spread(rows):
        values = [row["touchdown_curvature_per_m"] for row in rows]
        return max(values) - min(values)
    assert not math.isclose(spread(low), spread(high), rel_tol=1e-5)


def test_insufficient_buoyancy_is_not_mirrored():
    api = kernel()
    with pytest.raises(ValueError, match="buoyan"):
        api.lazy_wave_offsets(case(0.5), [-50.0, 0.0, 50.0])


def test_shapes_close_and_buoyancy_is_fixed_across_offsets():
    rows = kernel().lazy_wave_offsets(case(), [-50.0, 0.0, 50.0])
    for row in rows:
        assert abs(row["vertical_closure_error_m"]) < 1e-6
        assert abs(row["horizontal_closure_error_m"]) < 1e-6
        assert row["shape"][-1][2] == pytest.approx(0.0, abs=1e-6)
        assert row["buoyed_length_m"] == pytest.approx(rows[1]["buoyed_length_m"], abs=1e-6)
        assert row["buoyed_start_m"] == pytest.approx(rows[1]["buoyed_start_m"], abs=1e-6)


def test_scr_matches_independent_inextensible_catenary():
    api = kernel()
    row = api.scr_offsets(case(), [0.0])[0]
    angle = math.radians(10.0)
    a = 1500.0 * math.sin(angle) / (1.0 - math.sin(angle))
    length = math.sqrt(1500.0**2 + 2.0*a*1500.0)
    expected_span = a * math.asinh(length / a)
    assert row["horizontal_tension_n"] == pytest.approx(a*1000.0, rel=1e-7)
    assert row["suspended_length_m"] == pytest.approx(length, rel=1e-7)
    assert row["horizontal_span_m"] == pytest.approx(expected_span, rel=1e-7)
    assert row["top_angle_deg"] == pytest.approx(10.0, abs=1e-6)
