# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Profile validation without optional solver dependencies."""

from copy import deepcopy

import pytest

from digitalmodel.geotechnical.slope_profile import validate_profile


def profile():
    return {
        "units": "SI",
        "ground_surface_m": [[0, 0], [20, 20], [100, 20]],
        "bottom_elevation_m": 0,
        "water_unit_weight_kn_m3": 9.81,
        "layers": [{"unit_weight_kn_m3": 20, "c_kpa": 10, "phi_deg": 25}],
        "circles": [{"x_m": 10, "y_m": 40, "radius_m": 40}],
    }


def test_si_units_and_input_preservation():
    data = profile()
    before = deepcopy(data)
    result = validate_profile(data)
    assert data == before
    assert result["layers"][0]["unit_weight_kn_m3"] == 20
    assert result["layers"][0]["c_kpa"] == 10


def test_overlapping_fields_and_undrained_gradient():
    data = profile()
    data["layers"] = [{"gamma_kn_m3": 18, "Su_kpa": 12,
                       "su_gradient_kpa_per_m": 2, "reference_elevation_m": 20}]
    layer = validate_profile(data)["layers"][0]
    assert layer["unit_weight_kn_m3"] == 18
    assert layer["su_kpa"] == 12
    assert layer["su_gradient_kpa_per_m"] == 2


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf"), True])
@pytest.mark.parametrize("field", ["unit_weight_kn_m3", "c_kpa", "phi_deg"])
def test_nonfinite_soil_fields(field, value):
    data = profile()
    data["layers"][0][field] = value
    with pytest.raises(ValueError, match=field):
        validate_profile(data)


@pytest.mark.parametrize("change", [
    {"units": "imperial"},
    {"ground_surface_m": [[0, 0], [0, 1]]},
    {"bottom_elevation_m": 30},
    {"seismic_coefficient": float("nan")},
    {"circles": [{"x_m": 10, "y_m": 40, "radius_m": -1}]},
    {"water_table_m": [[0, float("nan")], [100, 10]]},
])
def test_inconsistent_global_inputs(change):
    with pytest.raises(ValueError):
        validate_profile(profile() | change)


@pytest.mark.parametrize("change", [
    {"unit_weight_kn_m3": -1}, {"phi_deg": 90}, {"c_kpa": -1},
    {"Su_kpa": 10}, {"permeability_m_s": 0},
])
def test_inconsistent_layer_inputs(change):
    data = profile()
    data["layers"][0].update(change)
    with pytest.raises(ValueError):
        validate_profile(data)


def test_crossing_layer_boundaries_rejected():
    data = profile()
    data["layers"].append({"unit_weight_kn_m3": 20, "c_kpa": 0,
                           "phi_deg": 30, "top_boundary_m": [[0, -1], [100, 21]]})
    with pytest.raises(ValueError, match="boundary"):
        validate_profile(data)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf"), True])
@pytest.mark.parametrize("field", ["Su_kpa", "su_gradient_kpa_per_m",
                                   "reference_elevation_m",
                                   "saturated_unit_weight_kn_m3", "permeability_m_s"])
def test_optional_soil_fields_reject_nonfinite(field, value):
    data = profile()
    layer = {"unit_weight_kn_m3": 20, "Su_kpa": 10}
    layer[field] = value
    data["layers"] = [layer]
    with pytest.raises(ValueError):
        validate_profile(data)


def test_conflicting_aliases_rejected():
    data = profile()
    data["layers"][0]["gamma_kn_m3"] = 18
    with pytest.raises(ValueError, match="conflicting"):
        validate_profile(data)


def test_unsupported_layer_fields_rejected():
    data = profile()
    data["layers"][0]["N1_60cs"] = 10
    with pytest.raises(ValueError, match="unsupported"):
        validate_profile(data)


@pytest.mark.parametrize("datum", [-100, 100])
def test_positive_strength_gradient_requires_explicit_datum(datum, tmp_path):
    data = profile()
    data["ground_surface_m"] = [[x, y + datum] for x, y in data["ground_surface_m"]]
    data["bottom_elevation_m"] += datum
    data["layers"] = [{"unit_weight_kn_m3": 18, "su_kpa": 5,
                       "su_gradient_kpa_per_m": 1.5}]
    with pytest.raises(ValueError, match="reference_elevation_m"):
        validate_profile(data)
    data["layers"][0]["reference_elevation_m"] = datum + 20
    layer = validate_profile(data)["layers"][0]
    assert layer["reference_elevation_m"] == datum + 20
    pytest.importorskip("xslope")
    from digitalmodel.geotechnical.slope_stability import build_slope_model
    from xslope.slice import generate_slices

    data["circles"][0]["y_m"] += datum
    model = build_slope_model(data, tmp_path)
    assert model["materials"][0]["r_elev"] == datum + 20
    success, generated = generate_slices(model, circle=model["circles"][0], num_slices=20)
    assert success
    slices, _ = generated
    inside = slices[(slices["y_cb"] > datum) & (slices["y_cb"] < datum + 20)]
    assert not inside.empty
    for _, row in inside.iterrows():
        assert row["c"] == pytest.approx(5 + 1.5 * (datum + 20 - row["y_cb"]))


def test_constant_undrained_strength_needs_no_datum():
    data = profile()
    data["layers"] = [{"unit_weight_kn_m3": 18, "su_kpa": 5}]
    assert validate_profile(data)["layers"][0]["su_gradient_kpa_per_m"] == 0
