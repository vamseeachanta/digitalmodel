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


@pytest.mark.parametrize("elevation_shift", [-100, 100])
@pytest.mark.parametrize("datum_offset", [-1, 0, 1])
def test_strength_datum_relative_to_model_bottom(elevation_shift, datum_offset):
    """Hand calculation using the guide's xslope 1.0.3 cp equation (2026).

    Synthetic geometry: lower elevation B is the elevation shift; datum R is
    B + offset. For offset -1, every layer point y >= B has R-y <= -1,
    so max(0, R-y) = 0 and the positive gradient is inert. Offset 0 is the
    exact boundary; offset 1 permits a positive depth term near the bottom.
    The guide's illustrative Su_ref=5 kPa and gradient=1.5 kPa/m are reused;
    these are synthetic inputs, not design coefficients or acceptance limits.
    """
    data = profile()
    data["ground_surface_m"] = [
        [x, y + elevation_shift] for x, y in data["ground_surface_m"]
    ]
    data["bottom_elevation_m"] = elevation_shift
    data["layers"] = [{"unit_weight_kn_m3": 20, "su_kpa": 5,
                       "su_gradient_kpa_per_m": 1.5,
                       "reference_elevation_m": elevation_shift + datum_offset}]
    before = deepcopy(data)
    if datum_offset < 0:
        with pytest.raises(ValueError, match=r"layers\[0\].reference_elevation_m.*below"):
            validate_profile(data)
    else:
        result = validate_profile(data)
        assert result["layers"][0]["reference_elevation_m"] == (
            elevation_shift + datum_offset
        )
    assert data == before


@pytest.mark.parametrize(("layer_index", "lower_minimum"), [(0, -5), (1, -20), (2, -30)])
@pytest.mark.parametrize("datum_offset", [-1, 0, 1, 5])
def test_strength_datum_uses_each_layer_lower_minimum(
    layer_index, lower_minimum, datum_offset
):
    """Independent geometry and cp hand calculation; no solver is needed.

    Synthetic layer tops: ground, horizontal -5 m, then a V with elevations
    -10, -20, -10 m at x=0,40,100 m; model bottom=-30 m. Linear segments
    attain their minima at vertices, so the three lower minima are -5, -20,
    -30 m. R=min+(-1) lies below the whole layer, making max(0,R-y)=0.
    R=min is the permitted equality. R=min+1 or min+5 permits growth in
    part of the layer, even below the V's -10 m endpoints. The cp equation
    and illustrative strength inputs are from the slope guide (xslope 1.0.3,
    2026); all geometry is synthetic, not a design recommendation.
    """
    data = profile()
    data["bottom_elevation_m"] = -30
    data["layers"] += [
        data["layers"][0] | {"top_boundary_m": [[0, -5], [100, -5]]},
        data["layers"][0] | {"top_boundary_m": [[0, -10], [40, -20], [100, -10]]},
    ]
    layer = data["layers"][layer_index]
    del layer["c_kpa"], layer["phi_deg"]
    layer.update(su_kpa=5, su_gradient_kpa_per_m=1.5,
                 reference_elevation_m=lower_minimum + datum_offset)
    before = deepcopy(data)
    if datum_offset < 0:
        with pytest.raises(
            ValueError, match=rf"layers\[{layer_index}\].reference_elevation_m.*below"
        ):
            validate_profile(data)
    else:
        result = validate_profile(data)
        assert result["layers"][layer_index]["reference_elevation_m"] == (
            lower_minimum + datum_offset
        )
    assert data == before


@pytest.mark.parametrize("gradient", [None, 0])
def test_zero_strength_gradient_accepts_datum_below_layer(gradient):
    """Guide cp equation: Su=5+0*max(0,-1-y)=5 for every y >= bottom=0.

    A synthetic datum of -1 m is below the bottom, but an omitted or explicit
    zero gradient is datum-independent and must remain valid (xslope 1.0.3,
    2026 cp rule). Weight 20 and Su 5 reuse the existing synthetic profile.
    """
    data = profile()
    data["layers"] = [{"unit_weight_kn_m3": 20, "su_kpa": 5,
                       "reference_elevation_m": -1}]
    if gradient is not None:
        data["layers"][0]["su_gradient_kpa_per_m"] = gradient
    result = validate_profile(data)
    assert result["layers"][0]["su_gradient_kpa_per_m"] == 0
    assert result["layers"][0]["reference_elevation_m"] == -1
