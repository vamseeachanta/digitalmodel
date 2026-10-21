# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Layer-aware diagnostics for inactive undrained strength gradients.

Basis: xslope 1.0.3 slice.py, cp strength rule, also documented in
https://xslope.org/en/latest/usage/input_template/ (material properties, 2026):
Su(y) = Su_ref + gradient * max(0, reference_elevation - y).
All new numbers below are synthetic inputs for the hand calculations stated
in each test; they are neither site properties nor design acceptance limits.
"""

from copy import deepcopy

import pytest

from digitalmodel.geotechnical.slope_profile import validate_profile
from tests.geotechnical.test_slope_profile import profile


def undrained_profile():
    """Reuse existing geometry with synthetic Su_ref=5 kPa and g=2 kPa/m."""
    data = profile()
    data["layers"] = [{"unit_weight_kn_m3": 20, "su_kpa": 5,
                       "su_gradient_kpa_per_m": 2, "reference_elevation_m": -1}]
    return data


@pytest.mark.parametrize("shift", [-100, 0, 100])
def test_datum_below_model_bottom_rejected_without_mutating_input(shift):
    """Base=0, datum=-1: y>=0 implies max(0,-1-y)=0, so Su=5 everywhere.

    Translating every elevation by -100, 0 or 100 m preserves this inequality;
    the invalidity must be independent of the chosen elevation origin.
    """
    data = undrained_profile()
    data["ground_surface_m"] = [[x, y + shift] for x, y in data["ground_surface_m"]]
    data["bottom_elevation_m"] += shift
    data["layers"][0]["reference_elevation_m"] += shift
    data["circles"][0]["y_m"] += shift
    before = deepcopy(data)
    with pytest.raises(ValueError, match=r"layers\[0\].*reference_elevation_m.*lower boundary"):
        validate_profile(data)
    assert data == before


@pytest.mark.parametrize("reference", [0, 1])
def test_datum_at_or_above_bottom_preserves_existing_acceptance(reference):
    """At base y=0, Su=5+2*max(0,reference): 5 or 7 kPa respectively.

    Equality is retained; this diagnostic concerns strictly lower datums only.
    """
    data = undrained_profile()
    data["layers"][0]["reference_elevation_m"] = reference
    assert validate_profile(data)["layers"][0]["reference_elevation_m"] == reference


def layered_profile():
    """Synthetic lower polyline has minimum -15 m at its interior vertex."""
    data = undrained_profile()
    data["bottom_elevation_m"] = -30
    data["layers"][0]["reference_elevation_m"] = -20
    data["layers"].append({"top_boundary_m": [[0, -5], [20, -15], [100, -5]],
                           "unit_weight_kn_m3": 20, "c_kpa": 0, "phi_deg": 30})
    return data


def test_upper_layer_uses_next_boundary_instead_of_model_base():
    """Upper layer y>=-15, datum=-20: max(0,-20-y)=0, so Su=5 throughout.

    The datum is above the model floor (-30); using that floor would miss it.
    """
    with pytest.raises(ValueError, match=r"layers\[0\].*reference_elevation_m.*lower boundary"):
        validate_profile(layered_profile())


def test_interior_boundary_minimum_keeps_partly_active_gradient():
    """Datum=-10, interior base y=-15: Su=5+2*(-10+15)=15 kPa.

    The end elevations (-5) lie above the datum; testing endpoints alone or
    using the maximum boundary elevation would wrongly reject this profile.
    """
    data = layered_profile()
    data["layers"][0]["reference_elevation_m"] = -10
    assert validate_profile(data)["layers"][0]["reference_elevation_m"] == -10


def test_lowest_layer_uses_model_bottom_and_names_layer():
    """Lower layer y>=-30, datum=-31 gives Su=5 everywhere in that layer."""
    data = layered_profile()
    data["layers"][0]["reference_elevation_m"] = -10
    data["layers"][1] = {**data["layers"][0],
                         "top_boundary_m": data["layers"][1]["top_boundary_m"],
                         "reference_elevation_m": -31}
    with pytest.raises(ValueError, match=r"layers\[1\].*reference_elevation_m.*lower boundary"):
        validate_profile(data)


@pytest.mark.parametrize("layered", [False, True])
def test_zero_gradient_remains_independent_of_datum(layered):
    """With g=0, Su=5+0*max(0,reference-y)=5 for any elevation datum."""
    data = layered_profile() if layered else undrained_profile()
    data["layers"][0]["su_gradient_kpa_per_m"] = 0
    assert validate_profile(data)["layers"][0]["su_kpa"] == 5


def test_adapter_rejects_inactive_gradient_before_optional_imports(tmp_path, monkeypatch):
    """As above, base=0 and datum=-1 make the gradient inactive everywhere."""
    import builtins

    from digitalmodel.geotechnical.slope_stability import build_slope_model

    real_import = builtins.__import__

    def guarded_import(name, *args, **kwargs):
        if name.split(".")[0] in {"xslope", "openpyxl"}:
            pytest.fail("invalid input reached an optional solver import")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", guarded_import)
    with pytest.raises(ValueError, match="reference_elevation_m"):
        build_slope_model(undrained_profile(), tmp_path)
    assert list(tmp_path.iterdir()) == []
