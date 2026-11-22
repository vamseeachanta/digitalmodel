# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Lookup contracts using synthetic dimensions, without catalog data or FE solves.

The designation strings are lookup labels only. The W14x90 row describes an
invented zero-fillet profile: depth 10 in, flange width 6 in, flange thickness
1 in, web thickness 0.5 in, and design k equal to flange thickness.
Its hand area is 2 * (6 * 1) + (10 - 2 * 1) * 0.5 = 16 square inches.
The other lookup rows use arbitrary depths 12 in and 11 in respectively, with
the same remaining dimensions, so selecting the wrong row is observable.

Unit basis: NIST SP 811, Appendix B.8, inch-to-meter entry: 1 in = 0.0254 m
exactly, hence 25.4 mm. Appendix B.1 identifies bold factors as exact.
https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors
"""

import importlib.util
from pathlib import Path

import pytest
import yaml
from shapely.geometry import box
from shapely.ops import unary_union

PROFILE_IN = {"d": 10.0, "bf": 6.0, "tf": 1.0, "tw": 0.5, "k_des": 1.0}
INCH_TO_MM = 25.4  # Exact conversion from the NIST definition cited above.
# Two flange rectangles plus the clear web rectangle; no fillet contribution.
HAND_AREA_IN2 = 2 * (6.0 * 1.0) + (10.0 - 2 * 1.0) * 0.5
# Floating-point comparison allowance: one trillionth of the hand area.
AREA_REL_TOL = 1e-12


@pytest.fixture
def lookup(tmp_path, monkeypatch):
    """Load a fresh wrapper and an intentionally unsorted temporary catalog."""
    wrapper = (
        Path(__file__).resolve().parents[2]
        / "scripts/integrations/aisc_section_lookup.py"
    )
    spec = importlib.util.spec_from_file_location("aisc_lookup_contract", wrapper)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    catalog = tmp_path / "synthetic_shapes.yaml"
    rows = {
        "W14x90": dict(PROFILE_IN),
        "W12x50": dict(PROFILE_IN, d=12.0),
        "W14x30": dict(PROFILE_IN, d=11.0),
    }
    catalog.write_text(yaml.safe_dump({"shapes": rows}, sort_keys=False))
    monkeypatch.setattr(module, "_CATALOG_PATH", catalog)
    monkeypatch.setattr(module, "_catalog_cache", None)
    try:
        yield module
    finally:
        module._catalog_cache = None


@pytest.mark.parametrize(
    ("name", "depth_in"),
    [
        ("W14x90", 10.0),
        ("W14X90", 10.0),
        ("w14x90", 10.0),
        ("w14X90", 10.0),
        ("w12X50", 12.0),
        ("W14X30", 11.0),
    ],
)
def test_lookup_normalizes_designation(lookup, name, depth_in):
    """Name case does not change the declared synthetic dimensions."""
    assert lookup.get_aisc_dimensions(name) == dict(PROFILE_IN, d=depth_in)


def test_list_shapes_returns_sorted_results(lookup):
    """Catalog insertion order does not determine the public ordering."""
    assert lookup.list_shapes() == ["W12x50", "W14x30", "W14x90"]


@pytest.mark.parametrize(
    ("series", "expected"),
    [
        (None, ["W12x50", "W14x30", "W14x90"]),
        ("", ["W12x50", "W14x30", "W14x90"]),
        ("W14", ["W14x30", "W14x90"]),
        ("w14", ["W14x30", "W14x90"]),
        ("W14X", ["W14x30", "W14x90"]),
        ("w14x", ["W14x30", "W14x90"]),
        ("W12", ["W12x50"]),
        ("W99", []),
    ],
)
def test_series_selection_is_case_normalized_and_sorted(lookup, series, expected):
    """Series filtering retains the sorted subset, including an empty subset."""
    assert lookup.list_shapes(series) == expected


@pytest.mark.parametrize("method", ["get_aisc_dimensions", "get_aisc_geometry"])
def test_unknown_designation_raises_key_error(lookup, method):
    """Both lookup entry points reject missing names and identify choices."""
    with pytest.raises(KeyError, match="Shape 'W99x99' not found") as error:
        getattr(lookup, method)("W99x99")
    assert "Available: W12x50, W14x30, W14x90" in str(error.value)


@pytest.mark.parametrize("unit", ["mm", "inches", "Metric", "", None])
def test_geometry_rejects_invalid_output_units(lookup, unit):
    """Only the documented output unit names are accepted."""
    with pytest.raises(ValueError, match="unit must be 'imperial' or 'metric'"):
        lookup.get_aisc_geometry("W14x90", unit=unit)


def test_dimensions_are_independent_copies(lookup):
    """Mutating one returned dictionary cannot alter later lookup results."""
    first = lookup.get_aisc_dimensions("W14x90")
    second = lookup.get_aisc_dimensions("w14X90")
    assert first is not second
    first["d"] += PROFILE_IN["tf"]
    del first["k_des"]
    assert second == PROFILE_IN
    assert lookup.get_aisc_dimensions("W14X90") == PROFILE_IN


def hand_profile(scale):
    """Build independent flange and clear-web rectangles from the stated example."""
    depth = PROFILE_IN["d"] * scale
    width = PROFILE_IN["bf"] * scale
    flange = PROFILE_IN["tf"] * scale
    web = PROFILE_IN["tw"] * scale
    # The web is centered across the width; two flanges occupy the ends.
    web_left = (width - web) / 2
    web_right = (width + web) / 2
    return unary_union(
        [
            box(0, 0, width, flange),
            box(web_left, flange, web_right, depth - flange),
            box(0, depth - flange, width, depth),
        ]
    )


def test_imperial_geometry_matches_hand_area(lookup):
    """Default output has the declared inch dimensions and hand-computed area."""
    geometry = lookup.get_aisc_geometry("W14x90")
    assert geometry.calculate_area() == pytest.approx(HAND_AREA_IN2, rel=AREA_REL_TOL)
    difference = geometry.geom.symmetric_difference(hand_profile(scale=1.0))
    assert difference.area <= HAND_AREA_IN2 * AREA_REL_TOL
    assert geometry.mesh is None


def test_metric_geometry_scales_hand_area(lookup):
    """Every length scales by the exact inch definition and area by its square."""
    geometry = lookup.get_aisc_geometry("w14X90", unit="metric")
    expected_area_mm2 = HAND_AREA_IN2 * INCH_TO_MM**2
    assert geometry.calculate_area() == pytest.approx(
        expected_area_mm2, rel=AREA_REL_TOL
    )
    difference = geometry.geom.symmetric_difference(hand_profile(scale=INCH_TO_MM))
    assert difference.area <= expected_area_mm2 * AREA_REL_TOL
    assert geometry.mesh is None
