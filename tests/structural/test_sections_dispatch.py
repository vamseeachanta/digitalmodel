# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Section API contracts using distinct library values and synthetic geometry."""

import json
import math
from types import MappingProxyType
from unittest.mock import Mock

import pytest
from sectionproperties.pre.library import rectangular_section

from digitalmodel import sections


def test_extraction_preserves_aisc_modulus_meanings(monkeypatch):
    """Plastic Z and positive-fibre elastic S retain both axis meanings."""
    library = Mock()
    for name, value in {
        "get_area": 8.0, "get_ic": (72.0, 18.0, 0.0),
        "get_z": (11.0, 12.0, 13.0, 14.0), "get_s": (21.0, 22.0),
        "get_j": 31.0, "get_gamma": 32.0,
    }.items():
        getattr(library, name).return_value = value
    monkeypatch.setattr(sections, "Section", lambda geom: library)
    result = sections._analyze_section(Mock(), "synthetic", "custom", 20)
    assert result.to_dict() == dict(
        label="synthetic", section_type="custom", area=8.0, ixx=72.0, iyy=18.0,
        zx=21.0, zy=22.0, sx=11.0, sy=13.0, j=31.0, cw=32.0, rx=3.0, ry=1.5,
    )


@pytest.fixture(scope="module")
def rectangle():
    """Analyze a synthetic 20 by 40 mm rectangle once for both axes."""
    return sections._analyze_section(
        rectangular_section(d=40, b=20), "rectangle", "custom", 20
    )


@pytest.mark.parametrize("name,expected", [
    ("sx", 20 * 40**2 / 6), ("sy", 40 * 20**2 / 6),
    ("zx", 20 * 40**2 / 4), ("zy", 40 * 20**2 / 4),
])
def test_rectangle_moduli_match_area_integrals(rectangle, name, expected):
    """Use the integration suite's 1% numerical allowance, not a design limit."""
    assert getattr(rectangle, name) == pytest.approx(expected, rel=1e-2)


SHAPES = [
    ("i_section", dict(d=40, b=20, t_f=3, t_w=2, r=0, n_r=4)),
    ("chs", dict(d=20, t=2, n=32)),
    ("rhs", dict(d=40, b=20, t=2, r_out=3, n_r=4)),
    ("angle", dict(d=40, b=20, t=2, r_r=2, r_t=1, n_r=4)),
    ("channel", dict(d=40, b=20, t_f=3, t_w=2, r=0, n_r=4)),
]


@pytest.mark.parametrize("section_type,dimensions", SHAPES)
def test_dispatch_forwards_options_without_mutating_input(
    monkeypatch, section_type, dimensions
):
    """Each selector calls its existing wrapper and preserves the caller mapping."""
    source = dict(dimensions, mesh_size=20, label="synthetic")
    result = sections.SectionProperties("synthetic", section_type, 1, 2, 3, 4, 5, 6, 7)
    calls = []

    def wrapper(**forwarded):
        calls.append(forwarded.copy())
        forwarded.clear()
        return result

    monkeypatch.setattr(sections, f"compute_{section_type}", wrapper)
    actual = sections.compute_section_properties(section_type, MappingProxyType(source))
    assert actual is result
    assert calls == [dict(dimensions, mesh_size=20, label="synthetic")]
    assert source == dict(dimensions, mesh_size=20, label="synthetic")
    assert json.loads(json.dumps(actual.to_dict())) == actual.to_dict()


@pytest.fixture
def no_analysis(monkeypatch):
    """Invalid requests cannot reach meshing or property analysis."""
    guard = Mock(side_effect=AssertionError("analysis reached for invalid input"))
    monkeypatch.setattr(sections, "_analyze_section", guard)
    return guard


@pytest.mark.parametrize("selector", ["tee", "custom", "__dict__", "CHS", "", None, []])
def test_unsupported_selectors_raise_before_analysis(no_analysis, selector):
    with pytest.raises(ValueError, match="Unsupported section type"):
        sections.compute_section_properties(selector, {"d": 20, "t": 2})
    no_analysis.assert_not_called()


@pytest.mark.parametrize("dimensions", [None, [], "d=20", 20])
def test_non_mapping_dimensions_raise_before_analysis(no_analysis, dimensions):
    with pytest.raises(TypeError, match="dimensions must be a mapping"):
        sections.compute_section_properties("chs", dimensions)
    no_analysis.assert_not_called()


@pytest.mark.parametrize("section_type,dimensions", SHAPES)
@pytest.mark.parametrize("invalid", ["missing", "extra"])
def test_invalid_keys_raise_before_analysis(no_analysis, section_type, dimensions, invalid):
    supplied = dict(dimensions)
    if invalid == "missing":
        del supplied["d"]
        message = "missing a required argument: 'd'"
    else:
        supplied["thickness"] = 2
        message = "unexpected keyword argument 'thickness'"
    with pytest.raises(TypeError, match=message):
        sections.compute_section_properties(section_type, supplied)
    no_analysis.assert_not_called()


class UnitBearingFloat(float):
    """Expose accidental acceptance or coercion of a unit-bearing scalar."""

    units = "mm"


@pytest.mark.parametrize("key,value", [
    ("d", "20"), ("d", UnitBearingFloat(20)), ("t", True),
    ("mesh_size", UnitBearingFloat(20)), ("n", "64"), ("label", 20),
    ("d", float("nan")), ("d", float("inf")),
    ("d", 10**400), ("d", -(10**400)), ("mesh_size", 10**400),
])
def test_invalid_values_raise_before_analysis(no_analysis, key, value):
    dimensions = {"d": 20, "t": 2, key: value}
    with pytest.raises(TypeError, match=key):
        sections.compute_section_properties("chs", dimensions)
    no_analysis.assert_not_called()


@pytest.mark.parametrize("section_type,dimensions", SHAPES)
@pytest.mark.parametrize("value", [4.5, 4.0, True, UnitBearingFloat(4)])
def test_point_counts_require_integers_before_dispatch(
    monkeypatch, no_analysis, section_type, dimensions, value
):
    """All selectors reject non-integer counts before geometry construction."""
    key = "n" if section_type == "chs" else "n_r"
    supplied = dict(dimensions, **{key: value})
    builders = {
        "i_section": "i_section", "chs": "circular_hollow_section",
        "rhs": "rectangular_hollow_section", "angle": "angle_section",
        "channel": "channel_section",
    }
    geometry = Mock(side_effect=AssertionError("geometry reached for invalid count"))
    monkeypatch.setattr(sections, builders[section_type], geometry)
    with pytest.raises(TypeError, match=rf"^{key} must be"):
        sections.compute_section_properties(section_type, supplied)
    geometry.assert_not_called()
    no_analysis.assert_not_called()


def test_dispatch_computes_serializable_chs_properties():
    """The public API analyzes a synthetic tube using the existing mm basis."""
    result = sections.compute_section_properties("chs", dict(d=20, t=2))
    assert isinstance(result, sections.SectionProperties)
    assert result.area == pytest.approx(math.pi * (10**2 - 8**2), rel=0.01)
    assert json.loads(json.dumps(result.to_dict())) == result.to_dict()
