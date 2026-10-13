# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Elevation display data and unchanged cathodic-protection calculation values."""

from __future__ import annotations

import copy
import math
import warnings
from pathlib import Path
from typing import Any
from xml.etree import ElementTree as ET

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import FigureBlock, TextBlock

INPUTS = (
    Path(__file__).resolve().parents[1] / "fixtures/cathodic_protection/workflow_inputs"
)
NO_GEOMETRY = (
    "Member geometry and zone elevations are not supplied for this case; "
    "no elevation is drawn."
)


def _cfg() -> dict[str, Any]:
    return yaml.safe_load((INPUTS / "jacket_report_example.yml").read_text())


def _run(cfg: dict[str, Any]) -> dict[str, Any]:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return run_cathodic_protection(copy.deepcopy(cfg))


def _svg(cfg: dict[str, Any]) -> ET.Element:
    spec = anode_design_report(_run(cfg))
    vectors = [figure.svg for figure in spec.figure_blocks() if figure.svg]
    assert len(vectors) == 1, "geometry requires one inline elevation figure"
    return ET.fromstring(vectors[0])


def _members(geometry: dict[str, Any]) -> list[dict[str, Any]]:
    return [
        *geometry["legs"],
        *[member for level in geometry["brace_levels"] for member in level["members"]],
        *geometry["diagonals"],
        *geometry.get("conductors", []),
        *geometry.get("risers", []),
    ]


def _other_geometry(cfg: dict[str, Any]) -> dict[str, Any]:
    other = copy.deepcopy(cfg)
    geometry = other["inputs"]["structure"]["report_geometry"]
    for member in _members(geometry):
        for point in ("start", "end"):
            x, z = member[point]
            member[point] = [x * 0.8 + 1.0, z * 1.3 + 5.0]
    for zone in geometry["zones"]:
        zone["lower_m"] = zone["lower_m"] * 1.3 + 5.0
        zone["upper_m"] = zone["upper_m"] * 1.3 + 5.0
    for level in geometry["brace_levels"]:
        level["elevation_m"] = level["elevation_m"] * 1.3 + 5.0
    for key in ("waterline_m", "mudline_m"):
        geometry[key] = geometry[key] * 1.3 + 5.0
    return other


@pytest.mark.parametrize("different", [False, True])
def test_elevation_zone_elevations_equal_design_data(different: bool) -> None:
    cfg = _other_geometry(_cfg()) if different else _cfg()
    geometry = cfg["inputs"]["structure"]["report_geometry"]
    svg = _svg(cfg)
    zones = {
        node.attrib["data-zone"]: node
        for node in svg.iter()
        if "data-zone" in node.attrib
    }
    assert set(zones) == {zone["name"] for zone in geometry["zones"]}
    for zone in geometry["zones"]:
        assert float(zones[zone["name"]].attrib["data-lower-m"]) == zone["lower_m"]
        assert float(zones[zone["name"]].attrib["data-upper-m"]) == zone["upper_m"]
    levels = {
        node.attrib["data-level"]: node
        for node in svg.iter()
        if "data-level" in node.attrib
    }
    assert (
        float(levels["waterline"].attrib["data-elevation-m"]) == geometry["waterline_m"]
    )
    assert float(levels["mudline"].attrib["data-elevation-m"]) == geometry["mudline_m"]


def test_elevation_is_to_scale() -> None:
    cfg = _cfg()
    svg = _svg(cfg)
    geometry = cfg["inputs"]["structure"]["report_geometry"]
    supplied = {member["name"]: member for member in _members(geometry)}
    drawn = [node for node in svg.iter() if "data-member" in node.attrib]
    assert len(drawn) == len(supplied)
    scale = float(svg.attrib["data-scale"])
    for node in drawn:
        member = supplied[node.attrib["data-member"]]
        true_length = math.dist(member["start"], member["end"])
        drawn_length = math.hypot(
            float(node.attrib["x2"]) - float(node.attrib["x1"]),
            float(node.attrib["y2"]) - float(node.attrib["y1"]),
        )
        assert drawn_length / true_length == pytest.approx(scale)
        assert float(node.attrib["stroke-width"]) / member[
            "diameter_m"
        ] == pytest.approx(scale)


def test_elevation_member_counts_match_design_data() -> None:
    cfg = _cfg()
    geometry = cfg["inputs"]["structure"]["report_geometry"]
    svg = _svg(cfg)
    kinds = [node.attrib.get("data-kind") for node in svg.iter()]
    for kind, key in (
        ("leg", "legs"),
        ("diagonal", "diagonals"),
        ("conductor", "conductors"),
        ("riser", "risers"),
        ("brace-level", "brace_levels"),
    ):
        assert kinds.count(kind) == len(geometry[key])
    assert kinds.count("horizontal") == sum(
        len(level["members"]) for level in geometry["brace_levels"]
    )


def test_elevation_is_inline_vector_without_external_reference() -> None:
    svg = _svg(_cfg())
    assert svg.tag.endswith("svg")
    for node in svg.iter():
        assert not node.tag.endswith(("img", "image"))
        assert not any(key.endswith(("href", "src")) for key in node.attrib)
    assert "data:image" not in ET.tostring(svg, encoding="unicode")


def test_no_elevation_without_geometry() -> None:
    cfg = _cfg()
    del cfg["inputs"]["structure"]["report_geometry"]
    spec = anode_design_report(_run(cfg))
    design = next(section for section in spec.sections if section.key == "design-basis")
    assert any(
        NO_GEOMETRY in block.markdown
        for block in design.blocks
        if isinstance(block, TextBlock)
    )
    assert not any(
        block.svg for block in design.blocks if isinstance(block, FigureBlock)
    )


def test_geometry_block_does_not_change_results() -> None:
    """Regression pin: display geometry does not alter any routed result value."""
    cfg = _cfg()
    without = copy.deepcopy(cfg)
    del without["inputs"]["structure"]["report_geometry"]
    assert _run(cfg)["results"] == _run(without)["results"]


def test_elevation_appendix_compares_areas_without_changing_design_areas() -> None:
    spec = anode_design_report(_run(_cfg()))
    appendix = "\n".join(
        block.markdown
        for section in spec.appendices
        for block in section.blocks
        if isinstance(block, TextBlock)
    )
    assert "illustrative geometry of the regression case" in appendix
    assert "cylinder lateral surface" in appendix
    for name, area in (
        ("submerged", 5000.0),
        ("splash", 300.0),
        ("atmospheric", 200.0),
    ):
        assert name in appendix
        assert f"{area:.3f}" in appendix
    assert "relative to the stated area" in appendix


@pytest.mark.parametrize(
    ("path", "value"),
    [
        (("datum",), ""),
        (("waterline_m",), float("nan")),
        (("mudline_m",), 1.0),
        (("zones", 1, "lower_m"), -73.0),
        (("zones", 0, "upper_m"), -75.0),
        (("zones", 1, "name"), "buried"),
        (("legs", 0, "diameter_m"), 0.0),
        (("legs", 0, "start", 0), float("inf")),
        (("legs", 0, "start", 0), 1e308),
        (("legs", 0, "start", 0), True),
        (("legs", 0, "start"), [-16.0, -74.0]),
        (("legs", 1, "name"), "leg-1"),
        (("brace_levels", 0, "elevation_m"), -69.0),
        (("legs",), []),
        (("unknown_field",), 1.0),
    ],
)
def test_elevation_rejects_invalid_geometry(
    path: tuple[str | int, ...], value: Any
) -> None:
    from digitalmodel.cathodic_protection.report_elevation import validate_geometry

    geometry = _cfg()["inputs"]["structure"]["report_geometry"]
    node = geometry
    for key in path[:-1]:
        node = node[key]
    node[path[-1]] = value
    with pytest.raises(ValueError):
        validate_geometry(geometry)


def test_geometry_zone_areas_clip_members_and_count_boundaries_once() -> None:
    from digitalmodel.cathodic_protection.report_elevation import geometry_zone_areas

    geometry = {
        "datum": "still-water level",
        "waterline_m": 0.0,
        "mudline_m": -10.0,
        "zones": [
            {"name": "submerged", "lower_m": -10.0, "upper_m": 0.0},
            {"name": "atmospheric", "lower_m": 0.0, "upper_m": 10.0},
        ],
        "legs": [{"name": "leg", "start": [0, 10], "end": [0, -10], "diameter_m": 2}],
        "brace_levels": [
            {
                "elevation_m": z,
                "members": [
                    {
                        "name": f"horizontal-{z}",
                        "start": [-2, z],
                        "end": [2, z],
                        "diameter_m": 1,
                    }
                ],
            }
            for z in (0, 10)
        ],
        "diagonals": [],
    }
    areas = geometry_zone_areas(geometry)
    assert areas["submerged"] == pytest.approx(20 * math.pi)
    assert areas["atmospheric"] == pytest.approx(28 * math.pi)
    assert sum(areas.values()) == pytest.approx(48 * math.pi)


def test_elevation_escapes_untrusted_svg_labels() -> None:
    from digitalmodel.cathodic_protection.report_elevation import design_blocks

    cfg = _cfg()
    geometry = cfg["inputs"]["structure"]["report_geometry"]
    name = 'leg"/><script>alert(1)</script>'
    geometry["datum"] = '<script>alert("datum")</script>'
    geometry["legs"][0]["name"] = name
    figure = design_blocks(cfg["inputs"])[0]
    assert isinstance(figure, FigureBlock) and figure.svg is not None
    svg = ET.fromstring(figure.svg)
    assert not any(node.tag.endswith("script") for node in svg.iter())
    assert name in {node.attrib.get("data-member") for node in svg.iter()}
    assert any(geometry["datum"] in (node.text or "") for node in svg.iter())


def test_elevation_draws_optional_risers_from_data() -> None:
    cfg = _cfg()
    geometry = cfg["inputs"]["structure"]["report_geometry"]
    geometry["risers"] = [
        {"name": "riser-1", "start": [20, 7], "end": [20, -70], "diameter_m": 0.3}
    ]
    svg = _svg(cfg)
    risers = [node for node in svg.iter() if node.attrib.get("data-kind") == "riser"]
    assert len(risers) == 1
    assert risers[0].attrib["data-member"] == "riser-1"
    assert float(risers[0].attrib["stroke-width"]) == 0.3 * float(
        svg.attrib["data-scale"]
    )
