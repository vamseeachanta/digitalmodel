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
    assert "coordinates and member sizes are illustrative" in appendix
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
        "basis": "design_data",
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


def test_sourced_members_use_true_length_and_count() -> None:
    """Area includes all frames while the drawing remains a projection."""
    from digitalmodel.cathodic_protection.report_elevation import geometry_zone_areas

    raw = _cfg()["inputs"]["structure"]["report_geometry"]
    raw["basis"] = "openly_licensed_reference"
    raw["citation"] = "Reference paper, member table"
    raw["attribution"] = "Reference authors"
    raw["licence"] = "CC BY 4.0"
    raw["legs"] = [{"name": "all legs", "start": [0, 10], "end": [0, -70],
                    "diameter_m": 1, "true_length_m": 100, "count": 3}]
    raw["brace_levels"] = []
    raw["diagonals"] = []
    raw["conductors"] = []
    raw["zones"] = [{"name": "deep steel", "calculation_zone": "deep",
                     "lower_m": -70, "upper_m": -30},
                    {"name": "upper steel", "calculation_zone": "upper",
                     "lower_m": -30, "upper_m": 10}]
    areas = geometry_zone_areas(raw)
    assert areas == pytest.approx({"deep": 150 * math.pi, "upper": 150 * math.pi})
    cfg = _cfg()
    cfg["inputs"]["structure"]["report_geometry"] = raw
    spec = anode_design_report(_run(cfg))
    figure = next(f for f in spec.figure_blocks() if f.svg)
    visible = " ".join(ET.fromstring(figure.svg).itertext())
    assert "Reference authors" in visible and "CC BY 4.0" in visible
    assert "OPENLY LICENSED REFERENCE" in visible
    assert "Reference paper, member table" in visible
    assert "Reference authors" in figure.caption and "CC BY 4.0" in figure.caption


def test_missing_geometry_basis_is_rejected() -> None:
    from digitalmodel.cathodic_protection.report_elevation import validate_geometry

    raw = _cfg()["inputs"]["structure"]["report_geometry"]
    del raw["basis"]
    with pytest.raises(ValueError, match="missing fields"):
        validate_geometry(raw)


def test_bad_display_data_still_produces_report() -> None:
    cfg = _cfg()
    cfg["inputs"]["structure"]["report_geometry"]["waterline_m"] = "missing"
    spec = anode_design_report(_run(cfg))
    design = next(s for s in spec.sections if s.key == "design-basis")
    assert not any(isinstance(b, FigureBlock) and b.svg for b in design.blocks)
    assert any("Elevation not evaluated:" in b.markdown for b in design.blocks
               if isinstance(b, TextBlock))


def test_unassigned_member_area_is_reported() -> None:
    from digitalmodel.cathodic_protection.report_elevation import geometry_caveats

    cfg = _cfg()
    raw = cfg["inputs"]["structure"]["report_geometry"]
    raw["risers"] = [{"name": "outside", "start": [0, -80], "end": [0, -90],
                      "diameter_m": 1}]
    text = " ".join(geometry_caveats(cfg["inputs"], {}))
    assert "Unassigned member area: 31.416 m²" in text


def test_zone_rectangles_and_visible_labels_follow_scale() -> None:
    cfg = _cfg()
    raw = cfg["inputs"]["structure"]["report_geometry"]
    svg = _svg(cfg)
    scale = float(svg.attrib["data-scale"])
    waterline = next(n for n in svg.iter() if n.attrib.get("data-level") == "waterline")
    line = next(n for n in waterline if n.tag.endswith("line"))
    zero_y = float(line.attrib["y1"])
    for zone in raw["zones"]:
        group = next(n for n in svg.iter() if n.attrib.get("data-zone") == zone["name"])
        rect = next(n for n in group if n.tag.endswith("rect"))
        assert float(rect.attrib["y"]) == pytest.approx(zero_y - zone["upper_m"] * scale)
        assert float(rect.attrib["height"]) == pytest.approx((zone["upper_m"] - zone["lower_m"]) * scale)
        assert f"{zone['name']}: {zone['lower_m']:g} to {zone['upper_m']:g} m" in "".join(group.itertext())


@pytest.mark.parametrize("count", [True, 0, 1.5, 10**400])
def test_invalid_member_count_is_display_limitation(count: Any) -> None:
    cfg = _cfg()
    cfg["inputs"]["structure"]["report_geometry"]["legs"][0]["count"] = count
    spec = anode_design_report(_run(cfg))
    assert not any(f.svg for f in spec.figure_blocks())
    assert "Elevation not evaluated:" in " ".join(
        b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock)
    )


def test_physical_length_can_project_to_a_point() -> None:
    from digitalmodel.cathodic_protection.report_elevation import geometry_zone_areas

    raw = _cfg()["inputs"]["structure"]["report_geometry"]
    raw["risers"] = [{"name": "perpendicular", "start": [0, -20], "end": [0, -20],
                      "true_length_m": 4, "diameter_m": 1, "count": 3}]
    without = copy.deepcopy(raw)
    without["risers"] = []
    assert geometry_zone_areas(raw)["submerged"] - geometry_zone_areas(without)["submerged"] == pytest.approx(12 * math.pi)


def test_jacket_additions_do_not_apply_to_other_structures() -> None:
    cfg = _cfg()
    cfg["inputs"]["design_data"]["structure_type"] = "monopile"
    cfg["inputs"]["structure"]["report_geometry"] = {"invalid": True}
    spec = anode_design_report(_run(cfg))
    assert not spec.appendices
    assert not any(f.svg for f in spec.figure_blocks())


def test_depth_diagram_has_scaled_levels_potentials_and_criterion() -> None:
    """Synthetic display values pin positions independently of jacket geometry."""
    cfg = _cfg()
    cfg["inputs"]["structure"]["report_geometry"] = {
        "type": "depth_true", "basis": "published_source",
        "datum": "still-water level", "waterline_m": 0, "mudline_m": -80,
        "citation": "Published paper, survey section", "attribution": "Survey authors",
        "criterion_V": -0.8, "criterion_source": "Protection potential criterion",
        "zones": [{"name": "deep", "calculation_zone": "submerged", "lower_m": -80, "upper_m": 0}],
        "monitoring": [{"elevation_m": -20, "elapsed_days": 52, "potential_V": -0.7,
                        "approximate": True},
                       {"elevation_m": -80, "elapsed_days": 518, "potential_V": -0.9,
                        "approximate": False}],
    }
    spec = anode_design_report(_run(cfg))
    figure = next(f for f in spec.figure_blocks() if f.svg)
    svg = ET.fromstring(figure.svg)
    assert svg.attrib["data-type"] == "depth_true"
    assert not any("data-member" in n.attrib for n in svg.iter())
    scale = float(svg.attrib["data-scale"])
    criterion = next(n for n in svg.iter() if n.attrib.get("data-criterion") == "-0.8")
    assert float(criterion.attrib["x1"]) == pytest.approx(350)
    points = [n for n in svg.iter() if "data-potential" in n.attrib]
    assert len(points) == 2
    assert [float(n.attrib["cy"]) for n in points] == pytest.approx([50 + 20 * scale, 50 + 80 * scale])
    assert [float(n.attrib["cx"]) for n in points] == pytest.approx([400, 300])
    assert "more negative than -0.800 V" in figure.caption
    assert "Survey authors" in figure.caption


def test_depth_diagram_allows_separate_categories_at_the_same_depth() -> None:
    from digitalmodel.cathodic_protection.report_elevation import design_blocks

    raw = {"type": "depth_true", "basis": "published_source", "datum": "still-water level",
           "waterline_m": 0, "mudline_m": -80, "citation": "Published survey",
           "attribution": "Survey authors", "criterion_V": -0.8,
           "criterion_source": "Protection criterion",
           "zones": [{"name": "bare", "calculation_zone": "bare", "lower_m": -80, "upper_m": 0},
                     {"name": "coated", "calculation_zone": "coated", "lower_m": -80, "upper_m": 0}],
           "monitoring": [{"elevation_m": -20, "elapsed_days": 52, "potential_V": -0.7, "approximate": True}]}
    blocks = design_blocks({"structure": {"report_geometry": raw}})
    assert isinstance(blocks[0], FigureBlock)
    svg = ET.fromstring(blocks[0].svg)
    bands = [n for n in svg.iter() if "data-zone" in n.attrib]
    assert len(bands) == 2
    rects = [next(n for n in g if n.tag.endswith("rect")) for g in bands]
    assert rects[0].attrib["x"] != rects[1].attrib["x"]
    assert [float(n.attrib["height"]) for n in rects] == [500, 500]


def test_reference_member_form_preserves_unestablished_geometry() -> None:
    """Published sections do not substitute for a complete member schedule."""
    form = yaml.safe_load((INPUTS / "reference_jacket_60m_members.yml").read_text())
    assert form["source"]["licence"] == "CC BY 4.0"
    assert form["dimensions"]["water_depth_m"]["value"] == 60
    assert form["dimensions"]["top_width_m"]["value"] == 15
    assert form["dimensions"]["mudline_width_m"]["value"] == 32
    assert form["dimensions"]["height_above_mudline_m"]["value"] == 74.1
    assert form["drawing_node_heights_above_mudline_m"]["values"] == [
        1, 17.8, 32.8, 45.8, 74.1
    ]
    missing = form["unestablished_geometry"]
    assert missing["complete_node_coordinates_3d_m"] is None
    assert missing["width_definition_in_leg_plan"] is None
    assert missing["leg_section_transition_lengths_m"] is None
    assert missing["complete_member_inventory"] is None
    sections = form["member_sections"]
    assert [(m["diameter_m"], m["wall_thickness_mm"]) for m in sections] == [
        (1.1, 70), (1.0, 20), (1.2, 52), (1.4, 45),
        (1.5, 48), (2.0, 50), (3.0, 50),
    ]
    assert all(m["true_length_m"] is None and m["count"] is None for m in sections)
    assert "report_geometry" not in form
    assert "inputs" not in form


def _synthetic_three_face_member_schedule() -> dict[str, Any]:
    """Supply synthetic grouped members of an equilateral three-face frame."""
    leg_length = math.sqrt(10**2 + (2 / math.sqrt(3)) ** 2)
    diagonal_length = math.sqrt(10**2 + 11**2 + (1 / math.sqrt(3)) ** 2)
    return {
        "basis": "design_data", "datum": "synthetic still-water level",
        "waterline_m": 0, "mudline_m": -10,
        "zones": [
            {"name": "lower synthetic steel", "calculation_zone": "lower",
             "lower_m": -10, "upper_m": -5},
            {"name": "upper synthetic steel", "calculation_zone": "upper",
             "lower_m": -5, "upper_m": 0},
        ],
        "legs": [{"name": "synthetic three legs", "start": [-5, 0],
                  "end": [-6, -10], "diameter_m": 1,
                  "true_length_m": leg_length, "count": 3}],
        "diagonals": [{"name": "synthetic six face diagonals", "start": [-5, 0],
                       "end": [6, -10], "diameter_m": 0.5,
                       "true_length_m": diagonal_length, "count": 6}],
        "brace_levels": [{"elevation_m": -5, "members": [{
            "name": "synthetic three midlevel sides", "start": [-5.5, -5],
            "end": [5.5, -5], "diameter_m": 0.4,
            "true_length_m": 11, "count": 3,
        }]}],
        "risers": [{"name": "synthetic unassigned members", "start": [0, -12],
                    "end": [0, -20], "diameter_m": 0.2,
                    "true_length_m": 8, "count": 3}],
    }


def test_synthetic_all_face_areas_match_independent_three_dimensional_sum() -> None:
    """Synthetic geometry verifies true lengths, multiplicity and zone clipping."""
    from digitalmodel.cathodic_protection.report_elevation import (
        geometry_caveats,
        geometry_zone_areas,
    )

    upper = [(5, -5 / math.sqrt(3), 0), (-5, -5 / math.sqrt(3), 0),
             (0, 10 / math.sqrt(3), 0)]
    lower = [(6, -6 / math.sqrt(3), -10), (-6, -6 / math.sqrt(3), -10),
             (0, 12 / math.sqrt(3), -10)]
    leg_area = sum(math.pi * math.dist(a, b) for a, b in zip(upper, lower))
    diagonal_area = sum(
        math.pi * 0.5 * math.dist(upper[i], lower[j])
        for i in range(3) for j in range(3) if i != j
    )
    middle = [tuple((a + b) / 2 for a, b in zip(p, q))
              for p, q in zip(upper, lower)]
    horizontal_area = sum(
        math.pi * 0.4 * math.dist(middle[i], middle[(i + 1) % 3])
        for i in range(3)
    )
    raw = _synthetic_three_face_member_schedule()
    areas = geometry_zone_areas(raw)
    assert areas["lower"] == pytest.approx((leg_area + diagonal_area) / 2)
    assert areas["upper"] == pytest.approx(
        (leg_area + diagonal_area) / 2 + horizontal_area
    )
    caveats = " ".join(geometry_caveats({"structure": {"report_geometry": raw}}, {}))
    unassigned = math.pi * 0.2 * 8 * 3
    assert f"Unassigned member area: {unassigned:.3f} m²" in caveats


def test_illustrative_basis_is_visible_on_figure_and_caption() -> None:
    """Illustrative members cannot be mistaken for surveyed geometry."""
    spec = anode_design_report(_run(_cfg()))
    figure = next(f for f in spec.figure_blocks() if f.figure_id == "fig-cp-jacket-elevation")
    assert "ILLUSTRATIVE" in figure.svg
    assert "ILLUSTRATIVE" in figure.caption
    assert "regression case" not in figure.caption


def test_non_jacket_display_does_not_change_title() -> None:
    """Display metadata cannot classify another structure as a jacket."""
    cfg = _cfg()
    cfg['inputs']['design_data']['structure_type'] = 'monopile'
    assert 'jacket at' not in anode_design_report(_run(cfg)).document.title


def test_invalid_depth_does_not_enter_title() -> None:
    """Nonfinite display depth remains missing evidence."""
    cfg = _cfg()
    cfg['inputs']['structure']['report_geometry']['mudline_m'] = float('nan')
    assert 'incomplete display data' in anode_design_report(_run(cfg)).document.title


def test_depth_diagram_groups_surveys_without_overprinted_labels() -> None:
    """One annotation per depth lists both published survey times."""
    cfg = yaml.safe_load((INPUTS / 'published_jacket_84m.yml').read_text())
    figure = next(f for f in anode_design_report(_run(cfg)).figure_blocks() if f.svg)
    labels = [n for n in ET.fromstring(figure.svg).iter() if n.tag.endswith('text') and 'days 52, 518' in (n.text or '')]
    assert len(labels) == 4
    assert len({(n.attrib['x'], n.attrib['y']) for n in labels}) == 4


def test_sourced_dimension_table_and_scale_bar_fit_small_structure() -> None:
    """Synthetic short members retain dimensions and a fitted physical scale."""
    from digitalmodel.cathodic_protection.report_elevation import design_blocks
    from digitalmodel.reporting import TableBlock

    raw = {'basis': 'openly_licensed_reference', 'citation': 'Synthetic geometry',
           'attribution': 'Synthetic dimensions', 'licence': 'CC BY 4.0',
           'datum': 'waterline', 'waterline_m': 0, 'mudline_m': -.1,
           'zones': [{'name': 'submerged', 'lower_m': -.1, 'upper_m': 0}],
           'legs': [{'name': 'leg', 'start': [0, 0], 'end': [0, -.1], 'diameter_m': .1}],
           'brace_levels': [], 'diagonals': []}
    blocks = design_blocks({'structure': {'report_geometry': raw}})
    figure = next(b for b in blocks if isinstance(b, FigureBlock))
    svg = ET.fromstring(figure.svg)
    bar = next(n for n in svg.iter() if 'data-scale-bar-m' in n.attrib)
    assert float(bar.attrib['data-scale-bar-m']) * float(svg.attrib['data-scale']) <= 200
    table = next(b for b in blocks if isinstance(b, TableBlock))
    assert table.columns[-4:] == ['Start x', 'Start elevation', 'End x', 'End elevation']
    assert table.rows[0][-4:] == [0, 0, 0, -.1]
