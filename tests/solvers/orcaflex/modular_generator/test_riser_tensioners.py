"""Typed top tensioners for top-tensioned and drilling risers (#3976).

The typed riser had no tensioner, so a top-tensioned riser existed only as a
generic pass-through. ``riser.tensioners`` now emits linear spring/damper
links from the vessel to the free riser top, with unstretched lengths that
give the target vertical tension in the generated geometry. Native data names
are compared with the public examples "H03 Floating and stowed lines" (linear
spring/damper) and "B01 Drilling riser". No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.riser_links_builder import (
    RiserLinksBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/riser_top_tensioned/spec.yml"
NATIVE_LINKS = REPO / (
    "docs/domains/orcaflex/examples/modular/H03/H03 Floating and stowed lines/includes/11_links.yml"
)
GOLDEN = Path(__file__).parent / "goldens/typed/riser_top_tensioned"
CONN = "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _links(data: dict) -> list[dict]:
    spec = ProjectInputSpec(**data)
    return RiserLinksBuilder(spec, BuilderContext()).build()["Links"]


def test_tensioner_links_geometry_and_unstretched_length(example):
    links = _links(example)
    assert [link["Name"] for link in links] == [f"Tensioner_{i}" for i in range(1, 5)]
    for i, link in enumerate(links):
        vessel_end, riser_end = link[CONN]
        angle = math.radians(45 + 90 * i)
        assert vessel_end[0] == "TLP"
        assert vessel_end[1] == pytest.approx(3 * math.cos(angle), abs=1e-6)
        assert vessel_end[2] == pytest.approx(3 * math.sin(angle), abs=1e-6)
        assert vessel_end[3] == 20
        assert riser_end == ["TTR", 0, 0, 0, "End A"]
        # Hand value in the example header: L0 = 8.544 - 267.0 / 50 = 3.204 m.
        assert link["UnstretchedLength"] == pytest.approx(3.204, abs=0.001)
        assert link["Stiffness"] == 50
        assert link["LinearSpring"] == "Yes" and link["LinearDamper"] == "No"


def test_target_vertical_tension_sums_to_total(example):
    spec = ProjectInputSpec(**example)
    units = RiserLinksBuilder.tensioner_geometry(spec.riser.tensioners[0], spec.riser)
    vertical = sum(u["tension"] * (u["z"] - 12) / u["length"] for u in units)
    assert vertical == pytest.approx(1000.0)


def test_vessel_heading_rotates_vessel_points(example):
    data = copy.deepcopy(example)
    data["riser"]["vessel"]["orientation"] = [0, 0, 90]
    data["riser"]["lines"][0]["end_a"]["position"] = [10, 0, 12]
    data["riser"]["tensioners"][0]["start_angle"] = 0
    first = _links(data)[0][CONN][0]
    # Riser top at global (10, 0) is (0, -10) in axes of a vessel heading 90 deg.
    assert first[1] == pytest.approx(3.0, abs=1e-6)
    assert first[2] == pytest.approx(-10.0, abs=1e-6)


def test_damping_emits_linear_damper(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["damping"] = 5
    link = _links(data)[0]
    assert link["LinearDamper"] == "Yes" and link["DampingConstant"] == 5


def test_spring_damper_link_data_names_match_native(example):
    data = copy.deepcopy(example)
    data["riser"]["links"] = [{
        "name": "Spring1", "link_type": "spring_damper",
        "connections": [
            {"object_name": "TLP", "x": 0, "y": 0, "z": 30},
            {"object_name": "TTR", "x": 0, "y": 0, "z": 0, "z_relative_to": "End A"},
        ],
        "unstretched_length": 5, "stiffness": 100,
    }]
    user_link = _links(data)[0]
    assert user_link["LinearSpring"] == "Yes" and user_link["LinearDamper"] == "No"
    native = next(
        link for link in yaml.safe_load(NATIVE_LINKS.read_text(encoding="utf-8"))["Links"]
        # PyYAML (YAML 1.1) reads the native unquoted Yes as True.
        if link.get("LinkType") == "Spring/damper" and link.get("LinearSpring") in ("Yes", True)
    )
    keys = set(user_link) - {"LinkType"}
    assert keys <= set(native)
    tensioner_keys = set(_links(example)[0]) - {"LinkType"}
    assert tensioner_keys <= set(native)


def test_tensioner_requires_free_end_a(example):
    data = copy.deepcopy(example)
    data["riser"]["lines"][0]["end_a"] = {"type": "vessel", "name": "TLP", "position": [0, 0, 12]}
    with pytest.raises(ValidationError, match="must be 'free'"):
        ProjectInputSpec(**data)


def test_tensioner_unknown_line_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["line"] = "Missing"
    with pytest.raises(ValidationError, match="unknown line"):
        ProjectInputSpec(**data)


def test_vessel_point_below_riser_top_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["vessel_z"] = 10
    with pytest.raises(ValueError, match="must be above"):
        _links(data)


def test_too_soft_tensioner_rejected(example):
    data = copy.deepcopy(example)
    data["riser"]["tensioners"][0]["stiffness"] = 20  # 267 kN / 20 kN/m > 8.544 m
    with pytest.raises(ValueError, match="too low"):
        _links(data)


def test_example_matches_golden(example, tmp_path):
    out = tmp_path / "out"
    ModularModelGenerator.from_spec(ProjectInputSpec(**example)).generate(out)
    includes = out / "includes"
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        got = yaml.safe_load((includes / name).read_text(encoding="utf-8"))
        want = yaml.safe_load((GOLDEN / name).read_text(encoding="utf-8"))
        assert got == want, name


def test_example_header_equilibrium_hand_check(example):
    """Recompute the example header's static estimate from the spec inputs.

    Effective-tension stretch of the riser balanced against the linearised
    tensioner vertical stiffness (axial plus geometric term). Arithmetic of
    the header only; it is not a licensed statics result.
    """
    g, rho = 9.80665, 1.025
    spec = ProjectInputSpec(**example).riser
    line_type, line = spec.line_types[0], spec.lines[0]
    tensioner = spec.tensioners[0]
    od, idia = line_type.outer_diameter, line_type.inner_diameter
    w = (line_type.mass_per_length + line.contents.density * math.pi * idia**2 / 4
         - rho * math.pi * od**2 / 4) * g
    length = line.get_total_length()
    weight = w * length
    assert w == pytest.approx(1.314, abs=0.001)
    assert weight == pytest.approx(672.7, abs=0.1)

    dz = tensioner.vessel_z - line.end_a.position[2]
    link = math.hypot(tensioner.radius, dz)
    unit = tensioner.total_tension / tensioner.count * link / dz
    k_vertical = tensioner.count * (
        tensioner.stiffness * (dz / link) ** 2 + unit / link * (tensioner.radius / link) ** 2
    )
    assert k_vertical == pytest.approx(190.8, abs=0.1)
    k_riser = line_type.axial_stiffness / length
    assert k_riser == pytest.approx(7990, abs=1)
    # Rise d: riser stretch (mean effective tension / k_riser) equals d,
    # with top tension T0 - k_vertical * d.
    t0 = tensioner.total_tension
    rise = (2 * t0 - weight) / (2 * k_riser + 2 * k_vertical)
    top = t0 - k_vertical * rise
    assert rise == pytest.approx(0.081, abs=0.001)
    assert top == pytest.approx(984.5, abs=0.1)
    assert top - weight == pytest.approx(311.9, abs=0.15)
