"""Typed mooring with generic host objects (digitalmodel #3972).

A typed ``mooring`` section names the vessel its fairleads connect to, but the
mooring schema has no place to define that vessel.  These tests check that a
``generic`` section may carry the host objects next to ``mooring``, that the
example specification generates and passes post-validation, and that the
emitted includes match the committed golden.  No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
import math
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/mooring_spread_catenary/spec.yml"
GOLDEN = Path(__file__).parent / "goldens/typed/mooring_spread_catenary"


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _generate(data: dict, out: Path) -> Path:
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    return out / "includes"


def test_mooring_with_generic_host_is_accepted(example):
    spec = ProjectInputSpec(**example)
    assert spec.is_mooring() and spec.is_generic()
    assert [v.name for v in spec.generic.vessels] == ["FPSO"]


def test_mooring_without_host_still_fails_post_validation(example, tmp_path):
    data = copy.deepcopy(example)
    del data["generic"]
    with pytest.raises(Exception, match="FPSO"):
        _generate(data, tmp_path / "out")


TEMPLATES = REPO / "docs/domains/orcaflex/library/templates"


def _section(template: str, key: str) -> dict:
    data = yaml.safe_load((TEMPLATES / template / "spec.yml").read_text(encoding="utf-8"))
    return data[key]


@pytest.mark.parametrize(
    ("template", "key"),
    [("pipeline_installation", "pipeline"), ("riser_catenary", "riser")],
)
def test_other_domain_combinations_still_rejected(example, template, key):
    data = copy.deepcopy(example)
    data[key] = _section(template, key)
    with pytest.raises(ValidationError, match="multiple model types"):
        ProjectInputSpec(**data)


def test_mooring_and_riser_without_generic_rejected(example):
    data = copy.deepcopy(example)
    del data["generic"]
    data["riser"] = _section("riser_catenary", "riser")
    with pytest.raises(ValidationError, match="multiple model types"):
        ProjectInputSpec(**data)


def test_example_generates_host_vessel_lines_and_winches(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    objects = yaml.safe_load((includes / "20_generic_objects.yml").read_text(encoding="utf-8"))
    assert [v["Name"] for v in objects["Vessels"]] == ["FPSO"]
    lines = {line["Name"]: line for line in objects["Lines"]}
    assert sorted(lines) == ["Leg1", "Leg2", "Leg3", "Leg4"]
    for line in lines.values():
        assert line["EndAConnection"] == "Fixed"
        assert line["EndBConnection"] == "FPSO"
        assert line["EndAZ"] == -200
    winches = {w["Name"]: w for w in objects["Winches"]}
    assert sorted(winches) == ["Winch_Leg1", "Winch_Leg2", "Winch_Leg3", "Winch_Leg4"]
    assert all(w["Connection"][0] == "FPSO" and w["Tension"] == 500 for w in winches.values())


@pytest.mark.parametrize(
    ("section", "collection"),
    [("buoys_6d", "6DBuoys"), ("buoys_3d", "3DBuoys")],
)
def test_pretensioned_mooring_generates_with_buoy_host(
    example, tmp_path, section, collection
):
    data = copy.deepcopy(example)
    data["generic"] = {section: [{"name": "BuoyHost", "connection": "Free"}]}
    for line in data["mooring"]["lines"]:
        # Object lookup is case-insensitive, as it is for vessel hosts.
        line["fairlead"]["vessel"] = "buoyhost"
    includes = _generate(data, tmp_path / "out")
    objects = yaml.safe_load((includes / "20_generic_objects.yml").read_text(encoding="utf-8"))
    assert [buoy["Name"] for buoy in objects[collection]] == ["BuoyHost"]
    assert len(objects["Lines"]) == len(objects["Winches"]) == 4
    assert all(line["EndBConnection"] == "buoyhost" for line in objects["Lines"])
    assert all(
        winch["Connection"][0] == "buoyhost" and winch["Tension"] == 500
        for winch in objects["Winches"]
    )


def test_example_anchor_radius_matches_hand_catenary(example):
    """Anchor placement in the example follows the inextensible catenary.

    Independently derive the comparator from the illustrative mass and
    displacement assumptions, rather than copying a router/golden result.
    This establishes arithmetic consistency, not chain-property qualification.
    """
    for line in example["mooring"]["lines"]:
        segment = line["segments"][0]
        diameter = segment["diameter"]
        mass = 21.9 * diameter**2  # Orcina Chain table: studlink, te/m.
        displaced = example["environment"]["water"]["density"] * math.pi * diameter**2
        weight = (mass - displaced) * 9.80665  # kN/m; assumed OD = 2d.
        fx, fy, fz = line["fairlead"]["position"]
        ax, ay, az = line["anchor"]["position"]
        height = fz - az
        assert height == pytest.approx(190.0)
        horizontal_tension = line["pretension"] - weight * height
        a = horizontal_tension / weight
        suspended = math.sqrt(height**2 + 2 * height * a)
        grounded = segment["length"] - suspended
        span = grounded + a * math.asinh(suspended / a)
        assert horizontal_tension == pytest.approx(299.0, abs=0.1)
        assert suspended == pytest.approx(378.8, abs=0.1)
        assert grounded == pytest.approx(521.2, abs=0.1)
        assert span == pytest.approx(832.9, abs=0.1)
        assert math.hypot(ax - fx, ay - fy) == pytest.approx(span, abs=0.5)


def test_example_property_basis_is_independent_of_serialization(example, tmp_path):
    """Compare generated quantities with the cited public preliminary relations.

    Source: Orcina webhelp, Chain and Chain: Mechanical properties, accessed
    2026-11-12; the example header gives the source URLs and discrepancies.
    Breaking load is a source comparator, not a factored design allowable.
    """
    includes = _generate(example, tmp_path / "out")
    objects = yaml.safe_load((includes / "20_generic_objects.yml").read_text(encoding="utf-8"))
    segment = example["mooring"]["lines"][0]["segments"][0]
    assert segment["grade"] == "R4"
    d = segment["diameter"]  # Source equations use metres, tonnes and kN.
    (lt,) = objects["LineTypes"]
    assert lt["MassPerUnitLength"] == pytest.approx(21.9 * d**2)  # Studlink.
    assert lt["EA"] == pytest.approx(0.854e8 * d**2)  # Studless.
    assert lt["AllowableTension"] == pytest.approx(2.74e4 * d**2 * (44 - 80 * d))
    # Pin the documented disagreements; these are generation assumptions.
    assert lt["OD"] == pytest.approx(2 * d)
    assert lt["OD"] != pytest.approx(1.8 * d)  # Public studless equivalent OD.
    assert lt["MassPerUnitLength"] != pytest.approx(19.9 * d**2)  # Studless.
    assert lt["Cd"] == [2.4, None, 0.008]
    assert lt.get("NormalDragDiameter", lt["OD"]) != pytest.approx(d)
    assert lt.get("AxialDragDiameter", lt["OD"]) != pytest.approx(d / math.pi)


def test_example_matches_golden(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        got = yaml.safe_load((includes / name).read_text(encoding="utf-8"))
        want = yaml.safe_load((GOLDEN / name).read_text(encoding="utf-8"))
        assert got == want, name
