"""Typed mooring with generic host objects (digitalmodel #3972).

A typed ``mooring`` section names the vessel its fairleads connect to, but the
mooring schema has no place to define that vessel.  These tests check that a
``generic`` section may carry the host objects next to ``mooring``, that the
example specification generates and passes post-validation, and that the
emitted includes match the committed golden.  No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
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


def test_example_anchor_radius_matches_hand_catenary(example):
    """Anchor placement in the example follows the inextensible catenary.

    Comparator: horizontal fairlead-to-anchor distance from the hand
    calculation recorded in the example header, 832.9 m.
    """
    import math

    for line in example["mooring"]["lines"]:
        fx, fy, fz = line["fairlead"]["position"]
        ax, ay, az = line["anchor"]["position"]
        assert math.hypot(ax - fx, ay - fy) == pytest.approx(832.9, abs=0.5)
        assert fz - az == pytest.approx(190.0)


def test_example_matches_golden(example, tmp_path):
    includes = _generate(example, tmp_path / "out")
    golden_files = sorted(p.name for p in GOLDEN.glob("*.yml"))
    assert golden_files == sorted(p.name for p in includes.glob("*.yml"))
    for name in golden_files:
        got = yaml.safe_load((includes / name).read_text(encoding="utf-8"))
        want = yaml.safe_load((GOLDEN / name).read_text(encoding="utf-8"))
        assert got == want, name
