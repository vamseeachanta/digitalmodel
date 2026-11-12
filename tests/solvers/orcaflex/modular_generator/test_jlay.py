"""J-lay tower installation (#3978).

The generator supported S-lay and floating installation only. ``equipment.jlay``
pins the pipe to the vessel at the tower top and seeds End B on the seabed at
the inextensible-catenary span for the hang-off angle. Comparators are the hand
values recorded in the example header. No OrcaFlex licence is used.
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
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/pipeline_jlay/spec.yml"
GOLDEN = Path(__file__).parent / "goldens/typed/pipeline_jlay"
CONN = (
    "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
    "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo"
)


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _builder(data: dict) -> LinesBuilder:
    return LinesBuilder(ProjectInputSpec(**data), BuilderContext())


def _pipeline(data: dict) -> dict:
    return next(line for line in _builder(data).build()["Lines"] if line["Name"] == "Pipeline")


def test_seed_matches_hand_catenary(example):
    seed = _builder(example).jlay_seed()
    assert seed["weight"] == pytest.approx(0.6805, abs=1e-4)
    assert seed["a"] == pytest.approx(166.52, abs=0.01)
    assert seed["suspended"] == pytest.approx(1184.88, abs=0.01)
    assert seed["suspended_span"] == pytest.approx(443.00, abs=0.01)
    assert seed["span"] == pytest.approx(2258.13, abs=0.01)
    assert seed["top_tension"] == pytest.approx(814.3, abs=0.1)
    assert seed["horizontal_tension"] == pytest.approx(113.3, abs=0.1)


def test_end_a_pinned_at_tower_top_and_end_b_aft(example):
    line = _pipeline(example)
    end_a, end_b = line[CONN]
    assert end_a[:4] == ["LayVessel", -20, 0, 30]
    assert end_a[4:6] == [180, 172]
    assert end_b[0] == "Anchored"
    assert end_b[1] == pytest.approx(-20 - 2258.13, abs=0.01)
    assert end_b[3] == pytest.approx(0.3299 / 2)
    assert line["ConnectionxBendingStiffness, ConnectionyBendingStiffness"] == [[0, None], [0, None]]


def test_vessel_heading_rotates_lay_direction(example):
    data = copy.deepcopy(example)
    data["equipment"]["vessel"]["orientation"] = [0, 0, 90]
    end_a, end_b = _pipeline(data)[CONN]
    # Heading 90 deg: vessel -x points to global -y; the hang-off at vessel
    # (-20, 0) is at global (0, -20).
    assert end_a[1:3] == [-20, 0]  # vessel axes
    assert end_b[1] == pytest.approx(0.0, abs=1e-6)
    assert end_b[2] == pytest.approx(-20 - 2258.13, abs=0.01)


@pytest.mark.parametrize(
    ("change", "message"),
    [
        (lambda d: d["equipment"].pop("vessel"), "requires a vessel"),
        (lambda d: d["equipment"].update(stinger={"radius": 100, "sections": [{"length": 10, "bend_radius": 100}]}),
         "cannot both be defined"),
        (lambda d: d["equipment"].update(tensioner={"capacity_kn": 2000, "tension_value": 800}),
         "tensioner cannot also be defined"),
    ],
)
def test_invalid_jlay_equipment_rejected(example, change, message):
    data = copy.deepcopy(example)
    change(data)
    with pytest.raises(ValidationError, match=message):
        ProjectInputSpec(**data)


def test_pipe_too_short_for_grounded_remainder_rejected(example):
    data = copy.deepcopy(example)
    data["pipeline"]["segments"][0]["length"] = 1000
    with pytest.raises(ValueError, match="grounded remainder"):
        _builder(data).jlay_seed()


def test_tower_angle_bounds(example):
    for angle in (0, 45):
        data = copy.deepcopy(example)
        data["equipment"]["jlay"]["tower_angle"] = angle
        with pytest.raises(ValidationError):
            ProjectInputSpec(**data)


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


def test_pipeline_contents_stated_empty_as_seeded(example):
    """The seed takes the pipe empty, so the emitted line states empty contents.

    Without these data the loaded model would take the solver's default
    contents, which the offline seed cannot confirm.
    """
    line = _pipeline(example)
    assert line["ContentsMethod"] == "Uniform"
    assert line["ContentsDensity"] == 0
