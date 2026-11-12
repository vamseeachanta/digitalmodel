"""Riser buoyancy zone emitted as a split section table (digitalmodel #3973).

``RiserLine.buoyancy_zone`` was validated and never emitted, so a lazy-wave
riser specified with a zone was generated without buoyancy.  The comparator
for the example is the section table of the public OrcaFlex example
"A01 Lazy wave riser" committed under docs/domains/orcaflex/examples/modular.
No OrcaFlex licence is used.
"""

from __future__ import annotations

import copy
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.riser_lines_builder import (
    RiserLinesBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

REPO = Path(__file__).resolve().parents[4]
EXAMPLE = REPO / "docs/domains/orcaflex/examples/modular/typed/riser_lazy_wave_zone/spec.yml"
NATIVE = REPO / (
    "docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/includes/07_lines.yml"
)
GOLDEN = Path(__file__).parent / "goldens/typed/riser_lazy_wave_zone"
SECTION_KEY = "LineType, Length, TargetSegmentLength"
BARE, FLOATS = "10in_flexible", "10in_flexible_with_floats"


@pytest.fixture
def example() -> dict:
    return yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))


def _table(data: dict) -> list[list]:
    spec = ProjectInputSpec(**data)
    lines = RiserLinesBuilder(spec, BuilderContext()).build()["Lines"]
    assert len(lines) == 1
    return lines[0][SECTION_KEY]


def _zone(data: dict, start: float, end: float, line_type: str = FLOATS) -> dict:
    data = copy.deepcopy(data)
    data["riser"]["lines"][0]["buoyancy_zone"] = {
        "start_arc_length": start, "end_arc_length": end, "line_type": line_type,
    }
    return data


def test_zone_reproduces_native_a01_section_table(example):
    native = yaml.safe_load(NATIVE.read_text(encoding="utf-8"))["Lines"]
    distributed = next(line for line in native if "Distributed" in line["Name"])
    native_rows = distributed[SECTION_KEY]
    rows = _table(example)
    assert [[r[1], r[2]] for r in rows] == [[n[1], n[2]] for n in native_rows]
    assert [r[0] == FLOATS for r in rows] == ["Floats" in n[0] for n in native_rows]


def test_zone_preserves_total_and_buoyant_length(example):
    rows = _table(_zone(example, 47.5, 151.25))
    assert sum(r[1] for r in rows) == pytest.approx(190.0)
    assert sum(r[1] for r in rows if r[0] == FLOATS) == pytest.approx(103.75)
    # The zone starts inside section 1 (0-50 m) and ends inside section 2
    # (50-170 m); each piece keeps its parent section's mesh size.
    assert rows == [
        [BARE, 47.5, 4], [FLOATS, 2.5, 4], [FLOATS, 101.25, 1], [BARE, 18.75, 1], [BARE, 20, 4],
    ]


def test_zone_on_section_boundaries_does_not_create_empty_pieces(example):
    rows = _table(_zone(example, 50, 170))
    assert rows == [[BARE, 50, 4], [FLOATS, 120, 1], [BARE, 20, 4]]


def test_no_zone_leaves_table_unchanged(example):
    data = copy.deepcopy(example)
    del data["riser"]["lines"][0]["buoyancy_zone"]
    assert _table(data) == [[BARE, 50, 4], [BARE, 120, 1], [BARE, 20, 4]]


def test_zone_beyond_line_length_rejected(example):
    with pytest.raises(ValidationError, match="buoyancy_zone"):
        ProjectInputSpec(**_zone(example, 150, 195))


def test_zone_unknown_line_type_rejected(example):
    with pytest.raises(ValidationError, match="unknown buoyancy_zone line_type"):
        ProjectInputSpec(**_zone(example, 80, 130, line_type="missing"))


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


@pytest.mark.parametrize("start", [80.0, 50.0 + 2e-9])
def test_close_interior_cuts_keep_positive_lengths(example, tmp_path, start):
    """Distinct cuts must survive both section splitting and YAML emission."""
    end = start + 1e-10
    data = _zone(example, start, end)
    rows = _table(data)
    out = tmp_path / "close_cuts"
    ModularModelGenerator.from_spec(ProjectInputSpec(**data)).generate(out)
    emitted = yaml.safe_load((out / "includes/07_riser_lines.yml").read_text())
    assert emitted["Lines"][0][SECTION_KEY] == rows
    assert all(row[1] > 0 for row in rows)
    buoyant_lengths = [row[1] for row in rows if row[0] == FLOATS]
    assert buoyant_lengths == [end - start]
    assert sum(row[1] for row in rows) == pytest.approx(190.0, abs=1e-12)
