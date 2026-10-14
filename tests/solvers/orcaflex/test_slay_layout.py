# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free geometry and force-application contracts for generic S-lay."""

import importlib.util
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.vessel_builder import VesselBuilder
from digitalmodel.solvers.orcaflex.modular_generator.builders.winch_builder import WinchBuilder

ROOT = Path(__file__).resolve().parents[3]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"


def scenario():
    module_spec = importlib.util.spec_from_file_location(
        "preparation", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    driver = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(driver)
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
    return driver.case_spec(table["basis"], table["cases"][0])


def test_pipe_ends_are_separated_and_upper_end_can_take_winch_load():
    spec = scenario()
    line = LinesBuilder(spec, BuilderContext()).build()["Lines"][0]
    connections = next(value for key, value in line.items() if key.startswith("Connection, "))
    top, bottom = connections
    assert top[0] == "Free"
    assert bottom[0] == "Anchored"
    assert top[1] > bottom[1]
    assert top[3] > -spec.environment.water.depth
    length = sum(segment.length for segment in spec.pipeline.segments)
    assert math.hypot(top[1] - bottom[1], spec.environment.water.depth + top[3]) < length
    stiffness = next(value for key, value in line.items() if key.startswith("ConnectionxBending"))
    assert stiffness == [[0, 0, 0], [0, 0, 0]]


def test_vessel_supports_attach_pipeline_at_declared_roller_stations():
    spec = scenario()
    vessel = VesselBuilder(spec, BuilderContext()).build()["Vessels"][0]
    assert vessel["SupportedLine"] == [spec.pipeline.name]
    supports = vessel["Supports"]
    assert len(supports) == len(spec.equipment.stinger.rollers) > 1
    assert len({tuple(support["SupportPosition"]) for support in supports}) == len(supports)
    for roller, support in zip(spec.equipment.stinger.rollers, supports):
        assert support["SupportType"] == roller.support_type
        assert support["SupportCoordinateSystem"] == "Stinger"
        theta = roller.arc_length / spec.equipment.stinger.radius
        assert support["SupportPosition"][0] == pytest.approx(
            spec.equipment.stinger.origin_position[0]
            - spec.equipment.stinger.radius * math.sin(theta), abs=0.001)


def test_winch_applies_tension_in_statics_and_every_stage_with_nonzero_lead():
    spec = scenario()
    winch = WinchBuilder(spec, BuilderContext()).build()["Winches"][0]
    assert winch["StageMode"] == ["Specified tension", "Specified tension"]
    assert winch["StageTension"] == [spec.equipment.tensioner.tension_value] * 2
    assert winch["StaticsStep1"] == "Specified tension"
    assert winch["ConnectionPoint"][0] != winch["ConnectionPoint"][1]
