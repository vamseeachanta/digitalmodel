# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free geometry and force-application contracts for generic S-lay."""

import importlib.util
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
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
    assert stiffness == [[], [0, None]]


def test_vessel_supports_attach_pipeline_at_declared_roller_stations():
    spec = scenario()
    vessel = VesselBuilder(spec, BuilderContext()).build()["Vessels"][0]
    assert vessel["SupportedLine"] == [spec.pipeline.name]
    supports = vessel["Supports"]
    assert len(supports) == len(spec.equipment.stinger.rollers) + 12
    assert len({tuple(support["SupportPosition"]) for support in supports}) == len(supports)
    for roller, support in zip(spec.equipment.stinger.rollers, supports[12:]):
        assert support["SupportType"] == roller.support_type
        assert support["SupportCoordinateSystem"] == "Stinger"
        theta = roller.arc_length / spec.equipment.stinger.radius
        assert support["SupportPosition"][0] == pytest.approx(
            spec.equipment.stinger.origin_position[0]
            - spec.equipment.stinger.radius * math.sin(theta), abs=0.001)


@pytest.mark.parametrize("durations", [[8, 16], [8], [8, 16, 16, 16, 16]])
def test_winch_applies_tension_in_statics_and_every_stage_with_nonzero_lead(tmp_path, durations):
    spec = scenario()
    spec.simulation.stages = durations
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    general = yaml.safe_load((tmp_path / "includes/01_general.yml").read_text())["General"]
    winch = yaml.safe_load((tmp_path / "includes/11_winches.yml").read_text())["Winches"][0]
    stages = winch["StageMode, StageValue"]
    assert stages == [["Specified tension", spec.equipment.tensioner.tension_value]] * (
        len(general["StageDuration"]) + 1)
    assert winch["WinchControlType"] == "By stage"
    connections = winch["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"]
    assert connections[0][1:4] != connections[1][1:4]


def test_winch_keys_match_native_export():
    """Reject private winch keys absent from the native reference."""
    spec = scenario()
    winch = WinchBuilder(spec, BuilderContext()).build()["Winches"][0]
    reference = ROOT / "docs/domains/orcaflex/examples/modular/D03/D03 Lay on tower/includes/12_winches.yml"
    native = yaml.safe_load(reference.read_text(encoding="utf-8"))["Winches"][0]
    assert set(winch) <= set(native)
    assert {"WinchControlType", "StageMode, StageValue",
            "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"} <= set(winch)


@pytest.mark.parametrize("case_index", range(6))
def test_support_types_are_defined_before_vessel_references(tmp_path, case_index):
    """Resolve every roller support type before its referencing vessel."""
    spec = scenario()
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text(encoding="utf-8"))
    case = table["cases"][case_index]
    spec.environment.water.depth = case["depth_m"]
    spec.equipment.tensioner.tension_value = case["top_tension_kn"]
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    includes = yaml.safe_load((tmp_path / "master.yml").read_text(encoding="utf-8"))
    defined = set()
    references = 0
    for include in includes:
        data = yaml.safe_load((tmp_path / include["includefile"]).read_text(encoding="utf-8"))
        defined.update(item["Name"] for item in data.get("SupportTypes", []))
        for vessel in data.get("Vessels", []):
            for support in vessel.get("Supports", []):
                assert support["SupportType"] in defined
                references += 1
    assert references == len(spec.equipment.stinger.rollers) + 12


@pytest.mark.parametrize("case_id", [f"d{d}-t{t}" for d in range(2) for t in range(3)])
def test_weight_only_screen_is_a_bound_not_the_stiff_design(case_id):
    """A span correction alone and a tangent correction alone are insufficient."""
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import screen_case

    screen = screen_case(CAMPAIGN, case_id)
    current = screen["current"]
    assert current["excess_m"] < 0
    assert current["departure_deg"] - current["tip_angle_deg"] > screen["angle_tolerance_deg"]
    length_only = screen["length_only"]
    assert abs(length_only["excess_m"]) <= screen["length_tolerance_m"]
    assert length_only["angle_mismatch_deg"] > screen["angle_tolerance_deg"]
    angle_only = screen["angle_only"]
    assert abs(angle_only["departure_deg"] - angle_only["tip_angle_deg"]) <= screen["angle_tolerance_deg"]
    assert angle_only["excess_m"] > current["excess_m"]
    assert abs(screen["combined"]["excess_m"]) <= screen["length_tolerance_m"]
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())
    declared = next(row for row in table["static_equilibrium_screen"]["all_cases"] if row["id"] == case_id)
    for key in ("excess_m", "departure_deg"):
        assert declared[key] == round(current[key], 1)
    for declared_key, computed_key in (("angle_only_arc_m", "supported_arc_m"),
                                       ("angle_only_tip_deg", "tip_angle_deg"),
                                       ("angle_only_excess_m", "excess_m")):
        assert declared[declared_key] == round(angle_only[computed_key], 1)
    assert declared["combined_arc_m"] == round(screen["combined"]["supported_arc_m"], 1)
    assert declared["combined_span_m"] == round(screen["combined"]["anchor_span_m"], 1)
    assert current["suspended_length_m"] > current["suspended_span_m"] > 0
    assert current["horizontal_tension_kn"] == pytest.approx(
        screen["top_tension_kn"] - screen["weight_kn_m"] * screen["depth_m"])
    assert current["taut_length_m"] + current["excess_m"] == pytest.approx(screen["pipe_length_m"])
