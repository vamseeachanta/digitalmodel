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
    assert stiffness == [[], [0, 0, 0, 0]]


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
    assert references == len(spec.equipment.stinger.rollers) > 1


@pytest.mark.parametrize("case_id", [f"d{d}-t{t}" for d in range(2) for t in range(3)])
def test_hand_closure_screens_leave_the_other_suspect_open(case_id):
    """A span correction alone and a tangent correction alone are insufficient."""
    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import screen_case

    screen = screen_case(CAMPAIGN, case_id)
    current = screen["current"]
    assert current["excess_m"] > screen["lead_m"]
    assert current["departure_deg"] - current["tip_angle_deg"] > screen["angle_tolerance_deg"]
    length_only = screen["length_only"]
    assert abs(length_only["excess_m"]) <= screen["length_tolerance_m"]
    assert length_only["angle_mismatch_deg"] > screen["angle_tolerance_deg"]
    angle_only = screen["angle_only"]
    assert abs(angle_only["departure_deg"] - angle_only["tip_angle_deg"]) <= screen["angle_tolerance_deg"]
    assert angle_only["excess_m"] > screen["lead_m"] + screen["length_tolerance_m"]
    table = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())
    declared = next(row for row in table["static_equilibrium_screen"]["all_cases"] if row["id"] == case_id)
    for key in ("excess_m", "departure_deg"):
        assert declared[key] == round(current[key], 1)
    for declared_key, computed_key in (("angle_only_arc_m", "supported_arc_m"),
                                       ("angle_only_tip_deg", "tip_angle_deg"),
                                       ("angle_only_excess_m", "excess_m")):
        assert declared[declared_key] == round(angle_only[computed_key], 1)
    assert current["suspended_length_m"] > current["suspended_span_m"] > 0
    assert current["horizontal_tension_kn"] == pytest.approx(
        screen["top_tension_kn"] - screen["weight_kn_m"] * screen["depth_m"])
    assert current["taut_length_m"] + current["excess_m"] == pytest.approx(screen["pipe_length_m"])


def test_anchor_span_probe_changes_only_anchor_x_and_binds_base_bytes(tmp_path):
    """The extra variation holds all loads, supports and production inputs fixed."""
    import hashlib
    import json
    from copy import deepcopy

    from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import prepare_anchor_span_probe

    preparation = json.loads((CAMPAIGN / "preparation.json").read_text())
    assert preparation["case_table_sha256"] == hashlib.sha256((CAMPAIGN / "case-table.yml").read_bytes()).hexdigest()
    before = {p: p.read_bytes() for p in CAMPAIGN.rglob("*") if p.is_file()}
    manifest = prepare_anchor_span_probe(CAMPAIGN, tmp_path)
    probe = tmp_path / manifest["probe_file"]
    document = yaml.safe_load(probe.read_text(encoding="utf-8"))
    assert document["BaseFile"] == "../models/d0-t0/master.yml"
    assert set(document) == {"BaseFile", "Lines"}
    original = yaml.safe_load((CAMPAIGN / "models/d0-t0/includes/07_lines.yml").read_text())
    expected = deepcopy(original["Lines"])
    key = next(key for key in expected[0] if key.startswith("Connection, "))
    end_a_x = expected[0][key][0][1]
    expected[0][key][1][1] = end_a_x - manifest["target_span_m"]
    assert document["Lines"] == expected
    assert manifest["target_span_m"] > manifest["initial_span_m"]
    assert manifest["target_span_m"] - manifest["initial_span_m"] == pytest.approx(
        manifest["screen"]["current"]["excess_m"])
    assert manifest["probe_sha256"] == hashlib.sha256(probe.read_bytes()).hexdigest()
    base_files = {p.relative_to(CAMPAIGN).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                  for p in (CAMPAIGN / "models/d0-t0").rglob("*.yml")}
    assert manifest["base_files"] == base_files
    assert manifest["case_table_sha256"] == hashlib.sha256((CAMPAIGN / "case-table.yml").read_bytes()).hexdigest()
    assert manifest["native_status"] == "not_run"
    assert set(manifest["required_readings"]) == {
        "end_a_displacement", "wire_direction_and_length", "active_roller_contacts",
        "departure_slope", "force_residuals", "tip_contact_force_and_curvature",
        "maximum_overbend_strain", "grounded_effective_tension"}
    saved = json.loads((tmp_path / "probes/d0-t0-anchor-span.json").read_text())
    assert saved == manifest
    assert all(p.read_bytes() == content for p, content in before.items())
    assert set(p for p in CAMPAIGN.rglob("*") if p.is_file()) == set(before)
    second = tmp_path / "repeat"
    prepare_anchor_span_probe(CAMPAIGN, second)
    assert {p.relative_to(tmp_path / "probes"): p.read_bytes() for p in (tmp_path / "probes").iterdir()} == {
        p.relative_to(second / "probes"): p.read_bytes() for p in (second / "probes").iterdir()}


def test_probe_preparation_never_imports_native_interface(tmp_path):
    """Text preparation must remain independent of the licensed interface."""
    import subprocess
    import sys

    code = '''
import importlib.abc, sys
from pathlib import Path
class DenyNative(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(("OrcFxAPI", "digitalmodel.solvers.orcaflex.orcaflex_api")):
            raise AssertionError("Native interface access during probe preparation")
sys.meta_path.insert(0, DenyNative())
from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import prepare_anchor_span_probe
prepare_anchor_span_probe(Path(sys.argv[1]), Path(sys.argv[2]))
'''
    result = subprocess.run([sys.executable, "-c", code, str(CAMPAIGN), str(tmp_path)],
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
