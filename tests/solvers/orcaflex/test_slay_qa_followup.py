"""Synthetic regressions for S-lay static QA readback and input guards."""

import importlib.util
import json
import math
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import slay_probe
from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.lines_builder import LinesBuilder

ROOT = Path(__file__).resolve().parents[3]
CAMPAIGN = ROOT / "docs/domains/pipeline-installation/slay-campaign"
TABLE = yaml.safe_load((CAMPAIGN / "case-table.yml").read_text())


def preparation_driver():
    spec = importlib.util.spec_from_file_location(
        "prepare_qa", ROOT / "scripts/orcaflex/prepare_slay_campaign.py")
    driver = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(driver)
    return driver


@pytest.mark.parametrize("end_x,forces,accepted", [
    (-31.1, [12, 0, 0], True),
    (-36.1, [0, 12, 0], True),
    (-36.1, [12, 0, 0], False),
    (-36.1, [12, 0.01, 0], True),
])
def test_end_a_contact_uses_support_nearest_measured_position(end_x, forces, accepted):
    """Moving End A shall use the new support; zero resultant means no contact."""
    reaction = slay_probe.end_a_support_reaction(end_x, [-31.1, -36.1, -41.1], forces)
    result = slay_probe.evaluate_static_acceptance(
        wire_length=10, end_a_reaction=reaction, last_support_force=0,
        grounded_tension=[0, 1], overbend_strain=0.00267,
        lift_off_arc=55, departure_angle=21,
        reference={"lift_off_arc": 55, "departure_angle": 21})
    assert result["rows"]["end_a_reaction"]["status"] == ("PASS" if accepted else "FAIL")
    assert result["counts"]["failed"] == (0 if accepted else 1)


def test_zero_end_a_resultant_cannot_pass_acceptance():
    with pytest.raises(ValueError, match="End A.*contact"):
        slay_probe.assert_static_acceptance(
            wire_length=10, end_a_reaction=0, last_support_force=0,
            grounded_tension=[0], overbend_strain=0.00267,
            lift_off_arc=55, departure_angle=21,
            reference={"lift_off_arc": 55, "departure_angle": 21})


@pytest.mark.parametrize("end_x,support_x,forces", [
    (float("nan"), [-31.1], [1]), (-31.1, [], []),
    (-31.1, [-31.1, -36.1], [1]),
    (-31.1, [float("inf")], [1]), (-31.1, [-31.1], [float("nan")]),
])
def test_end_a_support_selection_requires_finite_aligned_readbacks(end_x, support_x, forces):
    with pytest.raises(ValueError, match="support"):
        slay_probe.end_a_support_reaction(end_x, support_x, forces)


@pytest.mark.parametrize("depth", [80, 80.0])
def test_stinger_depth_lookup_compares_numerically(depth):
    case = dict(TABLE["cases"][3], depth_m=depth)
    spec = preparation_driver().case_spec(TABLE["basis"], case)
    assert sum(section.length for section in spec.equipment.stinger.sections) == 90


@pytest.mark.parametrize("consumer", ["prepare", "screen"])
@pytest.mark.parametrize("depth", [60, 80.0])
def test_depth_consumers_reject_unknown_and_accept_equivalent_depth(tmp_path, consumer, depth):
    table = deepcopy(TABLE)
    case = table["cases"][3]
    case["depth_m"] = depth
    if consumer == "prepare":
        invoke = lambda: preparation_driver().case_spec(table["basis"], case)
    else:
        (tmp_path / "case-table.yml").write_text(yaml.safe_dump(table))
        # Reuse only the retained model inputs; the screen shall not regenerate them.
        import shutil
        shutil.copytree(CAMPAIGN / "models" / case["id"], tmp_path / "models" / case["id"])
        invoke = lambda: slay_probe.screen_case(tmp_path, case["id"])
    if depth == 60:
        with pytest.raises(ValueError, match="stinger.*depth|depth.*stinger"):
            invoke()
    elif consumer == "screen":
        assert invoke()["current"]["supported_arc_m"] == 90
    else:
        assert sum(s.length for s in invoke().equipment.stinger.sections) == 90


def test_absent_depth_map_retains_the_base_stinger():
    basis = deepcopy(TABLE["basis"])
    del basis["stinger_by_depth"]
    spec = preparation_driver().case_spec(basis, TABLE["cases"][3])
    assert sum(s.length for s in spec.equipment.stinger.sections) == sum(
        s["length"] for s in basis["equipment"]["stinger"]["sections"])


@pytest.mark.parametrize("span", [2100, 1999.9])
def test_unreachable_end_b_is_rejected_before_native_execution(span):
    spec = preparation_driver().case_spec(TABLE["basis"], TABLE["cases"][0])
    stinger = spec.equipment.stinger
    end_a_x = stinger.origin_position[0] + stinger.firing_line_length
    spec.equipment.slay_end_b_x = end_a_x - span
    length = sum(s.length for s in spec.pipeline.segments)
    assert math.hypot(span, spec.environment.water.depth - 0.485) > length
    with pytest.raises(ValueError, match="End B.*reach|End B.*chord"):
        LinesBuilder(spec, BuilderContext()).build()


def test_criteria_revision_is_bumped_and_all_linked_records_agree():
    criteria = json.loads((CAMPAIGN / "criteria.json").read_text())
    assert criteria["revision"] == "execution-3"
    assert TABLE["criteria_revision"] == criteria["revision"]
    for name in ("first-timed-case.json", "preparation.json", "manifest.template.json"):
        assert json.loads((CAMPAIGN / name).read_text())["criteria_revision"] == criteria["revision"]


@pytest.mark.parametrize("mapping", [None, {}, {"80": {"length_m": 90}, "80.0": {"length_m": 60}},
                                    {"not-a-depth": {}}, {"nan": {}}, {"inf": {}}])
def test_invalid_depth_maps_are_rejected(mapping):
    with pytest.raises(ValueError, match="stinger.*depth|depth.*stinger"):
        slay_probe.stinger_layout_for_depth({"stinger_by_depth": mapping}, 80)


@pytest.mark.parametrize("excess", [-0.001, 0, 0.001])
def test_end_b_chord_screen_boundary(excess):
    spec = preparation_driver().case_spec(TABLE["basis"], TABLE["cases"][0])
    # Make the seed endpoint chord horizontal so equality is exact.
    spec.environment.water.depth = 0.485
    stinger = spec.equipment.stinger
    end_a_x = stinger.origin_position[0] + stinger.firing_line_length
    length = sum(s.length for s in spec.pipeline.segments)
    spec.equipment.slay_end_b_x = end_a_x - length - excess
    if excess > 0:
        with pytest.raises(ValueError, match="End B.*reach"):
            LinesBuilder(spec, BuilderContext()).build()
    else:
        LinesBuilder(spec, BuilderContext()).build()
