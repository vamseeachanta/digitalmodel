"""Tensioner representation: inclined constant-tension lines (lateral tie to the vessel) or a vertical
force on the tension ring with no lateral tie (the equivalent-string practice). The two bound the
split of rotation between the upper and lower flex joints."""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.global_model.build import build_generic_spec
from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec, Tensioners
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec


def _vertical(**extra) -> RiserGlobalModelSpec:
    d = synthetic_spec().model_dump()
    d["tensioners"]["representation"] = "vertical_force"
    d.update(extra)
    return RiserGlobalModelSpec.model_validate(d)


def test_default_representation_is_lines():
    assert synthetic_spec().tensioners.representation == "lines"
    gen = build_generic_spec(synthetic_spec())["generic"]
    assert len(gen["winches"]) == 6
    assert "GlobalAppliedLoads" not in gen["buoys_6d"][0]["properties"]


def test_vertical_force_replaces_the_lines_by_a_global_vertical_load_on_the_ring():
    spec = _vertical()
    gen = build_generic_spec(spec)["generic"]
    assert gen.get("winches", []) == []
    loads = gen["buoys_6d"][0]["properties"]["GlobalAppliedLoads"]
    # global (earth-fixed) direction: the force stays vertical whatever the ring does
    assert loads == [{"Origin": [0, 0, 0], "Force": [0, 0, pytest.approx(3.0e3)], "Moment": [0, 0, 0]}]


def test_vertical_force_restrains_ring_yaw_through_riser_torsion_and_damps_statics():
    """Without lines nothing holds the ring about z (lines exclude torsion): the riser carries torsion
    (finite twisting stiffness at the finite-stiffness lower flex-joint end) and statics is damped."""
    gen = build_generic_spec(_vertical())["generic"]
    riser = next(ln for ln in gen["lines"] if ln["name"] == "Riser")["properties"]
    assert riser["IncludeTorsion"] is True
    key = "ConnectionxBendingStiffness, ConnectionyBendingStiffness, ConnectionTwistingStiffness"
    assert riser[key][0] == ["Infinity", None, "Infinity"]
    assert riser[key][1][2] > 0 and riser[key][1][2] != "Infinity"
    assert gen["general_properties"]["StaticsMinDamping"] > 1
    ib = next(ln for ln in gen["lines"] if ln["name"] == "InnerBarrel")["properties"]
    assert ib["IncludeTorsion"] is False
    lines_gen = build_generic_spec(synthetic_spec())["generic"]
    assert next(ln for ln in lines_gen["lines"] if ln["name"] == "Riser")["properties"]["IncludeTorsion"] is False
    assert "general_properties" not in lines_gen


def test_unknown_representation_is_rejected():
    t = synthetic_spec().tensioners.model_dump()
    t["representation"] = "cylinders"
    with pytest.raises(ValueError):
        Tensioners.model_validate(t)


def test_vertical_force_rating_check_uses_the_vertical_share_per_tensioner():
    d = synthetic_spec().model_dump()
    d["tensioners"]["representation"] = "vertical_force"
    d["tensioners"]["rated_tension_each_n"] = 3.0e6 / 6 * 0.99  # just below T / n
    with pytest.raises(ValueError, match="exceeds the rated"):
        RiserGlobalModelSpec.model_validate(d)
    d["tensioners"]["rated_tension_each_n"] = 3.0e6 / 6 * 1.01
    RiserGlobalModelSpec.model_validate(d)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_vertical_force_statics_balance_and_remove_the_lateral_tie(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.hand_checks import tension_references
    from digitalmodel.drilling_riser.global_model.orcaflex_run import (
        load_and_solve_statics,
        ring_vertical_residual_n,
        static_responses,
        tensioner_vertical_sum_n,
    )

    lines = synthetic_spec().model_dump()
    lines["vessel_offset_m"] = (6.0, 0.0)
    lines = RiserGlobalModelSpec.model_validate(lines)
    vert = _vertical(vessel_offset_m=(6.0, 0.0))
    m_l = load_and_solve_statics(write_model(lines, tmp_path / "l") / "master.yml")
    m_v = load_and_solve_statics(write_model(vert, tmp_path / "v") / "master.yml")
    assert tensioner_vertical_sum_n(m_v) == pytest.approx(3.0e6, rel=1e-9)
    ref = tension_references(vert)
    assert abs(ring_vertical_residual_n(m_v, ref["ring_weight_n"])) < 1e-3 * 3.0e6
    r_l, r_v = static_responses(m_l, lines), static_responses(m_v, vert)
    # without the tie the ring follows the vessel less closely: more rotation at the UFJ
    assert r_v["ufj_angle_deg"] > r_l["ufj_angle_deg"]
    assert math.isfinite(r_v["lfj_angle_deg"]) and r_v["lfj_angle_deg"] > 0
