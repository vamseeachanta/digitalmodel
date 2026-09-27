"""Milestone 3: structural damping carried in the text model, a static vessel offset and a
depth-varying current profile (spec validation and text-model generation; no OrcFxAPI needed),
plus solver round trips on the synthetic riser (marked ``solver``)."""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.global_model.build import build_generic_spec
from digitalmodel.drilling_riser.global_model.spec import (
    CurrentProfile,
    RiserGlobalModelSpec,
    StructuralDamping,
)
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec


def _spec(**extra) -> RiserGlobalModelSpec:
    d = synthetic_spec().model_dump()
    d.update({k: (v.model_dump() if hasattr(v, "model_dump") else v) for k, v in extra.items()})
    return RiserGlobalModelSpec.model_validate(d)


# ------------------------------------------------------------------ structural damping
def test_structural_damping_is_stiffness_proportional_rayleigh():
    d = StructuralDamping(ratio_percent=0.3, period_s=9.0)
    # classical coefficients: mass 0, stiffness beta = 2 zeta / omega at the stated period
    assert d.stiffness_coefficient_s == pytest.approx(2 * 0.003 / (2 * math.pi / 9.0))
    assert d.stiffness_coefficient_s == pytest.approx(0.008594, abs=1e-6)


@pytest.mark.parametrize("bad", [{"ratio_percent": 0.0, "period_s": 9.0}, {"ratio_percent": 0.3, "period_s": 0.0}])
def test_structural_damping_rejects_non_positive_values(bad):
    with pytest.raises(ValueError):
        StructuralDamping(**bad)


def test_structural_damping_is_emitted_and_referenced_by_every_line_type():
    gen = build_generic_spec(_spec(structural_damping=StructuralDamping(ratio_percent=0.3, period_s=9.0)))
    sets = gen["generic"]["rayleigh_damping"]["data"]
    assert sets == [{"Name": "Structural", "Mode": "Coefficients (classical)", "MassCoefficient": 0.0,
                     "StiffnessCoefficient": pytest.approx(0.0085944, rel=1e-4),
                     "ApplyToGeometricStiffness": "Yes"}]
    for lt in gen["generic"]["line_types"]:
        assert lt["properties"]["RayleighDampingCoefficients"] == "Structural"


def test_no_structural_damping_emits_no_damping_set():
    gen = build_generic_spec(synthetic_spec())
    assert "rayleigh_damping" not in gen["generic"]
    assert all("RayleighDampingCoefficients" not in lt["properties"] for lt in gen["generic"]["line_types"])


# ------------------------------------------------------------------ vessel offset
def test_vessel_offset_moves_the_vessel_in_statics():
    gen = build_generic_spec(_spec(vessel_offset_m=(12.5, -3.0)))
    v = gen["generic"]["vessels"][0]
    assert v["initial_position"] == [12.5, -3.0, 0]


def test_default_vessel_offset_is_zero():
    assert build_generic_spec(synthetic_spec())["generic"]["vessels"][0]["initial_position"] == [0, 0, 0]


# ------------------------------------------------------------------ current profile
def test_current_profile_is_emitted_as_reference_speed_and_factors():
    cur = CurrentProfile(direction_deg=0.0, depth_speed_m_s=[(0.0, 1.0), (50.0, 0.5), (200.0, 0.1)])
    env = build_generic_spec(_spec(current=cur))["environment"]
    assert env["current"]["speed"] == pytest.approx(1.0)
    assert env["current"]["direction"] == pytest.approx(0.0)
    assert env["current"]["profile"] == [[0.0, 1.0], [50.0, 0.5], [200.0, pytest.approx(0.1)]]


def test_current_profile_with_a_subsurface_maximum_is_normalised_by_the_maximum():
    cur = CurrentProfile(direction_deg=90.0, depth_speed_m_s=[(0.0, 0.4), (100.0, 0.8), (300.0, 0.2)])
    env = build_generic_spec(_spec(current=cur))["environment"]
    assert env["current"]["speed"] == pytest.approx(0.8)
    assert env["current"]["profile"] == [[0.0, 0.5], [100.0, 1.0], [300.0, 0.25]]


@pytest.mark.parametrize("pts", [[(0.0, 1.0)], [(10.0, 1.0), (5.0, 0.5)], [(0.0, -0.1), (10.0, 0.2)]])
def test_current_profile_validation(pts):
    with pytest.raises(ValueError):
        CurrentProfile(direction_deg=0.0, depth_speed_m_s=pts)


def test_no_current_emits_no_current():
    assert "current" not in build_generic_spec(synthetic_spec())["environment"]


# ------------------------------------------------------------------ solver round trips
@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_text_model_carries_the_damping_set(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import load_model

    spec = _spec(structural_damping=StructuralDamping(ratio_percent=0.3, period_s=9.0))
    m = load_model(write_model(spec, tmp_path) / "master.yml")
    r = m["Structural"]
    assert r.StiffnessCoefficient == pytest.approx(0.0085944, rel=1e-4)
    assert all(o.RayleighDampingCoefficients == "Structural" for o in m.objects if o.typeName == "Line type")


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_static_offset_and_current_rotate_the_flex_joints(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import load_and_solve_statics, static_responses

    base = synthetic_spec()
    r0 = static_responses(load_and_solve_statics(write_model(base, tmp_path / "0") / "master.yml"), base)
    off = _spec(vessel_offset_m=(10.0, 0.0))
    r1 = static_responses(load_and_solve_statics(write_model(off, tmp_path / "1") / "master.yml"), off)
    cur = _spec(current=CurrentProfile(direction_deg=0.0, depth_speed_m_s=[(0.0, 1.0), (300.0, 1.0)]))
    r2 = static_responses(load_and_solve_statics(write_model(cur, tmp_path / "2") / "master.yml"), cur)
    assert r0["lfj_angle_deg"] < 0.01 and r0["ufj_angle_deg"] < 0.01
    assert r1["lfj_angle_deg"] > 0.5 and r1["ufj_angle_deg"] > 0.5
    assert r2["lfj_angle_deg"] > 0.1
    assert r1["riser_von_mises_max_pa"] > r0["riser_von_mises_max_pa"] > 0
    assert r0["te_top_n"] > r0["te_bottom_n"] > 0
