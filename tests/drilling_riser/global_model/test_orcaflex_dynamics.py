"""Solver round trips for the milestone-2 features on the synthetic riser (needs OrcFxAPI)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec
from .test_nonlinear_vessel_foundation import _foundation, _vessel_motion

pytestmark = [
    pytest.mark.solver,
    pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available"),
]


def _spec(**extra):
    from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec

    d = synthetic_spec().model_dump()
    d.update({k: (v.model_dump() if hasattr(v, "model_dump") else v) for k, v in extra.items()})
    return RiserGlobalModelSpec.model_validate(d)


def test_nonlinear_flex_joint_small_rotation_matches_linear_statics(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import end_effective_tensions, load_and_solve_statics
    from digitalmodel.drilling_riser.global_model.spec import flex_joint_from_secants

    base = synthetic_spec()
    k = base.upper_flex_joint.rotational_stiffness_nm_per_rad * 3.141592653589793 / 180
    nl = _spec(upper_flex_joint=flex_joint_from_secants(pivot_z_m=base.upper_flex_joint.pivot_z_m,
                                                       secants_nm_per_deg=[(1.0, k), (15.0, 0.5 * k)]))
    te_l = end_effective_tensions(load_and_solve_statics(write_model(base, tmp_path / "l") / "master.yml"))
    te_n = end_effective_tensions(load_and_solve_statics(write_model(nl, tmp_path / "n") / "master.yml"))
    assert te_n["riser_top_n"] == pytest.approx(te_l["riser_top_n"], rel=1e-6)


def test_regular_wave_dynamics_give_governing_responses(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import (
        governing_responses,
        load_and_solve_statics,
        run_dynamics,
    )
    from digitalmodel.drilling_riser.global_model.spec import Dynamics, RegularWave

    spec = _spec(vessel_motion=_vessel_motion(), regular_wave=RegularWave(height_m=3.0, period_s=9.0, direction_deg=0.0),
                 dynamics=Dynamics(time_step_s=0.1, build_up_s=9.0, duration_s=27.0))
    model = load_and_solve_statics(write_model(spec, tmp_path) / "master.yml")
    run_dynamics(model)
    r = governing_responses(model, spec)
    assert r["ufj_angle_max_deg"] > 0.01 and r["lfj_angle_max_deg"] > 0.0
    assert r["te_top_max_n"] >= r["te_top_min_n"] > 0
    assert r["riser_von_mises_max_pa"] > 0


def test_log_interval_time_step_and_damping_can_be_changed(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import (
        apply_rayleigh_damping,
        load_model,
        set_log_interval,
        set_time_step,
    )

    model = load_model(write_model(synthetic_spec(), tmp_path) / "master.yml")
    set_time_step(model, 0.05)
    set_log_interval(model, 0.05)
    apply_rayleigh_damping(model, ratio_percent=0.3, period_s=9.0)
    assert model.general.ImplicitConstantTimeStep == pytest.approx(0.05)
    assert model.general.TargetLogSampleInterval == pytest.approx(0.05)
    lt = [o for o in model.objects if o.typeName == "Line type"][0]
    assert lt.RayleighDampingCoefficients == "Structural"


def test_foundation_statics_carry_the_stack_into_the_conductor(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model
    from digitalmodel.drilling_riser.global_model.orcaflex_run import end_effective_tensions, load_and_solve_statics

    fixed = synthetic_spec()
    found = _spec(foundation=_foundation())
    te_f = end_effective_tensions(load_and_solve_statics(write_model(fixed, tmp_path / "f") / "master.yml"))
    te_c = end_effective_tensions(load_and_solve_statics(write_model(found, tmp_path / "c") / "master.yml"))
    # the riser above the stack does not see the boundary in still water
    assert te_c["riser_bottom_n"] == pytest.approx(te_f["riser_bottom_n"], rel=1e-4)
