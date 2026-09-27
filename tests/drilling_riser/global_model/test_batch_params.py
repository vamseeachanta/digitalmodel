"""W4 batch parameters: failed tensioner, section stiffness factors, tensioner representation, RAO origin shift and
flex-joint curve override (the sensitivity families of the load-case matrix)."""

from __future__ import annotations

import math
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.drilling_riser import campaign as cp
from digitalmodel.drilling_riser.global_model.build import build_generic_spec, winch_tension_n
from digitalmodel.drilling_riser.global_model.spec import Tensioners
from digitalmodel.solvers.orcaflex import parallel_runner as pr

from .conftest import synthetic_spec
from .test_nonlinear_vessel_foundation import _vessel_motion


@pytest.fixture
def base_spec(tmp_path: Path) -> str:
    d = synthetic_spec().model_dump(mode="json")
    d["vessel_motion"] = _vessel_motion().model_dump(mode="json")
    p = tmp_path / "model-spec.yml"
    p.write_text(yaml.safe_dump({"model": d}, sort_keys=False), encoding="utf-8")
    return str(p)


def _case(base, analysis="statics", **params):
    return {"case_id": "C-1", "analysis": analysis, "params": {"base_spec": base, "statics": "direct", **params}}


# --------------------------------------------------------------------------- one tensioner failed


def test_failed_count_must_leave_a_tensioner():
    kw = dict(count=6, sheave_radius_m=3.75, sheave_z_m=20.0, ring_attach_radius_m=1.8, total_vertical_tension_n=3e6)
    assert Tensioners(**kw, failed_count=1).failed_count == 1
    assert Tensioners(**kw).failed_count == 0
    for bad in (-1, 6):
        with pytest.raises(ValidationError):
            Tensioners(**kw, failed_count=bad)


def test_failed_tensioner_is_removed_and_the_others_keep_their_tension(base_spec):
    intact = cp.case_spec(_case(base_spec))
    failed = cp.case_spec(_case(base_spec, tensioners_failed=1))
    assert failed.tensioners.failed_count == 1
    w_i = build_generic_spec(intact)["generic"]["winches"]
    w_f = build_generic_spec(failed)["generic"]["winches"]
    assert [w["name"] for w in w_i] == [f"Tensioner{i}" for i in range(1, 7)]
    assert [w["name"] for w in w_f] == [f"Tensioner{i}" for i in range(2, 7)]  # Tensioner1 (first azimuth) fails
    t_i = w_i[1]["properties"]["StageMode, StageValue"][0][1]
    t_f = w_f[0]["properties"]["StageMode, StageValue"][0][1]
    assert t_f == pytest.approx(t_i) == pytest.approx(winch_tension_n(intact) / 1000.0)
    # the geometry of the remaining lines is unchanged
    assert w_f[0]["properties"] == w_i[1]["properties"]


def test_effective_vertical_target_with_a_failed_tensioner(base_spec):
    s = cp.case_spec(_case(base_spec, tensioners_failed=1))
    assert cp.tensioner_vertical_target_n(s) == pytest.approx(s.tensioners.total_vertical_tension_n * 5 / 6)
    s0 = cp.case_spec(_case(base_spec))
    assert cp.tensioner_vertical_target_n(s0) == s0.tensioners.total_vertical_tension_n


class _Obj:
    def __init__(self, **res):
        self._res = res

    def StaticResult(self, name, *args):
        return self._res[name]


@pytest.mark.parametrize(("tv_factor", "ok"), [(5 / 6, True), (1.0, False)])
def test_physical_checks_use_the_remaining_tensioners(base_spec, monkeypatch, tv_factor, ok):
    from digitalmodel.drilling_riser.global_model.hand_checks import tension_references

    spec = cp.case_spec(_case(base_spec, tensioners_failed=1))
    t = spec.tensioners.total_vertical_tension_n * tv_factor
    w = tension_references(spec)["ring_weight_n"]
    m = {"TensionRing": _Obj(**{"Rotation 3": 0.0}), "Riser": _Obj(**{"End GZ force": (w - t) / 1000.0}),
         "InnerBarrel": _Obj(**{"End GZ force": 0.0})}
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: t)
    monkeypatch.setattr(cp, "_end_gz_n", lambda model, line, end: model[line].StaticResult("End GZ force") * 1000.0)
    if ok:
        assert cp.physical_state_checks(m, spec)["tensioner_vertical_n"] == pytest.approx(t)
    else:
        with pytest.raises(pr.CaseFailed):
            cp.physical_state_checks(m, spec)


# --------------------------------------------------------------------------- sensitivity parameters


def test_section_stiffness_factors_scale_ei_and_ea(base_spec):
    base = cp.case_spec(_case(base_spec))
    s = cp.case_spec(_case(base_spec, section_stiffness_factors={"LMRP": 2.0, "LFJ upper body": 0.5}))
    lm0 = next(x for x in base.stack if x.name == "LMRP")
    lm = next(x for x in s.stack if x.name == "LMRP")
    assert lm.ei_nm2 == pytest.approx(2.0 * lm0.ei_nm2) and lm.ea_n == pytest.approx(2.0 * lm0.ea_n)
    fj0 = next(x for x in base.riser if x.name == "LFJ upper body")
    fj = next(x for x in s.riser if x.name == "LFJ upper body")
    assert fj.ei_nm2 == pytest.approx(0.5 * fj0.ei_nm2)
    bop0 = next(x for x in base.stack if x.name == "BOP")
    assert next(x for x in s.stack if x.name == "BOP").ei_nm2 == bop0.ei_nm2  # untouched


def test_unknown_section_name_is_rejected(base_spec):
    with pytest.raises(ValueError, match="no section"):
        cp.case_spec(_case(base_spec, section_stiffness_factors={"Tree": 2.0}))


def test_tensioner_representation_and_rao_origin_shift(base_spec):
    base = cp.case_spec(_case(base_spec))
    s = cp.case_spec(_case(base_spec, tensioner_representation="vertical_force", rao_origin_dx_m=6.3))
    assert s.tensioners.representation == "vertical_force"
    assert s.vessel_motion.rao_origin_m[0] == pytest.approx(base.vessel_motion.rao_origin_m[0] + 6.3)
    assert s.vessel_motion.rao_origin_m[1:] == base.vessel_motion.rao_origin_m[1:]


def test_flex_joint_curve_override(base_spec):
    curve = [[0.0, 0.0], [0.1, 30000.0], [1.0, 150000.0], [10.0, 800000.0]]  # (deg, N.m), synthetic
    s = cp.case_spec(_case(base_spec, flex_joint_curves={"lower": curve}))
    assert s.lower_flex_joint.moment_rotation_deg_nm == [tuple(p) for p in curve]
    assert s.lower_flex_joint.rotational_stiffness_nm_per_rad == pytest.approx(30000.0 / 0.1 * 180 / math.pi)
    base = cp.case_spec(_case(base_spec))
    assert s.upper_flex_joint == base.upper_flex_joint
    assert s.lower_flex_joint.pivot_z_m == base.lower_flex_joint.pivot_z_m
