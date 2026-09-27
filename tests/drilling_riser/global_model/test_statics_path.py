"""Campaign statics (W4 batch-1 probe findings, 2026-09-27):

* the static state is path-independent but the solver is not: a full-current start diverges for some variants, a
  still-water continuation flips the ring to the yawed branch at large offsets for others. The adapter therefore
  tries statics paths in order (reloading the model between attempts) and rejects a path whose ring yaw leaves
  1 deg at any step;
* the tensioner line tension is calibrated in a separate still-water, zero-offset solve per model variant (the
  tensioner setting) and passed to the cases as ``tensioner_line_tension_n``;
* the ring balance carries the buoyancy of the wetted ring; the tensioner vertical check is a 1 % branch detector.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser import campaign as cp
from digitalmodel.solvers.orcaflex import orcaflex_api
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


def _case(base, **params):
    return {"case_id": "C-1", "analysis": "statics", "params": {"base_spec": base, **params}}


CURRENT = {"depth_speed_m_s": [[0, 0.6], [300, 0.1]]}


class _Env:
    def __init__(self, speed):
        self.RefCurrentSpeed = speed


class _Winch:
    typeName = "Winch"

    def __init__(self, name, t):
        self.name, self.t = name, [t, t, t]

    def GetDataRowCount(self, _):
        return len(self.t)

    def GetData(self, _, i):
        return self.t[i]

    def SetData(self, _, i, v):
        self.t[i] = v


class _Body:
    def __init__(self, model):
        self.InitialX = self.InitialY = 0.0
        self._m = model

    def StaticResult(self, name, *args):
        return {"Rotation 3": self._m.yaw, "Wetted volume": 0.0}[name]


class _FakeModel(dict):
    """Records every statics call (vessel x, current speed); ``fail(model)`` decides whether a call diverges."""

    def __init__(self, speed=0.6, fail=lambda m: False, vertical=None):
        super().__init__()
        self["Vessel"], self["TensionRing"] = _Body(self), _Body(self)
        self.environment = _Env(speed)
        self.winches = [_Winch(f"Tensioner{i}", 100.0) for i in range(1, 7)]
        self.objects = self.winches
        self.calls, self.reloads, self.fail, self.yaw = [], 0, fail, 0.0
        self.vertical = vertical

    def CalculateStatics(self):
        self.calls.append((self["Vessel"].InitialX, self.environment.RefCurrentSpeed))
        if self.fail(self):
            raise RuntimeError("Static calculation failed (Whole system statics: Not converged.)")

    def UseCalculatedPositions(self, _):
        pass


def _reloader(m, speed):
    def reload():
        m.reloads += 1
        m.environment.RefCurrentSpeed = speed
        m.yaw = 0.0
        for n in ("Vessel", "TensionRing"):
            m[n].InitialX = m[n].InitialY = 0.0
    return reload


def _spec(base_spec, **kw):
    return cp.RiserCampaignAdapter().spec(_case(base_spec, heading_deg=0.0, **kw))


def test_first_path_is_the_full_current_continuation_from_the_seed(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=2.0, current=CURRENT)
    m = _FakeModel()
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {}, reload=_reloader(m, 0.6))
    wd = spec.environment.water_depth_m
    assert info["strategy"] == "current_at_seed" and m.reloads == 0 and info["attempts"] == []
    assert m.calls[0] == (pytest.approx(-0.02 * wd), 0.6) and m.calls[-1][0] == pytest.approx(0.02 * wd)
    assert all(s == 0.6 for _, s in m.calls)


def test_fallback_ramps_the_current_at_the_seed_after_a_divergence(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=2.0, current=CURRENT)
    wd = spec.environment.water_depth_m
    # full current at the seed from the straight start diverges; everything else converges
    m = _FakeModel(fail=lambda m: len(m.calls) == 1)
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {"statics_paths": ["current_at_seed", "ramp_at_seed"]}, reload=_reloader(m, 0.6))
    assert info["strategy"] == "ramp_at_seed" and m.reloads == 1
    assert info["attempts"][0]["strategy"] == "current_at_seed" and "Not converged" in info["attempts"][0]["error"]
    second = m.calls[1:]
    assert second[0] == (pytest.approx(-0.02 * wd), 0.0)  # still water at the seed
    assert [s for _, s in second[1:5]] == pytest.approx([0.15, 0.3, 0.45, 0.6])  # ramp at the seed
    assert second[-1] == (pytest.approx(0.02 * wd), 0.6)  # then to the target in full current
    assert m.environment.RefCurrentSpeed == 0.6


def test_a_yaw_flip_rejects_the_path(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=4.0, current=CURRENT)

    def flip(m):  # the first path flips the ring at its third step
        if m.reloads == 0 and len(m.calls) == 3:
            m.yaw = 180.0
        return False

    m = _FakeModel(fail=flip)
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {"statics_paths": ["current_at_seed", "ramp_at_seed"]}, reload=_reloader(m, 0.6))
    assert info["attempts"][0]["strategy"] == "current_at_seed" and "yaw" in info["attempts"][0]["error"]
    assert info["strategy"] == "ramp_at_seed"


def test_all_paths_failing_is_statics_diverged(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=2.0, current=CURRENT)
    m = _FakeModel(fail=lambda m: True)
    with pytest.raises(pr.CaseFailed) as e:
        cp.robust_statics(m, spec, {}, reload=_reloader(m, 0.6))
    assert e.value.status == "statics_diverged"
    for s in cp.statics_paths(current=True):
        assert s in str(e.value)


def test_licence_error_is_not_swallowed(base_spec):
    spec = _spec(base_spec, offset_pct_wd=2.0, current=CURRENT)
    m = _FakeModel()

    def lic():
        raise RuntimeError("Licence error: no licence available")

    m.CalculateStatics = lic
    with pytest.raises(RuntimeError, match="Licence"):
        cp.robust_statics(m, spec, {}, reload=_reloader(m, 0.6))


def test_without_current_an_aiding_current_is_used_then_removed(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=2.0)
    wd = spec.environment.water_depth_m
    m = _FakeModel(speed=0.0)
    m.environment.RefCurrentDirection = 0.0
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {}, reload=_reloader(m, 0.0))
    assert info["strategy"] == "aid_current" and m.reloads == 0
    second = m.calls
    assert second[0] == (pytest.approx(-0.02 * wd), cp.AID_CURRENT_M_S)
    assert second[-2] == (pytest.approx(0.02 * wd), cp.AID_CURRENT_M_S)
    assert second[-1] == (pytest.approx(0.02 * wd), 0.0)  # the final state is in still water
    assert m.environment.RefCurrentSpeed == 0.0


def test_the_last_successful_path_of_a_variant_is_tried_first(base_spec, monkeypatch):
    """Stage A: the path that converges depends on the variant (12.5 ppg: aiding current; 14.0 ppg: plain seeded);
    a failed attempt costs the full iteration budget, so a worker tries the variant's last successful path first."""
    monkeypatch.setattr(cp, "_LAST_OK", {})
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    spec = _spec(base_spec, offset_pct_wd=2.0)
    m1 = _FakeModel(speed=0.0, fail=lambda m: m.environment.RefCurrentSpeed > 0)  # the aiding current diverges
    m1.environment.RefCurrentDirection = 0.0
    assert cp.robust_statics(m1, spec, {}, reload=_reloader(m1, 0.0))["strategy"] == "direct_at_target"
    m2 = _FakeModel(speed=0.0, fail=lambda m: m.environment.RefCurrentSpeed > 0)
    m2.environment.RefCurrentDirection = 0.0
    info = cp.robust_statics(m2, spec, {}, reload=_reloader(m2, 0.0))
    assert info["strategy"] == "direct_at_target" and info["attempts"] == [] and m2.reloads == 0
    # another mud weight is another variant
    other = _spec(base_spec, offset_pct_wd=2.0, mud_density_kg_m3=1400.0)
    m3 = _FakeModel(speed=0.0)
    m3.environment.RefCurrentDirection = 0.0
    assert cp.robust_statics(m3, other, {}, reload=_reloader(m3, 0.0))["strategy"] == "aid_current"


def test_path_orders(base_spec):
    assert cp.statics_paths(current=True) == ["current_at_seed", "direct_at_target", "neighbour_walk", "tension_ramp",
                                              "ramp_at_seed", "ramp_at_target", "fine_steps"]
    # still water: the aiding current first (stage A: the plain seeded path failed on every 12.5 ppg case after the
    # full iteration budget, about 8 min per case at 57 workers, before the aiding current converged in two solves)
    assert cp.statics_paths(current=False) == ["aid_current", "direct_at_target", "neighbour_walk", "tension_ramp",
                                               "current_at_seed", "fine_steps"]


def test_tension_ramp_solves_at_a_higher_line_tension_then_steps_down_to_the_case_value(base_spec, monkeypatch):
    """Stage A: the TT-MIN, one-tensioner-failed cases (LFJ tension about 60 kips at 12.5 ppg) failed every path; the
    tension continuation starts from 1.5 x the case line tension and steps down to it at the case offset."""
    spec = _spec(base_spec, offset_pct_wd=2.0, current=CURRENT)
    wd = spec.environment.water_depth_m
    m = _FakeModel(fail=lambda m: m.reloads == 0)  # the first path fails
    seen = []
    orig = m.CalculateStatics

    def rec():
        seen.append((m["Vessel"].InitialX, m.environment.RefCurrentSpeed, m.winches[1].t[-1]))
        orig()

    m.CalculateStatics = rec
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {"statics_paths": ["current_at_seed", "tension_ramp"]}, reload=_reloader(m, 0.6))
    assert info["strategy"] == "tension_ramp"
    ramp = seen[1:]
    assert ramp[0][2] == pytest.approx(100.0 * cp.TENSION_RAMP[0]) and ramp[0][0] == pytest.approx(-0.02 * wd)
    tail = [t for x, s, t in ramp if x == pytest.approx(0.02 * wd)]
    assert tail[-len(cp.TENSION_RAMP):] == pytest.approx([100.0 * f for f in cp.TENSION_RAMP])
    assert all(w.t == [pytest.approx(100.0)] * 3 for w in m.winches)  # the case setting at the end
    assert all(s == 0.6 for _, s, _ in ramp)


def test_direct_path_solves_once_at_the_target(base_spec, monkeypatch):
    spec = _spec(base_spec, offset_pct_wd=3.0)
    wd = spec.environment.water_depth_m
    m = _FakeModel(speed=0.0, fail=lambda m: m.reloads < 2)
    m.environment.RefCurrentDirection = 0.0
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    paths = {"statics_paths": ["aid_current", "current_at_seed", "direct_at_target"]}
    info = cp.robust_statics(m, spec, paths, reload=_reloader(m, 0.0))
    assert info["strategy"] == "direct_at_target" and info["steps"] == 1
    assert m.calls[-1] == (pytest.approx(0.03 * wd), 0.0)


def test_calibration_scales_every_line_to_the_vertical_target(base_spec, monkeypatch):
    spec = _spec(base_spec)
    target = cp.tensioner_vertical_target_n(spec)
    m = _FakeModel(speed=0.0)
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: 0.98 * target * model.winches[0].t[-1] / 100.0)
    info = cp.calibrate_line_tension(m, spec, solve=m.CalculateStatics)
    assert info["factor"] == pytest.approx(1 / 0.98, rel=1e-9)
    assert info["line_tension_n"] == pytest.approx(100.0 / 0.98 * 1000.0)
    assert all(w.t == [pytest.approx(100.0 / 0.98)] * 3 for w in m.winches)


def test_explicit_line_tension_is_applied_in_prepare(base_spec):
    m = _FakeModel(speed=0.0)
    cp.RiserCampaignAdapter().prepare(m, _case(base_spec, tensioner_line_tension_n=123456.0))
    assert all(w.t == [pytest.approx(123.456)] * 3 for w in m.winches)


class _Obj:
    def __init__(self, **res):
        self._res = res

    def StaticResult(self, name, *args):
        return self._res[name]


@pytest.mark.parametrize(("tv_factor", "wet_m3", "ok"), [
    (1.005, 0.0, True),         # physical deviation at an offset (tensioner lines inclined)
    (1 - 0.013, 0.0, True),     # probe: TT-MIN, one failed, 10-yr loop current, -10 % WD (ring not yawed)
    (5682 / 6026, 0.0, False),  # the yawed branch
    (1.0, 1.2, True),           # the ring set down to MSL: its wetted volume is in the balance
])
def test_branch_tolerance_and_ring_buoyancy(base_spec, monkeypatch, tv_factor, wet_m3, ok):
    from digitalmodel.drilling_riser.global_model.hand_checks import tension_references

    spec = cp.case_spec(_case(base_spec))
    t = cp.tensioner_vertical_target_n(spec) * tv_factor
    w = tension_references(spec)["ring_weight_n"]
    b = spec.environment.water_density_kg_m3 * 9.80665 * wet_m3
    m = {"TensionRing": _Obj(**{"Rotation 3": 0.0, "Wetted volume": wet_m3}),
         "Riser": _Obj(**{"End GZ force": (w - t - b) / 1000.0}), "InnerBarrel": _Obj(**{"End GZ force": 0.0})}
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: t)
    monkeypatch.setattr(cp, "_end_gz_n", lambda model, line, end: model[line].StaticResult("End GZ force") * 1000.0)
    if ok:
        info = cp.physical_state_checks(m, spec)
        assert abs(info["ring_balance_n"]) < 1.0 and info["ring_buoyancy_n"] == pytest.approx(b)
    else:
        with pytest.raises(pr.CaseFailed):
            cp.physical_state_checks(m, spec)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_calibration_case_then_a_case_with_the_calibrated_tension(base_spec, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    ad = cp.RiserCampaignAdapter()
    cal = _case(base_spec, heading_deg=0.0, top_tension_n=2.6e6, calibrate_tension=True)  # ring below z_static
    m = orun.load_model(ad.build(cal, tmp_path / "cal"))
    ad.prepare(m, cal)
    info = ad.statics(m, cal)
    assert info["calibration"]["factor"] != pytest.approx(1.0, abs=1e-6)
    assert orun.tensioner_vertical_sum_n(m) == pytest.approx(2.6e6, rel=1e-6)
    use = _case(base_spec, heading_deg=0.0, top_tension_n=2.6e6,
                tensioner_line_tension_n=info["calibration"]["line_tension_n"])
    use["case_id"] = "C-2"
    m2 = orun.load_model(ad.build(use, tmp_path / "use"))
    ad.prepare(m2, use)
    ad.statics(m2, use)
    assert orun.tensioner_vertical_sum_n(m2) == pytest.approx(2.6e6, rel=1e-5)


def test_neighbour_walk_starts_at_the_nearest_converging_offset_and_walks_in_quarter_steps(base_spec, monkeypatch):
    """Stage A: at 12.5 ppg in the 1-yr current whole offsets (-7..-5, -3, -2, +3 % WD) fail on every path from the
    straight start while their neighbours converge; a converged neighbour walked in 0.25 % steps reaches them."""
    spec = _spec(base_spec, offset_pct_wd=-2.0, current=CURRENT)
    wd = spec.environment.water_depth_m
    bad = {round(-0.02 * wd, 6), round(-0.01 * wd, 6)}  # the target and its first neighbour fail from a straight start

    def fail(m):
        straight = len(m.calls) == 1 or m.fresh
        m.fresh = False
        return straight and round(m["Vessel"].InitialX, 6) in bad

    m = _FakeModel(fail=fail)
    m.fresh = True
    orig_ucp = m.UseCalculatedPositions
    m.UseCalculatedPositions = lambda v: orig_ucp(v)
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, {"statics_paths": ["neighbour_walk"]}, reload=_reloader(m, 0.6))
    assert info["strategy"] == "neighbour_walk"
    xs = [round(x / wd * 100, 4) for x, _ in m.calls]
    assert xs[:2] == [-1.0, -3.0]  # +1 % failed from a straight start, -1 % (i.e. -3 % WD) converged
    assert xs[2:] == [-2.75, -2.5, -2.25, -2.0]