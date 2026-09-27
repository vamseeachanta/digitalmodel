"""Campaign statics path (W4 batch-1 probe findings): line-tension calibration at the zero-offset still-water state,
offset continuation from there, current ramp; ring buoyancy in the balance; tensioner-vertical branch tolerance."""

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


class _Env:
    RefCurrentSpeed = 0.6


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


class _Pos:
    InitialX = InitialY = 0.0


class _FakeModel(dict):
    """Records every statics call: (vessel x, vessel y, current speed, line tension)."""

    def __init__(self, vertical_of_tension):
        super().__init__(Vessel=_Pos(), TensionRing=_Pos())
        self.environment = _Env()
        self.winches = [_Winch(f"Tensioner{i}", 100.0) for i in range(1, 7)]
        self.objects = self.winches
        self.calls = []
        self.vertical_of_tension = vertical_of_tension

    def CalculateStatics(self):
        self.calls.append((self["Vessel"].InitialX, self["Vessel"].InitialY, self.environment.RefCurrentSpeed,
                           self.winches[0].t[-1]))

    def UseCalculatedPositions(self, _):
        pass


def test_statics_path_calibrates_in_still_water_then_offsets_then_ramps_the_current(base_spec, monkeypatch):
    c = _case(base_spec, heading_deg=0.0, offset_pct_wd=2.0, current={"depth_speed_m_s": [[0, 0.6], [300, 0.1]]})
    ad = cp.RiserCampaignAdapter()
    spec = ad.spec(c)
    target = cp.tensioner_vertical_target_n(spec)
    # the vertical delivered is 0.98 x the target per unit of the specified line tension ratio
    m = _FakeModel(lambda t: 0.98 * target * t / 100.0)
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: model.vertical_of_tension(model.winches[0].t[-1]))
    monkeypatch.setattr(cp, "physical_state_checks", lambda model, s: {})
    info = cp.robust_statics(m, spec, c["params"])
    wd = spec.environment.water_depth_m
    xs = [x for x, _, _, _ in m.calls]
    speeds = [s for _, _, s, _ in m.calls]
    # 1) still water from the -2 % seed to zero offset, 2) calibration, 3) offset in 0.5 % steps, 4) current ramp
    assert xs[0] == pytest.approx(-0.02 * wd) and speeds[0] == 0.0
    i_cal = info["calibration"]["statics_calls"]
    assert all(s == 0.0 for s in speeds[: info["offset_steps_end"]])
    assert info["calibration"]["factor"] == pytest.approx(1 / 0.98, rel=1e-9)
    assert m.winches[3].t == [pytest.approx(100.0 / 0.98)] * 3  # every stage row scaled
    assert xs[-1] == pytest.approx(0.02 * wd)
    assert speeds[-4:] == pytest.approx([0.15, 0.3, 0.45, 0.6])
    assert m.environment.RefCurrentSpeed == 0.6
    assert info["current_ramp"] == [0.25, 0.5, 0.75, 1.0] and i_cal >= 1


def test_ramp_failure_is_statics_diverged_with_the_fraction_reached(base_spec, monkeypatch):
    c = _case(base_spec, heading_deg=0.0, offset_pct_wd=0.0, current={"depth_speed_m_s": [[0, 2.0], [300, 0.2]]})
    ad = cp.RiserCampaignAdapter()
    spec = ad.spec(c)
    target = cp.tensioner_vertical_target_n(spec)
    m = _FakeModel(lambda t: target * t / 100.0)
    orig = m.CalculateStatics

    def fail_at_full():
        if m.environment.RefCurrentSpeed > 1.6:
            raise RuntimeError("Static calculation failed (Whole system statics: Not converged.)")
        orig()

    m.CalculateStatics = fail_at_full
    monkeypatch.setattr(cp, "_tensioner_vertical_n", lambda model: model.vertical_of_tension(model.winches[0].t[-1]))
    with pytest.raises(pr.CaseFailed) as e:
        cp.robust_statics(m, spec, c["params"])
    assert e.value.status == "statics_diverged" and "0.75" in str(e.value)


class _Obj:
    def __init__(self, **res):
        self._res = res

    def StaticResult(self, name, *args):
        return self._res[name]


@pytest.mark.parametrize(("tv_factor", "wet_m3", "ok"), [
    (1.005, 0.0, True),     # physical deviation at an offset (tensioner lines inclined)
    (5682 / 6026, 0.0, False),  # the yawed branch
    (1.0, 1.2, True),       # the ring set down to MSL: its wetted volume is in the balance
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
def test_calibrated_line_tension_delivers_the_target_in_still_water(base_spec, tmp_path):
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    c = _case(base_spec, heading_deg=0.0, offset_pct_wd=0.0, top_tension_n=2.6e6)  # ring sits below z_static
    ad = cp.RiserCampaignAdapter()
    m = orun.load_model(ad.build(c, tmp_path / "m"))
    info = ad.statics(m, c)
    assert info["calibration"]["factor"] != pytest.approx(1.0, abs=1e-6)
    assert orun.tensioner_vertical_sum_n(m) == pytest.approx(2.6e6, rel=1e-6)
