"""Rolling go/no-go decision tests (digitalmodel #1359)."""

import numpy as np

from digitalmodel.marine_ops.installation.go_no_go import DecisionState
from digitalmodel.motion_forecast.criteria import Criterion
from digitalmodel.motion_forecast.decision import rolling_decision
from digitalmodel.motion_forecast.models import MotionForecast


def _motion(t, **dofs):
    base = {d: np.zeros_like(t) for d in
            ("surge", "sway", "heave", "roll", "pitch", "yaw")}
    base.update(dofs)
    return MotionForecast(t=t, dof=base, origin_time=float(t[0]),
                          horizon=float(t[-1] - t[0]))


# governing = |vertical excursion at origin| = |heave|, so heave IS the series
_EXC = Criterion(key="test", label="T", governing="gangway_tip_excursion",
                 caution=1.0, limit=2.0, unit="m", alpha=1.0, basis="",
                 poi_offset=(0.0, 0.0, 0.0))
_VEL = Criterion(key="hd", label="Helideck", governing="helideck_heave_velocity",
                 caution=0.30, limit=0.40, unit="m/s", alpha=1.0, basis="",
                 poi_offset=(0.0, 0.0, 0.0))


def test_go_with_lead_time_to_breach():
    t = np.linspace(0, 10, 1001)
    m = _motion(t, heave=0.3 * t)  # crosses caution@3.333s, limit@6.667s
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.GO and d.display == "GO"
    assert abs(d.lead_time_to_caution - 1.0 / 0.3) < 0.02
    assert abs(d.lead_time_to_no_go - 2.0 / 0.3) < 0.02


def test_already_over_limit_is_no_go_zero_lead():
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=np.full_like(t, 2.5))
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.NO_GO
    assert d.lead_time_to_no_go == 0.0


def test_never_breaches_is_go_none_lead():
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=np.full_like(t, 0.5))  # below caution
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.GO
    assert d.lead_time_to_caution is None
    assert d.lead_time_to_no_go is None


def test_caution_band_now_is_marginal():
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=np.full_like(t, 1.5))  # caution<=1.5<limit
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.MARGINAL and d.display == "CAUTION"
    assert d.lead_time_to_caution == 0.0
    assert d.lead_time_to_no_go is None


def test_boundary_value_equal_limit_is_marginal():
    """value == limit -> MARGINAL (matches _check_criterion), lead_no_go None."""
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=np.full_like(t, 2.0))  # == limit
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.MARGINAL
    assert d.lead_time_to_no_go is None  # never strictly exceeds the limit


def test_boundary_value_equal_caution_is_go():
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=np.full_like(t, 1.0))  # == caution
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.GO
    assert d.lead_time_to_caution is None


def test_non_finite_governing_is_fail_closed_no_go():
    t = np.linspace(0, 10, 201)
    h = np.full_like(t, 0.5)
    h[100] = np.nan
    m = _motion(t, heave=h)
    d = rolling_decision(m, _EXC)
    assert d.state is DecisionState.NO_GO
    assert d.lead_time_to_no_go == 0.0


def test_downward_velocity_breach_is_no_go():
    """End-to-end sign guard: downward heave over the velocity limit -> NO-GO."""
    t = np.linspace(0, 10, 201)
    m = _motion(t, heave=-0.5 * t)  # |dz/dt| = 0.5 > 0.40 limit
    d = rolling_decision(m, _VEL)
    assert d.state is DecisionState.NO_GO
    assert np.all(np.array(d.values) >= 0.0)
