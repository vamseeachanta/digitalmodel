"""Tests for the swab/surge tripping-pressure calculator (issue #1036).

Closed-form / license-free. Physical-relationship and inversion checks plus an
offline engine smoke test through basename ``swab_surge``.
"""

from pathlib import Path

import pytest

from digitalmodel.engine import engine
from digitalmodel.well.drilling.swab_surge import SwabSurge

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = REPO_ROOT / "examples" / "workflows" / "swab-surge" / "input.yml"


def _calc(**overrides):
    base = dict(
        pipe_od=5.0,
        pipe_id=4.276,
        hole_diameter=8.5,
        mud_weight=12.0,
        tvd=12000.0,
        plastic_viscosity=20.0,
        yield_point=15.0,
    )
    base.update(overrides)
    return SwabSurge(**base)


def test_zero_trip_speed_gives_no_swab_or_surge():
    c = _calc()
    r = c.evaluate(0.0)
    assert r.pressure_psi == 0.0
    assert r.surge_emw_ppg == pytest.approx(c.mud_weight)
    assert r.swab_emw_ppg == pytest.approx(c.mud_weight)


def test_surge_raises_and_swab_lowers_symmetrically():
    c = _calc()
    r = c.evaluate(90.0)
    assert r.pressure_psi > 0
    # Surge above, swab below the static mud weight, symmetric about it.
    assert r.surge_emw_ppg > c.mud_weight > r.swab_emw_ppg
    assert (r.surge_emw_ppg - c.mud_weight) == pytest.approx(
        c.mud_weight - r.swab_emw_ppg
    )


def test_pressure_increases_with_trip_speed():
    c = _calc()
    assert c.pressure(120.0) > c.pressure(60.0) > c.pressure(30.0)


def test_open_pipe_displaces_less_than_closed():
    closed = _calc(pipe_config="closed")
    open_ = _calc(pipe_config="open")
    # An open-ended string displaces only the steel cross-section -> smaller
    # effective velocity and pressure than a closed (e.g. floated) string.
    assert open_.pressure(90.0) < closed.pressure(90.0)


def test_displacement_factor_closed_matches_area_ratio():
    c = _calc()
    expected = 5.0**2 / (8.5**2 - 5.0**2)
    assert c.displacement_factor() == pytest.approx(expected)


def test_evaluate_pressure_matches_bingham_friction_formula():
    c = _calc()
    v_e = c.effective_velocity(90.0)
    d_eq = c.hole_diameter - c.pipe_od
    expected = (
        c.plastic_viscosity * v_e / (239400.0 * d_eq**2)
        + c.yield_point / (200.0 * d_eq)
    ) * c.tvd
    assert c.pressure(90.0) == pytest.approx(expected)


def test_max_safe_trip_speed_keeps_emw_inside_window():
    c = _calc()
    pore, frac = 11.5, 13.0
    limit = c.max_safe_trip_speed(pore, frac)
    gov = limit.governing_ft_min
    assert gov > 0
    # At the governing safe speed, both swab and surge stay inside the window
    # (within rounding).
    r = c.evaluate(gov)
    assert r.swab_emw_ppg >= pore - 1e-6
    assert r.surge_emw_ppg <= frac + 1e-6
    # Just above it, the binding (swab) side is violated.
    r2 = c.evaluate(gov + 50.0)
    assert r2.swab_emw_ppg < pore


def test_zero_safe_speed_when_yield_alone_exceeds_margin():
    # A very tight window: the yield offset alone breaks it -> no safe nonzero speed.
    c = _calc()
    limit = c.max_safe_trip_speed(pore_pressure_emw_ppg=11.95, fracture_emw_ppg=12.05)
    assert limit.swab_limited_ft_min == 0.0


def test_invalid_geometry_raises():
    with pytest.raises(ValueError):
        _calc(hole_diameter=5.0)  # not greater than pipe_od
    with pytest.raises(ValueError):
        _calc(pipe_id=5.5)  # >= pipe_od
    with pytest.raises(ValueError):
        _calc(pipe_config="weird")


def test_swab_surge_workflow_offline(tmp_path):
    assert INPUT_YML.exists()
    cfg = engine(inputfile=str(INPUT_YML))
    result = cfg["swab_surge"]["result"]
    assert "at_trip_speed" in result and "safe_trip_speed" in result
    assert result["at_trip_speed"]["pressure_psi"] > 0
    # Example window is benign at 90 ft/min: no kick, no loss.
    assert result["kick_at_trip_speed"] is False
    assert result["loss_at_trip_speed"] is False
    assert result["safe_trip_speed"]["governing_ft_min"] > 0
