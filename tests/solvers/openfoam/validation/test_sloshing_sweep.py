#!/usr/bin/env python3
"""
ABOUTME: Unit tests for the #641 fill/frequency sloshing sweep harness. All
tests are OpenFOAM-free: the 3x3 case matrix generation + params, the roll
moment ``forces`` functionObject emission (axis/origin), the moment parser on
synthetic ``moment.dat`` files, and the moment->amplitude/phase/in-phase/quad
reduction on a synthetic ``M(t) = C sin(wt + phi)`` of known amplitude/phase.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.sloshing_2d import (
    ROLL_MOMENT_FO_NAME,
    SloshingForcedRollConfig,
    build_forced_roll_case,
    parse_roll_moment,
    roll_moment_function_object,
)
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import (
    CONTRACT_FIELDS,
    SloshingSweep,
    SloshingSweepConfig,
    reduce_roll_moment,
)


# ---------------------------------------------------------------------------
# Case matrix generation (3 fills x 3 freqs = 9)
# ---------------------------------------------------------------------------


def test_matrix_is_three_by_three() -> None:
    cfg = SloshingSweepConfig()
    matrix = cfg.matrix()
    assert len(matrix) == 9
    fills = sorted({round(f, 6) for f, _ in matrix})
    periods = sorted({round(p, 6) for _, p in matrix})
    assert fills == [0.25, 0.50, 0.70]
    assert len(periods) == 3  # roll natural + 2 targets, all distinct


def test_matrix_covers_full_cartesian_product() -> None:
    cfg = SloshingSweepConfig(drive_periods=(1.9, 1.6, 2.2))
    matrix = cfg.matrix()
    expected = {(f, p) for f in (0.25, 0.50, 0.70) for p in (1.9, 1.6, 2.2)}
    assert {(round(f, 6), round(p, 6)) for f, p in matrix} == expected


def test_case_configs_carry_fill_and_period() -> None:
    cfg = SloshingSweepConfig(drive_periods=(1.9, 1.6, 2.2))
    sweep = SloshingSweep(cfg)
    configs = sweep.case_configs()
    assert len(configs) == 9
    for c in configs:
        assert isinstance(c, SloshingForcedRollConfig)
        # fill_depth = fill_level * tank_height, and drive period is imposed
        assert c.fill_level == pytest.approx(c.fill_depth / c.tank_height)
        assert c.drive_period in (1.9, 1.6, 2.2)
    # every case has a unique directory name
    assert len({c.name for c in configs}) == 9


def test_default_drive_periods_bracket_first_mode() -> None:
    cfg = SloshingSweepConfig()
    t_nat, t_lo, t_hi = cfg.default_drive_periods()
    assert t_lo < t_nat < t_hi
    assert t_lo == pytest.approx(0.85 * t_nat, rel=1e-6)
    assert t_hi == pytest.approx(1.15 * t_nat, rel=1e-6)


# ---------------------------------------------------------------------------
# Roll-moment forces functionObject emission (axis / origin)
# ---------------------------------------------------------------------------


def test_roll_moment_fo_has_forces_type_and_wall_patches() -> None:
    fo = roll_moment_function_object((0.45, 0.0, 0.0))
    assert "type            forces;" in fo
    assert ROLL_MOMENT_FO_NAME in fo
    for patch in ("leftWall", "rightWall", "lowerWall"):
        assert patch in fo
    # interFoam: use the rho field (dynamic pressure)
    assert "rho             rho;" in fo


def test_roll_moment_fo_cofr_is_roll_origin() -> None:
    fo = roll_moment_function_object((0.45, 0.0, 0.0))
    assert "CofR            (0.45 0 0);" in fo


def test_forced_roll_case_emits_moment_fo_at_floor_centre(tmp_path) -> None:
    config = SloshingForcedRollConfig()
    case_dir = build_forced_roll_case(config, tmp_path, with_moment=True)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "type            forces;" in control
    assert ROLL_MOMENT_FO_NAME in control
    # roll axis origin = centre of the tank floor = (L/2, 0, 0)
    ox, oy, oz = config.roll_origin
    assert oy == 0.0 and oz == 0.0
    assert f"CofR            ({ox:.8g} 0 0);" in control


def test_forced_roll_case_moment_fo_off_by_default(tmp_path) -> None:
    case_dir = build_forced_roll_case(SloshingForcedRollConfig(), tmp_path)
    control = (case_dir / "system" / "controlDict").read_text()
    assert "forces" not in control
    # the interfaceHeight probe is still present
    assert "interfaceHeight" in control


# ---------------------------------------------------------------------------
# Moment file parsing (modern + legacy layouts)
# ---------------------------------------------------------------------------


def _write_moment_dat(case_dir, body: str, t0: str = "0") -> None:
    d = case_dir / "postProcessing" / ROLL_MOMENT_FO_NAME / t0
    d.mkdir(parents=True, exist_ok=True)
    (d / "moment.dat").write_text(body)


def test_parse_moment_modern_total_first(tmp_path) -> None:
    # modern: time (total)(pressure)(viscous); total_z is the roll moment
    body = (
        "# Moment\n"
        "# CofR : (0.45 0 0)\n"
        "0.1 (1 2 3) (0.9 1.9 2.9) (0.1 0.1 0.1)\n"
        "0.2 (4 5 6) (3.9 4.9 5.9) (0.1 0.1 0.1)\n"
    )
    _write_moment_dat(tmp_path, body)
    times, mz = parse_roll_moment(tmp_path)
    assert times == [0.1, 0.2]
    assert mz == [3.0, 6.0]  # total_z


def test_parse_moment_legacy_pressure_plus_viscous(tmp_path) -> None:
    # legacy: time (pressure)(viscous); total_z = p_z + v_z
    body = (
        "# Time (pressure)(viscous)\n"
        "0.1 (0.9 1.9 2.5) (0.1 0.1 0.5)\n"
    )
    _write_moment_dat(tmp_path, body)
    times, mz = parse_roll_moment(tmp_path)
    assert times == [0.1]
    assert mz[0] == pytest.approx(3.0)  # 2.5 + 0.5


def test_parse_moment_missing_file_raises(tmp_path) -> None:
    with pytest.raises(FileNotFoundError):
        parse_roll_moment(tmp_path)


# ---------------------------------------------------------------------------
# Moment reduction on synthetic M(t) = C sin(wt + phi)
# ---------------------------------------------------------------------------


def _synthetic_moment(period, amplitude, phase, n_cycles=6, n_per_cycle=200):
    omega = 2.0 * math.pi / period
    t = np.linspace(0.0, n_cycles * period, n_cycles * n_per_cycle)
    m = amplitude * np.sin(omega * t + phase)
    return t, m


def test_reduction_recovers_amplitude_and_phase() -> None:
    period, C, phi = 1.9, 5.0, 0.3
    t, m = _synthetic_moment(period, C, phi)
    row = reduce_roll_moment(
        t, m, period, fill_level=0.25, roll_amplitude_deg=4.0
    )
    assert row["moment_amplitude"] == pytest.approx(C, rel=1e-3)
    assert row["moment_phase_rad"] == pytest.approx(phi, abs=1e-3)


def test_reduction_in_phase_quad_split() -> None:
    # M = C sin(wt + phi) => Ms = C cos phi, Mc = C sin phi
    # in_phase_coeff = -Ms = -C cos phi ; quad_coeff = -Mc = -C sin phi
    period, C, phi = 1.9, 5.0, 0.3
    t, m = _synthetic_moment(period, C, phi)
    row = reduce_roll_moment(
        t, m, period, fill_level=0.5, roll_amplitude_deg=4.0
    )
    assert row["in_phase_coeff"] == pytest.approx(-C * math.cos(phi), abs=1e-3)
    assert row["quad_coeff"] == pytest.approx(-C * math.sin(phi), abs=1e-3)


def test_reduction_pure_restoring_moment() -> None:
    # A restoring moment opposes displacement: M = -K sin(wt) (theta ~ sin).
    # => in_phase_coeff = +K > 0 (stiffness-like), quad_coeff ~ 0.
    period, K = 2.0, 7.0
    t, m = _synthetic_moment(period, K, math.pi)  # -K sin(wt)
    row = reduce_roll_moment(t, m, period, fill_level=0.5, roll_amplitude_deg=4.0)
    assert row["in_phase_coeff"] == pytest.approx(K, abs=1e-3)
    assert row["quad_coeff"] == pytest.approx(0.0, abs=1e-3)


def test_reduction_pure_damping_moment() -> None:
    # A damping moment opposes velocity: theta_dot ~ cos(wt), so M = -B cos(wt).
    # => quad_coeff = +B > 0 (dissipative), in_phase_coeff ~ 0.
    period, B = 2.0, 3.0
    omega = 2.0 * math.pi / period
    t = np.linspace(0.0, 6 * period, 1200)
    m = -B * np.cos(omega * t)
    row = reduce_roll_moment(t, m, period, fill_level=0.7, roll_amplitude_deg=4.0)
    assert row["quad_coeff"] == pytest.approx(B, abs=1e-3)
    assert row["in_phase_coeff"] == pytest.approx(0.0, abs=1e-3)


def test_reduction_emits_exact_contract_fields() -> None:
    period = 1.9
    t, m = _synthetic_moment(period, 5.0, 0.3)
    row = reduce_roll_moment(t, m, period, fill_level=0.25, roll_amplitude_deg=4.0)
    assert tuple(row.keys()) == CONTRACT_FIELDS
    assert row["fill_level"] == 0.25
    assert row["drive_period"] == period
    assert row["drive_freq_hz"] == pytest.approx(1.0 / period)
    assert row["roll_amplitude_deg"] == 4.0


def test_reduction_rejects_bad_inputs() -> None:
    t, m = _synthetic_moment(1.9, 5.0, 0.3)
    with pytest.raises(ValueError):
        reduce_roll_moment(t, m, 0.0, fill_level=0.25, roll_amplitude_deg=4.0)
    with pytest.raises(ValueError):
        reduce_roll_moment([0.0, 1.0], [1.0, 2.0], 1.9, fill_level=0.25,
                           roll_amplitude_deg=4.0)


# ---------------------------------------------------------------------------
# Sweep generation + collect (OpenFOAM-free: dirs + plan; skip unsolved cases)
# ---------------------------------------------------------------------------


def test_generate_writes_nine_cases_and_plan(tmp_path) -> None:
    sweep = SloshingSweep(SloshingSweepConfig(drive_periods=(1.9, 1.6, 2.2)))
    dirs = sweep.generate(tmp_path)
    assert len(dirs) == 9
    for d in dirs:
        assert (d / "system" / "controlDict").is_file()
        assert "forces" in (d / "system" / "controlDict").read_text()
    plan = tmp_path / "sweep_plan.json"
    assert plan.is_file()
    import json

    data = json.loads(plan.read_text())
    assert data["n_cases"] == 9
    assert data["contract_fields"] == list(CONTRACT_FIELDS)
    assert "sloshing_sweep run" in data["launch_command"]


def test_collect_skips_unsolved_and_reduces_solved(tmp_path) -> None:
    cfg = SloshingSweepConfig(drive_periods=(1.9, 1.6, 2.2))
    sweep = SloshingSweep(cfg)
    sweep.generate(tmp_path)
    # Fabricate a solved moment.dat for exactly one case.
    first = sweep.case_configs()[0]
    period = first.drive_period
    omega = 2.0 * math.pi / period
    lines = ["# Moment", "# CofR : (0.45 0 0)"]
    for i in range(600):
        t = i * (6 * period / 600)
        mz = 5.0 * math.sin(omega * t + 0.3)
        lines.append(f"{t:.6f} (0 0 {mz:.6f}) (0 0 0) (0 0 0)")
    _write_moment_dat(tmp_path / first.name, "\n".join(lines) + "\n")

    rows = sweep.collect(tmp_path)
    assert len(rows) == 1
    assert rows[0]["moment_amplitude"] == pytest.approx(5.0, rel=1e-2)
    assert (tmp_path / "sweep_manifest.json").is_file()
    assert (tmp_path / "sweep_manifest.csv").is_file()
