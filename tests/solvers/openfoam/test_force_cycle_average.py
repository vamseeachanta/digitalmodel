from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.force_cycle_average import (
    MIN_SEP,
    analyse,
    central_value,
    envelope_trend,
    extrema,
    load_force,
    main,
    transient_end,
)

MEAN = -50_000.0
AMPLITUDE = 300_000.0
TAU = 800.0
PERIOD = 1_500.0
PHASE = 0.35
VISCOUS = -174_000.0


def test_transient_exclusion_removes_large_startup_bias():
    period = 1_000.0
    t = np.arange(5_001, dtype=float)
    signal = 100.0 + 800.0 * np.exp(-t / 500.0)
    signal += 10.0 * np.sin(2.0 * np.pi * t / period)

    old_mean = np.mean(signal[t >= t[-1] - 4 * period])
    result = central_value(t, signal, period, n_periods=4)

    assert abs(old_mean - 100.0) / 100.0 > 0.10
    assert result["window_start"] >= result["transient_end"]
    assert abs(result["mean"] - 100.0) <= result["standard_error"]


def test_central_window_is_independent_of_extrema_start(tmp_path):
    period = 1_000.0
    t = np.arange(7_001, dtype=float)
    pressure = 220_000.0 + 900_000.0 * np.exp(-t / 550.0)
    pressure += np.where(
        t < 3_000.0,
        200_000.0 * np.sin(2.0 * np.pi * t / 650.0),
        40_000.0 * np.sin(2.0 * np.pi * t / period),
    )
    path = tmp_path / "start-independent.dat"
    _write_series(path, t, pressure, pressure, np.zeros_like(t))

    early = analyse(path, start=100.0, smooth=25)
    late = analyse(path, start=3_500.0, smooth=25)

    _, early_extrema = extrema(t, pressure, start=100.0, smooth=25)
    _, late_extrema = extrema(t, pressure, start=3_500.0, smooth=25)
    early_period = 2.0 * np.median(np.diff([item[0] for item in early_extrema]))
    late_period = 2.0 * np.median(np.diff([item[0] for item in late_extrema]))
    assert early_period != pytest.approx(late_period)

    for key in ("window_start", "window_end", "mean", "transient_end"):
        assert early["central_value"]["total"][key] == pytest.approx(
            late["central_value"]["total"][key]
        )


def test_short_post_transient_history_reduces_periods_or_omits_error():
    t = np.arange(3_501, dtype=float)
    signal = 100.0 + np.sin(2.0 * np.pi * t / 1_000.0)

    reduced = central_value(t, signal, 1_000.0, n_periods=4, central_start=1_000.0)
    too_short = central_value(t, signal, 1_000.0, n_periods=4, central_start=2_200.0)

    assert reduced["n_periods_used"] == 2
    assert reduced["n_periods_requested"] == 4
    assert "reduced" in reduced["window_note"]
    assert reduced["standard_error"] is not None
    assert too_short["n_periods_used"] == 1
    assert too_short["standard_error"] is None
    assert "too short" in too_short["window_note"]


def test_no_transient_keeps_exact_final_period_window():
    period = 1_000.0
    t = np.arange(8_001, dtype=float)
    signal = 100.0 + 10.0 * np.sin(2.0 * np.pi * t / period)

    assert transient_end(t, signal, period) == pytest.approx(t[0])
    result = central_value(t, signal, period, n_periods=4)

    assert result["window_start"] == pytest.approx(t[-1] - 4 * period)
    assert result["n_periods_used"] == 4
    assert result["window_note"] is None


def test_central_value_decaying_cosine_recovers_mean_and_se_shrinks():
    t = np.arange(12 * int(PERIOD) + 1, dtype=float)
    signal = MEAN + AMPLITUDE * np.exp(-t / (100 * PERIOD)) * np.cos(
        2.0 * np.pi * t / PERIOD + PHASE
    )

    four = central_value(t, signal, PERIOD, n_periods=4)
    eight = central_value(t, signal, PERIOD, n_periods=8)

    assert abs(four["mean"] - MEAN) <= four["standard_error"]
    assert abs(eight["mean"] - MEAN) <= eight["standard_error"]
    assert eight["standard_error"] < four["standard_error"]


def test_sustained_oscillation_mean_is_precise_independently_of_settling(tmp_path):
    rows = 10_001
    path = tmp_path / "sustained.dat"
    t = np.arange(rows, dtype=float)
    pressure = MEAN + AMPLITUDE * np.cos(2.0 * np.pi * t / PERIOD + PHASE)
    viscous = np.full_like(t, VISCOUS)
    _write_series(path, t, pressure + viscous, pressure, viscous)

    result = analyse(path, start=0.0, smooth=25)
    central = result["central_value"]["total"]

    assert result["settling_verdict"] == "not settled"
    assert abs(central["mean"] - (MEAN + VISCOUS)) <= central["standard_error"]


def test_cli_reports_central_value_and_writes_component_json(tmp_path, capsys):
    force_file = write_force(tmp_path / "long.dat", rows=10_000)
    output = tmp_path / "result.json"

    assert main([str(force_file), "--start", "0", "--json", str(output)]) == 0

    stdout = capsys.readouterr().out
    data = json.loads(output.read_text())
    assert "central value (" in stdout
    assert "of 4 periods, transient ends" in stdout
    assert "REPORTABLE AS      :" in stdout
    assert set(data["central_value"]) == {
        "n_periods", "n_periods_requested", "n_periods_used", "transient_end",
        "window_note", "total", "pressure", "viscous",
    }
    for component in ("total", "pressure", "viscous"):
        assert set(data["central_value"][component]) == {
            "window_start", "window_end", "blocks", "periods", "mean", "standard_error",
            "relative_standard_error_pct", "note", "n_periods_used",
            "n_periods_requested", "transient_end", "window_note",
        }
    old_line_prefixes = [
        "pressure-force extrema", "last half period", "cycle latest",
        "cycle previous", "cycle earlier", "CYCLE POWER GATE", "envelope",
        "Aitken asymptote", "damped-cosine fit", "wobble <", "SETTLING VERDICT",
    ]
    positions = [stdout.index(prefix) for prefix in old_line_prefixes]
    assert positions == sorted(positions)


def test_cli_central_start_overrides_detected_transient(tmp_path, capsys):
    force_file = write_force(tmp_path / "override.dat", rows=10_000)
    output = tmp_path / "override.json"

    assert main([
        str(force_file), "--start", "0", "--central-start", "3000",
        "--json", str(output),
    ]) == 0

    data = json.loads(output.read_text())
    stdout = capsys.readouterr().out
    assert data["central_value"]["transient_end"] == 3_000.0
    assert "transient ends 3000" in stdout


def test_zero_central_value_is_strict_json_compatible():
    t = np.arange(4_001, dtype=float)
    result = central_value(t, np.zeros_like(t), 1_000)

    assert result["relative_standard_error_pct"] is None
    assert "zero central value" in result["note"]
    json.dumps(result, allow_nan=False)


def test_large_residual_oscillation_reports_large_relative_standard_error(tmp_path):
    rows = 10_001
    path = tmp_path / "large-residual.dat"
    t = np.arange(rows, dtype=float)
    pressure = 20_000.0 + 2_000_000.0 * np.exp(-t / (6.0 * PERIOD)) * np.cos(
        2.0 * np.pi * t / PERIOD + PHASE
    )
    viscous = np.zeros_like(t)
    _write_series(path, t, pressure, pressure, viscous)

    result = analyse(path, start=0.0, smooth=25)

    assert result["estimator_agreement_pct"] <= 2.0
    assert result["settling_verdict"] == "settled"
    assert result["central_value"]["total"]["relative_standard_error_pct"] > 10.0


def test_central_value_fewer_than_four_blocks_has_note():
    t = np.arange(1_001, dtype=float)
    result = central_value(t, np.cos(2 * np.pi * t / 1_000), 1_000, n_periods=1)

    assert result["blocks"] == 0
    assert result["periods"] == 1
    assert result["standard_error"] is None
    assert result["relative_standard_error_pct"] is None
    assert result["note"]


def _write_series(path, t, total, pressure, viscous):
    with path.open("w") as handle:
        handle.write("# OpenFOAM forces function object\n")
        for values in zip(t, total, pressure, viscous):
            ti, ft, fp, fv = values
            handle.write(f"{ti:g} {ft:.12g} 0 0 {fp:.12g} 0 0 {fv:.12g} 0 0\n")


def write_force(path: Path, rows: int = 4_000) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    t = np.arange(rows, dtype=float)
    pressure = MEAN + AMPLITUDE * np.exp(-t / TAU) * np.cos(
        2.0 * np.pi * t / PERIOD + PHASE
    )
    viscous = np.full_like(t, VISCOUS)
    total = pressure + viscous
    with path.open("w") as handle:
        handle.write("# OpenFOAM forces function object\n# Time total pressure viscous\n")
        for ti, ft, fp, fv in zip(t, total, pressure, viscous):
            handle.write(
                f"{ti:g} {ft:.12g} 0 0 {fp:.12g} 0 0 {fv:.12g} 0 0\n"
            )
    return path


@pytest.fixture
def force_file(tmp_path):
    return write_force(tmp_path / "force.dat")


def test_extrema_pruning_and_asymptotes(force_file):
    _, t, total, pressure, _ = load_force(force_file)
    _, ex = extrema(t, pressure, start=500.0, smooth=25)
    assert len(ex) >= 4
    assert all(b[0] - a[0] >= MIN_SEP for a, b in zip(ex, ex[1:]))
    assert all(a[2] != b[2] for a, b in zip(ex, ex[1:]))

    result = analyse(force_file, start=500.0, smooth=25)
    expected_total = MEAN + VISCOUS
    assert result["aitken_total"] == pytest.approx(expected_total, rel=0.02)
    assert result["fit_total"] == pytest.approx(expected_total, rel=0.02)
    assert result["fit"]["period"] == pytest.approx(PERIOD, rel=0.05)

    analytic = result["fit"]["t0"] + result["fit"]["tau"] * math.log(
        result["fit"]["amplitude"] / (0.01 * abs(result["fit_total"]))
    )
    assert result["iteration_amp_below_pct"] == pytest.approx(analytic)


def test_cycle_change_decreases_as_the_transient_decays(force_file):
    early = analyse(force_file, start=0.0, smooth=25)
    late = analyse(force_file, start=500.0, smooth=25)
    early_changes = [
        abs(a["total"] - b["total"]) / abs(b["total"]) * 100.0
        for a, b in zip(reversed(early["cycles"]), reversed(early["cycles"][:-1]))
    ]
    assert early_changes == sorted(early_changes, reverse=True)
    assert late["cycle_change_pct"] <= early_changes[0]


def test_short_history_reports_that_three_extrema_are_needed(tmp_path):
    result = analyse(write_force(tmp_path / "short.dat", rows=1_200), start=0, smooth=25)
    assert len(result["extrema"]) < 3
    assert "need 3" in result["note"]


@pytest.mark.parametrize("function_name", ["forces_hull", "forces"])
def test_load_force_accepts_case_directory_layout(tmp_path, function_name):
    expected = write_force(
        tmp_path / "postProcessing" / function_name / "0" / "force.dat", rows=10
    )
    loaded, t, total, pressure, viscous = load_force(tmp_path)
    assert loaded == expected
    assert len(t) == 10
    np.testing.assert_allclose(total, pressure + viscous)


@pytest.mark.parametrize(
    ("amplitudes", "expected"),
    [
        ([4.0, 2.0, 1.0], "decaying"),
        ([1.0, 2.0, 4.0], "rising"),
        ([2.0, 2.0, 2.0], "flat"),
    ],
)
def test_envelope_trend_classifies_period_scaled_cosines(amplitudes, expected):
    period = 800.0
    t = np.arange(6 * int(period), dtype=float)
    amplitude = np.repeat(amplitudes, 2 * int(period))
    pressure = amplitude * np.cos(2.0 * np.pi * t / period)

    windows, verdict = envelope_trend(t, pressure, period)

    assert len(windows) == 3
    assert verdict == expected


def test_envelope_windows_scale_with_each_case_period():
    results = []
    fixed_verdicts = []
    for period in (780.0, 1_270.0):
        t = np.arange(6 * int(period), dtype=float)
        pressure = 3.0 * np.exp(-t / (6.0 * period)) * np.cos(
            2.0 * np.pi * t / period
        )
        windows, verdict = envelope_trend(t, pressure, period)
        results.append((windows, verdict))
        fixed_ranges = [
            np.ptp(pressure[(t >= t[-1] - offset - 1_500) & (t <= t[-1] - offset)])
            for offset in (1_500, 0)
        ]
        fixed_ratio = fixed_ranges[-1] / fixed_ranges[-2]
        fixed_verdicts.append("decaying" if fixed_ratio < 0.8 else "flat")

    assert [verdict for _, verdict in results] == ["decaying", "decaying"]
    assert fixed_verdicts == ["decaying", "flat"]
    assert results[0][0][-1][1] - results[0][0][-1][0] == pytest.approx(2 * 780)
    assert results[1][0][-1][1] - results[1][0][-1][0] == pytest.approx(2 * 1_270)


def test_growing_envelope_overrides_passing_cycle_gate(tmp_path, capsys):
    path = tmp_path / "growing.dat"
    t = np.arange(7_201, dtype=float)
    pressure = (20_000.0 + 8.0 * t) * np.cos(2.0 * np.pi * t / 1_200.0)
    viscous = np.full_like(t, VISCOUS)
    with path.open("w") as handle:
        for ti, fp, fv in zip(t, pressure, viscous):
            handle.write(f"{ti:g} {fp + fv:.12g} 0 0 {fp:.12g} 0 0 {fv:.12g} 0 0\n")

    result = analyse(path, start=500, smooth=25, gate_pct=100)
    assert result["cycle_power_gate"] is True
    assert result["envelope_verdict"] == "rising"
    assert result["settling_verdict"] == "not settled"
    assert main([str(path), "--gate-pct", "100"]) == 0
    assert "SETTLING VERDICT    : not settled" in capsys.readouterr().out


def test_degenerate_fit_is_rejected_and_falls_back_to_cycle_agreement(tmp_path):
    """An unconstrained damped-cosine fit can return tau<0 and a period of a few
    iterations. Such a fit is numerical noise and must not veto two estimators that
    agree: the verdict falls back to Aitken vs the latest cycle average."""
    import numpy as np
    from digitalmodel.solvers.openfoam import force_cycle_average as fca

    it = np.arange(1, 8001, dtype=float)
    # decaying oscillation on a constant mean, plus a little high-frequency noise that
    # tempts the fit into a spurious short period
    rng = np.random.default_rng(0)
    pressure = 30e3 + 200e3 * np.exp(-it / 1200.0) * np.cos(2 * np.pi * it / 1100.0)
    pressure += 4e3 * np.sin(2 * np.pi * it / 9.0) + rng.normal(0, 2e3, it.size)
    viscous = np.full_like(it, 130e3)
    total = pressure + viscous
    p = tmp_path / "force.dat"
    with open(p, "w") as fh:
        fh.write("# Force\n")
        for i, tt in enumerate(it):
            fh.write(f"{tt} {total[i]} 0 0 {pressure[i]} 0 0 {viscous[i]} 0 0\n")
    r = fca.analyse(str(p), start=600)
    if not r.get("fit_usable", True):
        assert r["estimator_agreement_basis"].startswith("Aitken vs latest cycle")
        assert r["fit_reject_reason"]
        assert r["envelope_verdict"] in {"decaying", "flat"}
        assert r["settling_verdict"] == "settled"


def test_rising_envelope_still_blocks_settled_even_with_fit_fallback(tmp_path):
    import numpy as np
    from digitalmodel.solvers.openfoam import force_cycle_average as fca

    it = np.arange(1, 8001, dtype=float)
    pressure = 30e3 + 40e3 * np.exp(it / 4000.0) * np.cos(2 * np.pi * it / 1100.0)
    viscous = np.full_like(it, 130e3)
    total = pressure + viscous
    p = tmp_path / "force.dat"
    with open(p, "w") as fh:
        fh.write("# Force\n")
        for i, tt in enumerate(it):
            fh.write(f"{tt} {total[i]} 0 0 {pressure[i]} 0 0 {viscous[i]} 0 0\n")
    r = fca.analyse(str(p), start=600)
    assert r["envelope_verdict"] == "rising"
    assert r["settling_verdict"] == "not settled"
