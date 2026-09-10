from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.matched_window_relative import analyse, main


PERIOD = 1_200.0
BASE = -220_000.0
FACTOR = 1.035


def _write_history(
    path: Path,
    *,
    start: int = 0,
    stop: int = 7_001,
    factor: float = 1.0,
    phase: float = 0.0,
    common: np.ndarray | None = None,
) -> Path:
    t = np.arange(start, stop, dtype=float)
    wobble = 65_000.0 * np.exp(-t / 8_000.0) * np.cos(
        2.0 * np.pi * t / PERIOD + phase
    )
    pressure = factor * (0.3 * BASE + wobble)
    viscous = np.full_like(t, factor * 0.7 * BASE)
    if common is not None:
        pressure = pressure + common[: len(t)]
    total = pressure + viscous
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as handle:
        handle.write("# synthetic OpenFOAM force history\n")
        for ti, ft, fp, fv in zip(t, total, pressure, viscous):
            handle.write(f"{ti:g} {ft:.12g} 0 0 {fp:.12g} 0 0 {fv:.12g} 0 0\n")
    return path


def test_known_factor_is_recovered_to_better_than_point_one_percent(tmp_path):
    b = _write_history(tmp_path / "b.dat")
    a = _write_history(tmp_path / "a.dat", factor=FACTOR)

    result = analyse(a, b, window=2_400, end=7_000, sweep=800, step=100)

    assert result["components"]["total"]["relative_pct"] == pytest.approx(
        (FACTOR - 1.0) * 100.0, abs=0.1
    )
    assert result["components"]["total"]["placements"] == 9


def test_common_decaying_transient_does_not_move_matched_relative(tmp_path):
    t = np.arange(7_001, dtype=float)
    common = 2_000_000.0 * np.exp(-t / 1_000_000.0) * np.cos(
        2.0 * np.pi * t / PERIOD
    )
    plain_a = _write_history(tmp_path / "plain-a.dat", factor=FACTOR)
    plain_b = _write_history(tmp_path / "plain-b.dat")
    transient_a = _write_history(tmp_path / "transient-a.dat", factor=FACTOR, common=common)
    transient_b = _write_history(tmp_path / "transient-b.dat", common=common)

    plain = analyse(plain_a, plain_b, window=2_400, end=7_000, sweep=800, step=100)
    transient = analyse(
        transient_a, transient_b, window=2_400, end=7_000, sweep=800, step=100
    )
    matched_delta = abs(
        transient["components"]["total"]["relative_pct"]
        - plain["components"]["total"]["relative_pct"]
    )
    last_quarter = t >= 6_700
    naive = (
        np.mean((FACTOR * BASE + common)[last_quarter])
        / np.mean((BASE + common)[last_quarter])
        - 1.0
    ) * 100.0

    assert matched_delta < 0.1
    assert abs(naive - (FACTOR - 1.0) * 100.0) > 2.0


def test_nearby_wobble_phases_have_stable_correlated_sweep(tmp_path):
    a = _write_history(tmp_path / "a.dat", factor=FACTOR, phase=0.08)
    b = _write_history(tmp_path / "b.dat", phase=0.0)

    row = analyse(a, b, window=2_400, end=7_000, sweep=800, step=100)[
        "components"
    ]["total"]

    assert row["relative_sd_pct"] < 0.15
    assert row["correlation"] > 0.8


def test_short_and_non_overlapping_histories_return_rc_2(tmp_path, capsys):
    short_a = _write_history(tmp_path / "short-a.dat", stop=1_000)
    short_b = _write_history(tmp_path / "short-b.dat", stop=1_000)
    assert main([str(short_a), str(short_b), "--window", "1200"]) == 2
    assert "shorter than" in capsys.readouterr().err

    early = _write_history(tmp_path / "early.dat", start=0, stop=2_000)
    late = _write_history(tmp_path / "late.dat", start=3_000, stop=5_000)
    assert main([str(early), str(late), "--window", "1200"]) == 2
    assert "overlap" in capsys.readouterr().err


def test_cli_all_components_labels_and_json(tmp_path, capsys):
    a = _write_history(tmp_path / "a.dat", factor=FACTOR)
    b = _write_history(tmp_path / "b.dat")
    output = tmp_path / "relative.json"

    assert main([
        str(a), str(b), "--window", "2400", "--component", "all",
        "--labels", "fine,coarse", "--json", str(output),
    ]) == 0
    result = json.loads(output.read_text())
    assert result["labels"] == ["fine", "coarse"]
    assert set(result["components"]) == {"total", "pressure", "viscous"}
    assert "fine/coarse" in capsys.readouterr().out
