# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Benchmark reductions checked with independent arithmetic, without a solver."""

from __future__ import annotations

import importlib.util
import math
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[3]
SCRIPT = REPO / "scripts/cfd/lib/bench_metrics.py"
spec = importlib.util.spec_from_file_location("bench_metrics", SCRIPT)
metrics = importlib.util.module_from_spec(spec)
spec.loader.exec_module(metrics)


def write_log(case: Path, elapsed=(2, 5, 9, 14)) -> Path:
    """Synthetic cumulative CPU seconds: increments 2, 3, 4, 5."""
    case.mkdir(parents=True, exist_ok=True)
    log = case / "log.synthetic"
    log.write_text(
        "".join(
            f"Time = {i}\nExecutionTime = {cpu} s  ClockTime = {cpu} s\n"
            for i, cpu in enumerate(elapsed, start=1)
        )
    )
    return log


def owner(case: Path, cells: int, rank: int | None = None) -> None:
    """Write the nCells header contract used by mesh_store.mesh_cells."""
    root = case if rank is None else case / f"processor{rank}"
    mesh = root / "constant/polyMesh"
    mesh.mkdir(parents=True)
    (mesh / "owner").write_text(f'FoamFile {{ note "nCells:{cells}"; }}\n')


def data(case: Path, name: str, text: str, segment="0") -> Path:
    path = case / "postProcessing" / "synthetic" / segment / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    return path


def test_cpu_windows_and_serial_cell_count(tmp_path):
    """Hand calculation: 14/4=3.5 s/step; first 5/2=2.5;
    last (14-5)/2=4.5; 3.5*10^6/100=35000 microseconds/cell/step.
    Serial 100 and parallel 40+60 describe the same mesh, not 200 cells.
    """
    log = write_log(tmp_path)
    owner(tmp_path, 100)
    owner(tmp_path, 40, rank=0)
    owner(tmp_path, 60, rank=1)
    result, notes = metrics.reduce_metrics(tmp_path, log, 2, 2, 0.25)
    assert result[:5] == [4, 3.5, 35000, 2.5, 4.5]
    assert len(notes) == 3  # optional force, coefficient and residual absent


def test_parallel_cells_require_all_expected_ranks(tmp_path):
    """Hand calculation: 40+60=100 cells; an absent rank is unknown."""
    owner(tmp_path, 40, rank=0)
    assert metrics.cell_count(tmp_path, 2) is None
    owner(tmp_path, 60, rank=1)
    assert metrics.cell_count(tmp_path, 2) == 100
    owner(tmp_path, 999, rank=2)  # stale, outside the requested decomposition
    assert metrics.cell_count(tmp_path, 2) == 100


@pytest.mark.parametrize("count", [0, -1])
def test_invalid_cell_counts_are_unavailable(tmp_path, count):
    owner(tmp_path, count)
    assert metrics.cell_count(tmp_path, 1) is None


def test_short_and_single_step_windows(tmp_path):
    """One step of 2 seconds has a mean and both window means of 2."""
    result, _ = metrics.reduce_metrics(tmp_path, write_log(tmp_path, (2,)), 1, 50, 0.25)
    assert result[0:2] == [1, 2]
    assert result[3:5] == [2, 2]
    assert math.isnan(result[2])


@pytest.mark.parametrize(
    "text",
    [
        "",
        "Time = 1\n",
        "ExecutionTime = 2 s\n",
        "Time = 1\nExecutionTime = nan s\n",
        "Time = 1\nExecutionTime = -1 s\n",
        "Time = 1\nExecutionTime = 2 s\nTime = 2\nExecutionTime = 1 s\n",
        "Time = 1\nExecutionTime = 2 s\nTime = 2\n",
        "Time = 1\nExecutionTime = 2 s\nTime = 1\nExecutionTime = 3 s\n",
        "Time = 1\nExecutionTime = 2 s\nExecutionTime = 3 s\n",
        "Time = 1\nTime = 2\nExecutionTime = 3 s\n",
    ],
)
def test_incomplete_or_invalid_timing_refuses_a_result(tmp_path, text):
    log = tmp_path / "log.synthetic"
    log.write_text(text)
    with pytest.raises(ValueError):
        metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)


@pytest.mark.parametrize("parentheses", [False, True])
def test_force_headers_and_coefficient_selection(tmp_path, parentheses):
    """Hand calculation: last quarter of four rows selects row four.
    Pressure=30, viscous=20 and porous=50 give total=100, shares 30%,20%.
    The active coefficient order places Cd after Cl: expected Cd=0.4.
    v2312 forces.C writeIntegratedDataFile and forceCoeffs.C header methods
    define the layouts; numbers here are constructed arithmetic fixtures.
    """
    log = write_log(tmp_path)
    row = "100 0 0 30 0 0 20 0 0 50 0 0"
    if parentheses:
        row = "(100 0 0) (30 0 0) (20 0 0) (50 0 0)"
    data(
        tmp_path,
        "force.dat",
        "# Time total_x total_y total_z pressure_x pressure_y pressure_z "
        "viscous_x viscous_y viscous_z porous_x porous_y porous_z\n"
        + "".join(f"{i} {row}\n" for i in range(1, 5)),
    )
    data(
        tmp_path,
        "coefficient.dat",
        "# Time Cl Cd\n1 9 0.1\n2 9 0.2\n3 9 0.3\n4 9 0.4\n",
    )
    result, _ = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert result[5:8] == pytest.approx([0.4, 30, 20])


def test_force_tail_is_an_arithmetic_sample_mean(tmp_path):
    """With five samples, floor(5*(1-1/4))=3: use the last two.
    Mean total=(10+30)/2=20, mean pressure=(2+6)/2=4,
    mean viscous=(8+24)/2=16: shares=20% and 80%.
    """
    log = write_log(tmp_path)
    data(
        tmp_path,
        "force.dat",
        "# Time total_x pressure_x viscous_x\n"
        "1 100 100 0\n2 100 100 0\n3 100 100 0\n4 10 2 8\n5 30 6 24\n",
    )
    result, _ = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert result[6:8] == pytest.approx([20, 80])


@pytest.mark.parametrize(
    "text",
    [
        "1 100 30 70\n",  # no header: field order cannot be inferred
        "# Time total_x pressure_x viscous_x\n1 0 3 -3\n",  # undefined shares
        "# Time total_x pressure_x viscous_x\n1 nan 3 7\n",
        "# Time total_x pressure_x viscous_x\n1 10 3\n",  # truncated row
        "# Time total_x pressure_x viscous_x\n1 10 3 7\ninvalid\n",
        "# Time total_x pressure_x pressure_x viscous_x\n1 10 3 3 7\n",
    ],
)
def test_unknown_force_data_is_not_evaluated(tmp_path, text):
    log = write_log(tmp_path)
    data(tmp_path, "force.dat", text)
    result, notes = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert all(math.isnan(value) for value in result[6:8])
    assert any("force" in note for note in notes)


def test_multiple_segments_are_not_selected_arbitrarily(tmp_path):
    log = write_log(tmp_path)
    for segment in ("0", "2"):
        data(tmp_path, "coefficient.dat", "# Time Cd\n1 0.4\n", segment)
    result, notes = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert math.isnan(result[5])
    assert any("coefficient" in note for note in notes)


def test_final_pressure_residual(tmp_path):
    """The last p_rgh initial residual is 0.002, independent of U."""
    log = write_log(tmp_path)
    with log.open("a") as stream:
        stream.write(
            "smoothSolver: Solving for p_rgh, Initial residual = 0.01, Final residual = 0\n"
            "smoothSolver: Solving for Ux, Initial residual = 0.9, Final residual = 0\n"
            "smoothSolver: Solving for p_rgh, Initial residual = 2e-3, Final residual = 0\n"
        )
    result, _ = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert result[-1] == 0.002


@pytest.mark.parametrize(
    "ranks,window,fraction",
    [
        (0, 2, 0.25),
        (1, 0, 0.25),
        (1, 2, 0),
        (1, 2, 1.1),
        (1, 2, float("nan")),
    ],
)
def test_invalid_reduction_options(tmp_path, ranks, window, fraction):
    with pytest.raises(ValueError):
        metrics.reduce_metrics(tmp_path, write_log(tmp_path), ranks, window, fraction)


def test_cli_has_only_metrics_on_stdout(tmp_path):
    log = write_log(tmp_path)
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--case",
            str(tmp_path),
            "--log",
            str(log),
            "--ranks",
            "1",
            "--window",
            "2",
            "--tail-fraction",
            "0.25",
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    assert len(result.stdout.strip().split("\t")) == 9
    assert "NOT_EVALUATED" in result.stderr
    assert str(tmp_path) not in result.stderr


def _stub(bindir: Path, name: str, body: str) -> None:
    script = bindir / name
    script.write_text(f"#!/usr/bin/env bash\n{body}\n")
    script.chmod(0o755)


@pytest.mark.parametrize("complete", [True, False])
def test_benchmark_runner_uses_reducer(tmp_path, complete):
    """Hand calculation: CPU total 14/4=3.5, 100 cells => 35000 us.
    Short windows span all four steps => 3.5 each; last-quarter Cd=0.4.
    Final pressure=30 and viscous=70 with total=100 => shares 30%,70%.
    An incomplete final timing record must prevent BENCH_DONE.
    All executable stand-ins write constructed text and never run a solver.
    """
    import os

    dest = tmp_path / "bench"
    variant = dest / "variants/base"
    (variant / "system").mkdir(parents=True)
    (variant / "system/controlDict").write_text("application syntheticSolver;\n")
    owner(variant, 100)
    owner(variant, 40, rank=0)
    owner(variant, 60, rank=1)
    (dest / "variants.tsv").write_text(
        "name\tranks\tmpirun_flags\tcorrectors\tsmoother\nbase\t2\t\t1/2/0\tunused\n"
    )
    (dest / "BENCH_META").write_text("initial_time=0\n")
    data(variant, "force.dat", "# Time total_x pressure_x viscous_x\n1 100 30 70\n")
    data(variant, "coefficient.dat", "# Time Cl Cd\n1 99 0.4\n")
    log = write_log(tmp_path / "input")
    if not complete:
        with log.open("a") as stream:
            stream.write("Time = 5\n")
    bindir = tmp_path / "bin"
    bindir.mkdir()
    _stub(bindir, "pgrep", "exit 1")
    _stub(bindir, "mpirun", 'shift 2\n"$@"')
    _stub(bindir, "syntheticSolver", 'cat "$SYNTHETIC_LOG"')
    env = dict(
        os.environ,
        PATH=f"{bindir}:{os.environ['PATH']}",
        WM_BASHRC="/dev/null",
        SYNTHETIC_LOG=str(log),
    )
    result = subprocess.run(
        ["bash", str(REPO / "scripts/cfd/bench_run.sh"), "--dest", str(dest)],
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert (dest / "BENCH_DONE").exists() is complete
    if complete:
        assert result.returncode == 0, result.stderr
        values = (dest / "bench_results.tsv").read_text().splitlines()[1].split("\t")
        assert values[:3] == ["base", "2", "4"]
        assert [float(v) for v in values[3:10]] == pytest.approx(
            [3.5, 35000, 3.5, 3.5, 0.4, 30, 70]
        )
    else:
        assert result.returncode != 0
        assert "invalid or incomplete benchmark timing data" in result.stderr
        assert len((dest / "bench_results.tsv").read_text().splitlines()) == 1


def test_invalid_final_residual_does_not_invalidate_timing(tmp_path):
    """Valid CPU total 14/4=3.5 survives an unavailable final residual.
    The older valid residual must not be substituted for the final invalid one.
    """
    log = write_log(tmp_path)
    with log.open("a") as stream:
        stream.write(
            "solver: Solving for p_rgh, Initial residual = 0.01, Final residual = 0\n"
            "solver: Solving for p_rgh, Initial residual = nan, Final residual = 0\n"
        )
    result, notes = metrics.reduce_metrics(tmp_path, log, 1, 2, 0.25)
    assert result[:2] == [4, 3.5]
    assert math.isnan(result[-1])
    assert any("residual" in note for note in notes)
