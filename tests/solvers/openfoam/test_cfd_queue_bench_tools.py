from __future__ import annotations

import os
import subprocess
import time
from pathlib import Path


REPO = Path(__file__).resolve().parents[3]
CFD = REPO / "scripts" / "cfd"


def _wait_for(path: Path, timeout: float = 5.0) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if path.exists():
            return
        time.sleep(0.05)
    raise AssertionError(f"timed out waiting for {path}")


def _stub(path: Path, name: str, body: str = "exit 0") -> None:
    target = path / name
    target.write_text(f"#!/usr/bin/env bash\n{body}\n")
    target.chmod(0o755)


def test_queue_releases_when_marker_appears(tmp_path: Path) -> None:
    marker = tmp_path / "READY"
    subprocess.run(
        [
            "bash", str(CFD / "queue_after.sh"), "--root", str(tmp_path),
            "--wait", str(marker), "--no-solver", "definitelyNoSuchSolver",
            "--run", "printf released > released.txt", "--cwd", str(tmp_path),
            "--name", "next",
        ],
        check=True,
    )
    waiting = tmp_path / "status" / "QUEUE_next_WAITING"
    _wait_for(waiting)
    marker.touch()
    _wait_for(tmp_path / "status" / "QUEUE_next_LAUNCHED")
    _wait_for(tmp_path / "released.txt")
    launched = (tmp_path / "status" / "QUEUE_next_LAUNCHED").read_text()
    assert "pid=" in launched
    assert "QUEUE next LAUNCHED" in (tmp_path / "next.log").read_text()


def test_queue_cancel_kills_only_recorded_waiter(tmp_path: Path) -> None:
    marker = tmp_path / "NEVER"
    subprocess.run(
        [
            "bash", str(CFD / "queue_after.sh"), "--root", str(tmp_path),
            "--wait", str(marker), "--run", "touch should-not-run",
            "--cwd", str(tmp_path), "--name", "cancel-me",
        ], check=True,
    )
    waiting = tmp_path / "status" / "QUEUE_cancel-me_WAITING"
    _wait_for(waiting)
    pid = int(next(x.split("=", 1)[1] for x in waiting.read_text().split() if x.startswith("pid=")))
    assert Path(f"/proc/{pid}").exists()
    subprocess.run(
        ["bash", str(CFD / "queue_after.sh"), "cancel", "--root", str(tmp_path), "--name", "cancel-me"],
        check=True,
    )
    _wait_for(tmp_path / "status" / "QUEUE_cancel-me_CANCELLED")
    deadline = time.monotonic() + 3
    while Path(f"/proc/{pid}").exists() and time.monotonic() < deadline:
        time.sleep(0.05)
    assert not Path(f"/proc/{pid}").exists()
    assert not (tmp_path / "should-not-run").exists()


def test_bench_run_writes_documented_tsv_columns(tmp_path: Path) -> None:
    dest = tmp_path / "bench"
    variant = dest / "variants" / "base"
    (variant / "system").mkdir(parents=True)
    (variant / "constant" / "polyMesh").mkdir(parents=True)
    (variant / "constant" / "polyMesh" / "owner").write_text('note "nCells:100";\n')
    (variant / "system" / "controlDict").write_text("application interFoam;\n")
    (dest / "variants.tsv").write_text("base\t2\t\t1/2/0\tsymGaussSeidel\n")
    (dest / "BENCH_META").write_text("iterations=60\ninitial_time=0\n")
    bindir = tmp_path / "bin"
    bindir.mkdir()
    _stub(bindir, "mpirun", "shift 2\n\"$@\"")
    _stub(bindir, "pgrep", "exit 1")
    _stub(
        bindir,
        "interFoam",
        """for i in $(seq 1 60); do
  echo "Time = $i"
  echo "smoothSolver:  Solving for p_rgh, Initial residual = 1e-05, Final residual = 1e-08, No Iterations 2"
  echo "ExecutionTime = $i s  ClockTime = $i s"
done
mkdir -p postProcessing/forces/0 processor0/60
printf '0 ((0 0 0) (30 0 0) (70 0 0))\n' > postProcessing/forces/0/force.dat
""",
    )
    env = dict(os.environ, PATH=f"{bindir}:{os.environ['PATH']}", WM_BASHRC="/dev/null")
    subprocess.run(["bash", str(CFD / "bench_run.sh"), "--dest", str(dest)], env=env, check=True)
    rows = (dest / "bench_results.tsv").read_text().splitlines()
    assert rows[0].split("\t") == [
        "variant", "ranks", "iterations", "s_per_it", "us_per_cell_iteration",
        "s_per_it_1_50", "s_per_it_last_50", "Cd_last_quarter",
        "pressure_share_pct", "viscous_share_pct", "final_p_rgh_initial_residual",
    ]
    assert rows[1].split("\t")[:3] == ["base", "2", "60"]
    assert (dest / "BENCH_DONE").is_file()


def test_bench_prep_creates_one_state_per_rank_and_variants(tmp_path: Path) -> None:
    source = tmp_path / "source"
    for part in ("system", "constant/polyMesh", "0.orig"):
        (source / part).mkdir(parents=True)
    (source / "constant/polyMesh/owner").write_text('note "nCells:10";\n')
    (source / "system/controlDict").write_text("application interFoam;\n")
    (source / "system/decomposeParDict").write_text("numberOfSubdomains 1;\n")
    (source / "system/fvSolution").write_text("PIMPLE {}\nsolvers { p_rgh {} }\n")
    variants = tmp_path / "variants.txt"
    variants.write_text("base|2||1/2/0|symGaussSeidel\nbound|4|--bind-to core|2/3/1|DICGaussSeidel\n")
    bindir = tmp_path / "bin"
    bindir.mkdir()
    _stub(bindir, "rsync", "dst=${@: -1}; for src in \"${@:1:$#-1}\"; do [[ $src == -* ]] || cp -a \"$src\" \"$dst/\"; done")
    _stub(bindir, "foamDictionary")
    _stub(bindir, "setFields")
    _stub(bindir, "decomposePar", "mkdir -p processor0/0")
    _stub(bindir, "renumberMesh")
    dest = tmp_path / "bench"
    env = dict(os.environ, PATH=f"{bindir}:{os.environ['PATH']}", WM_BASHRC="/dev/null")
    subprocess.run(
        ["bash", str(CFD / "bench_prep.sh"), "--source", str(source), "--dest", str(dest),
         "--iterations", "60", "--ranks", "2,4", "--variants", str(variants)],
        env=env, check=True,
    )
    assert sorted(p.name for p in (dest / "states").iterdir()) == ["ranks-2", "ranks-4"]
    assert sorted(p.name for p in (dest / "variants").iterdir()) == ["base", "bound"]
    assert (dest / "PREP_DONE").is_file()


def test_lane_probe_header_documents_all_output_columns() -> None:
    result = subprocess.run(
        ["bash", str(CFD / "lane_probe.sh"), "--header"],
        capture_output=True, text=True, check=True,
    )
    columns = result.stdout.strip().split("|")
    assert len(columns) == 14
    assert columns[0:4] == ["lane", "case", "state", "iteration/end"]
    assert columns[-1] == "p_rgh residual"
