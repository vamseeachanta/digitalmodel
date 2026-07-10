from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/setup/dispatch-cfd-run.py"


def load_dispatcher():
    spec = importlib.util.spec_from_file_location("dispatch_cfd_run", SCRIPT)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def write_manifest(path: Path, *, box: str, rows: list[dict]) -> None:
    path.write_text(
        json.dumps(
            {
                "meta": {"box": box},
                "case": {"cells": 216000},
                "scaling": rows,
            }
        )
    )


def completed_row(ranks: int, s_per_step: float) -> dict:
    return {
        "ranks": ranks,
        "status": "completed",
        "s_per_step": s_per_step,
        "cells_per_rank": round(216000 / ranks),
    }


def test_load_benchmark_skips_oversubscribed_rows(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(
        manifest,
        box="gpu-claw",
        rows=[
            completed_row(4, 0.8821),
            completed_row(8, 0.5899),
            completed_row(16, 0.6945),
        ],
    )

    profile = dispatcher.load_benchmark(manifest)
    selected = dispatcher.select_benchmark_row(profile, requested_ranks=None, cores=8)

    assert selected.ranks == 8
    assert selected.s_per_step == 0.5899


def test_load_gate_rejects_busy_shared_host(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "ace.json"
    write_manifest(manifest, box="ace-linux-2", rows=[completed_row(8, 1.0582)])
    host = dispatcher.HostConfig(
        name="ace-linux-2",
        ssh_target="ace-linux-2",
        manifest=manifest,
        dedicated=False,
    )
    probe = dispatcher.HostProbe(
        host="ace-linux-2",
        reachable=True,
        hostname="ace-linux-2",
        cores=8,
        load1=24.0,
        reason="ok",
    )

    candidate = dispatcher.evaluate_host(
        host,
        probe,
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=1.5,
    )

    assert not candidate.eligible
    assert "load/core" in candidate.reason


def test_projected_load_gate_rejects_rank_that_would_overcommit_host(
    tmp_path: Path,
) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    host = dispatcher.HostConfig("gpu-claw", "gpu-claw", manifest, True)
    probe = dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 11.9, "ok")

    candidate = dispatcher.evaluate_host(
        host,
        probe,
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=1.5,
    )

    assert not candidate.eligible
    assert "projected load/core" in candidate.reason


def test_select_host_prefers_fastest_eligible_candidate(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    ace_manifest = tmp_path / "ace.json"
    gpu_manifest = tmp_path / "gpu.json"
    write_manifest(ace_manifest, box="ace-linux-2", rows=[completed_row(8, 1.0582)])
    write_manifest(gpu_manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    candidates = [
        dispatcher.evaluate_host(
            dispatcher.HostConfig("ace-linux-2", "ace-linux-2", ace_manifest, False),
            dispatcher.HostProbe("ace-linux-2", True, "ace-linux-2", 32, 2.0, "ok"),
            dispatcher.load_benchmark(ace_manifest),
            requested_ranks=8,
            max_load_per_core=1.5,
        ),
        dispatcher.evaluate_host(
            dispatcher.HostConfig("gpu-claw", "gpu-claw", gpu_manifest, True),
            dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 0.5, "ok"),
            dispatcher.load_benchmark(gpu_manifest),
            requested_ranks=8,
            max_load_per_core=1.5,
        ),
    ]

    selected = dispatcher.select_candidate(candidates)

    assert selected.host.name == "gpu-claw"
    assert selected.row.ranks == 8
    assert selected.predicted_s_per_step == 0.5899


def test_dispatch_dry_run_builds_openfoam_ssh_command(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    candidate = dispatcher.evaluate_host(
        dispatcher.HostConfig("gpu-claw", "gpu-claw", manifest, True),
        dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 0.5, "ok"),
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=1.5,
    )

    argv = dispatcher.build_ssh_command(
        candidate,
        remote_command=["scripts/setup/verify-cfd-box.sh", "--benchmark", "~/cfd_work"],
        repo_dir="~/digitalmodel",
    )

    assert argv[:4] == ["ssh", "-o", "BatchMode=yes", "-o"]
    assert "gpu-claw" in argv
    remote_shell = argv[-1]
    assert "source /usr/lib/openfoam/openfoam2312/etc/bashrc" in remote_shell
    assert 'cd -- "$HOME"/digitalmodel' in remote_shell
    assert "CFD_DISPATCH_RANKS=8" in remote_shell
    assert "scripts/setup/verify-cfd-box.sh --benchmark '~/cfd_work'" in remote_shell


def test_remote_command_preserves_non_dispatch_braces() -> None:
    dispatcher = load_dispatcher()

    rendered = dispatcher._render_remote_command(
        ["awk", "{print $1}", "run-on-{host}-np-{ranks}", '{"rank": 8}'],
        host="gpu-claw",
        ranks=8,
    )

    assert "'{print $1}'" in rendered
    assert "run-on-gpu-claw-np-8" in rendered
    assert "'{\"rank\": 8}'" in rendered


def test_build_ssh_command_quotes_repo_dir_with_spaces(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    candidate = dispatcher.evaluate_host(
        dispatcher.HostConfig("gpu-claw", "gpu-claw", manifest, True),
        dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 0.5, "ok"),
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=1.5,
    )

    argv = dispatcher.build_ssh_command(
        candidate,
        remote_command=["echo", "{host}"],
        repo_dir="~/digital model",
    )

    assert "cd -- \"$HOME\"/'digital model';" in argv[-1]


def test_default_hosts_use_neutral_gpu_claw_target(monkeypatch) -> None:
    dispatcher = load_dispatcher()
    monkeypatch.delenv("CFD_GPU_CLAW_SSH", raising=False)

    gpu_host = dispatcher.default_hosts()[0]

    assert gpu_host.name == "gpu-claw"
    assert gpu_host.ssh_target == "gpu-claw"


def test_cli_dry_run_does_not_invoke_ssh(tmp_path: Path) -> None:
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--host",
            f"gpu-claw=gpu-claw,{manifest},dedicated",
            "--assume-probe",
            "gpu-claw=8,0.5",
            "--ranks",
            "8",
            "--dry-run",
            "--",
            "echo",
            "run",
        ],
        check=False,
        text=True,
        capture_output=True,
    )

    assert result.returncode == 0, result.stderr
    assert "selected: gpu-claw" in result.stdout
    assert "ssh " in result.stdout
    assert "echo run" in result.stdout


def test_dispatch_default_threshold_is_fixed_at_one_point_five(
    tmp_path: Path,
) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    candidate = dispatcher.evaluate_host(
        dispatcher.HostConfig("gpu-claw", "gpu-claw", manifest, True),
        dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 0.5, "ok"),
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=dispatcher.build_parser().parse_args([]).max_load_per_core,
    )

    assert candidate.eligible
    assert dispatcher.build_parser().parse_args([]).max_load_per_core == 1.5


def test_dispatch_exports_selected_rank_for_smoke_driver(tmp_path: Path) -> None:
    dispatcher = load_dispatcher()
    manifest = tmp_path / "gpu.json"
    write_manifest(manifest, box="gpu-claw", rows=[completed_row(8, 0.5899)])
    candidate = dispatcher.evaluate_host(
        dispatcher.HostConfig("gpu-claw", "gpu-claw", manifest, True),
        dispatcher.HostProbe("gpu-claw", True, "gpu-claw", 8, 0.5, "ok"),
        dispatcher.load_benchmark(manifest),
        requested_ranks=8,
        max_load_per_core=1.5,
    )

    remote = dispatcher.build_ssh_command(
        candidate,
        remote_command=["python", "scripts/cfd/run_synthetic_tank_3d_smoke.py", "--ranks", "{ranks}"],
        repo_dir="~/digitalmodel",
    )[-1]

    assert "CFD_DISPATCH_RANKS=8" in remote
    assert "run_synthetic_tank_3d_smoke.py --ranks 8" in remote
