#!/usr/bin/env python
"""Dispatch CFD work using benchmark manifests plus live SSH probes."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence


REPO_ROOT = Path(__file__).resolve().parents[2]
OPENFOAM_BASHRC = "/usr/lib/openfoam/openfoam2312/etc/bashrc"


@dataclass(frozen=True)
class HostConfig:
    name: str
    ssh_target: str
    manifest: Path
    dedicated: bool = False


@dataclass(frozen=True)
class HostProbe:
    host: str
    reachable: bool
    hostname: str
    cores: int
    load1: float
    reason: str


@dataclass(frozen=True)
class BenchmarkRow:
    ranks: int
    s_per_step: float
    cells_per_rank: int | None = None


@dataclass(frozen=True)
class BenchmarkProfile:
    box: str
    manifest: Path
    cells: int | None
    rows: tuple[BenchmarkRow, ...]


@dataclass(frozen=True)
class Candidate:
    host: HostConfig
    probe: HostProbe
    profile: BenchmarkProfile | None
    row: BenchmarkRow | None
    eligible: bool
    reason: str
    predicted_s_per_step: float | None


def default_hosts() -> list[HostConfig]:
    gpu_target = os.environ.get("CFD_GPU_CLAW_SSH", "gpu-claw")
    ace_target = os.environ.get("CFD_ACE_LINUX_2_SSH", "ace-linux-2")
    return [
        HostConfig(
            name="gpu-claw",
            ssh_target=gpu_target,
            manifest=REPO_ROOT / "docs/api/cfd/sloshing-3d-benchmark-gpu-claw.json",
            dedicated=True,
        ),
        HostConfig(
            name="ace-linux-2",
            ssh_target=ace_target,
            manifest=REPO_ROOT / "docs/api/cfd/sloshing-3d-benchmark.json",
            dedicated=False,
        ),
    ]


def _resolve_manifest(path: str) -> Path:
    p = Path(path)
    return p if p.is_absolute() else REPO_ROOT / p


def parse_host(value: str) -> HostConfig:
    """Parse name=ssh-target,manifest[,dedicated|shared]."""
    try:
        name, rest = value.split("=", 1)
        ssh_target, manifest, *flags = rest.split(",")
    except ValueError as exc:
        raise argparse.ArgumentTypeError(
            "--host must be name=ssh-target,manifest[,dedicated|shared]"
        ) from exc
    if not name or not ssh_target or not manifest:
        raise argparse.ArgumentTypeError(
            "--host must include non-empty name, ssh target, and manifest"
        )
    dedicated = bool(flags and flags[0].strip().lower() == "dedicated")
    return HostConfig(name, ssh_target, _resolve_manifest(manifest), dedicated)


def parse_assume_probe(values: Sequence[str]) -> dict[str, HostProbe]:
    """Parse repeated name=cores,load1[,hostname] test/offline probe overrides."""
    probes: dict[str, HostProbe] = {}
    for value in values:
        try:
            name, rest = value.split("=", 1)
            cores_s, load_s, *hostname = rest.split(",")
            cores = int(cores_s)
            load1 = float(load_s)
        except ValueError as exc:
            raise argparse.ArgumentTypeError(
                "--assume-probe must be name=cores,load1[,hostname]"
            ) from exc
        probes[name] = HostProbe(
            host=name,
            reachable=True,
            hostname=hostname[0] if hostname else name,
            cores=cores,
            load1=load1,
            reason="assumed",
        )
    return probes


def load_benchmark(path: Path) -> BenchmarkProfile:
    data = json.loads(path.read_text())
    rows: list[BenchmarkRow] = []
    for row in data.get("scaling", []):
        if row.get("status") != "completed" or row.get("s_per_step") is None:
            continue
        rows.append(
            BenchmarkRow(
                ranks=int(row["ranks"]),
                s_per_step=float(row["s_per_step"]),
                cells_per_rank=row.get("cells_per_rank"),
            )
        )
    return BenchmarkProfile(
        box=data.get("meta", {}).get("box", path.stem),
        manifest=path,
        cells=data.get("case", {}).get("cells"),
        rows=tuple(sorted(rows, key=lambda item: item.ranks)),
    )


def select_benchmark_row(
    profile: BenchmarkProfile, requested_ranks: int | None, cores: int
) -> BenchmarkRow | None:
    eligible = [row for row in profile.rows if row.ranks <= cores]
    if requested_ranks is not None:
        exact = [row for row in eligible if row.ranks == requested_ranks]
        return exact[0] if exact else None
    return min(eligible, key=lambda row: row.s_per_step, default=None)


def probe_host(
    host: HostConfig,
    *,
    timeout: int = 8,
    runner=subprocess.run,
) -> HostProbe:
    remote = (
        "printf 'hostname=%s\\n' \"$(hostname -s)\"; "
        "printf 'cores=%s\\n' \"$(nproc)\"; "
        "awk '{print \"load1=\"$1}' /proc/loadavg"
    )
    result = runner(
        [
            "ssh",
            "-o",
            "BatchMode=yes",
            "-o",
            f"ConnectTimeout={timeout}",
            host.ssh_target,
            remote,
        ],
        check=False,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        reason = (
            result.stderr or result.stdout or f"ssh rc={result.returncode}"
        ).strip()
        return HostProbe(host.name, False, "", 0, 0.0, reason)
    values: dict[str, str] = {}
    for line in result.stdout.splitlines():
        if "=" in line:
            key, val = line.split("=", 1)
            values[key.strip()] = val.strip()
    try:
        cores = int(values["cores"])
        load1 = float(values["load1"])
    except (KeyError, ValueError) as exc:
        return HostProbe(host.name, False, "", 0, 0.0, f"bad probe output: {exc}")
    return HostProbe(
        host=host.name,
        reachable=True,
        hostname=values.get("hostname", host.name),
        cores=cores,
        load1=load1,
        reason="ok",
    )


def evaluate_host(
    host: HostConfig,
    probe: HostProbe,
    profile: BenchmarkProfile | None,
    *,
    requested_ranks: int | None,
    max_load_per_core: float,
) -> Candidate:
    if not probe.reachable:
        return Candidate(host, probe, profile, None, False, probe.reason, None)
    if profile is None:
        return Candidate(host, probe, None, None, False, "missing benchmark", None)
    if probe.cores < 1:
        return Candidate(host, probe, profile, None, False, "invalid core count", None)
    row = select_benchmark_row(profile, requested_ranks, probe.cores)
    if row is None:
        rank_text = str(requested_ranks) if requested_ranks is not None else "any"
        return Candidate(
            host,
            probe,
            profile,
            None,
            False,
            f"no valid row for ranks={rank_text}",
            None,
        )
    projected_load_per_core = (probe.load1 + row.ranks) / probe.cores
    if max_load_per_core > 0 and projected_load_per_core > max_load_per_core:
        return Candidate(
            host,
            probe,
            profile,
            row,
            False,
            "projected load/core "
            f"{projected_load_per_core:.2f} exceeds {max_load_per_core:.2f}",
            row.s_per_step,
        )
    return Candidate(host, probe, profile, row, True, "ok", row.s_per_step)


def select_candidate(candidates: Sequence[Candidate]) -> Candidate:
    eligible = [candidate for candidate in candidates if candidate.eligible]
    if not eligible:
        raise RuntimeError("no eligible CFD host")
    return min(
        eligible, key=lambda candidate: candidate.predicted_s_per_step or float("inf")
    )


def _render_remote_command(args: Sequence[str], *, host: str, ranks: int) -> str:
    tokens = {"{host}": host, "{ranks}": str(ranks)}
    rendered = []
    for arg in args:
        for token, value in tokens.items():
            arg = arg.replace(token, value)
        rendered.append(arg)
    return " ".join(shlex.quote(arg) for arg in rendered)


def _quote_remote_path(path: str) -> str:
    if path == "~":
        return '"$HOME"'
    if path.startswith("~/"):
        return '"$HOME"/' + shlex.quote(path[2:])
    return shlex.quote(path)


def build_ssh_command(
    candidate: Candidate,
    *,
    remote_command: Sequence[str],
    repo_dir: str,
    openfoam_bashrc: str = OPENFOAM_BASHRC,
) -> list[str]:
    if candidate.row is None or candidate.predicted_s_per_step is None:
        raise ValueError("candidate must include a selected benchmark row")
    command = _render_remote_command(
        remote_command, host=candidate.host.name, ranks=candidate.row.ranks
    )
    execution_class = "dedicated" if candidate.host.dedicated else "shared-fallback"
    remote_shell = (
        'export PATH="$HOME/.npm-global/bin:$HOME/.local/bin:$PATH"; '
        f"source {shlex.quote(openfoam_bashrc)} || exit $?; "
        "set -e; "
        f"cd -- {_quote_remote_path(repo_dir)}; "
        f"export CFD_DISPATCH_HOST={shlex.quote(candidate.host.name)}; "
        f"export CFD_DISPATCH_RANKS={candidate.row.ranks}; "
        f"export CFD_EXECUTION_CLASS={execution_class}; "
        f"export CFD_DISPATCH_S_PER_STEP={candidate.predicted_s_per_step}; "
        f"{command}"
    )
    ssh_args = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=8"]
    return [*ssh_args, candidate.host.ssh_target, remote_shell]


def print_table(candidates: Sequence[Candidate], selected: Candidate | None) -> None:
    print("CFD dispatch candidates:")
    print(
        f"{'host':<14} {'ok':<3} {'cores':>5} {'load/core':>9} {'ranks':>5} {'s/step':>8}  reason"
    )
    for candidate in candidates:
        load = (
            candidate.probe.load1 / candidate.probe.cores
            if candidate.probe.cores
            else None
        )
        s_step = candidate.predicted_s_per_step
        print(
            f"{candidate.host.name:<14} {str(candidate.eligible):<3} "
            f"{candidate.probe.cores:>5} "
            f"{(f'{load:.2f}' if isinstance(load, float) else '-'):>9} "
            f"{(candidate.row.ranks if candidate.row else '-'):>5} "
            f"{(f'{s_step:.4f}' if isinstance(s_step, float) else '-'):>8}  "
            f"{candidate.reason}"
        )
    if selected:
        print(
            f"selected: {selected.host.name} "
            f"ranks={selected.row.ranks if selected.row else '-'} "
            f"s_per_step={selected.predicted_s_per_step}"
        )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", action="append", type=parse_host)
    parser.add_argument(
        "--assume-probe",
        action="append",
        default=[],
        help="name=cores,load1[,hostname]",
    )
    parser.add_argument(
        "--ranks",
        type=int,
        default=None,
        help="rank count to compare and dispatch",
    )
    parser.add_argument("--max-load-per-core", type=float, default=1.5)
    parser.add_argument("--repo-dir", default="~/digitalmodel")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("remote_command", nargs=argparse.REMAINDER)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    remote_command = list(args.remote_command)
    if remote_command and remote_command[0] == "--":
        remote_command = remote_command[1:]
    hosts = args.host or default_hosts()
    assumed = parse_assume_probe(args.assume_probe)
    candidates: list[Candidate] = []
    for host in hosts:
        probe = assumed.get(host.name) or probe_host(host)
        try:
            profile = load_benchmark(host.manifest)
        except OSError as exc:
            candidates.append(Candidate(host, probe, None, None, False, str(exc), None))
            continue
        candidates.append(
            evaluate_host(
                host,
                probe,
                profile,
                requested_ranks=args.ranks,
                max_load_per_core=args.max_load_per_core,
            )
        )
    try:
        selected = select_candidate(candidates)
    except RuntimeError as exc:
        print_table(candidates, None)
        print(f"dispatch failed: {exc}", file=sys.stderr)
        return 1
    print_table(candidates, selected)
    if not remote_command:
        return 0
    ssh_argv = build_ssh_command(
        selected, remote_command=remote_command, repo_dir=args.repo_dir
    )
    if args.dry_run:
        print("dry-run command:")
        print(" ".join(shlex.quote(part) for part in ssh_argv))
        return 0
    return subprocess.run(ssh_argv, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
