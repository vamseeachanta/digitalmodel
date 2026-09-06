"""Command-line orchestration for the three warm-start safety layers."""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import yaml

from .admissibility import evaluate
from .checks import evaluate_checkpoint, stop_and_fallback
from .decision import decide
from .fields import (KEEP_FIELDS, clean_restart, prepare_analytic, prepare_geometry,
                     prepare_potential, resharpen_alpha, reset_control, rewrite_speed_fields)
from .record import RecordStore, append_ledger, timestamp


def _latest(case: Path) -> Path:
    times = [p for p in case.iterdir() if p.is_dir() and p.name.replace(".", "", 1).isdigit()]
    if not times:
        raise FileNotFoundError(f"no numeric source time under {case}")
    return max(times, key=lambda p: float(p.name))


def _campaign(target: Path) -> Path:
    configured_root = os.environ.get("DM_CFD_ROOT")
    if configured_root:
        root = Path(configured_root).expanduser().resolve()
        if not os.environ.get("DM_CFD_CAMPAIGN"):
            print(f"warm_start: resolved campaign {root.name} from DM_CFD_ROOT {root}", file=sys.stderr)
        return root
    configured = os.environ.get("DM_CFD_CAMPAIGN")
    if configured:
        path = Path(configured).expanduser()
        if path.is_absolute():
            return path
        for parent in (target, *target.parents):
            if parent.name == configured:
                return parent
        return Path.home() / "cfd" / configured
    cfd_home = (Path.home() / "cfd").resolve()
    cwd = Path.cwd().resolve()
    try:
        campaign = cwd.relative_to(cfd_home).parts[0]
        root, reason = cfd_home / campaign, f"working directory {cwd}"
    except (ValueError, IndexError):
        campaign, root = "campaign", cfd_home / "campaign"
        reason = f"literal fallback (working directory is outside {cfd_home})"
    print(f"warm_start: resolved campaign {campaign} from {reason}", file=sys.stderr)
    return root


def _reference(args, target: Path) -> tuple[dict, Path]:
    campaign = _campaign(target)
    reference_dir = Path(args.record) if args.record else campaign / "warm_start"
    path = reference_dir / f"level_{args.mesh_level}.yml"
    data = yaml.safe_load(path.read_text()) if path.exists() else {}
    if args.n_cold:
        data["n_cold"] = args.n_cold
    if "n_cold" not in data:
        raise ValueError(f"n_cold absent; pass --n-cold or create {path}")
    return data, reference_dir


def _normalise_legacy(argv: list[str]) -> list[str]:
    commands = {"plan", "prepare", "run", "monitor", "check", "record"}
    if argv and argv[0] not in commands and "--from" in argv:
        argv = ["prepare", *argv]
    # Requested shorthand: --from speed SOURCE / geometry SOURCE.
    if "--from" in argv:
        i = argv.index("--from")
        if i + 1 < len(argv) and argv[i+1] in {"speed", "geometry"}:
            hop = argv[i+1]
            if i + 2 >= len(argv) or argv[i+2].startswith("-"):
                raise ValueError(f"--from {hop} requires a source case")
            source = argv[i+2]
            argv = argv[:i] + ["--from", "case", "--hop", hop, "--source", source] + argv[i+3:]
    return argv


def parser() -> argparse.ArgumentParser:
    ap = argparse.ArgumentParser(prog="warm_start.py")
    sub = ap.add_subparsers(dest="command", required=True)
    for name in ("plan", "prepare", "run"):
        p = sub.add_parser(name)
        p.add_argument("--target", type=Path, required=True)
        p.add_argument("--from", dest="from_mode", choices=("case", "potential", "analytic", "cold"), required=True)
        p.add_argument("--source", type=Path); p.add_argument("--source-time", default="latestTime")
        p.add_argument("--hop", choices=("speed", "geometry")); p.add_argument("--eta", type=Path)
        p.add_argument("--u", type=Path); p.add_argument("--ranks", type=int)
        p.add_argument("--mesh-level", default="default"); p.add_argument("--source-mesh-level")
        p.add_argument("--n-cold", type=int)
        p.add_argument("--n-abort", type=int); p.add_argument("--checkpoint", type=int, default=400)
        p.add_argument("--max-du", type=float, default=.10); p.add_argument("--margin", type=float, default=.10)
        p.add_argument("--record", type=Path); p.add_argument("--ledger", type=Path)
        p.add_argument("--calibrate", action="store_true"); p.add_argument("--dry-run", action="store_true")
        p.add_argument("--relaunch"); p.add_argument("--rescale-u", action="store_true")
    check = sub.add_parser("check", aliases=["monitor"])
    check.add_argument("--target", type=Path, required=True); check.add_argument("--mesh-level", default="default")
    check.add_argument("--n-cold", type=int); check.add_argument("--n-abort", type=int)
    check.add_argument("--checkpoint", type=int, default=400); check.add_argument("--hop", default="speed")
    check.add_argument("--record", type=Path); check.add_argument("--ledger", type=Path)
    check.add_argument("--relaunch"); check.add_argument("--pid", type=int); check.add_argument("--fallback", action="store_true")
    rec = sub.add_parser("record")
    rec.add_argument("--record", type=Path, required=True); rec.add_argument("--hop", choices=("speed", "geometry", "potential", "analytic"))
    rec.add_argument("--mesh-level", default="default"); rec.add_argument("--n-cold", type=int, default=5000)
    rec.add_argument("--outcome", choices=("WARM_OK", "WARM_ABORTED", "WARM_FAILED_CAP")); rec.add_argument("--iterations", type=int)
    rec.add_argument("--target"); rec.add_argument("--source"); rec.add_argument("--reason"); rec.add_argument("--rebuild", action="store_true")
    return ap


def _ledger_values(args, decision, event, iterations="", reason=""):
    return {"target": args.target.name, "hop": decision.hop,
            "source": args.source.name if getattr(args, "source", None) else "",
            "level": args.mesh_level, "event": event, "p": decision.probability,
            "n_warm_est": decision.n_warm_est, "S": decision.saving,
            "n_abort": decision.n_abort, "EV": decision.ev, "margin": decision.margin,
            "iterations": iterations, "reason": reason}


def plan_or_prepare(args) -> int:
    target = args.target.resolve(); source = args.source.resolve() if args.source else None
    hop = args.hop or args.from_mode
    if hop == "case":
        raise ValueError("--hop speed|geometry is required with --from case")
    reference, record_dir = _reference(args, target)
    n_cold = int(reference["n_cold"])
    store = RecordStore(record_dir, hop, args.mesh_level, n_cold)
    gate = evaluate(source, target, hop, max_du=args.max_du, ranks=args.ranks,
                    level=args.mesh_level, source_level=args.source_mesh_level,
                    allow_pending_mesh=args.command == "plan" and args.dry_run)
    print(gate.render())
    existing = store.load().get("hops", [])
    decision = decide(hop, n_cold, args.checkpoint, existing, n_abort=args.n_abort,
                      margin_fraction=args.margin, calibrate=args.calibrate) if gate.passed else None
    if decision:
        block = decision.block(target.name, source.name if source else "-", args.mesh_level); print(block)
    else:
        block = f"warm_start plan target={target.name} hop={hop} -> COLD_BY_GATE {gate.first_failure}"
    ledger = args.ledger or _campaign(target) / "warm_start.tsv"
    if args.command != "plan" and not args.dry_run:
        if not gate.passed:
            (target / "COLD_FALLBACK").write_text(block + "\n")
            return 3
        if decision.decision == "COLD_BY_EV":
            (target / "COLD_FALLBACK").write_text(block + "\n")
            return 4
    if args.command == "plan":
        return 0 if gate.passed and decision.decision.startswith("WARM") else (3 if not gate.passed else 4)
    if args.dry_run:
        _print_commands(args, hop, n_cold)
        return 0 if gate.passed and decision.decision.startswith("WARM") else (3 if not gate.passed else 4)
    append_ledger(ledger, _ledger_values(args, decision,
                  "PLAN_WARM_CALIBRATION" if decision.decision == "WARM_CALIBRATION" else "PLAN_WARM"))
    source_time = _latest(source) if args.source_time == "latestTime" and source else (source / args.source_time if source else None)
    if hop == "speed":
        clean_restart(source_time, target); rewrite_speed_fields(target)
    elif hop == "geometry":
        if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
        prepare_geometry(source, source_time.name, target, args.ranks)
        if "flat_water_volume" in reference:
            resharpen_alpha(target / "0" / "alpha.water", float(reference["flat_water_volume"]))
    elif hop == "potential":
        if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
        prepare_potential(target)
    elif hop == "analytic":
        if not args.eta or not args.u: raise ValueError("analytic mode requires --eta and --u")
        if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
        prepare_analytic(target, args.eta, args.u)
    reset_control(target, n_cold)
    (target / "WARM_PLANNED").write_text(block + "\n")
    store.append({"id": f"{timestamp()}_{target.name}", "source": source.name if source else None,
                  "target": target.name, "decision": decision.decision, "ev": decision.__dict__,
                  "outcome": None, "iterations": None, "reason": None})
    if args.command == "run":
        (target / "WARM_RUNNING").write_text(block + "\n")
        command = args.relaunch or str(target / "solve_chain.sh")
        subprocess.Popen(command, cwd=target, shell=True, start_new_session=True)
    return 0


def _print_commands(args, hop, n_cold):
    if hop == "speed":
        print(f"COMMAND: copy {' '.join(KEEP_FIELDS)} {args.source}/<latest>/ -> {args.target}/0/")
        print("COMMAND: changeDictionary -time 0")
    elif hop == "geometry":
        prefix = f"mpirun -np {args.ranks} " if args.ranks else ""
        suffix = " -parallel" if args.ranks else ""
        print(f"COMMAND: {prefix}mapFieldsPar {args.source} -sourceTime {args.source_time} -consistent -mapMethod cellVolumeWeight -fields '({' '.join(KEEP_FIELDS)})'{suffix}")
    elif hop == "potential": print("COMMAND: potentialFoam -writephi")
    else: print("COMMAND: postProcess -func writeCellCentres -time 0")
    print(f"CONTROL: startFrom startTime; startTime 0; endTime {n_cold}; stopAt endTime; runTimeModifiable true")


def check(args) -> int:
    target = args.target.resolve(); reference, record_dir = _reference(args, target)
    n_cold = int(reference["n_cold"]); n_abort = args.n_abort or int((n_cold/3)//args.checkpoint*args.checkpoint)
    result = evaluate_checkpoint(target, reference, n_cold=n_cold, n_abort=n_abort,
                                 checkpoint=args.checkpoint, hop=args.hop)
    print(f"{result.verdict}: iteration={result.iteration} reason={result.reason or '-'}")
    for detail in result.details: print(" ", detail)
    if result.verdict == "OK": (target / "WARM_OK").write_text(f"iterations={result.iteration}\n")
    elif result.verdict == "ABORT":
        marker = "WARM_FAILED_CAP" if result.reason == "cap" else "WARM_ABORTED"
        (target / marker).write_text(f"{result.reason} iterations={result.iteration}\n")
        if args.fallback and result.reason != "cap": stop_and_fallback(target, result.reason, args.relaunch, args.pid)
    return 5 if result.verdict == "ABORT" else 0


def record_command(args) -> int:
    if not args.hop:
        for path in sorted(args.record.glob("record_*.yml")): print(path.read_text(), end="")
        return 0
    store = RecordStore(args.record, args.hop, args.mesh_level, args.n_cold)
    if args.outcome:
        store.append({"id": f"{timestamp()}_{args.target or 'manual'}", "source": args.source,
                      "target": args.target, "decision": "WARM", "outcome": args.outcome,
                      "iterations": args.iterations, "reason": args.reason})
    print(yaml.safe_dump(store.load(), sort_keys=False), end="")
    return 0


def main(argv=None) -> int:
    try:
        args = parser().parse_args(_normalise_legacy(list(argv if argv is not None else sys.argv[1:])))
        if args.command in {"check", "monitor"}: return check(args)
        if args.command == "record": return record_command(args)
        return plan_or_prepare(args)
    except (OSError, ValueError, KeyError, yaml.YAMLError) as exc:
        print(f"warm_start: {exc}", file=sys.stderr); return 2
