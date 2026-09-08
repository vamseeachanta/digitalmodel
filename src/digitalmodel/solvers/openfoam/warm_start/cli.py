"""Command-line orchestration for the three warm-start safety layers."""
from __future__ import annotations

import argparse
import hashlib
import os
import re
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

import yaml

from .admissibility import evaluate
from .checks import cold_reference_statistics, evaluate_checkpoint, stop_and_fallback
from .decision import decide
from .fields import (KEEP_FIELDS, clean_restart, prepare_analytic, prepare_geometry,
                     prepare_potential, resharpen_alpha, reset_control,
                     restore_cold_restart, rewrite_speed_fields, run as run_foam,
                     identical_decomposition, verify_warm_fields)
from .record import RecordStore, append_ledger, timestamp


def _numeric_times(directory: Path) -> list[Path]:
    return ([p for p in directory.iterdir()
             if p.is_dir() and p.name.replace(".", "", 1).isdigit()]
            if directory.is_dir() else [])


def _latest(case: Path, target: Path | None = None) -> Path:
    serial = _numeric_times(case)
    processor_times = [time for processor in case.glob("processor[0-9]*")
                       for time in _numeric_times(processor)]
    if not serial and not processor_times:
        raise FileNotFoundError(f"no numeric source time under {case}")
    serial_latest = max(serial, key=lambda p: float(p.name)) if serial else None
    processor_latest = (max(processor_times, key=lambda p: float(p.name))
                        if processor_times else None)
    latest_value = max(float(path.name) for path in (*serial, *processor_times))
    if latest_value == 0:
        raise ValueError("latestTime resolves to 0; use --source-time 0 explicitly for a cold source")
    chosen = max((*serial, *processor_times), key=lambda path: float(path.name))
    serial_path = next((path for path in serial if float(path.name) == latest_value),
                       case / chosen.name)
    processor_is_newer = (processor_latest is not None and
                          (serial_latest is None or
                           float(processor_latest.name) > float(serial_latest.name)))
    if processor_is_newer:
        if target is not None and identical_decomposition(case, target):
            return serial_path
        run_foam(["reconstructPar", "-time", processor_latest.name, "-fields",
                  "(" + " ".join(KEEP_FIELDS) + ")"], case)
        serial_path = case / processor_latest.name
    return serial_path


def _warm_aware_relaunch(command: str, target: Path) -> bool:
    try:
        words = shlex.split(command)
    except ValueError:
        return False
    for word in words:
        path = Path(word)
        candidate = path if path.is_absolute() else target / path
        if candidate.is_file() and (candidate.suffix == ".sh" or candidate.parent == target):
            text = candidate.read_text(errors="ignore")
            if ("WARM_FIELDS" in text or
                    re.search(r"internalField\s+['\"*]*nonuniform", text) is not None):
                return True
    return False


def _write_warm_fields_marker(target: Path, source: Path | None,
                              source_time: Path | None, zeros: tuple[Path, ...]) -> None:
    fields = {}
    for zero in zeros:
        for name in KEEP_FIELDS:
            relative = str((zero / name).relative_to(target))
            fields[relative] = hashlib.sha256((zero / name).read_bytes()).hexdigest()
    data = {"source": str(source.resolve()) if source else None,
            "source_time": source_time.name if source_time else None,
            "sha256": fields}
    (target / "WARM_FIELDS").write_text(yaml.safe_dump(data, sort_keys=False))


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
    reference_dir = (Path(args.record) if args.record
                     else _campaign(target) / "warm_start")
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
        p.add_argument("--source-settled-override", metavar="REASON")
        p.add_argument("--n-cold", type=int)
        p.add_argument("--n-abort", type=int); p.add_argument("--checkpoint", type=int, default=400)
        p.add_argument("--max-du", type=float, default=.10); p.add_argument("--margin", type=float, default=.10)
        p.add_argument("--record", type=Path); p.add_argument("--ledger", type=Path)
        p.add_argument("--calibrate", action="store_true"); p.add_argument("--dry-run", action="store_true")
        p.add_argument("--relaunch"); p.add_argument("--rescale-u", action="store_true")
    check = sub.add_parser("check", aliases=["monitor"])
    check.add_argument("--target", type=Path, required=True); check.add_argument("--mesh-level", default="default")
    check.add_argument("--cold-ref", type=Path)
    check.add_argument("--n-cold", type=int); check.add_argument("--n-abort", type=int)
    check.add_argument("--checkpoint", type=int, default=400); check.add_argument("--hop", default="speed")
    check.add_argument("--record", type=Path); check.add_argument("--ledger", type=Path)
    check.add_argument("--relaunch"); check.add_argument("--pid", type=int)
    check.add_argument("--act", action="store_true"); check.add_argument("--fallback", action="store_true", help=argparse.SUPPRESS)
    rec = sub.add_parser("record")
    rec.add_argument("action", nargs="?", choices=("add-cold",))
    rec.add_argument("--record", type=Path); rec.add_argument("--case", type=Path)
    rec.add_argument("--hop", choices=("speed", "geometry", "potential", "analytic"))
    rec.add_argument("--mesh-level", default="default"); rec.add_argument("--n-cold", type=int, default=5000)
    rec.add_argument("--outcome", choices=("WARM_OK", "WARM_ABORTED", "WARM_FAILED_CAP")); rec.add_argument("--iterations", type=int)
    rec.add_argument("--target"); rec.add_argument("--source"); rec.add_argument("--reason"); rec.add_argument("--rebuild", action="store_true")
    return ap


def _ledger_values(args, decision, event, iterations="", reason=""):
    return {"target": args.target.name, "hop": decision.hop if decision else (args.hop or args.from_mode),
            "source": args.source.name if getattr(args, "source", None) else "",
            "level": args.mesh_level, "event": event, "p": decision.probability if decision else "",
            "n_warm_est": decision.n_warm_est if decision else "", "S": decision.saving if decision else "",
            "n_abort": decision.n_abort if decision else "", "EV": decision.ev if decision else "",
            "margin": decision.margin if decision else "",
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
                    source_time=args.source_time,
                    allow_pending_mesh=args.command == "plan" and args.dry_run)
    override = args.source_settled_override
    if override:
        if hop not in {"speed", "geometry"}:
            raise ValueError("--source-settled-override applies only to speed/geometry hops")
        gate = type(gate)(tuple(
            type(check)(check.identifier, True, override, "OVERRIDDEN")
            if check.identifier == "A1" else check for check in gate.checks
        ))
    print(gate.render())
    existing = store.load().get("hops", [])
    try:
        decision = decide(hop, n_cold, args.checkpoint, existing, n_abort=args.n_abort,
                          margin_fraction=args.margin, calibrate=args.calibrate) if gate.passed else None
    except ValueError as exc:
        reason = str(exc)
        if args.command == "run":
            (target / "COLD_FALLBACK").write_text(reason + "\n")
            if args.relaunch and not args.calibrate:
                restore_cold_restart(target)
                subprocess.Popen(args.relaunch, cwd=target, shell=True, start_new_session=True)
        print(f"warm_start: {reason}", file=sys.stderr)
        return 2
    if decision:
        block = decision.block(target.name, source.name if source else "-", args.mesh_level); print(block)
    else:
        block = f"warm_start plan target={target.name} hop={hop} -> COLD_BY_GATE {gate.first_failure}"
    ledger = args.ledger or _campaign(target) / "warm_start.tsv"
    if override and args.command != "plan" and not args.dry_run:
        append_ledger(ledger, _ledger_values(args, decision, "A1_OVERRIDDEN", reason=override))
        if not args.calibrate:
            (target / "COLD_FALLBACK").write_text(
                gate.render() + "\nsource-settled override requires --calibrate\n"
            )
            if args.command == "run" and args.relaunch:
                restore_cold_restart(target)
                subprocess.Popen(args.relaunch, cwd=target, shell=True, start_new_session=True)
                return 2
            return 3
    if args.command != "plan" and not args.dry_run:
        if not gate.passed:
            marker = gate.render() + "\n" + block + "\n"
            (target / "COLD_FALLBACK").write_text(marker)
            store.append({"id": f"{timestamp()}_{target.name}",
                          "source": source.name if source else None,
                          "target": target.name, "decision": "COLD_BY_GATE",
                          "ev": None, "outcome": "NOT_ATTEMPTED",
                          "iterations": None, "reason": gate.first_failure})
            append_ledger(ledger, _ledger_values(
                args, None, "WARM_NOT_ATTEMPTED", reason=gate.first_failure))
            if args.command == "run" and args.relaunch and not args.calibrate:
                restore_cold_restart(target)
                subprocess.Popen(args.relaunch, cwd=target, shell=True,
                                 start_new_session=True)
                return 2
            return 3 if args.calibrate else 2
        if decision.decision == "COLD_BY_EV":
            (target / "COLD_FALLBACK").write_text(block + "\n")
            if args.command == "run":
                if args.relaunch:
                    restore_cold_restart(target)
                    subprocess.Popen(args.relaunch, cwd=target, shell=True,
                                     start_new_session=True)
                return 2
            return 4
    if args.command == "plan":
        return 0 if gate.passed and decision.decision.startswith("WARM") else (3 if not gate.passed else 4)
    if args.dry_run:
        _print_commands(args, hop, n_cold)
        return 0 if gate.passed and decision.decision.startswith("WARM") else (3 if not gate.passed else 4)
    append_ledger(ledger, _ledger_values(args, decision,
                  "PLAN_WARM_CALIBRATION" if decision.decision == "WARM_CALIBRATION" else "PLAN_WARM",
                  reason=override or ""))
    command = args.relaunch or str(target / "solve_chain.sh")
    if args.command == "run" and not _warm_aware_relaunch(command, target):
        reason = "relaunch script resets 0/ from 0.orig; use a warm-aware chain"
        (target / "COLD_FALLBACK").write_text(reason + "\n")
        print(f"warm_start: {reason}", file=sys.stderr)
        return 3
    copied = False
    try:
        source_time = (_latest(source, target) if args.source_time == "latestTime" and source
                       else (source / args.source_time if source else None))
        zeros: tuple[Path, ...] = (target / "0",)
        if hop == "speed":
            copied = True
            zeros = clean_restart(source_time, target) or (target / "0",)
            rewrite_speed_fields(target)
        elif hop == "geometry":
            if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
            copied = True
            prepare_geometry(source, source_time.name, target, args.ranks)
            zeros = (target / "0",)
            if "flat_water_volume" in reference:
                resharpen_alpha(target / "0" / "alpha.water", float(reference["flat_water_volume"]))
        elif hop == "potential":
            if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
            copied = True
            prepare_potential(target)
            zeros = (target / "0",)
        elif hop == "analytic":
            if not args.eta or not args.u: raise ValueError("analytic mode requires --eta and --u")
            if not (target / "0.cold").exists(): shutil.copytree(target / "0", target / "0.cold")
            copied = True
            prepare_analytic(target, args.eta, args.u)
            zeros = (target / "0",)
        verify_warm_fields(zeros)
        _write_warm_fields_marker(target, source, source_time, zeros)
        reset_control(target, n_cold)
        marker = gate.render() + "\n" + block + "\n"
        if source:
            marker += f"source_path={source.resolve()}\n"
        (target / "WARM_PLANNED").write_text(marker)
        store.append({"id": f"{timestamp()}_{target.name}", "source": source.name if source else None,
                      "target": target.name, "decision": decision.decision, "ev": decision.__dict__,
                      "outcome": None, "iterations": None, "reason": override})
        if args.command == "run":
            (target / "WARM_RUNNING").write_text(marker)
            subprocess.Popen(command, cwd=target, shell=True, start_new_session=True)
        return 0
    except Exception as exc:
        if not copied:
            raise
        reason = f"warm prepare failed: {type(exc).__name__}: {exc}"
        try:
            restore_cold_restart(target)
            (target / "COLD_FALLBACK").write_text(reason + "\n")
            store.append({"id": f"{timestamp()}_{target.name}",
                          "source": source.name if source else None,
                          "target": target.name, "decision": decision.decision,
                          "ev": decision.__dict__, "outcome": "WARM_ABORTED",
                          "iterations": None, "reason": reason})
            append_ledger(ledger, _ledger_values(args, decision, "WARM_PREPARE_FAILED",
                                                 reason=reason))
            if args.relaunch and not args.calibrate:
                subprocess.Popen(args.relaunch, cwd=target, shell=True, start_new_session=True)
        except Exception as fallback_exc:
            print(f"warm_start: cold fallback also failed: {fallback_exc}", file=sys.stderr)
        print(f"warm_start: {reason}", file=sys.stderr)
        return 3 if args.calibrate else 2


def _print_commands(args, hop, n_cold):
    if hop == "speed":
        print(f"COMMAND: copy {' '.join(KEEP_FIELDS)} per rank for matching decompositions; "
              f"otherwise reconstruct all fields then copy {args.source}/<latest>/ -> {args.target}/0/")
        print("COMMAND: changeDictionary -time 0 [-parallel]")
        print(f"VERIFY: {' '.join(KEEP_FIELDS)} internalField nonuniform List; no macros")
    elif hop == "geometry":
        prefix = f"mpirun -np {args.ranks} " if args.ranks else ""
        suffix = " -parallel" if args.ranks else ""
        print(f"COMMAND: {prefix}mapFieldsPar {args.source} -sourceTime {args.source_time} -consistent -mapMethod cellVolumeWeight -fields '({' '.join(KEEP_FIELDS)})'{suffix}")
    elif hop == "potential": print("COMMAND: potentialFoam -writephi")
    else: print("COMMAND: postProcess -func writeCellCentres -time 0")
    print(f"CONTROL: startFrom startTime; startTime 0; endTime {n_cold}; stopAt endTime; runTimeModifiable true")


_COLD_KEYS = ("first_cycle_amplitude_pressure", "settled_viscous",
              "cold_settling_iteration")


def _source_case(target: Path) -> Path | None:
    for marker_name in ("WARM_RUNNING", "WARM_PLANNED"):
        marker = target / marker_name
        if not marker.exists():
            continue
        text = marker.read_text(errors="ignore")
        match = re.search(r"(?m)^source_path=(.+)$", text)
        if match:
            return Path(match.group(1).strip())
        match = re.search(r"\bsource=([^\s]+)", text)
        if match and match.group(1) != "-":
            name = match.group(1)
            for candidate in (target.parent / name, _campaign(target) / "cases" / name):
                if candidate.exists():
                    return candidate
    return None


def _checkpoint_reference(args, target: Path, recorded: dict, level_path: Path) -> dict:
    if args.cold_ref:
        return {**recorded, **cold_reference_statistics(args.cold_ref)}
    source = _source_case(target)
    if source is not None:
        try:
            return {**recorded, **cold_reference_statistics(source)}
        except (OSError, ValueError):
            pass
    if all(recorded.get(key) is not None for key in _COLD_KEYS):
        return recorded
    raise ValueError(
        "missing cold reference statistics; pass --cold-ref <case-or-force.dat>, "
        f"provide the warm source history, or create {level_path} with record add-cold"
    )


def check(args) -> int:
    target = args.target.resolve()
    recorded, record_dir = _reference(args, target)
    level_path = record_dir / f"level_{args.mesh_level}.yml"
    reference = _checkpoint_reference(args, target, recorded, level_path)
    n_cold = int(reference["n_cold"]); n_abort = args.n_abort or int((n_cold/3)//args.checkpoint*args.checkpoint)
    result = evaluate_checkpoint(target, reference, n_cold=n_cold, n_abort=n_abort,
                                 checkpoint=args.checkpoint, hop=args.hop)
    reason = result.reason or f"iteration={result.iteration}"
    shape_note = (f"; shape reference={args.cold_ref} (different condition at the same speed accepted)"
                  if args.cold_ref else "")
    print(f"{result.verdict} {reason}{shape_note}")
    acting = args.act or args.fallback
    if acting and result.verdict == "OK":
        (target / "WARM_OK").write_text(f"iterations={result.iteration}\n")
    elif acting and result.verdict == "ABORT":
        if result.reason == "cap":
            (target / "WARM_FAILED_CAP").write_text(
                f"{result.reason} iterations={result.iteration}\n"
            )
        stop_and_fallback(target, result.reason, args.relaunch, args.pid)
    return 3 if result.verdict == "ABORT" else 0


def record_command(args) -> int:
    if args.action == "add-cold":
        if not args.case:
            raise ValueError("record add-cold requires --case <case-or-force.dat>")
        record_dir = Path(args.record) if args.record else _campaign(args.case) / "warm_start"
        data = cold_reference_statistics(args.case)
        data["n_cold"] = args.n_cold
        path = record_dir / f"level_{args.mesh_level}.yml"
        if path.exists():
            data = {**(yaml.safe_load(path.read_text()) or {}), **data}
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(yaml.safe_dump(data, sort_keys=False))
        print(f"recorded cold reference {args.mesh_level} in {path}")
        return 0
    if not args.record:
        raise ValueError("record requires --record, or use record add-cold --case <case>")
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
