#!/usr/bin/env python
"""Solver baseline benchmark pack (#2300): run fixed cases, write a receipt.

Extends ``solver_smoke_test.py`` (can this host solve?) to a timed, repeatable
baseline (how fast, and does it still get the same answer?). Cases, variants
and tolerances live in ``digitalmodel.solvers.benchmark``.

Usage::

    python scripts/solver_benchmark.py run --machine-label ace-win-1 \\
        --solvers orcaflex orcawave aqwa mapdl --out <receipts-dir>
    python scripts/solver_benchmark.py run --machine-label ace-linux-1 \\
        --solvers openfoam --out <receipts-dir>
    python scripts/solver_benchmark.py compare new.json baseline.json

``run`` exits 0 only when every selected case completed every repeat with a
consistent fingerprint; ``compare`` exits 0 when every fingerprint matches (or
the solver version changed). The receipt carries a logical machine label, not
the hostname, and no licence-server addresses or user paths.

Run OrcaFlex/OrcaWave from an interactive or credentialed logon: their licence
checkout fails under an SSH public-key logon (see solver_smoke_test.py).
"""

from __future__ import annotations

import argparse
import contextlib
import datetime as dt
import io
import json
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from digitalmodel.solvers.benchmark import compare, runner  # noqa: E402
from digitalmodel.solvers.benchmark.solvers import CASES  # noqa: E402


def _run(args) -> int:
    cases = [CASES[name]() for name in args.solvers]
    for case in cases:
        if args.variants:
            case.variants = [v if v in ("all", "default") else int(v)
                             for v in args.variants]
    scratch = Path(args.work_dir or tempfile.mkdtemp(prefix="solver_bench_"))
    lock = Path(tempfile.gettempdir()) / "solver_benchmark.lock"
    try:
        from loguru import logger

        logger.remove()
    except Exception:
        pass
    try:
        with runner.pack_lock(lock), contextlib.redirect_stdout(io.StringIO()):
            receipt = runner.run_pack(
                cases, scratch, machine_label=args.machine_label,
                repeats=args.repeats, warmup=args.warmup,
                allow_busy=args.allow_busy, keep=args.keep)
    finally:
        if not args.keep and not args.work_dir:
            shutil.rmtree(scratch, ignore_errors=True)

    stamp = dt.datetime.now().strftime("%Y%m%dT%H%M%S")
    out_dir = Path(args.out) / args.machine_label
    out_dir.mkdir(parents=True, exist_ok=True)
    out = out_dir / f"benchmark-{'-'.join(args.solvers)}-{stamp}.json"
    out.write_text(json.dumps(receipt, indent=2) + "\n")

    for entry in receipt["results"]:
        solve = entry["solve_s"]["median"] if entry["solve_s"] else "-"
        state = "OK" if entry["baseline_eligible"] else "NOT ELIGIBLE"
        print(f"{entry['case']:>20} {entry['variant_label']}={entry['variant']!s:<8}"
              f" ok {entry['n_ok']}/{receipt['repeats']}  solve median {solve} s"
              f"  {state}  {'; '.join(entry['errors'])}{entry.get('skipped', '')}")
    print(f"receipt: {out}")
    print(f"RESULT: {'PASS' if receipt['ok'] else 'FAIL'}")
    return 0 if receipt["ok"] else 1


def _compare(args) -> int:
    new = json.loads(Path(args.new).read_text())
    base = json.loads(Path(args.baseline).read_text())
    result = compare.compare(new, base, slower_ratio=args.slower_ratio)
    for row in result["rows"]:
        print(f"{row['case']:>20} {row['variant']!s:<8} {row['fingerprint']:<16}"
              f"{row['timing']:<8}{row.get('time_ratio', '')}")
    print(f"RESULT: {'PASS' if result['ok'] else 'FAIL'}")
    return 0 if result["ok"] else 1


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = parser.add_subparsers(dest="command", required=True)

    run = sub.add_parser("run", help="run the pack and write a receipt")
    run.add_argument("--machine-label", required=True,
                     help="logical machine label (e.g. ace-win-1); never the hostname")
    run.add_argument("--solvers", nargs="+", choices=sorted(CASES), required=True)
    run.add_argument("--repeats", type=int, default=3)
    run.add_argument("--warmup", type=int, default=1,
                     help="untimed warm-up runs per variant (default 1)")
    run.add_argument("--variants", nargs="+",
                     help="override the case variants, e.g. 1 4 all")
    run.add_argument("--out", required=True, help="receipt directory")
    run.add_argument("--work-dir", help="scratch directory (default: temp)")
    run.add_argument("--keep", action="store_true", help="keep solver scratch output")
    run.add_argument("--allow-busy", action="store_true",
                     help="run on a busy host; results are not baseline-eligible")
    run.set_defaults(func=_run)

    cmp_ = sub.add_parser("compare", help="compare a receipt with a baseline")
    cmp_.add_argument("new")
    cmp_.add_argument("baseline")
    cmp_.add_argument("--slower-ratio", type=float, default=compare.SLOWER_RATIO)
    cmp_.set_defaults(func=_compare)

    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
