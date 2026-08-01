#!/usr/bin/env python
"""End-to-end solver smoke test for licensed hosts (OrcaFlex + AQWA).

Unlike the readiness ``doctor`` commands (``orcawave-doctor``, ``openfoam
doctor``), which only report whether a binding imports and an executable
exists, this *actually solves* a tiny model on each solver. That is the only
check that proves the licence is reachable and can be checked out, which is the
failure mode that matters on a licensed run host.

The probes themselves live in ``digitalmodel.solvers.smoke.probes`` so this CLI
and the engine arm (``basename: solver_smoke_test``, used by the deckhand
licensed-run lane) run exactly the same checks.

Designed to be run unattended (scheduled task, SSH, deckhand preflight):
no prompts, machine-readable ``--json``, and an exit code that means
"this host can solve".

Usage::

    python scripts/solver_smoke_test.py                    # both solvers
    python scripts/solver_smoke_test.py --solver orcaflex
    python scripts/solver_smoke_test.py --json             # for automation

Exit codes: ``0`` every selected solver solved; ``1`` at least one failed.

Running this remotely over SSH
------------------------------
The Windows licensed host answers SSH with Git bash (MSYS), so use POSIX-style
paths -- ``D:\\ws\\...`` backslashes are escape characters there::

    ssh <user>@<licensed-host> \\
      '/d/ws/digitalmodel/.venv/Scripts/python.exe \\
       /d/ws/digitalmodel/scripts/solver_smoke_test.py --json'

Call the venv interpreter by absolute path; there is no shell activation and
no ``uv`` on PATH in a non-login SSH session.

**AQWA works over SSH; OrcaFlex does not.** ANSYS licensing is a plain TCP
checkout against the FlexNet server and is unaffected by session type. OrcaFlex
checkout fails under SSH *public-key* auth with "could not access the FlexNet
service. Error 21", even though the environment, the ``ORCINA_LICENSE_FILE``
registry value and TCP reachability to the licence server are byte-identical to
a working interactive session -- the key-auth logon token simply cannot
complete the checkout. Route remote OrcaFlex work through an executor that owns
a credentialed logon (the deckhand licensed-run lane's ``solver-smoke-test``
workflow, or a scheduled task) rather than running it in the SSH session.
"""

from __future__ import annotations

import argparse
import contextlib
import io
import json
import shutil
import sys
import tempfile
from pathlib import Path

# Support running straight from a checkout without the package installed.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from digitalmodel.solvers.smoke.probes import CHECKS, run_probes  # noqa: E402


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--solver",
        choices=[*CHECKS, "all"],
        default="all",
        help="which solver to smoke test (default: all)",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="where to write scratch solver output (default: a temp dir)",
    )
    parser.add_argument(
        "--keep",
        action="store_true",
        help="keep the scratch output instead of deleting it",
    )
    parser.add_argument(
        "--json", action="store_true", help="emit a machine-readable JSON report"
    )
    parser.add_argument(
        "--include-host",
        action="store_true",
        help="include this machine's name in the report (private deployment data)",
    )
    args = parser.parse_args(argv)

    selected = list(CHECKS) if args.solver == "all" else [args.solver]

    if args.output_dir:
        root = args.output_dir
        root.mkdir(parents=True, exist_ok=True)
        ephemeral = False
    else:
        root = Path(tempfile.mkdtemp(prefix="solver_smoke_"))
        ephemeral = not args.keep

    if args.json:
        # The diffraction stack logs heavily through loguru AND prints a
        # validation summary with bare print(); keep stdout clean so the JSON
        # document is the only thing on it.
        try:
            from loguru import logger

            logger.remove()
        except Exception:
            pass

    try:
        if args.json:
            with contextlib.redirect_stdout(io.StringIO()):
                report = run_probes(selected, root, include_host=args.include_host)
        else:
            report = run_probes(selected, root, include_host=args.include_host)
    finally:
        if ephemeral:
            shutil.rmtree(root, ignore_errors=True)

    if args.json:
        print(json.dumps(report, indent=2, default=str))
    else:
        print()
        for entry in report["results"]:
            print(f"{entry['solver']:>9}: {'PASS' if entry['ok'] else 'FAIL'}")
            for key, value in entry.items():
                if key in ("solver", "ok", "traceback"):
                    continue
                print(f"{'':>11}{key} = {value}")
        print(f"\nRESULT: {'PASS' if report['ok'] else 'FAIL'}")

    return 0 if report["ok"] else 1


if __name__ == "__main__":
    sys.exit(main())
