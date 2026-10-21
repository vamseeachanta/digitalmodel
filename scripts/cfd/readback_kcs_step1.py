# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read step 1 evidence offline or check admission; never run a CFD command."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from digitalmodel.solvers.openfoam.validation.kcs_step1 import (
    compare_run,
    compare_triplet,
    load_policy,
    load_reference,
    require_run_admission,
)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("compare", "triplet", "admit"))
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--criteria", type=Path, required=True)
    parser.add_argument("--history", type=Path, action="append", default=[])
    parser.add_argument("--metadata", type=Path, action="append", default=[])
    parser.add_argument("--next-mesh", choices=("coarse", "medium", "fine"))
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        expected = 3 if args.command == "triplet" else 1
        if args.command == "admit" and args.next_mesh == "coarse":
            expected = 0
        if len(args.history) != expected or len(args.metadata) != expected:
            raise ValueError(f"command requires {expected} history/metadata pairs")
        if args.command == "admit":
            if args.next_mesh is None:
                raise ValueError("admission requires --next-mesh")
            report = require_run_admission(args.next_mesh, args.reference, args.criteria,
                                           args.history[0] if expected else None,
                                           args.metadata[0] if expected else None)
            passed = report["admitted"]
        else:
            row, policy = load_reference(args.reference), load_policy(args.criteria)
            metadata = [json.loads(path.read_text(encoding="utf-8")) for path in args.metadata]
            if args.command == "triplet":
                report = compare_triplet(args.history, metadata, row, policy)
                passed = report["qualified"]
            else:
                report = compare_run(args.history[0], metadata[0], row, policy)
                passed = report["comparison_passed"]
    except (OSError, ValueError) as exc:
        reason = str(exc) if isinstance(exc, ValueError) else "required evidence file could not be read"
        report, passed = {"admitted": False, "qualified": False, "reason": reason}, False
    args.output.write_text(json.dumps(report, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    print("PASS" if passed else "FAIL")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
