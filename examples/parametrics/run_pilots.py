#!/usr/bin/env python3
"""Run the parametrics P0 pilots (one sweep per solver), all offline.

    uv run python examples/parametrics/run_pilots.py [--out /tmp/parametrics]

ANSYS padeye + OrcaWave depth sweeps PREPARE per-case inputs (the solve is the
licensed P1 step); the OrcaFlex FOWT watch-circle sweep runs the closed-form
check and reports real results. No licence required.
"""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.parametrics.pilots import (
    ansys_padeye_sweep,
    orcaflex_fowt_watch_circle_sweep,
    orcawave_depth_sweep,
)


def _print(summary) -> None:
    print(f"\n## {summary.study_name}  ({len(summary.results)} cases)")
    for r in summary.results:
        params = ", ".join(f"{k}={v}" for k, v in r.parameters.items())
        extra = ""
        if r.max_utilisation is not None:
            extra = f"  UC={r.max_utilisation}  margin={r.min_clearance_m}"
        print(f"  [{r.status:9}] {r.case_id}  {params}{extra}")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--out", type=Path, default=Path("/tmp/parametrics-p0"))
    args = ap.parse_args()

    _print(ansys_padeye_sweep(args.out / "ansys-padeye"))
    _print(orcawave_depth_sweep(args.out / "orcawave-depth"))
    _print(orcaflex_fowt_watch_circle_sweep())
    print(f"\nprepared inputs under {args.out}/  (solve licensed cases on the host)")


if __name__ == "__main__":
    main()
