#!/usr/bin/env python3
"""
ABOUTME: Command-line surface for the fill / drive-frequency sloshing sweep
(#641): generate the case directories, run the solver over each in place, or
collect the reduced manifest. Kept out of the harness module so importing the
sweep contract does not pull in the runner.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional, Sequence

from .sloshing_sweep import SloshingSweep, SloshingSweepConfig

__all__ = ["main"]


def _run_all_cases(parent: Path, config: SloshingSweepConfig) -> None:
    """Run every generated case (blockMesh -> setFields -> interFoam) in place."""
    from ..runner import OpenFOAMRunConfig, OpenFOAMRunner

    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(run_set_fields=True, to_vtk=False)
    )
    sweep = SloshingSweep(config)
    for cfg in sweep.case_configs():
        case_dir = parent / cfg.name
        result = runner.run(case_dir)
        print(f"[{cfg.name}] {result.status.value}"
              + (f" ({result.error_message})" if result.error_message else ""))


def main(argv: Optional[Sequence[str]] = None) -> int:
    """CLI entry point: ``generate`` | ``run`` | ``collect``."""
    import argparse

    parser = argparse.ArgumentParser(
        prog="sloshing_sweep",
        description="Ballast-tank fill / frequency sloshing sweep (#641).",
    )
    parser.add_argument(
        "command", choices=("generate", "run", "collect"),
        help="generate case dirs, run the solver on each, or collect the manifest",
    )
    parser.add_argument(
        "--parent", type=Path, required=True,
        help="parent directory for the sweep case tree",
    )
    args = parser.parse_args(argv)

    config = SloshingSweepConfig()
    sweep = SloshingSweep(config)

    if args.command == "generate":
        dirs = sweep.generate(args.parent)
        print(f"Generated {len(dirs)} cases under {args.parent}")
        print("Launch the full sweep with:")
        print("  " + sweep.launch_command(args.parent))
        return 0

    if args.command == "run":
        _run_all_cases(args.parent, config)
        rows = sweep.collect(args.parent)
        print(f"Collected {len(rows)} contract rows -> "
              f"{args.parent / 'sweep_manifest.json'}")
        return 0

    # collect
    rows = sweep.collect(args.parent)
    print(f"Collected {len(rows)} contract rows -> "
          f"{args.parent / 'sweep_manifest.json'}")
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI dispatch
    raise SystemExit(main())
