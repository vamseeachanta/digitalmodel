"""Run one manifest-bound phase; the owning harness supplies process limits."""
import argparse
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from digitalmodel.solvers.smoke.model_probe import run_phase


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--phase", required=True, choices=("solve", "readback"))
    args = parser.parse_args()
    proof = run_phase(args.manifest, args.output, args.phase)
    print(json.dumps(proof, allow_nan=False))
    return 0 if proof["ok"] else 1


if __name__ == "__main__":
    sys.exit(main())
