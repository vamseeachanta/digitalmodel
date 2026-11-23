#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retain synthetic native arrays and five-fixture parity without fitted values.

The analytical test module owns the independently derived acceptance equations.
This builder records only solver outputs, inputs, digests and execution order.
"""

import argparse
from dataclasses import asdict
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import compare_cards
from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    get_expected_downhole_card, load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    EverittJenningsContextSolver,
)

ROOT = Path(__file__).resolve().parents[2]
TEST = ROOT / "tests/marine_ops/artificial_lift/dynacard/test_solver_analytical.py"
DATA = TEST.parent / "testdata"
CRITERIA = DATA / "solver_guard_criteria.json"


def digest(path):
    """Hash bytes read back from an existing evidence file."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_json(path, value):
    """Use stable JSON bytes with no non-finite numerical values."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n")
    return digest(path)


def analytical_outputs():
    """Run the two native cases with the inputs defined by their tests."""
    spec = importlib.util.spec_from_file_location("solver_analytical_cases", TEST)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    results = {}
    for name in ("static", "harmonic"):
        inputs, _, _, _, _, card = module.native_case(name)
        results[name] = {"position_m": card.position.tolist(),
                         "load_n": card.load.tolist(), "stroke_m": card.stroke,
                         "inputs": inputs, "basis": "synthetic analytical boundary"}
    return results


def parity_outputs():
    """Measure each required stored reference card with the shared comparator."""
    results = []
    for number in range(1, 6):
        path = TEST.parent.parent / "test_data" / f"cleansed_well_{number:03d}.json"
        raw = json.loads(path.read_text())
        expected = get_expected_downhole_card(raw)
        if expected is None:
            raise ValueError(f"required reference card missing: {path.name}")
        actual = EverittJenningsContextSolver(load_from_json_file(path)).solve()
        results.append({"fixture": path.name, "input_sha256": digest(path),
                        **asdict(compare_cards(actual.downhole_card, expected))})
    load_errors = [record["load_nrmse_pct"] for record in results]
    criteria = json.loads(CRITERIA.read_text())
    median, maximum = float(np.median(load_errors)), float(np.max(load_errors))
    return {"cards": results, "median_load_nrmse_pct": median,
            "maximum_load_nrmse_pct": maximum,
            "median_margin_percentage_points": criteria["parity_median_load_nrmse_pct"] - median,
            "maximum_margin_percentage_points": criteria["parity_maximum_load_nrmse_pct"] - maximum}


def run_order(path):
    """Pin the unchanged criteria and first execution time before solving."""
    record = json.loads(path.read_text())
    if record["criteria_sha256"] != digest(CRITERIA):
        raise ValueError("criteria digest changed before computation")
    if "first_analytical_started_at" not in record:
        record["first_analytical_started_at"] = datetime.now(timezone.utc).isoformat()
        write_json(path, record)
    written = datetime.fromisoformat(record["criteria_written_at"])
    for key in ("first_comparison_started_at", "first_analytical_started_at"):
        if written >= datetime.fromisoformat(record[key]):
            raise ValueError("criteria were not pinned before execution")
    print(json.dumps(record, sort_keys=True), flush=True)
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--execution-record", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=DATA)
    parser.add_argument("--base-revision", required=True)
    args = parser.parse_args()
    order = run_order(args.execution_record)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    results = {"analytical": analytical_outputs(), "parity": parity_outputs(),
               "intended_use": "solver regression checks only; not field validation"}
    output = args.output_dir / "solver_guard_results.json"
    manifest = {**order, "base_revision": args.base_revision,
                "results_sha256": write_json(output, results),
                "source_sha256": {str(path.relative_to(ROOT)): digest(path)
                                  for path in (TEST, Path(__file__).resolve(),
                                               *(ROOT / "src/digitalmodel/marine_ops/artificial_lift/dynacard" / relative for relative in (
                                                   "everitt_jennings/solver.py", "everitt_jennings/adapter.py",
                                                   "everitt_jennings/damping.py", "everitt_jennings/units.py",
                                                   "solver_guards.py", "models.py", "comparison.py",
                                                   "data_loader.py", "constants.py")))}}
    write_json(args.output_dir / "solver_guard_manifest.json", manifest)
    print("Results SHA-256:", manifest["results_sha256"])
    print(json.dumps(results["parity"], sort_keys=True))


if __name__ == "__main__":
    main()
