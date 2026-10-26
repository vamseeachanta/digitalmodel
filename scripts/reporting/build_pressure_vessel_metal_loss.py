# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate reproducible cylindrical-shell chart CSVs and their provenance."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.asset_integrity.pressure_vessel_metal_loss import (
    ShellInputs,
    evaluate_shell_loss,
)

WORKFLOW = Path("examples/workflows/pressure-vessel-metal-loss")
ASSUMED = "ASSUMED for this illustrative calculation; not measured or material-qualified"
SOURCES = (
    WORKFLOW / "input.json",
    Path("src/digitalmodel/asset_integrity/pressure_vessel_metal_loss.py"),
    Path("src/digitalmodel/ansys/pressure_vessel.py"),
    Path("scripts/reporting/build_pressure_vessel_metal_loss.py"),
)
CHARTS = {
    "membrane-stress.csv": (
        "metal_loss_mm", "hoop_stress_mpa", "longitudinal_stress_mpa",
    ),
    "remaining-thickness.csv": (
        "metal_loss_mm", "remaining_thickness_mm",
        "implementation_required_thickness_mm", "thickness_difference_mm",
    ),
}


def build(root: Path, output: Path) -> tuple[Path, ...]:
    """Write chart data, rounding once to 0.1 of each column's stated unit.

    Manifest rows store the exact chart strings, including signed differences
    calculated before rounding. Hashes identify the input, calculation sources
    and output bytes. Output contains no timestamps or local filesystem paths.
    """
    config = json.loads((root / WORKFLOW / "input.json").read_text(encoding="utf-8"))
    if config["basis"] != ASSUMED:
        raise ValueError("Inputs must carry the explicit ASSUMED basis")
    inputs = ShellInputs(**config["shell"])
    losses = config["metal_loss_mm"]
    if not isinstance(losses, list) or not losses:
        raise ValueError("metal_loss_mm must be a nonempty list")
    results = [asdict(evaluate_shell_loss(inputs, loss)) for loss in losses]
    if any(a >= b for a, b in zip(losses, losses[1:])):
        raise ValueError("metal_loss_mm must be strictly increasing")
    manifest: dict[str, Any] = {
        "basis": ASSUMED,
        "inputs": config,
        "membrane_basis": "First-principles force equilibrium; calculation module docstring",
        "thickness_basis": "the result of our implementation; zero corrosion allowance",
        "acceptance_criteria": "DATA GAP; no assessment level or service verdict",
        "geometry_basis": "Uniform external loss; fixed inner radius; closed ends; thin shell",
        "chart_precision": "0.1 mm and 0.1 MPa; differences computed before rounding",
        "source_sha256": {
            path.as_posix(): hashlib.sha256((root / path).read_bytes()).hexdigest()
            for path in SOURCES
        },
        "charts": {},
    }
    output.mkdir(parents=True, exist_ok=True)
    paths = []
    for filename, fields in CHARTS.items():
        rows = [{field: f"{result[field]:.1f}" for field in fields} for result in results]
        path = output / filename
        with path.open("w", encoding="utf-8", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
        manifest["charts"][filename] = {
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            "rows": rows,
        }
        paths.append(path)
    path = output / "manifest.json"
    path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return (*paths, path)


def main() -> None:
    """Build the example in an explicit output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    build(Path(__file__).resolve().parents[2], args.output)


if __name__ == "__main__":
    main()
