"""Drive-off at 12 kn and 20 deg; write SI diagnostics and headless plots.

The primary model gives an interaction increment, the fallback a total force.
Engine-off propeller drag and windmilling are not modelled. Braking increments
are reported as computed; fallback forces at bollard are not evaluated.
CSV speeds are m/s, n is rev/s, T is N, and J and loading are dimensionless.
"""
import csv
import math
from dataclasses import asdict
from pathlib import Path

import matplotlib
import yaml

from digitalmodel.hydrodynamics.propeller_rudder import driveoff_sweep
from digitalmodel.hydrodynamics.propeller_rudder_workflow import (
    KNOT_TO_M_PER_S,
    validate_propeller_rudder_input,
)

matplotlib.use("Agg")

WORKFLOW = Path(__file__).resolve().parent / "workflows/propeller-rudder-interaction"
CAPTION = (
    "Primary: interaction increment; fallback: total rudder force.\n"
    "Engine off: propeller drag and windmilling not modelled.\n"
    "K_T <= 0: primary increment reported as computed."
)


def _csv_row(point):
    row = asdict(point)
    for method in ("primary", "fallback"):
        forces = row.pop(method)
        for component, unit in (("F_surge", "N"), ("F_sway", "N"), ("F_yaw", "Nm")):
            row[f"{method}_{component}_{unit}"] = (
                None if forces is None else forces[component]
            )
    row["label"] = {
        "engine_off": "engine off: propeller drag and windmilling not modelled",
        "braking": "K_T <= 0: primary increment reported as computed",
    }.get(point.regime, point.regime)
    return row


def _write_plots(points, output_dir):
    from matplotlib import pyplot as plt

    shaft_speeds = [point.n for point in points]
    figures = {}
    for name, ylabel in (
        ("sway", "Sway force [N]"), ("slipstream", "Axial velocity [m/s]"),
    ):
        figure, axis = plt.subplots(figsize=(8, 5))
        try:
            if name == "sway":
                for method, label in (
                    ("primary", "Interaction increment"),
                    ("fallback", "Total rudder force"),
                ):
                    values = [getattr(point, method) for point in points]
                    axis.plot(
                        shaft_speeds, [v.F_sway if v else math.nan for v in values],
                        "o-", label=label,
                    )
                axis.legend()
            else:
                axis.plot(
                    shaft_speeds, [point.slipstream_axial for point in points], "o-",
                )
            axis.set(
                xlabel="Shaft speed [rev/s]", ylabel=ylabel,
                title="Drive-off: 12 kn, rudder 20 deg",
            )
            axis.grid(True)
            figure.subplots_adjust(bottom=0.26)
            figure.text(0.5, 0.02, CAPTION, ha="center", fontsize=9)
            path = output_dir / f"driveoff_{name}.png"
            figure.savefig(path, dpi=150)
            figures[name] = path
        finally:
            plt.close(figure)
    return figures


def main(output_dir=None) -> dict:
    """Validate the registry input, sweep nine shaft speeds and return artifacts."""
    payload = yaml.safe_load((WORKFLOW / "input.yml").read_text(encoding="utf-8"))
    config = validate_propeller_rudder_input(payload["propeller_rudder"])
    points = driveoff_sweep(
        12.0 * KNOT_TO_M_PER_S, [i / 4.0 for i in range(9)], 20.0,
        config.vessel, config.rudder,
    )
    destination = (
        Path(output_dir) if output_dir is not None else WORKFLOW / "results/driveoff"
    )
    destination.mkdir(parents=True, exist_ok=True)
    rows = [_csv_row(point) for point in points]
    csv_path = destination / "driveoff.csv"
    with csv_path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    return {"csv": csv_path, **_write_plots(points, destination)}


if __name__ == "__main__":
    main()
