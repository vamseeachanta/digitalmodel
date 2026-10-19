"""Reproduce the surface-card arithmetic from issue #1855 without diagnosis.

Run with the existing digitized CSV as positional input. Optional
--image-output creates a synthetic image and calibrated CSV in an explicit
output directory; no field image or engineering inputs are inferred.
"""

from __future__ import annotations

import argparse
import csv
import math
from pathlib import Path

import numpy as np
from PIL import Image

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import analyse_card
from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.digitization import (
    AxisCalibration,
    DigitizedCard,
    digitize_image,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.surface_undulations import (
    analyse_undulations,
)


def digitization_example(output_dir: Path) -> DigitizedCard:
    """Synthetic calibration: x=2*column, load=100-10*row.

    All dimensions, colours and axis ticks below define an invented raster;
    they are not empirical constants or field settings. Thick traces at
    rows 1..2 and 7..8 have mean loads 85 and 25 respectively.
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    pixels = np.full((10, 5, 3), 255, dtype=np.uint8)
    pixels[1:3] = (10, 20, 30)
    pixels[7:9] = (10, 20, 30)
    image_path = output_dir / "synthetic-card.png"
    Image.fromarray(pixels).save(image_path)
    card = digitize_image(
        image_path,
        AxisCalibration(0, 0, 4, 8),
        AxisCalibration(0, 100, 9, 10),
        plot_box=(0, 0, 5, 10),
        rgb_min=(10, 20, 30),
        rgb_max=(10, 20, 30),
        max_gap_px=1,
    )
    card.write_csv(output_dir / "synthetic-card.csv")
    return card


def run_example(card_csv: Path) -> dict:
    """Use the corrected issue inputs; this is not a general well-input reader."""
    with card_csv.open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    positions = tuple(float(row["position_in"]) for row in rows)
    up = tuple(float(row["upstroke_load_lb"]) for row in rows)
    down = tuple(float(row["downstroke_load_lb"]) for row in rows)
    if (
        len(rows) < 2
        or not all(math.isfinite(v) for v in positions + up + down)
        or any(b <= a for a, b in zip(positions, positions[1:]))
        or any(a < b for a, b in zip(up, down))
    ):
        raise ValueError("CSV requires ordered finite positions and upper/lower loads")
    # Issue #1855, corrected data (2026): rod length 4200 ft, not the
    # 4300 ft pump depth; stroke 41 in, speed 6.4 SPM. Wave speed 16300 ft/s
    # is the stated steel assumption, not a measured wave speed for this well.
    # Uniform string gives Fc=1. Peak picks and ±1.5 in error are the issue's
    # revised screenshot interpretation; they are deliberately not auto-fitted.
    if positions[0] < 0 or positions[-1] > 41:
        raise ValueError("CSV positions must lie inside the stated example stroke")
    ringing = analyse_undulations(
        rod_length_ft=4200,
        wave_speed_ft_s=16300,
        taper_factor=1,
        stroke_in=41,
        strokes_per_minute=6.4,
        peak_positions_in=(7.5, 23.5, 35),
        position_error_in=1.5,
    )
    metrics = analyse_card(positions + positions[::-1], up + down[::-1], 6.4)
    return {
        "ringing": ringing,
        "card": metrics,
        "coverage_in": (positions[0], positions[-1]),
    }


def main() -> None:
    """Print the answer and unresolved checks, retaining coverage limitations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("card_csv", type=Path)
    parser.add_argument("--image-output", type=Path)
    args = parser.parse_args()
    result = run_example(args.card_csv)
    ringing, card = result["ringing"], result["card"]
    print(f"Ideal ringing period: {ringing.period_s:.2f} s")
    print(f"Natural frequency: {ringing.natural_frequency_spm:.1f} cycles/min")
    print(f"Pumping/natural frequency ratio: {100 * ringing.speed_ratio:.1f}%")
    print(
        f"Predicted oscillations per half stroke: "
        f"{ringing.undulations_per_half_stroke:.1f}"
    )
    print(f"Peak timing bounds: {ringing.timing_status}")
    for index, bounds in enumerate(ringing.interval_bounds, start=1):
        print(f"  Interval {index}: [{bounds.lower_s:.2f}, {bounds.upper_s:.2f}] s")
    print(
        "Ringing is consistent with the surface-card humps; "
        "this does not establish pump condition."
    )
    print(
        "Equal time intervals crowd in position near reversal because "
        "polished-rod velocity approaches zero."
    )
    low, high = result["coverage_in"]
    print(f"CSV position coverage: {low:.1f} to {high:.1f} in")
    print(f"Sampled polygon area: {card.card_area_in_lb:.1f} in-lb")
    print(
        f"Sampled-work power: {card.polished_rod_hp:.2f} hp; "
        "endpoint closure and screenshot envelope remain approximations."
    )
    print(
        f"Sampled peak/minimum loads: {card.peak_load_lb:.1f}/"
        f"{card.minimum_load_lb:.1f} lb; absolute datum requires calibration."
    )
    for name, reason in ringing.not_evaluated.items():
        print(f"{name}: NOT_EVALUATED — {reason}")
    if args.image_output is not None:
        digitized = digitization_example(args.image_output)
        print(f"Synthetic image digitized: {len(digitized.position_in)} columns")


if __name__ == "__main__":
    main()
