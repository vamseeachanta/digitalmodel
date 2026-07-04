# ABOUTME: Builds a representative indexed FFS lookup (ffs_results.json +
# ABOUTME: ffs_cases.csv) across pipe + plate domains for the Deckhand API.
"""Build the FFS indexed lookup artifact.

Sweeps representative corroded-pipe and plate-metal-loss scenarios through the
uniform query router and writes ``ffs_results.json`` (meta/lookup/index) +
``ffs_cases.csv`` — the O(1) lookup table a downstream API (Deckhand) queries.

Run:
    .venv/bin/python examples/demos/asset_integrity/ffs_lookup_build.py
"""

from __future__ import annotations

import itertools
from pathlib import Path

from digitalmodel.asset_integrity.ffs_lookup import (
    build_lookup,
    evaluate_query,
    write_lookup,
)

OUT_DIR = Path(__file__).resolve().parent / "output"

# --- sweep definitions -----------------------------------------------------
PIPE_METHODS = ["b31g", "modified_b31g", "rstreng", "dnv_f101"]
PIPE_GRADES = ["X52", "X65"]
PIPE_D_IN, PIPE_T_IN = 20.0, 0.5
PIPE_DEPTH_FRAC = [0.2, 0.4, 0.6]          # d / t
PIPE_LENGTHS_IN = [2.0, 4.0, 8.0, 16.0]

PLATE_GRADES = ["Grade A", "AH36", "EH40"]
PLATE_L, PLATE_W = 2400.0, 800.0
PLATE_THICK_MM = [12.0, 16.0, 20.0]
PLATE_SIGMA_X = [100.0, 150.0, 200.0]
PLATE_METAL_LOSS = [0.0, 2.0, 4.0, 6.0]


def build_records() -> list:
    records = []
    for method, grade, frac, L in itertools.product(
        PIPE_METHODS, PIPE_GRADES, PIPE_DEPTH_FRAC, PIPE_LENGTHS_IN
    ):
        records.append(evaluate_query(
            "pipe_corroded_strength", method=method, grade=grade,
            D_in=PIPE_D_IN, t_in=PIPE_T_IN, d_in=round(frac * PIPE_T_IN, 4),
            L_in=L,
        ))
    for grade, t, sx, loss in itertools.product(
        PLATE_GRADES, PLATE_THICK_MM, PLATE_SIGMA_X, PLATE_METAL_LOSS
    ):
        records.append(evaluate_query(
            "plate_metal_loss", grade=grade, length_mm=PLATE_L, width_mm=PLATE_W,
            thickness_mm=t, sigma_x_mpa=sx, metal_loss_mm=loss,
        ))
    return records


def main() -> None:
    records = build_records()
    lookup = build_lookup(records, meta_extra={"generated_by": "ffs_lookup_build"})
    paths = write_lookup(lookup, OUT_DIR)
    n_pipe = sum(1 for r in records if r["domain"] == "pipe_corroded_strength")
    n_plate = len(records) - n_pipe
    n_unacc = sum(1 for r in records if r["acceptable"] is False)
    print(f"FFS lookup: {len(records)} records "
          f"({n_pipe} pipe corroded-strength, {n_plate} plate metal-loss); "
          f"{n_unacc} unacceptable")
    print(f"  index entries: {len(lookup['index'])}")
    print(f"  JSON -> {paths['json']}")
    print(f"  CSV  -> {paths['csv']}")


if __name__ == "__main__":
    main()
