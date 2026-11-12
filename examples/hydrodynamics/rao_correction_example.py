# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic warning example; invoke with a new destination outside the checkout.

Independent answer: selecting a and b out of a, b, c marks two of three rows.
The arbitrary magnitudes 2, 4, 8 remain 2, 4, 8. Proposed replacement values
3 and 5 are not accepted without evidence. These are data-handling examples,
not published physical RAOs, a scaling rule, or acceptance limits.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq

from scripts.hf.rao_correction import correct, flag, verify


def run(destination: Path) -> None:
    """Write synthetic artifacts and show preservation and a refused replacement."""
    destination.mkdir(parents=True, exist_ok=False)
    source = destination / "source"
    source.mkdir()
    rows = [
        {"vessel_id": identifier, "freq_rad_s": 1.0, "magnitude": magnitude}
        for identifier, magnitude in (("a", 2.0), ("b", 4.0), ("c", 8.0))
    ]
    pq.write_table(pa.Table.from_pylist(rows), source / "rao.parquet")
    (source / "README.md").write_text("# Synthetic dataset\n", encoding="utf-8")
    flagged = destination / "flagged"
    mapping = {"installation": ["a"], "parametric": ["b"]}
    flag(source, flagged, mapping, "2026-10-21")
    verify(source, flagged)
    table = pq.ParquetFile(flagged / "rao.parquet").read()
    print("magnitudes:", table["magnitude"].to_pylist())
    print("statuses:", table["correction_status"].to_pylist())
    # A candidate is an input, not a derived factor applied to invalid results.
    proposed = [{**rows[0], "magnitude": 3.0}, {**rows[1], "magnitude": 5.0}]
    candidate = destination / "candidate.parquet"
    pq.write_table(pa.Table.from_pylist(proposed), candidate)
    try:
        correct(
            source,
            flagged,
            destination / "corrected",
            candidate,
            destination / "missing-gates.json",
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )
    except ValueError:
        print("replacement: NOT_EVALUATED (required gate evidence absent)")
    else:
        raise AssertionError("Missing evidence was accepted")
    print("publication: NOT_EVALUATED (external claims-verifier required)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("destination", type=Path)
    run(parser.parse_args().destination)
