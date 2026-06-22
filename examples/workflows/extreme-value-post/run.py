#!/usr/bin/env python
"""Offline extreme-value post-processing demo (issue #961).

Fits Gumbel (Type I) and 3-parameter Weibull extreme-value distributions to a
committed synthetic block-maxima dataset and reports return-period extremes
(10 / 50 / 100 / 1000-yr) using ``digitalmodel.orcaflex.postprocessor``.

Fully OFFLINE and LICENSE-FREE: no OrcFxAPI, no OrcaFlex ``.sim``, no license.
The postprocessor operates on a numeric array of block maxima; this script
just loads that array from CSV (per the sibling ``input.yml``), calls
``fit_gumbel`` / ``fit_weibull``, and writes a JSON summary.

Run::

    uv run python examples/workflows/extreme-value-post/run.py

References: Gumbel (1958); DNV-RP-C205 Sec. 3.4; API RP 2SK.
"""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import Dict, List

import numpy as np
import yaml

from digitalmodel.orcaflex.postprocessor import (
    ExtremeValueResult,
    fit_gumbel,
    fit_weibull,
)

HERE = Path(__file__).resolve().parent


def load_block_maxima(csv_path: Path, column: str) -> np.ndarray:
    """Load a single column of block maxima from a (comment-tolerant) CSV."""
    values: List[float] = []
    with csv_path.open(newline="") as fh:
        rows = (line for line in fh if not line.lstrip().startswith("#"))
        reader = csv.DictReader(rows)
        for row in reader:
            raw = row.get(column)
            if raw is None or raw.strip() == "":
                continue
            values.append(float(raw))
    if not values:
        raise ValueError(f"No values found in column '{column}' of {csv_path}")
    return np.asarray(values, dtype=float)


def run(input_yml: Path = HERE / "input.yml") -> Dict[str, ExtremeValueResult]:
    """Execute the offline extreme-value workflow described by ``input_yml``.

    Returns a mapping of distribution name -> ExtremeValueResult and writes a
    JSON summary to the configured output directory.
    """
    cfg = yaml.safe_load(input_yml.read_text())
    spec = cfg["extreme_value_post"]

    data_cfg = spec["data"]
    csv_path = (HERE / data_cfg["path"]).resolve()
    maxima = load_block_maxima(csv_path, data_cfg["column"])
    return_periods = [float(t) for t in spec["return_periods"]]

    fitters = {"gumbel": fit_gumbel, "weibull": fit_weibull}
    results: Dict[str, ExtremeValueResult] = {}
    for dist in spec["distributions"]:
        fitter = fitters[dist.lower()]
        results[dist.lower()] = fitter(maxima, return_periods=return_periods)

    # Write summary JSON.
    out_dir = (HERE / cfg["outputs"]["directory"]).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / cfg["outputs"]["summary_json"]
    summary = {
        "variable_name": data_cfg.get("variable_name", ""),
        "n_blocks": int(maxima.size),
        "return_periods": return_periods,
        "fits": {name: res.model_dump() for name, res in results.items()},
    }
    out_path.write_text(json.dumps(summary, indent=2))

    return results


def main() -> None:
    results = run()
    print("Extreme-value post-processing (offline, license-free) — issue #961")
    print("=" * 66)
    for name, res in results.items():
        print(f"\n{res.distribution} fit:")
        print(f"  location={res.location}  scale={res.scale}  shape={res.shape}")
        print(f"  KS stat={res.ks_statistic}  p-value={res.ks_pvalue}")
        print("  return-period extremes:")
        for period, value in res.return_values.items():
            print(f"    {period:>5}-yr : {value}")


if __name__ == "__main__":
    main()
