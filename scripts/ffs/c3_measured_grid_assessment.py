# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run the corroded-pipeline article chain on a measured UT C-scan grid.

Article: "Fitness for service of corroded pipelines: strength and remaining
useful life (RUL)", revision 6. The measured grid is the anonymized worst-zone
scan published in ``tests/asset_integrity/test_data/real_inspection`` (see its
README for provenance). The assessment calls the same function that rates the
synthetic Section A in ``scripts/reporting/build_pipeline_ffs_report.py``
(``evaluate_section``), so both sections go through one chain.

Preparation of the measured grid, all stated in the output:

- Defect window: the run of axial rows around the thinnest reading that hold a
  reading with more than 10 % loss (the local-loss engine's flaw threshold),
  plus one row each side; columns span the first to the last column with such a
  reading inside that run.
- Readings above the nominal wall are taken as nominal (no loss, no credit).
- Unmeasured cells (blank in the scan) take the thinnest measured reading of
  their own axial row inside the window. Every method here uses the deepest
  loss of each row, so this fill changes no result; the script checks that by
  rerunning with blanks set to nominal.

Usage, from the repository root with ``src`` on PYTHONPATH::

    python scripts/ffs/c3_measured_grid_assessment.py --out docs/articles/fitness-for-service-corroded-pipelines/data
"""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import math
from pathlib import Path

import numpy as np
import pandas as pd

from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSComponent
from digitalmodel.asset_integrity.corroded_pipe import (
    ASME_B318_CLASS1_DESIGN_FACTOR,
    ASME_B318_CLASS1_SAFETY_FACTOR,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import SMTS_PSI

MM_PER_IN = 25.4
PSI_PER_MPA = 145.0377
GRID = Path(
    "tests/asset_integrity/test_data/real_inspection/"
    "ut_grid_RJ-101-boxend_worstzone_fullres.csv"
)
BUILDER = Path("scripts/reporting/build_pipeline_ffs_report.py")

# Pipe and scan, from the fixture README: 21.25 in OD x 0.875 in nominal wall,
# X-80; 64 circumferential stations; 4 mm axial pitch at full resolution.
NOMINAL_OD_IN = 21.25
NOMINAL_WT_IN = 0.875
GRADE = "X80"
AXIAL_PITCH_MM = 4.0
CIRC_STATIONS = 64
# Assumptions for this article (stated in the output):
# the 64 stations are equally spaced round the full circumference;
DESIGN_PRESSURE_PSI = 2400.0  # about half the rating before loss, as Section A
AXIAL_STRESS_PSI = 10000.0  # copied from Section A
RATE_MM_PER_YR = 0.253  # this scan's rate in the campaign results register
SENSITIVITY = [0.5, 1.0, 2.0]
INTERVAL_FRACTION = 0.5
FLAW_LOSS = 0.10


def load_builder(root):
    spec = importlib.util.spec_from_file_location(
        "pipeline_ffs_builder", root / BUILDER
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def defect_window(grid_mm):
    nominal_mm = NOMINAL_WT_IN * MM_PER_IN
    flaw = grid_mm < nominal_mm * (1 - FLAW_LOSS)
    r0, c0 = np.unravel_index(np.nanargmin(grid_mm), grid_mm.shape)
    rows = flaw.any(axis=1)
    top = r0
    while top > 0 and rows[top - 1]:
        top -= 1
    bottom = r0
    while bottom < len(rows) - 1 and rows[bottom + 1]:
        bottom += 1
    cols = np.where(flaw[top : bottom + 1].any(axis=0))[0]
    window = (
        int(max(top - 1, 0)),
        int(min(bottom + 1, grid_mm.shape[0] - 1)),
        int(cols.min()),
        int(cols.max()),
    )
    other = [int(r) for r in np.where(rows)[0] if r < top or r > bottom]
    return window, (int(r0), int(c0)), other


def prepare(grid_mm, window, blank="row-minimum"):
    r_lo, r_hi, c_lo, c_hi = window
    nominal_mm = NOMINAL_WT_IN * MM_PER_IN
    patch = grid_mm[r_lo : r_hi + 1, c_lo : c_hi + 1].copy()
    blanks = int(np.isnan(patch).sum())
    above = int((patch > nominal_mm).sum())
    patch = np.minimum(patch, nominal_mm)
    for i in range(patch.shape[0]):
        row = patch[i]
        fill = np.nanmin(row) if blank == "row-minimum" else nominal_mm
        row[np.isnan(row)] = fill
    return patch, blanks, above


def section_inputs(patch_mm, name):
    positions_in = [i * AXIAL_PITCH_MM / MM_PER_IN for i in range(patch_mm.shape[0])]
    pitch_in = math.pi * NOMINAL_OD_IN / CIRC_STATIONS
    return {
        "component_id": name,
        "data_origin": "measured UT C-scan, anonymized; see the fixture README",
        "nominal_od_in": NOMINAL_OD_IN,
        "nominal_wt_in": NOMINAL_WT_IN,
        "smys_psi": 80000.0,
        "smts_psi": float(SMTS_PSI[GRADE]),
        "design_pressure_psi": DESIGN_PRESSURE_PSI,
        "axial_stress_psi": AXIAL_STRESS_PSI,
        "safety_factor": ASME_B318_CLASS1_SAFETY_FACTOR,
        "usage_factor": ASME_B318_CLASS1_DESIGN_FACTOR,
        "axial_design_factor": ASME_B318_CLASS1_DESIGN_FACTOR,
        "axial_positions_in": positions_in,
        "circumferential_width_in": (patch_mm.shape[1] - 1) * pitch_in,
        # The full scan continues beyond both window ends with every reading
        # inside the 10 % threshold, so the defect length is bounded.
        "length_confirmed": True,
        "grid": (patch_mm / MM_PER_IN).tolist(),
    }


def run(builder, data, rate_mm):
    component = FFSComponent(
        component_id=data["component_id"],
        design_code="B31.8",
        nominal_od_in=data["nominal_od_in"],
        nominal_wt_in=data["nominal_wt_in"],
        design_pressure_psi=data["design_pressure_psi"],
        smys_psi=data["smys_psi"],
        corrosion_rate_in_per_yr=rate_mm / MM_PER_IN,
        rsf_a=0.90,
    )
    cfg = {
        "corrosion_rate_in_per_yr": rate_mm / MM_PER_IN,
        "sensitivity_multipliers": SENSITIVITY,
        "interval_life_fraction": INTERVAL_FRACTION,
    }
    return builder.evaluate_section(dict(data), component, cfg)


def summary(section):
    mpa = lambda psi: psi / PSI_PER_MPA  # noqa: E731
    design = section["inputs"]["design_pressure_psi"]
    methods = []
    for row in section["strength"]["methods"]:
        if row["allowable_pressure_psi"] is None:
            continue
        methods.append(
            {
                "method": row["method"].replace("_", " "),
                "failure_psi": row["capacity_pressure_psi"],
                "failure_mpa": mpa(row["capacity_pressure_psi"]),
                "allowable_psi": row["allowable_pressure_psi"],
                "allowable_mpa": mpa(row["allowable_pressure_psi"]),
                "applicability_ok": row["applicability"]["ok"],
                "flags": row["applicability"]["flags"],
            }
        )
    level2 = section["level2"]
    return {
        "design_pressure_psi": design,
        "design_pressure_mpa": mpa(design),
        "t_min_mm": section["coordinator"]["t_measured_min_in"] * MM_PER_IN,
        "pressure_wall_mm": section["coordinator"]["t_min_in"] * MM_PER_IN,
        "uncorroded_rating_psi": section["uncorroded_rating_psi"],
        "uncorroded_rating_mpa": mpa(section["uncorroded_rating_psi"]),
        "rsf": level2["rsf"],
        "flaw_length_mm": level2["flaw_length_in"] * MM_PER_IN,
        "folias_factor": level2["folias_factor"],
        "local_loss_rating_psi": section["local_loss_rating_psi"],
        "local_loss_rating_mpa": mpa(section["local_loss_rating_psi"]),
        "local_loss_rating_pct_design": 100 * section["local_loss_rating_psi"] / design,
        "life_limit_wall_mm": section["life_limit_wall_in"] * MM_PER_IN,
        "life_basis": section["life_basis"],
        "life_search_status": (section["life_search"] or {}).get("status"),
        "remaining_life_yr": section["remaining_life_yr"],
        "interval_yr": section["interval_yr"],
        "life_sensitivity": [
            {"rate_mm_per_yr": r * MM_PER_IN, "life_yr": life}
            for r, life in section["life_sensitivity"]
        ],
        "methods": methods,
        "applicability": section["applicability"],
        "decision": section["decision"],
        "sufficiency_status": section["sufficiency"]["status"],
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    root, out = args.root, args.out
    out.mkdir(parents=True, exist_ok=True)
    builder = load_builder(root)

    grid_path = root / GRID
    grid_mm = pd.read_csv(grid_path, header=None).to_numpy(dtype=float)
    window, deepest, other_rows = defect_window(grid_mm)
    patch, blanks, above = prepare(grid_mm, window)
    check, _, _ = prepare(grid_mm, window, blank="nominal")
    measured = summary(
        run(builder, section_inputs(patch, "Measured section"), RATE_MM_PER_YR)
    )
    fill_check = summary(
        run(builder, section_inputs(check, "Measured section"), RATE_MM_PER_YR)
    )
    keys = ("local_loss_rating_psi", "remaining_life_yr", "rsf")
    fill_neutral = all(
        math.isclose(measured[k], fill_check[k], rel_tol=1e-12) for k in keys
    ) and all(
        math.isclose(a["allowable_psi"], b["allowable_psi"], rel_tol=1e-12)
        for a, b in zip(measured["methods"], fill_check["methods"])
    )

    synthetic = summary(builder.evaluate(root)["sections"][0])

    nominal_mm = NOMINAL_WT_IN * MM_PER_IN
    finite = grid_mm[np.isfinite(grid_mm)]
    record = {
        "grid_file": GRID.as_posix(),
        "grid_sha256": hashlib.sha256(
            grid_path.read_bytes().replace(b"\r\n", b"\n")
        ).hexdigest(),
        "grid_shape": list(grid_mm.shape),
        "grid_readings": int(finite.size),
        "grid_blank_cells": int(np.isnan(grid_mm).sum()),
        "grid_min_mm": float(finite.min()),
        "grid_max_mm": float(finite.max()),
        "grid_mean_mm": float(finite.mean()),
        "nominal_wt_mm": nominal_mm,
        "max_loss_pct": 100 * (1 - float(finite.min()) / nominal_mm),
        "cells_over_10pct_loss": int((finite < nominal_mm * (1 - FLAW_LOSS)).sum()),
        "deepest_row_col": list(deepest),
        "window_rows": [window[0], window[1]],
        "window_cols": [window[2], window[3]],
        "window_shape": list(patch.shape),
        "window_length_mm": (patch.shape[0] - 1) * AXIAL_PITCH_MM,
        "window_width_mm": (patch.shape[1] - 1)
        * math.pi
        * NOMINAL_OD_IN
        * MM_PER_IN
        / CIRC_STATIONS,
        "window_blank_cells_filled": blanks,
        "window_above_nominal_set_to_nominal": above,
        "blank_fill_changes_no_result": fill_neutral,
        "rows_over_10pct_outside_window": other_rows,
        "other_rows_min_mm": float(np.nanmin(grid_mm[other_rows]))
        if other_rows
        else None,
        "assumptions": {
            "design_pressure_psi": DESIGN_PRESSURE_PSI,
            "axial_stress_psi": AXIAL_STRESS_PSI,
            "corrosion_rate_mm_per_yr": RATE_MM_PER_YR,
            "circumferential_pitch_mm": math.pi
            * NOMINAL_OD_IN
            * MM_PER_IN
            / CIRC_STATIONS,
            "smts_psi": float(SMTS_PSI[GRADE]),
        },
        "measured": measured,
        "synthetic_section_a": synthetic,
    }
    (out / "measured-grid-results.json").write_text(
        json.dumps(record, indent=1) + "\n", encoding="utf-8", newline="\n"
    )
    rows = []
    for i in range(patch.shape[0]):
        for j in range(patch.shape[1]):
            raw = grid_mm[window[0] + i, window[2] + j]
            rows.append(
                {
                    "axial_mm": i * AXIAL_PITCH_MM,
                    "column": j + 1,
                    "wall_mm": round(float(patch[i, j]), 3),
                    "measured": bool(np.isfinite(raw)),
                }
            )
    pd.DataFrame(rows).to_csv(
        out / "measured-grid-window.csv", index=False, lineterminator="\n"
    )
    print(
        json.dumps(
            {
                k: record[k]
                for k in record
                if k not in ("measured", "synthetic_section_a")
            },
            indent=1,
        )
    )
    print(json.dumps(measured, indent=1))
    print(
        "SYNTHETIC",
        json.dumps(
            {
                k: synthetic[k]
                for k in (
                    "local_loss_rating_psi",
                    "local_loss_rating_pct_design",
                    "remaining_life_yr",
                    "rsf",
                )
            },
            indent=1,
        ),
    )


if __name__ == "__main__":
    main()
