# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproduce the frozen wall selections as pressure-only module screens.

Run with PYTHONPATH=src:. python -m scripts.riser_study.wall_basis_table
--output-dir <output-directory>. Outputs are tab-wt.csv and its JSON manifest.
Only the four explicitly listed strategies are executed. The M-T report's
strategy routing is retained without generating an envelope or a native result.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import inspect
import json
import math
import subprocess
from decimal import Decimal
from pathlib import Path

from digitalmodel import codes
from digitalmodel.materials.grades import get as get_grade
from digitalmodel.structural.analysis.wall_thickness import (
    DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_2rd import ApiRp2rdStrategy
from digitalmodel.structural.analysis.wall_thickness_codes.api_std_2rd import ApiStd2rdStrategy
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy

ROOT = Path(__file__).resolve().parents[2]
DATA = Path(__file__).resolve().parent
MATRIX = DATA / "wall_basis_matrix.csv"
BASIS = DATA / "wall_basis.json"
# Explicit editions from the supplied sizing basis, not a latest-edition claim.
STRATEGIES = (
    ("API RP 2RD", codes.API_RP_2RD.label, ApiRp2rdStrategy()),
    ("API RP 1111", ApiRp1111Strategy(2015).edition_info.display_label, ApiRp1111Strategy(2015)),
    ("API STD 2RD", codes.API_STD_2RD.label, ApiStd2rdStrategy()),
    ("DNV-ST-F101", DnvStF101Strategy(2021).edition_info.display_label, DnvStF101Strategy(2021)),
)
NUMERIC_COLUMNS = ("outside_diameter_mm", "wall_thickness_mm", "checked_wall_mm",
                   "design_pressure_MPa_g", "corrosion_allowance_mm", "water_depth_m")
CODE_IDS = {"API RP 1111": "API_RP1111", "API RP 2RD": "API_RP2RD",
            "API STD 2RD": "API_STD2RD", "DNV-ST-F101": "F101"}


def load_basis():
    """Read values with their supplied assumption or repository provenance."""
    return json.loads(BASIS.read_text(encoding="utf-8"))


def _value(basis, key):
    return basis[key]["value"]


def _checked_wall(wall_mm, corrosion_mm, basis):
    # Decimal grid arithmetic prevents a floating boundary from adding a tick.
    return float(Decimal(str(wall_mm)) * (1 - Decimal(str(
        _value(basis, "mill_minus_tolerance")))) - Decimal(str(corrosion_mm)))


def checked_geometry(od_mm, wall_mm, corrosion_mm, basis):
    """Apply the study deduction order once to all implementations.

    Returns:
        Geometry in metres with no remaining corrosion or mill deduction.
    """
    checked = _checked_wall(wall_mm, corrosion_mm, basis)
    if checked <= 0:
        raise ValueError("Checked wall must be positive")
    return PipeGeometry(od_mm / 1000, checked / 1000,
                        corrosion_allowance=0, fabrication_tolerance=0)


def read_matrix(path=MATRIX):
    """Read the sizing-only projection and reject incomplete or changed scope."""
    with Path(path).open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))
    expected_ids = {f"R{nps:02d}-W{wall}-{config}"
                    for nps in (8, 10, 12) for wall in (1, 2) for config in ("SCR", "SLWR")}
    if len(rows) != len(expected_ids) or {row["case_id"] for row in rows} != expected_ids:
        raise ValueError("Matrix must contain each of the twelve case IDs exactly once")
    basis = load_basis()
    for row in rows:
        row["nps_in"] = int(row["nps_in"])
        for key in NUMERIC_COLUMNS:
            row[key] = float(row[key])
            if not math.isfinite(row[key]) or row[key] <= 0:
                raise ValueError(f"Matrix quantity must be finite and positive: {key}")
        if row["nps_in"] != int(row["case_id"][1:3]):
            raise ValueError("Matrix size does not match case ID")
        if row["material_grade"] != "X65/L450_PSL2":
            raise ValueError("Unsupported matrix material grade")
        checked = _checked_wall(row["wall_thickness_mm"], row["corrosion_allowance_mm"], basis)
        if not math.isclose(checked, row["checked_wall_mm"], abs_tol=1e-9, rel_tol=0):
            raise ValueError("Matrix checked wall disagrees with the deduction rule")
    for nps in (8, 10, 12):
        section = [row for row in rows if row["nps_in"] == nps]
        for key in ("outside_diameter_mm", "design_pressure_MPa_g", "material_grade",
                    "corrosion_allowance_mm", "water_depth_m", "governing_check"):
            if len({row[key] for row in section}) != 1:
                raise ValueError(f"Inconsistent section basis: {key}")
        for option in ("W1", "W2"):
            pair = [row for row in section if row["case_id"].split("-")[1] == option]
            if any(pair[0][key] != pair[1][key] for key in NUMERIC_COLUMNS):
                raise ValueError("Configuration pair has inconsistent sizing inputs")
    return rows


def evaluate(section, wall_mm, basis):
    """Retain every native check under both pressure and empty-pipe loads.

    Signed internal-pressure ratios returned for the empty condition remain
    signed. They cannot govern the positive pressure envelope. No pressure
    support is credited in the pressure-only burst case.
    """
    geometry = checked_geometry(section["outside_diameter_mm"], wall_mm,
                                section["corrosion_allowance_mm"], basis)
    grade = get_grade("X65")  # SI registry, not the legacy converted-ksi table.
    material = PipeMaterial(section["material_grade"], grade.smys_mpa * 1e6,
                            grade.smts_mpa * 1e6, _value(basis, "youngs_modulus_pa"),
                            _value(basis, "poissons_ratio"))
    pressure = section["design_pressure_MPa_g"] * 1e6
    external = (_value(basis, "seawater_density_kg_m3") * _value(basis, "gravity_m_s2")
                * section["water_depth_m"])
    condition = f'empty_{section["water_depth_m"]:g}m'
    loads = (("pressure_only", DesignLoads(internal_pressure=pressure)),
             (condition, DesignLoads(external_pressure=external)))
    rows = []
    burst_capacity = None
    for name, label, strategy in STRATEGIES:
        for load_id, load in loads:
            native = strategy.run_checks(geometry, material, load, DesignFactors())
            if set(native) != set(strategy.check_names):
                raise ValueError("Missing or unexpected strategy checks")
            for check, (utilisation, details) in native.items():
                if not math.isfinite(utilisation):
                    raise ValueError("Non-finite strategy utilisation")
                rows.append({"code": name, "code_label": label, "condition": load_id,
                             "check": check, "check_id": f"{CODE_IDS[name]}_{check}_{load_id}",
                             "utilisation": utilisation, "details": details,
                             "internal_pressure_pa": load.internal_pressure,
                             "external_pressure_pa": load.external_pressure})
            if name == "API RP 2RD" and load_id == "pressure_only":
                burst_capacity = native["burst"][1]["p_b"]
    if burst_capacity is None or burst_capacity <= 0:
        raise ValueError("Missing positive RP 2RD burst capacity for the pressure guard")
    factor = _value(basis, "riser_pressure_factor")
    rows.append({"code": "pressure guard", "code_label": "Study guard: 0.60 Pb (API RP 2RD capacity)",
                 "condition": "pressure_only", "check": "riser_pressure_guard",
                 "check_id": "riser_pressure_guard_pressure_only",
                 "utilisation": pressure / (factor * burst_capacity),
                 "details": {"p_b": burst_capacity, "factor": factor},
                 "internal_pressure_pa": pressure, "external_pressure_pa": 0.0})
    return rows


def select_walls(section, basis):
    """Enumerate the 0.1 mm grid; W2 increases W1's checked wall by 25%."""
    step = Decimal(str(_value(basis, "grid_mm")))
    max_tick = math.ceil(Decimal(str(section["outside_diameter_mm"])) / (2 * step))
    selected = []
    for tick in range(1, max_tick):
        wall = float(tick * step)
        checked = _checked_wall(wall, section["corrosion_allowance_mm"], basis)
        if checked <= 0:
            continue
        if selected and Decimal(str(checked)) < (Decimal(str(selected[0]["checked_wall_mm"]))
                        * Decimal(str(_value(basis, "w2_checked_wall_multiplier")))):
            continue
        rows = evaluate(section, wall, basis)
        if max(row["utilisation"] for row in rows) <= 1:
            selected.append({"wall_option": f"W{len(selected) + 1}", "wall_mm": wall,
                             "checked_wall_mm": checked,
                             "outside_diameter_mm": section["outside_diameter_mm"], "checks": rows})
            if len(selected) == 2:
                return selected
    raise ValueError("No passing pair of walls found on the physical grid")


def _sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def build(output_dir):
    """Write the full check table and a deterministic provenance manifest.

    Args:
        output_dir: Destination for the CSV and JSON manifest.

    Returns:
        Manifest including full-precision results and source byte digests.
    """
    basis, matrix = load_basis(), read_matrix()
    selected = {nps: select_walls(next(row for row in matrix if row["nps_in"] == nps), basis)
                for nps in sorted({row["nps_in"] for row in matrix})}
    commit = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()
    # HEAD identifies the source base when local edits are present. Digests bind
    # the actual bytes, including this adapter, without claiming a clean commit.
    dirty = bool(subprocess.check_output(["git", "status", "--porcelain", "--untracked-files=normal"],
                                        cwd=ROOT, text=True).strip())
    output = Path(output_dir)
    output.mkdir(parents=True, exist_ok=True)
    table = output / "tab-wt.csv"
    fields = ("case_id", "size_nps_in", "outside_diameter_mm", "wall_option", "wall_mm",
              "checked_wall_mm", "check", "check_id", "condition", "utilisation", "governing",
              "code_label", "internal_pressure_pa", "external_pressure_pa", "commit")
    with table.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for case in matrix:
            result = selected[case["nps_in"]][int(case["case_id"].split("-")[1][1:]) - 1]
            if abs(result["wall_mm"] - case["wall_thickness_mm"]) >= 0.1:
                raise ValueError("Selected wall does not reproduce the frozen matrix")
            governing = max(result["checks"], key=lambda row: row["utilisation"])
            if governing["check_id"] != case["governing_check"]:
                raise ValueError("Governing check does not match the matrix")
            for check in result["checks"]:
                writer.writerow({"case_id": case["case_id"], "size_nps_in": case["nps_in"],
                                 "outside_diameter_mm": case["outside_diameter_mm"],
                                 "wall_option": result["wall_option"], "wall_mm": result["wall_mm"],
                                 "checked_wall_mm": result["checked_wall_mm"], "commit": commit,
                                 "governing": str(check is governing).lower(),
                                 **{key: check[key] for key in ("check", "check_id", "condition",
                                    "code_label", "utilisation", "internal_pressure_pa", "external_pressure_pa")}})
    paths = {Path(__file__), MATRIX, BASIS, Path(inspect.getfile(get_grade)),
             ROOT / "src/digitalmodel/codes.py", ROOT / "src/digitalmodel/structural/analysis/wall_thickness.py",
             ROOT / "src/digitalmodel/structural/analysis/wall_thickness_mt_report.py",
             ROOT / "scripts/reporting/build_riser_pipe_comparison.py"}
    paths.update(Path(inspect.getfile(type(strategy))) for _, _, strategy in STRATEGIES)
    manifest = {"artifact": "TAB-WT", "commit": commit, "source_tree_dirty": dirty,
                "commit_scope": "HEAD source revision; sources digests bind the actual calculation bytes",
                "matrix_scope": "Sizing columns only; all twelve case IDs retained",
                "basis": basis, "matrix": matrix,
                "selections": {str(nps): results for nps, results in selected.items()},
                "sources": [{"path": path.relative_to(ROOT).as_posix(), "sha256": _sha(path)}
                            for path in sorted(paths)],
                "table": {"file": table.name, "rows": sum(len(result["checks"]) * 2 for results in selected.values() for result in results),
                          "sha256": _sha(table)},
                "limits": [
                    "Pressure-only module screens; not standard certification, procurement or dynamic resistance.",
                    "Zero moment and effective tension; ambient temperature with no derating; seamless material.",
                    "Common checked wall = (1 - mill minus tolerance) * nominal wall - corrosion allowance.",
                    "All returned checks retained; empty-pipe internal-pressure ratios can be negative.",
                    "No internal-pressure support for burst; full-depth empty pressure without buckle arrestors.",
                    "W1 is the first passing grid wall; W2 is a checked-wall uplift, not a second pressure minimum.",
                    "RP 1111 and F101 are pipeline cross-checks; F101 uses implemented medium-class factors.",
                    "RP 1111 propagation is imposed as a wall criterion by the study's no-arrestor assumption.",
                    "STD 2RD standalone burst output is unmodified; the separate 0.60 Pb guard also applies.",
                    "ISO 13623 and PD 8010-2 collapse solvers are excluded.",
                    "Ovality, fabrication and resistance factors are the implemented values, not measured inputs.",
                ]}
    (output / "tab-wt.manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True,
                                                         allow_nan=False) + "\n", encoding="utf-8")
    return manifest


def main():
    """Generate the table in the explicitly supplied output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    manifest = build(args.output_dir)
    print(f'TAB-WT: {manifest["table"]["rows"]} check rows; six selected sections; twelve cases')


if __name__ == "__main__":
    main()
