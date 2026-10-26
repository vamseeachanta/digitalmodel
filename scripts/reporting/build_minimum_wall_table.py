# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Tabulate implemented strategy results without adding resistance formulas.

Enumerate the existing comparison builder's 0.1 mm nominal-wall grid, excluding
nonpositive remaining walls. Pressure sizing uses each route's pressure checks
only, including the additional hoop check of the API RP 2RD route. Search stock
walls in ascending order and require pressure, collapse AND propagation to pass.
Missing or suppressed checks leave stock selection incomplete.

Five pressure routes already deduct corrosion; pass nominal wall and allowance
unchanged to them. For the other four, pass the remaining wall with zero
allowance to pressure checks only. External checks retain their native wall
bases. F101 and F201 use the geometry's multiplicative fabrication fraction.
Combined checks are outside this table's scope. No standard text was consulted.
"""

import argparse
import csv
import hashlib
import inspect
import io
import json
import math
from pathlib import Path
import re
import subprocess

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode, DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY

ROOT = Path(__file__).resolve().parents[2]
INPUT = ROOT / "scripts/reporting/minimum_wall_table.json"
CRITERIA = ROOT / "scripts/reporting/riser_pipe_comparison.criteria.json"
OUTPUT = ROOT / "docs/reports/minimum-wall-table"
NATIVE_CORROSION = {
    DesignCode.ASME_B31_4, DesignCode.PD_8010_2, DesignCode.ISO_13623,
    DesignCode.DNV_ST_F101, DesignCode.DNV_ST_F201,
}
FABRICATION = {DesignCode.DNV_ST_F101, DesignCode.DNV_ST_F201}
SUPPRESSED = {DesignCode.ISO_13623, DesignCode.PD_8010_2}
PRESSURE_METHODS = {
    DesignCode.DNV_ST_F101: ("pressure_containment",),
    DesignCode.DNV_ST_F201: ("pressure_containment",),
    DesignCode.API_RP_1111: ("burst",),
    DesignCode.API_RP_2RD: ("burst", "hoop"),
    DesignCode.API_STD_2RD: ("burst",),
    DesignCode.ASME_B31_4: ("burst",),
    DesignCode.ASME_B31_8: ("burst",),
    DesignCode.ISO_13623: ("pressure_containment",),
    DesignCode.PD_8010_2: ("hoop_stress",),
}
# Symbol inventory only: values and standard qualification are not supplied here.
TRANSITION = ("p_el coefficient and exponent", "p_y coefficient", "p_c transition exponents", "f_c")
PROPAGATION = ("p_pr coefficient and exponent", "f_p")
COEFFICIENTS = {
    DesignCode.API_RP_1111: {
        "burst": ("_check_burst.p_b coefficients and branch threshold", "EDITION_FACTORS.f_d"),
        "collapse": ("_check_collapse.p_el coefficient", "_check_collapse.p_y coefficient",
                     "p_c transition exponents", "EDITION_FACTORS.f_c"),
        "propagation": ("_check_propagation.p_pr coefficient and exponent", "EDITION_FACTORS.f_p"),
    },
    DesignCode.API_RP_2RD: {
        "burst": ("_check_burst.p_b coefficient", "C_A (burst and collapse)"),
        "hoop": ("_check_hoop.sigma_allow coefficient", "d_i coefficient", "sigma_h divisor"),
        "collapse": ("_check_collapse.p_el coefficient", "_check_collapse.p_y coefficient",
                     "p_c transition exponents", "C_A (burst and collapse)"),
    },
    DesignCode.API_STD_2RD: {
        "burst": ("_check_burst.p_b coefficient", "_check_burst.f_d"),
        "collapse": ("_check_collapse.p_el coefficient", "_check_collapse.p_y coefficient",
                     "p_c transition exponents", "_check_collapse.f_c"),
    },
    DesignCode.ASME_B31_4: {
        "burst": ("p_d coefficient", "DESIGN_FACTOR_F", "JOINT_FACTOR_E", "TEMP_FACTOR_T"),
        "collapse": TRANSITION, "propagation": PROPAGATION,
    },
    DesignCode.ASME_B31_8: {
        "burst": ("p_d coefficient", "F", "E_j", "T_factor"),
        "collapse": TRANSITION, "propagation": PROPAGATION,
    },
    DesignCode.DNV_ST_F101: {
        "pressure_containment": ("EDITION_FACTORS.f_y", "EDITION_FACTORS.f_u",
                                 "f_cb strength divisor", "p_b coefficients",
                                 "DesignFactors.gamma_sc", "DesignFactors.gamma_m", "DesignFactors.gamma_inc"),
        "collapse": ("p_el coefficient and exponent", "p_p coefficient", "PipeMaterial.alpha_fab",
                     "EDITION_FACTORS.f_0", "DesignFactors.gamma_sc", "DesignFactors.gamma_m", "DesignFactors.gamma_inc"),
        "propagation": ("p_pr coefficient and exponent", "PipeMaterial.alpha_fab",
                        "DesignFactors.gamma_sc", "DesignFactors.gamma_m", "DesignFactors.gamma_inc"),
    },
    DesignCode.DNV_ST_F201: {
        "pressure_containment": ("DEFAULT_FACTORS.alpha_U", "f_cb strength divisor", "p_b coefficients",
                                 "DEFAULT_FACTORS.gamma_m", "DEFAULT_FACTORS.gamma_SC", "DEFAULT_FACTORS.gamma_dyn"),
    },
    DesignCode.ISO_13623: {
        "pressure_containment": ("p_d coefficient", "e_j", "gamma_s"),
        "collapse": ("p_el coefficient and exponent", "p_p coefficient", "PipeMaterial.alpha_fab", "f_0", "f_c"),
        "propagation": PROPAGATION,
    },
    DesignCode.PD_8010_2: {
        "hoop_stress": ("sigma_h divisor", "f_d"),
        "collapse": ("p_el coefficient and exponent", "p_p coefficient", "f_0", "f_c"),
        "propagation": PROPAGATION,
    },
}
FIELDS = (
    "code", "design_pressure_mpa", "external_pressure_mpa", "pressure_basis",
    "pressure_wall_basis", "external_wall_basis", "pressure_minimum_wall_mm",
    "pressure_minimum_set_by", "stock_wall_mm", "pressure_utilisation_percent",
    "collapse_utilisation_percent", "propagation_utilisation_percent",
    "collapse_status", "propagation_status", "failed_checks", "governing_check",
    "selection_status", "selection_set_by", "strategy_source",
)
REVISION_HISTORY = [
    {"revision": 0, "date": "2026-10-26",
     "change": "Initial pressure grid and stock selection trace with explicit check gaps"},
    {"revision": 1, "date": "2026-10-26",
     "change": "Preserve per-check coefficient references and unresolved qualification gaps for stock trials"},
]


def validate(cfg):
    """Reject nonphysical, ambiguous or unordered input before writing output."""
    pipe = cfg["pipe"]

    def finite(value):
        return (isinstance(value, (int, float)) and not isinstance(value, bool)
                and math.isfinite(value))

    if not all(finite(value) for value in pipe.values()):
        raise ValueError("Pipe properties must be finite numbers")
    diameter = pipe["outer_diameter_mm"]
    if not 0 <= pipe["corrosion_allowance_mm"] < diameter / 2:
        raise ValueError("Corrosion must leave positive wall inside the diameter")
    if not 0 <= pipe["fabrication_tolerance_fraction"] < 1:
        raise ValueError("Fabrication tolerance must be a fraction in [0, 1)")
    if not -1 < pipe["poissons_ratio"] < 0.5:
        raise ValueError("Poisson ratio must be in (-1, 0.5)")
    if not 0 < pipe["smys_mpa"] <= pipe["smts_mpa"] or pipe["youngs_modulus_mpa"] <= 0:
        raise ValueError("Material strengths and modulus must be positive")
    external = cfg["external_pressure_mpa"]
    if not finite(external) or external < 0:
        raise ValueError("External pressure must be finite and nonnegative")
    for key in ("pressures_mpa", "stock_walls_mm"):
        values = cfg[key]
        if not values or not all(finite(value) and value > 0 for value in values):
            raise ValueError("Pressure and stock lists must be positive and finite")
        if values != sorted(set(values)):
            raise ValueError("Pressure and stock lists must be strictly increasing")
    if min(cfg["pressures_mpa"]) < external:
        raise ValueError("Design pressures below external pressure are outside this sweep")
    if any(not pipe["corrosion_allowance_mm"] < wall < diameter / 2
           for wall in cfg["stock_walls_mm"]):
        raise ValueError("Stock walls must leave positive remaining wall inside the diameter")
    if cfg["pressure_basis"] != "ASSUMED gauge":
        raise ValueError("Pressure basis remains ASSUMED gauge pending confirmed data")
    if cfg["data_basis"] != "assumed, based on previous experience":
        raise ValueError("Generic inputs must retain their assumed data basis")
    if not re.fullmatch(r"[0-9a-f]{40}", cfg["source_commit"]):
        raise ValueError("A full source commit is required")


def _objects(cfg, wall_mm, pressure_mpa, *, remaining=False):
    pipe = cfg["pipe"]
    allowance = pipe["corrosion_allowance_mm"]
    geometry = PipeGeometry(
        pipe["outer_diameter_mm"] / 1000,
        (wall_mm - allowance if remaining else wall_mm) / 1000,
        0 if remaining else allowance / 1000,
        pipe["fabrication_tolerance_fraction"],
    )
    material = PipeMaterial(
        "assumed generic steel", pipe["smys_mpa"] * 1e6, pipe["smts_mpa"] * 1e6,
        pipe["youngs_modulus_mpa"] * 1e6, pipe["poissons_ratio"],
    )
    loads = DesignLoads(pressure_mpa * 1e6, cfg["external_pressure_mpa"] * 1e6)
    return geometry, material, loads


def _call(strategy, name, objects, code):
    args = (*objects, DesignFactors()) if code in FABRICATION else objects
    value, _details = getattr(strategy, "_check_" + name)(*args)
    if not math.isfinite(value) or value < 0:
        raise ValueError("Invalid utilisation from strategy")
    return value


def pressure_checks(cfg, code, wall_mm, pressure_mpa):
    """Apply corrosion once, returning unrounded pressure-only utilisations."""
    strategy = CODE_REGISTRY[code]()
    objects = _objects(cfg, wall_mm, pressure_mpa, remaining=code not in NATIVE_CORROSION)
    return {name: _call(strategy, name, objects, code) for name in PRESSURE_METHODS[code]}


def external_checks(cfg, code, wall_mm, pressure_mpa):
    """Evaluate available external checks; never call the known faulty solvers."""
    strategy = CODE_REGISTRY[code]()
    objects = _objects(cfg, wall_mm, pressure_mpa)
    checks = {}
    if "collapse" in strategy.check_names and code not in SUPPRESSED:
        checks["collapse"] = _call(strategy, "collapse", objects, code)
    propagation = "propagation_buckling" if code == DesignCode.DNV_ST_F101 else "propagation"
    if propagation in strategy.check_names:
        checks["propagation"] = _call(strategy, propagation, objects, code)
    return checks


def pressure_minimum(cfg, code, pressure_mpa):
    """Enumerate the first passing nominal wall on the existing 0.1 mm grid.

    The reference search is build_riser_pipe_comparison._minimum_wall at the
    pinned source commit. Its integer ticks avoid accumulated floating steps;
    its strict half-diameter bound and exhaustive ordering are preserved here.
    Only the chosen check set and corrosion handling differ.
    """
    pipe = cfg["pipe"]
    for tick in range(1, math.ceil(pipe["outer_diameter_mm"] * 5)):
        wall = tick / 10
        if wall <= pipe["corrosion_allowance_mm"]:
            continue
        checks = pressure_checks(cfg, code, wall, pressure_mpa)
        governing = max(checks, key=checks.get)
        if checks[governing] <= 1:
            return {"wall_mm": wall, "governing_check": governing}
    return None


def rows_for(cfg, code, pressure_mpa):
    """Trace each stock trial, recording the failures that set the next wall."""
    minimum = pressure_minimum(cfg, code, pressure_mpa)
    wall_basis = "corroded wall; allowance deducted by strategy"
    if code not in NATIVE_CORROSION:
        wall_basis = "remaining wall supplied to nominal-wall pressure strategy"
    if code in FABRICATION:
        wall_basis += "; multiplied by (1 - fabrication tolerance fraction)"
    external_basis = ("corroded wall" if code in NATIVE_CORROSION - {DesignCode.ASME_B31_4}
                      else "nominal wall")
    if code == DesignCode.DNV_ST_F201:
        external_basis = "not implemented"
    common = {
        "code": code.value, "design_pressure_mpa": pressure_mpa,
        "external_pressure_mpa": cfg["external_pressure_mpa"],
        "pressure_basis": cfg["pressure_basis"], "pressure_wall_basis": wall_basis,
        "external_wall_basis": external_basis,
        "pressure_minimum_wall_mm": minimum["wall_mm"] if minimum else "",
        "pressure_minimum_set_by": minimum["governing_check"] if minimum else "",
        "strategy_source": Path(inspect.getfile(CODE_REGISTRY[code])).relative_to(ROOT).as_posix(),
    }
    rows = []
    prior_failed = ""
    for wall in cfg["stock_walls_mm"]:
        pressure = pressure_checks(cfg, code, wall, pressure_mpa)
        external = external_checks(cfg, code, wall, pressure_mpa)
        checks = {**pressure, **external}
        failed = ";".join(name for name, value in checks.items() if value > 1)
        complete = {"collapse", "propagation"} <= external.keys()
        status = "rejected" if failed else "selected" if complete else "incomplete check set"
        governing = max(checks, key=checks.get)
        row = dict(
            common, stock_wall_mm=wall,
            pressure_utilisation_percent=round(100 * max(pressure.values()), 1),
            failed_checks=failed, governing_check=governing,
            selection_status=status,
            selection_set_by=prior_failed or "pressure grid; stock list",
        )
        for name in ("collapse", "propagation"):
            row[name + "_utilisation_percent"] = (
                round(100 * external[name], 1) if name in external else "")
            row[name + "_status"] = (
                ("pass" if external[name] <= 1 else "fail")
                if name in external else "not implemented")
        if code in SUPPRESSED:
            row["collapse_status"] = "collapse not shown (#1920)"
        rows.append(row)
        if not failed:
            break
        prior_failed = failed
    if rows[-1]["selection_status"] == "rejected":
        rows[-1]["selection_status"] = "stock list exhausted"
    if minimum is None:
        rows[-1]["selection_status"] = "pressure minimum not found on grid"
    return rows


def source_digests(commit):
    """Fail closed if the running strategy bytes differ from the pinned commit."""
    analysis = ROOT / "src/digitalmodel/structural/analysis"
    sources = [analysis / "wall_thickness.py", *sorted((analysis / "wall_thickness_codes").glob("*.py"))]
    sources.append(ROOT / "scripts/reporting/build_riser_pipe_comparison.py")
    sources.append(CRITERIA)
    digests = {}
    for source in sources:
        relative = source.relative_to(ROOT).as_posix()
        pinned = subprocess.check_output(["git", "show", f"{commit}:{relative}"], cwd=ROOT)
        if source.read_bytes() != pinned:
            raise ValueError(f"Source digest mismatch: {relative}")
        digests[relative] = {"sha256": hashlib.sha256(pinned).hexdigest()}
    return digests


def _coefficient_sources(symbols, strategy):
    """Locate applied definitions; an executed table key is not an edition claim."""
    sources = {}
    for symbol in symbols:
        definition = type(strategy)
        if symbol.startswith("DesignFactors."):
            definition = DesignFactors
        elif symbol.startswith("PipeMaterial."):
            definition = PipeMaterial
        sources[symbol] = {
            "path": Path(inspect.getfile(definition)).relative_to(ROOT).as_posix(),
            "symbol": symbol,
        }
        if symbol.startswith("EDITION_FACTORS."):
            sources[symbol]["edition_key"] = strategy.edition_year
    return sources


def _check_provenance(code, name, strategy, entry, criteria):
    """Separate an unverified reference label from a check's evaluation status."""
    native = "propagation_buckling" if code == DesignCode.DNV_ST_F101 and name == "propagation" else name
    available = native in strategy.check_names
    status = "evaluated" if available else "not implemented"
    gaps = ["Publisher text was not consulted; coefficient values and applicability are unverified",
            "Edition and clause qualification are not established against publisher text"]
    if entry:
        gaps.append(criteria["qualification"])
        if "qualification" in entry:
            gaps.append(entry["qualification"])
    else:
        gaps.append("No comparison criterion record covers this route")
    if not available:
        gaps.append("Check is not implemented")
    if name == "collapse" and code in SUPPRESSED:
        status = "suppressed (#1920)"
        gaps.append("Known collapse solver fault (#1920); no collapse result is used")
    reference = entry.get("checks", {}).get(native, {})
    prefix = "Unqualified repository metadata: "
    symbols = list(COEFFICIENTS[code][name]) if available else []
    return {
        "coefficient_symbols": symbols,
        "coefficient_sources": _coefficient_sources(symbols, strategy),
        "implementation_method": "_check_" + native if available else None,
        "source_reference": {
            "standard": code.value, "title": entry.get("title", code.value),
            "edition": prefix + entry.get("reference_edition", entry.get("revision", "Not established")) if available else None,
            "clause": prefix + reference.get("section", "Clause number and title not established") if available else None,
            "metadata_source": entry.get("metadata_source") if available else None,
            "basis": "Repository metadata only; publisher source not consulted",
        },
        "criterion": reference.get("criterion", "Returned utilisation <= unity") if available else None,
        "limit": 1.0 if available else None,
        "criterion_applied": status == "evaluated",
        "evaluation_status": status, "qualification": "unqualified", "qualification_gaps": gaps,
    }


def check_provenance(digests):
    """Bind inherited criteria to strategy bytes and retain unresolved gaps."""
    criteria = json.loads(CRITERIA.read_text(encoding="utf-8"))
    inherited = {entry["module"]: entry for entry in criteria["codes"].values()}
    inventory = {}
    fields = ("title", "revision", "constants", "checks", "qualification",
              "reference_edition", "metadata_source", "module_sha256", "publisher")
    for code in DesignCode:
        strategy = CODE_REGISTRY[code]()
        source = Path(inspect.getfile(type(strategy))).relative_to(ROOT).as_posix()
        entry = inherited.get(source, {})
        if entry and entry["module_sha256"] != digests[source]["sha256"]:
            raise ValueError(f"Criterion module digest mismatch: {source}")
        inventory[code.value] = {
            "strategy_source": source,
            "inherited_criteria": {key: entry[key] for key in fields if key in entry},
            "checks": {name: _check_provenance(code, name, strategy, entry, criteria)
                       for name in (*PRESSURE_METHODS[code], "collapse", "propagation")},
        }
    return {"source": CRITERIA.relative_to(ROOT).as_posix(),
            "criteria_revision": criteria["criteria_revision"], "routes": inventory,
            "context": {key: value for key, value in criteria.items() if key != "codes"}}


def build(input_path=INPUT, output_dir=OUTPUT):
    """Write deterministic CSV and one manifest row per CSV row with provenance."""
    input_path, output_dir = Path(input_path), Path(output_dir)
    cfg = json.loads(input_path.read_text(encoding="utf-8"))
    validate(cfg)
    digests = source_digests(cfg["source_commit"])
    provenance = check_provenance(digests)
    rows = [row for code in DesignCode for pressure in cfg["pressures_mpa"]
            for row in rows_for(cfg, code, pressure)]
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=FIELDS, lineterminator="\n")
    writer.writeheader()
    for row in rows:
        writer.writerow({key: f"{value:.1f}" if isinstance(value, (float, int)) else value
                         for key, value in row.items()})
    payload = buffer.getvalue().encode()
    manifest = {
        "schema_version": 2, "source_commit": cfg["source_commit"],
        "strategy_sources": digests, "inputs": cfg,
        "criteria_source": provenance["source"], "criteria_revision": provenance["criteria_revision"],
        "check_provenance": provenance["routes"],
        "inherited_criteria_context": provenance["context"],
        "input_sha256": hashlib.sha256(input_path.read_bytes()).hexdigest(),
        "generator": Path(__file__).relative_to(ROOT).as_posix(),
        "generator_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "csv_sha256": hashlib.sha256(payload).hexdigest(),
        "rows": list(csv.DictReader(io.StringIO(payload.decode()))),
        "method": "first passing 0.1 mm nominal wall; exhaustive grid strictly below half the diameter; stock trials in stated order",
        "disposition_basis": "unrounded utilisations <= 1; display rounded to 0.1 percent; selection needs pressure, collapse AND propagation",
        "factor_basis": "one fixed default factor set per route; medium safety class; no per-condition factors; B31.4 ignores external pressure",
        "wall_basis": "nominal or corroded wall, per code, as implemented; pressure-only corrosion wrapper for four nominal routes",
        "fabrication_basis": "F101 and F201 deduct a fraction multiplicatively; default 0.125; other routes do not apply this deduction",
        "limitations": [
            "Implemented strategy results; standard conformity not established", "no edition claim",
            "pressure basis remains ASSUMED gauge pending confirmed pressure data",
            "collapse not shown (#1920) for ISO 13623 and PD 8010-2; no completed stock selection for these routes",
            "propagation not implemented for API RP 2RD, API STD 2RD and DNV-ST-F201; collapse also not implemented for DNV-ST-F201",
            "external checks retain their implemented wall bases; no extra corrosion deduction is imposed on nominal-wall external checks",
            "stock list is illustrative; applicability, material qualification, combined loading and availability are not established",
        ],
        "combined_loading_notes": {
            "API-RP-2RD": "Any moment-tension oval remains SCREENING and is never quoted as capacity; no combined check in this route",
            "API-RP-1111": "Any oval remains SCREENING and unquoted until registration has a passed review record; wording, values and table switch together",
            "scope": "No combined-loading values or ovals are generated here",
        },
        "revision_history": REVISION_HISTORY,
    }
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "minimum-wall.csv").write_bytes(payload)
    (output_dir / "minimum-wall.manifest.json").write_text(
        json.dumps(manifest, indent=2, allow_nan=False) + "\n", encoding="utf-8", newline="\n")
    return manifest


def main():
    """Generate the table and its manifest from explicit generic inputs."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=INPUT)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    args = parser.parse_args()
    build(args.input, args.output)


if __name__ == "__main__":
    main()
