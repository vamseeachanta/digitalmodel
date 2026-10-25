# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Validate screening provenance, units, references and physical input bounds."""

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator, FormatChecker


ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "examples/riser-campaign"
BASIS = CAMPAIGN / "basis.md"
PROVENANCE_SOURCES = {
    "field-development rows": None,
    "FPSO particulars": "public-fpso",
    "semi-submersible particulars": "public-semi",
    "spar particulars": "public-spar",
    "TLP particulars": "public-tlp",
    "floater RAOs": "screening-basis",
    "metocean": "screening-basis",
    "riser properties": "screening-basis",
    "geometry and numerical settings": "screening-basis",
}


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """Reject duplicate JSON keys instead of silently discarding input."""
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate JSON key: {key}")
        result[key] = value
    return result


def validate_manifest(data: dict[str, Any], schema: dict[str, Any] | None = None,
                      basis_payload: bytes | None = None) -> None:
    """Reject schema, provenance, coverage and geometry inconsistencies.

    Args:
        data: Decoded manifest whose numeric groups carry an explicit basis.
        schema: Optional frozen schema mapping; default is the campaign schema.
        basis_payload: Optional frozen authored basis; default is the campaign basis.

    Raises:
        ValueError: A shape, reference, unit or physical bound is invalid.
    """
    json.dumps(data, allow_nan=False)
    if schema is None:
        schema = json.loads((CAMPAIGN / "manifest.schema.json").read_text(encoding="utf-8"))
    Draft202012Validator.check_schema(schema)
    errors = sorted(Draft202012Validator(schema, format_checker=FormatChecker()).iter_errors(data),
                    key=lambda e: str(e.path))
    if errors:
        raise ValueError(f"manifest schema: {errors[0].json_path}: {errors[0].message}")
    sources = {s["id"] for s in data["sources"]}
    if len(sources) != len(data["sources"]):
        raise ValueError("duplicate source id")
    _validate_provenance(data, BASIS.read_bytes() if basis_payload is None else basis_payload)
    raos = {r["id"]: r for r in data["rao_sets"]}
    if len(raos) != len(data["rao_sets"]):
        raise ValueError("duplicate RAO set id")
    for group in [data["settings"], *data["parameter_ranges"], *raos.values()]:
        if group["source_id"] not in sources:
            raise ValueError("unresolved source reference")
    for rao in raos.values():
        _validate_rao_generation(rao)
        periods = [r["period_s"] for r in rao["rows"]]
        if periods != sorted(set(periods)):
            raise ValueError("RAO periods must be strictly increasing")
    for group in data["parameter_ranges"]:
        if any(bounds[0] > bounds[1] for bounds in group["bounds"].values()):
            raise ValueError("reversed parameter range")
    if {r["floater_type"] for r in data["parameter_ranges"]} != {"FPSO", "semi-submersible", "spar", "TLP"}:
        raise ValueError("parameter ranges must uniquely cover every floater type")
    ids, coverage = set(), set()
    for case in data["cases"]:
        if case["case_id"] in ids:
            raise ValueError("duplicate case id")
        ids.add(case["case_id"])
        coverage.add((case["floater_type"], case["configuration"], case["water_depth_m"]))
        _validate_case(case, data, sources, raos)
    for floater in ("FPSO", "semi-submersible", "spar", "TLP"):
        for configuration in ("SCR", "lazy-wave"):
            depths = {d for f, c, d in coverage if (f, c) == (floater, configuration)}
            if len(depths) < 3:
                raise ValueError("matrix requires three depths for every floater and configuration")


def _validate_provenance(data: dict[str, Any], basis_payload: bytes) -> None:
    sources = {s["id"]: s for s in data["sources"]}
    table = data["data_provenance"]
    if len(table) != len(PROVENANCE_SOURCES) or {r["input_group"] for r in table} != set(PROVENANCE_SOURCES):
        raise ValueError("provenance table must uniquely cover every input group")
    for row in table:
        expected = PROVENANCE_SOURCES[row["input_group"]]
        if row["source_id"] != expected or (expected is not None and expected not in sources):
            raise ValueError("provenance source reference mismatch")
        if expected is None:
            if row["status"] != "NOT_USED" or row["sha256"] is not None:
                raise ValueError("unused field-development data cannot carry an input digest")
        elif row["status"] != "ASSUMED" or any(
            row[field] != sources[expected][field] for field in ("sha256", "licence_or_terms", "date", "version")
        ):
            raise ValueError("provenance metadata differs from its source")
    if hashlib.sha256(basis_payload).hexdigest() != sources["screening-basis"]["sha256"]:
        raise ValueError("authored basis digest mismatch")


def _validate_rao_generation(rao: dict[str, Any]) -> None:
    method = rao["generation"]
    if len(method["period_factors"]) != len(rao["rows"]):
        raise ValueError("synthetic RAO generation requires a factor for every period")
    for row, factor in zip(rao["rows"], method["period_factors"]):
        expected = [a * factor * method["size_factor"] for a in method["base_amplitudes"]]
        if any(not math.isclose(a, b, rel_tol=1e-12, abs_tol=1e-12)
               for a, b in zip(row["amplitudes"], expected)) or row["phases_deg"] != [0] * 6:
            raise ValueError("synthetic RAO table differs from recorded generation inputs")


def _validate_case(case: dict[str, Any], data: dict[str, Any], sources: set[str],
                   raos: dict[str, dict[str, Any]]) -> None:
    groups = [case, case["pipe"], case["principal_particulars"], case["current_profile"],
              *case["offset_cases"], *case["sea_states"]]
    if case["buoyancy"] is not None:
        groups.append(case["buoyancy"])
    if any(group["source_id"] not in sources for group in groups):
        raise ValueError("unresolved case source reference")
    if case["rao_set_id"] not in raos:
        raise ValueError("unresolved RAO set reference")
    rao = raos[case["rao_set_id"]]
    if (rao["floater_type"], rao["size_class"]) != (case["floater_type"], case["size_class"]):
        raise ValueError("RAO type and size mismatch")
    pipe = case["pipe"]
    if 2 * pipe["wall_thickness_m"] >= pipe["outer_diameter_m"]:
        raise ValueError("pipe wall leaves no bore")
    if case["principal_particulars"]["draft_m"] >= case["water_depth_m"]:
        raise ValueError("floater draft reaches seabed")
    bounds = next(g["bounds"] for g in data["parameter_ranges"] if g["floater_type"] == case["floater_type"])
    if any(not low <= case["principal_particulars"][name] <= high for name, (low, high) in bounds.items()):
        raise ValueError("particulars outside declared range")
    buoyancy = case["buoyancy"]
    if case["configuration"] == "SCR" and buoyancy is not None:
        raise ValueError("SCR cannot carry lazy-wave buoyancy")
    if case["configuration"] == "lazy-wave":
        if buoyancy is None or not 0 < buoyancy["sag_fraction"] < buoyancy["hog_fraction"] < 1:
            raise ValueError("lazy-wave sag and hog fractions are invalid")
        if buoyancy["module_density_kg_m3"] >= data["settings"]["seawater_density_kg_m3"]:
            raise ValueError("buoyancy module must be lighter than seawater")
    points = case["current_profile"]["points"]
    depths = [p["depth_fraction"] for p in points]
    if depths != sorted(set(depths)) or depths[0] != 0 or depths[-1] != 1:
        raise ValueError("current profile must span surface to seabed in increasing order")
    for name in ("offset_cases", "sea_states"):
        names = [r["id"] for r in case[name]]
        if len(names) != len(set(names)):
            raise ValueError(f"duplicate {name} id")
    if case["offset_cases"][0]["x_m"] != 0:
        raise ValueError("first offset must be the nominal geometry")
    for state in case["sea_states"]:
        if state["direction_deg"] != rao["heading_deg"]:
            raise ValueError("wave direction lacks an RAO heading")
        if not rao["rows"][0]["period_s"] <= state["period_s"] <= rao["rows"][-1]["period_s"]:
            raise ValueError("wave period outside RAO support")


def load_manifest(path: Path | str) -> dict[str, Any]:
    """Read strict UTF-8 JSON and return a validated campaign."""
    data: dict[str, Any] = json.loads(Path(path).read_text(encoding="utf-8"), object_pairs_hook=unique_object)
    validate_manifest(data)
    return data


def main() -> None:
    """Validate a manifest without generating inputs or importing solvers."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path, nargs="?", default=CAMPAIGN / "manifest.json")
    args = parser.parse_args()
    data = load_manifest(args.manifest)
    print(f"Validated {len(data['cases'])} screening cases; all engineering inputs are ASSUMED")


if __name__ == "__main__":
    main()
