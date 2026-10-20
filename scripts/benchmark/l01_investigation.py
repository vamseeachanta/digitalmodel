"""Offline L01 request generation and conditional comparison of normalized exports.

No solver imports, native deck issuance, subprocesses, or licence operations.
See docs/domains/orcawave/L01_aqwa_benchmark/solver_request.html for the contract.
"""
import argparse
import cmath
import hashlib
import json
import math
from pathlib import Path
import re

# Issue 714 scope and API_EXECUTION_SUMMARY.md: case-count diagnostic ladder.
STAGES = ((4, 4), (10, 9), (20, 9))
# API_EXECUTION_SUMMARY.md, Hydrodynamic Data: six motion DOFs and 6 x 6 matrices.
DOFS = 6
BASIS_FIELDS = {
    "geometry", "mesh", "mass", "inertia", "centre_of_gravity", "reference_point",
    "water_depth", "density", "gravity", "units", "axes", "heading_convention",
    "phase_convention", "normalization", "dof_order", "solve_type", "source_rights",
    "restraints",
}


def number(value, *, positive=False):
    if type(value) not in (int, float) or not math.isfinite(value):
        raise ValueError("finite numeric value required")
    if positive and value <= 0:
        raise ValueError("positive value required")
    return value


def text(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("nonempty citation or declaration required")
    return value


def vector(values, length, *, positive=False):
    if not isinstance(values, list) or len(values) != length:
        raise ValueError("invalid vector dimensions")
    return [number(v, positive=positive) for v in values]


def matrix(values, *, positive=False):
    if not isinstance(values, list) or len(values) != DOFS:
        raise ValueError("invalid matrix dimensions")
    return [vector(row, DOFS, positive=positive) for row in values]


def validate_config(config):
    frequencies = config["frequencies_rad_s"]
    headings = config["headings_deg"]
    vector(frequencies, len(frequencies), positive=True)
    vector(headings, len(headings))
    if len(frequencies) < STAGES[-1][0] or len(headings) < STAGES[-1][1]:
        raise ValueError("grid cannot supply the diagnostic ladder")
    if any(b <= a for a, b in zip(frequencies, frequencies[1:])):
        raise ValueError("frequencies must be strictly increasing")
    # Canonical headings, no implicit wrapping; 360 degrees is one revolution.
    # Source: multi_solver_comparator.py, _compute_phase_statistics wrap formula.
    if len(set(headings)) != len(headings) or any(not 0 <= h < 360 for h in headings):
        raise ValueError("headings must be unique canonical degrees")
    gap = number(config["max_relative_gap"], positive=True)
    text(config["sampling_citation"])
    # Source: benchmark_abscissa.py MAX_RELATIVE_GAP / half-power-band contract.
    if any((b - a) / a > gap for a, b in zip(frequencies, frequencies[1:])):
        raise ValueError("sampling gap exceeds the supplied criterion")
    threads = config["threads"]
    if (not isinstance(threads, list) or not threads
            or any(type(t) is not int or t <= 0 for t in threads)
            or len(set(threads)) != len(threads)):
        raise ValueError("unique positive integer thread counts required")
    number(config["budget_minutes"], positive=True)
    text(config["resource_citation"])
    basis = config["basis"]
    if not isinstance(basis, dict) or set(basis) != BASIS_FIELDS:
        raise ValueError("complete explicit physical basis required")
    for value in basis.values():
        text(value)
    criteria = config["criteria"]
    for key in ("citation", "aqg_band"):
        text(criteria[key])
    number(criteria["max_normalized_error"], positive=True)
    vector(criteria["rao_scales"], DOFS, positive=True)
    vector(criteria["phase_amplitude_floors"], DOFS, positive=True)
    for key in ("added_mass_scales", "damping_scales"):
        matrix(criteria[key], positive=True)


def subset(values, count):
    """Deterministic endpoint-inclusive indices; no invented frequency values."""
    return [values[i * (len(values) - 1) // (count - 1)] for i in range(count)]


def build_request(config):
    validate_config(config)
    runs = []
    for nf, nh in STAGES:
        for threads in config["threads"]:
            # Issue 714 requires DivideNonPlanarPanels sensitivity, not a default fix.
            for divide in (False, True):
                runs.append({
                    "cases": nf * nh, "threads": threads,
                    "divide_non_planar_panels": divide,
                    "frequencies_rad_s": subset(config["frequencies_rad_s"], nf),
                    "headings_deg": subset(config["headings_deg"], nh),
                    "budget_minutes": config["budget_minutes"],
                    "scientific_comparison": False,
                })
    return {
        "config_sha256": digest(config), "scaling_runs": runs,
        "comparison_grid": {key: config[key] for key in
                            ("frequencies_rad_s", "headings_deg")},
        "basis": config["basis"], "criteria": config["criteria"],
        "sampling_citation": config["sampling_citation"],
        "max_relative_gap": config["max_relative_gap"],
        "resource_citation": config["resource_citation"],
        "estimated_licensed_minutes": "NOT_EVALUATED",
        "native_inputs": "NOT_EVALUATED", "engineering_qualification": "NOT_EVALUATED",
    }


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, allow_nan=False,
                                     separators=(",", ":")).encode("utf-8")).hexdigest()


def validate_export(config, payload, solver):
    if payload["solver"] != solver or payload["source"] != "solver":
        raise ValueError("distinct named solver exports required; no placeholders")
    if payload["native_readback"] is not True:
        raise ValueError("native readback attestation required")
    if not re.fullmatch(r"[0-9a-f]{64}", text(payload["native_sha256"])):
        raise ValueError("native artifact SHA-256 required")
    if payload["basis"] != config["basis"]:
        raise ValueError("physical basis or conventions differ")
    raos, matrices = {}, {}
    for row in payload["raos"]:
        key = (number(row["frequency_rad_s"], positive=True), number(row["heading_deg"]))
        if key in raos:
            raise ValueError("duplicate RAO key")
        values = row["values"]
        if not isinstance(values, list) or len(values) != DOFS:
            raise ValueError("invalid RAO dimensions")
        # Complex pair cardinality is algebraic, not an engineering constant.
        raos[key] = [complex(*vector(pair, 2)) for pair in values]
    for row in payload["matrices"]:
        key = number(row["frequency_rad_s"], positive=True)
        if key in matrices:
            raise ValueError("duplicate matrix frequency")
        matrices[key] = {name: matrix(row[name]) for name in ("added_mass", "damping")}
    expected = {(f, h) for f in config["frequencies_rad_s"] for h in config["headings_deg"]}
    if set(raos) != expected or set(matrices) != set(config["frequencies_rad_s"]):
        raise ValueError("full declared grid coverage required; no inner joins")
    return raos, matrices


def compare(config, aqwa, orcawave):
    """Conditional numerical band check; supplied evidence is not authenticated."""
    validate_config(config)
    a_raos, a_matrices = validate_export(config, aqwa, "AQWA")
    b_raos, b_matrices = validate_export(config, orcawave, "OrcaWave")
    criteria = config["criteria"]
    errors, phases = [], []
    excluded = 0
    for key in sorted(a_raos):
        for dof, (a, b) in enumerate(zip(a_raos[key], b_raos[key])):
            # Supplied engineering scale, never the measured response denominator.
            # Criteria explicitly define this pointwise norm in solver_request.html.
            errors.append({"quantity": "rao", "frequency_rad_s": key[0],
                           "heading_deg": key[1], "component": dof,
                           "normalized_error": abs(a - b) / criteria["rao_scales"][dof]})
            if min(abs(a), abs(b)) <= criteria["phase_amplitude_floors"][dof]:
                excluded += 1
            else:
                # Circular phase, as in multi_solver_comparator.py phase statistics.
                phase = abs(math.degrees(math.remainder(
                    cmath.phase(b) - cmath.phase(a), math.tau)))
                if not math.isfinite(phase):
                    raise ValueError("phase calculation overflow")
                phases.append(phase)
    for frequency in sorted(a_matrices):
        for quantity in ("added_mass", "damping"):
            for i in range(DOFS):
                for j in range(DOFS):
                    error = abs(a_matrices[frequency][quantity][i][j]
                                - b_matrices[frequency][quantity][i][j])
                    errors.append({"quantity": quantity, "frequency_rad_s": frequency,
                                   "component": [i, j], "normalized_error":
                                   error / criteria[f"{quantity}_scales"][i][j]})
    if any(not math.isfinite(row["normalized_error"]) for row in errors):
        raise ValueError("comparison overflow")
    governing = max(errors, key=lambda row: row["normalized_error"])
    failures = [row for row in errors
                if row["normalized_error"] > criteria["max_normalized_error"]]
    return {
        "config_sha256": digest(config), "aqwa_export_sha256": digest(aqwa),
        "orcawave_export_sha256": digest(orcawave), "criteria": criteria,
        "band_check": "OUTSIDE_BAND" if failures else "WITHIN_BAND",
        "governing": governing, "failures": failures, "rao_samples": len(a_raos),
        "matrix_frequencies": len(a_matrices), "phase_not_evaluated": excluded,
        "max_phase_difference_deg": max(phases) if phases else None,
        "phase_role": "diagnostic_only", "engineering_qualification": "NOT_EVALUATED",
        "evidence_status": "caller_supplied_not_independently_authenticated",
    }


def unique_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"), object_pairs_hook=unique_pairs)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("prepare", "compare"))
    parser.add_argument("config", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--aqwa", type=Path)
    parser.add_argument("--orcawave", type=Path)
    args = parser.parse_args(argv)
    try:
        config = read_json(args.config)
        if args.mode == "prepare":
            result = build_request(config)
        else:
            if args.aqwa is None or args.orcawave is None:
                raise ValueError("both normalized solver exports required")
            result = compare(config, read_json(args.aqwa), read_json(args.orcawave))
        serialized = json.dumps(result, indent=2, allow_nan=False) + "\n"
        # Serialize before exclusive creation so invalid numbers leave no partial file.
        with args.output.open("x", encoding="utf-8", newline="\n") as stream:
            stream.write(serialized)
        return int(result.get("band_check") == "OUTSIDE_BAND")
    except (ValueError, KeyError, TypeError, OSError, OverflowError) as error:
        parser.exit(1, f"Investigation refused: {error}\n")


if __name__ == "__main__":
    raise SystemExit(main())
