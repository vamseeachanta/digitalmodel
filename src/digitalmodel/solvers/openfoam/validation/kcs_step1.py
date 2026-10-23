# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline Case 2.1 readback and admission checks; no solver is launched here."""

from __future__ import annotations

import csv
import hashlib
import io
import json
import math
import statistics
from pathlib import Path

from digitalmodel.solvers.openfoam.grid_convergence import grid_convergence

ROW_ID = "kcs_t2015_case_2_1_free_with_rudder"
V1_POLICY_SHA256 = "37177238094562a70a023a5732db4d224239c4ca608b9ff1f8f0bbdcb1ae29fd"
UNITS = {"time_s": "s", "ct": "1", "cp": "1", "cv": "1", "sinkage_cm": "cm", "trim_deg": "deg"}
SIGNS = {"sinkage_cm": "positive_up", "trim_deg": "positive_bow_up"}


def digest(value: dict) -> str:
    """Bind canonical reference/criteria content, rejecting nonfinite JSON."""
    data = json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False)
    return hashlib.sha256(data.encode()).hexdigest()


def _number(value, name: str, *, positive: bool = True) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be numeric")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")
    return float(value)


def reference_conditions(row: dict, froude: float) -> dict:
    """Keep Case 2.1 distinct from the fixed-bare campaign at every call."""
    if row.get("id") != ROW_ID:
        raise ValueError("step 1 requires the Case 2.1 reference row")
    if row.get("body_condition") != "free_to_heave_and_pitch" or row.get("appendages") != "rudder":
        raise ValueError("reference conditions require free heave/pitch and rudder")
    conditions = {
        "row_id": row["id"], "froude": froude,
        "body_condition": row["body_condition"], "appendages": row["appendages"],
        "wetted_surface_m2": row["wetted_surface"],
        "density_kg_m3": row["values"]["rho"]["value"],
        "nu_m2_s": row["values"]["nu"]["value"],
        "lpp_m": row["values"]["lpp"]["value"],
        "speed_m_s": row["values"]["velocity_model"]["value"],
    }
    for key in ("froude", "wetted_surface_m2", "density_kg_m3", "nu_m2_s", "lpp_m", "speed_m_s"):
        _number(conditions[key], key)
    return conditions


def validate_policy(policy: dict) -> None:
    """Reject missing or invalid predeclared criteria before reading results."""
    try:
        if (policy["schema_version"] != 1 or policy["criteria_id"] != "kcs-step1-v1"
                or policy["reference_row"] != ROW_ID or not policy["tolerance_basis"]):
            raise ValueError("unknown step 1 criteria")
        if policy["froude"] != 0.26:
            raise ValueError("v1 step 1 criteria support Fr 0.260 only")
        if set(policy["tolerances"]) != {"ct", "sinkage_cm", "trim_deg"}:
            raise ValueError("criteria need exactly three quantity tolerances")
        for key in ("ct", "sinkage_cm", "trim_deg"):
            spec = policy["tolerances"][key]
            expected_kind = "relative" if key == "ct" else "absolute"
            if spec["kind"] != expected_kind or spec["units"] != UNITS[key]:
                raise ValueError("quantity tolerance has wrong kind or units")
            _number(spec["limit"], key)
        for key in ("window_seconds", "maximum_relative_drift", "maximum_relative_std", "sampling_relative_tolerance"):
            _number(policy["force_history"][key], key)
        count = policy["force_history"]["minimum_samples_per_window"]
        if type(count) is not int or count < 2:
            raise ValueError("force windows need at least two samples")
        for key in ("p_rgh", "U"):
            _number(policy["residual_limits"][key], key)
        for quantity in ("sinkage_cm", "trim_deg"):
            for key in ("maximum_absolute_drift", "maximum_absolute_std"):
                _number(policy["motion_limits"][quantity][key], key)
        for key in ("maximum_y_plus", "maximum_dx_over_lpp", "maximum_dz_over_lpp"):
            _number(policy["mesh_limits"][key], key)
        if policy["gci"]["required_meshes"] != 3:
            raise ValueError("GCI requires three meshes")
        _number(policy["gci"]["maximum_fine_percent"], "GCI limit")
        for key in ("minimum_order", "maximum_order", "maximum_asymptotic_deviation"):
            _number(policy["gci"][key], key)
        if not policy["retention"]:
            raise ValueError("retention criteria missing")
        if digest(policy) != V1_POLICY_SHA256:
            raise ValueError("v1 criteria content changed; a reviewed revision is required")
    except (KeyError, TypeError) as exc:
        raise ValueError("convergence criteria missing or malformed") from exc


def _read_history(path: Path, limits: dict) -> tuple[list[dict], str]:
    data = Path(path).read_bytes()
    reader = csv.DictReader(io.StringIO(data.decode("utf-8")))
    if reader.fieldnames is None or len(reader.fieldnames) != len(UNITS) or set(reader.fieldnames) != set(UNITS):
        raise ValueError("history needs time_s, ct, cp, cv, sinkage_cm, trim_deg")
    rows = []
    for raw in reader:
        try:
            if None in raw or any(value is None for value in raw.values()):
                raise ValueError("ragged history row")
            row = {key: float(value) for key, value in raw.items()}
        except (TypeError, ValueError) as exc:
            raise ValueError("invalid numeric history row") from exc
        if not all(math.isfinite(x) for x in row.values()):
            raise ValueError("history contains nonfinite values")
        if min(row[k] for k in ("ct", "cp", "cv")) <= 0 or not math.isclose(row["ct"], row["cp"] + row["cv"], rel_tol=1e-8, abs_tol=1e-12):
            raise ValueError("signed coefficients must satisfy Ct = Cp + Cv > 0")
        rows.append(row)
    if len(rows) < 2:
        raise ValueError("history is too short")
    steps = [b["time_s"] - a["time_s"] for a, b in zip(rows, rows[1:])]
    if min(steps) <= 0 or any(not math.isclose(dt, steps[0], rel_tol=limits["sampling_relative_tolerance"], abs_tol=1e-9) for dt in steps):
        raise ValueError("history needs regular strictly increasing physical time")
    return rows, hashlib.sha256(data).hexdigest()


def _force_windows(rows: list[dict], limits: dict) -> tuple[dict, list[dict]]:
    window = limits["window_seconds"]
    end = rows[-1]["time_s"]
    if end - rows[0]["time_s"] < 2 * window:
        raise ValueError("history does not cover both declared windows")
    previous = [r for r in rows if end - 2 * window < r["time_s"] <= end - window]
    last = [r for r in rows if end - window < r["time_s"] <= end]
    if min(len(previous), len(last)) < limits["minimum_samples_per_window"]:
        raise ValueError("too few samples in declared force windows")
    parts = {}
    for key in ("cp", "cv"):
        means = [statistics.fmean(r[key] for r in part) for part in (previous, last)]
        drift = abs(means[1] - means[0]) / abs(means[1])
        fluctuation = max(statistics.pstdev(r[key] for r in part) / abs(mean) for part, mean in zip((previous, last), means))
        parts[key] = {"relative_drift": drift, "relative_std": fluctuation,
                      "passed": drift <= limits["maximum_relative_drift"] and fluctuation <= limits["maximum_relative_std"]}
    return {"passed": all(p["passed"] for p in parts.values()), "components": parts,
            "window_seconds": window, "samples": [len(previous), len(last)]}, last


def _motion_settling(rows: list[dict], policy: dict) -> dict:
    window, end = policy["force_history"]["window_seconds"], rows[-1]["time_s"]
    previous = [r for r in rows if end - 2 * window < r["time_s"] <= end - window]
    last = [r for r in rows if end - window < r["time_s"] <= end]
    result = {}
    for key, limit in policy["motion_limits"].items():
        drift = abs(statistics.fmean(r[key] for r in previous) - statistics.fmean(r[key] for r in last))
        std = max(statistics.pstdev(r[key] for r in part) for part in (previous, last))
        result[key] = {"absolute_drift": drift, "absolute_std": std, "units": UNITS[key],
                       "passed": drift <= limit["maximum_absolute_drift"] and std <= limit["maximum_absolute_std"]}
    return {"passed": all(r["passed"] for r in result.values()), "quantities": result}


def _diagnostics(meta: dict, policy: dict, row: dict) -> dict:
    try:
        residuals, yp, surface = meta["residuals"], meta["y_plus"], meta["free_surface"]
        for key in ("p_rgh", "U"):
            _number(residuals[key], key, positive=False)
        for key in ("min", "mean", "max"):
            _number(yp[key], "y+", positive=False)
        if not yp["min"] <= yp["mean"] <= yp["max"]:
            raise ValueError("unordered y+ summary")
        for key in ("dx_m", "dz_m"):
            _number(surface[key], key)
        lpp = _number(row["values"]["lpp"]["value"], "Lpp")
        mesh = policy["mesh_limits"]
        passed = (yp["max"] <= mesh["maximum_y_plus"]
                  and surface["dx_m"] / lpp <= mesh["maximum_dx_over_lpp"]
                  and surface["dz_m"] / lpp <= mesh["maximum_dz_over_lpp"]
                  and all(residuals[k] <= limit for k, limit in policy["residual_limits"].items()))
        return {"passed": passed, "residuals": residuals, "y_plus": yp,
                "free_surface": surface, "basis": "observed run metadata"}
    except (KeyError, TypeError) as exc:
        raise ValueError("observed residuals, y+ and free-surface resolution are required") from exc


def reference_is_qualified(row: dict) -> bool:
    """Source qualification is separate from diagnostic numerical agreement."""
    try:
        series = row["series"]
        evidence = series["readback"]
        sha = evidence["source_sha256"]
        return bool(
            series["units_status"] == "verified" and evidence["status"] == "verified"
            and evidence["sheet_name"] and isinstance(sha, str) and len(sha) == 64
            and all(c in "0123456789abcdef" for c in sha)
            and evidence["table_sha256"] == digest({k: series[k] for k in ("points", "units", "sign_convention")})
            and series["sign_convention"] == SIGNS
            and all(evidence["cells"][k] for k in ("ct", "sinkage_cm", "trim_deg"))
            and all(series["page_locators"][k] for k in ("ct", "sinkage_cm", "trim_deg"))
            and all(series[k] for k in ("source_document", "source_url", "published_data_file", "licence_note"))
        )
    except (KeyError, TypeError, ValueError):
        return False


def _check_basis(meta: dict, row: dict, policy: dict) -> dict:
    try:
        if (meta["schema_version"] != 1 or meta["evidence_kind"] not in ("synthetic", "native")
                or meta["mesh"] not in ("coarse", "medium", "fine")):
            raise ValueError("unknown run metadata schema, kind or mesh")
        if meta["reference_sha256"] != digest(row) or meta["criteria_sha256"] != digest(policy):
            raise ValueError("reference or criteria digest mismatch")
        if (row["body_condition"] != "free_to_heave_and_pitch" or row["appendages"] != "rudder"
                or meta["conditions"] != reference_conditions(row, policy["froude"])):
            raise ValueError("run conditions do not match Case 2.1")
        if meta["units"] != UNITS or meta["sign_convention"] != SIGNS:
            raise ValueError("history units or signs do not match Case 2.1")
        if row["series"]["sign_convention"] != SIGNS or row["reynolds_applies_at_froude"] != policy["froude"]:
            raise ValueError("reference signs or Reynolds applicability do not match Case 2.1")
        if row["series"]["units"] != {k: UNITS.get(k, "1") for k in ("froude", "ct", "sinkage_cm", "trim_deg")}:
            raise ValueError("reference units need a reviewed conversion")
        points = [p for p in row["series"]["points"] if math.isclose(p["froude"], policy["froude"], abs_tol=1e-9, rel_tol=0)]
        if len(points) != 1:
            raise ValueError("reference table needs one matching Froude row")
        for key in ("ct", "sinkage_cm", "trim_deg"):
            if not math.isfinite(points[0][key]):
                raise ValueError("reference table has nonfinite values")
        return points[0]
    except (KeyError, TypeError) as exc:
        raise ValueError("reference table or run basis is incomplete") from exc


def compare_run(history: Path, metadata: dict, row: dict, policy: dict) -> dict:
    """Read the actual history once; compare at full precision before reporting.

    CSV coefficients are signed full-hull values normalized with the declared
    condition tuple. The schema checks consistency, not input authenticity.
    """
    validate_policy(policy)
    point = _check_basis(metadata, row, policy)
    rows, history_sha = _read_history(history, policy["force_history"])
    if metadata.get("history_sha256") != history_sha:
        raise ValueError("history digest does not match run metadata")
    force, last = _force_windows(rows, policy["force_history"])
    motion = _motion_settling(rows, policy)
    diagnostics = _diagnostics(metadata, policy, row)
    quantities = {}
    for key, tolerance in policy["tolerances"].items():
        actual, expected = statistics.fmean(r[key] for r in last), point[key]
        bound = tolerance["limit"] * abs(expected) if tolerance["kind"] == "relative" else tolerance["limit"]
        quantities[key] = {"computed": actual, "reference": expected, "units": UNITS[key],
                           "reference_minus_computed": expected - actual,
                           "absolute_tolerance": bound, "passed": abs(actual - expected) <= bound}
    passed = all(q["passed"] for q in quantities.values()) and force["passed"] and motion["passed"] and diagnostics["passed"]
    smoke_passed = reference_is_qualified(row) and metadata["evidence_kind"] == "native" and passed
    return {"schema_version": 1, "evidence_kind": metadata["evidence_kind"],
            "mesh": metadata["mesh"], "comparison_passed": passed, "qualified": False,
            "coarse_smoke_passed": smoke_passed and metadata["mesh"] == "coarse",
            "reference_qualified": reference_is_qualified(row), "reference_sha256": digest(row),
            "criteria_sha256": digest(policy), "history_sha256": history_sha,
            "quantities": quantities, "force_convergence": force, "diagnostics": diagnostics,
            "motion_convergence": motion,
            "coefficient_means": {k: statistics.fmean(r[k] for r in last) for k in ("ct", "cp", "cv")},
            "component_validation": "V2a/V2b fixed-bare thresholds do not qualify Case 2.1",
            "grid_uncertainty": "not established by a single run"}


def load_reference(path: Path) -> dict:
    """Load exactly one Case 2.1 row from the canonical reference fixture."""
    import yaml

    try:
        data = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
        rows = [r for r in data["reference_rows"] if r["id"] == ROW_ID]
        if len(rows) != 1:
            raise ValueError("reference table needs exactly one Case 2.1 row")
        points = rows[0]["series"]["points"]
        if len(points) != 6 or len({p["froude"] for p in points}) != 6:
            raise ValueError("reference table needs six distinct Case 2.1 rows")
        for point in points:
            for key in ("froude", "ct", "sinkage_cm", "trim_deg"):
                if isinstance(point[key], bool) or not math.isfinite(point[key]):
                    raise ValueError("reference table has invalid values")
        return rows[0]
    except (OSError, KeyError, TypeError, yaml.YAMLError) as exc:
        raise ValueError("reference table is missing or malformed") from exc


def load_policy(path: Path) -> dict:
    try:
        policy = json.loads(Path(path).read_text(encoding="utf-8"))
        validate_policy(policy)
        return policy
    except (OSError, TypeError, json.JSONDecodeError) as exc:
        raise ValueError("criteria are missing or malformed") from exc


def require_run_admission(next_mesh: str, reference_path: Path, criteria_path: Path,
                          coarse_history: Path | None = None,
                          coarse_metadata: Path | None = None, *,
                          case: str = "2.1", condition_id: str | None = None) -> dict:
    """Fail closed before requesting a step 1 run; never launch a solver.

    This validates supplied evidence, not the authenticity of external files.
    A future launcher must call this at its execution boundary. It does not
    retrofit the separate fixed-bare study or provide a run transaction.
    """
    if case == "2.10":
        from .kcs_step3 import require_run_admission as head_wave_admission
        return head_wave_admission(next_mesh, reference_path, criteria_path,
                                   coarse_history, coarse_metadata, condition_id=condition_id)
    if case != "2.1" or condition_id is not None:
        raise ValueError("unknown case or condition for step 1 admission")
    if next_mesh not in ("coarse", "medium", "fine"):
        raise ValueError("unknown requested mesh")
    row = load_reference(reference_path)
    if not reference_is_qualified(row):
        raise ValueError("reference source readback and units are not verified")
    policy = load_policy(criteria_path)
    try:
        reference_conditions(row, policy["froude"])
    except (KeyError, TypeError) as exc:
        raise ValueError("reference conditions are incomplete") from exc
    result = {"admitted": True, "mesh": next_mesh,
              "reference_sha256": digest(row), "criteria_sha256": digest(policy)}
    if next_mesh == "coarse":
        return result
    if coarse_history is None or coarse_metadata is None:
        raise ValueError("passing coarse smoke evidence is required")
    try:
        meta = json.loads(Path(coarse_metadata).read_text(encoding="utf-8"))
        if meta["mesh"] != "coarse" or meta["evidence_kind"] != "native":
            raise ValueError("smoke evidence must be native and coarse")
        report = compare_run(coarse_history, meta, row, policy)
    except (OSError, KeyError, TypeError, json.JSONDecodeError) as exc:
        raise ValueError("coarse smoke evidence is missing or malformed") from exc
    if not report["coarse_smoke_passed"]:
        raise ValueError("coarse smoke comparison or convergence criteria failed")
    result["coarse_history_sha256"] = report["history_sha256"]
    return result


def compare_triplet(histories: list[Path], metadata: list[dict], row: dict, policy: dict) -> dict:
    """Report GCI only for a documented systematic fine/medium/coarse family."""
    if len(histories) != 3 or len(metadata) != 3:
        raise ValueError("triplet readback needs exactly three completed meshes")
    try:
        order = {"fine": 0, "medium": 1, "coarse": 2}
        if {m["mesh"] for m in metadata} != set(order):
            raise ValueError("triplet needs one fine, medium and coarse mesh")
        pairs = sorted(zip(histories, metadata), key=lambda pair: order[pair[1]["mesh"]])
        metadata = [meta for _, meta in pairs]
        for key in ("mesh_family", "numerics"):
            if not metadata[0][key] or len({m[key] for m in metadata}) != 1:
                raise ValueError("mesh family and numerics must be identical")
        if any(m["refinement_axes"] != "xyz" for m in metadata):
            raise ValueError("three-dimensional systematic refinement is required")
        h = [_number(m["h_m"], "representative cell size") for m in metadata]
        reports = [compare_run(path, meta, row, policy) for path, meta in pairs]
        if any(not r["force_convergence"]["passed"] or not r["motion_convergence"]["passed"] or not r["diagnostics"]["passed"] for r in reports):
            raise ValueError("GCI requires passing convergence and mesh diagnostics on every grid")
        for a, b in zip(metadata, metadata[1:]):
            for key in ("dx_m", "dz_m"):
                if not math.isclose(b["free_surface"][key] / a["free_surface"][key], b["h_m"] / a["h_m"], rel_tol=1e-6):
                    raise ValueError("free-surface spacing does not match systematic refinement")
        gci = {key: three_grid_gci(h, [r["coefficient_means"][key] for r in reports],
                                 policy["gci"]["minimum_order"], policy["gci"]["maximum_order"],
                                 policy["gci"]["maximum_asymptotic_deviation"]) for key in ("ct", "cp", "cv")}
    except (KeyError, TypeError) as exc:
        raise ValueError("systematic mesh evidence is incomplete") from exc
    gci_passed = all(g["gci_fine_pct"] <= policy["gci"]["maximum_fine_percent"] for g in gci.values())
    qualified = (gci_passed and reference_is_qualified(row)
                 and all(r["comparison_passed"] and r["evidence_kind"] == "native" for r in reports))
    return {"schema_version": 1, "qualified": qualified, "gci_passed": gci_passed,
            "qualification_scope": "resistance grid uncertainty; motion comparison and settling only",
            "gci": gci, "runs": reports,
            "mesh_evidence": [{k: m[k] for k in ("mesh", "h_m", "mesh_family", "numerics", "refinement_axes")} for m in metadata]}


def three_grid_gci(h: list[float], values: list[float], minimum_order: float = .5,
                   maximum_order: float = 3, maximum_asymptotic_deviation: float = .05) -> dict:
    """Restrict the existing GCI procedure to a convergent constant-r triplet.

    Inputs are fine, medium, coarse. Systematic mesh-family evidence is checked
    separately by ``compare_triplet``. The upstream implementation owns the
    three-grid safety factor; this wrapper introduces no alternative factor.
    """
    if len(h) != 3 or len(values) != 3:
        raise ValueError("GCI requires exactly three meshes")
    if not all(math.isfinite(x) and x > 0 for x in [*h, *values]):
        raise ValueError("GCI requires positive finite sizes and coefficients")
    if not h[0] < h[1] < h[2]:
        raise ValueError("GCI sizes must increase fine to coarse")
    r = h[1] / h[0]
    if r < 1.1 or not math.isclose(r, h[2] / h[1], rel_tol=1e-6):
        raise ValueError("GCI requires equal systematic refinement ratios")
    e21, e32 = values[1] - values[0], values[2] - values[1]
    if e21 == 0 or e32 == 0 or e32 / e21 <= 1:
        raise ValueError("GCI series is degenerate, oscillatory or divergent")
    result = grid_convergence(h, values).to_dict()
    if not all(math.isfinite(result[k]) for k in ("p", "f_ext", "gci_fine_pct")):
        raise ValueError("GCI arithmetic is not finite")
    if not minimum_order <= result["p"] <= maximum_order:
        raise ValueError("GCI observed order lies outside the predeclared range")
    if abs(result["asymptotic_ratio"] - 1) > maximum_asymptotic_deviation:
        raise ValueError("GCI asymptotic ratio lies outside the declared range")
    result["method_source"] = {
        "title": "Procedure for Estimation and Reporting of Uncertainty Due to Discretization in CFD Applications",
        "url": "https://www.asme.org/wwwasmeorg/media/resourcefiles/shop/journals/jfenumaccuracy.pdf",
        "section": "Steps 2 to 5, PDF pages 5 to 7",
        "ittc": "7.5-03-01-01, revision 05, 2024, Convergence Studies",
    }
    return result
