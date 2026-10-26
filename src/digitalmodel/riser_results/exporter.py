# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Convert adapter snapshots into validated, traceable engineering results.

Basis: structural/analysis/wall_thickness_codes/api_rp_2rd.py, ApiRp2rdStrategy
labels legacy WSD allowable C_f*(2/3)*SMYS. Native equivalent stresses must include
membrane, pressure and bending on the checked section, reconciled during readback.
Extreme C_f=1.2 and functional C_f=1 are screening assumptions recorded in
docs/domains/risers/results-exchange/basis-v1.md, not verified standard factors.
FAT80: public comparison, section 3.2.1 / Figure 16, 80 MPa range at
2 million cycles, m=3; one-slope extension and SCF are screening assumptions.
Annualisation assumes an 8760-hour year, without renormalisation; see basis-v1.
SI prefix factors and radians-to-degrees below are unit definitions.
"""

from __future__ import annotations

import hashlib
import json
import math
import re
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Protocol

from digitalmodel.structural.rainflow_cycles import count_cycles

from .schema import Document, number, read_document, text, validate_document, vector

METHOD_SOURCE = "src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:ApiRp2rdStrategy"
SN_SOURCE = "https://strathprints.strath.ac.uk/52862/ section 3.2.1 / Figure 16"
FACTOR_SOURCE = "docs/domains/risers/results-exchange/basis-v1.md#strength-screening-inputs"
FACTORS = {
    "length": {"m": 1.0, "mm": 0.001},
    "force": {"N": 0.001, "kN": 1.0},
    "moment": {"N.m": 0.001, "kN.m": 1.0},
    "stress": {"Pa": 1e-6, "kPa": 0.001, "MPa": 1.0},
    "angle": {"deg": 1.0, "rad": 180 / math.pi},
}


class ResultsAdapter(Protocol):
    """Boundary for all native calls; read returns the documented snapshot shape."""

    def read(self, result_file: Path, case_row: Document, options: Document) -> Document:
        """Read completed results without modifying or rerunning the model."""
        ...


def _positive(value: Any, label: str) -> float:
    result = number(value, label, 0)
    if result == 0:
        raise ValueError(f"{label}: must be positive")
    return result


def _row_float(case: Document, key: str) -> float:
    value = float(case[key])
    return _positive(value, key)


def _geometry(snapshot: Document, case: Document) -> None:
    diameter = _row_float(case, "outside_diameter_mm") * 0.001
    nominal = _row_float(case, "wall_thickness_mm") * 0.001
    checked = _row_float(case, "checked_wall_mm") * 0.001
    if not 0 < checked <= nominal < diameter / 2:
        raise ValueError("invalid nominal or checked section")
    for key, expected in (("outside_diameter_m", diameter), ("checked_wall_m", checked)):
        actual = number(snapshot["stress_geometry"][key], key)
        if not math.isclose(actual, expected, rel_tol=1e-9, abs_tol=1e-12):
            raise ValueError("native stress geometry differs from frozen checked section")


def _scale(snapshot: Document, dimension: str) -> float:
    try:
        return FACTORS[dimension][snapshot["units"][dimension]]
    except (KeyError, TypeError) as error:
        raise ValueError(f"unsupported {dimension} unit") from error


def _array(values: Any, scale: float, label: str, size: int | None = None) -> list[float]:
    return [value * scale for value in vector(values, label, size)]


def _histories(snapshot: Document, options: Document, arcs: list[float],
               touchdown_arcs: list[float], length: float, stress: float,
               samples: int) -> list[Document]:
    fibres = vector(options["fibres_deg"], "fibres_deg")
    if len(set(fibres)) != len(fibres) or any(not 0 <= f < 360 for f in fibres):
        raise ValueError("fibres_deg: expected unique angles in [0,360)")
    # Every material station swept by the final touchdown gets its own history.
    zone = {arc for arc in arcs if min(touchdown_arcs) <= arc <= max(touchdown_arcs)}
    expected = {("touchdown", arc, fibre) for arc in zone for fibre in fibres}
    expected |= {("hang_off", arcs[0], fibre) for fibre in fibres}
    found = set()
    rows = []
    for history in snapshot["stress_histories"]:
        arc = number(history["arc_length"], "history arc") * length
        fibre = number(history["fibre_deg"], "history fibre")
        identity = (history["name"], arc, fibre)
        if identity in found or identity not in expected:
            raise ValueError("unexpected or duplicate material station/fibre")
        found.add(identity)
        values = _array(history["stress"], stress, "signed longitudinal stress", samples)
        histogram: dict[float, float] = {}
        for cycle in count_cycles(values):
            histogram[cycle.range_value] = histogram.get(cycle.range_value, 0) + cycle.count
        rows.append({"name": history["name"], "arc_length_m": arc, "fibre_deg": fibre,
                     "stress_range_MPa": max(values) - min(values),
                     "range_histogram": [{"stress_range_MPa": r, "cycles": n}
                                         for r, n in sorted(histogram.items())]})
    if found != expected:
        raise ValueError("missing fixed station/fibre history in touchdown or hang-off region")
    return rows


def _fatigue(rows: list[Document], case: Document, options: Document,
             run: Document, duration: float, common: Document) -> Document:
    sea = options["sea_state"]
    if sea["id"] not in text(case["fatigue_seas"], "fatigue_seas").split(";"):
        raise ValueError("sea state is absent from the frozen case row")
    occurrence = number(sea["occurrence"], "annual occurrence", 0)
    if occurrence > 1:
        raise ValueError("occurrence must be an annual fraction")
    if duration < _row_float(case, "fatigue_stationary_seconds"):
        raise ValueError("fatigue history is shorter than frozen stationary duration")
    scf = _positive(options["scf"], "scf")
    # Study S-N basis cited in module docstring; stress is a range, not amplitude.
    m = 3.0
    intercept = 2_000_000 * 80.0**m
    year_multiplier = (8760 * 3600 / duration) * occurrence
    hot_spots = []
    for row in rows:
        histogram = row["range_histogram"]
        annual_cycles = sum(bin_["cycles"] for bin_ in histogram) * year_multiplier
        annual_damage = sum(bin_["cycles"] * (bin_["stress_range_MPa"] * scf)**m / intercept
                            for bin_ in histogram) * year_multiplier
        hot_spots.append({**row, "cycles_per_year": annual_cycles, "scf": scf,
                          "damage_per_year": annual_damage})
    damage = max(hot["damage_per_year"] for hot in hot_spots)
    fatigue = {**common, "sea_state": dict(sea), "hot_spots": hot_spots,
               "sn_curve": {"name": "FAT80 single-slope screening benchmark", "source": SN_SOURCE,
                            "m": m, "log_a": math.log10(intercept), "thickness_correction": 1.0,
                            "thickness_basis": "not qualified; factor unity assumed"},
               "damage_per_year": damage, "life_years": 1 / damage if damage else None,
               "damage_basis": "air-equivalent benchmark damage",
               "life_basis": "reciprocal benchmark annual damage; service life not established",
               "duration_s": duration, "annual_hours_basis": 8760,
               "rainflow_source": "src/digitalmodel/structural/rainflow_cycles.py",
               "aggregation": "one sea only; sum seas at identical station/fibre before taking a maximum",
               "limitations": "screening sensitivity: three sea states, not a full fatigue analysis",
               "regions_not_evaluated": ["buoyancy_transitions", "hog", "sag"],
               "run": dict(run)}
    validate_document(fatigue, "fatigue")
    return fatigue


def build_documents(snapshot: Document, case: Document, options: Document,
                    run: Document) -> tuple[Document, Document | None]:
    """Build strength and optional per-sea fatigue documents from one snapshot."""
    try:
        return _build(snapshot, case, options, run)
    except (KeyError, TypeError, IndexError, OverflowError) as error:
        raise ValueError("incomplete or invalid extraction snapshot or settings") from error


def _build(snapshot: Document, case: Document, options: Document,
           run: Document) -> tuple[Document, Document | None]:
    _geometry(snapshot, case)
    mode = options["analysis_type"]
    if mode not in {"static", "dynamic"}:
        raise ValueError("analysis_type must be static or dynamic")
    dynamic = mode == "dynamic"
    if not dynamic and options.get("sea_state"):
        raise ValueError("fatigue requires dynamic histories")
    condition = options["load_condition"]
    factors = {"functional": 1.0, "extreme": 1.2}
    if condition not in factors or number(options["design_case_factor"], "design_case_factor") != factors[condition]:
        raise ValueError("load condition or factor differs from versioned screening basis")
    allowable = _positive(options["smys_MPa"], "smys_MPa") * (2 / 3) * factors[condition]
    length, force, moment, stress, angle = (_scale(snapshot, name)
                                         for name in ("length", "force", "moment", "stress", "angle"))
    arcs = _array(snapshot["arc_length"], length, "arc_length")
    if arcs[0] != 0 or any(b <= a for a, b in zip(arcs, arcs[1:])):
        raise ValueError("arc stations must increase from hang-off zero")
    envelopes = {}
    for variable, scale, field in (("effective_tension", force, "effective_tension_kN"),
                                   ("bending_moment", moment, "bending_moment_kNm"),
                                   ("von_mises_stress", stress, "von_mises_stress_MPa")):
        profile = snapshot["profiles"][variable]
        env: Document = {"static": _array(profile["static"], scale, variable, len(arcs))}
        for bound in ("min", "max"):
            env[bound] = _array(profile[bound], scale, variable, len(arcs)) if dynamic else None
        if dynamic and any(a > b for a, b in zip(env["min"], env["max"])):
            raise ValueError("inverted range graph envelope")
        if variable == "von_mises_stress" and any(
                v < 0 for values in env.values() if values is not None for v in values):
            raise ValueError("equivalent stress cannot be negative")
        envelopes[field] = env
    equivalent = envelopes["von_mises_stress_MPa"]
    governing = [max(a, b) for a, b in zip(equivalent["static"], equivalent["max"])] if dynamic else equivalent["static"]
    utilisation = [value / allowable for value in governing]
    times = vector(snapshot["times_s"], "times_s") if dynamic else []
    if dynamic:
        start, end = vector(options["period_s"], "period_s", 2)
        if len(times) < 2 or any(b <= a for a, b in zip(times, times[1:])):
            raise ValueError("time samples must increase")
        if times[0] != start or times[-1] != end or end <= start:
            raise ValueError("recorded history does not cover the specified stationary period")
    touchdown_arcs = _array(snapshot["touchdown_arc_length"], length, "touchdown", len(times) if dynamic else 1)
    if any(arc not in arcs for arc in touchdown_arcs):
        raise ValueError("final touchdown must be on an extracted material station")
    rows = _histories(snapshot, options, arcs, touchdown_arcs, length, stress, len(times)) if dynamic else []
    critical = max((row for row in rows if row["name"] == "touchdown"),
                   key=lambda row: row["stress_range_MPa"]) if dynamic else None
    td_arc = critical["arc_length_m"] if critical else touchdown_arcs[0]
    peak_index = utilisation.index(max(utilisation))
    peak_arc = arcs[peak_index]
    location = "hang_off" if peak_arc == 0 else ("touchdown" if min(touchdown_arcs) <= peak_arc <= max(touchdown_arcs) else "span")
    common = {"case_id": text(case["case_id"], "case_id"), "analysis_type": mode,
              "data_basis": text(snapshot["data_basis"], "data_basis"),
              "load_case": dict(options["load_case"]),
              "qualification_status": "not qualified",
              "stress_geometry": dict(snapshot["stress_geometry"]), "run": dict(run)}
    top = snapshot["top_angle"]
    hang = snapshot["hang_off_tension"]
    angle_history = _array(top["history"], angle, "top angle", len(times)) if dynamic else []
    hang_history = _array(hang["history"], force, "hang-off tension", len(times)) if dynamic else []
    strength = {**common, "software": dict(snapshot["software"]), "load_case": dict(options["load_case"]),
                "arc_length_m": arcs, "effective_tension_kN": envelopes["effective_tension_kN"]["max" if dynamic else "static"],
                "bending_moment_kNm": envelopes["bending_moment_kNm"]["max" if dynamic else "static"],
                "von_mises_stress_MPa": governing, "von_mises_utilisation": utilisation,
                "envelopes": envelopes,
                "utilisation_method": {"name": "API RP 2RD legacy WSD equivalent stress / allowable",
                                       "source": METHOD_SOURCE, "allowable_MPa": allowable,
                                       "smys_MPa": options["smys_MPa"], "basic_factor": 2 / 3,
                                       "design_case_factor": factors[condition],
                                       "design_case_factor_source": FACTOR_SOURCE,
                                       "design_case_factor_status": "assumed, based on previous experience",
                                       "load_condition": condition,
                                       "basis": "native pressure/membrane/bending equivalent stress on checked section; screening only"},
                "max_utilisation": {"value": utilisation[peak_index], "arc_length_m": peak_arc, "location": location},
                "touchdown": {"arc_length_m": td_arc, "utilisation": utilisation[arcs.index(td_arc)],
                              "stress_range_MPa": critical["stress_range_MPa"] if critical else None,
                              "fibre_deg": critical["fibre_deg"] if critical else None,
                              "dynamic_arc_min_m": min(touchdown_arcs) if dynamic else None,
                              "dynamic_arc_max_m": max(touchdown_arcs) if dynamic else None,
                              "earlier_contacts_m": [[number(v, "earlier contact") * length for v in contact]
                                                     for contact in snapshot["earlier_contacts"]]},
                "top_angle_deg": {"static": number(top["static"], "static angle") * angle,
                                  "dynamic_min": min(angle_history) if dynamic else None,
                                  "dynamic_max": max(angle_history) if dynamic else None},
                "top_angle_basis": "absolute from vertical; flex-joint excursion not evaluated",
                "hang_off_tension_kN": {"static": number(hang["static"], "static tension") * force,
                                        "dynamic_max": max(hang_history) if dynamic else None}}
    validate_document(strength, "strength")
    fatigue = _fatigue(rows, case, options, run, times[-1] - times[0], common) if options.get("sea_state") else None
    return strength, fatigue


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def export_results(sim: Path, input_file: Path, case: Document, options: Document,
                   out: Path, adapter: ResultsAdapter) -> list[Path]:
    """Read once, validate everything, then write exchange files without NaN."""
    input_digest, result_digest = _sha256(input_file), _sha256(sim)
    row_digest = hashlib.sha256(json.dumps(case, sort_keys=True, allow_nan=False).encode()).hexdigest()
    extraction_digest = hashlib.sha256(json.dumps(options, sort_keys=True, allow_nan=False).encode()).hexdigest()
    identity = {"input_sha256": input_digest, "result_sha256": result_digest, "result_file": sim.name,
                "matrix_row_sha256": row_digest, "extraction_sha256": extraction_digest}
    sea = options.get("sea_state")
    if sea is not None and not re.fullmatch(r"[A-Za-z0-9_-]+", text(sea["id"], "sea id")):
        raise ValueError("invalid sea state identifier")
    names = ["strength.json"] + ([f"fatigue-{sea['id']}.json"] if sea else [])
    paths = [out / name for name in names]
    if any(path.exists() for path in paths):
        if not all(path.is_file() for path in paths):
            raise FileExistsError("partial output exists; preserve it and select a new output directory")
        for path, kind in zip(paths, ["strength", "fatigue"]):
            existing = read_document(path)
            validate_document(existing, kind)
            if existing["case_id"] != case["case_id"] or any(
                    existing["run"].get(key) != value for key, value in identity.items()):
                raise FileExistsError("different results already exist; select a distinct output directory")
        return paths
    snapshot = adapter.read(sim, case, options)
    if _sha256(input_file) != input_digest or _sha256(sim) != result_digest:
        raise ValueError("input or result changed during extraction")
    run = {**identity, "date": datetime.now(timezone.utc).isoformat()}
    strength, fatigue = build_documents(snapshot, case, options, run)
    documents = {"strength.json": strength}
    if fatigue is not None:
        documents[f"fatigue-{fatigue['sea_state']['id']}.json"] = fatigue
    encoded = {name: json.dumps(doc, indent=2, allow_nan=False) + "\n" for name, doc in documents.items()}
    out.mkdir(parents=True, exist_ok=True)
    for name, content in encoded.items():
        with (out / name).open("x", encoding="utf-8") as stream:
            stream.write(content)
    return paths
