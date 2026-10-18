# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Provenance boundaries for solver-free shore pull-in preparation."""

from datetime import date
import math
import re
from urllib.parse import urlsplit

ASSUMPTION_LABEL = "assumed, based on previous experience"
ASSUMABLE_PIPE_INPUTS = {
    "pipeline.dimensions.wall_thickness": "m",
    "pipeline.coatings.corrosion.thickness": "m",
    "pipeline.coatings.corrosion.density": "te/m3",
    "pipeline.coatings.weight.0.thickness": "m",
    "pipeline.coatings.weight.0.density": "te/m3",
}
STUDY_PARAMETER_QUALIFICATION = "study parameter chosen for this preparation; not sourced or measured"
STUDY_PARAMETER_INPUTS = (
    "water_depth_m",
    "tail_tension_kn",
    "winch_efficiency",
    "cases.0.rise_m",
    "cases.0.buoyancy_fraction",
    "cases.1.rise_m",
    "cases.1.buoyancy_fraction",
    "cases.2.rise_m",
    "cases.2.buoyancy_fraction",
)
DATASET_ANCHORS = {
    "pipeline.dimensions.outer_diameter",
    "equipment.vessel.properties.loa",
    "equipment.vessel.properties.beam",
    "equipment.vessel.properties.draft",
}


def numeric_paths(value: object, prefix: str = "") -> set[str]:
    """Identify every numeric input leaf that needs an input source."""
    if isinstance(value, dict):
        return set().union(*(numeric_paths(v, f"{prefix}.{k}" if prefix else k) for k, v in value.items()))
    if isinstance(value, list):
        return set().union(*(numeric_paths(v, f"{prefix}.{i}") for i, v in enumerate(value)))
    return {prefix} if type(value) in (int, float) else set()


def source_locator(source: dict) -> str:
    """Identify an explicit prose locator or a physical CSV line."""
    locator = source.get("locator")
    if isinstance(locator, str) and locator.strip():
        return locator
    line = source.get("csv_line")
    if type(line) is int and line > 1:
        return f"CSV line {line}; header is line 1"
    raise ValueError("Missing source locator")


def audit_source(source: dict) -> None:
    """Check metadata for every listed source, including context-only sources."""
    try:
        url = urlsplit(source["url"])
        date.fromisoformat(source["retrieval_date"])
        complete = (url.scheme in {"http", "https"} and bool(url.netloc)
                    and type(source["record_count"]) is int and source["record_count"] > 0
                    and re.fullmatch(r"[0-9a-f]{64}", source["sha256"]))
    except (KeyError, TypeError, ValueError):
        complete = False
    if not complete:
        raise ValueError("Incomplete public input provenance")
    source_locator(source)


def audit_geometry_basis(table: dict) -> None:
    """Audit retrieved geometry sources or explicitly unconfirmed bibliography.

    Retrieved texts obey exactly the public-source metadata rules. An
    unavailable primary text cannot claim retrieved bytes: its numerical use
    must instead be labelled as an assumption, with a basis and access reason.
    Publisher metadata may be linked through an independently audited source;
    that metadata does not confirm an equation, value or primary-text locator.
    """
    references = table.get("geometry_basis")
    if not isinstance(references, dict) or set(references) != {"pianc", "profile"}:
        raise ValueError("Missing geometry references")
    for reference in references.values():
        status = reference.get("evidence_status")
        if status == "retrieved":
            audit_source(reference)
            continue
        try:
            date.fromisoformat(reference["attempt_date"])
            complete = (status == "unavailable"
                        and reference["qualification"] == ASSUMPTION_LABEL
                        and all(isinstance(reference.get(key), str) and reference[key].strip()
                                for key in ("citation", "locator", "basis", "reason"))
                        and not {"sha256", "retrieval_date", "record_count"}.intersection(reference))
            if "url" in reference:
                url = urlsplit(reference["url"])
                complete = complete and url.scheme in {"http", "https"} and bool(url.netloc)
            if "metadata_source" in reference:
                complete = complete and reference["metadata_source"] in table["sources"]
        except (KeyError, TypeError, ValueError):
            complete = False
        if not complete:
            raise ValueError("Incomplete or unqualified geometry reference")


def audit_dataset_anchors(table: dict) -> None:
    """Compare numeric anchors with the retained dataset columns in metres."""
    columns = table["public_dataset"]["columns"]
    expected = {
        "pipeline.dimensions.outer_diameter": float(columns["PIPELAY_CAPACITY_IN"]) * 0.0254,
        **{f"equipment.vessel.properties.{key}": float(columns[column]) for key, column in
           (("loa", "LOA_M"), ("beam", "BEAM_M"), ("draft", "DRAFT_M"))},
    }
    for path, anchor in expected.items():
        value = table["basis"]
        for part in path.split("."):
            value = value[part]
        if isinstance(value, bool) or not math.isfinite(value) or not math.isclose(value, anchor, rel_tol=0, abs_tol=1e-12):
            raise ValueError(f"Dataset anchor value differs from recorded column: {path}")


def audit_input_sources(table: dict) -> list[dict]:
    """Require provenance or an explicitly permitted input qualification.

    Diameter and measured hull dimensions retain the prescribed dataset.
    Five unmeasured wall/coating properties may use a labelled assumption with
    its basis and a matching table row. Only the nine listed case-definition
    paths may instead be declared study parameters, each with its reason.
    Other scenario inputs retain their repository assumption references.
    """
    for source in table["sources"].values():
        audit_source(source)
    audit_geometry_basis(table)
    audit_dataset_anchors(table)
    required = numeric_paths(table["basis"]) | numeric_paths(table["cases"], "cases")
    if not required <= set(table["input_sources"]):
        raise ValueError("Missing numeric input source")
    rows: dict[str, dict] = {}
    for path, entry in table["input_sources"].items():
        audit_entry(table, path, entry, rows)
    expected = [rows[path] for path in ASSUMABLE_PIPE_INPUTS if path in rows]
    if table.get("assumed_inputs", []) != expected:
        raise ValueError("The assumption basis table must match input values, units, labels and bases")
    return expected



def audit_entry(table: dict, path: str, entry: dict, rows: dict) -> None:
    """Admit only the explicit input qualification or source for a path."""
    if entry.get("qualification") == STUDY_PARAMETER_QUALIFICATION:
        if path not in STUDY_PARAMETER_INPUTS:
            raise ValueError(f"Only declared study parameter paths may use this qualification: {path}")
        if "source" in entry or "label" in entry:
            raise ValueError(f"A study parameter must not claim a source or pipe assumption: {path}")
        basis = entry.get("basis")
        if not isinstance(basis, str) or not basis.strip():
            raise ValueError(f"A study parameter requires a recorded basis: {path}")
        return
    label = entry.get("label")
    if label is not None:
        if label != ASSUMPTION_LABEL:
            raise ValueError(f"Invalid assumption label: {path}")
        if path not in ASSUMABLE_PIPE_INPUTS:
            raise ValueError(f"Only pipe wall thickness or coating properties may be assumed: {path}")
        basis = entry.get("basis")
        if not isinstance(basis, str) or not basis.strip():
            raise ValueError(f"An assumed input requires a recorded basis: {path}")
        if "source" in entry:
            raise ValueError(f"An assumed input must not claim a public source: {path}")
        value = table["basis"]
        for part in path.split("."):
            value = value[int(part)] if isinstance(value, list) else value[part]
        rows[path] = {"input": path, "value": value, "unit": ASSUMABLE_PIPE_INPUTS[path],
                      "label": label, "basis": basis}
        return
    source = table["sources"].get(entry.get("source"))
    if not isinstance(source, dict):
        raise ValueError(f"A public source or permitted assumption label is required: {path}")
    if path in DATASET_ANCHORS | ASSUMABLE_PIPE_INPUTS.keys() and (entry["source"] != "dataset" or source != table["public_dataset"]):
        raise ValueError(f"The prescribed public dataset is required: {path}")
    if path in ASSUMABLE_PIPE_INPUTS:
        raise ValueError(f"The public dataset contains no wall or coating measurement: {path}")
