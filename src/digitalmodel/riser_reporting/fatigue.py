# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Table-only supplied-result view; the standalone catenary report API is retained.

Arc-length plots, sensitivities and workflow routing are outside this slice.
Supplied life and margin are cross-checked; no fatigue solution or certification
status is calculated here.
"""
from functools import partial
from hashlib import sha256
from html import escape
import json
import math
from numbers import Real
from pathlib import Path
from typing import Any, Iterable

from digitalmodel.reporting import Provenance, ProvenanceError, ReportSection, SectionMode
from digitalmodel.reporting import assemble_report

COLUMNS = ("segment_id", "wave_damage", "viv_damage", "total_damage",
           "fatigue_life_years", "required_life_years", "dff_margin", "passes_dff")
HEADERS = ("Segment", "Wave damage (1)", "VIV damage (1)", "Total damage (1)",
           "Fatigue life (years)", "Required life (years)", "DFF margin (1)", "Passes DFF")
CSS = """body{font:14px sans-serif;margin:2em}table{border-collapse:collapse;
width:100%;table-layout:fixed}td,th{border:1px solid #888;padding:.4em;
overflow-wrap:anywhere;text-align:left}p{overflow-wrap:anywhere}
thead{display:table-header-group}@page{margin:15mm}
@media print{body{margin:0;font-size:9pt}tr{break-inside:avoid}}
"""


def canonical_digest(payload: Any) -> str:
    """Hash canonical UTF-8 JSON, rejecting nonfinite or non-JSON content."""
    encoded = json.dumps(payload, sort_keys=True, separators=(",", ":"),
                         ensure_ascii=False, allow_nan=False).encode("utf-8")
    return "sha256:" + sha256(encoded).hexdigest()


def _text(value: Any) -> str:
    return escape(str(value), quote=True)


def _require_text(value: Any) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("Evidence text must be nonempty")


def _positive_real(value: Any, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be a finite positive number")


def _validate_results(results: dict, inputs: dict) -> dict:
    for key in ("design_life_years", "dff"):
        _positive_real(inputs.get(key), key)
    required_life = inputs["design_life_years"] * inputs["dff"]
    _positive_real(required_life, "Input-derived required life")
    if not isinstance(results, dict) or not isinstance(results.get("segments"), list):
        raise ValueError("Results require a segment list")
    rows = results["segments"]
    if not rows or results.get("screening_status") not in ("pass", "fail"):
        raise ValueError("Nonempty segments and a pass/fail status are required")
    for row in rows:
        if not isinstance(row, dict) or not all(key in row for key in COLUMNS):
            raise ValueError("Required segment fields are missing")
        _require_text(row["segment_id"])
        for key in COLUMNS[1:-1]:
            value = row[key]
            if isinstance(value, bool) or not isinstance(value, Real) or not math.isfinite(value) or value < 0:
                raise ValueError(f"{key} must be a finite nonnegative number")
            if key not in ("wave_damage", "viv_damage") and value == 0:
                raise ValueError(f"{key} must be positive")
        # Relative tolerance admits floating-point roundoff, without an absolute
        # allowance that would hide discrepancies in small positive quantities.
        if not math.isclose(row["required_life_years"], required_life, rel_tol=1e-9, abs_tol=0.0):
            raise ValueError("Supplied required life conflicts with design life times DFF")
        expected_margin = row["fatigue_life_years"] / row["required_life_years"]
        if not math.isclose(row["dff_margin"], expected_margin, rel_tol=1e-9, abs_tol=0.0):
            raise ValueError("Supplied DFF margin conflicts with the life ratio")
        if type(row["passes_dff"]) is not bool or row["passes_dff"] != (
                row["fatigue_life_years"] >= row["required_life_years"]):
            raise ValueError("Supplied segment verdict conflicts with its life criterion")
        if row["passes_dff"] != (row["dff_margin"] >= 1):
            raise ValueError("Supplied DFF margin conflicts with the segment verdict")
    ids = [row["segment_id"] for row in rows]
    if len(set(ids)) != len(ids) or results.get("governing_segment") not in ids:
        raise ValueError("Segment identifiers must be unique and include the governing segment")
    governing = rows[ids.index(results["governing_segment"])]
    margin = results.get("governing_dff_margin")
    if isinstance(margin, bool) or not isinstance(margin, Real) or margin != governing["dff_margin"]:
        raise ValueError("Governing DFF margin conflicts with the segment record")
    passed = results["screening_status"] == "pass"
    if passed != governing["passes_dff"] or passed != all(r["passes_dff"] for r in rows):
        raise ValueError("Screening status conflicts with supplied segment verdicts")
    return dict(governing)


def _validate_metadata(method: str, references: list, assumptions: list[str] | None, workflow_version: str) -> dict:
    for value in (method, workflow_version):
        _require_text(value)
    if not isinstance(references, list) or not references:
        raise ValueError("Standard reference records are required")
    for reference in references:
        if not isinstance(reference, dict):
            raise ValueError("Each reference must be a record")
        for key in ("identifier", "title", "edition", "locator", "qualification"):
            _require_text(reference.get(key))
        if reference["qualification"] not in ("verified", "unverified"):
            raise ValueError("Reference qualification must be verified or unverified")
    if assumptions is not None:
        if not isinstance(assumptions, list):
            raise ValueError("Assumptions must be a list or an explicit absent record")
        for assumption in assumptions:
            _require_text(assumption)
    return dict(method=method, references=references, assumptions=assumptions,
                workflow_version=workflow_version)


def _bind_sources(provenance: Provenance, inputs: dict, results: dict, metadata: dict) -> dict:
    provenance.require()
    if provenance.validated_path not in ("screening", "unvalidated"):
        raise ProvenanceError("Supplied-result views require screening or unvalidated status")
    identifiers = [source.identifier for source in provenance.sources]
    if len(set(identifiers)) != len(identifiers):
        raise ProvenanceError("Source identifiers must be unique")
    for identifier in identifiers:
        _require_text(identifier)
    bound = {}
    for kind, payload in (("spec", inputs), ("workflow_results", results), ("metadata", metadata)):
        sources = [source for source in provenance.sources if source.kind == kind]
        if len(sources) != 1 or sources[0].digest != canonical_digest(payload):
            raise ProvenanceError(f"Exactly one digest-matching {kind} source is required")
        bound[kind] = sources[0].identifier
    return bound


def _table(headers: tuple, rows: Iterable) -> str:
    head = "".join(f"<th>{_text(cell)}</th>" for cell in headers)
    body = "".join("<tr>" + "".join(f"<td>{_text(cell)}</td>" for cell in row) + "</tr>"
                   for row in rows)
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def _content(results: dict, inputs: dict, metadata: dict, sources: dict, governing: dict, status: str) -> dict:
    evidence = (f'<p>Method: {_text(metadata["method"])}; '
                f'<a href="#references">supplied references</a>; '
                f'source: {_text(sources["workflow_results"])}; '
                f'metadata: {_text(sources["metadata"])}.</p>')
    operator = ">=" if governing["passes_dff"] else "<"
    finding = (f'<p>Supplied governing segment: {_text(governing["segment_id"])}; '
               f'supplied verdict: {results["screening_status"].upper()}; criterion: '
               f'{_text(governing["fatigue_life_years"])} years {_text(operator)} '
               f'{_text(governing["required_life_years"])} years; '
               f'DFF margin (1): {_text(governing["dff_margin"])}.</p>' + evidence)
    assumptions = metadata["assumptions"]
    note = "Assumptions record not supplied; completeness is not established."
    if assumptions is not None:
        note = "; ".join(assumptions) or "Supplied assumptions record is empty; completeness is not established."
    reference_rows = [[reference[key] for key in
                       ("identifier", "title", "edition", "locator", "qualification")]
                      for reference in metadata["references"]]
    return {
        "summary": ("Executive summary", f"<p>{status.upper()} supplied-result view; not certified.</p>" + finding),
        "inputs": ("Inputs and assumptions", _table(("Input", "Supplied value"),
            [(key, json.dumps(value, sort_keys=True, ensure_ascii=False, allow_nan=False))
             for key, value in sorted(inputs.items())]) + f"<p>{_text(note)}</p>"
            f'<p>Source: {_text(sources["spec"])}; metadata: {_text(sources["metadata"])}.</p>'),
        "method": ("Method and standards", f'<p id="references">{_text(metadata["method"])}</p>'
            + _table(("Identifier", "Title", "Edition", "Locator", "Supplied qualification"), reference_rows)
            + f'<p>Workflow version: {_text(metadata["workflow_version"])}; '
            f'source: {_text(sources["metadata"])}. Reference qualification is caller-supplied.</p>'),
        "results": ("Results", _table(HEADERS, [[row[key] for key in COLUMNS]
            for row in results["segments"]]) + evidence),
        "findings": ("Critical-location findings", finding + "<p>Coordinates are not supplied; no arc-length profile is established.</p>"),
        "conclusions": ("Conclusions", finding + "<p>Engineering adequacy requires independent qualification. "
            "Sensitivities are not supplied; no sensitivity conclusion is established.</p>"),
    }


def _section(data: dict, *, key: str, **_: Any) -> str:
    heading, content = data[key]
    return f'<div class="section" id="{key}"><h2>{heading}</h2>{content}</div>'


def render_fatigue_view(results: dict, inputs: dict, *, method: str, references: list,
                        assumptions: list[str] | None, workflow_version: str,
                        provenance: Provenance, title: str = "Riser fatigue analysis",
                        output_path: Path | str | None = None) -> str | Path:
    """Render supplied observations after shape, verdict and digest checks.

    Metadata binds method, references, assumptions and workflow_version as one
    JSON object. Identifiers are displayed only; no source path is opened.
    Optional output follows assemble_report's HTML-or-Path return convention.
    """
    _require_text(title)
    if not isinstance(inputs, dict):
        raise ValueError("Original inputs must be a mapping")
    governing = _validate_results(results, inputs)
    metadata = _validate_metadata(method, references, assumptions, workflow_version)
    sources = _bind_sources(provenance, inputs, results, metadata)
    data = _content(results, inputs, metadata, sources, governing, str(provenance.validated_path))
    sections = []
    for key, (heading, _) in data.items():
        kinds = ("metadata",) if key == "method" else (
            "spec" if key == "inputs" else "workflow_results", "metadata")
        sections.append(ReportSection(key, heading, SectionMode.ALWAYS,
            partial(_section, key=key), reads=[sources[kind] for kind in kinds]))
    return assemble_report(_text(title), provenance=provenance, sections=sections,
                           data=data, css=CSS, output_path=output_path)
