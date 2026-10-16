# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a demand-only hull-girder report with explicit allowable evidence."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import sys
import tomllib
from datetime import date
from html import escape
from pathlib import Path

DEMO = Path(__file__).resolve().parent
ROOT = DEMO.parents[2]
if str(ROOT / "src") not in sys.path:
    sys.path.insert(0, str(ROOT / "src"))

import numpy as np
import yaml

from digitalmodel.naval_architecture.hull_girder_screening import (
    DEFAULT_CLOSURE_TOLERANCE,
    AllowableCurve,
    DistributedWeight,
    build_weight_curve,
    buoyancy_box,
    still_water_sf_bm,
)

CURVES = {
    "shear_force": ("positive_t", "negative_t"),
    "bending_moment": ("hogging_t_m", "sagging_t_m"),
    "total_shear_force": ("positive_t", "negative_t"),
    "total_bending_moment": ("hogging_t_m", "sagging_t_m"),
}
INPUTS = (
    "examples/workflows/hull-girder-report-pack/build_report.py",
    "examples/workflows/hull-girder-report-pack/allowable-basis.json",
    "examples/workflows/hull-girder-report-pack/revisions.json",
    "examples/workflows/hull-girder-report-pack/screening_input.yml",
    "src/digitalmodel/naval_architecture/hull_girder_screening.py",
    "src/digitalmodel/naval_architecture/hull_girder_strength.py",
    "tests/naval_architecture/test_hull_girder_screening.py",
)


def _curve_signs(settings: dict, key: str) -> dict:
    curve = settings["allowables"][key]
    positive, negative = CURVES[key]
    if set(curve) - {"x_m", positive, negative}:
        raise ValueError("unregistered curve field in allowable inventory")
    values = curve[positive]
    opposite = curve.get(negative, values)
    if not all(math.isfinite(float(v)) for v in [*curve["x_m"], *values, *opposite]):
        raise ValueError("nonfinite allowable inventory value")
    AllowableCurve(tuple(curve["x_m"]), tuple(values), tuple(opposite))
    return {"signs": ["positive", "negative"],
            "negative_inherits_positive": negative not in curve}


def allowable_inventory(settings: dict, basis: dict) -> list[dict]:
    """Reject incomplete inventories and unsupported source qualifications."""
    expected = {f"allowables.{key}" for key in CURVES} | {
        "section_modulus.approved_sm.deck_m3",
        "section_modulus.approved_sm.keel_m3",
        "section_modulus.yield_mpa", "permissible_stress",
    }
    rows = copy.deepcopy(basis["allowables"])
    if len(rows) != len(expected) or {r["key"] for r in rows} != expected:
        raise ValueError("incomplete or duplicated allowable inventory")
    sm = settings.get("section_modulus", {})
    if (set(settings.get("allowables", {})) != set(CURVES)
            or set(sm) != {"yield_mpa", "approved_sm"}
            or set(sm.get("approved_sm", {})) != {"deck_m3", "keel_m3", "source"}):
        raise ValueError("unregistered strength input in allowable inventory")
    for row in rows:
        key = row["key"]
        if row["label"] not in {"cited", "derived", "assumed", "not established"}:
            raise ValueError("unsupported source qualification label")
        if row["label"] != "not established":
            raise ValueError("unsupported source qualification for missing evidence")
        if not row.get("reason") or not row.get("required_evidence"):
            raise ValueError("missing qualification evidence requirement")
        if key.startswith("allowables."):
            row.update(_curve_signs(settings, key.split(".")[1]))
        elif key != "permissible_stress":
            value = sm["yield_mpa"] if key.endswith("yield_mpa") else sm["approved_sm"][key.split(".")[-1]]
            if not math.isfinite(float(value)) or float(value) <= 0:
                raise ValueError("invalid strength input in allowable inventory")
    return rows


def _fixture(settings: dict) -> tuple[float, dict]:
    """Restrict the analytical comparator to its documented load geometry."""
    length = float(settings["length_m"])
    weights = settings["weights"]
    blocks = weights.get("lightship", [])
    if len(blocks) != 1 or weights.get("tanks") or weights.get("point_weights"):
        raise ValueError("unsupported loading fixture")
    block = blocks[0]
    valid = (
        math.isfinite(length) and length > 0
        and math.isfinite(float(block["weight_t"])) and block["weight_t"] > 0
        and block["x_start_m"] == length / 4
        and block["x_end_m"] == 3 * length / 4
        and block.get("lcg_m", length / 2) == length / 2
        and settings["buoyancy"] == {"method": "box"}
        and int(settings["n_stations"]) >= 11
        and (int(settings["n_stations"]) - 1) % 4 == 0
    )
    if not valid:
        raise ValueError("loading fixture does not match analytical comparator")
    return length, block


def _calculate(settings: dict, basis: dict) -> dict:
    length, block = _fixture(settings)
    x = np.linspace(0.0, length, int(settings["n_stations"]))
    weights = build_weight_curve(x, [DistributedWeight(
        "central distributed load", float(block["weight_t"]),
        float(block["x_start_m"]), float(block["x_end_m"]),
    )])
    buoyancy, equilibrium = buoyancy_box(x, weights)
    result = still_water_sf_bm(x, weights, buoyancy, equilibrium=equilibrium)
    comparisons = []
    for quantity, actual, reference, tolerance in (
        ("Shear magnitude", abs(result.max_shear_t), block["weight_t"] / 4,
         basis["validation"]["shear_relative_tolerance"]),
        ("Sagging moment magnitude", abs(result.max_sagging_t_m),
         block["weight_t"] * length / 16,
         basis["validation"]["bending_relative_tolerance"]),
    ):
        difference = (actual - reference) / reference
        comparisons.append({"quantity": quantity, "calculated": actual,
                            "reference": reference, "difference_percent": difference * 100,
                            "criterion_percent": tolerance * 100,
                            "satisfies_criterion": abs(difference) <= tolerance})
    if not result.closure_ok or not all(r["satisfies_criterion"] for r in comparisons):
        raise ValueError("analytical demand comparison does not satisfy its criterion")
    return {
        "strength_disposition": "not evaluated", "qualified_allowables": 0,
        "finite_element_performed": False, "length_m": length,
        "weight_t": float(block["weight_t"]), "n_stations": len(x),
        "max_shear_t": result.max_shear_t,
        "max_sagging_t_m": result.max_sagging_t_m,
        "x_max_sagging_m": result.x_max_sagging_m, "closure_ok": result.closure_ok,
        "closure_shear_fraction": result.closure_shear_fraction,
        "closure_moment_fraction": result.closure_moment_fraction,
        "closure_tolerance": DEFAULT_CLOSURE_TOLERANCE,
        "analytical_comparisons": comparisons,
        "x_m": list(result.x_m), "bending_moment_t_m": list(result.bending_moment_t_m),
        "frames": [{"x_m": float(f["x_m"]),
                    "shear_t": float(np.interp(f["x_m"], x, result.shear_force_t)),
                    "bending_t_m": float(np.interp(f["x_m"], x, result.bending_moment_t_m)),
                    "allowable": None, "utilization": None}
                   for f in settings["frames"]],
    }


def _table(headers: list, rows: list, caption: str) -> str:
    head = "".join(f"<th>{escape(str(h))}</th>" for h in headers)
    body = "".join("<tr>" + "".join(f"<td>{escape(str(v))}</td>" for v in row)
                   + "</tr>" for row in rows)
    key = caption.split('. ')[0].lower().replace(' ', '-').replace('.', '-')
    return (f'<table id="{key}"><caption>{escape(caption)}</caption><thead><tr>{head}</tr>'
            f"</thead><tbody>{body}</tbody></table>")


def _section(key: str, title: str, body: str) -> str:
    return f'<section id="{key}"><h2>{escape(title)}</h2>{body}</section>'


def _revision_history(record: dict) -> str:
    history = record["history"]
    if (not history or history[-1]["revision"] != record["revision"]
            or len({r["revision"] for r in history}) != len(history)):
        raise ValueError("declared revision must match the newest unique revision row")
    dates = [date.fromisoformat(row["date"]) for row in history]
    if dates != sorted(dates):
        raise ValueError("revision dates must be ordered")
    rows = "".join(
        f'<tr data-revision="{escape(row["revision"], quote=True)}"><td>'
        f'{escape(row["revision"])}</td><td>{escape(row["date"])}</td>'
        f'<td>{escape(row["change"])}</td></tr>' for row in history
    )
    return _section("revision-history", "Revision history",
                    '<table><caption>Report issue record.</caption><thead><tr>'
                    '<th>Revision</th><th>Date</th><th>What changed and why</th>'
                    f'</tr></thead><tbody>{rows}</tbody></table>')


def _bending_svg(results: dict) -> str:
    length = results["length_m"]
    scale = abs(results["max_sagging_t_m"])
    points = " ".join(f"{62 + 550 * x / length:.2f},{46 - 240 * m / scale:.2f}"
                      for x, m in zip(results["x_m"], results["bending_moment_t_m"]))
    return (
        '<figure id="figure-5-1"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 360" '
        'role="img" aria-labelledby="bm-title"><title id="bm-title">'
        'Still-water bending moment along the synthetic fixture</title>'
        '<path d="M62 46H612 M62 46V286H612" fill="none" stroke="#6b6a60"/>'
        f'<polyline points="{points}" fill="none" stroke="#2a78d6" stroke-width="2"/>'
        '<g font-size="12" fill="#172033"><text x="18" y="50">0</text>'
        f'<text x="8" y="304">{results["max_sagging_t_m"]:.4g}</text>'
        '<text x="58" y="326">0</text>'
        f'<text x="580" y="326">{length:.4g}</text>'
        '<text x="230" y="350">Position along hull (m)</text>'
        '<text x="62" y="22">Bending moment (t·m)</text></g></svg>'
        '<figcaption>Figure 5.1. Calculated still-water bending moment; '
        'strength boundaries are not established.</figcaption></figure>'
    )


def _design(results: dict, inventory: list) -> str:
    rows = [[r["name"], r["units"], r["label"], r["reason"]] for r in inventory]
    return (
        '<p>The synthetic fixture load distribution and missing strength basis define the scope below.</p>'
        '<h3>3.1 Site data</h3><p>Not applicable: the example has no site-specific loading.</p>'
        '<h3>3.2 Structure data</h3><p>No midship scantlings or qualified section properties '
        'are supplied; elastic stress and ultimate capacity are not calculated.</p>'
        '<h3>3.3 Environmental data</h3><p>Only still-water equilibrium is represented. '
        'No wave, slamming, whipping or thermal demand is calculated.</p>'
        '<h3>3.4 Vessel data</h3><p>The generic box hull has length '
        f'{results["length_m"]:.4g} m and a central distributed load of '
        f'{results["weight_t"]:.4g} t over its middle half. Geometry and loading are '
        'a synthetic textbook box-barge analytical fixture <a href="#i3">[I3]</a>, not measurements.</p>'
        '<h3>3.5 Criteria</h3><p>Strength acceptance is not evaluated. The strength-input '
        'evidence is summarized in <a href="#table-3-1">Table 3.1</a>; its required evidence is detailed in '
        '<a href="#appendix-a">Appendix A</a>. Calculation comparisons use the existing '
        'analytical fixture criteria <a href="#i2">[I2]</a>.</p>'
        + _table(["Strength input", "Units", "Label", "Reason"], rows, 'Table 3.1. Strength-input evidence.')
        + '<h3>3.6 Method</h3><p>See <a href="#methodology">Section 4</a>.</p>'
        '<h3>3.7 Load cases</h3><p>One still-water loading condition is evaluated at '
        f'{results["n_stations"]} integration stations and {len(results["frames"])} reporting '
        'frames. No load case has a qualified strength limit.</p>'
    )


def _detailed_results(results: dict) -> str:
    rows = [[f'{r["x_m"]:.4g}', f'{r["shear_t"]:.4g}', f'{r["bending_t_m"]:.4g}',
             'not established', 'not evaluated'] for r in results["frames"]]
    comparisons = [[r["quantity"], f'{r["calculated"]:.4g}', f'{r["reference"]:.4g}',
                    f'{r["difference_percent"]:+.1f}', f'{r["criterion_percent"]:.1f}']
                   for r in results["analytical_comparisons"]]
    return (
        '<p>The calculated demand has its largest sagging magnitude at '
        f'{results["x_max_sagging_m"]:.4g} m, with moment '
        f'{results["max_sagging_t_m"]:.4g} t·m. <a href="#table-5-1">Table 5.1</a> gives demand without assigning '
        'a strength utilisation. Zero shear at the central frame is retained. '
        '<a href="#figure-5-1">Figure 5.1</a> shows the longitudinal moment distribution. '
        f'Raw end shear and moment residuals are {100 * abs(results["closure_shear_fraction"]):.1f} % '
        f'and {100 * abs(results["closure_moment_fraction"]):.1f} %, respectively, against the '
        f'{100 * results["closure_tolerance"]:.1f} % closure criterion.</p>'
        + _bending_svg(results)
        + _table(['Position (m)', 'Shear (t)', 'Moment (t·m)', 'Strength limit', 'Utilisation'],
                 rows, 'Table 5.1. Still-water frame demand, using mass-equivalent load units.')
        + '<p><a href="#table-5-2">Table 5.2</a> compares calculated magnitudes with the continuous central-block '
        'solution. Signed differences use the analytical value as reference and are displayed '
        'to one decimal place; unrounded differences govern the comparison. Both comparisons '
        'satisfy their stated numerical criteria; neither qualifies a strength limit.</p>'
        + _table(['Quantity', 'Calculated (t or t·m)', 'Analytical (t or t·m)',
                  'Difference (%)', 'Absolute difference criterion (%)'], comparisons,
                 'Table 5.2. Numerical demand comparison against the analytical fixture.')
    )


def _references(basis: dict, version: str) -> str:
    source = basis["sources"]["iacs_index"]
    terms = basis["sources"]["iacs_terms"]
    return (
        '<p>The published metadata and internal calculation basis are distinguished below.</p>'
        '<h3>9.1 Published references</h3><p id="p1">[P1] IACS, UR S11, '
        f'<a href="{escape(source["url"], quote=True)}">Longitudinal Strength Standard</a>, '
        'Rev.10, December 2020. Applicable clause title is not established from opened '
        'catalogue metadata. No numerical rule value is reproduced.</p>'
        f'<p id="p2">[P2] IACS, <a href="{escape(terms["url"], quote=True)}">'
        'Terms and conditions</a>. Source access records use UTC; catalogue and terms '
        f'were opened on {escape(source["observed_at_utc"][:10])}.</p>'
        '<h3>9.2 Internal references</h3>'
        f'<p id="i1">[I1] Digitalmodel {escape(version)}, '
        '<code>src/digitalmodel/naval_architecture/hull_girder_screening.py</code>, '
        'weight distribution, box buoyancy and still-water integration.</p>'
        '<p id="i2">[I2] <code>tests/naval_architecture/test_hull_girder_screening.py</code>, '
        '<code>test_sagging_barge_matches_hand_calc</code>, analytical load case and '
        'comparison tolerances. This is an internal numerical comparison, not external validation.</p>'
        '<p id="i3">[I3] <code>examples/workflows/hull-girder-report-pack/screening_input.yml</code>, '
        'synthetic loading fixture. The legacy wave implementation cites Rev.7, 2010; '
        'equivalence to the catalogue edition is not established and wave results are excluded.</p>'
    )


def _contents() -> str:
    return '<nav aria-label="Contents"><h2>Contents</h2><ul>' + ''.join(
        f'<li><a href="#{key}">{title}</a></li>' for key, title in (
            ('summary', '1 Executive Summary'), ('introduction', '2 Introduction'),
            ('design-data', '3 Design Data'), ('methodology', '4 Analysis Methodology'),
            ('results', '5 Detailed Results'), ('findings', '6 Key Findings'),
            ('limitations', '7 Limitations'), ('recommendations', '8 Recommendations'),
            ('references', '9 References'), ('symbols', '10 Acronyms and Symbols'),
            ('appendix-a', 'Appendix A'),
        )
    ) + '</ul></nav>'


def _summary(results: dict) -> str:
    return (
        '<p>Obtain qualified longitudinal-strength limits before accepting a loading '
        'condition. Strength acceptance is not evaluated because zero of eight '
        'strength inputs are qualified. Missing loading-manual, material and rule '
        'evidence prevents an operating-limit conclusion; no independent finite-element '
        'comparison is established.</p>'
        '<p>The synthetic fixture has its governing still-water sagging moment of '
        f'{results["max_sagging_t_m"]:.4g} t·m at {results["x_max_sagging_m"]:.4g} m. '
        f'The maximum shear magnitude is {abs(results["max_shear_t"]):.4g} t. '
        'The shear and moment comparisons satisfy their existing numerical criteria '
        f'of {results["analytical_comparisons"][0]["criterion_percent"]:.1f} % and '
        f'{results["analytical_comparisons"][1]["criterion_percent"]:.1f} %, respectively '
        '<a href="#table-5-2">(Table 5.2)</a>. These are calculation-agreement criteria, '
        'not strength limits.</p>'
    )


def _render(results: dict, inventory: list, basis: dict, revisions: dict) -> str:
    version = tomllib.loads((ROOT / 'pyproject.toml').read_text())["project"]["version"]
    revision_html = _revision_history(revisions)
    method = (f'<p>Digitalmodel {escape(version)} is used to integrate the distributed '
              'weight minus buoyancy along the free-ended hull <a href="#i1">[I1]</a>. '
              'Box buoyancy balances total weight and longitudinal moment. The raw end '
              f'residual criterion is {100 * results["closure_tolerance"]:.1f} % of '
              'displacement for shear and displacement times length for moment.</p>'
              '<p>With x measured from the aft end, q(x) = w(x) − b(x), '
              'V(x) = ∫q(x)dx (4.1), and M(x) = ∫V(x)dx (4.2). '
              'Positive moment denotes hogging and negative moment sagging. Trapezoidal '
              'integration uses cell-averaged weights. The continuous central-block '
              'comparator is |V|max = W/4 (4.3), |M|max = WL/16 (4.4) '
              '<a href="#i2">[I2]</a>. No wave or strength calculation is performed.</p>')
    sections = [
        _section('summary', '1 Executive Summary', _summary(results)), revision_html,
        _section('introduction', '2 Introduction', '<p>This internal analysis identifies still-water demand and the evidence needed for a longitudinal-strength decision. Local strength, ultimate collapse and class compliance are outside the calculated scope.</p>'),
        _section('design-data', '3 Design Data', _design(results, inventory)),
        _section('methodology', '4 Analysis Methodology', method),
        _section('results', '5 Detailed Results', _detailed_results(results)),
        _section('findings', '6 Key Findings', '<p>The two analytical demand comparisons satisfy their numerical criteria in <a href="#table-5-2">Table 5.2</a>. No frame or section can be assigned a strength disposition because all eight strength-input entries remain unqualified.</p>'),
        _section('limitations', '7 Limitations', '<p>Missing strength evidence prevents allowable replacement and strength acceptance; no measured loading condition, wave combination, section stress, ultimate collapse, local buckling or independent finite-element validation is established. The consequences and required evidence are given in <a href="#appendix-a">Appendix A</a>.</p>'),
        _section('recommendations', '8 Recommendations', '<p>Obtain the six loading-manual curve/property entries, qualified material evidence and the applicable rule clause before assigning a permitted loading condition. Confirm the actual weight, buoyancy and section model and obtain an independent comparison before extending this synthetic fixture to an engineering configuration.</p>'),
        _section('references', '9 References', _references(basis, version)),
        _section('symbols', '10 Acronyms and Symbols', '<p>SF denotes shear force; BM denotes bending moment. In the marine convention used here, t is a mass-equivalent load ordinate and t·m is its moment; no force conversion is applied. W is total distributed weight (t), L hull length (m), x longitudinal position (m), q net load (t/m), V shear (t), M bending moment (t·m), and Z section modulus (m³). Strength utilisation means demand divided by a qualified limit and is not calculated.</p>'),
        _section('appendix-a', 'Appendix A — Missing strength evidence', '<p>The synthetic curves and section properties in the original example have no qualified strength basis; all eight strength inputs are labelled not established. No numerical placeholder is promoted to an approved limit. Restricted standard values are not established because reproduction permission is unavailable <a href="#p1">[P1]</a>, <a href="#p2">[P2]</a>. The general rule cannot authenticate configuration-specific loading-manual curves. Missing evidence affects both signs of every curve.</p>' + _table(['Strength input', 'Label', 'Missing evidence', 'Required evidence'], [[r['name'], r['label'], r['reason'], r['required_evidence']] for r in inventory], 'Table A.1. Evidence required before a strength conclusion.')),
    ]
    sections.insert(2, _contents())
    css = ('body{font:16px/1.5 system-ui,sans-serif;color:#172033;margin:auto;max-width:1000px;padding:24px}'
           'h2{margin-top:32px}h3{font-size:18px}table{border-collapse:collapse;width:100%;table-layout:fixed}'
           'td,th{border:1px solid #d7dee8;padding:8px;overflow-wrap:anywhere;text-align:left}'
           'caption,figcaption{caption-side:bottom;text-align:left;padding:8px 0;font-size:14px}'
           'svg{width:100%;max-width:720px}a{color:#2a78d6}code{overflow-wrap:anywhere}'
           '@media print{body{font-size:11pt;max-width:none}tr{break-inside:avoid}h2,h3{break-after:avoid}}')
    return ('<!doctype html>\n<html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width,initial-scale=1">'
            f'<meta name="report-revision" content="{escape(revisions["revision"], quote=True)}">'
            f'<title>Hull-girder analysis</title><style>{css}</style></head><body>'
            '<header><h1>Hull-girder analysis</h1><p>Still-water demand and strength-input evidence — not for design use</p></header>'
            + ''.join(sections) + '</body></html>\n')


def _digest(path: Path) -> dict:
    data = path.read_bytes()
    return {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()}


def build(output_dir: Path, *, basis_path: Path = DEMO / "allowable-basis.json",
          revisions_path: Path = DEMO / "revisions.json",
          settings_path: Path = DEMO / "screening_input.yml") -> dict:
    """Write the deterministic report and its numerical and integrity records."""
    settings = yaml.safe_load(settings_path.read_text())["hull_girder_screening"]
    basis = json.loads(basis_path.read_text())
    revisions = json.loads(revisions_path.read_text())
    inventory = allowable_inventory(settings, basis)
    _fixture(settings)
    _revision_history(revisions)
    for actual, canonical in ((basis_path, DEMO / "allowable-basis.json"),
                              (revisions_path, DEMO / "revisions.json"),
                              (settings_path, DEMO / "screening_input.yml")):
        if actual.read_bytes() != canonical.read_bytes():
            raise ValueError("input digest differs from the canonical report basis")
    output_dir = Path(output_dir)
    for name in ("report.html", "results.json", "manifest.json"):
        if (output_dir / name).is_symlink():
            raise ValueError("output symlink would overwrite a caller file")
    results = _calculate(settings, basis)
    report = _render(results, inventory, basis, revisions)
    results["allowables"] = inventory
    results["report_revision"] = revisions["revision"]
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(report, encoding="utf-8", newline="\n")
    (output_dir / "results.json").write_text(
        json.dumps(results, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8"
    )
    manifest = {
        "schema": "hull-girder-report/1", "report_revision": revisions["revision"],
        "strength_disposition": results["strength_disposition"],
        "inputs": {path: _digest(ROOT / path) for path in INPUTS},
        "project_version": tomllib.loads((ROOT / "pyproject.toml").read_text())["project"]["version"],
        "outputs": {name: _digest(output_dir / name) for name in ("report.html", "results.json")},
        "source_records": basis["sources"],
        "rights": basis["rights"], "reporting_conventions": basis["reporting_conventions"],
        "validation_basis": basis["validation"],
    }
    (output_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8"
    )
    verify_manifest(output_dir)
    return manifest


def verify_manifest(output_dir: Path) -> None:
    """Read back every owned input and output digest without path escapes."""
    manifest = json.loads((output_dir / "manifest.json").read_text())
    if (set(manifest["inputs"]) != set(INPUTS)
            or set(manifest["outputs"]) != {"report.html", "results.json"}):
        raise ValueError("manifest inventory mismatch")
    if manifest["project_version"] != tomllib.loads((ROOT / "pyproject.toml").read_text())["project"]["version"]:
        raise ValueError("manifest project version mismatch")
    for directory, records in ((ROOT, manifest["inputs"]), (output_dir, manifest["outputs"])):
        for name, record in records.items():
            path = directory / name
            if Path(name).is_absolute() or not path.resolve().is_relative_to(directory.resolve()):
                raise ValueError("manifest path escape")
            if _digest(path) != record:
                raise ValueError(f"manifest digest mismatch: {name}")


def main() -> int:
    """Build to an explicit destination or verify an existing report pack."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--verify', action='store_true')
    args = parser.parse_args()
    if args.verify:
        verify_manifest(args.output_dir)
    else:
        build(args.output_dir)
    print('Report integrity verified; strength acceptance is not evaluated.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
