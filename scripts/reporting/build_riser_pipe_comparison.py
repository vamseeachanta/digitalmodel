# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare registered riser checks for a nominal pipe without replacing checks."""

import argparse
import hashlib
import html
import inspect
import json
import math
from pathlib import Path

import yaml

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode, DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.units import Q_

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "scripts" / "reporting"
INPUT = DATA / "riser_pipe_comparison.yml"
CRITERIA = DATA / "riser_pipe_comparison.criteria.json"
REVISIONS = DATA / "riser_pipe_comparison.revisions.json"
STEM = "riser-pipe-comparison"
DEFAULT_OUTPUT = ROOT / "docs/reports/2026-10-09-compute-program/riser-pipe-comparison"
CODES = {"API RP 2RD": DesignCode.API_RP_2RD,
         "API STD 2RD": DesignCode.API_STD_2RD}


def _pipe_objects(config):
    """Convert declared dimensions and strengths to the modules' SI inputs."""
    pipe = config["pipe"]
    geometry = PipeGeometry(
        Q_(pipe["outer_diameter_mm"], "mm").to("m").magnitude,
        Q_(pipe["wall_thickness_mm"], "mm").to("m").magnitude,
        Q_(pipe.get("corrosion_allowance_mm", 0), "mm").to("m").magnitude,
        pipe.get("fabrication_tolerance", 0),
    )
    material = PipeMaterial(
        pipe["grade"], Q_(pipe["smys"], pipe["strength_unit"]).to("Pa").magnitude,
        Q_(pipe["smts"], pipe["strength_unit"]).to("Pa").magnitude,
        pipe["youngs_modulus_pa"], pipe["poissons_ratio"],
    )
    return geometry, material


def evaluate(config):
    """Return every registered check and its governing supplied case.

    Args:
        config: One pipe definition with declared units and SI load cases.

    Returns:
        Native utilisation ratios, governing cases, and SI pipe properties.
    """
    _validate(config)
    geometry, material = _pipe_objects(config)
    model = {"results": {}, "governing": {}, "config": config,
             "pipe_si": {"smys_pa": material.smys, "smts_pa": material.smts}}
    for name, code in CODES.items():
        states = {}
        for case in config["cases"]:
            loads = DesignLoads(
                internal_pressure=case["internal_pressure_pa"],
                external_pressure=case["external_pressure_pa"],
                bending_moment=case["bending_moment_nm"],
                effective_tension=case["effective_tension_n"],
            )
            native = CODE_REGISTRY[code]().run_checks(
                geometry, material, loads, DesignFactors())
            for state, (utilisation, details) in native.items():
                if state not in states or utilisation > states[state]["utilisation"]:
                    states[state] = {"utilisation": utilisation,
                                     "case": case["name"], "details": details}
        model["results"][name] = states
        governing = max(states, key=lambda state: states[state]["utilisation"])
        model["governing"][name] = {"state": governing, **states[governing]}
    return model


def _validate(config):
    """Reject malformed or unsupported quantities before invoking the modules."""
    pipe = config["pipe"]
    if pipe["strength_unit"] not in {"Pa", "MPa", "psi"}:
        raise ValueError("Unsupported strength unit")
    fields = ("outer_diameter_mm", "wall_thickness_mm", "smys", "smts",
              "youngs_modulus_pa", "poissons_ratio")
    for field in fields:
        _finite(pipe[field], field)
    if not 0 <= pipe["poissons_ratio"] < 0.5:
        raise ValueError("Poisson ratio must be in the elastic model range")
    for field in ("corrosion_allowance_mm", "fabrication_tolerance"):
        if pipe.get(field, 0) != 0:
            raise ValueError("This report supports nominal wall without deductions")
    _finite(config["water_depth_m"], "water_depth_m")
    if config["water_depth_m"] < 0 or not config["cases"]:
        raise ValueError("Depth must be non-negative and cases must be present")
    names = set()
    for case in config["cases"]:
        if not isinstance(case["name"], str) or not case["name"] or case["name"] in names:
            raise ValueError("Case names must be nonempty and unique")
        names.add(case["name"])
        for field in ("internal_pressure_pa", "external_pressure_pa",
                      "bending_moment_nm", "effective_tension_n"):
            _finite(case[field], field)
        if case["external_pressure_pa"] < 0 or case["internal_pressure_pa"] < case["external_pressure_pa"]:
            raise ValueError("Internal-pressure cases require non-negative net pressure")


def _finite(value, field):
    """Validate a quantity without including its supplied value in errors."""
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
        raise ValueError(f"Non-finite or non-numeric quantity: {field}")


def _sha(path):
    """Digest actual file bytes."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _records(criteria_path, revisions_path):
    """Bind report criteria to module bytes and a declared revision record."""
    criteria = json.loads(Path(criteria_path).read_text())
    record = json.loads(Path(revisions_path).read_text())
    if not record["history"] or record["history"][-1]["revision"] != record["revision"]:
        raise ValueError("Declared revision does not match the newest history row")
    if record["criteria_sha256"] != _sha(criteria_path):
        raise ValueError("Criterion record digest mismatch")
    if set(criteria["codes"]) != set(CODES) or criteria["limit"] != 1:
        raise ValueError("Unexpected module comparison criteria")
    for name, code in CODES.items():
        source = Path(inspect.getfile(CODE_REGISTRY[code]))
        if _sha(source) != criteria["codes"][name]["module_sha256"]:
            raise ValueError(f"Import digest mismatch: {name}")
    return criteria, record


def _percentage(value):
    """Keep unbounded module results distinct from absent results and zero."""
    return "unbounded" if math.isinf(value) else f"{100 * value:.1f}"


def _code_key(code):
    """Produce stable numeric-cell identifiers."""
    return code.lower().replace(" ", "-")


def numeric_values(model):
    """Return formatted values used in the page's summary and tables.

    Args:
        model: Native module results and the supplied pipe configuration.

    Returns:
        Numeric-cell identifiers mapped to display strings.
    """
    config, pipe = model["config"], model["config"]["pipe"]
    values = {"od": f'{pipe["outer_diameter_mm"]:.1f}',
              "wall": f'{pipe["wall_thickness_mm"]:.3f}',
              "smys": f'{model["pipe_si"]["smys_pa"] / 1e6:.1f}',
              "smts": f'{model["pipe_si"]["smts_pa"] / 1e6:.1f}',
              "elastic": f'{pipe["youngs_modulus_pa"] / 1e9:.1f}',
              "poisson": f'{pipe["poissons_ratio"]:.1f}',
              "depth": f'{config["water_depth_m"]:.1f}'}
    for index, case in enumerate(config["cases"]):
        for field, divisor in (("internal_pressure_pa", 1e6), ("external_pressure_pa", 1e6),
                               ("bending_moment_nm", 1e3), ("effective_tension_n", 1e3)):
            values[f"case-{index}-{field}"] = f"{case[field] / divisor:.1f}"
    for code, states in model["results"].items():
        key = _code_key(code)
        values[f"governing-{key}"] = _percentage(model["governing"][code]["utilisation"])
        for state, result in states.items():
            values[f"{key}-{state}"] = _percentage(result["utilisation"])
            values[f"limit-{key}-{state}"] = "100.0"
    return values


def _number(values, key):
    """Render an identifiable numeric value for independent page readback."""
    return f'<span data-number="{key}">{values[key]}</span>'


def _summary(model, values):
    """State the largest returned result without claiming complete code coverage."""
    lines = ['<h2>Summary</h2><p>Internal program benchmarking analysis for one generic assumed pipe. '
             'The comparison covers repository-module outputs; standard conformity is not established.</p>']
    for code, result in model["governing"].items():
        disposition = "within" if result["utilisation"] <= 1 else "above"
        lines.append(f'<p>{code}: governing returned check is {result["state"]}, case '
                     f'{html.escape(result["case"])}, utilisation '
                     f'{_number(values, "governing-" + _code_key(code))}% — {disposition} '
                     'the module unity criterion. Dispositions use unrounded results.</p>')
    lines.append('<p>API RP 1111 is not compared because its recorded edition conflicts with the standards inventory.</p>')
    return "\n".join(lines)


def _history(record):
    """Generate revision history directly after the summary from retained data."""
    rows = ''.join(f'<tr data-revision="{html.escape(row["revision"])}"><td>{html.escape(row["revision"])}</td>'
                   f'<td>{html.escape(row["date"])}</td><td>{html.escape(row["change"])}</td></tr>'
                   for row in record["history"])
    return ('<h2>Revision history</h2><table><thead><tr><th>Revision</th><th>Date</th>'
            '<th>What changed and why</th></tr></thead><tbody>' + rows +
            '</tbody></table><p class="caption">Report issue history.</p>')


def _input_table(model, values):
    """Present sourced properties separately from assumed quantities."""
    labels = {"od": "Outer diameter (mm)", "wall": "Nominal wall thickness (mm)",
              "smys": "Specified minimum yield strength (MPa)",
              "smts": "Specified minimum tensile strength (MPa)",
              "elastic": "Young modulus (GPa)", "poisson": "Poisson ratio (1)",
              "depth": "Water depth (m)"}
    rows = []
    for key, label in labels.items():
        field = {"od": "outer_diameter_mm", "wall": "wall_thickness_mm",
                 "smys": "smys", "smts": "smts"}.get(key)
        basis = html.escape(model["config"].get("input_basis", {}).get(field, "Assumed"))
        rows.append(f'<tr><td>{label}</td><td>{_number(values, key)}</td><td>{basis}</td></tr>')
    for index, case in enumerate(model["config"]["cases"]):
        for field, label in (("internal_pressure_pa", "Internal pressure (MPa)"),
                             ("external_pressure_pa", "External pressure (MPa)"),
                             ("bending_moment_nm", "Bending moment (kN m)"),
                             ("effective_tension_n", "Effective tension (kN)")):
            rows.append(f'<tr><td>{html.escape(case["name"])}: {label}</td>'
                        f'<td>{_number(values, f"case-{index}-{field}")}</td><td>Assumed</td></tr>')
    return ('<h2>Input pipe</h2><p>Material: ' + html.escape(model["config"]["pipe"]["grade"]) +
            '.</p><table><thead><tr><th>Quantity (units)</th><th>Value</th><th>Basis</th></tr></thead><tbody>' +
            ''.join(rows) + '</tbody></table><p class="caption">Nominal properties and supplied load cases.</p>'
            '<p>Loads, depth and elastic properties are assumed, based on previous experience. '
            'Zero corrosion allowance and fabrication deduction are assumed. Both modules use nominal wall; '
            'collapse credits no internal-pressure support. Water depth is context and does not determine '
            'the independently supplied absolute external pressure. Material suitability, tolerance allowances '
            'and material certification are not established.</p>')


def _state_table(state, model, values, criteria):
    """Show each returned check or an explicit unavailable comparison."""
    rows, bars = [], []
    for code, states in model["results"].items():
        if state not in states:
            gap = ("Not Evaluated: the legacy module returns no combined-load check (tracked in #377)"
                   if state == "combined" else "Not returned by this strategy")
            rows.append(f'<tr><td>{code}</td><td colspan="4">{html.escape(gap)}</td></tr>')
            continue
        result, key = states[state], _code_key(code)
        reference = criteria["codes"][code]
        section = reference["checks"][state]["section"]
        edition = reference.get("reference_edition", reference["revision"])
        disposition = "Within module limit" if result["utilisation"] <= 1 else "Above module limit"
        rows.append(f'<tr><td>{code}</td><td>{_number(values, key + "-" + state)}</td>'
                    f'<td>{html.escape(result["case"])}</td><td>Utilisation &le; '
                    f'{_number(values, "limit-" + key + "-" + state)}%; '
                    f'{html.escape(reference["title"])}; {html.escape(edition)}; '
                    f'{html.escape(section)}.</td><td>{disposition}</td></tr>')
        width = min(result["utilisation"], 2) * 150
        y = 12 + len(bars) * 28
        bars.append(f'<text x="0" y="{y + 12}">{code}</text><rect x="130" y="{y}" '
                    f'width="{width:.1f}" height="16"/><text x="440" y="{y + 12}">'
                    f'{values[key + "-" + state]}%</text>')
    rows.append('<tr><td>API RP 1111</td><td colspan="4">Not compared: edition defect</td></tr>')
    return (f'<h2 id="{state}">{state.capitalize()}</h2><table><thead><tr><th>Code</th>'
            '<th>Utilisation (%)</th><th>Governing supplied case</th><th>Criterion and reference</th>'
            '<th>Disposition</th></tr></thead><tbody>' + ''.join(rows) +
            '</tbody></table><p class="caption">Returned ' + state + ' results; reference locators remain unqualified.</p>'
            '<figure><svg viewBox="0 0 540 80" role="img" aria-label="Returned utilisation comparison">' +
            ''.join(bars) + '</svg><figcaption>Returned utilisation; bar lengths are capped at twice unity.</figcaption></figure>')


def _discussion(model):
    """Explain observed differences using the returned numerical results."""
    rp, std = model["results"]["API RP 2RD"], model["results"]["API STD 2RD"]
    lines = ['<h2>Discussion and limits</h2>']
    for state, left, right in (("burst", rp, std), ("collapse", std, rp)):
        comparator = right[state]["utilisation"]
        if comparator > 0 and math.isfinite(left[state]["utilisation"]):
            difference = 100 * (left[state]["utilisation"] / comparator - 1)
            code = "API RP 2RD" if state == "burst" else "API STD 2RD"
            lines.append(f'<p>{code} {state} utilisation is {difference:.1f}% higher than its comparator '
                         'for the same supplied pipe and loads. The modules return equal characteristic '
                         'pressure capacities and different design capacities for these pressure checks.</p>')
    lines.append('<p>Not Evaluated: the legacy module returns no combined-load check (tracked in #377). '
                 'A ranking of complete code assessments is not established. '
                 'Not Evaluated: legacy edition and clause locators are not established; '
                 'STD clause locators are inherited from repository '
                 'references and have not been checked against a licensed standard. Required minimum wall '
                 'and a second sourced pipe configuration are not calculated. The next step is to qualify '
                 'the citation basis and complete legacy combined-load coverage before a design disposition.</p>')
    return '\n'.join(lines)


def _citations(criteria):
    """Inventory inherited coefficients without reproducing licensed values."""
    entries = []
    for code, reference in criteria["codes"].items():
        for constant in reference["constants"]:
            state = next((name for name in reference["checks"] if name in constant.lower()), "burst")
            entries.append({"code": code, "constant": constant,
                            **{key: reference[key] for key in ("code_id", "title", "publisher", "revision",
                                                               "source_sibling", "source_project", "module",
                                                               "module_sha256")},
                            "section": reference["checks"][state]["section"],
                            "qualification": "reference only; clause mapping not established"})
    return {"citations": entries,
            "qualification": "partial; standard conformity not established",
            "note": "No new standards-derived coefficient is introduced. Existing module coefficients are inventoried by symbol; numeric factors are not reproduced."}


def _page(model, criteria, record):
    """Render one standalone page with local styling and inline graphics."""
    values = numeric_values(model)
    body = [_summary(model, values), _history(record), _input_table(model, values)]
    for state in ("burst", "collapse", "hoop", "combined"):
        body.append(_state_table(state, model, values, criteria))
    body.append(_discussion(model))
    links = ''.join(f'<li><a href="{html.escape(source["url"], quote=True)}">'
                    f'{html.escape(source["title"])}</a>; retrieved '
                    f'{html.escape(source["retrieved_at"])}</li>' for source in model["config"]["sources"])
    body.append('<h2>Input basis</h2><p>Dimensions and strengths are assumed generic properties; '
                'no schedule or grade designation is used.</p><ul>' + links + '</ul>')
    return ('<!doctype html>\n<html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width,initial-scale=1">'
            '<title>Program benchmarking analysis: riser pipe modules</title><style>'
            'body{font:16px system-ui,sans-serif;max-width:1100px;margin:32px auto;padding:0 24px;color:#203044}'
            'h1,h2{color:#163956}table{border-collapse:collapse;width:100%;margin:12px 0}'
            'th,td{border:1px solid #bdcbd4;padding:9px;text-align:left}th{background:#edf3f7}'
            '.caption,figcaption{font-size:14px;color:#42596b}svg{width:100%;max-width:700px}'
            'rect{fill:#386c94}svg text{font-size:12px}figure{margin:12px 0}'
            '</style></head><body><h1>Program benchmarking analysis: riser pipe modules</h1>\n' +
            '\n'.join(body) + '\n</body></html>\n')


def _relative(path):
    """Keep machine paths out of portable receipts."""
    try:
        return Path(path).resolve().relative_to(ROOT).as_posix()
    except ValueError:
        return Path(path).name


def build(input_path, output_dir, *, criteria_path=CRITERIA, revisions_path=REVISIONS):
    """Build the page, citation inventory and byte-digest manifest.

    Args:
        input_path: YAML pipe definition and public-source metadata.
        output_dir: Explicit destination for the three generated files.
        criteria_path: Frozen module criteria and coefficient inventory.
        revisions_path: Declared revision and immutable issue history.

    Returns:
        The portable manifest with rounded metrics and unrounded dispositions.
    """
    criteria, record = _records(criteria_path, revisions_path)
    config = yaml.safe_load(Path(input_path).read_text())
    model = evaluate(config)
    if any(set(states) != set(criteria["codes"][code]["checks"]) for code, states in model["results"].items()):
        raise ValueError("Returned checks differ from the frozen criteria")
    for source in config["sources"]:
        if not source["url"].startswith(("https://", "http://")):
            raise ValueError("Public source URL scheme is unsupported")
    output = Path(output_dir)
    output.mkdir(parents=True, exist_ok=True)
    page, sidecar = output / f"{STEM}.html", output / f"{STEM}.citations.json"
    page.write_text(_page(model, criteria, record), encoding="utf-8", newline="\n")
    sidecar.write_text(json.dumps(_citations(criteria), indent=2, sort_keys=True) + '\n', encoding="utf-8")
    manifest = _manifest(model, criteria, record, input_path, criteria_path, revisions_path, (page, sidecar))
    (output / f"{STEM}.manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True, allow_nan=False) + '\n', encoding="utf-8")
    return manifest


def _manifest(model, criteria, record, input_path, criteria_path, revisions_path, outputs):
    """Retain compact metrics, criterion decisions and actual artifact digests."""
    results = {}
    for code, states in model["results"].items():
        results[code] = {state: {"utilisation_percent": "unbounded" if math.isinf(result["utilisation"])
                                else round(100 * result["utilisation"], 1),
                                "within_module_limit": result["utilisation"] <= criteria["limit"],
                                "case": result["case"]} for state, result in states.items()}
    paths = [input_path, criteria_path, revisions_path, Path(__file__)]
    paths.extend(ROOT / entry["module"] for entry in criteria["codes"].values())
    return {"revision": record["revision"], "criteria_revision": criteria["criteria_revision"],
            "criteria_recorded_at": record["criteria_recorded_at"], "basis_commit": model["config"]["basis_commit"],
            "qualification": "partial; standard conformity not established", "results": results,
            "precision": "0.1 percent; dispositions use unrounded module results",
            "inputs": {_relative(path): _sha(path) for path in paths},
            "artifacts": {path.name: {"sha256": _sha(path), "size_bytes": path.stat().st_size} for path in outputs}}


def main():
    """Run the builder with explicit, portable input and output defaults."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=INPUT)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()
    manifest = build(args.input, args.output_dir)
    print(json.dumps({"revision": manifest["revision"], "artifacts": manifest["artifacts"]}, sort_keys=True))


if __name__ == "__main__":
    main()
