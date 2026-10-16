# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Compare registered riser checks for a nominal pipe without replacing checks."""

import argparse
import hashlib
import html
import inspect
import json
import math
from decimal import Decimal
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode, DesignFactors, DesignLoads, PipeGeometry, PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY, CodeStrategy
from digitalmodel.units import Q_

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "scripts" / "reporting"
INPUT = DATA / "riser_pipe_comparison.yml"
CRITERIA = DATA / "riser_pipe_comparison.criteria.json"
REVISIONS = DATA / "riser_pipe_comparison.revisions.json"
STEM = "riser-pipe-comparison"
DEFAULT_OUTPUT = ROOT / "docs/reports/2026-10-09-compute-program/riser-pipe-comparison"
CODES = {"API RP 1111": DesignCode.API_RP_1111,
         "API RP 2RD": DesignCode.API_RP_2RD,
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
             "pipe_si": {"smys_pa": material.smys, "smts_pa": material.smts},
             "minimum_wall": {}}
    for name, code in CODES.items():
        states = {}
        for case in config["cases"]:
            loads = DesignLoads(
                internal_pressure=case["internal_pressure_pa"],
                external_pressure=case["external_pressure_pa"],
                bending_moment=case["bending_moment_nm"],
                effective_tension=case["effective_tension_n"],
            )
            native = _module_checks(CODE_REGISTRY[code](),
                geometry, material, loads, DesignFactors())
            for state, (utilisation, details) in native.items():
                if state not in states or utilisation > states[state]["utilisation"]:
                    states[state] = {"utilisation": utilisation,
                                     "case": case["name"], "details": details}
        model["results"][name] = states
        governing = max(states, key=lambda state: states[state]["utilisation"])
        model["governing"][name] = {"state": governing, **states[governing]}
        model["minimum_wall"][name] = _minimum_wall(config, code, material)
    return model


def _module_checks(
    strategy: CodeStrategy, geometry: PipeGeometry, material: PipeMaterial,
    loads: DesignLoads, factors: DesignFactors,
) -> dict[str, tuple[float, dict[str, float]]]:
    """Reject empty, negative or NaN results while preserving positive infinity."""
    native: dict[str, tuple[float, dict[str, float]]] = strategy.run_checks(geometry, material, loads, factors)
    if not native or any(math.isnan(value) or value < 0 for value, _ in native.values()):
        raise ValueError("Invalid module result")
    return native


def _minimum_wall(
    config: dict[str, Any], code: DesignCode, material: PipeMaterial,
) -> dict[str, Any]:
    """Find the first nominal wall passing all returned checks on a 0.1 mm grid.

    Enumerate the grid rather than assuming that every check is monotonic.
    The upper bound is strictly below half the supplied outside diameter.
    This is a module-only search, without code applicability qualification.
    """
    diameter = config["pipe"]["outer_diameter_mm"]
    strategy = CODE_REGISTRY[code]()
    for tick in range(1, math.ceil(diameter * 5)):
        wall_mm = tick / 10
        geometry = PipeGeometry(diameter / 1000, wall_mm / 1000,
                                corrosion_allowance=0, fabrication_tolerance=0)
        envelope: list[tuple[float, str, str]] = []
        for case in config["cases"]:
            loads = DesignLoads(case["internal_pressure_pa"], case["external_pressure_pa"],
                                case["bending_moment_nm"], case["effective_tension_n"])
            native = _module_checks(strategy, geometry, material, loads, DesignFactors())
            envelope.extend((value, state, case["name"]) for state, (value, _) in native.items())
        utilisation, state, case_name = max(envelope, key=lambda row: row[0])
        if utilisation <= 1:
            return {"status": "found", "wall_mm": wall_mm, "state": state, "case": case_name}
    return {"status": "not found on search grid"}


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


def _input_number(value, divisor=1):
    """Preserve supplied numeric precision through display-unit scaling."""
    return format(Decimal(str(value)) / Decimal(str(divisor)), "f")


def numeric_values(model):
    """Return formatted values used in the page's summary and tables.

    Args:
        model: Native module results and the supplied pipe configuration.

    Returns:
        Numeric-cell identifiers mapped to display strings.
    """
    config, pipe = model["config"], model["config"]["pipe"]
    values = {"od": _input_number(pipe["outer_diameter_mm"]),
              "wall": _input_number(pipe["wall_thickness_mm"]),
              "smys": _input_number(model["pipe_si"]["smys_pa"], 1e6),
              "smts": _input_number(model["pipe_si"]["smts_pa"], 1e6),
              "elastic": _input_number(pipe["youngs_modulus_pa"], 1e9),
              "poisson": _input_number(pipe["poissons_ratio"]),
              "depth": _input_number(config["water_depth_m"])}
    for index, case in enumerate(config["cases"]):
        for field, divisor in (("internal_pressure_pa", 1e6), ("external_pressure_pa", 1e6),
                               ("bending_moment_nm", 1e3), ("effective_tension_n", 1e3)):
            values[f"case-{index}-{field}"] = _input_number(case[field], divisor)
    for code, states in model["results"].items():
        key = _code_key(code)
        minimum = model["minimum_wall"][code]
        if minimum["status"] == "found":
            values[f"minimum-{key}"] = f'{minimum["wall_mm"]:.1f}'
        values[f"governing-{key}"] = _percentage(model["governing"][code]["utilisation"])
        for state, result in states.items():
            values[f"{key}-{state}"] = _percentage(result["utilisation"])
            values[f"limit-{key}-{state}"] = "100.0"
    return values


def _number(values, key):
    """Render an identifiable numeric value for independent page readback."""
    return f'<span data-number="{key}">{values[key]}</span>'


def _rp1111_screen_note():
    """Qualify external-only screening wherever its results are presented."""
    return ('<p>RP 1111 collapse and propagation use external pressure with no internal-pressure credit: '
            'these are empty-pipe screens, not net-pressure results for the supplied operating condition. '
            'Under RP 1111, a propagation exceedance is a buckle-arrestor criterion, not a wall requirement. '
            'Arrestor design and standard conformity are not established.</p>')


def _factor_basis(model):
    """Disclose returned implementation factors without qualifying a standard."""
    states = model["results"]["API RP 1111"]
    factors = [(state, symbol, states[state]["details"][symbol])
               for state, symbol in (("burst", "f_d"), ("collapse", "f_c"), ("propagation", "f_p"))]
    rows = ''.join(f'<tr><td>{state.capitalize()}</td><td>{symbol} = {value:.2f}</td></tr>'
                   for state, symbol, value in factors)
    return ('<h2>Module factor basis</h2><p>API RP 1111: pipeline burst factor; collapse factor unqualified. '
            'The module uses the pipeline burst basis, not a riser burst basis. '
            'Collapse and propagation factors are unqualified implementation values; '
            'their standard applicability is not established.</p>'
            '<table><thead><tr><th>Returned check</th><th>Implementation factor (1)</th></tr></thead><tbody>' +
            rows + '</tbody></table><p class="caption">Factors read from the returned module details; '
            'citation symbols and pinned module bytes are retained in the sidecars.</p>'
            '<p>All three strategies ignore the DesignFactors argument and use factors fixed inside the modules. '
            'The builder cannot request a riser-factor RP 1111 run through that argument. '
            'Factor overrides, edition metadata and historical propagation factors require module qualification '
            '(tracked in #377).</p>')


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
    lines.append('<p>API RP 1111 module edition metadata is unqualified: the strategy defaults to '
                 '2015, with an edition label that conflicts with the standards inventory. '
                 'The comparison uses that implementation without endorsing the edition label.</p>')
    lines.append(_rp1111_screen_note())
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
            'Zero corrosion allowance and fabrication deduction are assumed. All three modules use nominal wall; '
            'collapse and RP 1111 propagation credit no internal-pressure support. Water depth is context and does not determine '
            'the independently supplied absolute external pressure. Material suitability, tolerance allowances '
            'and material certification are not established.</p>')


def _state_table(state, model, values, criteria):
    """Show each returned check or an explicit unavailable comparison."""
    rows, bars = [], []
    for code, states in model["results"].items():
        if state not in states:
            gap = ("Not Evaluated: the legacy module returns no combined-load check (tracked in #377)"
                   if state == "combined" and code == "API RP 2RD"
                   else "Not Evaluated: not returned by this strategy")
            rows.append(f'<tr><td>{code}</td><td colspan="4">{html.escape(gap)}</td></tr>')
            continue
        result, key = states[state], _code_key(code)
        reference = criteria["codes"][code]
        section = reference["checks"][state]["section"]
        edition = reference.get("reference_edition", reference["revision"])
        disposition = "Within module limit" if result["utilisation"] <= 1 else "Above module limit"
        label = code + ("<br>pipeline burst factor; collapse factor unqualified" if code == "API RP 1111" else "")
        rows.append(f'<tr><td>{label}</td><td>{_number(values, key + "-" + state)}</td>'
                    f'<td>{html.escape(result["case"])}</td><td>Utilisation &le; '
                    f'{_number(values, "limit-" + key + "-" + state)}%; '
                    f'{html.escape(reference["title"])}; {html.escape(edition)}; '
                    f'{html.escape(section)}.</td><td>{disposition}</td></tr>')
        width = min(result["utilisation"], 2) * 150
        y = 12 + len(bars) * 28
        bars.append(f'<text x="0" y="{y + 12}">{code}</text><rect x="130" y="{y}" '
                    f'width="{width:.1f}" height="16"/><text x="440" y="{y + 12}">'
                    f'{values[key + "-" + state]}%</text>')
    pressure_note = (_rp1111_screen_note() + '<p>RP 1111 propagation net demand would be p_o - p_i, '
                     'non-positive for every case accepted by this builder; the returned module screen uses p_o alone.</p>'
                     if state == "propagation" else "")
    return (f'<h2 id="{state}">{state.capitalize()}</h2>' + pressure_note + '<table><thead><tr><th>Code</th>'
            '<th>Utilisation (%)</th><th>Governing supplied case</th><th>Criterion and reference</th>'
            '<th>Disposition</th></tr></thead><tbody>' + ''.join(rows) +
            '</tbody></table><p class="caption">Returned ' + state + ' results; reference locators remain unqualified.</p>'
            '<figure><svg viewBox="0 0 540 110" role="img" aria-label="Returned utilisation comparison">' +
            ''.join(bars) + '</svg><figcaption>Returned utilisation; bar lengths are capped at twice unity.</figcaption></figure>')


def _minimum_table(model: dict[str, Any], values: dict[str, str]) -> str:
    """Report the search bounds and the governing check at the first passing wall."""
    rows = []
    for code, result in model["minimum_wall"].items():
        if result["status"] == "found":
            rows.append(f'<tr><td>{code}</td><td>{_number(values, "minimum-" + _code_key(code))}</td>'
                        f'<td>{result["state"]}</td><td>{html.escape(result["case"])}</td></tr>')
        else:
            rows.append(f'<tr><td>{code}</td><td colspan="3">Not found on search grid</td></tr>')
    return ('<h2>Minimum passing nominal wall</h2><p>First passing point on a 0.1 mm grid, '
            'starting at 0.1 mm and ending strictly below half the outer diameter. All returned '
            'checks must pass for every supplied case. Enumeration makes no monotonicity assumption. '
            'The governing check and case below apply at the searched wall, not the supplied wall. '
            'This is not a qualified design minimum: missing checks, applicability ranges, '
            'corrosion, fabrication deductions and manufacturing availability are not established.</p>' +
            _rp1111_screen_note() +
            '<p>RP 1111 includes propagation; RP 2RD omits combined loads; STD 2RD includes combined loads. '
            'The values are not directly comparable design minima. The RP 1111 grid result includes an '
            'empty-pipe propagation screen; it does not establish a required operating wall.</p>'
            '<table><thead><tr><th>Code</th><th>Nominal wall (mm)</th><th>Governing check</th>'
            '<th>Governing supplied case</th></tr></thead><tbody>' + ''.join(rows) +
            '</tbody></table><p class="caption">Module-only minimum on the declared search grid.</p>')


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
                 'references and have not been checked against a licensed standard. RP 1111 returns propagation '
                 'but no hoop or combined check; neither 2RD strategy returns propagation. '
                 'The report uses one generic pipe; shallow and deep reference configurations are tested separately. '
                 'The next step is to qualify '
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
                            "qualification": "unqualified module metadata; clause mapping not established"})
    return {"citations": entries,
            "qualification": "partial; standard conformity not established",
            "note": "No new standards-derived coefficient is introduced. Existing module coefficients are inventoried by symbol; numeric factors are not reproduced."}


def _page(model, criteria, record):
    """Render one standalone page with local styling and inline graphics."""
    values = numeric_values(model)
    body = [_summary(model, values), _history(record), _input_table(model, values), _factor_basis(model)]
    body.append(_minimum_table(model, values))
    for state in ("burst", "collapse", "propagation", "hoop", "combined"):
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
            "minimum_wall": model["minimum_wall"],
            "wall_search": "0.1 mm grid; positive wall strictly below half the outside diameter; all returned checks and supplied cases",
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
