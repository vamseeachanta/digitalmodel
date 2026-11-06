# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the pipeline module report from synthetic, independently derived cases.

Numeric basis A: polar integration of an annulus with radii 1 and 2 m gives
wall/bore/outside areas 3*pi, pi, 4*pi m^2 and I=15*pi/4 m^4; thickness=1 m.
Numeric basis B: integrate uniform strain alpha*dT over an unrestrained length.
Synthetic L=1000 m, alpha=1/100000 /K and dT=+/-20 K give strain +/-1/5000
and growth +/-1/5 m. Numeric basis C: arbitrary capacity 100 MPa multiplied
by arbitrary caller factors 4/5, 9/10 and 3/5 gives 80, 90 and 60 MPa.
These bases supply all calculation inputs; none is a code allowable.
"""

import argparse
import ast
import hashlib
import html
import json
from pathlib import Path
import stat

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "src/digitalmodel/subsea/pipeline"
DESTINATION = ROOT / "docs/reports/modules/subsea_pipeline"
SCRIPT = "scripts/reports/build_subsea_pipeline_module_report.py"
CHANGES = [
    {
        "revision": "A",
        "date": "2026-10-28",
        "change": "Independent synthetic calculations and a public-method inventory are added to distinguish arithmetic checks from unresolved engineering qualification. Stale calculation-package imports are corrected so the public functions can be used.",
    },
    {
        "revision": "B",
        "date": "2026-11-06",
        "change": "Qualification is restricted to tested symbols, the incidental-pressure output mislabel is disclosed, and provenance is limited to executed sources and dedicated evidence so unrelated source edits do not stale the report. The reserved scratch option becomes optional and the compact layout choice is explained.",
    },
    {
        "revision": "C",
        "date": "2026-11-06",
        "change": "Output validation rejects unrelated checkout destinations and linked targets before calculation or writing. The inventory rebuild test is named explicitly, and revision B is dated to its introducing commit.",
    },
    {
        "revision": "D",
        "date": "2026-11-06",
        "change": "Output aliases are validated by their resolved destination, and external directories must be empty or contain only recognised report deliverables before replacement. Link tests handle unavailable privileges, and the change panel displays the latest revision.",
    },
    {
        "revision": "E",
        "date": "2026-11-06",
        "change": "Recognised report files with cloud placeholder metadata can be rebuilt while symbolic links and directory junctions remain rejected. Scratch containment uses the resolved checkout root so checkout aliases retain the same restriction.",
    },
    {
        "revision": "F",
        "date": "2026-11-06",
        "change": "Deliverable protection rejects every name-surrogate reparse tag, including WSL symbolic links, while retaining cloud placeholder rebuilds. Inventory evidence now covers public async functions and methods as well as synchronous declarations.",
    },
]

# Synthetic inputs from bases A, B and C in the module docstring, respectively.
ANNULUS = {"outer_diameter": 4.0, "inner_diameter": 2.0, "wall_thickness": 1.0}
GROWTH = {"length": 1000.0, "alpha": 1 / 100000}
TEMPERATURE_CHANGES = (20.0, -20.0)
BASE_CAPACITY_MPA = 100.0
FACTORS = {"design": 4 / 5, "incidentalPressure": 9 / 10, "hydroStaticTest": 3 / 5}
GEOMETRY_UNITS = {
    "outer_diameter": "m",
    "inner_diameter": "m",
    "wall_thickness": "m",
    "A": "m^2",
    "Ai": "m^2",
    "Ao": "m^2",
    "I": "m^4",
}


def source_digests():
    """Hash executed sources and dedicated evidence, excluding inventory-only code."""
    paths = [
        "src/digitalmodel/subsea/pipeline/calculations/pipe_properties.py",
        "src/digitalmodel/subsea/pipeline/calculations/stress_calculations.py",
        "src/digitalmodel/subsea/pipeline/thermal_expansion.py",
        "src/digitalmodel/units.py",
        SCRIPT,
        "tests/subsea/pipeline/test_module_report_hand_values.py",
        "tests/reporting/test_subsea_pipeline_module_report.py",
    ]
    return {
        path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        for path in sorted(paths)
    }


def _category(path, symbol):
    if path.name == "thermal_expansion.py":
        return "thermal strain"
    if path.name in {"pipe_properties.py", "pipe_sizing.py"}:
        return "geometry"
    if "buckling" in path.as_posix():
        return "buckling"
    if "free_span" in path.parts or path.name == "span_rectification.py":
        return "span"
    if path.name == "pipeline_pressure_workflow.py" and (
        symbol.startswith("WallThicknessSizingWorkflow.")
        or symbol == "run_sizing_from_yaml"
    ):
        return "workflow"
    if path.name in {"pipeline.py", "integrity_workflows.py"}:
        return "workflow"
    if "pressure" in path.name or path.name == "stress_calculations.py":
        return "pressure"
    return "workflow"


# Evidence attaches to these exact declarations, never to a whole module.
HAND_CHECKED = {
    ("calculations/pipe_properties.py", symbol)
    for symbol in (
        "calculate_section_properties",
        "calculate_wall_thickness",
        "calculate_diameter_from_thickness",
        "calculate_geometric_properties",
        "validate_pipe_geometry",
    )
} | {("thermal_expansion.py", "free_thermal_expansion")}
ARITHMETIC_CHECKED = (
    "calculations/stress_calculations.py",
    "APISTD2RDCalculations.calculate_design_pressures",
)


def _qualification(path, symbol, category):
    identity = (path.relative_to(PACKAGE).as_posix(), symbol)
    if identity in HAND_CHECKED and identity[0] == "calculations/pipe_properties.py":
        return (
            "HAND_CALCULATION_CHECKED",
            "A",
            (
                "The consistent synthetic annulus only is checked; broader input validity and engineering use need separate evidence."
            ),
        )
    if identity in HAND_CHECKED and identity[0] == "thermal_expansion.py":
        return (
            "HAND_CALCULATION_CHECKED",
            "B",
            (
                "Uniform free growth only is checked; restraint, soil response, buckling and walking need independent evidence."
            ),
        )
    if identity == ARITHMETIC_CHECKED:
        return (
            "ARITHMETIC_ONLY",
            "C",
            (
                "The supplied capacity and factors need their own equation, edition, applicability and independent worked-case evidence."
            ),
        )
    needed = {
        "thermal strain": "Independent hand-value cases for this declaration and its input assumptions are needed.",
        "geometry": "Independent cases for composite sections, coatings, buoyancy and unit conventions are needed.",
        "buckling": "Independent restraint and soil-force cases, equation attribution, imperfection sensitivity and qualified mode factors are needed.",
        "span": "Independent frequency, onset, amplitude and fatigue cases with edition-specific boundary, sag, proximity and empirical-factor evidence are needed.",
        "pressure": "Edition-specific equations, material and corrosion basis, units, capacity factors and independent worked cases are needed.",
        "workflow": "End-to-end input/output cases, dependency evidence and independently qualified delegated methods are needed.",
    }
    return "NOT_EVALUATED", "Source inventory only", needed[category]


def method_inventory():
    """Locate public declarations statically without executing unqualified routes."""
    rows = []
    for path in sorted(PACKAGE.rglob("*.py")):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        declarations = []
        for node in tree.body:
            if isinstance(
                node, (ast.FunctionDef, ast.AsyncFunctionDef)
            ) and not node.name.startswith("_"):
                declarations.append(node.name)
            if isinstance(node, ast.ClassDef) and not node.name.startswith("_"):
                declarations.extend(
                    f"{node.name}.{method.name}"
                    for method in node.body
                    if isinstance(method, (ast.FunctionDef, ast.AsyncFunctionDef))
                    and not method.name.startswith("_")
                )
        for symbol in declarations:
            category = _category(path, symbol)
            evaluation, basis, needed = _qualification(path, symbol, category)
            rows.append(
                {
                    "path": path.relative_to(ROOT).as_posix(),
                    "symbol": symbol,
                    "category": category,
                    "evaluation": evaluation,
                    "basis": basis,
                    "evidence_needed": needed,
                }
            )
    return rows


def build_payload():
    """Execute the public arithmetic functions with the declared synthetic inputs."""
    from digitalmodel.subsea.pipeline.calculations import (
        APISTD2RDCalculations,
        calculate_diameter_from_thickness,
        calculate_geometric_properties,
        calculate_section_properties,
        calculate_wall_thickness,
        validate_pipe_geometry,
    )
    from digitalmodel.subsea.pipeline.thermal_expansion import free_thermal_expansion

    od, inner, thickness = (
        ANNULUS[key] for key in ("outer_diameter", "inner_diameter", "wall_thickness")
    )
    route_inputs = {
        "od_id": {"outer_diameter": od, "inner_diameter": inner},
        "od_t": {"outer_diameter": od, "wall_thickness": thickness},
        "id_t": {"inner_diameter": inner, "wall_thickness": thickness},
        "od_id_t": dict(ANNULUS),
    }
    cases = []
    for temperature in TEMPERATURE_CHANGES:
        growth = free_thermal_expansion(GROWTH["length"], GROWTH["alpha"], temperature)
        cases.append(
            {
                "temperature_change": temperature,
                "strain": growth / GROWTH["length"],
                "growth": growth,
            }
        )
    return {
        "annulus": {
            "inputs": dict(ANNULUS),
            "units": dict(GEOMETRY_UNITS),
            "routes": {
                name: calculate_geometric_properties(**values)
                for name, values in route_inputs.items()
            },
            "section": calculate_section_properties(od, inner),
            "wall_thickness": calculate_wall_thickness(od, inner),
            "derived_inner_diameter": calculate_diameter_from_thickness(
                od, thickness, "outer"
            ),
            "derived_outer_diameter": calculate_diameter_from_thickness(
                inner, thickness, "inner"
            ),
            "consistent": validate_pipe_geometry(od, inner, thickness),
            "evaluation": "HAND_CALCULATION_CHECKED",
            "basis": "A",
        },
        "growth": {
            "inputs": dict(GROWTH),
            "cases": cases,
            "units": {
                "length": "m",
                "alpha": "K^-1",
                "temperature_change": "K",
                "strain": "1",
                "growth": "m",
            },
            "evaluation": "HAND_CALCULATION_CHECKED",
            "basis": "B",
            "buckling_and_walking": "NOT_EVALUATED",
        },
        "pressure_factors": {
            "inputs": {"base_capacity": BASE_CAPACITY_MPA, "factors": dict(FACTORS)},
            "results": APISTD2RDCalculations.calculate_design_pressures(
                BASE_CAPACITY_MPA, {"internalPressure": dict(FACTORS)}
            ),
            "units": {"base_capacity": "MPa", "factors": "1", "results": "MPa"},
            "evaluation": "ARITHMETIC_ONLY",
            "basis": "C",
            "capacity_qualification": "NOT_EVALUATED",
            "factor_qualification": "NOT_EVALUATED",
        },
        "methods": method_inventory(),
    }


def _json(value):
    return json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"


def _table(headers, rows, caption):
    head = "".join(f"<th>{html.escape(str(value))}</th>" for value in headers)
    body = "\n".join(
        "<tr>"
        + "".join(f"<td>{html.escape(str(value))}</td>" for value in row)
        + "</tr>"
        for row in rows
    )
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table><p class=caption>{html.escape(caption)}</p>"


def render(payload, sources):
    """Render the ten-section report with results taken from the public payload."""
    annulus, growth, pressure = (
        payload[key] for key in ("annulus", "growth", "pressure_factors")
    )
    geometry_rows = [
        (route, key, f"{value:.3f}", annulus["units"][key], "A")
        for route, result in annulus["routes"].items()
        for key, value in result.items()
    ]
    growth_rows = [
        (
            f"{case['temperature_change']:+g}",
            f"{case['strain']:.1e}",
            f"{case['growth']:+.3f}",
            "B",
        )
        for case in growth["cases"]
    ]
    factor_keys = {
        "design": "design",
        "accidental": "incidentalPressure",
        "test": "hydroStaticTest",
    }
    pressure_rows = [
        (
            "incidental (returned as accidental)" if name == "accidental" else name,
            f"{pressure['inputs']['base_capacity']:.1f}",
            f"{pressure['inputs']['factors'][factor_keys[name]]:.1f}",
            f"{value:.1f}",
            "C",
        )
        for name, value in pressure["results"].items()
    ]
    history = _table(
        ["Revision", "Date", "What changed and why"],
        [(row["revision"], row["date"], row["change"]) for row in CHANGES],
        "Revision history. Issues of this module analysis.",
    )
    methods = _table(
        [
            "Route",
            "Public symbol",
            "Source locator",
            "Evaluation",
            "Basis / evidence needed",
        ],
        [
            (
                row["category"],
                row["symbol"],
                row["path"],
                row["evaluation"],
                f"{row['basis']}: {row['evidence_needed']}",
            )
            for row in payload["methods"]
        ],
        "Method inventory. Locators identify implementation evidence, not standards qualification.",
    )
    source_table = _table(
        ["Repository-relative source", "SHA-256"],
        sources.items(),
        "Source digests. Exact executed calculation, builder and dedicated evidence bytes.",
    )
    design_data = _table(
        ["Example", "Input", "Value", "Unit", "Source"],
        [
            ("Annulus", key, f"{value:g}", annulus["units"][key], "A")
            for key, value in annulus["inputs"].items()
        ]
        + [
            ("Free growth", key, f"{value:g}", growth["units"][key], "B")
            for key, value in growth["inputs"].items()
        ]
        + [
            (
                "Free growth",
                "temperature_change",
                f"{case['temperature_change']:+g}",
                "K",
                "B",
            )
            for case in growth["cases"]
        ]
        + [
            (
                "Factor arithmetic",
                "base_capacity",
                f"{pressure['inputs']['base_capacity']:g}",
                "MPa",
                "C",
            )
        ]
        + [
            ("Factor arithmetic", key, f"{value:.1f}", "1", "C")
            for key, value in pressure["inputs"]["factors"].items()
        ],
        "Design data. Arbitrary synthetic inputs; no production or material property is supplied.",
    )
    geometry = _table(
        ["Dimension route", "Returned field", "Result", "Unit", "Basis"],
        geometry_rows,
        "Geometry results. Each available-dimension entry point is evaluated against the annulus integrals.",
    )
    thermal = _table(
        ["Temperature change [K]", "Uniform strain [1]", "Signed growth [m]", "Basis"],
        growth_rows,
        "Free-growth results. Positive values denote heating; negative values denote cooling.",
    )
    products = _table(
        [
            "Output",
            "Supplied capacity [MPa]",
            "Supplied factor [1]",
            "Product [MPa]",
            "Basis",
        ],
        pressure_rows,
        "Pressure-factor results. ARITHMETIC_ONLY; no design allowable is established.",
    )
    payload_json = _json(payload).replace("<", "\\u003c")
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Subsea pipeline module analysis</title>
<style>
body {{font: 1rem/1.6 system-ui,sans-serif;color:#203444;background:#f2f5f7;margin:0}}
main {{max-width:86rem;margin:auto;background:white;padding:2rem}}
h1,h2,h3 {{color:#173f52}} h2 {{border-top:2px solid #d6e3e8;padding-top:1rem}}
table {{border-collapse:collapse;width:100%;font-size:.9rem;display:block;overflow:auto}}
td,th {{padding:.5rem .7rem;border:1px solid #d6e3e8;text-align:left;vertical-align:top;overflow-wrap:anywhere}}
th {{background:#e8f0f3}} .caption {{font-size:.85rem;color:#425c69;margin-top:.3rem}}
aside {{border-left:4px solid #467b82;background:#edf5f5;padding:1rem}}
code {{overflow-wrap:anywhere}} a {{color:#175f72}} nav a {{margin-right:1rem}}
@media print {{main {{padding:0}} body {{background:white}} table {{font-size:.75rem}}}}
</style></head><body><main>
<h1>Subsea pipeline module analysis</h1>
<nav><a href="#s1">Summary</a><a href="#s4">Methods</a><a href="#s5">Worked results</a><a href="#s8">Way forward</a><a href="#s9">Sources</a></nav>
<h2 id="s1">1. Executive Summary</h2>
<p>The synthetic annulus, signed free thermal growth and caller-supplied pressure-factor products are checked against independent hand calculations. Geometry and growth are HAND_CALCULATION_CHECKED for these cases; pressure multiplication is ARITHMETIC_ONLY. Buckling, span acceptance and standard-derived pressure capacities remain NOT_EVALUATED. No engineering design is released by this report.</p>
<h3>Revision history</h3>
{history}
<aside id="change-panel"><strong>Change panel</strong><p>{html.escape(CHANGES[-1]["change"])}</p><a href="changes.json">Machine-readable revision history</a></aside>
<h2 id="s2">2. Introduction</h2>
<p>The scope is the public Python functions and methods declared in <code>src/digitalmodel/subsea/pipeline/</code>. Each is located by static source parsing; private helpers, generated dataclass methods and imported aliases are outside the declaration inventory. The executed examples use only geometry, unrestrained thermal expansion and factor multiplication. Other workflows are inspected without execution.</p>
<p>The inputs are deliberately synthetic and separately defined for each calculation. The annulus is not a pressure-design example. No material grade, soil, loading spectrum or operating acceptance criterion is inferred from these arithmetic inputs.</p>
<h2 id="s3">3. Design Data and Basis</h2>
{design_data}
<p>Length, area and second moment use metres, square metres and fourth-power metres. Temperature differences use kelvin and the supplied coefficient uses reciprocal kelvin. Pressure-factor products retain the supplied MPa unit; the public function itself performs no unit conversion.</p>
<h2 id="s4">4. Analysis Methodology</h2>
<p><strong>A — independent annulus integration.</strong> In polar coordinates, dA = r dr dθ. Integration over radii from one to two metres and a complete revolution gives wall area 3π m². The bore and outside disks have areas π and 4π m². With y = r sin θ, integration of y² dA gives the centroidal second moment 15π/4 m⁴. Radial separation gives thickness one metre. These identities supply the independent test oracles.</p>
<p><strong>B — independent strain integration.</strong> Uniform strain ε = α ΔT is integrated over the reference length: ΔL = ∫ ε dx. The supplied coefficient and heating/cooling changes give strains +1/5000 and −1/5000 and growth +1/5 m and −1/5 m. Reported strains are recovered from public-function growth divided by reference length.</p>
<p><strong>C — independent supplied arithmetic.</strong> The arbitrary capacity is multiplied separately by each arbitrary caller factor. Exact rational multiplication supplies the independent expected pressures. A computed burst pressure is never used as an oracle. The input parameter name <code>burst_pressure</code> in the existing API does not qualify the synthetic capacity supplied here.</p>
<p>The hand-value tests use relative tolerance 1e−12 for binary floating-point rounding only. That computational tolerance is not an engineering acceptance criterion. Source comments, tests of proportionality and implementation self-comparisons do not establish a standard equation or empirical factor.</p>
{methods}
<h2 id="s5">5. Detailed Results</h2>
{geometry}
<p>The direct section, thickness, diameter-reconstruction and geometry-consistency entry points are also exercised. Their complete returned values are retained in the embedded calculation payload and asserted against basis A by the report tests.</p>
{thermal}
{products}
<p>HAND_CALCULATION_CHECKED means agreement with the independent synthetic integral in the stated case. ARITHMETIC_ONLY means agreement with supplied multiplication. NOT_EVALUATED means the method is not qualified by this report; it does not mean a true zero or a passing design check.</p>
<h2 id="s6">6. Key Findings</h2>
<p>The available-dimension geometry routes recover the declared annulus. Heating and cooling retain their signed free-growth response. The factor API returns the <code>incidentalPressure</code> product under the legacy key <code>accidental</code>. Incidental and accidental are distinct load categories; the label mismatch does not change the multiplication. Normal public imports now resolve the local calculation modules.</p>
<p>The inventory does not transfer these results to engineering restraint, free-span or pressure-capacity methods. In particular, free thermal growth does not predict buckling or walking, and factor multiplication does not qualify its supplied capacity.</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>Buckling restraint, axial feed-in, imperfection response, soil friction, upheaval resistance and mode constants require separate independent cases. The legacy thermal-buckling route references helpers absent from its class; the legacy upheaval route contains an unfinished eigenvalue branch. Those workflows remain NOT_EVALUATED and are not executed or repaired here.</p>
<p>Span frequency boundary, sag and seabed-proximity corrections, onset thresholds, VIV response, fatigue curves and allowable lengths need independent equation and factor evidence. Passing existing span regressions does not establish these sources. Collapse, burst, combined loading, derating and pressure-sizing routes require equation-specific and edition-specific evidence plus qualified material and corrosion inputs. No licensed solver is imported or run.</p>
<p>The <code>incidentalPressure</code> product is returned as <code>accidental</code>; incidental and accidental are distinct load categories. The legacy API key is retained for compatibility, and its meaning must be resolved before engineering use.</p>
<p>Geometry validation is checked for the consistent example, not certified for every invalid input. Composite geometry, reliability, installation and workflow declarations are inventoried without qualification. This report supplies neither a production design nor approval for service.</p>
<h2 id="s8">8. Recommendations</h2>
<p>The way forward is to obtain edition-specific equations and applicability limits for each engineering method, then independently worked cases for restraint, lateral and upheaval response, span frequency and empirical corrections, and pressure capacities. Material properties, corrosion basis, soil inputs, boundary conditions and acceptance criteria shall be supplied before a design conclusion can be calculated.</p>
<p>Each unqualified route shall retain NOT_EVALUATED until its own evidence is reviewed. The tracking issue remains open while the requested engineering methods are unqualified. The present arithmetic examples can be rebuilt with the command in the accompanying README.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>Bases A, B and C are the independent calculations written in this report and in <code>tests/subsea/pipeline/test_module_report_hand_values.py</code> and <code>tests/reporting/test_subsea_pipeline_module_report.py</code>. The synthetic inputs are committed in the builder source. Implementation locators identify declarations by repository-relative path and symbol, not by mutable line numbers or external equation evidence. The inventory describes the sources available at generation; added or removed declarations need a deliberate rebuild. Edits to unevaluated bodies and source line shifts do not invalidate the delivered report. No standard-derived numeric capacity or allowable is reproduced as qualified evidence.</p>
{source_table}
<p><a href="manifest.json">The digest manifest</a> binds source files and the other delivered artifacts. Its own digest is excluded to avoid self-reference; independent rebuilds compare the complete manifest bytes. Absolute paths, wall-clock build times and scratch locations are excluded from the output. Embedded JSON retains full calculation precision; visible result tables are rounded for reading.</p>
<script id="calculation" type="application/json">{payload_json}</script>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p><strong>Layout choice.</strong> The ten headings follow the existing module-analysis structure. A compact self-contained stylesheet is retained for the wide declaration inventory and arithmetic tables, rather than the assessment template designed for a site-specific operating decision. This presentation choice adds no engineering qualification.</p>
<p>OD: outside diameter; ID: inside diameter; VIV: vortex-induced vibration; MPa: megapascal; K: kelvin; SHA-256: artifact digest algorithm. The method inventory is a bounded source review and arithmetic assessment. Independent engineering qualification of the unevaluated methods remains pending.</p>
</main></body></html>
"""


def _readme():
    return """# Subsea pipeline module analysis

Read [the report](index.html), [revision history](changes.json) and
[source/artifact digests](manifest.json).

The worked examples are a synthetic annulus, signed unrestrained thermal growth
and multiplication of a supplied capacity by supplied factors. The independent
hand calculations are written in the report and its two dedicated test files.
They do not qualify buckling, span factors or pressure capacities; these remain
`NOT_EVALUATED` pending independent engineering evidence.

Rebuild from the repository root in the existing scientific Python environment.

```bash
PYTHONPATH=src python scripts/reports/build_subsea_pipeline_module_report.py
```

Use `--output-dir "$REPORT_OUTPUT"` for a separate destination. The builder writes
only `index.html`, `changes.json`, `manifest.json` and `README.md`. Inside the
checkout, only `docs/reports/modules/subsea_pipeline` is permitted. Output aliases
are checked by their resolved destination. Each deliverable target must not be a
symbolic link, directory junction or other name-surrogate reparse point, including
WSL symbolic links. Cloud placeholder reparse tags are permitted
for recognised report deliverables. Outside the canonical report directory, an
existing output directory must be empty or contain
only the four deliverable names as regular files, including a `manifest.json`
object with `"algorithm": "sha256"`. Unrelated files and unrecognised reports are
preserved by rejecting the destination before calculation or writing. No intermediate
calculation files are needed. `--scratch` is an optional reserved compatibility
argument; it writes nothing and, when supplied, must resolve outside the checkout.
The manifest hashes the three executed pipeline modules, unit support, the builder
and two dedicated test files, plus the other three deliverables. Its own digest is
excluded to avoid self-reference; compare all four output files byte for byte
between independent builds, including `manifest.json`.

The public-method inventory lists repository-relative paths and symbols without
line numbers, with explicit case-limited qualification status. It describes the
source declarations at generation, without a hard digest requirement on unevaluated
modules. Added or removed declarations need a deliberate rebuild; unrelated body
edits and line shifts do not invalidate delivered bytes.
Adding, removing or renaming a public declaration makes
`tests/reporting/test_subsea_pipeline_module_report.py::test_rebuild_is_deterministic`
fail because the rebuilt inventory differs from the committed HTML. This coupling
is retained deliberately; run the rebuild command above after such a change.

Layout choice: the ten headings follow the existing module-analysis structure.
The compact self-contained stylesheet supports the wide declaration inventory
and arithmetic tables; the assessment template addresses site-specific operating
decisions instead. This presentation choice adds no engineering qualification.

Input ownership resides in this repository's builder; these are synthetic arithmetic inputs, not measured data,
material properties or code allowables. The issue remains open for the
unevaluated engineering methods.
"""


def _is_link(path):
    """Recognize symbolic links and junctions without rejecting cloud files."""
    try:
        metadata = path.lstat()
    except FileNotFoundError:
        return False
    # The Windows name-surrogate bit includes junctions and WSL symbolic links.
    return stat.S_ISLNK(metadata.st_mode) or bool(
        getattr(metadata, "st_reparse_tag", 0) & 0x20000000
    )


def _validate_output_directory(output_dir):
    """Protect checkout paths, linked targets and unrelated external files."""
    output_dir = Path(output_dir).resolve()
    if (
        output_dir.is_relative_to(ROOT.resolve())
        and output_dir != DESTINATION.resolve()
    ):
        raise ValueError("Checkout output must be the module report directory")
    deliverables = {"index.html", "changes.json", "manifest.json", "README.md"}
    for name in sorted(deliverables):
        if _is_link(output_dir / name):
            raise ValueError(
                f"Report output target must not be a link or junction: {name}"
            )
    if output_dir.exists():
        if not output_dir.is_dir():
            raise ValueError("Report output must be a directory")
        if output_dir != DESTINATION.resolve():
            entries = list(output_dir.iterdir())
            if entries:
                if any(
                    entry.name not in deliverables or not entry.is_file()
                    for entry in entries
                ):
                    raise ValueError("Report output contains unrelated files")
                try:
                    manifest = json.loads(
                        (output_dir / "manifest.json").read_text(encoding="utf-8")
                    )
                except (OSError, UnicodeError, json.JSONDecodeError) as error:
                    raise ValueError("Report output needs a recognised manifest") from error
                if not isinstance(manifest, dict) or manifest.get("algorithm") != "sha256":
                    raise ValueError("Report output needs a sha256 manifest")
    return output_dir


def build_report(*, scratch=None, output_dir=DESTINATION):
    """Write only the declared deliverables, with no intermediate checkout files."""
    if scratch is not None:
        scratch = Path(scratch).resolve()
        if scratch.is_relative_to(ROOT.resolve()):
            raise ValueError("Calculation scratch must be outside the checkout")
    output_dir = _validate_output_directory(output_dir)
    payload = build_payload()
    sources = source_digests()
    files = {
        "index.html": render(payload, sources),
        "changes.json": _json(CHANGES),
        "README.md": _readme(),
    }
    manifest = {
        "algorithm": "sha256",
        "sources": sources,
        "artifacts": {
            name: hashlib.sha256(content.encode("utf-8")).hexdigest()
            for name, content in sorted(files.items())
        },
        "self_digest": "Excluded to avoid a self-referential digest; compare manifest.json bytes externally.",
    }
    files["manifest.json"] = _json(manifest)
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, content in files.items():
        (output_dir / name).write_text(content, encoding="utf-8", newline="\n")
    return manifest


def main():
    """Build the report, retaining an optional reserved scratch argument."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--scratch",
        type=Path,
        help="Reserved compatibility argument; no files are written there. Must be outside the checkout.",
    )
    parser.add_argument("--output-dir", default=DESTINATION, type=Path)
    args = parser.parse_args()
    build_report(scratch=args.scratch, output_dir=args.output_dir)


if __name__ == "__main__":
    main()
