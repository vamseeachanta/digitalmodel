# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build bounded pipe arithmetic evidence using normal public package imports.

Only final deliverables enter the output directory. Scratch must be external.
The worked document supplies every engineering input; no density is selected
from a physical material default. Figures are generated inline from live
geometry. Exact-rational numerical oracles live separately in the tests.
"""

import argparse
import ast
import hashlib
import html
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/structural/pipe_cross_section"
EXAMPLE = f"{PACKAGE}/docs/worked-layered-pipe-buoyancy.md"
REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
CHANGES = [{
    "revision": "A",
    "date": "2026-10-28",
    "change": "Added a reproducible module analysis and independent annular-integral checks to bound the synthetic example and its limits.",
}]
SOURCES = [
    f"{PACKAGE}/{name}.py" for name in ("__init__", "models", "calculator", "visualization", "cli")
] + [
    "src/digitalmodel/units.py", EXAMPLE,
    "tests/structural/pipe_cross_section/test_pipe_calculator.py",
    "tests/structural/pipe_cross_section/test_documented_layered_pipe_buoyancy.py",
    "tests/reporting/test_pipe_cross_section_module_report.py",
    "scripts/reports/build_pipe_cross_section_module_report.py",
    "docs/api/_assets/brand.css",
]
# Public symbol inventory reviewed at REVISION. Each row identifies the input
# and output contract; listing an API does not qualify its physical behavior.
API_GROUPS = [
    ("models", "CoatingType InternalContents", "Enumeration members", "Material and contents labels; classification only."),
    ("models", "inch_to_mm mm_to_inch", "Length in inches or mm", "Converted length through Pint; exact international inch definition."),
    ("models", "PipeLayer", "Name, inner/outer diameters (mm), explicit density (kg/m³), optional display color/type", "A homogeneous annulus; geometry is supplied directly."),
    ("models", "PipeLayer.thickness_mm PipeLayer.inner_radius_m PipeLayer.outer_radius_m PipeLayer.cross_sectional_area_m2 PipeLayer.weight_per_meter_kg PipeLayer.weight_per_meter_kN", "Layer dimensions and density", "Radial thickness, SI radii, annular area, mass/length and converted force/length."),
    ("models", "PipeLayer.to_dict", "Layer state", "Rounded display dictionary; Area_m2_m means m², Weight_kg_m means kg/m."),
    ("models", "PipeCrossSectionConfig PipeCrossSectionConfig.from_inches PipeCrossSectionConfig.from_dict PipeCrossSectionConfig.to_dict", "Metric/imperial dimensions or dictionary; optional pipe_cross_section wrapper and legacy dimension keys", "Configuration object or dictionary; imperial steel/concrete lengths convert, lpp thickness stays in mm."),
    ("models", "BuoyancyResult BuoyancyResult.to_dict", "Displacement, mass equivalents, converted forces and sign flag", "Structured result and rounded dictionary; no independent acceptance decision."),
    ("calculator", "PipeCrossSection PipeCrossSection.from_config PipeCrossSection.from_dict PipeCrossSection.from_inches", "Steel dimensions, optional coating thicknesses and explicit densities; configuration/dictionary/imperial alternatives", "Concentric steel, then optional lpp and concrete annuli, in that order."),
    ("calculator", "PipeCrossSection.steel_layer PipeCrossSection.inner_diameter_mm PipeCrossSection.outer_diameter_mm PipeCrossSection.outer_diameter_inch", "Constructed layer list", "Steel annulus and bore/exterior dimensions; inches use Pint conversion."),
    ("calculator", "PipeCrossSection.total_weight_in_air_kg_m PipeCrossSection.total_weight_in_air_kN_m PipeCrossSection.displaced_volume_m3_m PipeCrossSection.buoyancy_force_kg_m PipeCrossSection.buoyancy_force_kN_m PipeCrossSection.submerged_weight_kg_m PipeCrossSection.submerged_weight_kN_m PipeCrossSection.is_buoyant", "Layer areas/densities and external water density", "Layer mass sum, full exterior disk displacement, water mass, their difference, converted forces and sign flag."),
    ("calculator", "PipeCrossSection.get_buoyancy_result PipeCrossSection.get_summary PipeCrossSection.to_dict PipeCrossSection.to_csv_data PipeCrossSection.print_summary", "Constructed pipe", "Structured buoyancy, rounded summaries/dictionaries/CSV-ready rows or console text. Dry mass excludes bore contents."),
    ("visualization", "create_circle_points", "Radius, center and sample count", "Sampled circle coordinates for drawing; no new section-property calculation."),
    ("visualization", "PipeCrossSectionVisualizer PipeCrossSectionVisualizer.create_cross_section_figure PipeCrossSectionVisualizer.create_weight_distribution_figure PipeCrossSectionVisualizer.create_plotly_figure PipeCrossSectionVisualizer.get_plots_html", "Pipe object", "Optional Plotly figures/HTML. Rendering dependencies and legacy output claims are NOT_EVALUATED here."),
    ("visualization", "PipeCrossSectionVisualizer.generate_html_report PipeCrossSectionVisualizer.export_static_image PipeCrossSectionVisualizer.export_csv generate_html_report export_static_image export_csv", "Pipe and destination path", "HTML, static image or CSV file; optional plotting dependencies. Report builder uses its own inline SVG instead."),
    ("cli", "parse_args load_config_from_yaml main", "CLI arguments or YAML path", "Parsed arguments, configuration dictionary, or exported files. Root cli_main aliases main; CLI execution is NOT_EVALUATED."),
]


def json_text(value):
    """Serialize deterministic evidence with finite numeric values only."""
    return json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n"


def escaped(value):
    """Escape text for HTML text and attribute contexts."""
    return html.escape(str(value), quote=True)


def calculate(scratch):
    """Execute the documented synthetic inputs through public scalar APIs."""
    from digitalmodel.structural.pipe_cross_section import PipeCrossSection

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    inputs, cases = {}, {}
    # Inputs and Derivation in EXAMPLE are the basis for all these constants.
    for name, coating in (("bare", 0.0), ("coated", 5.25)):
        inputs[name] = {
            "steel_od_mm": 200.25, "steel_wt_mm": 10.125, "steel_density": 7800.25,
            "lpp_thickness_mm": coating, "lpp_density": 1200.75,
            "concrete_thickness_mm": 0.0, "seawater_density": 1020.5,
            "internal_contents": "air", "internal_fluid_density": 0.0,
        }
        pipe = PipeCrossSection(**inputs[name])
        cases[name] = {
            "inner_diameter_mm": pipe.inner_diameter_mm,
            "outer_diameter_mm": pipe.outer_diameter_mm,
            "outer_diameter_inch": pipe.outer_diameter_inch,
            "mass_in_air_kg_m": pipe.total_weight_in_air_kg_m,
            "displaced_area_m2": pipe.displaced_volume_m3_m,
            "displaced_water_mass_kg_m": pipe.buoyancy_force_kg_m,
            "submerged_mass_kg_m": pipe.submerged_weight_kg_m,
            "air_force_kN_m": pipe.total_weight_in_air_kN_m,
            "buoyancy_force_kN_m": pipe.buoyancy_force_kN_m,
            "submerged_force_kN_m": pipe.submerged_weight_kN_m,
            "is_buoyant": pipe.is_buoyant,
            "layers": [{
                "inner_diameter_mm": layer.inner_diameter_mm,
                "outer_diameter_mm": layer.outer_diameter_mm,
                "thickness_mm": layer.thickness_mm,
                "density_kg_m3": layer.density_kg_m3,
                "area_m2": layer.cross_sectional_area_m2,
                "mass_kg_m": layer.weight_per_meter_kg,
            } for layer in pipe.layers],
        }
    data = {"source_revision": REVISION, "input_basis": EXAMPLE, "inputs": inputs, "cases": cases}
    (scratch / "public-api-results.json").write_text(json_text(data), encoding="utf-8", newline="\n")
    return data


def inventory():
    """Return grouped contracts and exact public source line locations."""
    rows = []
    for module, symbols, inputs, outputs in API_GROUPS:
        path = f"{PACKAGE}/{module}.py"
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        locations = {}
        for node in tree.body:
            if isinstance(node, (ast.ClassDef, ast.FunctionDef)) and not node.name.startswith("_"):
                locations[node.name] = node.lineno
                if isinstance(node, ast.ClassDef):
                    for method in node.body:
                        if isinstance(method, ast.FunctionDef) and not method.name.startswith("_"):
                            locations[f"{node.name}.{method.name}"] = method.lineno
        rows.append({"source": path, "symbols": {name: locations[name] for name in symbols.split()},
                     "inputs": inputs, "outputs": outputs})
    return rows


def source_link(relative, label=None, line=None):
    """Link a report to repository-local evidence without remote resources."""
    target = ("../../../../" + relative) + (f"#L{line}" if line else "")
    return f'<a href="{escaped(target)}">{escaped(label or relative)}</a>'


def table(headers, rows):
    """Render a table with already escaped cell contents."""
    return '<div class="table-wrap"><table><thead><tr>' + ''.join(
        f'<th>{escaped(header)}</th>' for header in headers
    ) + '</tr></thead><tbody>' + ''.join(
        '<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows
    ) + '</tbody></table></div>'


def figure(data):
    """Draw concentric geometry from public diameters; drawing units are layout."""
    drawings = []
    for center, (case, values) in zip((160, 480), data["cases"].items(), strict=True):
        # Common view scale; pixel constants below are presentation only.
        scale = 200 / max(item["outer_diameter_mm"] for item in data["cases"].values())
        circles = []
        for layer in reversed(values["layers"]):
            radius = layer["outer_diameter_mm"] * scale / 2
            circles.append(f'<circle cx="{center}" cy="125" r="{radius:.3f}" class="material"/>')
        bore = values["inner_diameter_mm"] * scale / 2
        circles.append(f'<circle cx="{center}" cy="125" r="{bore:.3f}" class="bore"/>')
        drawings.append(''.join(circles) + f'<text x="{center}" y="255" text-anchor="middle">{escaped(case)}: OD {values["outer_diameter_mm"]:g} mm [E, C]</text>')
    return '<svg viewBox="0 0 640 280" role="img" aria-label="Bare and coated concentric pipe sections">' + ''.join(drawings) + '</svg>'


def render(data):
    """Render the ten-section analysis; every result comes from calculate."""
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    brand = "\n".join(line.rstrip() for line in brand.splitlines())
    history = table(("Revision", "Date", "What changed and why"), [
        [escaped(item[key]) for key in ("revision", "date", "change")] for item in CHANGES
    ])
    inventory_table = table(("Reviewed public symbols", "Inputs", "Outputs and limits"), [
        ['<br>'.join(source_link(row["source"], name, line) for name, line in row["symbols"].items()),
         escaped(row["inputs"]), escaped(row["outputs"])] for row in inventory()
    ])
    input_units = {
        "steel_od_mm": "mm", "steel_wt_mm": "mm", "steel_density": "kg/m³",
        "lpp_thickness_mm": "mm", "lpp_density": "kg/m³", "concrete_thickness_mm": "mm",
        "seawater_density": "kg/m³", "internal_contents": "label", "internal_fluid_density": "kg/m³",
    }
    inputs = table(("Input", "Bare", "Coated", "Unit", "Basis"), [
        [f'<code>{escaped(key)}</code>', escaped(value), escaped(data["inputs"]["coated"][key]),
         input_units[key], '[E] synthetic assumption'] for key, value in data["inputs"]["bare"].items()
    ])
    layers = table(("Case / layer", "Inner diameter (mm)", "Outer diameter (mm)", "Radial thickness (mm)",
                    "Density (kg/m³)", "Area (m²)", "Mass/length (kg/m)", "Basis"), [
        [escaped(case + " / " + ("steel" if index == 0 else "coating")),
         f'{layer["inner_diameter_mm"]:g}', f'{layer["outer_diameter_mm"]:g}',
         f'{layer["thickness_mm"]:g}', f'{layer["density_kg_m3"]:g}',
         f'{layer["area_m2"]:.4e}', f'{layer["mass_kg_m"]:.1f}', '[E, M] A = π(R² − r²); m′ = ρA']
        for case, values in data["cases"].items() for index, layer in enumerate(values["layers"])
    ])
    descriptors = [
        ("inner_diameter_mm", "Bore diameter", "mm", "g", "[E, C] steel OD − twice wall thickness"),
        ("outer_diameter_mm", "Exterior diameter", "mm", "g", "[E, C] steel OD + twice coating thickness"),
        ("outer_diameter_inch", "Exterior diameter", "inch", ".4f", "[C, U] exterior mm / 25.4"),
        ("displaced_area_m2", "Displaced volume per length", "m³/m (= m²)", ".4e", "[E, C] πR_outer²"),
        ("mass_in_air_kg_m", "Material mass per length", "kg/m", ".1f", "[E, C] ΣρⱼAⱼ"),
        ("displaced_water_mass_kg_m", "Displaced water mass per length", "kg/m", ".1f", "[E, C] ρ_water A_displaced"),
        ("submerged_mass_kg_m", "Submerged mass equivalent per length", "kg/m", ".1f", "[E, C] material mass − displaced water mass"),
    ]
    results = table(("Quantity", "Bare", "Coated", "Unit", "Basis"), [
        [label, *(format(data["cases"][case][key], spec) for case in ("bare", "coated")), unit, basis]
        for key, label, unit, spec, basis in descriptors
    ])
    forces = table(("Implementation force conversion", "Bare (kN/m)", "Coated (kN/m)", "Basis"), [
        [label, *(f'{data["cases"][case][key]:.3f}' for case in ("bare", "coated")),
         '[C, M] corresponding kg/m × 9.81 / 1000']
        for key, label in (("air_force_kN_m", "Material gravitational force per length"),
                           ("buoyancy_force_kN_m", "Buoyant force per length"),
                           ("submerged_force_kN_m", "Submerged gravitational force equivalent per length"))
    ])
    payload = json_text(data).replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
    refs = [
        ("E", EXAMPLE, "Synthetic inputs, sealed-bore assumptions, exact polar integrals, executable example and numerical retention"),
        ("M", f"{PACKAGE}/models.py", "PipeLayer annular area, density multiplication, dimensional conversion and display rounding"),
        ("C", f"{PACKAGE}/calculator.py", "Layer construction, public geometry and mass properties, displacement and force-conversion convention"),
        ("U", "src/digitalmodel/units.py", "Pint-backed unit conversion; international inch is exactly 25.4 mm"),
        ("V", f"{PACKAGE}/visualization.py", "Reviewed visualization/export interfaces; not used for the generated diagram"),
        ("L", f"{PACKAGE}/cli.py", "Reviewed command-line and configuration interfaces"),
        ("T", "tests/reporting/test_pipe_cross_section_module_report.py", "Independent rational-radius oracle, executable replay, byte rebuilds and negative controls"),
        ("B", "scripts/reports/build_pipe_cross_section_module_report.py", "Deterministic builder, inline diagram and delivery manifest"),
    ]
    references = '<ul>' + ''.join(f'<li id="ref-{key}">[{key}] {source_link(path)}: {escaped(note)}.</li>' for key, path, note in refs) + '</ul>'
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Pipe cross-section module analysis</title>
<style>{brand}
.wrap{{max-width:1120px;margin:auto;padding:2rem}}header,section,.history{{background:var(--panel);border:1px solid var(--edge);border-radius:var(--r);padding:1.4rem;margin-bottom:1rem}}
h1,h2,h3{{color:var(--brand)}}.table-wrap{{overflow-x:auto}}table{{width:100%;border-collapse:collapse;font-variant-numeric:tabular-nums}}th,td{{text-align:left;padding:.6rem;border-bottom:1px solid var(--edge);vertical-align:top}}th{{background:var(--panel-2)}}code{{overflow-wrap:anywhere}}a{{color:var(--brand)}}.eq,pre{{padding:1rem;background:var(--panel-2);overflow:auto}}svg{{width:100%;max-width:780px}}.material{{fill:var(--muted);stroke:var(--brand);stroke-width:2}}.bore{{fill:var(--panel);stroke:var(--brand);stroke-width:2}}svg text{{fill:var(--ink);font-size:15px}}.notice{{border-left:4px solid var(--caution);padding-left:1rem}}@media print{{.wrap{{padding:0}}section{{break-inside:avoid}}}}
</style></head><body class="hs-doc"><main class="wrap">
<header><h1>Pipe cross-section module analysis</h1><p>Concentric annular geometry, layer mass and sealed-bore displacement.</p>
<p class="notice">Synthetic calculation evidence. Physical qualification and installation acceptance: NOT_EVALUATED.</p>
<nav><a href="#s3">Input basis</a> · <a href="#s5">Worked calculation</a> · <a href="#s7">Results</a> · <a href="#s8">Limits</a> · <a href="results.json">Unrounded evidence</a> · <a href="manifest.json">Byte manifest</a></nav></header>
<section><h2 id="s1">1. Summary</h2>
<p>Normal public APIs evaluate the existing bare and coated synthetic example [E, C]. The submerged mass equivalents are
<strong>{data["cases"]["bare"]["submerged_mass_kg_m"]:.1f} kg/m</strong> and <strong>{data["cases"]["coated"]["submerged_mass_kg_m"]:.1f} kg/m</strong>, respectively, rounded only for presentation.
Independent polar integrals check the unrounded quantities [T]. These results describe fully immersed, uniform sections with a sealed dry bore and omitted closure/end masses; they do not select a design or establish stability.</p></section>
<div class="history"><h3>Revision history</h3>{history}<p>Revision and date identify this document issue; <a href="changes.json">change history</a> carries the same row.</p></div>
<section><h2 id="s2">2. Context and scope</h2>
<p>The reviewed calculation source revision is <code>{REVISION}</code>. Current source bytes are bound separately in the manifest. Review covers public constructors, properties, configuration, unit conversion, serialization, visualization and CLI interfaces. Numerical evidence covers only the stated scalar synthetic cases; no solver is used.</p>
<p>The root package exports the model classes, enums, conversions, calculator, visualizer, export helpers and <code>cli_main</code> [M, C, V, L]. The inventory below includes public functions defined in their submodules as well. Private layer construction is reviewed through its public constructor [C].</p>{inventory_table}
<p>Enum coating labels include steel, polypropylene, polyethylene, epoxy, concrete, insulation and custom material. The standard constructor actually builds steel, optional <code>lpp_*</code>, then optional <code>concrete_*</code> layers. The example uses the lpp interface solely for a synthetic concentric coating; a label is no material qualification [M, C].</p></section>
<section><h2 id="s3">3. Input basis</h2>
<p>All calculation inputs below are explicit synthetic assumptions copied from [E], not physical material defaults. Zero coating or concrete thickness omits that annulus. Concrete density is inactive because no concrete annulus is constructed; its physical default remains NOT_EVALUATED.</p>{inputs}
<figure>{figure(data)}<figcaption>Builder-generated sections at a common drawing scale, using returned public diameters [E, C]. Pixel coordinates are layout quantities, not additional engineering inputs.</figcaption></figure>
<p>Dimensions enter in millimetres, radial thickness differs from diameter increment, and milli denotes a thousandth [E]. The exact metre radii are bore 9/100, steel exterior 801/8000 and coated exterior 843/8000 [E]. The <code>from_inches</code> routes convert steel and concrete lengths through Pint; lpp thickness remains in millimetres [M, C, U].</p></section>
<section><h2 id="s4">4. Method</h2>
<p>For a homogeneous annulus, integrating the polar area element s ds dθ gives the following chain [E]. Factors of two follow from diameter being twice radius; the radial square follows from integration, not fitting.</p>
<div class="eq">A(r,R) = ∫₀²π ∫ᵣᴿ s ds dθ = π(R² − r²); m′ = ρA; m′_air = ΣⱼρⱼAⱼ.<br>
A_displaced = ∫₀²π ∫₀ᴿ s ds dθ = πR_outer²; m′_water = ρ_water A_displaced;<br>
m′_sub = m′_air − m′_water.</div>
<p>Area is in m²; volume per axial length is in m³/m and has the same numerical value. Density in kg/m³ times area gives kg/m. Subtracting displaced water mass is the mass-equivalent form of the buoyancy principle explained in [E]. A positive result means material mass exceeds displaced water mass under this basis, not that installation is acceptable.</p>
<p>The full exterior disk includes the sealed void. Internal contents and internal fluid density are stored inputs but do not enter the current material layer sum or change displacement [C]. A flooded bore requires a separately reviewed mass/displacement convention. Flooding behavior is NOT_EVALUATED; changing a label does not model it.</p>
<p>Legacy names containing <code>weight</code> or <code>force</code> with kg/m units mean mass per length or mass equivalent. Force per length is separately reported in kN/m using the existing implementation factor 9.81 m/s² and 1000 N/kN [M, C]. This audits that conversion only; local gravity selection is NOT_EVALUATED.</p></section>
<section><h2 id="s5">5. Worked calculation</h2>
<p>The material area and mass columns come directly from <code>PipeLayer</code> properties [M]. Public pipe properties supply the results in the next sections [C]. The independent exact areas are steel π × 123201/64000000, coating π × 8631/8000000, bare displacement π × 641601/64000000 and coated displacement π × 710649/64000000 m²; each follows from the rational radii and integrals in [E], not a calculator summary.</p>{layers}
<p>For the bare case sum only steel mass; for the coated case sum steel and coating mass. Multiply the exterior disk area by the assumed water density, then subtract before rounding [E, C]. Coating changes both material mass and exterior displacement; neither change alone is the submerged answer.</p></section>
<section><h2 id="s6">6. Checks</h2>
<p>[T] derives areas from exact rational radii and synthetic densities independently of production summaries. Live area checks use relative tolerance 1e-12 and absolute floor 1e-15 m²; mass checks use relative tolerance 1e-12 and floor 1e-10 kg/m, matching the executable example [E]. These allowances cover binary roundoff in this short arithmetic chain and are far below presentation increments; they are not engineering acceptance tolerances. Force tolerance uses the mass floor multiplied by the implementation conversion [C, T].</p>
<p>The executable document and a separate public-API replay check freshness. Two fresh external builds must match each other and all delivered bytes. The independent manifest readback binds the delivered files and source evidence. Negative controls change a result magnitude or sign; independent arithmetic must reject both even when HTML, JSON, builder and hashes agree with the changed value [T].</p>
<p>Areas are displayed in scientific notation, masses to one decimal place and force equivalents to three decimal places. Inputs retain their stated precision. Unrounded JSON drives assertions; the rounded document table is never a numerical oracle [E, T]. The evidence is available in <a href="results.json">results.json</a> and <a href="manifest.json">manifest.json</a>.</p></section>
<section><h2 id="s7">7. Results</h2>{results}
<p>Units distinguish mass equivalents from forces. The sign indicator <code>is_buoyant</code> is an implementation comparison with zero [C]; its physical interpretation and any stability decision are NOT_EVALUATED.</p>{forces}
<p>The force conversion uses the same live mass quantities, not the rounded kg/m entries. Existing <code>get_summary</code>, <code>to_dict</code>, <code>BuoyancyResult.to_dict</code> and CSV rows round derived quantities; they are display exports rather than unrounded numerical evidence [M, C, E]. JSON export through the CLI serializes <code>to_dict</code>. This report retains public scalar precision separately.</p></section>
<section><h2 id="s8">8. Limits / NOT_EVALUATED</h2>
<ul><li>Physical material densities/defaults, coating grades, porosity, absorption, dimensional tolerances, corrosion, eccentricity and noncircular sections: NOT_EVALUATED.</li>
<li>Sealed dry bore only. Closure/end masses and end effects are omitted. Internal-fluid mass and open/flooded-bore behavior: NOT_EVALUATED. The interface accepts contents fields without adding their mass, and <code>PipeCrossSection.to_dict</code> omits <code>internal_fluid_density</code> from its configuration export. These are open method/export findings beyond the present basis [C, E].</li>
<li>Internal/external pressure, stress capacity, stiffness or inertia qualification, buckling, code compliance, installation acceptance, seabed stability, hydrodynamics and operational limits: NOT_EVALUATED.</li>
<li>Legacy summary wording may imply floating/sinking or standards compatibility. Such claims are not adopted here. Invalid-input policy and alternative coating construction beyond the supported constructor: NOT_EVALUATED [C, M].</li>
<li>Optional Plotly/Matplotlib image and legacy HTML exports, CLI/YAML runtime behavior and physical gravity choice: reviewed interfaces only; execution/qualification NOT_EVALUATED. The delivered page requires no remote scripts or assets [V, L, B].</li></ul></section>
<section><h2 id="s9">9. Way forward</h2>
<p>Retain the synthetic cases as arithmetic regressions. Before physical use, obtain a traceable geometry/material/water basis, define bore sealing and flooding states with closure and contents masses, and have the governing mass/displacement convention reviewed. Establish acceptance criteria and validation evidence separately. Until then, unqualified parts of the wider module remain open; this example provides no release decision.</p></section>
<section><h2 id="s10">10. References and evidence</h2>{references}
<p>Bracketed identifiers identify local source or hand-derivation evidence for every numerical table and equation. Source code is evidence of implementation, not authority for a physical design basis. Revision letters, section numbers and the document date are editorial metadata. No licensed standard value is reproduced.</p>
<p>The manifest records source and delivered-byte digests without environment identities or generation times; it omits its own digest to avoid self-reference. The results payload retains unrounded public outputs and the exact synthetic inputs.</p>
<p><a href="README.md">Rebuild instructions</a> · <a href="changes.json">Revision data</a></p></section>
<script id="calculation" type="application/json">{payload}</script>
</main></body></html>
'''


def readme():
    """Generate portable instructions without environment-specific paths."""
    return '''# Pipe cross-section module analysis

The [self-contained report](index.html) evaluates the existing synthetic bare
and coated example through normal public APIs. [Results](results.json) retain
unrounded values; [history](changes.json) records revisions and the
[manifest](manifest.json) binds the delivered bytes and calculation sources.

## Rebuild

With source imports and the declared Pint dependency available, run from the
repository root. Supply existing or new external directories for scratch and
for a fresh delivery; replace both symbolic directory arguments.

```sh
PYTHONPATH=src python scripts/reports/build_pipe_cross_section_module_report.py --scratch EXTERNAL_SCRATCH --output-dir EXTERNAL_OUTPUT
```

The builder writes calculation evidence only to external scratch and final
files only to the explicit output directory. It uses no remote assets or
calculation service. The reviewed revision and exact source-byte digests are
recorded separately so source drift is visible.

## Evidence and limits

The basis is the [worked example](../../../../src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md).
The [report tests](../../../../tests/reporting/test_pipe_cross_section_module_report.py)
separate exact-rational polar integration from production replay, two fresh
byte-identical rebuilds, manifest readback and tampering controls.

The calculation assumes a fully immersed pipe with a sealed dry bore and
omits closure/end masses. Mass equivalents in kg/m differ from force per
length in kN/m. Physical densities/defaults, flooding behavior and installation
acceptance remain NOT_EVALUATED. Optional legacy plotting exports are not
exercised by this builder.
'''


def build(scratch, output_dir):
    """Calculate once, then bind the final deterministic delivery bytes."""
    scratch, output = Path(scratch).resolve(), Path(output_dir).resolve()
    if scratch == output or scratch in output.parents or output in scratch.parents:
        raise ValueError("Scratch and output directories must be disjoint")
    data = calculate(scratch)
    artifacts = {"index.html": render(data), "results.json": json_text(data),
                 "changes.json": json_text(CHANGES), "README.md": readme()}
    output.mkdir(parents=True, exist_ok=True)
    for name, content in artifacts.items():
        (output / name).write_text(content, encoding="utf-8", newline="\n")
    manifest = {
        "reviewed_source_revision": REVISION,
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in SOURCES},
        "artifacts": {name: hashlib.sha256((output / name).read_bytes()).hexdigest() for name in artifacts},
        "scope": "Synthetic sealed dry-bore arithmetic only; physical qualification NOT_EVALUATED.",
    }
    (output / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")
    return output


def main():
    """Require explicit external calculation scratch and delivery destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
