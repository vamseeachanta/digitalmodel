# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build a standalone cross-section analysis from production calculations.

Rebuild: PYTHONPATH=src python scripts/reporting/build_cross_sections_module_report.py
Use --output-dir for a disposable rebuild, or --check to detect stale artifacts.
The builder uses no network, external files or solver integrations.
"""

from __future__ import annotations

import argparse
from dataclasses import asdict
from html import escape
import json
from pathlib import Path
import re

from digitalmodel.subsea.cross_sections.calculations import (
    calculate_elastic_section,
    calculate_radial_section,
)
from digitalmodel.subsea.cross_sections.reporting import build_report_model
from digitalmodel.subsea.cross_sections.schema import (
    CrossSectionDefinition,
    Provenance,
    RadialLayer,
    UnitValue,
)
from digitalmodel.subsea.cross_sections.visualization import render_cross_section_svg

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/cross_sections"
DATE = "2026-10-22"
SOURCES = {
    "area": ("OpenStax Prealgebra 2e (2020), §9.5, circle area",
             "https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures"),
    "mass": ("OpenStax University Physics Volume 1 (2016), §14.1, Eq. 14.1 and Table 14.1",
             "https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure"),
    "buoyancy": ("OpenStax University Physics Volume 1 (2016), §14.4, Archimedes' principle",
                 "https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy"),
    "elasticity": ("OpenStax University Physics Volume 1 (2016), §12.3, Table 12.1 and stress-strain relation",
                   "https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus"),
    "inertia": ("Engineering Statics: Open and Interactive, §10.2, Eq. 10.2.10 (undated public edition, accessed 2026-10-22)",
                "https://engineeringstatics.org/MOI-common-shapes.html#MOI-circles"),
    "steel": ("Engineering ToolBox (2004, updated 2025), Metals and Alloys - Densities, steel row",
              "https://www.engineeringtoolbox.com/metal-alloys-densities-d_50.html"),
    "si": ("BIPM SI Brochure, ninth edition (2019), Table 7, SI prefixes",
           "https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf"),
}
CHANGES = {
    "document": "cross-section-module-analysis", "revision": 0, "date": DATE,
    "changes": [
        "Document per-layer mass, explicit bore buoyancy and bonded elastic stiffness.",
        "Separate assumed example inputs from public material reference values.",
        "Record fixture geometry and missing material data without assuming results.",
        "Correct empty report selection and preserve the open bore in radial figures.",
        "Avoid comparing packed-component diameter magnitudes in incompatible units.",
    ],
}


def example_definition() -> CrossSectionDefinition:
    """Synthetic steel/coating/insulation stack, with every material input explicit."""
    provenance = Provenance(
        source_id="worked-example", source_type="project_assumption",
        note="Geometry and non-steel properties assumed, based on previous experience; see input register.",
    )
    layers = []
    # Steel rho: SOURCES['steel'], steel row, 7850 kg/m³. Other densities and
    # geometry are declared illustrative assumptions, not inferred properties.
    for name, inner, outer, density in [
        ("steel", 0.2, 0.3, 7850), ("coating", 0.3, 0.4, 1000), ("insulation", 0.4, 0.5, 100),
    ]:
        layers.append(RadialLayer(
            name=name, role=name, material=name, provenance=provenance,
            inner_diameter=UnitValue(value=inner, unit="m"),
            outer_diameter=UnitValue(value=outer, unit="m"),
            density=UnitValue(value=density, unit="kg/m^3"),
        ))
    return CrossSectionDefinition(
        id="worked-section", name="Illustrative concentric pipe section",
        family="rigid_pipeline_flowline", duty="static", radial_layers=layers,
        provenance=[provenance],
    )


def calculate() -> dict:
    """Return production results and a fixture inventory without filling data gaps."""
    definition = example_definition()
    # Water: SOURCES['mass'], Table 14.1, fresh water. Gravity is a deliberately
    # rounded illustrative assumption. Steel E: SOURCES['elasticity'], Table 12.1.
    # Coating/insulation E are explicit assumptions, not reference properties.
    moduli = [2e11, 1e9, 1e6]
    environment = dict(water_density_kg_m3=1000, gravity_m_s2=10)
    sealed = calculate_radial_section(definition.radial_layers, **environment, bore_condition="sealed")
    flooded = calculate_radial_section(definition.radial_layers, **environment, bore_condition="flooded")
    elastic = calculate_elastic_section(definition.radial_layers, youngs_moduli_pa=moduli)
    results = asdict(elastic) | {
        key: value for key, value in asdict(sealed).items() if key != "layers"
    }
    results["flooded_submerged_weight_n_m"] = flooded.submerged_weight_n_m
    inventory = []
    for fixture in build_report_model().fixtures:
        item = fixture.definition
        outer = item.radial_layers[-1].derived_outer_diameter if item.radial_layers else None
        inventory.append({
            "id": item.id, "family": item.family,
            "outer_mm": outer.value if outer else None,
            "layers": len(item.radial_layers),
            "components": sum(component.count for component in item.packed_components),
            "material_data": "NOT_EVALUATED: complete densities and moduli absent",
        })
    return {
        "definition": definition, "moduli": moduli, "environment": environment,
        "layers": sealed.layers, "results": results, "fixtures": inventory,
    }


def format_number(value: float) -> str:
    """Use one decimal place, switching to scientific notation for small values."""
    return f"{value:.1e}" if value != 0 and abs(value) < 0.1 else f"{value:,.1f}"


def table(headers: list[str], rows: list[list[str]]) -> str:
    """Render trusted cell markup in a conventional accessible table."""
    return ('<table><thead><tr>' + ''.join(f'<th scope="col">{escape(h)}</th>' for h in headers)
            + '</tr></thead><tbody>'
            + ''.join('<tr>' + ''.join(f'<td>{cell}</td>' for cell in row) + '</tr>' for row in rows)
            + '</tbody></table>')


def mass_chart(data: dict) -> str:
    """Draw per-layer mass bars from calculated values; lengths use a common scale."""
    maximum = max(item.mass_per_length_kg_m for item in data["layers"])
    rows = []
    for index, (layer, result) in enumerate(zip(data["definition"].radial_layers, data["layers"], strict=True)):
        width = 300 * result.mass_per_length_kg_m / maximum
        y = 30 + index * 45
        rows.append(f'<text x="5" y="{y + 16}">{escape(layer.name)}</text>'
                    f'<rect x="120" y="{y}" width="{width:.1f}" height="24" fill="#4c78a8"/>'
                    f'<text x="440" y="{y + 16}">{format_number(result.mass_per_length_kg_m)} kg/m</text>')
    return ('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 620 180" role="img" '
            'aria-label="Calculated mass per length by layer">' + ''.join(rows) + '</svg>')


def render(data: dict) -> str:
    """Render calculated quantities, source locators and engineering limitations."""
    style_source = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", style_source, re.S)
    if style is None:
        raise ValueError("shared report stylesheet is missing")

    def number(key):
        return f'<span data-result="{key}">{format_number(data["results"][key])}</span>'

    def source(key):
        label, url = SOURCES[key]
        return f'<a href="{escape(url)}">{escape(label)}</a>'

    input_rows, layer_rows = [], []
    for layer, modulus, properties in zip(data["definition"].radial_layers, data["moduli"], data["layers"], strict=True):
        # BIPM SI Brochure (2019), Table 7: milli=10^-3, so m -> mm multiplies by 1000.
        input_rows.append([
            escape(layer.name), format_number(layer.derived_inner_diameter.value * 1000),
            format_number(layer.derived_outer_diameter.value * 1000), format_number(layer.density.value),
            format_number(modulus), "public reference" if layer.role == "steel" else "assumed, based on previous experience",
        ])
        layer_rows.append([escape(layer.name), format_number(properties.area_m2), format_number(properties.mass_per_length_kg_m)])
    fixture_rows = [[escape(row["id"]), str(row["layers"]), str(row["components"]),
                     format_number(row["outer_mm"]) if row["outer_mm"] is not None else "bundle envelope unspecified",
                     row["material_data"]] for row in data["fixtures"]]
    result_rows = [[label, number(key), unit] for label, key, unit in [
        ("Outer diameter", "outer_diameter_m", "m"), ("Material area", "area_m2", "m²"),
        ("Dry mass per length", "mass_per_length_kg_m", "kg/m"),
        ("Sealed displacement per length", "displaced_area_m2", "m²"),
        ("Sealed submerged weight, downward positive", "submerged_weight_n_m", "N/m"),
        ("Flooded submerged weight, downward positive", "flooded_submerged_weight_n_m", "N/m"),
        ("Centroidal second moment I", "second_moment_m4", "m⁴"),
        ("Geometric section modulus Z", "section_modulus_m3", "m³"),
        ("Axial stiffness EA", "axial_stiffness_n", "N"),
        ("Bonded bending stiffness EI", "bending_stiffness_n_m2", "N m²"),
    ]]
    sources = '<ul>' + ''.join(f'<li>{source(key)}</li>' for key in SOURCES) + '</ul>'
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cross-section module analysis</title>{style[0]}
<style>.hs-doc table{{width:100%;border-collapse:collapse}} .hs-doc th,.hs-doc td{{padding:.5rem;border:1px solid #bbb;text-align:left;white-space:normal}}
.hs-doc svg{{max-width:100%;height:auto}} .hs-doc section{{margin:2rem 0}} .hs-doc code{{overflow-wrap:anywhere}}</style>
</head><body class="doc hs-doc"><main>
<h1>Cross-section module analysis</h1>
<section id="summary"><h2>Summary</h2>
<p>The module calculates uniform concentric layer area, dry mass, submerged weight and
linear elastic section properties with explicit SI inputs. The worked steel, coating
and insulation example gives {number('mass_per_length_kg_m')} kg/m dry mass and
{number('submerged_weight_n_m')} N/m sealed submerged weight. These are arithmetic
results under declared assumptions; no design acceptance is established.</p></section>
<section id="revision"><h2>Revision history</h2>
{table(['Revision', 'Date', 'What changed and why'], [['0', DATE, 'Initial issue: document computed section properties and distinguish missing fixture material data.']])}
</section>
<section id="inputs"><h2>Inputs</h2>
<p>All calculation geometry is in metres, density in kg/m³, gravity in m/s² and modulus
in Pa. The module uses explicit unit strings and does not use an implicit unit registry.
The caller must convert other units before calculation; millimetre and inch inputs are rejected.
The source fixtures retain their native units for IO and schematic reporting.</p>
<p>The example geometry and non-steel properties are assumed, based on previous experience,
and chosen for transparent hand arithmetic; they are not a proposed pipe design.
Steel density is from {source('steel')}; steel E is from {source('elasticity')}.
Neither value qualifies a specific grade, temperature or product. Geometry is assumed for all layers.</p>
{table(['Layer', 'ID (mm)', 'OD (mm)', 'Density (kg/m³)', 'E (Pa)', 'Material basis'], input_rows)}
<p>Water density is {format_number(data['environment']['water_density_kg_m3'])} kg/m³,
using fresh water from {source('mass')}. Gravity is {format_number(data['environment']['gravity_m_s2'])} m/s²,
assumed, based on previous experience, rounded for arithmetic. Neither is a seawater-site default.
The bore is empty and sealed for the primary result; the flooded comparison excludes bore displacement.</p>
</section>
<section id="method"><h2>Method</h2>
<ol><li>A = π(OD − ID)(OD + ID)/4 and m/L = ρA: {source('area')}; {source('mass')}.</li>
<li>m_total/L = ΣρᵢAᵢ. Outer diameter builds from the first ID plus twice the summed thicknesses.</li>
<li>W_sub/L = (m_total/L − ρ_water A_displaced)g: {source('buoyancy')}.
A_displaced = πOD_outer²/4 for a sealed empty bore, or ΣAᵢ for a freely flooded bore.</li>
<li>I = π(OD⁴ − ID⁴)/64 about any centroidal diameter: subtract the two circle results in
{source('inertia')}. The implementation factors the fourth-power difference to reduce cancellation.</li>
<li>Z = I/(OD_outer/2). With σ = Eε and ε = ε₀ + κy, integration gives
N = ε₀ΣEᵢAᵢ and M = κΣEᵢIᵢ; hence EA = ΣEᵢAᵢ and EI = ΣEᵢIᵢ: {source('elasticity')}.
All layers share strain and curvature; Z alone does not establish a composite stress capacity.</li></ol>
</section>
<section id="worked-example"><h2>Worked example</h2>
<p>For the three layers, A/π = 0.0125, 0.0175, 0.0225 m². Therefore
m/(Lπ) = 98.125 + 17.5 + 2.25 = 117.875 kg/m. Sealed displacement/π is 0.0625 m²;
W/(Lπ) = (117.875 − 62.5) × 10 = 553.75 N/m. These expressions are independent hand
arithmetic from the input register, recorded at the precision needed to reproduce them.</p>
{table(['Layer', 'Area (m²)', 'Mass per length (kg/m)'], layer_rows)}
<figure>{render_cross_section_svg(data['definition'])}<figcaption>Diameter ratios use the supplied layer geometry; the bore is open in the section view.</figcaption></figure>
<figure>{mass_chart(data)}<figcaption>Bar lengths and labels use the computed layer masses.</figcaption></figure>
{table(['Quantity', 'Calculated value', 'Unit'], result_rows)}
</section>
<section id="fixtures"><h2>Fixture results</h2>
<p>The packaged IO fixtures round-trip through YAML, JSON, schema and validation.
The recorded outer diameters and component counts below come from those definitions.
Mass and stiffness remain NOT_EVALUATED: the fixtures do not specify every material property.
A bore/fluid envelope in a fixture is not automatically a solid material layer.</p>
{table(['Fixture', 'Radial layers', 'Total packed components', 'Outer diameter (mm)', 'Material results'], fixture_rows)}
</section>
<section id="evidence"><h2>Validation evidence</h2>
<p>The tests in <code>tests/subsea/cross_sections/</code> compare calculated values to
independent hand arithmetic, including solid circles, annuli, thin walls, bonded layers,
unit round trips, non-finite inputs and every validation-report branch.
Thin-wall OD-based area overestimates by t/(OD − t); the test bounds its own chosen geometry.
Floating-point tolerances are arithmetic allowances and do not imply engineering acceptance.</p>
<p>The report test regenerates both artifacts twice, compares bytes, checks displayed results
against live calculations and detects stale output. It also changes a production result to
ensure the report follows the computation.</p>
<pre><code>PYTHONPATH=src python scripts/reporting/build_cross_sections_module_report.py
PYTHONPATH=src python scripts/reporting/build_cross_sections_module_report.py --check</code></pre>
</section>
<section id="limits"><h2>Limits of use</h2>
<p>Concentric, homogeneous, contiguous layers only. The calculation API requires exact shared
boundaries in metres; the fixture schema's existing input-consistency tolerance is not a
physical gap allowance. Radial geometry must satisfy 0 ≤ ID &lt; OD. Every mass layer needs
a density; every stiffness layer needs E. No material property is inferred from its name.</p>
<p>The bonded elastic model excludes slip, ovalisation, plasticity and temperature dependence.
Sealed buoyancy excludes bore contents mass; flooded buoyancy assumes free water access.
Negative submerged weight means upward force. Geometric section modulus does not establish
stress distribution between dissimilar materials.</p>
<h3>NOT_EVALUATED</h3><ul><li>Design-code compliance, strength, collapse, buckling, fatigue and corrosion allowance.</li>
<li>Seawater conditions, contents mass, trapped pressure, water absorption and end effects.</li>
<li>Packed cable and umbilical mechanical properties, helical lay and manufacturing packing.</li>
<li>Fixture mass and stiffness where complete material inputs are missing.</li>
<li>Thermal/electrical ratings and licensed solver comparison.</li></ul></section>
<section id="recommendations"><h2>Way forward</h2>
<p>Supply documented product dimensions, densities, moduli, contents and environmental values
before using these quantities in a design. Establish layer bonding and bore condition first.
Perform the applicable strength and stability assessment separately; a numerical section
property is not evidence of system acceptance.</p></section>
<section id="sources"><h2>Sources</h2>{sources}
<p>Fixture geometry source: <code>src/digitalmodel/subsea/cross_sections/fixtures/</code>,
representative schema examples. Public equations support the arithmetic; example assumptions
are not sourced material qualification.</p></section>
<section id="changes"><h2>Engineering changes</h2><ul>{''.join(f'<li>{escape(change)}</li>' for change in CHANGES['changes'])}</ul></section>
</main></body></html>
'''


def build(output_dir: Path = OUTPUT, *, check: bool = False) -> None:
    """Write final deliverables, or compare existing files without modifying them."""
    artifacts = {
        "index.html": render(calculate()),
        "changes.json": json.dumps(CHANGES, indent=2, ensure_ascii=False) + "\n",
    }
    if check:
        for name, text in artifacts.items():
            path = output_dir / name
            if not path.is_file() or path.read_text(encoding="utf-8") != text:
                raise ValueError(f"stale or missing report artifact: {name}")
        return
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, text in artifacts.items():
        (output_dir / name).write_text(text, encoding="utf-8")


def main() -> None:
    """Parse the reproducible build options."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    build(args.output_dir, check=args.check)


if __name__ == "__main__":
    main()
