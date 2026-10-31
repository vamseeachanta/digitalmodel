# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the synthetic full-wall geometry report with external scratch storage."""

import argparse
import hashlib
from html import escape
import json
from pathlib import Path
import re
import runpy

ROOT = Path(__file__).resolve().parents[4]
EXAMPLE = "examples/domains/subsea/flexible_pipe_geometry.py"
NOTE = "examples/domains/subsea/flexible_pipe_geometry.md"
# Source G: NOTE, Independent hand calculation. This software comparison
# tolerance does not prescribe material accuracy or engineering acceptance.
RELATIVE_TOLERANCE = 1e-12
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Synthetic full-wall geometry and mass are presented with an independent radius-based calculation, generated figures and explicit qualification limits.",
}]
SOURCE_PATHS = {
    "G": (NOTE, "Synthetic inputs, circle geometry and independent hand calculation"),
    "E": (EXAMPLE, "Public build_section interface and explicit fluid densities"),
    "C": ("src/digitalmodel/subsea/flexible_pipes/cross_section.py", "Geometry, full-wall mass and section interfaces"),
    "D": ("src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py", "Interface map only; no standard design limits qualified"),
    "B": ("src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py", "Interface map only; no sizing qualified"),
    "I": ("src/digitalmodel/subsea/flexible_pipes/__init__.py", "Public exports and deferred methods"),
    "T": ("tests/reporting/test_flexible_pipes_module_report.py", "Independent oracle, rebuild and stale-calculation tests"),
    "L": ("docs/api/cfd/turbulent-flat-plate-verification.html", "Existing report layout only"),
    "P": ("docs/api/_assets/brand.css", "Existing palette only"),
    "R": ("docs/reports/modules/flexible_pipes/build_report.py", "Report generation and comparison criterion"),
}


def refresh_checks(data):
    """Re-evaluate checks from returned values before reporting their status."""
    for check in data["checks"]:
        value = data["values"][check["key"]]["value"]
        expected = check["expected"]
        check["relative_error"] = abs(value - expected) / abs(expected)
        check["passes"] = check["relative_error"] <= RELATIVE_TOLERANCE
    return data


def calculate(scratch):
    """Execute build_section and retain a transparent radius-based comparator."""
    import math

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    example = runpy.run_path(str(ROOT / EXAMPLE))
    section = example["build_section"]()
    values, checks, inputs = {}, [], []

    def add(key, value, unit, decimals, expected=None):
        values[key] = {"value": value, "unit": unit, "decimals": decimals, "source": "G"}
        if expected is None:
            inputs.append(key)
        else:
            checks.append({"key": key, "expected": expected})

    # All engineering inputs and decimal places: G, Synthetic inputs and Run
    # offline. Two radii per diameter and circle area pi*r^2 are definitions G.
    areas, masses, layers = [], [], []
    for index, layer in enumerate(section.layers, start=1):
        key = f"layer_{index}."
        ri = layer.inner_diameter / 2
        ro = ri + layer.thickness
        area = math.pi * (ro**2 - ri**2)
        areas.append(area)
        masses.append(layer.density * area)
        layers.append({"index": index, "type": layer.layer_type.value})
        add(key + "inner_diameter", layer.inner_diameter, "m", 3)
        add(key + "thickness", layer.thickness, "m", 3)
        add(key + "density", layer.density, "kg/m^3", 1)
        add(key + "outer_diameter", layer.outer_diameter, "m", 3, 2 * ro)
        add(key + "inner_radius", layer.inner_diameter / 2, "m", 3, ri)
        add(key + "outer_radius", layer.outer_diameter / 2, "m", 3, ro)
        add(key + "annular_area", layer.annular_area, "m^2", 9, area)
        add(key + "dry_mass", layer.mass_per_length(), "kg/m", 6, masses[-1])
    external = example["EXTERNAL_FLUID_DENSITY"]
    contents = example["BORE_FLUID_DENSITY"]
    add("external_density", external, "kg/m^3", 1)
    add("bore_density", contents, "kg/m^3", 1)
    rb = section.layers[0].inner_diameter / 2
    rext = section.layers[-1].inner_diameter / 2 + section.layers[-1].thickness
    add("bore_diameter", section.bore_diameter, "m", 3, 2 * rb)
    add("outer_diameter", section.outer_diameter, "m", 3, 2 * rext)
    add("wall_area", sum(layer.annular_area for layer in section.layers), "m^2", 9,
        math.pi * (rext**2 - rb**2))
    add("envelope_area", math.pi * (section.outer_diameter / 2)**2, "m^2", 9,
        math.pi * rext**2)
    add("bore_area", math.pi * (section.bore_diameter / 2)**2, "m^2", 9,
        math.pi * rb**2)
    add("dry_mass", section.dry_mass_per_length(), "kg/m", 6, sum(masses))
    add("displaced_mass", section.displaced_mass_per_length(external), "kg/m", 6,
        external * math.pi * rext**2)
    add("content_mass", section.content_mass_per_length(contents), "kg/m", 6,
        contents * math.pi * rb**2)
    data = refresh_checks({"values": values, "checks": checks, "input_keys": inputs, "layers": layers})
    scratch.mkdir(parents=True, exist_ok=True)
    write_checked(scratch / "calculation.json", json.dumps(data, indent=2, allow_nan=False) + "\n")
    return data


def write_checked(path, text):
    """Read back every saved artifact before reporting successful generation."""
    path.write_text(text, encoding="utf-8", newline="\n")
    if path.read_text(encoding="utf-8") != text:
        raise OSError("Generated artifact readback differs")


def source_register():
    """Bind the public synthetic basis, implementation and renderer to bytes."""
    return {
        "revision": CHANGES[-1]["revision"],
        "readiness": "synthetic arithmetic only",
        "freshness": "Exact source SHA-256 equality on rebuild; no volatile metadata",
        "units": ["m", "m^2", "kg/m^3", "kg/m"],
        "sources": [{"id": key, "path": path, "use": use,
                     "sha256": hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}
                    for key, (path, use) in SOURCE_PATHS.items()],
    }


def value_table(data, keys, caption):
    """Render raw calculated values at the documented display resolution."""
    rows = []
    for key in keys:
        item = data["values"][key]
        shown = f'{item["value"]:.{item["decimals"]}f}'
        rows.append(f'<tr><th scope="row">{escape(key)}</th>'
                    f'<td data-key="{escape(key)}" data-unit="{escape(item["unit"])}">{shown}</td>'
                    f'<td>{escape(item["unit"])}</td><td>[G, C]</td></tr>')
    return ('<table><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Source</th></tr></thead>'
            f'<tbody>{"".join(rows)}</tbody><caption>{escape(caption)}</caption></table>')


def geometry_figure(data):
    """Draw concentric walls to calculated radii; plotting coordinates are pixels."""
    # Viewport and scale are presentation choices, not engineering constants.
    scale = 120 / (data["values"]["outer_diameter"]["value"] / 2)
    keys = [f'layer_{layer["index"]}.outer_radius' for layer in reversed(data["layers"])]
    keys += ["bore_diameter", "outer_diameter"]
    circles = []
    for key in keys:
        value = data["values"][key]["value"]
        radius = value / 2 if key.endswith("diameter") else value
        fill = "none" if key == "outer_diameter" else "var(--panel)" if key == "bore_diameter" else "var(--panel-2)"
        circles.append(f'<circle cx="180" cy="150" r="{radius * scale:.12g}" '
                       f'data-key="{key}" data-scale="{scale:.12g}" '
                       f'fill="{fill}" stroke="var(--brand)"><title>{escape(key)}</title></circle>')
    labels = "".join(f'<text x="330" y="{100 + 30 * index}">{escape(layer["type"])}</text>'
                     for index, layer in enumerate(data["layers"]))
    return ('<figure><svg viewBox="0 0 650 300" role="img" aria-label="Calculated concentric full-wall geometry">'
            f'{"".join(circles)}{labels}</svg><figcaption>Full-wall cross-section, drawn from the calculated radii in m; '
            'layers are listed from bore to exterior. Wound armour and voids are absent [G, C].</figcaption></figure>')


def render(scratch):
    """Generate the self-contained report without importing solver or fatigue code."""
    data = refresh_checks(calculate(scratch))
    inputs = value_table(data, data["input_keys"], "Synthetic inputs; chosen for arithmetic only [G, E].")
    outputs = value_table(data, [key for key in data["values"] if key not in data["input_keys"]],
                          "Worked geometry and mass; display rounding does not express input accuracy [G, C].")
    history = "".join(f'<tr><td>{escape(row["revision"])}</td><td>{row["date"]}</td><td>{escape(row["change"])}</td></tr>' for row in CHANGES)
    checks = "".join(f'<tr><td>{escape(row["key"])}</td><td>{row["relative_error"]:.1e}</td>'
                     f'<td>{"PASS" if row["passes"] else "FAIL"}</td></tr>' for row in data["checks"])
    disposition = "PASS" if all(row["passes"] for row in data["checks"]) else "FAIL"
    sources = "".join(f'<li>[{row["id"]}] <code>{escape(row["path"])}</code>: {escape(row["use"])}; '
                      f'SHA-256: <code>{row["sha256"]}</code></li>' for row in source_register()["sources"])
    # Same existing layout and canonical palette used by the seakeeping report.
    layout = re.search(r"<style>(.*?)</style>", (ROOT / SOURCE_PATHS["L"][0]).read_text(encoding="utf-8"), re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / SOURCE_PATHS["P"][0]).read_text(encoding="utf-8"), flags=re.S)
    styles = "\n".join(line.rstrip() for line in (brand + "\n" + layout).splitlines())
    payload = json.dumps(data, sort_keys=True, allow_nan=False).replace("<", "\\u003c")
    return f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Flexible-pipe module analysis</title><style>{styles}</style></head>
<body class="hs-doc"><main class="wrap"><header><h1>Flexible-pipe module analysis</h1>
<p>Synthetic full-wall geometry and mass. Engineering qualification remains NOT_EVALUATED.</p></header>
<h2 id="s1">1. Summary</h2>
<p>The public geometry example is evaluated through <code>build_section</code>. The numerical scope is contiguous full-wall geometry and mass per unit length.
The radius-based comparison gives {disposition} against the arithmetic criterion in Section 6. This disposition applies only to the synthetic case; manufactured-pipe acceptance is not established [G, E, C].</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody>{history}</tbody><caption>Report issue history; earlier rows are retained on reissue.</caption></table>
<h2 id="s2">2. Purpose and interface map</h2>
<p>The existing methods are mapped below. Numerical evaluation is limited to the geometry example [E, I].</p>
<table><thead><tr><th>Module</th><th>Interface and delegates</th><th>Evaluation boundary</th></tr></thead><tbody>
<tr><td>cross_section</td><td>Layer / LayerType: mean_diameter, outer_diameter, annular_area, structural_area, mass_per_length, axial_stiffness, bending_stiffness.
FlexiblePipeSection: bore_diameter, outer_diameter, dry_mass_per_length, displaced_mass_per_length, content_mass_per_length,
submerged_weight_per_length, tensile_armour_area, axial_stiffness, bending_stiffness. Section totals delegate to layer methods.</td><td>Geometry and full-wall mass only [C]. Weight and stiffness remain NOT_EVALUATED.</td></tr>
<tr><td>design_checks_17J</td><td>TensileArmour, LoadCase, FlexCheckResult; verify_flexible_pipe delegates to allowable_wire_stress,
armour_tension_stress and armour_bending_stress; max_utilisation summarizes the result.</td><td>Interface map only [D]. Standard design limits and structural qualification are NOT_EVALUATED.</td></tr>
<tr><td>bend_stiffener</td><td>BendStiffenerResult; screen_bend_stiffener delegates to developed_min_radius and required_effective_ei.</td>
<td>Interface map only [B]. Bend-stiffener sizing is NOT_EVALUATED.</td></tr></tbody>
<caption>Existing public interfaces and their calculation boundaries [C, D, B, I].</caption></table>
<h2 id="s3">3. Inputs and units</h2>
<p>The synthetic dimensions and densities are supplied by the adjacent example note. These chosen values are not material specifications.
External and bore fluid densities are explicitly supplied; library defaults are not used [G, E].</p>{inputs}
{geometry_figure(data)}
<h2 id="s4">4. Method and equation sources</h2>
<p>Elementary circle geometry is used as the independent basis: r_inner = D_inner / 2; r_outer = r_inner + t;
A_wall = π(r_outer² − r_inner²); m_layer = ρ A_wall. The diameter factor is definitional [G].</p>
<p>The production mass expression uses mean diameter: m_layer = ρ π D_mean t. The comparator instead subtracts squared radii.
Total dry mass is the sum of layer masses. Displaced mass is external density times the entire outer-envelope circle;
content mass is bore density times the bore circle [G, C].</p>
<p>No standard factors or empirical coefficients are adopted. The source register identifies repository methods and the synthetic basis;
it does not establish compliance with an external standard.</p>
<h2 id="s5">5. Worked values</h2>
<p>All values below are generated from the returned public example section. Areas and masses retain the example's display precision;
the embedded calculation retains unrounded values for the independent arithmetic comparison [G, E, C].</p>{outputs}
<h2 id="s6">6. Actual checks and criterion</h2>
<p>Each calculated result is compared with a radius-based reference using |calculated − reference| / |reference| ≤ {RELATIVE_TOLERANCE:.1e}.
Every reference in this synthetic case is nonzero. This is a software arithmetic criterion, not an engineering release tolerance [G, R, T].
Displayed relative differences use one decimal in scientific notation; decisions use unrounded values.</p>
<table><thead><tr><th>Quantity</th><th>Relative difference (dimensionless)</th><th>Arithmetic disposition</th></tr></thead>
<tbody>{checks}</tbody><caption>Checks executed during this build against radius-derived values [G, C, R].</caption></table>
<p>The focused rebuild test independently specifies the synthetic inputs, derives every result from radii, checks units and rounding,
compares separate builds, resolves local links, and perturbs a returned mass to expose stale rendering [T]. A clean rebuild establishes reproducibility only.</p>
<h2 id="s7">7. Correctness findings and known limitations</h2>
<p>No geometry or full-wall mass defect is established by this case. The following interface findings limit broader use and are separate from the arithmetic comparison.</p>
<ul><li>Layer mass uses the full annular wall even when metallic_area is supplied; structural_area uses the metallic override. Wound-wire mass and void fractions therefore require another basis [C].</li>
<li>Missing minimum bend radius gives zero curvature utilisation in verify_flexible_pipe, although the result docstring describes an infinite value when absent.
An omitted check cannot establish bend-radius acceptance [D].</li>
<li>Non-finite values are not explicitly rejected by the layer input comparisons. The present case supplies finite positive dimensions and densities [C, G].</li></ul>
<p>The concentric full walls omit interlocked layers, wire helices, gaps, friction, ovalisation and manufacturing tolerances.
Stiffness estimates and structural screening interfaces are not qualified by a mass calculation [C, D, B].</p>
<h2 id="s8">8. NOT_EVALUATED and required basis</h2>
<table><thead><tr><th>Method</th><th>Status</th><th>Needed reference basis</th></tr></thead><tbody>
<tr><td>Standard design limits and armour capacity</td><td>NOT_EVALUATED</td><td>Applicable API 17J / ISO 13628-2 and API 17B editions, governing clause titles, load cases, material certificates and qualified manufacturer capacities [D].</td></tr>
<tr><td>Armour homogenization, EA and EI</td><td>NOT_EVALUATED</td><td>Actual wire areas, lay angles, layer interaction, slip and test-backed stiffness curves [C].</td></tr>
<tr><td>Fatigue</td><td>NOT_EVALUATED</td><td>Qualified stress histories, stress concentration factors, fatigue curves, environment and cycle-counting basis [I].</td></tr>
<tr><td>Bend-stiffener sizing and MBR</td><td>NOT_EVALUATED</td><td>Qualified pipe MBR, load/rotation envelope, nonlinear polymer moment-curvature data, taper geometry and applicable API 17L1 / 17L2 basis [B].</td></tr>
<tr><td>Real manufactured-pipe behaviour, collapse and pressure capacity</td><td>NOT_EVALUATED</td><td>Manufactured layer geometry, voids, tolerances, pressure/temperature/environment data and independent test evidence [C, I].</td></tr>
<tr><td>Catenary integration and solver response</td><td>NOT_EVALUATED</td><td>Qualified system geometry, boundary conditions, load histories and independently checked native outputs [I].</td></tr>
</tbody><caption>Deferred methods require a qualified reference basis before acceptance.</caption></table>
<h2 id="s9">9. Source register and reproducibility</h2>
<p>The authoritative input is the existing repository synthetic note, not measured data. Intended-use readiness is synthetic arithmetic only.
Source SHA-256 digests identify the exact bytes consumed. No timestamps or scratch/output paths are embedded; no metadata is removed for the deterministic comparison.</p>
<ul>{sources}</ul><p><a href="source_register.json">Machine-readable source register</a> · <a href="changes.json">Change record</a></p>
<p>From the repository root, with externally supplied scratch and output directories:</p>
<pre>PYTHONPATH=src uv run --no-project python docs/reports/modules/flexible_pipes/build_report.py --scratch "$REPORT_SCRATCH" --output "$REPORT_OUTPUT"</pre>
<p>The output directory receives the report, source register and change record. Calculation intermediates remain in scratch.
The local README documents rebuilding and delivery. Tests always use separate external directories.</p>
<h2 id="s10">10. Way forward</h2>
<p>The synthetic geometry and mass comparison can support software regression for this case. Before applying the methods to a manufactured pipe,
obtain the reference basis listed in Section 8, independently verify the corresponding method and define acceptance criteria for the governing case.
Structural and API qualification remain open under <a href="https://github.com/vamseeachanta/digitalmodel/issues/3343">issue 3343</a>.
No structural acceptance follows from the arithmetic disposition or deterministic rebuild.</p>
<script id="calculation" type="application/json">{payload}</script>
</main></body></html>
'''


def main():
    """Require explicit calculation scratch and delivery destinations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    page = render(args.scratch)
    args.output.mkdir(parents=True, exist_ok=True)
    for name, content in {
        "index.html": page,
        "changes.json": json.dumps(CHANGES, indent=2) + "\n",
        "source_register.json": json.dumps(source_register(), indent=2) + "\n",
    }.items():
        write_checked(args.output / name, content)


if __name__ == "__main__":
    main()
