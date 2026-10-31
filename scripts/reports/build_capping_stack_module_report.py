# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build the bounded scalar capping-stack report with external scratch only."""

import argparse
import ast
from dataclasses import asdict
import hashlib
import html
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "src/digitalmodel/subsea/capping_stack"
REVIEWED_REVISION = "09089a121a16069c4b657d1a320a630a23ab1dd5"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Initial scalar module report added to expose the input, method and answer chain and its limits.",
}]
SOURCES = [
    f"{PACKAGE}/{name}" for name in (
        "__init__.py", "containment_capacity.py", "interface_check.py",
        "deployment.py", "containment_flow.py", "_root_finding.py",
        "docs/worked-static-containment.md",
    )
] + [
    "scripts/reports/build_capping_stack_module_report.py",
    "tests/reporting/test_capping_stack_module_report.py",
    "docs/api/_assets/brand.css",
    "docs/api/cfd/turbulent-flat-plate-verification.html",
]
# Inventory describes implementation, not standards conformance. [S] in the report.
INVENTORY = {
    "shut_in_wellhead_pressure": (
        "Reservoir pressure [Pa], positive datum-to-mudline vertical separation [m], column density [kg/m³].",
        "Clamped scalar shut-in pressure [Pa]. Evaluated here at zero separation only.",
    ),
    "check_containment": (
        "Positive supplied rating [Pa] and the same pressure, depth and density inputs.",
        "ContainmentResult: pressure, rating and hydrostatic head [Pa], utilisation [-], inclusive flag, density [kg/m³], notes.",
    ),
    "check_compatibility": (
        "Stack and wellhead nominal bores [in], pressure-class enums, connector enums, bore tolerance [in].",
        "InterfaceResult: compatible flag and reason strings. NOT_EVALUATED: nominal comparisons do not qualify seal or connector performance.",
    ),
    "submerged_weight": (
        "Mass [kg], displaced volume [m³], water density [kg/m³].",
        "Net downward force [N]. Evaluated here with explicit displacement and density.",
    ),
    "feasibility": (
        "CappingStack (mass [kg], optional volume [m³]); DeploymentScenario (crane SWL [N], water depth [m], Hs [m], Hs limit [m], DAF [-]); water density [kg/m³].",
        "DeploymentResult: feasible flag, submerged force and hook load [N], lift utilisation [-], governing constraint, notes. Only supplied scalar comparisons are exercised.",
    ),
    "linear_inflow": (
        "Absolute bottomhole and reservoir pressures [Pa], productivity [m³/(s Pa)].",
        "Nonnegative local volumetric rate [m³/s]. NOT_EVALUATED.",
    ),
    "vogel_inflow": (
        "Absolute bottomhole and reservoir pressures [Pa], maximum local rate [m³/s].",
        "Local volumetric rate [m³/s] from the existing empirical form. NOT_EVALUATED.",
    ),
    "gas_backpressure_inflow": (
        "Absolute bottomhole and reservoir pressures [Pa], coefficient [m³/(s Pa^(2n))], exponent n [-].",
        "Local volumetric rate [m³/s] from the existing pressure-squared form. NOT_EVALUATED.",
    ),
    "solve_operating_point": (
        "Inflow and choke callbacks [m³/s]; absolute reservoir/outlet pressures [Pa]; density [kg/m³], depth/length/diameter [m], gravity [m/s²], friction factor and opening [-].",
        "FlowOperatingPoint: local rate and intersection residuals [m³/s], wellhead/bottomhole pressures [Pa], opening [-]. NOT_EVALUATED.",
    ),
    "staged_shut_in": (
        "Steady well inputs, stage times [s], openings [-], supplied transient pressure allowances [Pa], rating [Pa].",
        "StagedContainment: stage results, times [s], allowances and peak [Pa], peak time [s], terminal and bounded-peak containment results. NOT_EVALUATED; supplied allowances are not computed surge.",
    ),
    "transient_shut_in": (
        "Steady well inputs, closure times [s], openings [-], acoustic speed [m/s], cell count [-], duration [s], reverse productivity [m³/(s Pa)], rating [Pa].",
        "TransientContainment: pressure [Pa] and local rate [m³/s] histories, times [s], peak [Pa], peak time [s], static terminal and computed-peak containment results. NOT_EVALUATED: elastic constant-density model only.",
    ),
}


def json_text(value):
    """Use stable UTF-8 JSON with finite numbers and LF termination."""
    return json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n"


def embedded_json(value):
    """Prevent JSON strings from ending their HTML script element."""
    return json.dumps(value, sort_keys=True, ensure_ascii=True, allow_nan=False).replace(
        "&", "\\u0026"
    ).replace("<", "\\u003c").replace(">", "\\u003e")


def calculate(scratch):
    """Call normal public APIs using explicitly synthetic scalar inputs [H]."""
    from digitalmodel.subsea.capping_stack import (
        CappingStack, DeploymentScenario, check_containment, feasibility,
        shut_in_wellhead_pressure, submerged_weight,
    )

    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    scratch.mkdir(parents=True, exist_ok=True)
    # [H] Synthetic arithmetic fixtures; [G] standard gravity for dimensional SWL.
    inputs = {
        "column_density_kg_m3": 1000, "datum_separation_m": 0,
        "reservoir_pressures_pa": [3000000, 4000000, 5000000],
        "supplied_rating_pa": 4000000,
        "pressure_reference": "common atmospheric gauge; synthetic scalar comparison",
        "mass_kg": 2000, "displaced_volume_m3": 1, "dry_volume_m3": 0,
        "water_density_kg_m3": 1000, "daf": 1,
        "crane_swl_n": 2000 * 9.80665, "water_depth_m": 0,
        "hs_m": [1, 3], "hs_limit_m": 1,
    }
    containment = []
    for pressure in inputs["reservoir_pressures_pa"]:
        row = asdict(check_containment(
            inputs["supplied_rating_pa"], pressure, inputs["datum_separation_m"],
            column_density=inputs["column_density_kg_m3"],
        ))
        # Explanatory production notes are not evidence of physical flow behaviour.
        row.pop("notes")
        row["direct_siwhp"] = shut_in_wellhead_pressure(
            pressure, inputs["datum_separation_m"], inputs["column_density_kg_m3"],
        )
        containment.append(row)
    dry = submerged_weight(inputs["mass_kg"], inputs["dry_volume_m3"], inputs["water_density_kg_m3"])
    wet = submerged_weight(inputs["mass_kg"], inputs["displaced_volume_m3"], inputs["water_density_kg_m3"])
    scenarios = []
    for hs in inputs["hs_m"]:
        row = asdict(feasibility(
            CappingStack(inputs["mass_kg"], displaced_volume=inputs["displaced_volume_m3"]),
            DeploymentScenario(
                inputs["crane_swl_n"], water_depth=inputs["water_depth_m"], hs=hs,
                hs_limit=inputs["hs_limit_m"], daf=inputs["daf"],
            ), seawater_density=inputs["water_density_kg_m3"],
        ))
        row.pop("notes")
        scenarios.append(row)
    return {
        "inputs": inputs, "containment": containment,
        "lift": {"dry_force_n": dry, "wet_force_n": wet,
                 "wet_dry_ratio": wet / dry, "scenarios": scenarios},
    }


def inventory_rows():
    """Bind the human inventory to all public module functions in the source."""
    rows = []
    names = set()
    for filename in ("containment_capacity.py", "interface_check.py", "deployment.py", "containment_flow.py"):
        tree = ast.parse((ROOT / PACKAGE / filename).read_text(encoding="utf-8"))
        for node in tree.body:
            if isinstance(node, ast.FunctionDef) and not node.name.startswith("_"):
                names.add(node.name)
                inputs, outputs = INVENTORY[node.name]
                rows.append(
                    f'<tr><td><code>{html.escape(node.name)}</code><br>{html.escape(filename)}</td>'
                    f'<td>{html.escape(inputs)}</td><td>{html.escape(outputs)} [S]</td></tr>'
                )
    if names != set(INVENTORY):
        raise ValueError("Public function inventory has drifted")
    return "".join(rows)


def result_rows(data):
    """Format calculated values only; percentages are rounded to one decimal."""
    return "".join(
        f'<tr><td>{pressure / 1e6:.1f}</td><td>{row["hydrostatic_head"] / 1e6:.1f}</td>'
        f'<td>{row["siwhp"] / 1e6:.1f}</td><td>{row["rated_working_pressure"] / 1e6:.1f}</td>'
        f'<td>{row["utilisation"]:.1%}</td><td>{row["is_contained"]}</td><td>[H, S]</td></tr>'
        for pressure, row in zip(data["inputs"]["reservoir_pressures_pa"], data["containment"])
    )


def render(data):
    """Render a self-contained report using the existing house typography."""
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r"<style>(.*?)</style>", exemplar, re.S)[1]
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8"), flags=re.S)
    escape = html.escape
    history = "".join(
        f'<tr><td>{escape(row["revision"])}</td><td>{escape(row["date"])}</td><td>{escape(row["change"])}</td></tr>'
        for row in CHANGES
    )
    provenance = "".join(
        f'<li><code>{escape(path)}</code>; SHA-256: <code>{hashlib.sha256((ROOT / path).read_bytes()).hexdigest()}</code></li>'
        for path in SOURCES
    )
    lift_rows = "".join(
        f'<tr><td>{hs:.1f}</td><td>{data["inputs"]["hs_limit_m"]:.1f}</td>'
        f'<td>{row["hook_load"]:.1f}</td><td>{row["lift_utilisation"]:.1%}</td>'
        f'<td>{row["feasible"]}</td><td>{escape(row["governing_constraint"])}</td><td>[H, G, S]</td></tr>'
        for hs, row in zip(data["inputs"]["hs_m"], data["lift"]["scenarios"])
    )
    # Diagram positions are presentation coordinates, not dimensions or results.
    figure = f'''<svg viewBox="0 0 720 200" role="img" aria-label="Calculated wet-to-dry force ratio">
<rect x="180" y="35" width="{400 * data['lift']['wet_dry_ratio']:.1f}" height="35" fill="var(--brand)"/>
<rect x="180" y="100" width="400" height="35" fill="var(--panel-2)" stroke="currentColor"/>
<text x="15" y="60">Wet force</text><text x="15" y="125">Dry reference</text>
<text x="180" y="175">Wet / dry = {data['lift']['wet_dry_ratio']:.1%} [H, S]</text></svg>'''
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Capping-stack scalar module analysis</title><style>{brand}\n{layout}
pre, code {{ overflow-wrap: anywhere; white-space: pre-wrap; }}
nav a {{ margin-right: 1em; }} svg {{ max-width: 100%; }}
</style></head><body class="hs-doc"><main class="wrap">
<header><h1>Capping-stack scalar module analysis</h1>
<p>Static containment comparison and buoyancy balance; draft for technical review.</p></header>
<nav aria-label="Report sections"><a href="#s1">Summary</a><a href="#s3">Inputs</a>
<a href="#s5">Calculation</a><a href="#s7">Results</a><a href="#s8">Limits</a><a href="#s10">Evidence</a></nav>
<h2 id="s1">1. Summary</h2>
<p>The public capping-stack API reproduces the synthetic scalar pressure boundaries and submerged-weight ratio derived below [H, S]. The pressure comparison accepts equality and rejects exceedance. These are arithmetic checks against supplied limits; they establish no equipment or operating qualification.</p>
<p>The calculated wet-to-dry force ratio is {data['lift']['wet_dry_ratio']:.1%} [H, S]. Raising only the supplied sea state changes the returned governing constraint while hook load remains unchanged. The report inventories the wider package without evaluating its interface or inflow/transient methods.</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody></table>
<h2 id="s2">2. Context and scope</h2>
<p>The package provides static pressure comparison, nominal interface checks, scalar lift screens and constant-density inflow/transient helpers. Reviewed source revision: <code>{REVIEWED_REVISION}</code> [S]. The manifest binds the exact reviewed file bytes and report-generation evidence. A rebuild checks the current bytes; a matching replay alone is not independent numerical evidence.</p>
<p>Scope is limited to scalar pressure containment with zero vertical separation and the submerged-weight ratio. The cases are synthetic fixtures, not a field example, pressure class, deployment recommendation or selected design basis. The existing <a href="../../../../src/digitalmodel/subsea/capping_stack/docs/worked-static-containment.md">public-source worked document</a> and its revision history are preserved; its nonzero-column example is separate evidence [W].</p>
<h2 id="s3">3. Input basis</h2>
<p>[H] defines all numerical inputs below as arbitrary arithmetic test values. They are assumptions for this calculation only. No physical calibration is inferred. [G] supplies standard gravity for the separate dimensional force check; gravity cancels from the ratio and the zero-height pressure check.</p>
<table><thead><tr><th>Input</th><th>Unit</th><th>Basis</th></tr></thead><tbody>
<tr><td>Column and water density</td><td>kg/m³</td><td>Explicit synthetic densities [H]; not a gas model or measured seawater property.</td></tr>
<tr><td>Datum separation; water depth</td><td>m</td><td>Zero-height boundary [H]; no actual subsea location.</td></tr>
<tr><td>Reservoir pressures; supplied rating</td><td>Pa</td><td>Scalar pressure inputs [H]; no equipment class selected.</td></tr>
<tr><td>Mass; wet and dry displaced volume</td><td>kg; m³</td><td>Synthetic force balance [H]; explicit volume overrides the default.</td></tr>
<tr><td>DAF; crane SWL; Hs and Hs limit</td><td>dimensionless; N; m</td><td>Unit multiplier; dry-force reference as comparison limit; arbitrary sea-state pair [H, G].</td></tr>
</tbody></table>
<p>Exact unrounded input values follow; suffixes specify SI units. Reservoir and comparison pressures share the atmospheric gauge reference stated in the payload. The upper datum maps to the mudline argument only as a scalar fixture; no external seawater pressure is subtracted.</p>
<pre>{escape(json_text(data['inputs']))}</pre>
<h2 id="s4">4. Method and public function inventory</h2>
<p>For z positive downward from the upper datum, a static fluid element gives A dp = ρ A g dz. Integration gives p_res − p_wh,raw = ∫ρg dz = ρgh for constant density. The implementation then uses p_screen = max(p_res − ρgh, 0), U = p_screen/R, and is_contained = (p_screen ≤ R) [H, S]. Pressure units are kg/(m s²), or Pa. The zero clamp is an implementation policy; it is not evidence of flow direction or absolute vacuum.</p>
<p>Buoyancy force is the weight of displaced fluid. Net downward force W = (m − ρV)g, hook force F = DAF × W, lift utilisation = F/SWL, and the returned scalar flag requires both F/SWL ≤ unity and Hs ≤ Hs_limit [H, S]. DAF means supplied dynamic amplification factor, Hs means significant wave height and SWL means the supplied crane load limit.</p>
<table><thead><tr><th>Public function / source</th><th>Inputs and units</th><th>Output and evaluated scope</th></tr></thead><tbody>{inventory_rows()}</tbody></table>
<p>Public types: ContainmentResult; PressureClass (including its pascals property), ConnectorClass, WellheadSpec, CappingStackSpec, InterfaceResult; CappingStack, DeploymentScenario, DeploymentResult; FlowOperatingPoint, StagedContainment, TransientContainment. The pressure property converts the enum using PSI_TO_PA. Exported GAS_COLUMN_DENSITY, OIL_COLUMN_DENSITY, BRINE_COLUMN_DENSITY and PSI_TO_PA are convenience inputs, not qualified data. The private root-finding helper is implementation support, not a public entry point [S].</p>
<h2 id="s5">5. Worked calculation</h2>
<p>[H] Independent hand derivation: with ρ = 1000 kg/m³ and h = 0 m, ∫ρg dz = 0 Pa for any finite constant g. For p_res = 3.0, 4.0 and 5.0 MPa, scalar wellhead pressures are respectively 3.0, 4.0 and 5.0 MPa. With supplied R = 4.0 MPa, U = 3/4, unity and 5/4; the exact inclusive flags are True, True and False. These pressures and the rating are arbitrary scalar inputs, not an equipment pressure class.</p>
<p>[H] For mass 2000 kg, volume 1 m³ and water density 1000 kg/m³, W_wet/W_dry = (2000 − 1000 × 1)/2000 = one-half. The dry comparison uses the same mass and zero displaced volume. Standard gravity from [G] supplies dimensional forces; it is not needed for the one-half ratio. The supplied crane limit is the dry force, and DAF is unity, so lift utilisation is also one-half.</p>
<figure>{figure}<figcaption>Generated force-ratio diagram from public helper outputs; bar lengths are proportional forces, not physical dimensions [H, S].</figcaption></figure>
<h2 id="s6">6. Checks</h2>
<p>The dedicated regression test independently derives the preceding balances without importing implementation constants. Positive values use relative tolerance 1e-12 with no absolute tolerance, solely for binary floating-point roundoff; exact zero and inclusive decision flags are compared exactly. This is not an uncertainty allowance or a decision tolerance [T].</p>
<p>Separate checks compare API replay, two clean external rebuilds and committed bytes. Manifest digests are checked by independent readback. Negative controls change a result magnitude or sign and coherently regenerate both HTML and hashes: internal agreement passes integrity checks but fails the hand oracle. Additional tampering without regeneration fails the manifest check [T]. These are regression and freshness checks, not physical validation.</p>
<h2 id="s7">7. Results</h2>
<table><thead><tr><th>p_res [MPa]</th><th>Head [MPa]</th><th>p_screen [MPa]</th><th>Supplied R [MPa]</th><th>Utilisation [%]</th><th>is_contained</th><th>Basis</th></tr></thead><tbody>{result_rows(data)}</tbody></table>
<p>Dry reference force: {data['lift']['dry_force_n']:.1f} N; wet force: {data['lift']['wet_force_n']:.1f} N; wet/dry ratio: {data['lift']['wet_dry_ratio']:.1%} [H, G, S]. Forces use the standard-gravity convention. Display values are rounded; <a href="results.json">results.json</a> retains unrounded API outputs.</p>
<table><thead><tr><th>Hs [m]</th><th>Supplied limit [m]</th><th>Hook load [N]</th><th>Lift utilisation [%]</th><th>feasible</th><th>Governing constraint</th><th>Basis</th></tr></thead><tbody>{lift_rows}</tbody></table>
<p>The feasible flag is the API field name for these two supplied comparisons. It does not establish deployment suitability. The higher sea state changes the governing status without a change to computed hook load because Hs is not used to derive DAF in this helper [S].</p>
<h2 id="s8">8. Limits / NOT_EVALUATED</h2>
<ul><li>Pressure reference and datum: static functions do not tag gauge versus absolute pressure or perform ambient-pressure conversion. Callers must provide a consistent reference for pressure and rating. Flow helpers require absolute pressures. Mixing these conventions is outside the evidence [S].</li>
<li>Density: static containment defaults to GAS_COLUMN_DENSITY = 200 kg/m³; the oil and brine presets are 800 and 1030 kg/m³ respectively [S]. These convenience densities are assumed, based on previous experience. Constant density and gravity are assumed; this report supplies density explicitly and checks no nonzero hydrostatic gradient. The clamp does not resolve an overbalanced well or flow behaviour [S, W].</li>
<li>Displacement and dynamics: omitted displacement is inferred as mass divided by STACK_BULK_DENSITY. The implemented bulk density is 4000 kg/m³, water density defaults to 1025 kg/m³ and DAF defaults to 2.0 [S]. These convenience assumptions are assumed, based on previous experience; they are not calibrated data. No default is qualified here; displacement, density and DAF are explicit in every displayed case. Water depth is stored but unused by feasibility; current, splash-zone loads, crane geometry and line dynamics are NOT_EVALUATED [S].</li>
<li>Input domains: static pressure and lift helpers reject nonpositive rating or crane SWL, but do not comprehensively validate finite or physically meaningful density, depth, mass, volume and dynamic multiplier. Broader input validation and the treatment of buoyant upward force remain open findings, without an algorithm or acceptance-policy change [S].</li>
<li>Nominal interface compatibility, connector/seal mechanics, interface standards, well-control operations, emergency-response decisions, deployment suitability and all standards qualification are NOT_EVALUATED. No licensed clause values or acceptance criteria are reproduced.</li>
<li>Inflow curves, choke callbacks, local gas-volume conversion, elastic transients, multiphase/compositional behaviour, cavitation and physical calibration are NOT_EVALUATED. Existing transient helpers do not imply transient multiphase qualification [S].</li></ul>
<h2 id="s9">9. Way forward</h2>
<p>Maintain the report as bounded arithmetic evidence. Before engineering use, establish pressure reference, external pressure treatment, credible column density, datum and rating provenance. Supply measured mass/displacement, qualified crane and environmental criteria and a justified dynamic-load method. Resolve input-domain and acceptance-policy findings separately. Obtain independent review of methods, standards applicability and physical data before any operational conclusion. The wider qualification portions remain open.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{escape(CHANGES[-1]['change'])}</p></details>
<h2 id="s10">10. References and evidence</h2>
<ul><li>[H] Independent derivations in the worked calculation: static vertical force balance and displaced-fluid weight. Every synthetic input is defined in the input payload and independently pinned in the dedicated test. No measurement or published equipment example is asserted.</li>
<li>[G] <a href="https://physics.nist.gov/cgi-bin/cuu/Value?gn">NIST: standard acceleration of gravity</a>, exact standard-gravity convention, g = 9.80665 m/s²; consulted 2026-10-28. This constant checks dimensional forces independently of the source constants.</li>
<li>[S] Public package inventory at reviewed revision <code>{REVIEWED_REVISION}</code>; exact source hashes follow. Existing source docstrings are descriptions of code, not evidence of standards qualification.</li>
<li>[W] <a href="../../../../src/digitalmodel/subsea/capping_stack/docs/worked-static-containment.md">Preserved static-containment worked document</a>: separate public-source example, hydrostatic derivation, conversions and documented limits. Its historical source-access statements are unchanged and are not new source-access claims.</li>
<li>[T] <code>tests/reporting/test_capping_stack_module_report.py</code>: independent oracle, negative controls, replay, rebuild, manifest and link assertions. Test outcomes belong to the delivery record; executing the builder alone does not assert a passing test run.</li></ul>
<p>Delivery files: <a href="results.json">unrounded results</a>, <a href="changes.json">change history</a>, <a href="manifest.json">byte manifest</a> and <a href="README.md">rebuild instructions</a>. The manifest excludes its own digest to avoid a circular binding. Its digest convention is SHA-256 over exact UTF-8 bytes with LF line endings. No runtime timestamps, absolute paths or environment identities enter the evidence.</p>
<details><summary>Source file digests</summary><ul>{provenance}</ul></details>
</main><script id="calculation" type="application/json">{embedded_json(data)}</script>
<script id="changes" type="application/json">{embedded_json(CHANGES)}</script></body></html>'''
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


README = """# Capping-stack scalar module analysis

The [module report](index.html) inventories the public API and presents synthetic
pressure boundaries and a buoyancy force ratio. Physical and operational
qualification remain `NOT_EVALUATED`. The existing public-source worked document
and its history are preserved.

Rebuild with normal source imports and caller-selected external scratch:

```sh
PYTHONPATH=src uv run --no-project --with pytest python scripts/reports/build_capping_stack_module_report.py --scratch "$REPORT_SCRATCH" --output-dir docs/reports/modules/capping_stack
```

Set `REPORT_SCRATCH` to a writable directory outside the checkout. Both command
arguments are required. All intermediate files belong there; only the five
small delivery files are written to the output directory. The builder requires
the standard library and the existing public package, without a solver.

The [results](results.json) retain unrounded inputs and public API outputs.
The [manifest](manifest.json) binds all other deliverable bytes and the exact
source, builder, styles and regression test; it excludes itself.
The [change history](changes.json) matches the report revision table.

Run `tests/reporting/test_capping_stack_module_report.py` with source imports
and an external pytest base. Its independent hand calculation is distinct from
API replay and byte-rebuild checks. Coherently regenerated wrong values fail
the numerical oracle, even when all regenerated manifest digests match.
"""


def build(scratch, output_dir):
    """Write deterministic artifacts and bind their actual bytes in a manifest."""
    data = calculate(scratch)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    outputs = {
        "index.html": render(data), "results.json": json_text(data),
        "changes.json": json_text(CHANGES), "README.md": README,
    }
    for name, text in outputs.items():
        (output_dir / name).write_text(text, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1, "reviewed_source_revision": REVIEWED_REVISION,
        "digest_convention": "SHA-256 over exact UTF-8 bytes with LF line endings; manifest excludes itself.",
        "sources": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in SOURCES},
        "artifacts": {name: hashlib.sha256((output_dir / name).read_bytes()).hexdigest() for name in outputs},
    }
    (output_dir / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")


def main():
    """Require explicit calculation scratch and delivery destinations."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
