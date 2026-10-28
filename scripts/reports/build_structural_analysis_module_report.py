# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the structural analysis report from synthetic public-function inputs.

Run with PYTHONPATH=src and --scratch outside the checkout. No solver is used.
The numerical basis is derived in the report; no design factor is prescribed.
"""

import argparse
import hashlib
import html
import json
from pathlib import Path
from typing import Any

from digitalmodel.structural.structural_analysis import (
    ColumnBucklingAnalyzer,
    MaterialProperties,
    StressCalculator,
)

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/structural_analysis"
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "Independent synthetic beam and ideal-column calculations, units, "
              "method limits and reproducible report artifacts are added. "
              "Nonzero transverse plate stress is rejected because its effect "
              "was previously omitted from the returned utilization.",
}]
SOURCES = [
    "scripts/reports/build_structural_analysis_module_report.py",
    "src/digitalmodel/structural/structural_analysis/__init__.py",
    "src/digitalmodel/structural/structural_analysis/models.py",
    "src/digitalmodel/structural/structural_analysis/stress_calculator.py",
    "src/digitalmodel/structural/structural_analysis/buckling.py",
    "src/digitalmodel/structural/structural_analysis/capacity.py",
    "src/digitalmodel/codes.py",
    "tests/structural/structural_analysis/test_buckling_capacity.py",
    "tests/structural/structural_analysis/test_structural_analysis_unit.py",
    "tests/structural/structural_analysis/test_module_report_hand_values.py",
    "tests/structural/structural_analysis/test_grillage_buckling.py",
    "src/digitalmodel/structural/buckling_parametric.py",
    "tests/structural/test_buckling_parametric.py",
    "tests/reporting/test_structural_analysis_module_report.py",
    "docs/api/_assets/brand.css",
]

# [A] Synthetic demonstration inputs specified for this report, not measured data.
INPUTS: dict[str, dict[str, float]] = {
    "beam": {"width_m": 0.2, "height_m": 0.4, "axial_force_n": 800000,
             "moment_y_nm": 16000, "moment_z_nm": 8000, "y_m": 0.1, "z_m": 0.2},
    "column": {"youngs_modulus_mpa": 200000, "i_mm4": 1000000,
               "effective_length_mm": 2000},
    "unused_material": {"yield_strength_mpa": 250, "ultimate_strength_mpa": 400,
                        "poissons_ratio": 0.3, "density_kg_m3": 7850},
}


def method_inventory() -> list[dict[str, str]]:
    """Retain the inspected public mechanics methods and unresolved evidence."""
    groups = [
        ("StressState", "models.py", [
            ("von_mises", "MPa → MPa", "Equivalent stress only; yield criterion and material basis require qualification."),
            ("principal_stresses", "MPa → MPa", "Symmetric tensor eigenvalues; independent tensor cases remain outside this worked basis."),
            ("max_shear", "MPa → MPa", "Half principal range; independent tensor cases remain outside this worked basis."),
        ]),
        ("StressCalculator", "stress_calculator.py", [
            ("beam_stress", "N, N m, m², m⁴, m → MPa", "Linear centroidal principal-axis beam theory; prescribed moment signs and two corners only; no local stress or stability check."),
            ("shear_stress", "N, m³, m⁴, m → MPa", "VQ/(It); first moment, shear path and applicable section require independent evidence."),
            ("torsional_stress", "N m, m, m⁴ → MPa", "Tr/J requires circular elastic torsion; arbitrary noncircular and warping torsion are not established."),
            ("hoop_stress", "MPa, m, m → MPa", "Thin-cylinder membrane expression; thinness, geometry and pressure basis require confirmation."),
            ("longitudinal_stress", "MPa, m, m → MPa", "Closed-end thin-cylinder membrane expression; end restraint and wall assumptions require confirmation."),
        ]),
        ("PlateBucklingAnalyzer", "buckling.py", [
            ("elastic_buckling_stress", "MPa, mm, dimensionless → MPa", "Only simply_supported is supported; aspect-ratio treatment is approximate, not a general finite-plate modal search."),
            ("reduced_slenderness", "MPa, mm → 1", "Inherits plate elastic approximation and material yield input; code basis remains unqualified."),
            ("johnson_ostenfeld", "MPa → MPa", "Inelastic interpolation, thresholds and applicability require a verified source and independent example."),
            ("check_plate_buckling", "MPa, mm, dimensionless → MPa, 1", "Nonzero sigma_y is rejected; compression/shear interaction and shear correction require qualification; reversed compression signs are not screened."),
        ]),
        ("ColumnBucklingAnalyzer", "buckling.py", [
            ("euler_buckling_load", "MPa, mm⁴, mm → N", "Ideal straight elastic pin-ended column; prescribed effective length; imperfections, yielding and code curves are excluded."),
            ("slenderness_ratio", "mm, mm → 1", "Effective length and gyration radius must be positive and independently established; this ratio is not worked here."),
            ("reduction_factor", "1 → 1", "An unknown curve silently falls back to the default curve; imperfection factors and branch behavior need source qualification."),
            ("check_column_buckling", "N, mm², mm⁴, mm, MPa → MPa, 1", "Inherits reduction-factor assumptions; effective length, curve selection and material factors require design evidence."),
        ]),
        ("MemberCapacityChecker", "capacity.py", [
            ("check_tension_member", "N, mm², MPa, dimensionless → N, 1", "Gross-yield/net-section model; coefficients, factors, connection geometry and material strengths require source qualification."),
            ("check_combined_loading", "N, N mm, mm², mm³, MPa → N, 1", "Existing simplified linear interaction; signed moments can cancel, tension is omitted, and supplied critical loads are not independently qualified."),
        ]),
    ]
    worked = {"StressCalculator.beam_stress", "ColumnBucklingAnalyzer.euler_buckling_load"}
    return [{
        "method": f"{group}.{name}", "units": units, "limits": limits,
        "source": f"src/digitalmodel/structural/structural_analysis/{source}",
        "qualification": "IDEAL_MECHANICS_CHECKED" if f"{group}.{name}" in worked else "NOT_EVALUATED",
    } for group, source, methods in groups for name, units, limits in methods]


def calculate(scratch: str | Path) -> dict[str, Any]:
    """Execute the two synthetic cases using the public module functions."""
    scratch = Path(scratch).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    # There are no disk intermediates; validation precedes all output writes.
    beam, column, unused = INPUTS["beam"], INPUTS["column"], INPUTS["unused_material"]
    material = MaterialProperties(
        unused["yield_strength_mpa"], unused["ultimate_strength_mpa"],
        column["youngs_modulus_mpa"], unused["poissons_ratio"],
        unused["density_kg_m3"], "Synthetic",
    )
    stress = StressCalculator(material)
    width, height = beam["width_m"], beam["height_m"]
    # [B] Integrating 1, z² and y² over the rectangle gives A, Iy and Iz.
    area, iy, iz = width * height, width * height**3 / 12, height * width**3 / 12
    geometry = (area, iy, iz, beam["y_m"], beam["z_m"])
    axial, my, mz = beam["axial_force_n"], beam["moment_y_nm"], beam["moment_z_nm"]
    beam_results = {
        "area_m2": area, "i_y_m4": iy, "i_z_m4": iz,
        "axial_mpa": stress.beam_stress(axial, 0, 0, *geometry),
        "bending_y_mpa": stress.beam_stress(0, my, 0, *geometry),
        "bending_z_mpa": stress.beam_stress(0, 0, mz, *geometry),
        "positive_corner_mpa": stress.beam_stress(axial, my, mz, *geometry),
        "negative_corner_mpa": stress.beam_stress(
            axial, my, mz, area, iy, iz, -beam["y_m"], -beam["z_m"],
        ),
    }
    analyzer = ColumnBucklingAnalyzer(material)
    load = analyzer.euler_buckling_load(column["i_mm4"], column["effective_length_mm"])
    # [C] The comparison case doubles the prescribed effective length.
    double_load = analyzer.euler_buckling_load(
        column["i_mm4"], 2 * column["effective_length_mm"],
    )
    return {
        "basis": "SYNTHETIC", "criteria_revision": "A", "inputs": INPUTS,
        "beam": beam_results,
        "column": {"load_n": load, "double_length_load_n": double_load,
                   "length_load_ratio": double_load / load},
        "units": {
            "beam": {"area_m2": "m²", "i_y_m4": "m⁴", "i_z_m4": "m⁴",
                     **{key: "MPa" for key in beam_results if key.endswith("mpa")}},
            "column": {"load_n": "N", "double_length_load_n": "N", "length_load_ratio": "1"},
        },
        "methods": method_inventory(),
    }


def json_text(data: object) -> str:
    """Encode deterministic UTF-8 JSON; nonfinite values are rejected."""
    return json.dumps(data, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n"


def result_rows(data: dict[str, Any]) -> str:
    """Format every visible worked value from the calculated payload."""
    labels = {
        "area_m2": "Rectangle area", "i_y_m4": "Second moment about y",
        "i_z_m4": "Second moment about z", "axial_mpa": "Axial stress term",
        "bending_y_mpa": "Bending stress term about y", "bending_z_mpa": "Bending stress term about z",
        "positive_corner_mpa": "Stress at (+y, +z)", "negative_corner_mpa": "Stress at (−y, −z)",
        "load_n": "Ideal pin-ended critical load", "double_length_load_n": "Critical load at doubled length",
        "length_load_ratio": "Doubled-length load / original load",
    }
    rows = []
    for group in ("beam", "column"):
        for key, value in data[group].items():
            unit = data["units"][group][key]
            if key.startswith("i_"):
                display = f"{value:.1e}"
            elif key.endswith("ratio"):
                display, unit = f"{value * 100:.1f}", "%"  # [D] Percent definition.
            elif key.endswith("mpa") or key.endswith("_n"):
                display = f"{value:.1f}"
            else:
                display = f"{value:.2f}"
            source = "[A, B, E]" if group == "beam" else "[A, C, F]"
            rows.append(f'<tr><td>{labels[key]}</td><td data-value="{group}.{key}">{display}</td>'
                        f'<td>{unit}</td><td>{source}</td><td>IDEAL_MECHANICS_CHECKED</td></tr>')
    return "\n".join(rows)


def input_rows(data: dict[str, Any]) -> str:
    """Present the synthetic design data with units and source beside each row."""
    units = {
        "width_m": "m", "height_m": "m", "axial_force_n": "N", "moment_y_nm": "N m",
        "moment_z_nm": "N m", "y_m": "m", "z_m": "m", "youngs_modulus_mpa": "N/mm² (MPa)",
        "i_mm4": "mm⁴", "effective_length_mm": "mm", "yield_strength_mpa": "MPa",
        "ultimate_strength_mpa": "MPa", "poissons_ratio": "1", "density_kg_m3": "kg/m³",
    }
    return "\n".join(
        f'<tr><td>{group}: {name}</td><td>{value:g}</td><td>{units[name]}</td><td>[A] SYNTHETIC</td></tr>'
        for group, values in data["inputs"].items() for name, value in values.items()
    )


def render(scratch: str | Path) -> str:
    """Render the ten-section module analysis around the public calculations."""
    data = calculate(scratch)
    inventory = "\n".join(
        '<tr>' + ''.join(f'<td>{cell}</td>' for cell in (
            f'<code>{row["method"]}</code>', html.escape(row["units"]),
            row["qualification"], html.escape(row["limits"]), html.escape(row["source"]),
        )) + '</tr>' for row in data["methods"]
    )
    provenance = "\n".join(
        f'<li><code>{source}</code>; SHA-256: {hashlib.sha256((ROOT / source).read_bytes()).hexdigest()}</li>'
        for source in SOURCES
    )
    history = "".join(f'<tr><td>{row["revision"]}</td><td>{row["date"]}</td>'
                      f'<td>{html.escape(row["change"])}</td></tr>' for row in CHANGES)
    payload = json_text(data).replace("<", "\\u003c")
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    page = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Structural analysis module analysis</title><style>{brand}
body{{margin:0;background:var(--bg,#f5f7fa);color:var(--ink,#142838);font-family:system-ui,sans-serif;line-height:1.6}}
main{{max-width:1100px;margin:auto;padding:2rem}}h1,h2,h3{{line-height:1.25}}h2{{margin-top:2.5rem}}
table{{width:100%;border-collapse:collapse;font-size:.9rem}}td,th{{padding:.6rem;text-align:left;vertical-align:top;border-bottom:1px solid var(--edge)}}
code{{overflow-wrap:anywhere}}.table-wrap{{overflow:auto}}.equation,details{{padding:1rem;background:var(--panel-2);margin:1rem 0}}
.status{{border-left:4px solid var(--brand);padding-left:1rem}}caption,figcaption{{caption-side:bottom;text-align:left;padding:.6rem 0}}
@media print{{main{{padding:0}}table{{font-size:.75rem}}h2{{break-after:avoid}}tr{{break-inside:avoid}}}}
</style></head><body class="hs-doc"><main>
<header><p>Engineering mechanics · Synthetic worked basis · Revision {CHANGES[-1]["revision"]}</p>
<h1>Structural analysis module analysis</h1><p>Buckling, capacity and stress routines: units, worked mechanics and limits of design use.</p></header>
<h2 id="s1">1. Executive Summary</h2>
<p>The synthetic rectangular beam and ideal pin-ended column are calculated through the public module functions. The beam stress is compared with section equilibrium and integrated section properties; the column load is compared with the sine-mode eigenvalue [A–C]. The calculated values and units are presented in the Detailed Results table.</p>
<p>The beam stress is {data["beam"]["positive_corner_mpa"]:.1f} MPa at (+y, +z) and {data["beam"]["negative_corner_mpa"]:.1f} MPa at (−y, −z). The separate ideal pin-ended critical load is {data["column"]["load_n"]:.1f} N [A–C, E, F]. These values reproduce the independent mechanics expressions under the stated roundoff criterion [I]; design acceptance remains NOT_EVALUATED.</p>
<p class="status">IDEAL_MECHANICS_CHECKED applies only to the two worked expressions and their stated cases. All source-unqualified code capacities are NOT_EVALUATED for design use. No standard buckling curve, reduction factor or combined-capacity rule is qualified by these examples [E–H].</p>
<h3>Revision history</h3><table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead><tbody>{history}</tbody>
<caption>Revision history for the issued synthetic module analysis.</caption></table>
<h2 id="s2">2. Introduction</h2>
<p>The review covers the public stress-state, stress-calculator, plate-buckling, column-buckling and member-capacity methods listed below [E–H]. Command-line wrappers, corrosion/metal-loss extensions, hull material tables and licensed solver workflows are outside the review. No solver is imported or executed by this builder.</p>
<p>The result is a reproducible software mechanics example. Existing package statements of production readiness or code compliance do not establish the independent design qualification missing here.</p>
<h2 id="s3">3. Design Data and Basis</h2>
<p>All dimensional values and material fields below are SYNTHETIC demonstration inputs [A]. They do not represent a physical component or a certified material. Only the elastic modulus enters the column example; no material property enters the beam stress calculation. Yield strength, ultimate strength, Poisson ratio and density are unused constructor fields.</p>
<div class="table-wrap"><table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source and qualification</th></tr></thead><tbody>{input_rows(data)}</tbody>
<caption>Synthetic design data; unused material fields are identified explicitly [A].</caption></table></div>
<p>The beam width lies along y and its height along z. Coordinates are centroidal principal axes. Positive axial force and the supplied positive moment contributions produce positive normal stress under the implemented algebraic convention. The opposite corner reverses both bending terms without reversing the axial term [B, E]. This is distinct from the compression-positive convention of the buckling and combined-loading APIs.</p>
<p>The column is straight, prismatic, elastic and ideally pin ended, without initial eccentricity or imperfection. Effective length is prescribed rather than calculated from a frame. Its synthetic section is separate from the beam rectangle [A, C].</p>
<h2 id="s4">4. Analysis Methodology</h2>
<h3>Independent beam basis</h3>
<p>For width b and height h, integration over −b/2 ≤ y ≤ b/2 and −h/2 ≤ z ≤ h/2 gives the area and second moments [B]:</p>
<div class="equation">A = ∫∫ dA = bh; I_y = ∫∫ z² dA = bh³/12; I_z = ∫∫ y² dA = hb³/12 [B].</div>
<p>The denominator follows from ∫z² dz = z³/3 evaluated at the symmetric bounds, with the remaining width integration. Normal-force and moment equilibrium then give the stress field in the module's signed convention [B, E]:</p>
<div class="equation">σ(y,z) = N/A + M_y z/I_y + M_z y/I_z [B].</div>
<p>SI geometry in metres with forces in newtons and moments in newton metres produces pascals, converted to MPa by division by 10⁶ [D]. An independent millimetre calculation uses N mm, mm² and mm⁴ and gives N/mm² = MPa. One metre equals 10³ millimetres; area and second moments therefore convert with the second and fourth powers, respectively [D]. Each term is isolated through a public beam call before the two corner calls [E, I].</p>
<h3>Independent column basis</h3>
<p>For a constant flexural rigidity EI, equilibrium gives the fourth-order displacement equation. A sine mode satisfies both zero displacement and zero bending moment at the pin ends [C]:</p>
<div class="equation">EI v'''' + P v'' = 0; v = sin(πx/L); (EI π⁴/L⁴ − P π²/L²)v = 0; P = π² EI/L² [C].</div>
<p>Substitution of the synthetic column inputs reduces EI/L² to 50000 N, hence P = 50000π² N. Doubling L multiplies its squared denominator by four and reduces P to one quarter [A, C]. The public column API uses elastic modulus in N/mm², second moment in mm⁴ and effective length in mm; it returns N [F]. This is an ideal elastic eigenvalue, not a design resistance.</p>
<p>The independent assertions allow 32 binary64 machine epsilons in relative error [I]. This allowance bounds roundoff in the small number of products, divisions and π evaluations; it is not an allowance for model error, geometry uncertainty or design-code deviation. Relative error is used for these nonzero references. No numerical tolerance is established for design qualification.</p>
<h3>Public-method inventory</h3><div class="table-wrap"><table><thead><tr><th>Method</th><th>Input → output units</th><th>Qualification in this report</th><th>Limits and evidence gap</th><th>Source</th></tr></thead><tbody>{inventory}</tbody>
<caption>Reviewed public mechanics methods. Dimensionless quantities use unit 1. NOT_EVALUATED means no independent qualification is established by this worked basis [E–I].</caption></table></div>
<h2 id="s5">5. Detailed Results</h2>
<p>Every displayed value below is calculated from the committed synthetic inputs. Stress terms and column loads are returned by public functions; section properties follow the independent rectangle integrals [A–C, E, F]. Scientific notation is used for second moments; the length ratio is displayed as a percentage. Unrounded values are retained in the embedded payload and used in assertions [I].</p>
<div class="table-wrap"><table><thead><tr><th>Quantity and governing case</th><th>Value</th><th>Unit</th><th>Source</th><th>Qualification</th></tr></thead><tbody>{result_rows(data)}</tbody>
<caption>Worked numbers from public calculations with independent equilibrium and eigenfunction checks [A–C, I].</caption></table></div>
<p>The beam has a uniform axial term and equal bending contributions at the positive corner. The opposite corner retains positive stress because the axial term exceeds the sum of the reversed bending terms [B]. No allowable stress or design utilization is calculated. The column load is a bifurcation eigenvalue for the ideal case; it is not compared with a factored design load [C].</p>
<h2 id="s6">6. Key Findings</h2>
<p>The worked functions reproduce the independent mechanics values within the roundoff-only criterion for the synthetic cases [I]. This establishes unit conversion and sign behavior at the stated points, plus inverse-square effective-length scaling. It does not establish general member design conformity.</p>
<p>Review found that transverse plate stress sigma_y was accepted but omitted from utilization. The method now raises ValueError for nonzero sigma_y, and the regression demonstrates rejection before any utilization is returned [F, I]. This rejection also propagates through panel, grillage and parametric-sweep callers that pass nonzero transverse stress. The former biaxial grillage and default-sweep tests are changed to require rejection because their returned results omitted transverse plate interaction. The default sweep contains such unsupported cases and now raises; a supported load set or a qualified biaxial model is required before that sweep can complete. Existing zero-transverse-stress behavior is retained.</p>
<h2 id="s7">7. Limitations and Not Covered</h2>
<p>All code-derived plate resistance, column reduction and member capacities remain NOT_EVALUATED. Source editions, clause applicability, factors, section class, imperfections, residual stress, effective lengths and independent published capacity examples are absent. No numerical value from a licensed publication is reproduced or inferred.</p>
<p>The plate elastic aspect-ratio expression is approximate and does not search longitudinal mode numbers. The combined plate calculation supports no nonzero transverse stress. The column reduction routine accepts an unknown curve by silently selecting its default. The existing simplified linear combined-capacity formula can cancel signed moment contributions and omits axial tension from interaction [F, G]. These unresolved methods are inventoried, not qualified or silently corrected.</p>
<p>Domain validation is incomplete: nonpositive geometry, nonfinite inputs and invalid material or factor values are not comprehensively rejected. Zero or negative denominators may raise arithmetic errors or produce meaningless software dispositions [E–H]. The worked inputs avoid these domains; design acceptance cannot rely on their current handling.</p>
<h2 id="s8">8. Recommendations</h2>
<p>The two worked cases may be used as reproducible software regression examples under the stated ideal assumptions. Before design use, the intended standard edition, clause titles, material certificates, section geometry, restraint/effective-length basis, load combinations and factor selection should be supplied and independently reviewed.</p>
<p>Plate modal behavior and transverse interaction require a defined supported model before implementation. Unknown column curves and signed member interactions require explicit API contracts and independent failing examples before correction. Capacity comparisons should then use an independently published worked case with its governing criteria. The tracking issue remains open while these requested methods remain unqualified.</p>
<h2 id="s9">9. References and Appendices</h2>
<p>[A] The INPUTS mapping in the committed builder is the authoritative synthetic input record. Dataset identity: structural-analysis-synthetic; revision A; computational owner: digitalmodel. Readiness is limited to the ideal software examples; physical design readiness is unavailable. Source and artifact SHA-256 digests are retained in manifest.json. A changed input, implementation or criterion requires a rebuild and renewed review.</p>
<p>[B] Independent rectangle and beam derivation in this report: direct integration of squared distances and axial/moment equilibrium. Geometry and force values are synthetic [A]; coefficients follow the shown integrals rather than a design standard.</p>
<p>[C] Independent column derivation in this report: the homogeneous elastic beam-column differential equation and its pin-ended sine eigenfunction. The length-doubling comparison follows algebraically.</p>
<p>[D] SI prefix definitions: mega denotes 10⁶ and milli denotes 10⁻³. A pascal is N/m²; consequently N/mm² is MPa. Percent denotes one hundredth. These are unit identities, not empirical or code factors.</p>
<p>[E] stress_calculator.py: StressCalculator; [F] buckling.py: PlateBucklingAnalyzer and ColumnBucklingAnalyzer; [G] capacity.py: MemberCapacityChecker; [H] models.py: StressState and geometry/material dataclasses; [I] the two hand-value/report test files and the existing buckling/capacity and structural unit tests. The exact repository sources are pinned by digest below. Standard labels in source code are identifiers, not independently verified citations or qualification evidence.</p>
<details><summary>Exact source-byte register</summary><ul>{provenance}</ul></details>
<p>Regenerate with <code>PYTHONPATH=src python scripts/reports/build_structural_analysis_module_report.py --scratch "$TMPDIR/structural-analysis"</code>. NumPy is required. Scratch must resolve outside the checkout; calculations are in memory and no intermediates are written. Use <code>--output-dir</code> for an independent delivery directory. The manifest hashes the three other deliverables and all listed sources; its own digest is intentionally excluded to avoid recursive hashing.</p>
<details id="changes-panel"><summary>Engineering changes</summary><p>{html.escape(CHANGES[-1]["change"])}</p></details>
<h2 id="s10">10. Acronyms and Review Notes</h2>
<p>SI: International System of Units. MPa: megapascal. API: application programming interface. E: elastic modulus; I: second moment of area; L: effective length; P: axial bifurcation load; N: applied axial force; M: bending moment. Unit 1 denotes a dimensionless value.</p>
<p>Review status: independent design-code qualification is NOT_EVALUATED. Numerical assertions, deterministic rebuilds and digest checks establish only the explicitly tested software behavior. The way forward is the source and design-evidence assessment specified above.</p>
</main><script id="calculation" type="application/json">{payload}</script></body></html>
"""
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


def readme() -> str:
    """Supply the one-command rebuild and explicit qualification boundary."""
    return """# Structural analysis module analysis

Open [index.html](index.html) for the ten-section synthetic beam and ideal-column
analysis, method inventory, unit conventions, independent derivations and limits.

From the repository root, with NumPy available and TMPDIR outside the checkout:

```sh
PYTHONPATH=src python scripts/reports/build_structural_analysis_module_report.py --scratch "$TMPDIR/structural-analysis"
```

Use `--output-dir <directory>` to reproduce the four deliverables elsewhere.
No solver is used. Calculations are in memory; no intermediate files are written.
The builder rejects scratch inside the checkout before writing output.

The synthetic input basis and qualification criteria are revision A, dated
2026-10-28. `changes.json` records the revision cause. `manifest.json` binds source
bytes and the other three artifacts by SHA-256; it excludes its own recursive
digest. Two independent builds are compared with the committed deliverables by
`tests/reporting/test_structural_analysis_module_report.py`.

IDEAL_MECHANICS_CHECKED covers the worked beam stress and ideal elastic column
only. Design-code capacities, reduction curves and simplified combined loading
remain NOT_EVALUATED. The issue remains open pending independent source and
design qualification. See the report for the missing evidence and way forward.
"""


def build(scratch: str | Path, output_dir: str | Path = DESTINATION) -> None:
    """Write exactly the named deliverables and read bytes back for hashing."""
    page = render(scratch)
    output_dir = Path(output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    outputs = {"index.html": page, "changes.json": json_text(CHANGES), "README.md": readme()}
    for name, text in outputs.items():
        (output_dir / name).write_text(text, encoding="utf-8", newline="\n")
    manifest = {
        "schema_version": 1, "basis": "SYNTHETIC", "criteria_revision": "A",
        "dataset_id": "structural-analysis-synthetic", "owner_repo": "digitalmodel",
        "digest_convention": "SHA-256 of exact UTF-8 LF bytes; manifest self-digest excluded.",
        "sources": {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in SOURCES},
        "artifacts": {name: hashlib.sha256((output_dir / name).read_bytes()).hexdigest() for name in outputs},
    }
    (output_dir / "manifest.json").write_text(json_text(manifest), encoding="utf-8", newline="\n")


def main() -> None:
    """Parse the bounded report output and scratch options."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    build(args.scratch, args.output_dir)


if __name__ == "__main__":
    main()
