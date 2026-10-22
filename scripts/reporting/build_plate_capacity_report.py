# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the plate-capacity analysis; example units are N, mm and MPa.

Reproduce with PYTHONPATH=src uv run --no-project --with loguru --with numpy
--with scipy --with pydantic --with pyyaml python
scripts/reporting/build_plate_capacity_report.py. Dependencies are required by
existing parent-package imports; the plate and bar arithmetic uses math only.
Use --output with an external scratch directory for a comparison build.
"""

from __future__ import annotations

import argparse
import html
import json
from math import sqrt
from pathlib import Path

from digitalmodel.solvers.fea_model.fea_model import fea_model
from digitalmodel.structural.plate_capacity.elastic_buckling import (
    plate_buckling_coefficient,
    plate_elastic_buckling_stress,
)

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/plate_capacity"
DATE = "2026-10-22"
SOURCE_ROOT = (
    "https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/resources/"
)


def calculate() -> dict:
    """Compute synthetic plate (MPa, mm) and bar (N, mm) examples and samples."""
    # Synthetic arithmetic inputs, not published material properties or limits.
    inputs = dict(
        youngs_modulus=120000.0,
        poisson_ratio=0.3,
        thickness=2.0,
        width=200.0,
        length=400.0,
    )
    applied_stress, yield_stress = 20.0, 250.0  # MPa; required example inputs.
    critical = plate_elastic_buckling_stress(**inputs)
    plate = {
        "coefficient": plate_buckling_coefficient(
            length=inputs["length"], width=inputs["width"]
        ),
        "critical_stress": critical,
        "geometric_slenderness": inputs["width"] / inputs["thickness"],
        # Definitions for this elastic screen, not a code resistance curve.
        "normalised_slenderness": sqrt(yield_stress / critical),
        "utilisation": applied_stress / critical,
    }
    first = dict(youngs_modulus=200000.0, area=100.0, length=1000.0)
    second = dict(first, length=2000.0)
    load = 10000.0  # N; synthetic end load.
    bar = {}
    for name, elements in (("one", [first]), ("two", [first, second])):
        bar[name] = fea_model({"axial_bar": {"elements": elements, "tip_load": load}})[
            "axial_bar"
        ]["result"]
    # Sample spacing is a plotting choice, not an engineering coefficient.
    samples = []
    for step in range(5, 31):
        ratio = step / 10
        case = dict(inputs, length=inputs["width"] * ratio)
        samples.append(
            {
                "ratio": ratio,
                "coefficient": plate_buckling_coefficient(
                    length=case["length"], width=case["width"]
                ),
                "stress": plate_elastic_buckling_stress(**case),
            }
        )
    return {
        "inputs": inputs,
        "applied_stress": applied_stress,
        "yield_stress": yield_stress,
        "plate": plate,
        "bar_inputs": {"elements": [first, second], "tip_load": load},
        "bar": bar,
        "samples": samples,
    }


def _table(headers: list[str], rows: list[list[str]]) -> str:
    """Render escaped table cells; quantity units are supplied in the cells."""
    head = "".join(f'<th scope="col">{html.escape(s)}</th>' for s in headers)
    body = "".join(
        "<tr>" + "".join(f"<td>{html.escape(s)}</td>" for s in row) + "</tr>"
        for row in rows
    )
    return f'<div class="table"><table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>'


def _coefficient_figure(samples: list[dict]) -> str:
    """Render dimensionless a/b versus computed k; all labels use one decimal."""
    xmin, xmax = min(s["ratio"] for s in samples), max(s["ratio"] for s in samples)
    ymin, ymax = 0.0, max(s["coefficient"] for s in samples) * 1.1
    points, coordinates = [], []
    for sample in samples:
        x = 70 + (sample["ratio"] - xmin) / (xmax - xmin) * 590
        y = 290 - (sample["coefficient"] - ymin) / (ymax - ymin) * 230
        coordinates.append(f"{x:.1f},{y:.1f}")
        tip = f"a/b={sample['ratio']:.1f}; k={sample['coefficient']:.1f}; critical stress={sample['stress']:.1f} MPa"
        points.append(
            f'<circle cx="{x:.1f}" cy="{y:.1f}" r="4" tabindex="0" '
            f'data-ratio="{sample["ratio"]:.1f}" data-k="{sample["coefficient"]:.1f}" '
            f'aria-label="{tip}"><title>{tip}</title></circle>'
        )
    return f'''<figure><svg viewBox="0 0 730 355" role="img" aria-labelledby="coefficient-title">
<title id="coefficient-title">Rectangular plate minimum buckling coefficient</title>
<path d="M70 60 V290 H660" fill="none" stroke="currentColor"/>
<text x="70" y="30">Minimum k (dimensionless)</text>
<text x="300" y="340">Aspect ratio a/b (dimensionless)</text>
<text x="38" y="295">{ymin:.1f}</text><text x="38" y="65">{ymax:.1f}</text>
<text x="60" y="315">{xmin:.1f}</text><text x="650" y="315">{xmax:.1f}</text>
<polyline points="{" ".join(coordinates)}" fill="none" stroke="#167d96"/>
{"".join(points)}</svg><figcaption>Figure 1. Computed samples of the integer-mode minimum,
MIT (2013), Eq. 11.16. Hover or focus a point for its ratio, coefficient and stress.
Connecting segments guide the eye; they are not additional computed cases.</figcaption></figure>'''


def render(record: dict) -> str:
    """Render a standalone analysis with computed N/mm, mm and MPa quantities."""
    inp, plate, bars = record["inputs"], record["plate"], record["bar"]
    inputs = _table(
        ["Input", "Value", "Basis"],
        [
            [
                "Young's modulus E",
                f"{inp['youngs_modulus']:.1f} MPa",
                "Synthetic input",
            ],
            [
                "Poisson ratio nu",
                f"{inp['poisson_ratio']:.1f}",
                "Synthetic input; dimensionless",
            ],
            ["Thickness t", f"{inp['thickness']:.1f} mm", "Synthetic input"],
            ["Width b", f"{inp['width']:.1f} mm", "Transverse to compression"],
            ["Length a", f"{inp['length']:.1f} mm", "Parallel to compression"],
            [
                "Applied compressive stress",
                f"{record['applied_stress']:.1f} MPa",
                "Synthetic input; positive compression",
            ],
            [
                "Yield stress for slenderness definition",
                f"{record['yield_stress']:.1f} MPa",
                "Synthetic input; no code acceptance limit",
            ],
        ],
    )
    results = _table(
        ["Quantity", "Computed value"],
        [
            ["Minimum coefficient k", f"{plate['coefficient']:.1f}"],
            ["Elastic critical stress", f"{plate['critical_stress']:.1f} MPa"],
            ["Geometric slenderness b/t", f"{plate['geometric_slenderness']:.1f}"],
            [
                "Normalised slenderness sqrt(f_y/sigma_cr)",
                f"{plate['normalised_slenderness']:.1f}",
            ],
            [
                "Elastic utilisation applied/sigma_cr",
                f"{100 * plate['utilisation']:.1f} %",
            ],
        ],
    )
    bar_inputs = _table(
        ["Element", "E (MPa)", "A (mm²)", "L (mm)"],
        [
            [
                str(i),
                f"{e['youngs_modulus']:.1f}",
                f"{e['area']:.1f}",
                f"{e['length']:.1f}",
            ]
            for i, e in enumerate(record["bar_inputs"]["elements"], 1)
        ],
    )
    bar_results = _table(
        ["Fixture", "Equivalent stiffness", "Tip displacement"],
        [
            [
                name,
                f"{bar['equivalent_stiffness']:.1f} N/mm",
                f"{bar['displacements'][-1]:.1f} mm",
            ]
            for name, bar in bars.items()
        ],
    )
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Plate capacity module analysis</title>
<style>
:root {{ color-scheme:light dark; font-family:system-ui,sans-serif; line-height:1.6; }}
body {{ max-width:1080px; margin:auto; padding:2rem; }}
header {{ border-bottom:4px solid #167d96; }}
section {{ margin:2rem 0; }} nav a {{ margin-right:1rem; }}
.table {{ overflow-x:auto; }} table {{ border-collapse:collapse; width:100%; }}
th,td {{ padding:.7rem; text-align:left; border-bottom:1px solid #888; }}
svg {{ width:100%; height:auto; }} circle {{ fill:#167d96; }}
circle:focus {{ stroke:currentColor; stroke-width:3; }}
figure {{ margin:1rem 0; }} code {{ overflow-wrap:anywhere; }}
@media print {{ body {{ padding:0; }} section {{ break-inside:avoid; }} }}
</style></head><body>
<header><p>Structural calculations · Revision 0 · {DATE}</p><h1>Plate capacity module analysis</h1>
<p>Simply supported elastic plate buckling with an axial bar verification appendix.</p></header>
<section id="summary"><h2>Summary</h2>
<p>The synthetic plate has an elastic critical stress of {plate["critical_stress"]:.1f} MPa,
normalised slenderness {plate["normalised_slenderness"]:.1f} and elastic utilisation
{100 * plate["utilisation"]:.1f} %. These values describe the stated perfect-plate model;
they do not establish code resistance or acceptance. Confirm boundary conditions,
material properties, imperfections and a governing resistance method before design use.</p></section>
<section id="revision"><h2>Revision history</h2>
{_table(["Revision", "Date", "What changed and why"], [["0", DATE, "Initial analysis; add independently checked plate and bar arithmetic and state unevaluated checks."]])}</section>
<nav aria-label="Report sections"><a href="#inputs">Inputs</a><a href="#method">Method</a>
<a href="#worked">Worked example</a><a href="#limits">Limits</a><a href="#bar">Bar appendix</a>
<a href="#sources">Sources</a></nav>
<section id="inputs"><h2>Inputs</h2>
<p>All numerical inputs are synthetic arithmetic choices. They are not qualified steel
properties, measured geometry, or tabulated standard coefficients. Lengths are mm,
stresses and E are MPa (N/mm²), and Poisson ratio is dimensionless.</p>{inputs}</section>
<section id="method"><h2>Method and equation sources</h2>
<p>Four simply supported edges, uniform uniaxial compression along a and free transverse
in-plane expansion are assumed. MIT 2.080J (2013), Lecture 11, Section 11.2,
Eqs. 11.15–11.17 give N_cr=k*pi²*D/b² and
k=min over positive integers m of (m*b/a+a/(m*b))². The transverse mode has one half-wave.
The continuous minimum is at m=a/b; the two adjacent positive integers bracket the discrete minimum.
For a long plate k approaches 4.0. Lecture 4, Eq. 4.60 gives
D=E*t³/[12*(1-nu²)], hence sigma_cr=N_cr/t=k*pi²*E/[12*(1-nu²)]*(t/b)².</p>
<p>Geometric slenderness is defined here as b/t. Normalised slenderness is defined here as
sqrt(f_y/sigma_cr), a nondimensional comparison of input yield stress to the elastic critical stress.
Utilisation is defined here as applied compressive stress divided by sigma_cr.
Neither definition supplies a standard resistance curve, reduction factor or acceptance limit.</p></section>
<section id="worked"><h2>Worked example</h2>
<p>For the inputs above, a/b={inp["length"] / inp["width"]:.1f}; m=2 gives
k=(1+1)²={plate["coefficient"]:.1f}. Substituting the input values into the cited equation
gives the following computed answer. The test docstrings contain the independent hand arithmetic.</p>
{results}{_coefficient_figure(record["samples"])}</section>
<section id="limits"><h2>Limits of use and NOT_EVALUATED checks</h2>
<p>The plate is geometrically perfect, thin, homogeneous, isotropic and linearly elastic.
Positive finite E and dimensions and -1&lt;nu&lt;0.5 are enforced; these bounds follow from
positive elastic bulk and shear moduli in MIT Lecture 4, Eqs. 4.14 and 4.16.
Thinness and elastic applicability must be established independently.</p>
<ul><li>NOT_EVALUATED: DNV-RP-C201 design resistance, partial factors, effective widths,
interaction and imperfection reductions. An applicable licensed edition and clause basis
are not present in the calculations folder; no licensed values are reproduced.</li>
<li>NOT_EVALUATED: yielding, residual stress, corrosion, stiffeners, biaxial compression,
shear, lateral restraint and post-buckling reserve. Required models and evidence are absent.</li>
<li>NOT_EVALUATED: a physical test, external solver comparison or project acceptance.
The calculation is a mathematical elastic screen only.</li>
<li>NOT_EVALUATED: historical scenario scripts outside the calculations folder;
those scripts are not independent numerical references and are not imported.</li></ul>
<h3>Calculation inventory</h3><p>The plate_capacity/calculations folder contains only an empty
__init__.py: zero public functions, zero uncovered public calculations.
An AST inventory test will require an explicit assertion or NOT_EVALUATED entry when this changes.
The elastic_buckling functions outside that folder have independent hand-calculated value tests.
The superseded folder is excluded and unchanged.</p></section>
<section id="bar"><h2>Verification appendix: axial bar model</h2>
<p>The public fea_model entry accepts axial_bar with elements and a signed tip_load.
It assembles a one-dimensional chain fixed at the left node and loaded at the right node;
there are no interior applied loads. This analytical fixture does not start an external solver.
MIT Lecture 4, Eq. 4.25 gives N=EA*epsilon and Lecture 2, Eq. 2.5 gives epsilon=du/dx.
For a prismatic element these give k=EA/L, K_e=k[[1,-1],[-1,1]], and delta=P*L/(EA).
Two elements in series carry the same force, so k_eq=k1*k2/(k1+k2).</p>
{bar_inputs}<p>The one-element case uses the first element. Both cases use
P={record["bar_inputs"]["tip_load"]:.1f} N. Hand arithmetic gives
k1=E1*A1/L1 and k2=E2*A2/L2. The assembled matrix also satisfies nodal equilibrium.</p>
{bar_results}<p>NOT_EVALUATED: bending, bar buckling, plasticity, distributed loading and
external-program model export. Compression is permitted as a signed axial force and does
not constitute a geometric stability assessment.</p></section>
<section id="sources"><h2>Sources and reproduction</h2>
<ul><li><a href="{SOURCE_ROOT}mit2_080jf13_lecture11/">MIT 2.080J Structural Mechanics,
Fall 2013, Lecture 11</a>, Section 11.2, Eqs. 11.15–11.17: rectangular and long-plate buckling.</li>
<li><a href="{SOURCE_ROOT}mit2_080jf13_lecture4/">MIT 2.080J Structural Mechanics,
Fall 2013, Lecture 4</a>, Eqs. 4.14, 4.16, 4.25 and 4.60: elastic moduli, axial rigidity and plate rigidity.</li>
<li><a href="{SOURCE_ROOT}mit2_080jf13_lecture2/">MIT 2.080J Structural Mechanics,
Fall 2013, Lecture 2</a>, Eq. 2.5: small axial strain.</li></ul>
<p>The open course PDFs were read for the equations; no licensed standard values are used.</p>
<p>Rebuild from the repository root with one command:</p>
<pre><code>PYTHONPATH=src uv run --no-project --with loguru --with numpy --with scipy --with pydantic --with pyyaml python scripts/reporting/build_plate_capacity_report.py</code></pre>
<p>Append <code>--output &lt;scratch-directory&gt;</code> to compare a scratch build.
The builder regenerates this standalone HTML, its computed SVG figure and changes.json.
The rebuild test compares both deliverables byte for byte. Optional dependencies above
come from existing parent-package imports. No network request or external solver is used by the builder.</p>
</section></body></html>
'''


def build(output: Path = OUTPUT) -> None:
    """Write deterministic HTML and revision metadata; computed units N/mm, mm, MPa."""
    record = calculate()
    output.mkdir(parents=True, exist_ok=True)
    (output / "index.html").write_text(render(record), encoding="utf-8")
    changes = {
        "document": "plate-capacity-module-analysis",
        "revision": 0,
        "date": DATE,
        "changes": [
            {
                "section": "plate",
                "description": "Add sourced elastic rectangular plate arithmetic and worked inputs.",
            },
            {
                "section": "bar",
                "description": "Add one- and two-element axial verification without an external solver.",
            },
            {
                "section": "limits",
                "description": "Inventory calculations and distinguish elastic screening from code resistance.",
            },
        ],
    }
    (output / "changes.json").write_text(
        json.dumps(changes, indent=2) + "\n", encoding="utf-8"
    )


def main() -> None:
    """Parse an output folder and build the N/mm, mm and MPa worked report."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    args = parser.parse_args()
    build(args.output)


if __name__ == "__main__":
    main()
