# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a synthetic touchdown-catenary geometry example in a selected directory.

Basis: tests/subsea/mooring_analysis/test_catenary_geometry.py,
test_vertical_span_closed_form, case A (explicit synthetic inputs).
Mechanics: General Physics I: Classical Mechanics, section 19.3, The Catenary
(2024 web edition), https://phys.libretexts.org/@go/page/92208.
"""

from __future__ import annotations

import argparse
from html import escape
from math import log, sqrt
from pathlib import Path

from digitalmodel.subsea.mooring_analysis.catenary import CatenaryAnalyzer
from digitalmodel.subsea.mooring_analysis.models import LineType, MooringLineProperties


SOURCE = "https://phys.libretexts.org/@go/page/92208"


def row(identifier: str, label: str, value: str, unit: str, basis: str) -> str:
    """Render visible values and units with stable identifiers for readback."""
    return (
        f'<tr id="{escape(identifier)}"><th scope="row">{escape(label)}</th>'
        f'<td class="value">{escape(value)}</td><td class="unit">{escape(unit)}</td>'
        f'<td>{escape(basis)}</td></tr>'
    )


def render_example() -> str:
    """Call the public solver with explicit inputs and render a standalone page."""
    # Synthetic case A, cited above. Placeholder material properties are not
    # used by solve_catenary; they are retained explicitly, not read from tests.
    line = MooringLineProperties(
        line_type=LineType.CHAIN, length=500.0, diameter=1.0,
        mbl=1.0, weight_water=1000.0, ea=1.0,
    )
    height = 100.0
    horizontal_tension = 981.0
    analyzer = CatenaryAnalyzer(water_depth=height)
    result = analyzer.solve_catenary(
        line, horizontal_tension=horizontal_tension, vertical_height=height,
    )

    # Case-A independent hand calculation from section 19.3, rather than
    # recycled solver output: a=z=100, cosh(x/a)=2, T=H+wz.
    expected = (
        ("suspended-length", "Suspended length, s", result.arc_length,
         100.0 * sqrt(3.0), "m", "100√3 m"),
        ("horizontal-reach", "Horizontal reach, x", result.horizontal_distance,
         100.0 * log(2.0 + sqrt(3.0)), "m", "100 ln(2 + √3) m"),
        ("fairlead-tension", "Fairlead tension, T", result.fairlead_tension,
         981.0 + 9.81 * 100.0, "kN", "981 + 9.81 × 100 kN"),
    )
    inputs = "\n".join([
        row("height", "Vertical span, z (also water depth)", f"{height:.1f}", "m", "Synthetic case A"),
        row("line-length", "Total line length, L", f"{line.length:.1f}", "m", "Synthetic case A"),
        row("weight-water", "Submerged mass per length, weight_water",
            f"{line.weight_water:.1f}", "kg/m", "Synthetic case A; API mass convention"),
        row("horizontal-tension", "Horizontal tension, H", f"{horizontal_tension:.1f}", "kN", "Synthetic case A"),
        row("gravity", "Gravity, g", f"{analyzer.g:.2f}", "m/s²", "CatenaryAnalyzer gravity convention"),
        row("diameter", "Diameter (unused)", f"{line.diameter:.1f}", "mm", "Synthetic placeholder"),
        row("mbl", "Breaking load (unused)", f"{line.mbl:.1f}", "kN", "Synthetic placeholder; no acceptance check"),
        row("ea", "Axial stiffness, EA (unused)", f"{line.ea:.1f}", "kN", "Synthetic placeholder; inextensible model"),
    ])
    results = "\n".join(
        row(key, label, f"{actual:.1f}", unit, formula)
        for key, label, actual, _, unit, formula in expected
    )
    comparisons = "\n".join(
        f"<tr><th scope=\"row\">{escape(label)}</th><td>{hand:.1f}</td>"
        f"<td>{escape(unit)}</td><td>{abs(actual - hand) / abs(hand) * 100:.1f} %</td></tr>"
        for _, label, actual, hand, unit, _ in expected
    )
    return f"""<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8" /><title>Touchdown catenary geometry worked example</title></head>
<body>
<h1>Touchdown catenary geometry worked example</h1>
<h2>Summary</h2>
<p>This synthetic example calls <code>CatenaryAnalyzer.solve_catenary</code>
for a uniform inextensible line with positive weight and a horizontal touchdown tangent.
It demonstrates geometry and tension only. Design qualification: <strong>NOT_EVALUATED</strong>.</p>
<h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-25</td><td>Add a reproducible synthetic geometry example
with independent hand calculations to expose the public solver's inputs and answer.</td></tr></tbody></table>
<h2>Inputs and basis</h2>
<p>The inputs are explicit synthetic case A from
<code>tests/subsea/mooring_analysis/test_catenary_geometry.py::test_vertical_span_closed_form</code>.
They are transcribed here; no test fixture is imported. They are not material or field data.
Line type is chain; diameter, breaking load and EA are arbitrary placeholders unused by this call.
The API's <code>weight_water</code> is in kg/m; multiplying by gravity and converting N to kN
gives the positive distributed load w in kN/m.</p>
<table><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>{inputs}</tbody></table>
<h2>Method and independent hand calculation</h2>
<p>Source: <a href="{SOURCE}">General Physics I: Classical Mechanics,
section 19.3, The Catenary (2024 web edition)</a>. Its identities are
z = a(cosh(x/a) − 1), s = a sinh(x/a), and a = H/w.
Here z is the vertical span above touchdown and x is the horizontal reach from touchdown.</p>
<p>With the API gravity convention, w = 1000 × 9.81 / 1000 = 9.81 kN/m
and a = 981 / 9.81 = 100 m. The divisor converts N to kN.</p>
<p>Eliminate x using cosh²(u) − sinh²(u) = 1:
s² = a²[(1 + z/a)² − 1], hence s² = z² + 2az.
For z = a = 100 m, s = 100√3 m.
Also cosh(x/a) = 2, so x = 100 arcosh(2) = 100 ln(2 + √3) m.</p>
<p>Force balance gives vertical tension V = ws and
T² = H² + (ws)² = (H + wz)². For positive H and w,
T = H + wz = 981 + 9.81 × 100 = 1962 kN.</p>
<table><thead><tr><th>Derived quantity</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>
{row("unit-weight", "Distributed load, w", f"{line.weight_water * analyzer.g / 1000:.2f}", "kN/m", "weight_water × g / 1000; N to kN conversion")}
{row("parameter", "Catenary parameter, a", f"{result.catenary_parameter:.1f}", "m", "H / w")}
</tbody></table>
<h2>Results and readback</h2>
<p>These values come from the public solver. Results are displayed to one decimal place.</p>
<table><thead><tr><th>Result</th><th>Value</th><th>Unit</th><th>Independent expression</th></tr></thead>
<tbody>{results}</tbody></table>
<p>The hand values below use the independent expressions above. Relative differences are
computed before display rounding; they qualify only this synthetic example.</p>
<table><thead><tr><th>Quantity</th><th>Hand value</th><th>Unit</th><th>Relative difference</th></tr></thead>
<tbody>{comparisons}</tbody></table>
<h2>Assumptions and limits</h2>
<p>Required assumptions: uniform inextensible positive weight, horizontal touchdown tangent,
and sufficient line length. Here L = 500 m exceeds s = 100√3 m, so grounded length is
{line.length - result.arc_length:.1f} m. This is a touchdown-to-fairlead span,
not a general two-suspended-endpoint solution.</p>
<p><strong>NOT_EVALUATED</strong>: stiffness, design-factor acceptance, model export,
environmental loading, dynamics and the separate historical port. The placeholder breaking
load cannot support a design acceptance conclusion. This example does not qualify the full
module or complete the parent report issue.</p>
<h2>Rebuild</h2>
<p>From the repository root, with Python dependencies installed:</p>
<pre>PYTHONPATH=src python docs/reports/modules/mooring_analysis/build_worked_example.py --output-dir "$TMPDIR/worked-example"</pre>
<p>Only <code>worked_example.html</code> is written in the selected directory.
The build uses no external input data, network access or licensed software.
It has no generated timestamps, absolute paths, shared index edits or external page assets.</p>
</body>
</html>
"""


def main() -> None:
    """Write the example only when explicitly invoked with an output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    page = render_example()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "worked_example.html").write_text(page, encoding="utf-8")
    print("Wrote worked_example.html")


if __name__ == "__main__":
    main()
