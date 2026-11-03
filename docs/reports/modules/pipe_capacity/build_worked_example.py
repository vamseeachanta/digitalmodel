# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Render a synthetic annulus example using the existing section sizing method."""

import argparse
from pathlib import Path

from digitalmodel.structural.pipe_capacity.common.PipeSizing import PipeSizing


def build_worked_example(output_dir: Path) -> Path:
    """Write the self-contained page only inside the selected directory.

    Basis: synthetic arithmetic probes of outside diameter 4 m and inside
    diameter 2 m. All equation coefficients are derived by the polar integrals
    documented in the page; no standards or material properties are used.
    """
    cfg = {"Outer_Pipe": {"Geometry": {"Nominal_OD": 4.0, "Nominal_ID": 2.0}}}
    PipeSizing(cfg).pipe_section_properties("Outer_Pipe")
    properties = cfg["Outer_Pipe"]["section_properties"]["pipe"]
    geometry = cfg["Outer_Pipe"]["Geometry"]
    # One decimal place is the declared display resolution, not solver precision.
    rows = "\n".join(
        f'<tr id="{key}"><th scope="row">{label}</th>'
        f'<td class="value">{value:.1f}</td><td class="unit">{unit}</td></tr>'
        for key, label, value, unit in (
            ("Nominal_OD", "Outside diameter", geometry["Nominal_OD"], "m"),
            ("Nominal_ID", "Inside diameter", geometry["Nominal_ID"], "m"),
            ("A", "Annulus area A", properties["A"], "m^2"),
            ("I", "Centroidal second moment Ix = Iy = I", properties["I"], "m^4"),
            ("J", "Polar second moment J", properties["J"], "m^4"),
        )
    )
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Pipe section sizing worked example</title>
</head>
<body>
<main>
<h1>Pipe section sizing worked example</h1>
<h2>Summary</h2>
<p>This synthetic annulus example calls the existing section sizing method
and illustrates geometric sizing prerequisites. The diameters are arithmetic probes,
not a product specification. Scope: <strong>NOT_EVALUATED</strong> for
pressure containment, collapse and combined loading. No full-module qualification
or completion of the parent report issue is claimed.</p>
<h2>Revision history</h2>
<table>
<caption>Revision history</caption>
<thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>0</td><td>2026-10-25</td><td>Initial synthetic geometry example
to document section inputs, independent integrals and results.</td></tr></tbody>
</table>
<h2>Inputs and basis</h2>
<p>The chosen outside diameter is 4 m and inside diameter is 2 m, giving outer
radius Ro = 2 m and inner radius Ri = 1 m. These are defined synthetic inputs,
not measured data or recommended dimensions. All lengths are interpreted in metres;
the method is unit agnostic and performs no unit conversion.</p>
<p>Basis: the independent polar-coordinate hand calculation below supplies every
equation coefficient. The displayed results are calculated by
<code>PipeSizing.pipe_section_properties</code> in
<code>src/digitalmodel/structural/pipe_capacity/common/PipeSizing.py</code>.</p>
<h2>Method and independent hand calculation</h2>
<p>For a concentric circular annulus about its centroid, let x = r cos(theta),
y = r sin(theta), dA = r dr dtheta, with Ri &lt;= r &lt;= Ro and
0 &lt;= theta &lt;= 2*pi. The factor 2*pi is the angular integral of unity;
the integral of sin(theta)^2 or cos(theta)^2 over that interval is pi.</p>
<pre>
A = integral(theta=0..2*pi) integral(r=1..2) r dr dtheta
  = 2*pi * [r^2/2]_1^2 = 3*pi m^2
Ix = integral(theta=0..2*pi) integral(r=1..2) (r*sin(theta))^2 r dr dtheta
   = pi * [r^4/4]_1^2 = 15*pi/4 m^4
Iy = Ix by circular symmetry; I denotes either centroidal diametral moment.
J = integral(theta=0..2*pi) integral(r=1..2) r^2 r dr dtheta
  = 2*pi * [r^4/4]_1^2 = 15*pi/2 m^4 = Ix + Iy
</pre>
<p>The powers and denominators follow integration of r and r^3. Substituting
Ro = Do/2 and Ri = Di/2 gives the diameter formulas used by the method:</p>
<pre>
A = pi/4 * (Do^2 - Di^2)
I = pi/64 * (Do^4 - Di^4)
J = pi/32 * (Do^4 - Di^4)
</pre>
<p>The minimal mapping contains only
<code>{{"Outer_Pipe": {{"Geometry": {{"Nominal_OD": 4.0, "Nominal_ID": 2.0}}}}}}</code>.
Construct <code>PipeSizing(cfg)</code>, then call
<code>pipe_section_properties("Outer_Pipe")</code> directly. Read A, I and J
from <code>cfg["Outer_Pipe"]["section_properties"]["pipe"]</code>.
No material, pressure or loading mapping is supplied.</p>
<h2>Calculated results and units</h2>
<table>
<caption>Inputs and section properties, rounded to one decimal place</caption>
<thead><tr><th>Quantity</th><th>Value</th><th>Unit</th></tr></thead>
<tbody>
{rows}
</tbody>
</table>
<p>Rounding is for display only. The tests compare unrounded section properties
against the independent integrals and read back these rendered numbers and units.</p>
<h2>Limits and way forward</h2>
<p><strong>NOT_EVALUATED:</strong> pressure containment, collapse and combined loading.
Geometry alone establishes no capacity, allowable pressure, code compliance or
fitness for service. No material strength, corrosion allowance, manufacturing
tolerance, load case, design factor or licensed standard is evaluated.</p>
<p>Before selecting a section for service, establish those inputs and perform the
applicable pressure, stability and interaction assessments. The full module report
and shared report indexes remain separate follow-up work.</p>
<h2>Rebuild</h2>
<p>From the repository root, select an output directory:</p>
<pre>PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/pipe_capacity/build_worked_example.py --output-dir "$TMPDIR/worked-example"</pre>
<p>The builder itself uses the standard library and the existing sizing class.
It needs no external input data, licensed software, network access or large
computation. If uv must obtain pytest, prepare that environment before an offline
run; the builder can also be called with its Python interpreter directly.
Only <code>worked_example.html</code> is written in the selected directory.
The page has no external resources, timestamps or absolute output paths.</p>
</main>
</body>
</html>
"""
    output_dir.mkdir(parents=True, exist_ok=True)
    output = output_dir / "worked_example.html"
    output.write_text(html, encoding="utf-8")
    return output


def main() -> None:
    """Parse the selected output directory and build the worked example."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    build_worked_example(args.output_dir)


if __name__ == "__main__":
    main()
