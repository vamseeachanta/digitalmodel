# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the synthetic section-shape example; see README.md, hand calculation H1."""

from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path
import sys


# H1 in the adjacent README defines these synthetic inputs, in metres.
LENGTH_M = 4.0
STATIONS_M = (-2.0, -1.0, 0.0, 1.0, 2.0)


def load_formulations():
    """Load the shipped helpers without importing plotting or solver packages."""
    root = Path(__file__).resolve().parents[4]
    source = root / "src/digitalmodel/hydrodynamics/passing_ship/formulations.py"
    spec = importlib.util.spec_from_file_location("section_formulations", source)
    if spec is None or spec.loader is None:
        raise ImportError("Section formulations could not be loaded")
    module = importlib.util.module_from_spec(spec)
    # Dependency imports must not create bytecode outside the output directory.
    previous = sys.dont_write_bytecode
    try:
        sys.dont_write_bytecode = True
        spec.loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = previous
    return module


def render_example() -> str:
    """Compute the displayed results using the existing section helpers."""
    formulations = load_formulations()
    rows = []
    for x in STATIONS_M:
        shape = formulations.s1_function(x, LENGTH_M)
        slope = formulations.ds1_dx(x, LENGTH_M)
        meaning = (
            "one-sided from interior" if abs(x) == LENGTH_M / 2
            else "interior derivative"
        )
        slope_text = "0" if slope == 0 else f"{slope:g}"
        rows.append(
            f"<tr><td>{x:g}</td><td>{shape:g}</td>"
            f"<td>{slope_text}</td><td>{meaning}</td></tr>"
        )
    results = "\n".join(rows)
    stations = ", ".join(f"{x:g}" for x in STATIONS_M)
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Synthetic normalized-section worked example</title>
</head>
<body>
<main>
<h1>Synthetic normalized-section worked example</h1>
<section id="summary">
<h2>Summary</h2>
<p>This synthetic example evaluates the normalized area shape and polynomial
slope using <code>s1_function</code> and <code>ds1_dx</code>.
The values agree with the independent hand polynomial H1 below.
Scope: <strong>NOT_EVALUATED</strong> for physical forces;
hydrodynamic force qualification remains unevaluated.</p>
</section>
<section id="revision-history">
<h2>Revision history</h2>
<table id="revisions">
<thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-25</td><td>Added the synthetic section
example to expose inputs, hand derivation, units and evidence limits.</td></tr></tbody>
</table>
</section>
<section id="inputs-section">
<h2>Inputs and basis</h2>
<p>H1 is an independent hand calculation, defined for this example rather than
measured geometry or calibration data. Length and stations are synthetic choices;
endpoint values and the unit midpoint define the parabola.</p>
<table id="inputs">
<thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>
<tr><td>Length L</td><td>{LENGTH_M:g}</td><td>m</td><td>H1 synthetic definition</td></tr>
<tr><td>Stations x</td><td>{stations}</td><td>m</td><td>H1 synthetic definition</td></tr>
</tbody>
</table>
</section>
<section id="method">
<h2>Hand calculation H1</h2>
<p>Take x in metres about the midpoint and define
<code>S(x) = a*x^2 + b*x + c</code>, with S dimensionless.
The endpoint conditions S(-2 m) = S(2 m) = 0 and midpoint S(0) = 1 give
c = 1 and the equations 4 m^2*a - 2 m*b + 1 = 0 and
4 m^2*a + 2 m*b + 1 = 0. Subtraction gives 4 m*b = 0, so b = 0 m^-1.
Either endpoint equation then gives a = -1/4 m^-2.</p>
<p>For numerical x expressed in metres, <code>S(x) = 1 - x^2/4</code>
and <code>dS/dx = -x/2</code> per metre in the interior.
Direct substitution gives S = (0, 3/4, 1, 3/4, 0) and interior slopes
(1/2, 0, -1/2) m^-1. These are independent expected values.</p>
<p>The implementation uses <code>s1_function(x, L) = 1 - (2*x/L)^2</code>
and <code>ds1_dx(x, L) = -8*x/L^2</code> on the closed section interval.
At L = 4 m these reduce to H1. Both helpers return zero outside that interval.
The table reports their endpoint slope values as one-sided limits from the
interior: the two-sided derivative does not exist at the endpoints of the
zero-extended shape. No exterior derivative is being qualified.</p>
</section>
<section id="results-section">
<h2>Computed results</h2>
<table id="results">
<thead><tr><th>x [m]</th><th>S [dimensionless]</th>
<th>Polynomial slope [m^-1]</th><th>Slope meaning</th></tr></thead>
<tbody>
{results}
</tbody>
</table>
</section>
<section id="limits">
<h2>Units and limits</h2>
<p>S is a dimensionless area shape, not an area in square metres or a force.
Its derivative with respect to dimensional x has units m^-1. No physical area
scale, density, speed, separation or water depth is supplied. No force in N or
moment in N m is calculated, and no force campaign, calibration, external
coefficient table or licensed software is used.</p>
<p>Status: <strong>NOT_EVALUATED</strong> for hydrodynamic force qualification.
This checks only the section helpers at the stated synthetic stations. It does
not establish full-module qualification or complete the parent report issue.
The way forward is separately reviewed physical-force evidence and a full report.</p>
</section>
<section id="sources">
<h2>Sources</h2>
<p>H1: the synthetic boundary conditions and independent polynomial derivation in
this report and the adjacent README.md. All example numbers follow from H1.
Implementation: <code>src/digitalmodel/hydrodynamics/passing_ship/formulations.py</code>,
<code>s1_function</code> and <code>ds1_dx</code>. No external reference values are used.</p>
</section>
</main>
</body>
</html>
"""


def build(output_dir: Path) -> Path:
    """Write only the example HTML inside the selected output directory."""
    content = render_example()
    output_dir.mkdir(parents=True, exist_ok=True)
    target = output_dir / "worked_example.html"
    if target.is_symlink():
        raise ValueError("The report target must not be a symbolic link")
    target.write_text(content, encoding="utf-8")
    return target


def main() -> None:
    """Run the explicitly requested report build."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir)
    print("Wrote worked_example.html")


if __name__ == "__main__":
    main()
