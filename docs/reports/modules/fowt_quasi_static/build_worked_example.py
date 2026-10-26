# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Build a standalone synthetic point-mass inertia-shift worked example."""

from __future__ import annotations

import argparse
from html import escape
from pathlib import Path

from digitalmodel.marine_ops.fowt_quasi_static.basis import parallel_axis


# Input specification: README.md, Inputs, method and answer section,
# beside this builder; constructed demonstration inputs in SI units.
MASS_KG = 2.0
OFFSET_M = (1.0, 2.0, 0.0)
# Control defined in the same README section, at the reference origin.
CONTROL_OFFSET_M = (0.0, 0.0, 0.0)
AXES = "xyz"


def number(value: float) -> str:
    """Render exact example values with one decimal and unsigned zero."""
    return f"{float(value) if value else 0.0:.1f}"


def cell(key: str, value: float, unit: str) -> str:
    """Render a visible numeric cell with a machine-readable key and unit."""
    return (f'<td data-key="{escape(key)}" data-unit="{escape(unit)}">'
            f'{number(value)}</td>')


def tensor_table(prefix: str, tensor, caption: str) -> str:
    """Render every tensor component, including symmetric entries."""
    header = "".join(f'<th scope="col">{axis}</th>' for axis in AXES)
    rows = []
    for row, axis in enumerate(AXES):
        values = "".join(cell(f"{prefix}-{axis}{other}", tensor[row, column], "kg m^2")
                         for column, other in enumerate(AXES))
        rows.append(f'<tr><th scope="row">{axis}</th>{values}</tr>')
    return (f'<table><caption>{escape(caption)} [kg m^2]</caption>'
            f'<thead><tr><th scope="col">Axis</th>{header}</tr></thead>'
            f'<tbody>{"".join(rows)}</tbody></table>')


def build(output_dir: Path) -> Path:
    """Write one deterministic HTML file into the selected output directory."""
    shift = parallel_axis(MASS_KG, OFFSET_M)
    control = parallel_axis(MASS_KG, CONTROL_OFFSET_M)
    input_rows = [f'<tr><th scope="row">Point mass</th>{cell("mass", MASS_KG, "kg")}<td>kg</td></tr>']
    for prefix, coordinates, label in (
        ("offset", OFFSET_M, "Offset"),
        ("control-offset", CONTROL_OFFSET_M, "Control offset"),
    ):
        for axis, value in zip(AXES, coordinates):
            input_rows.append(f'<tr><th scope="row">{label} {axis}</th>'
                              f'{cell(f"{prefix}-{axis}", value, "m")}<td>m</td></tr>')
    m = number(MASS_KG)
    x, y, z = map(number, OFFSET_M)
    # Each displayed answer is computed above. The scalar expressions explain
    # axis distances independently of the vector identity used by parallel_axis.
    diagonal = ", ".join(number(shift[i, i]) for i, _ in enumerate(AXES))
    html = f'''<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Synthetic point-mass inertia-shift worked example</title>
<style>
body {{ font-family: sans-serif; max-width: 60em; margin: auto; padding: 1em; }}
table {{ border-collapse: collapse; margin-block: 1em; }}
th, td {{ border: thin solid; padding: .5em; text-align: right; }}
caption {{ text-align: left; font-weight: bold; }}
</style>
</head>
<body>
<h1>Synthetic point-mass inertia-shift worked example</h1>
<section id="summary">
<h2>Summary</h2>
<p>A synthetic point mass gives the inertia-shift diagonal ({diagonal}) kg m^2
and symmetric xy entries {number(shift[0, 1])} kg m^2. The other off-diagonal
entries are zero. The zero-offset control gives a zero tensor.</p>
</section>
<section id="revision-history">
<h2>Revision history</h2>
<table><caption>Report revisions</caption>
<thead><tr><th scope="col">Revision</th><th scope="col">Date</th><th scope="col">What changed and why</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-25</td><td>Add the synthetic point-mass example to expose inputs, axis-distance arithmetic and the resulting shift.</td></tr>
<tr><td>1</td><td>2026-10-25</td><td>Identify the repository input specification to remove an unsupported external source claim.</td></tr></tbody>
</table>
</section>
<section id="inputs">
<h2>Inputs</h2>
<p>Input basis: README.md, Inputs, method and answer section.
#3318 requests a worked example but does not prescribe these values.
These are constructed demonstration inputs, rather than measured or turbine data.
Coordinates are Cartesian, measured from the reference origin to the point mass.
The control uses the same mass at the origin. Intrinsic point-mass inertia
about its own location is zero by definition.</p>
<table><caption>Synthetic mass and offsets</caption>
<thead><tr><th scope="col">Quantity</th><th scope="col">Value</th><th scope="col">Unit</th></tr></thead>
<tbody>{"".join(input_rows)}</tbody></table>
</section>
<section id="method">
<h2>Method and independent hand calculation</h2>
<p>The inertia about an axis is mass times squared perpendicular distance to
that axis. Tensor off-diagonal entries use negative coordinate products.
Thus Ixx = m(y^2 + z^2), Iyy = m(x^2 + z^2), Izz = m(x^2 + y^2),
Ixy = Iyx = -mxy, Ixz = Izx = -mxz, and Iyz = Izy = -myz.</p>
<ul>
<li>Ixx = {m}({y}^2 + {z}^2) = {number(shift[0, 0])} kg m^2.</li>
<li>Iyy = {m}({x}^2 + {z}^2) = {number(shift[1, 1])} kg m^2.</li>
<li>Izz = {m}({x}^2 + {y}^2) = {number(shift[2, 2])} kg m^2.</li>
<li>Ixy = Iyx = -{m} × {x} × {y} = {number(shift[0, 1])} kg m^2.</li>
<li>Ixz = Izx = -{m} × {x} × {z} = {number(shift[0, 2])} kg m^2.</li>
<li>Iyz = Izy = -{m} × {y} × {z} = {number(shift[1, 2])} kg m^2.</li>
</ul>
<p>The implementation calls only <code>basis.parallel_axis</code> for the
calculation: ΔI = m[(r · r)E - r ⊗ r], where E is the identity tensor and
r is the offset vector. The tests derive expected values separately from the
axis-distance and coordinate-product definitions above. Units follow from
kg × m × m = kg m^2.</p>
</section>
<section id="results">
<h2>Results</h2>
{tensor_table("shift", shift, "Inertia shift about the reference origin; row and column order x, y, z")}
<p>For the zero-offset control, every perpendicular distance and coordinate
product vanishes, so every component is zero.</p>
{tensor_table("control", control, "Zero-offset control; row and column order x, y, z")}
</section>
<section id="limits">
<h2>Limits and next steps</h2>
<p>NOT_EVALUATED: distributed-body aggregation, turbine datasets, hydrodynamics,
response, mooring equilibrium, mooring qualification and full-module qualification.
This example demonstrates only the rigid-body inertia shift for a synthetic
point mass. It makes no physical-system qualification or parent-issue completion
claim. The full module report and shared index entries remain follow-up work.</p>
<p>The report requires no external input data, solver, network access or large
computation. Only the selected output directory receives generated content.</p>
</section>
</body>
</html>
'''
    output_dir.mkdir(parents=True, exist_ok=True)
    destination = output_dir / "worked-example.html"
    destination.write_text(html, encoding="utf-8", newline="\n")
    return destination


def main() -> None:
    """Parse the output destination and generate the worked example."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True,
                        help="Directory receiving worked-example.html")
    arguments = parser.parse_args()
    build(arguments.output_dir)


if __name__ == "__main__":
    main()
