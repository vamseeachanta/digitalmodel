# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Render a synthetic beam admission example without evaluating a response."""

from __future__ import annotations

import argparse
from html import escape
import math
from pathlib import Path


def example_settings() -> dict:
    """Return [S1], the explicit synthetic SI fixture documented in the report."""
    return {
        "nodes": [{"id": 0, "x": 0.0, "y": 0.0}, {"id": 1, "x": 3.0, "y": 4.0}],
        "elements": [{"id": 0, "node1": 0, "node2": 1,
                      "E": 200e9, "I": 8e-6, "A": 0.01}],
        "material": {"E": 200e9, "nu": 0.3, "rho": 7800.0, "sigma_y": 250e6},
        "boundary_conditions": {"fixed_nodes": [0]},
        "loads": [{"node": 1, "direction": "y", "magnitude": -1000.0}],
    }


def _fields(nodes, element, material, boundary_conditions, load) -> list[tuple]:
    """Assign common labels and units to either input values or model readback."""
    rows = []
    for node_id, coordinates in sorted(nodes.items()):
        for axis, coordinate in zip(("x", "y"), coordinates):
            rows.append((f"node_{node_id}_{axis}", f"Node {node_id}: {axis}", coordinate, "m"))
    rows.extend([
        ("element_id", "Element identifier", element["id"], "identifier"),
        ("node1", "Element start node", element["node1"], "identifier"),
        ("node2", "Element end node", element["node2"], "identifier"),
        ("material_E", "Material elastic modulus E", material["E"], "Pa"),
        ("element_E", "Element elastic modulus E", element["E"], "Pa"),
        ("I", "Second moment of area I", element["I"], "m^4"),
        ("A", "Cross-sectional area A", element["A"], "m^2"),
        ("nu", "Poisson ratio", material["nu"], "dimensionless"),
        ("rho", "Density", material["rho"], "kg/m^3"),
        ("sigma_y", "Yield stress", material["sigma_y"], "Pa"),
        ("fixed_nodes", "Fixed nodes", str(boundary_conditions["fixed_nodes"]), "node identifiers"),
        ("load_node", "Load node", load["node"], "identifier"),
        ("load_direction", "Load direction", load["direction"], "global axis"),
        ("load_magnitude", "Signed load magnitude", load["magnitude"], "N"),
    ])
    return rows


def _value(value) -> str:
    return escape(format(float(value), ".6g") if isinstance(value, (int, float)) else str(value))


def _table(rows, prefix: str = "", basis: str = "[S1]") -> str:
    body = []
    for key, label, value, unit in rows:
        body.append(
            f'<tr data-field="{escape(prefix + key)}"><th scope="row">{escape(label)}</th>'
            f"<td>{_value(value)}</td><td>{escape(unit)}</td><td>{escape(basis)}</td></tr>"
        )
    return (
        "<table><thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Basis</th></tr>"
        "</thead><tbody>" + "\n".join(body) + "</tbody></table>"
    )


def render_report() -> str:
    """Build the model and render its admitted values; invoke no solver."""
    from digitalmodel.structural.fe.model_builder import build_beam_model

    settings = example_settings()
    inputs = _fields(
        {node["id"]: (node["x"], node["y"]) for node in settings["nodes"]},
        settings["elements"][0], settings["material"],
        settings["boundary_conditions"], settings["loads"][0],
    )
    model = build_beam_model(settings, "beam_admission_example")
    element = model.elements[0]
    admitted = _fields(
        model.nodes,
        {"id": element.elem_id, "node1": element.node1, "node2": element.node2,
         "E": element.E, "I": element.I, "A": element.A},
        model.material, model.boundary_conditions, model.loads[0],
    )
    # [H1] Independent hand check of the prescribed geometry, not a model result.
    hand_length = math.sqrt(3.0**2 + 4.0**2)
    geometry = [
        ("hand_length", "Independent hand length", hand_length, "m"),
        ("admitted_length", "Admitted element length", element.L, "m"),
    ]
    responses = [(key, label, "NOT_EVALUATED", "not calculated") for key, label in (
        ("buckling", "Buckling"), ("stress", "Stress"), ("displacement", "Displacement"),
    )]
    return f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Beam model construction worked example</title>
<style>
body {{ font-family: system-ui, sans-serif; max-width: 70rem; margin: auto; padding: 2rem; color: #182a3a; }}
table {{ border-collapse: collapse; width: 100%; margin-block: 1rem; }}
th, td {{ border: thin solid #ccd5dc; padding: .5rem; text-align: left; }}
thead {{ background: #edf3f7; }} code {{ overflow-wrap: anywhere; }}
</style></head><body><main>
<h1>Beam model construction worked example</h1>
<h2>Summary</h2>
<p>This synthetic example demonstrates model construction only through
<code>build_beam_model</code>. The admitted element length is {_value(element.L)} m,
compared with the independent hand length of {_value(hand_length)} m [H1].
The specified coordinates, connectivity, material, section, support and load are
read back below. Buckling, stress and displacement are NOT_EVALUATED.</p>
<h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>A</td><td>2026-10-25</td><td>The synthetic admission example is introduced
to document inputs and independent geometry without a response calculation.</td></tr>
<tr><td>B</td><td>2026-10-26</td><td>The isolated rebuild command declares NumPy
so the required dependency is available in a clean environment.</td></tr></tbody></table>
<h2>Inputs</h2>
<p>[S1] All physical values and identifiers are synthetic assumptions for this
example, assumed, based on previous experience. They are not measured data,
certified material properties or an approved design. SI units are explicit.
The section area and second moment are supplied independently; no section shape
or dimensional consistency of a physical cross-section is established.</p>
{_table(inputs, "input_")}
<p>The boundary condition requests a fixed support at the start node. The signed
load is applied at the end node along the global y axis; a negative value denotes
the negative axis direction [S1]. Support and load admission does not establish
how a response solver would enforce them.</p>
<h2>Method and equations</h2>
<p>The explicit settings are passed to
<code>src/digitalmodel/structural/fe/model_builder.py:build_beam_model</code>.
Values in the next table are read from the returned nodes, element, material,
boundary conditions and load, rather than copied from the input table.</p>
<p>[H1] The independent hand calculation uses the Pythagorean identity for a
straight segment: L = sqrt((x_end - x_start)^2 + (y_end - y_start)^2).
For the prescribed coordinates, L = sqrt(3^2 + 4^2) = 5 m.
The exponents follow the identity, and the coordinates are specified in [S1].</p>
<h2>Results: admitted values</h2>
{_table(admitted, "admitted_")}
{_table(geometry, basis="[H1]")}
{_table(responses, basis="Model construction only")}
<h2>Limits</h2>
<p>This example checks data admission and straight-segment geometry only.
No stiffness assembly, eigenvalue analysis, stress recovery, displacement
solution or design-code check is performed. The inclined segment does not
qualify solver coordinate transformations, support enforcement, material
behaviour or load response. Yield stress, density and Poisson ratio are admitted
data only; no physical response is inferred from them.</p>
<p>The way forward is separate analytical workflow examples and response checks
before engineering use. Full module reporting and qualification remain
follow-up work; this example does not complete the parent reporting issue.</p>
<h2>Rebuild</h2>
<pre><code>PYTHONPATH=src uv run --no-project --with pytest --with 'numpy&gt;=1.24,&lt;2' python docs/reports/modules/structural_fe/build_worked_example.py --output-dir selected-output</code></pre>
<p>Run from a source checkout with NumPy available. The only generated file is
<code>worked-example.html</code> inside the selected output directory.
No external input data, network access, licensed software or large computation
is used by the generator. Fixed revision metadata and omission of runtime paths
and timestamps allow byte-for-byte rebuilds.</p>
</main></body></html>
"""


def main(argv: list[str] | None = None) -> None:
    """Write the report only into the explicitly selected output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args(argv)
    report = render_report()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "worked-example.html").write_text(report, encoding="utf-8")


if __name__ == "__main__":
    main()
