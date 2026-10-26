# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a bounded synthetic closure-kinematics example for issue #3340.

Numeric basis: author-selected illustrative assumptions of 0.6 m full travel,
0.2 m/s constant loaded speed and elapsed samples (0, 1.5, 3, 4) s; these inputs
are not supplied by issue #3340. The normalization bounds (0, 1) define the
assumed clipped linear conductance law; no equipment data or standards-derived
numeric criteria are used. See README.md for the hand basis and method source.
"""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.subsea.hipps.valve_sizing import closure_fraction, stroke_time


def render_example() -> str:
    """Compute the fixed synthetic case and render deterministic HTML."""
    # Author-selected illustrative inputs; issue #3340 supplies no numeric case.
    travel_m = 0.6
    loaded_speed_m_s = 0.2
    elapsed_s = (0.0, 1.5, 3.0, 4.0)
    closure_time_s = stroke_time(
        stroke_length=travel_m, actuator_speed=loaded_speed_m_s,
    )
    rows = "\n".join(
        f"<tr><td>{elapsed:.1f}</td><td>s</td>"
        f"<td>{closure_fraction(elapsed, closure_time_s, 'linear'):.1f}</td>"
        "<td>dimensionless</td></tr>"
        for elapsed in elapsed_s
    )
    not_evaluated = "\n".join(
        f"<tr><td>{name}</td><td>NOT_EVALUATED</td></tr>"
        for name in (
            "Detection delay", "SIL", "Pressure transients", "Capacity",
            "Equipment suitability", "Standards compliance", "Actual flow",
        )
    )
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>HIPPS closure-kinematics worked example</title>
<style>
body {{ font-family: sans-serif; margin: 2rem auto; max-width: 64rem;
        padding: 0 1rem; line-height: 1.5; color: #172b3a; }}
table {{ border-collapse: collapse; margin-bottom: 1rem; width: 100%; }}
th, td {{ border: 1px solid #9caebc; padding: 0.5rem; text-align: left; }}
th {{ background: #eaf0f4; }}
code {{ white-space: normal; }}
</style>
</head>
<body><main>
<h1>HIPPS closure-kinematics worked example</h1>
<section id="summary">
<h2>Summary</h2>
<p>Synthetic, constant-speed actuator travel gives a closure time of
{closure_time_s:.1f} s. The assumed linear law gives remaining idealized conductance
of 1.0, 0.5, 0.0 and 0.0 at elapsed times 0.0, 1.5, 3.0 and 4.0 s.
This arithmetic example is a bounded part of issue #3340; the parent issue
remains open and full-module qualification is not established.</p>
<p>Draft for maintainer review.</p>
</section>
<section id="revision-history">
<h2>Revision history</h2>
<table id="revisions">
<thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>1</td><td>2026-10-25</td><td>Initial synthetic closure example
to expose travel-time and linear-conductance arithmetic.</td></tr>
<tr><td>2</td><td>2026-10-25</td><td>Corrected input provenance to identify
author-selected assumptions and added the kinematic method source.</td></tr></tbody>
</table>
</section>
<section id="basis">
<h2>Basis and inputs</h2>
<p>The travel, speed and sample times are author-selected illustrative assumptions,
not supplied by issue #3340 and not measured equipment data.
Maintainer acceptance of this basis is required before merge.
Loaded speed is assumed constant for the full stroke. Elapsed time starts
with actuator motion; no preceding detection or logic delay is included.</p>
<table id="inputs">
<thead><tr><th>Input</th><th>Value</th><th>Unit</th></tr></thead>
<tbody>
<tr><td>Full actuator travel</td><td>{travel_m:.1f}</td><td>m</td></tr>
<tr><td>Constant loaded actuator speed</td><td>{loaded_speed_m_s:.1f}</td><td>m/s</td></tr>
</tbody>
</table>
</section>
<section id="method">
<h2>Equations and implementation</h2>
<p>Independent kinematic basis: constant speed means L = v T, hence
<code>T = L / v</code>. Thus (0.6 m) / (0.2 m/s) = 3.0 s.</p>
<p>Method source: OpenStax, <cite>University Physics Volume 1</cite>,
2016 edition, section 3.4, <a
href="https://openstax.org/books/university-physics-volume-1/pages/3-4-motion-with-constant-acceleration"
>Motion with Constant Acceleration</a>, Equation 3.10 with constant velocity.
This is not a source of the selected numeric inputs.</p>
<p>The completed actuator travel fraction is <code>clip(t / T, 0, 1)</code>,
where clipping restricts the ratio to the normalization interval from 0 to 1.
The separately assumed linear remaining-conductance law is
<code>g(t) = 1 - clip(t / T, 0, 1)</code>.
At the selected sample times the completed fractions are 0, 1/2, 1 and 1;
subtracting from 1 gives remaining fractions 1, 1/2, 0 and 0.</p>
<p>The calculation calls <code>stroke_time</code> and
<code>closure_fraction(..., characteristic="linear")</code> from
<code>src/digitalmodel/subsea/hipps/valve_sizing.py</code>.
The unitless normalization bounds are definitions, not licensed criteria.</p>
</section>
<section id="results">
<h2>Computed results</h2>
<table id="closure-time">
<thead><tr><th>Quantity</th><th>Value</th><th>Unit</th></tr></thead>
<tbody><tr><td>Closure time</td><td>{closure_time_s:.1f}</td><td>s</td></tr></tbody>
</table>
<table id="conductance">
<thead><tr><th>Elapsed time</th><th>Time unit</th>
<th>Remaining idealized conductance</th><th>Conductance unit</th></tr></thead>
<tbody>
{rows}
</tbody>
</table>
<p>Displayed results are rounded to one decimal place; computations use
the unrounded travel-time result.</p>
</section>
<section id="limits">
<h2>Limits and way forward</h2>
<p>Actuator travel describes mechanical motion. Idealized conductance is a
normalized assumed valve characteristic; it is not actual flow, a flow
coefficient, a leakage rating or evidence of physical isolation. Actual flow
also depends on fluid properties, pressure conditions and the valve's qualified
characteristic. Zero idealized conductance here is a mathematical endpoint.</p>
<p>This example supplies no protection adequacy, capacity, timing acceptance
or compliance decision. Further analysis requires qualified actuator and valve
data, detection and logic timing, reliability evidence and a coupled transient
model with applicable acceptance criteria.</p>
<table id="not-evaluated">
<thead><tr><th>Scope item</th><th>Status</th></tr></thead>
<tbody>
{not_evaluated}
</tbody>
</table>
</section>
</main></body>
</html>
"""


def main() -> None:
    """Write the report only under the explicitly selected output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    content = render_example()
    output_dir = args.output_dir.resolve()
    destination = output_dir / "closure_kinematics.html"
    # An existing output symlink must not redirect writes outside the selection.
    if destination.is_symlink():
        parser.error("the report destination must not be a symbolic link")
    output_dir.mkdir(parents=True, exist_ok=True)
    destination.write_text(content, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
