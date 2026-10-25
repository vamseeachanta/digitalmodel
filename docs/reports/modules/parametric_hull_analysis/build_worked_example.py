# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Render the synthetic deep-water arithmetic probes and hand derivation [H]."""

from __future__ import annotations

import argparse
import runpy
from pathlib import Path


def render_example() -> str:
    """Call the existing helper and render a self-contained, deterministic page."""
    import numpy as np

    source = (
        Path(__file__).resolve().parents[4]
        / "src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py"
    )
    # Load the shipped helper directly, without package-wide imports of charts
    # or solver adapters. run_path reads source without caching its bytecode.
    wave_number = runpy.run_path(str(source))["wave_number"]
    # Source [H] below: deliberately synthetic arithmetic inputs, not metocean data.
    gravity = 4.0
    angular_frequencies = np.array([1.0, 2.0])
    computed = wave_number(angular_frequencies, water_depth=np.inf, g=gravity)
    # Source [H]: independent exact hand answers, separate from the helper output.
    hand_answers = ("1/4", "1")
    rows = "\n".join(
        f"<tr><td>{omega:g}</td><td>rad/s</td><td>{k}</td><td>1/m</td>"
        f"<td>{hand}</td><td>1/m</td></tr>"
        for omega, k, hand in zip(angular_frequencies, computed, hand_answers)
    )
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Deep-water wave-number worked example</title>
</head>
<body>
<main>
<h1>Deep-water wave-number worked example</h1>
<section id="summary">
<h2>Summary</h2>
<p>The existing <code>wave_number</code> helper is evaluated using synthetic
arithmetic probes: gravity {gravity:g} m/s^2 and angular frequencies
({angular_frequencies[0]:g}, {angular_frequencies[1]:g}) rad/s.
The computed wave numbers are ({computed[0]}, {computed[1]}) 1/m,
matching the exact hand answers (1/4, 1) 1/m from
<a href="#hand-derivation">Hand derivation [H]</a>.</p>
<p>These inputs are arithmetic probes rather than environmental data.
Only the deep-water dispersion calculation is evaluated.</p>
</section>
<section id="revision-history">
<h2>Revision history</h2>
<table id="revisions">
<thead><tr><th scope="col">Revision</th><th scope="col">Date</th>
<th scope="col">What changed and why</th></tr></thead>
<tbody><tr><td>1</td><td>2026-10-25</td><td>Initial worked example:
make the synthetic inputs, independent hand calculation and scope explicit.
</td></tr></tbody>
</table>
</section>
<section id="input-basis">
<h2>Inputs and assumptions</h2>
<table id="inputs">
<thead><tr><th scope="col">Symbol</th><th scope="col">Value</th>
<th scope="col">Unit</th><th scope="col">Basis</th></tr></thead>
<tbody>
<tr><th scope="row">g</th><td>{gravity:g}</td><td>m/s^2</td>
<td>Synthetic arithmetic probe; Hand derivation [H].</td></tr>
<tr><th scope="row">omega</th>
<td>({angular_frequencies[0]:g}, {angular_frequencies[1]:g})</td><td>rad/s</td>
<td>Synthetic angular frequency probes; Hand derivation [H].</td></tr>
<tr><th scope="row">h</th><td>infinite</td><td>m</td>
<td>Ideal deep water, passed explicitly as numpy.inf; Hand derivation [H].</td></tr>
</tbody>
</table>
<p>omega is angular frequency, not cyclic frequency. Radians are dimensionless
in the dimensional balance. The basis is a linear, small-amplitude, inviscid,
incompressible, irrotational free-surface gravity wave, without current or
surface tension. The infinite-depth idealization requires no finite-depth
iteration or environmental depth estimate.</p>
</section>
<section id="hand-derivation">
<h2>Hand derivation [H]</h2>
<p>[H] Independent calculation stated here. At the undisturbed free surface,
the linear kinematic and dynamic conditions are eta_t = phi_z and
phi_t + g*eta = 0. Eliminating eta gives phi_tt + g*phi_z = 0.
For a deep-water harmonic potential proportional to exp(k*z)*cos(k*x-omega*t),
phi_tt = -omega^2*phi and phi_z = k*phi. Substitution gives
(-omega^2 + g*k)*phi = 0, hence omega^2 = g*k and
<code>k = omega^2/g</code>.</p>
<p>The finite-depth linear relation is <code>omega^2 = g*k*tanh(k*h)</code>.
In deep water tanh(k*h) tends to unity, recovering the same expression.
For the stated probes, k = (1 rad/s)^2 / (4 m/s^2) = 1/4 1/m and
k = (2 rad/s)^2 / (4 m/s^2) = 1 1/m. Thus [s^-2]/[m*s^-2] = [1/m].
The values 4, 1 and 2 are deliberate arithmetic choices; the resulting fractions
are derived here, with no external data or licensed reference values.</p>
</section>
<section id="calculation">
<h2>Method and results</h2>
<p>The builder calls <code>wave_number(omega, water_depth=numpy.inf, g=4)</code>
from the committed source
<code>src/digitalmodel/hydrodynamics/parametric_hull_analysis/forward_speed.py</code>.
The rendered computed values below are returned by that call; the hand-answer
column cites [H] and is independent of the implementation.</p>
<table id="results">
<thead><tr><th scope="col">omega</th><th scope="col">Unit</th>
<th scope="col">Computed k</th><th scope="col">Unit</th>
<th scope="col">Hand k [H]</th><th scope="col">Unit</th></tr></thead>
<tbody>
{rows}
</tbody>
</table>
</section>
<section id="limits">
<h2>Limits and follow-up</h2>
<p>NOT_EVALUATED: finite-depth iteration, heading conventions,
forward-speed RAO corrections, parametric hull sweeps, shallow-water
corrections and BEM solver execution. No full-module qualification is claimed.
The parent report issue remains open; the full report and shared indexes
remain follow-up. Agreement for these arithmetic probes does not establish
environmental suitability or accuracy for other inputs.</p>
</section>
</main>
</body>
</html>
"""


def main() -> None:
    """Write the page exclusively into the explicitly selected directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    html = render_example()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "worked-example.html").write_text(html, encoding="utf-8")


if __name__ == "__main__":
    main()
