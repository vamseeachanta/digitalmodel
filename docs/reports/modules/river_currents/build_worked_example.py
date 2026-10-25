# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the standalone synthetic linear-profile mathematical demonstration."""

import argparse
from pathlib import Path

from digitalmodel.hydrodynamics.river_currents.profiles import (
    power_law_depth_averaged,
    power_law_profile,
)


def render_example():
    """Render helper results alongside the independent triangle calculation."""
    # S1: prescribed synthetic inputs, reproduced in the page and local README.
    depth = 4.0  # m
    surface_speed = 2.0  # m/s
    exponent_m = 1.0  # dimensionless; makes the profile linear
    heights = (("bed", 0.0), ("mid", 2.0), ("surface", 4.0))  # m, S1

    # C1: triangle area = base * height / 2, independent of the helper formula.
    triangle_integral = depth * surface_speed / 2
    hand_mean = triangle_integral / depth
    helper_mean = power_law_depth_averaged(depth, surface_speed, exponent_m)

    def cells(key, value, unit):
        return (
            f'<td id="{key}">{value:.1f}</td>'
            f'<td id="{key}-unit">{unit}</td>'
        )

    profile_rows = "\n".join(
        "<tr>"
        + cells(f"height-{label}", height, "m")
        + cells(
            f"speed-{label}",
            power_law_profile(height, depth, surface_speed, exponent_m),
            "m/s",
        )
        + '<td>S1 height; power_law_profile result; C1 hand check</td></tr>'
        for label, height in heights
    )
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Synthetic river profile worked example</title>
<style>
body {{ font-family: sans-serif; max-width: 60em; margin: auto; padding: 1em; }}
table {{ border-collapse: collapse; margin-bottom: 1em; }}
th, td {{ border: thin solid; padding: .5em; text-align: left; }}
</style>
</head>
<body>
<h1>Synthetic river profile worked example</h1>
<section>
<h2>Summary</h2>
<p>This mathematical demonstration uses explicitly synthetic inputs, not a measured river.
The linear power-law profile has a depth mean of {hand_mean:.1f} m/s by an independent
triangle calculation (C1). The helpers are exercised only for this prescribed case;
this example does not qualify the full module or complete the parent report issue.</p>
</section>
<section>
<h2>Revision history</h2>
<table>
<thead><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-25</td><td>Added the synthetic linear profile
and hand integral to supply a standalone worked example.</td></tr></tbody>
</table>
</section>
<section>
<h2>Inputs</h2>
<p id="synthetic-basis">S1 — Synthetic prescription: H = 4.0 m, surface speed
u_s = 2.0 m/s, exponent_m = 1.0 (dimensionless), with sample heights
z = (0.0, 2.0, 4.0) m above the bed. These are selected mathematical inputs,
not site data or calibrated river parameters.</p>
<table>
<thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>
<tr><th>Depth H</th>{cells('depth', depth, 'm')}<td>S1</td></tr>
<tr><th>Surface speed u_s</th>{cells('surface-speed', surface_speed, 'm/s')}<td>S1</td></tr>
<tr><th>exponent_m</th>{cells('exponent', exponent_m, 'dimensionless')}<td>S1</td></tr>
</tbody>
</table>
</section>
<section>
<h2>Equations and hand calculation</h2>
<p>The coordinate z is height above the bed, from zero to H, rather than depth
below the surface. The prescribed power law is u(z) = u_s (z/H)^(1/exponent_m).</p>
<p id="hand-calculation">C1 — Substitution of S1 gives
u(z) = (2.0 m/s)(z / (4.0 m)) = z / (2.0 s).
Thus z = (0.0, 2.0, 4.0) m gives u = (0.0, 1.0, 2.0) m/s.
The area under this straight line is a triangle:
integral from 0.0 m to 4.0 m of u(z) dz = (4.0 m)(2.0 m/s)/2 = 4.0 m²/s.
Dividing by H gives mean velocity = (4.0 m²/s)/(4.0 m) = 1.0 m/s.</p>
<p>The triangle calculation does not use the depth-average helper. For comparison,
the helper implements mean velocity = u_s exponent_m / (exponent_m + 1).
Both <code>power_law_profile</code> and <code>power_law_depth_averaged</code>
are called with the S1 inputs.</p>
</section>
<section>
<h2>Results</h2>
<table>
<thead><tr><th>Height above bed</th><th>Unit</th><th>Helper velocity</th><th>Unit</th>
<th>Basis</th></tr></thead>
<tbody>{profile_rows}</tbody>
</table>
<table>
<thead><tr><th>Quantity</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>
<tr><th>Triangle integral</th>{cells('triangle-integral', triangle_integral, 'm²/s')}
<td>C1: triangle area</td></tr>
<tr><th>Hand mean</th>{cells('hand-mean', hand_mean, 'm/s')}<td>C1: area / H</td></tr>
<tr><th>Helper mean</th>{cells('helper-mean', helper_mean, 'm/s')}
<td>power_law_depth_averaged with S1</td></tr>
</tbody>
</table>
</section>
<section>
<h2>Limits and way forward</h2>
<ul>
<li>bank effects: NOT_EVALUATED</li>
<li>force calibration: NOT_EVALUATED</li>
<li>extreme-value fitting: NOT_EVALUATED</li>
</ul>
<p>No field measurements, loading coefficients or extreme-event observations are used.
Natural-river applicability and engineering design suitability remain unevaluated.
The full module report and shared index entries remain follow-up work.</p>
</section>
</body>
</html>
"""


def main():
    """Write only the HTML artifact within the explicitly selected directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    content = render_example()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "worked-example.html").write_text(content, encoding="utf-8")


if __name__ == "__main__":
    main()
