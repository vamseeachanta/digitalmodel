# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Render the synthetic cost arithmetic specified in issue #3308."""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.structural.offshore_resilience.minimum_facility import (
    ModularCostComparison,
)


def render_worked_example() -> str:
    """Return deterministic HTML computed with the existing cost model."""
    # Source: issue #3308 synthetic scenario, in abstract currency units.
    # The required weather-window argument is an unused zero-day placeholder;
    # neither cost property uses it, and no installation duration is evaluated.
    comparison = ModularCostComparison(
        conventional_capex_usd=100.0,
        minimum_facility_capex_usd=70.0,
        heavy_lift_vessel_savings_usd=10.0,
        installation_weather_window_days=0,
    )
    conventional = comparison.conventional_capex_usd
    minimum = comparison.minimum_facility_capex_usd
    avoided = comparison.heavy_lift_vessel_savings_usd
    savings = comparison.net_savings_usd
    fraction = comparison.savings_fraction
    return f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Offshore resilience cost arithmetic worked example</title>
</head>
<body>
<h1>Offshore resilience cost arithmetic worked example</h1>
<h2>Summary</h2>
<p>This synthetic example calls ModularCostComparison and obtains
{savings:.1f} currency units of net savings and a {fraction:.1f} dimensionless
savings fraction. These inputs are not market estimates.</p>
<h2>Revision history</h2>
<table>
<thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-25</td>
<td>Add the synthetic arithmetic example requested in issue #3308.</td></tr></tbody>
</table>
<h2>Inputs</h2>
<p>Basis: issue #3308, synthetic worked-example specification. All cost inputs
share the same abstract currency unit. The avoided installation cost is an
additional saving, excluded from the minimum-facility input, so it is added
once. The model's field names end in _usd; this example assigns abstract
currency units without monetary calibration or currency conversion.</p>
<table>
<thead><tr><th>Quantity</th><th>Value and unit</th></tr></thead>
<tbody>
<tr><td>Conventional cost</td>
<td id="conventional-cost">{conventional:.1f} currency units</td></tr>
<tr><td>Minimum-facility cost</td>
<td id="minimum-facility-cost">{minimum:.1f} currency units</td></tr>
<tr><td>Avoided installation cost</td>
<td id="avoided-installation-cost">{avoided:.1f} currency units</td></tr>
</tbody>
</table>
<p>The required installation_weather_window_days argument is set to
{comparison.installation_weather_window_days} days solely as an unused
placeholder. Neither cost property uses it; no installation duration is
assessed.</p>
<h2>Equations and results</h2>
<p>Source: ModularCostComparison.net_savings_usd and savings_fraction in
src/digitalmodel/structural/offshore_resilience/minimum_facility.py.
The arithmetic follows the independent hand calculation in issue #3308.</p>
<p>Net savings = (conventional cost - minimum-facility cost) + avoided
installation cost.</p>
<p>({conventional:.1f} - {minimum:.1f}) + {avoided:.1f} =
{savings:.1f} currency units</p>
<p>Savings fraction = net savings / conventional cost.</p>
<p>{savings:.1f} / {conventional:.1f} = {fraction:.1f} dimensionless</p>
<table>
<thead><tr><th>Computed quantity</th><th>Value and unit</th></tr></thead>
<tbody>
<tr><td>Net savings</td>
<td id="net-savings">{savings:.1f} currency units</td></tr>
<tr><td>Savings fraction</td>
<td id="savings-fraction">{fraction:.1f} dimensionless</td></tr>
</tbody>
</table>
<h2>Limits and way forward</h2>
<p>Scope: NOT_EVALUATED for platform classification, installed cost,
installation feasibility and structural adequacy. No classification rules are
invoked. This example demonstrates arithmetic only; it provides no full-module
qualification and does not complete the parent report issue.</p>
<p>A full report, shared indexes and engineering assessment remain follow-up.
Any engineering use needs a documented cost basis and separate installation
and structural assessments.</p>
</body>
</html>
"""


def main() -> None:
    """Write only the worked example into the requested output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    html = render_worked_example()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "worked_example.html").write_text(html, encoding="utf-8")


if __name__ == "__main__":
    main()
