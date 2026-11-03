# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Worked synthetic spring example; writes results only to an explicit directory.

All numerical geometry, stiffness, wind data and validity bounds are synthetic
arithmetic inputs selected for simple hand calculations. They are not
material data or design recommendations. The expected wind-only solution uses
OpenStax University Physics Volume 1 (2016), 12.1/15.1 and NASA Drag Equation
(2021, updated 2025); see docs/domains/hydrodynamics/passing-ship-response.md.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

from digitalmodel.hydrodynamics.passing_ship import (
    AnalysisCase,
    EnvironmentSpec,
    HullSpec,
    MooredVesselSpec,
    MooringLineSpec,
    PassingShipDimensions,
    PassingShipScenario,
    PassingShipSpec,
    PassingTrack,
    ResponseSettings,
    WindLoadSpec,
    run_analysis_cases,
)
from digitalmodel.hydrodynamics.passing_ship.response_report import response_sections


def example_spec() -> PassingShipSpec:
    """Build the six-spring hand-calculation arrangement in SI units."""
    lines = []
    for label, x, nx, ny in [
        ("surge-positive", 0, 1, 0),
        ("surge-negative", 0, -1, 0),
        ("bow-positive", 2, 0, 1),
        ("bow-negative", 2, 0, -1),
        ("aft-positive", -2, 0, 1),
        ("aft-negative", -2, 0, -1),
    ]:
        lines.append(
            MooringLineSpec(
                line_id=label,
                line_type="synthetic",
                length=20,
                pretension=10,
                fairlead_x=x,
                fairlead_y=0,
                anchor_x=x - 20 * nx,
                anchor_y=-20 * ny,
                horizontal_stiffness_n_m=1000,
            )
        )
    return PassingShipSpec(
        name="synthetic-spring-response",
        moored_vessel=MooredVesselSpec(
            hull=HullSpec(loa=10, beam=2, draft=1, block_coefficient=0.75),
            mooring_lines=lines,
            wind_load=WindLoadSpec(
                air_density_kg_m3=1,
                frontal_area_m2=4,
                lateral_area_m2=8,
                surge_coefficient=1,
                sway_coefficient=1,
                centre_of_pressure_x_m=2,
                centre_of_pressure_y_m=1,
            ),
        ),
        passing_ship=PassingShipScenario(
            dimensions=PassingShipDimensions(
                loa=8, beam=2, draft=1, block_coefficient=0.75
            ),
            track=PassingTrack(speed_knots=2, separation_m=20),
        ),
        environment=EnvironmentSpec(wind_speed=10, wind_direction_deg=0),
        response_settings=ResponseSettings(
            max_translation_m=1,
            max_yaw_rad=0.1,
            force_tolerance_n=1e-7,
            moment_tolerance_nm=1e-7,
        ),
        analysis_cases=[
            AnalysisCase(name="still", include_passing_ship=False),
            AnalysisCase(name="wind", include_passing_ship=False, include_wind=True),
            AnalysisCase(name="passing"),
            AnalysisCase(name="wind-and-passing", include_wind=True),
            AnalysisCase(
                name="faster-and-closer", passing_speed_knots=4, separation_m=10
            ),
        ],
    )


def run_example(output_dir: Path):
    """Export the input, response CSV and standalone HTML for five cases."""
    spec = example_spec()
    # Explicit illustration grid, not a complete passage event or a time-step rule.
    results = run_analysis_cases(spec, time_s=[0, 1, 2], start_stagger_m=0)
    output_dir.mkdir(parents=True, exist_ok=True)
    spec.to_yaml(output_dir / "spec.yml")
    columns = ["case", "time_s", "surge_m", "sway_m", "yaw_rad"]
    columns += [
        f"{line.line_id}_tension_n" for line in spec.moored_vessel.mooring_lines
    ]
    with (output_dir / "response.csv").open("w", encoding="utf-8", newline="\n") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(columns)
        for case in results:
            for i, time in enumerate(case.time):
                writer.writerow(
                    [
                        case.name,
                        time,
                        *case.motions[i],
                        *(v[i] for v in case.line_tensions_n.values()),
                    ]
                )
    # Embed the plotting library so the result is viewable without network access.
    from plotly.offline import get_plotlyjs

    html = (
        '<!doctype html><html lang="en"><meta charset="utf-8">'
        "<title>Passing ship response analysis</title>"
        f"<script>{get_plotlyjs()}</script><body>"
        f"{response_sections(results)}</body></html>"
    )
    (output_dir / "response.html").write_text(html, encoding="utf-8", newline="\n")
    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    run_example(parser.parse_args().output_dir)
