# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Render an offline jumper HTML example from cited open lifting inputs.

All numeric inputs are defined in README-synthetic-jumper.md, Data basis.
They illustrate the reporting interface and are not solver results.
"""

import argparse
from pathlib import Path

from digitalmodel.solvers.orcaflex.reporting import (
    OrcaFlexAnalysisReport,
    generate_orcaflex_report,
)
from digitalmodel.solvers.orcaflex.reporting.models import (
    DynamicResultsData,
    GeometryData,
    KeyPointData,
    LineProfileData,
    TimeSeriesData,
)


def build_report() -> OrcaFlexAnalysisReport:
    """Construct the published input chord and length schedule without a solver."""
    # MoorDyn-C 2.3.1, tests/Mooring/lifting.txt, POINT PROPERTIES.
    # The input chord is 100 m; the unstretched line length is 100.5 m.
    profile = LineProfileData(
        arc_length=[0.0, 100.0],
        x=[0.0, 0.0],
        y=[0.0, 0.0],
        z=[0.0, -100.0],
    )
    key_points = [
        KeyPointData(label=label, arc_length_m=s, x=x, y=y, z=z)
        for label, s, x, y, z in zip(
            ("Fixed end", "Free end"),
            profile.arc_length,
            profile.x,
            profile.y,
            profile.z,
        )
    ]
    # MoorDyn-C 2.3.1, tests/lifting.cpp, lifting(): dt=1 s, lift_vel=1 m/s.
    # Initial length: tests/Mooring/lifting.txt, LINES, UnstrLen.
    # Constant-rate kinematics: OpenStax University Physics Volume 1,
    # first edition (2016), section 3.4, equation 3.10, L = L0 + rate*t.
    times = [0.0, 1.0, 2.0, 3.0, 4.0]
    initial_length = 100.5
    length_rate = -1.0
    dynamic = DynamicResultsData(
        ramp_end_time_s=0.0,
        time_series=[
            TimeSeriesData(
                id="length",
                label="Prescribed unstretched length",
                t=times,
                values=[initial_length + length_rate * t for t in times],
                units="m",
            ),
            TimeSeriesData(
                id="length-rate",
                label="Prescribed length rate",
                t=times,
                values=[length_rate] * len(times),
                units="m/s",
            ),
        ],
    )
    return OrcaFlexAnalysisReport(
        project_name="Open lifting input example",
        structure_id="Prescribed lifting line",
        structure_type="jumper",
        geometry=GeometryData(
            coordinate_system="Initial input chord, z-positive-up [m]",
            # MoorDyn-C 2.3.1, tests/Mooring/lifting.txt, OPTIONS, WtrDpth.
            water_depth_m=500.0,
            line_profile=profile,
            key_points=key_points,
        ),
        dynamic_results=dynamic,
        summary_notes=(
            "Source: MoorDyn-C 2.3.1, tests/Mooring/lifting.txt "
            "(POINT PROPERTIES, LINES, OPTIONS) and tests/lifting.cpp (lifting()). "
            "Initial input chord and prescribed length histories only; "
            "not solver results. No equilibrium geometry, tension, bending or "
            "endpoint motion is calculated. This open lifting input case "
            "illustrates the jumper renderer, not a qualified jumper design. "
            "No fitness verdict is made. Pipe properties, "
            "weight tally, rigging, crane configuration, solver section definitions, "
            "centre of gravity per kit, end rotations, go/no-go criteria and "
            "installation calculation references are unavailable. "
            "PDF export and solver "
            "extraction remain follow-up work."
        ),
        recommendations=[
            "Provide reviewed inputs and extracted results before engineering use."
        ],
    )


def render_example(output_dir: Path) -> Path:
    """Write a single HTML file with embedded CSS and Plotly JavaScript.

    Args:
        output_dir: Explicit destination for the generated report.

    Returns:
        Path of the generated HTML file.
    """
    return generate_orcaflex_report(
        build_report(),
        output_path=Path(output_dir) / "synthetic_jumper.html",
        include_plotlyjs=True,
    )


def main() -> None:
    """Parse the required destination and render the example."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        type=Path,
        required=True,
        help="Directory for the standalone synthetic HTML report",
    )
    args = parser.parse_args()
    render_example(args.output_dir)


if __name__ == "__main__":
    main()
