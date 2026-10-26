# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Render an offline jumper HTML example from explicitly synthetic data.

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
    """Construct the illustrative geometry and histories without a solver."""
    # Source: companion note, Data basis, geometry and cumulative lengths.
    profile = LineProfileData(
        arc_length=[0.0, 3.0, 7.0, 10.0],
        x=[0.0, 3.0, 3.0, 6.0],
        y=[0.0, 0.0, 4.0, 4.0],
        z=[-8.0, -8.0, -8.0, -8.0],
    )
    key_points = [
        KeyPointData(label=label, arc_length_m=s, x=x, y=y, z=z)
        for label, s, x, y, z in zip(
            ("End A", "Corner A", "Corner B", "End B"),
            profile.arc_length,
            profile.x,
            profile.y,
            profile.z,
        )
    ]
    # Source: companion note, Data basis, piecewise-linear sample histories.
    times = [0.0, 1.0, 2.0, 3.0, 4.0]
    dynamic = DynamicResultsData(
        ramp_end_time_s=0.0,
        time_series=[
            TimeSeriesData(
                id="tension",
                label="Synthetic tension at End A",
                t=times,
                values=[10.0, 14.0, 10.0, 6.0, 10.0],
                units="kN",
                arc_length_m=profile.arc_length[0],
            ),
            TimeSeriesData(
                id="bending",
                label="Synthetic bending at Corner A",
                t=times,
                values=[-2.0, 0.0, 2.0, 0.0, -2.0],
                units="kN·m",
                arc_length_m=profile.arc_length[1],
            ),
        ],
    )
    return OrcaFlexAnalysisReport(
        project_name="Synthetic reporting example",
        structure_id="Illustrative jumper",
        structure_type="jumper",
        geometry=GeometryData(
            coordinate_system="Synthetic local Cartesian, z-positive-up [m]",
            water_depth_m=10.0,  # Source: companion note, Data basis.
            line_profile=profile,
            key_points=key_points,
        ),
        dynamic_results=dynamic,
        summary_notes=(
            "Synthetic geometry and prescribed time histories only; no solver "
            "analysis was performed. No fitness verdict is made. Pipe properties, "
            "weight tally, rigging, crane configuration, solver section definitions, "
            "centre of gravity per kit, end rotations, go/no-go criteria and "
            "calculation references are unavailable. PDF export and solver "
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
