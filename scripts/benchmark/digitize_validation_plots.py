#!/usr/bin/env python3
"""
Generate WAMIT reference data templates for OrcaWave validation cases.

Since reliable automated digitization of PDF plot images requires OCR/CV
libraries that may not be available, this script generates structured YAML
template files populated with the metadata known from the PDF text. The
numeric values can then be filled in by either:

  1. Running OrcaWave on the .owd files (preferred, exact)
  2. Manual digitization of the extracted plot images

Usage:
    uv run python scripts/benchmark/digitize_validation_plots.py --all
    uv run python scripts/benchmark/digitize_validation_plots.py --case 2.7
    uv run python scripts/benchmark/digitize_validation_plots.py --case 3.2
    uv run python scripts/benchmark/digitize_validation_plots.py --list

Generates reference_data.yaml in each case directory under:
    docs/modules/orcawave/L00_validation_wamit/<case>/reference_data.yaml
"""

from __future__ import annotations

import argparse
import sys
import textwrap
from pathlib import Path

import yaml

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

BASE_DIR = (
    Path(__file__).parent.parent.parent
    / "docs"
    / "modules"
    / "orcawave"
    / "L00_validation_wamit"
)

SUPPORTED_CASES = ["2.7", "2.8", "3.2", "3.3"]

# Standard heading set used by 2.7 and 2.8
HEADINGS_20DEG = [
    float(h) for h in range(0, 360, 20)
]  # 0, 20, 40, ..., 340

SIX_DOF = ["surge", "sway", "heave", "roll", "pitch", "yaw"]


# ---------------------------------------------------------------------------
# Case-specific template builders
# ---------------------------------------------------------------------------


def build_case_27() -> dict:
    """Case 2.7 - Pyramid (ZC=0.8), CS and PI mean drift loads.

    Source: OrcaWave Validation Report pp. 28-32
    Body: inverted pyramid, vertices at (0,0,-3), (-2,5,0), (8,-5,0), (-3,-5,0)
    Mesh: 408 body panels, 582 control surface panels, no symmetry
    Headings: 0-340 deg in 20 deg steps
    Periods: 4 <= T <= 8 s (chosen to avoid IRR below ~2.8 s)
    Figures: 23 (CS+PI mean drift, beta=0), 24 (convergence T=5s)
    """
    return {
        "case_id": "2.7",
        "title": "Inverted Pyramid - CS and PI Mean Drift Loads",
        "vessel_name": "Pyramid_ZC08",
        "solver": "WAMIT v7.3",
        "source": "OrcaWave Validation Report, Orcina Project 1429, revision 01",
        "source_figures": ["Figure 23 (p31)", "Figure 24 (p32)"],
        "data_type": "mean_drift",
        "water_depth": "infinite",
        "body_description": (
            "Inverted pyramid with vertices at (0,0,-3), (-2,5,0), "
            "(8,-5,0), (-3,-5,0). No symmetry planes. "
            "Non-wall-sided (oblique waterline intersection)."
        ),
        "mesh": {
            "body_panels": 408,
            "control_surface_panels": 582,
            "symmetry": "none",
            "convergence_variants": [
                "06 panels",
                "28 panels",
                "108 panels",
                "408 panels",
            ],
        },
        "headings_deg": HEADINGS_20DEG,
        "periods_s": "4.0 to 8.0 (chosen to avoid IRR below ~2.8 s)",
        "frequencies_rad_s": [],  # TBD from .owd execution
        "dofs": SIX_DOF,
        "results": {
            "pi_mean_drift": _empty_dof_block(
                "PI mean drift loads (kN/m^2 for forces, kN.m/m^2 for moments)"
            ),
            "cs_mean_drift": _empty_dof_block(
                "CS mean drift loads (kN/m^2 for forces, kN.m/m^2 for moments)"
            ),
        },
        "convergence_study": {
            "description": (
                "Figure 24: max absolute CS-PI difference vs panel count "
                "at T=5s, over all heading pairs"
            ),
            "wave_period_s": 5.0,
            "panel_counts": [],  # TBD
            "max_abs_diff_per_dof": {dof: [] for dof in SIX_DOF},
        },
        "notes": (
            "PI loads show good agreement between OrcaWave and WAMIT. "
            "CS loads agree for surge, sway, yaw but differ for heave, "
            "roll, pitch on non-wall-sided bodies. See Section 2.7.3."
        ),
        "status": "template",
        "populated_by": None,
    }


def build_case_28() -> dict:
    """Case 2.8 - Ellipsoid, CS mean drift load convergence study.

    Source: OrcaWave Validation Report pp. 33-36
    Body: ellipsoid (1/2)x^2 + y^2 + (z-z0)^2 = 1, z0=0.2m
    Symmetry: xz + yz (bi-symmetric)
    Mesh: 96 panels per quadrant (body + control surface)
    Headings: 0-340 deg in 20 deg steps
    Period: T=2s (single representative period for convergence)
    Figures: 26 (convergence T=2s), 27 (wall-sided variant z0=0)
    """
    return {
        "case_id": "2.8",
        "title": "Ellipsoid - CS Mean Drift Load Convergence Study",
        "vessel_name": "Ellipsoid_96p",
        "solver": "WAMIT v7.3",
        "source": "OrcaWave Validation Report, Orcina Project 1429, revision 01",
        "source_figures": ["Figure 26 (p35)", "Figure 27 (p36)"],
        "data_type": "mean_drift_convergence",
        "water_depth": "infinite",
        "body_description": (
            "Ellipsoid: (1/2)x^2 + y^2 + (z - z0)^2 = 1, z0 = 0.2 m. "
            "Non-wall-sided (z0 != 0). Bi-symmetric (xz + yz planes). "
            "Control surface: same shape enlarged by factor 2."
        ),
        "mesh": {
            "panels_per_quadrant": 96,
            "symmetry": "xz+yz",
            "convergence_variants": (
                "Multiple mesh densities from coarse to fine "
                "(panels per quadrant on horizontal axis of Figure 26)"
            ),
        },
        "headings_deg": HEADINGS_20DEG,
        "periods_s": [2.0],
        "frequencies_rad_s": [],  # TBD
        "dofs": SIX_DOF,
        "results": {
            "pi_mean_drift": _empty_dof_block(
                "PI mean drift loads at T=2s"
            ),
            "cs_mean_drift": _empty_dof_block(
                "CS mean drift loads at T=2s"
            ),
        },
        "convergence_study": {
            "description": (
                "Figure 26: max absolute CS-PI difference vs panels per "
                "quadrant at T=2s, over all heading pairs. "
                "Figure 27: same study for wall-sided variant (z0=0)."
            ),
            "wave_period_s": 2.0,
            "panel_counts_per_quadrant": [],  # TBD
            "max_abs_diff_per_dof": {dof: [] for dof in SIX_DOF},
        },
        "wall_sided_variant": {
            "description": (
                "z0=0 makes the ellipsoid wall-sided. "
                "Both OrcaWave and WAMIT CS loads converge to PI limit. "
                "Results in Figure 27."
            ),
            "z0": 0.0,
            "panel_counts_per_quadrant": [],
            "max_abs_diff_per_dof": {dof: [] for dof in SIX_DOF},
        },
        "notes": (
            "For non-wall-sided body (z0=0.2), WAMIT CS loads do not "
            "converge to PI limit. For wall-sided body (z0=0), both "
            "programs pass convergence test. Orcina hypothesis: WAMIT CS "
            "results incorrect for non-wall-sided bodies. See Section 2.8."
        ),
        "status": "template",
        "populated_by": None,
    }


def build_case_32() -> dict:
    """Case 3.2 - Hemisphere QTFs in shallow water.

    Source: OrcaWave Validation Report pp. 43-46
    Body: hemisphere R=1m, freely floating, depth=3m
    Mesh: 240 panels/quadrant (body + interior free surface)
    Free surface zone: panelled to r=3m (192 panels/quadrant),
        quadrature zone 3m<=r<=7m, asymptotic r>=7m
    Control surface: hemisphere shape, 80 panels/quadrant
    Heading: beta=0 deg
    QTF: difference-frequency, delta_omega=0.1 rad/s, omega1: 0.5-4.5 rad/s
    Figures: 33 (surge QTF), 34 (heave QTF)
    """
    return {
        "case_id": "3.2",
        "title": "Floating Hemisphere - Difference-Frequency QTFs",
        "vessel_name": "Sphere_R5",
        "solver": "WAMIT v6.4S",
        "source": "OrcaWave Validation Report, Orcina Project 1429, revision 01",
        "source_figures": ["Figure 33 (p45)", "Figure 34 (p46)"],
        "data_type": "qtf_difference_frequency",
        "water_depth": 3.0,
        "body_description": (
            "Hemisphere R=1m, freely floating. Based on WAMIT TEST101 case. "
            "Body mesh includes interior free surface panels for IRR removal. "
            "Bi-symmetric (xz + yz)."
        ),
        "mesh": {
            "body_panels_per_quadrant": 240,
            "control_surface_panels_per_quadrant": 80,
            "free_surface_panelled_zone": {
                "radius_m": 3.0,
                "panels_per_quadrant": 192,
            },
            "quadrature_zone": {"inner_radius_m": 3.0, "outer_radius_m": 7.0},
            "asymptotic_zone": {"inner_radius_m": 7.0},
            "symmetry": "xz+yz",
        },
        "headings_deg": [0.0],
        "qtf_parameters": {
            "type": "difference_frequency",
            "difference_frequency_rad_s": 0.1,
            "omega1_range_rad_s": [0.5, 4.5],
            "omega1_values_rad_s": [],  # TBD from .owd
        },
        "frequencies_rad_s": [],  # TBD
        "dofs": SIX_DOF,
        "results": {
            "pi_quadratic_load": _empty_dof_block(
                "PI quadratic load (kN/m^2)"
            ),
            "cs_quadratic_load": _empty_dof_block(
                "CS quadratic load (kN/m^2)"
            ),
            "direct_potential_load": _empty_dof_block(
                "Direct potential load (kN/m^2)"
            ),
            "indirect_potential_load": _empty_dof_block(
                "Indirect potential load (kN/m^2)"
            ),
        },
        "notes": (
            "Close PI/CS agreement over most of frequency range. "
            "For omega >= 3.5 rad/s, visible CS-PI difference in surge "
            "quadratic force (finer mesh probably needed). "
            "Direct and indirect potential loads also agree well. "
            "See Section 3.2."
        ),
        "status": "template",
        "populated_by": None,
    }


def build_case_33() -> dict:
    """Case 3.3 - Multi-body QTF analysis (cylinder + ellipsoid).

    Source: OrcaWave Validation Report pp. 47-50
    Bodies: vertical cylinder (fixed) + ellipsoid (freely floating)
    Same bodies as Case 2.6 but with finer vertical mesh discretization
    Symmetry: YZ plane (global mesh)
    Heading: beta1=0, beta2=30 (bidirectional QTFs)
    QTF: difference-frequency, delta_omega=2.5 rad/s, omega1: 1-4 rad/s
    Figures: 36 (cylinder full QTFs), 37 (ellipsoid full QTFs)
    """
    return {
        "case_id": "3.3",
        "title": "Multi-body Cylinder + Ellipsoid - Full QTFs",
        "vessel_names": ["Cylinder", "Spheroid"],
        "solver": "WAMIT v6.4S",
        "source": "OrcaWave Validation Report, Orcina Project 1429, revision 01",
        "source_figures": ["Figure 36 (p49)", "Figure 37 (p50)"],
        "data_type": "qtf_full_multibody",
        "water_depth": "infinite",
        "body_descriptions": {
            "Cylinder": (
                "Vertical cylinder, FIXED. Same geometry as Case 2.6 "
                "but with finer vertical mesh discretization. "
                "Mesh includes interior free surface for IRR removal."
            ),
            "Spheroid": (
                "Ellipsoid, FREELY FLOATING. Position offset from "
                "cylinder. Mesh includes interior free surface for "
                "IRR removal. Two planes of symmetry per body."
            ),
        },
        "mesh": {
            "cylinder_panels_per_quadrant": 420,
            "ellipsoid_panels_per_quadrant": 220,
            "symmetry": "yz (global)",
            "notes": (
                "Finer vertical discretization than Case 2.6 meshes. "
                "Model rotated 90 deg from WAMIT TEST103a to use YZ "
                "symmetry."
            ),
        },
        "headings_deg": {
            "beta1": 0.0,
            "beta2": 30.0,
        },
        "qtf_parameters": {
            "type": "difference_frequency",
            "difference_frequency_rad_s": 2.5,
            "omega1_range_rad_s": [1.0, 4.0],
            "omega1_values_rad_s": [],  # TBD from .owd
        },
        "frequencies_rad_s": [],  # TBD
        "dofs": SIX_DOF,
        "results": {
            "cylinder_full_qtf": _empty_dof_block(
                "Full QTFs for cylinder (PI quadratic + indirect potential)"
            ),
            "ellipsoid_full_qtf": _empty_dof_block(
                "Full QTFs for ellipsoid (PI quadratic + indirect potential)"
            ),
        },
        "notes": (
            "Full QTFs = PI quadratic load + indirect potential load. "
            "Excellent agreement between OrcaWave and WAMIT for both bodies. "
            "Bidirectional QTFs validated (beta1=0, beta2=30). "
            "12 total DOFs (6 per body). See Section 3.3."
        ),
        "status": "template",
        "populated_by": None,
    }


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _empty_dof_block(description: str) -> dict:
    """Create an empty DOF result block with magnitude and phase arrays."""
    block: dict = {"description": description}
    for dof in SIX_DOF:
        block[dof] = {
            "magnitude": [],
            "phase": [],
        }
    return block


CASE_BUILDERS: dict[str, callable] = {
    "2.7": build_case_27,
    "2.8": build_case_28,
    "3.2": build_case_32,
    "3.3": build_case_33,
}


def _yaml_header(case_id: str, data: dict) -> str:
    """Build a YAML comment header with provenance and instructions."""
    title = data.get("title", f"Case {case_id}")
    data_type = data.get("data_type", "unknown")
    figures = data.get("source_figures", [])
    fig_str = ", ".join(figures) if figures else "N/A"

    return textwrap.dedent(f"""\
        # WAMIT Reference Data for Case {case_id} - {title}
        # Source: OrcaWave Validation Report, Project 1429, revision 01
        # Data type: {data_type}
        #
        # Status: TEMPLATE - numeric values require population via:
        #   1. OrcaWave .owd execution (preferred, exact values)
        #   2. Manual digitization from {fig_str}
        #
        # To populate with OrcaWave:
        #   Run the .owd file in the 'OrcaWave v11.0 files/' subdirectory,
        #   extract results, and fill in the empty arrays below.
        #   Then set status: 'populated' and populated_by: 'owd_execution'
        #
    """)


def write_reference_data(case_id: str, dry_run: bool = False) -> Path:
    """Generate and write a reference_data.yaml for the given case."""
    builder = CASE_BUILDERS[case_id]
    data = builder()

    case_dir = BASE_DIR / case_id
    if not case_dir.is_dir():
        print(f"  WARNING: Case directory does not exist: {case_dir}")
        case_dir.mkdir(parents=True, exist_ok=True)

    out_path = case_dir / "reference_data.yaml"
    header = _yaml_header(case_id, data)

    yaml_content = yaml.dump(
        data,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
        width=100,
    )

    full_content = header + yaml_content

    if dry_run:
        print(f"\n--- {out_path} (dry run) ---")
        # Print first 30 lines
        lines = full_content.splitlines()
        for line in lines[:30]:
            print(f"  {line}")
        if len(lines) > 30:
            print(f"  ... ({len(lines) - 30} more lines)")
    else:
        out_path.write_text(full_content, encoding="utf-8")
        print(f"  Written: {out_path}")

    return out_path


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Generate WAMIT reference data YAML templates for "
            "OrcaWave validation cases."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--case",
        type=str,
        choices=SUPPORTED_CASES,
        help="Generate template for a specific case",
    )
    group.add_argument(
        "--all",
        action="store_true",
        help="Generate templates for all 4 Phase 1 cases",
    )
    group.add_argument(
        "--list",
        action="store_true",
        help="List supported cases and exit",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print template content without writing files",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    if args.list:
        print("Supported validation cases:")
        print(f"{'Case':>6}  {'Type':>30}  {'Solver':>12}")
        print(f"{'-' * 6}  {'-' * 30}  {'-' * 12}")
        for case_id, builder in CASE_BUILDERS.items():
            data = builder()
            print(
                f"  {case_id:4s}  {data['data_type']:>30s}  {data['solver']:>12s}"
            )
        return 0

    cases = SUPPORTED_CASES if args.all else [args.case]
    written = []

    print("Generating WAMIT reference data templates")
    print(f"Base directory: {BASE_DIR}")
    if args.dry_run:
        print("(DRY RUN - no files will be written)\n")
    print()

    for case_id in cases:
        print(f"Case {case_id}:")
        path = write_reference_data(case_id, dry_run=args.dry_run)
        written.append((case_id, path))

    print(f"\nSummary: {len(written)} template(s) {'previewed' if args.dry_run else 'written'}")
    for case_id, path in written:
        data = CASE_BUILDERS[case_id]()
        print(f"  {case_id}: {data['title']}")

    if not args.dry_run:
        print(
            "\nNext steps: run OrcaWave on the .owd files to populate "
            "the empty arrays, then set status to 'populated'."
        )

    return 0


if __name__ == "__main__":
    sys.exit(main())
