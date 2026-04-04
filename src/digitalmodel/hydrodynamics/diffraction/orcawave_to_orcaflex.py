"""
ABOUTME: Automated OrcaWave → OrcaFlex handoff pipeline.

Single-command pipeline that takes an OrcaWave .xlsx results file and
produces an OrcaFlex-compatible vessel type YAML + CSV data files.

Flow:
  .xlsx → RAOData extraction → DiffractionResults → OrcaFlex export

Does NOT require OrcFxAPI — reads xlsx sidecars generated on licensed-win-1.

Usage:
    # As library
    from digitalmodel.hydrodynamics.diffraction.orcawave_to_orcaflex import (
        convert_orcawave_xlsx_to_orcaflex,
    )
    outputs = convert_orcawave_xlsx_to_orcaflex(
        xlsx_path="fixtures/test01_unit_box.xlsx",
        output_dir="output/orcaflex",
    )

    # As CLI
    uv run python -m digitalmodel.hydrodynamics.diffraction.orcawave_to_orcaflex \\
        fixtures/test01_unit_box.xlsx -o output/orcaflex

Traceability: #1768, #1592
"""
from __future__ import annotations

import argparse
import sys
from datetime import datetime
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import (
    OrcaFlexExporter,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    RAOComponent,
    RAOSet,
)
from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    HydroCoefficients,
    xlsx_to_hydro_coefficients,
    xlsx_to_rao_data,
)
from digitalmodel.hydrodynamics.models import RAOData


# ---------------------------------------------------------------------------
# Bridge: RAOData + HydroCoefficients → DiffractionResults
# ---------------------------------------------------------------------------


def rao_data_to_diffraction_results(
    rao: RAOData,
    coeffs: HydroCoefficients | None = None,
    water_depth: float = 100.0,
    analysis_tool: str = "OrcaWave",
    source_file: str | None = None,
) -> DiffractionResults:
    """Convert RAOData + HydroCoefficients to the unified DiffractionResults schema.

    This bridges the rao_extractor output to the OrcaFlexExporter input,
    enabling the full pipeline without OrcFxAPI.

    Parameters
    ----------
    rao : RAOData
        6-DOF RAO amplitudes and phases from xlsx extraction.
    coeffs : HydroCoefficients, optional
        Added mass and damping matrices. If None, zero-filled matrices are used.
    water_depth : float
        Water depth in meters (default 100.0).
    analysis_tool : str
        Source solver name (default "OrcaWave").
    source_file : str, optional
        Path to the source xlsx file for metadata.

    Returns
    -------
    DiffractionResults
        Populated schema ready for OrcaFlexExporter.
    """
    freq_data = FrequencyData(
        values=rao.frequencies,
        periods=2.0 * np.pi / rao.frequencies,
        count=len(rao.frequencies),
        min_freq=float(np.min(rao.frequencies)),
        max_freq=float(np.max(rao.frequencies)),
    )
    heading_data = HeadingData(
        values=rao.directions,
        count=len(rao.directions),
        min_heading=float(np.min(rao.directions)),
        max_heading=float(np.max(rao.directions)),
    )

    dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
    components: dict[DOF, RAOComponent] = {}
    for i, dof in enumerate(dof_list):
        unit = "m/m" if i < 3 else "deg/m"
        components[dof] = RAOComponent(
            dof=dof,
            magnitude=rao.amplitudes[:, :, i],
            phase=rao.phases[:, :, i],
            frequencies=freq_data,
            headings=heading_data,
            unit=unit,
        )

    rao_set = RAOSet(
        vessel_name=rao.vessel_name,
        analysis_tool=analysis_tool,
        water_depth=water_depth,
        surge=components[DOF.SURGE],
        sway=components[DOF.SWAY],
        heave=components[DOF.HEAVE],
        roll=components[DOF.ROLL],
        pitch=components[DOF.PITCH],
        yaw=components[DOF.YAW],
        created_date=datetime.now().strftime("%Y-%m-%d"),
        source_file=source_file,
    )

    # Build added mass / damping sets
    if coeffs is not None:
        am_freq = FrequencyData(
            values=coeffs.frequencies,
            periods=2.0 * np.pi / coeffs.frequencies,
            count=len(coeffs.frequencies),
            min_freq=float(np.min(coeffs.frequencies)),
            max_freq=float(np.max(coeffs.frequencies)),
        )
        am_matrices = [
            HydrodynamicMatrix(
                matrix=coeffs.added_mass[i],
                frequency=float(coeffs.frequencies[i]),
                matrix_type="added_mass",
                units={"linear": "kg", "angular": "kg.m^2"},
            )
            for i in range(len(coeffs.frequencies))
        ]
        damp_matrices = [
            HydrodynamicMatrix(
                matrix=coeffs.damping[i],
                frequency=float(coeffs.frequencies[i]),
                matrix_type="damping",
                units={"linear": "N.s/m", "angular": "N.m.s/rad"},
            )
            for i in range(len(coeffs.frequencies))
        ]
        added_mass_set = AddedMassSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=am_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )
        damping_set = DampingSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=am_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )
    else:
        # DiffractionResults requires non-None sets — fill with zeros
        zero_freq = FrequencyData(
            values=rao.frequencies,
            periods=2.0 * np.pi / rao.frequencies,
            count=len(rao.frequencies),
            min_freq=float(np.min(rao.frequencies)),
            max_freq=float(np.max(rao.frequencies)),
        )
        zero_am = [
            HydrodynamicMatrix(
                matrix=np.zeros((6, 6)),
                frequency=float(f),
                matrix_type="added_mass",
                units={"linear": "kg"},
            )
            for f in rao.frequencies
        ]
        zero_damp = [
            HydrodynamicMatrix(
                matrix=np.zeros((6, 6)),
                frequency=float(f),
                matrix_type="damping",
                units={"linear": "N.s/m"},
            )
            for f in rao.frequencies
        ]
        added_mass_set = AddedMassSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=zero_am,
            frequencies=zero_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )
        damping_set = DampingSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=zero_damp,
            frequencies=zero_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )

    source_files = [source_file] if source_file else None

    return DiffractionResults(
        vessel_name=rao.vessel_name,
        analysis_tool=analysis_tool,
        water_depth=water_depth,
        raos=rao_set,
        added_mass=added_mass_set,
        damping=damping_set,
        created_date=datetime.now().strftime("%Y-%m-%d"),
        source_files=source_files,
    )


# ---------------------------------------------------------------------------
# Main pipeline function
# ---------------------------------------------------------------------------


def convert_orcawave_xlsx_to_orcaflex(
    xlsx_path: str | Path,
    output_dir: str | Path,
    vessel_name: str | None = None,
    water_depth: float = 100.0,
) -> dict[str, Path]:
    """Convert an OrcaWave .xlsx results file to OrcaFlex vessel type files.

    This is the single-command entry point for the automated handoff pipeline.

    Parameters
    ----------
    xlsx_path : str or Path
        Path to OrcaWave .xlsx file (pipeline or native format).
    output_dir : str or Path
        Directory for output files (created if needed).
    vessel_name : str, optional
        Override the vessel name. Defaults to xlsx filename stem.
    water_depth : float
        Water depth in meters (default 100.0).

    Returns
    -------
    dict[str, Path]
        Mapping of output type to file path:
        - 'vessel_type': YAML vessel type file
        - 'rao_csv': RAO CSV file
        - 'added_mass_csv': Added mass CSV
        - 'damping_csv': Damping CSV
        - 'excel': Hydrodynamics Excel workbook
        - 'summary': Text summary report
    """
    xlsx_path = Path(xlsx_path)
    output_dir = Path(output_dir)

    # Step 1: Extract RAO data and hydro coefficients from xlsx
    rao = xlsx_to_rao_data(xlsx_path, vessel_name=vessel_name)
    coeffs = xlsx_to_hydro_coefficients(xlsx_path)

    # Step 2: Convert to unified DiffractionResults
    results = rao_data_to_diffraction_results(
        rao=rao,
        coeffs=coeffs,
        water_depth=water_depth,
        source_file=str(xlsx_path),
    )

    # Step 3: Export to OrcaFlex formats
    exporter = OrcaFlexExporter(results, output_dir)
    outputs = exporter.export_all()

    return outputs


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="orcawave_to_orcaflex",
        description="Convert OrcaWave .xlsx results to OrcaFlex vessel type files.",
    )
    parser.add_argument(
        "xlsx_path",
        type=Path,
        help="Path to OrcaWave .xlsx results file",
    )
    parser.add_argument(
        "-o", "--output-dir",
        type=Path,
        default=Path("output/orcaflex"),
        help="Output directory (default: output/orcaflex)",
    )
    parser.add_argument(
        "--vessel-name",
        type=str,
        default=None,
        help="Override vessel name (default: xlsx filename stem)",
    )
    parser.add_argument(
        "--water-depth",
        type=float,
        default=100.0,
        help="Water depth in meters (default: 100.0)",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    """CLI entry point."""
    parser = _build_parser()
    args = parser.parse_args(argv)

    if not args.xlsx_path.exists():
        print(f"ERROR: File not found: {args.xlsx_path}", file=sys.stderr)
        return 1

    try:
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=args.xlsx_path,
            output_dir=args.output_dir,
            vessel_name=args.vessel_name,
            water_depth=args.water_depth,
        )
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1

    print(f"OrcaWave → OrcaFlex conversion complete:")
    print(f"  Source: {args.xlsx_path}")
    print(f"  Output: {args.output_dir}")
    for key, path in outputs.items():
        size_kb = path.stat().st_size / 1024
        print(f"    {key}: {path.name} ({size_kb:.1f} KB)")

    return 0


if __name__ == "__main__":
    sys.exit(main())


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "convert_orcawave_xlsx_to_orcaflex",
    "rao_data_to_diffraction_results",
]
