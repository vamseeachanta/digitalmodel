"""OrcaFlex format converter.

Converts DiffractionResults to OrcaFlex-compatible formats including:
- Vessel type YAML files
- CSV data files for RAOs and coefficients
- QTF CSV files for second-order wave forces
"""

import csv
from pathlib import Path
from typing import Any, Dict, List, Optional

import yaml
from loguru import logger

from digitalmodel.modules.diffraction import DiffractionResults, OrcaFlexExporter
from .base import BaseConverter
from ..core.exceptions import ConverterError
from ..models import ConversionResult, QTFData
from ..models.conversion_result import ConversionStatus


class OrcaFlexConverter(BaseConverter):
    """Convert DiffractionResults to OrcaFlex-compatible formats.

    This converter integrates with the existing OrcaFlexExporter from the
    diffraction module to produce vessel type YAML files and coefficient
    CSV files. It also supports QTF (Quadratic Transfer Function) export
    for second-order wave forces.

    Attributes:
        vessel_name: Custom vessel name to use in output files.
        include_qtf: Whether to export QTF data if available.
    """

    @property
    def output_format(self) -> str:
        """Return the output format identifier."""
        return "orcaflex"

    def __init__(
        self,
        output_dir: Optional[Path] = None,
        vessel_name: Optional[str] = None,
        include_qtf: bool = True,
    ):
        """Initialize the OrcaFlex converter.

        Args:
            output_dir: Directory for output files.
            vessel_name: Custom vessel name. If not provided, uses the
                vessel name from the DiffractionResults.
            include_qtf: Whether to export QTF data if available.
                Defaults to True.
        """
        super().__init__(output_dir)
        self.vessel_name = vessel_name
        self.include_qtf = include_qtf
        self._qtf_data: Optional[QTFData] = None

    def set_qtf_data(self, qtf: QTFData) -> None:
        """Set QTF data for export.

        Args:
            qtf: QTFData model containing second-order wave force
                coefficients.
        """
        self._qtf_data = qtf

    def convert(
        self,
        results: DiffractionResults,
        output_path: Optional[Path] = None,
    ) -> Path:
        """Convert DiffractionResults to OrcaFlex format.

        Uses the existing OrcaFlexExporter to generate vessel type YAML
        and coefficient CSV files. Optionally exports QTF data if available.

        Args:
            results: DiffractionResults to convert.
            output_path: Optional override for output directory.

        Returns:
            Path to the output directory containing all generated files.

        Raises:
            ConverterError: If conversion fails.
        """
        output_dir = Path(output_path) if output_path else self.output_dir
        self.output_dir = output_dir
        self._ensure_output_dir()

        # Validate input and log warnings
        warnings = self.validate_input(results)
        if warnings:
            for w in warnings:
                logger.warning(w)

        # Determine vessel name
        vessel_name = self.vessel_name or results.vessel_name or "vessel"

        # Use existing OrcaFlexExporter for core export
        try:
            self._call_orcaflex_exporter(results, output_dir)

            # Export QTF if available
            if self._qtf_data and self.include_qtf:
                qtf_path = self._export_qtf(output_dir, vessel_name)
                logger.info(f"Exported QTF: {qtf_path}")

            return output_dir

        except ConverterError:
            raise
        except Exception as e:
            raise ConverterError(
                f"OrcaFlex conversion failed: {e}",
                source_format="DiffractionResults",
                target_format="OrcaFlex",
            ) from e

    def _call_orcaflex_exporter(
        self, results: DiffractionResults, output_dir: Path
    ) -> None:
        """Call the OrcaFlexExporter to generate output files.

        Args:
            results: DiffractionResults to export.
            output_dir: Directory for output files.
        """
        exporter = OrcaFlexExporter(results, output_dir)

        # Export vessel type YAML
        vessel_yaml_path = exporter.export_vessel_type()
        logger.info(f"Exported vessel type: {vessel_yaml_path}")

        # Export RAO CSV files
        rao_path = exporter.export_raos_csv()
        logger.info(f"Exported RAO file: {rao_path}")

        # Export added mass/damping CSV
        exporter.export_added_mass_csv()
        exporter.export_damping_csv()

    def convert_to_vessel_yaml(
        self,
        results: DiffractionResults,
        output_file: Path,
    ) -> Path:
        """Export just the vessel type YAML file.

        Creates an OrcaFlex-compatible vessel type YAML file without
        the full coefficient export.

        Args:
            results: DiffractionResults to convert.
            output_file: Path for the output YAML file.

        Returns:
            Path to the created YAML file.
        """
        vessel_name = self.vessel_name or results.vessel_name or "vessel"

        vessel_data = self._build_vessel_yaml(results, vessel_name)

        with open(output_file, "w") as f:
            yaml.dump(vessel_data, f, default_flow_style=False, sort_keys=False)

        return output_file

    def _build_vessel_yaml(
        self,
        results: DiffractionResults,
        vessel_name: str,
    ) -> Dict[str, Any]:
        """Build OrcaFlex vessel type YAML structure.

        Creates a dictionary structure compatible with OrcaFlex vessel
        type YAML format.

        Args:
            results: DiffractionResults for vessel configuration.
            vessel_name: Name to use for the vessel.

        Returns:
            Dictionary structure for YAML serialization.
        """
        wave_drift = "None" if self._qtf_data is None else "Full QTF"

        return {
            "VesselType": {
                "Name": vessel_name,
                "Category": "Vessel",
                "Connection": "Free",
                "PrimaryMotion": "Calculated (6 DOF)",
                "SuperimposedMotion": "None",
                "IncludedEffects": {
                    "AddedMassAndDamping": "Yes",
                    "OtherDamping": "None",
                    "WaveDrift": wave_drift,
                },
                "RAOOrigin": [0, 0, 0],
                "RAOPhaseConvention": "AQWA",
            }
        }

    def _export_qtf(self, output_dir: Path, vessel_name: str) -> Path:
        """Export QTF data to OrcaFlex format.

        Creates a CSV file with QTF coefficients in OrcaFlex-compatible
        format with frequency pairs and complex coefficients for each DOF.

        Args:
            output_dir: Directory for the output file.
            vessel_name: Vessel name for the output filename.

        Returns:
            Path to the created QTF CSV file.

        Raises:
            ConverterError: If no QTF data has been set.
        """
        if self._qtf_data is None:
            raise ConverterError("No QTF data set")

        qtf_file = output_dir / f"{vessel_name}_qtf.csv"

        # OrcaFlex QTF format: frequency pairs with complex coefficients
        with open(qtf_file, "w", newline="") as f:
            writer = csv.writer(f)

            # Header
            header = ["Freq1_rad/s", "Freq2_rad/s", "Heading_deg"]
            dof_names = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
            for dof_name in dof_names:
                header.extend([f"{dof_name}_Re", f"{dof_name}_Im"])
            writer.writerow(header)

            # Data - iterate over components grouped by frequency pairs
            self._write_qtf_data(writer)

        return qtf_file

    def _write_qtf_data(self, writer: csv.writer) -> None:
        """Write QTF data rows to CSV writer.

        Args:
            writer: CSV writer instance.
        """
        qtf = self._qtf_data

        # Get unique frequencies from the components
        freq_1 = qtf.frequencies
        freq_2 = qtf.frequencies
        headings = qtf.headings

        # For each frequency pair and heading, collect DOF data
        for i, f1 in enumerate(freq_1):
            for j, f2 in enumerate(freq_2):
                for heading in headings:
                    row = [f1, f2, heading]

                    # Collect data for each DOF
                    for dof in range(6):
                        component = qtf.get_component(dof, heading)
                        if component is not None:
                            # Get amplitude and phase at this frequency pair
                            amplitude = component.amplitude[i, j]
                            phase = component.phase[i, j]

                            # Convert to real/imaginary
                            import numpy as np

                            real = amplitude * np.cos(phase)
                            imag = amplitude * np.sin(phase)
                            row.extend([real, imag])
                        else:
                            row.extend([0.0, 0.0])

                    writer.writerow(row)


def convert_to_orcaflex(
    results: DiffractionResults,
    output_dir: Path,
    qtf_data: Optional[QTFData] = None,
) -> ConversionResult:
    """Convenience function to convert DiffractionResults to OrcaFlex format.

    Args:
        results: DiffractionResults to convert.
        output_dir: Directory for output files.
        qtf_data: Optional QTF data to include in export.

    Returns:
        ConversionResult with status and output information.
    """
    converter = OrcaFlexConverter(output_dir=output_dir)

    if qtf_data:
        converter.set_qtf_data(qtf_data)

    try:
        output_path = converter.convert(results)
        return ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="DiffractionResults",
            target_format="orcaflex",
            primary_output=output_path,
        )
    except Exception as e:
        result = ConversionResult(
            status=ConversionStatus.FAILED,
            source_format="DiffractionResults",
            target_format="orcaflex",
        )
        result.add_error(str(e))
        return result
