#!/usr/bin/env python3
"""
OrcaFlex Export Module

Converts unified diffraction output schemas to OrcaFlex-compatible formats:
- Vessel type YAML files
- CSV data files for RAOs and coefficients
- Excel workbooks with formatted tables
"""

import yaml
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, Optional
from datetime import datetime

from digitalmodel.hydrodynamics.diffraction.diffraction_units import rad_per_s_to_period_s
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults, RAOSet, AddedMassSet, DampingSet,
    DOF, RAOComponent, HydrodynamicMatrix
)


class OrcaFlexExporter:
    """Export diffraction results to OrcaFlex formats"""

    def __init__(self, results: DiffractionResults, output_dir: Path):
        """
        Initialize exporter

        Args:
            results: Unified diffraction results
            output_dir: Directory for output files
        """
        self.results = results
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        self.vessel_name = results.vessel_name.replace(" ", "_")

    def export_all(self) -> Dict[str, Path]:
        """
        Export all results to OrcaFlex formats

        Returns:
            Dictionary mapping output type to file path
        """
        outputs = {}

        # Export vessel type YAML
        outputs['vessel_type'] = self.export_vessel_type()

        # Export RAO CSV files
        outputs['rao_csv'] = self.export_raos_csv()

        # Export added mass CSV
        outputs['added_mass_csv'] = self.export_added_mass_csv()

        # Export damping CSV
        outputs['damping_csv'] = self.export_damping_csv()

        # Export Excel workbook
        outputs['excel'] = self.export_excel_workbook()

        # Export summary report
        outputs['summary'] = self.export_summary_report()

        return outputs

    def export_vessel_type(self) -> Path:
        """
        Export OrcaFlex vessel type YAML file

        Returns:
            Path to created vessel type file
        """
        output_file = self.output_dir / f"{self.vessel_name}_vessel_type.yml"

        vessel_type = {
            'VesselType': {
                'Name': self.vessel_name,
                'WaterDepth': self.results.water_depth,
                'DiffractionSource': self.results.analysis_tool,
                'CreatedDate': self.results.created_date,

                # RAO data references
                'RAODataFile': f"{self.vessel_name}_raos.csv",

                # Added mass and damping data
                'AddedMassDataFile': f"{self.vessel_name}_added_mass.csv",
                'DampingDataFile': f"{self.vessel_name}_damping.csv",

                # Metadata
                'Notes': self.results.notes or f"Generated from {self.results.analysis_tool} analysis"
            }
        }

        with open(output_file, 'w') as f:
            yaml.dump(vessel_type, f, default_flow_style=False, sort_keys=False)

        print(f"Vessel type exported: {output_file}")
        return output_file

    def export_raos_csv(self) -> Path:
        """
        Export RAOs to CSV format compatible with OrcaFlex

        CSV format:
        - Rows: Frequencies
        - Columns: Headings for each DOF (magnitude and phase)

        Returns:
            Path to created CSV file
        """
        output_file = self.output_dir / f"{self.vessel_name}_raos.csv"

        raos = self.results.raos
        freqs = raos.surge.frequencies.values
        periods = raos.surge.frequencies.periods
        headings = raos.surge.headings.values

        # Build multi-level column headers
        columns = ['Frequency (rad/s)', 'Period (s)']

        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        for heading in headings:
            for dof in dof_names:
                columns.append(f"{dof}_Mag_H{heading:.1f}")
                columns.append(f"{dof}_Phase_H{heading:.1f}")

        # Initialize data array
        nfreq = len(freqs)
        nhead = len(headings)
        ncols = 2 + 6 * 2 * nhead  # freq + period + 6 DOFs * 2 (mag+phase) * nheadings

        data = np.zeros((nfreq, ncols))
        data[:, 0] = freqs
        data[:, 1] = periods

        # Fill in RAO data
        col_idx = 2
        for h_idx, heading in enumerate(headings):
            for dof_idx, dof_name in enumerate(dof_names):
                dof_enum = DOF[dof_name.upper()]
                component = raos.get_component(dof_enum)

                # Magnitude
                data[:, col_idx] = component.magnitude[:, h_idx]
                col_idx += 1

                # Phase
                data[:, col_idx] = component.phase[:, h_idx]
                col_idx += 1

        # Create DataFrame and export
        df = pd.DataFrame(data, columns=columns)
        df.to_csv(output_file, index=False, float_format='%.6f')

        print(f"RAO CSV exported: {output_file}")
        return output_file

    def export_added_mass_csv(self) -> Path:
        """
        Export added mass matrices to CSV

        Format: One matrix per frequency with labeled rows/columns

        Returns:
            Path to created CSV file
        """
        output_file = self.output_dir / f"{self.vessel_name}_added_mass.csv"

        am_set = self.results.added_mass

        # Create long-form CSV with frequency, DOF_i, DOF_j, value
        rows = []

        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        for matrix in am_set.matrices:
            freq = matrix.frequency
            for i, dof_i in enumerate(dof_names):
                for j, dof_j in enumerate(dof_names):
                    rows.append({
                        'Frequency (rad/s)': freq,
                        'Period (s)': rad_per_s_to_period_s(freq),
                        'DOF_i': dof_i,
                        'DOF_j': dof_j,
                        'AddedMass': matrix.matrix[i, j],
                        'Unit': self._get_added_mass_unit(i, j)
                    })

        df = pd.DataFrame(rows)
        df.to_csv(output_file, index=False, float_format='%.6e')

        print(f"Added mass CSV exported: {output_file}")
        return output_file

    def export_damping_csv(self) -> Path:
        """
        Export damping matrices to CSV

        Returns:
            Path to created CSV file
        """
        output_file = self.output_dir / f"{self.vessel_name}_damping.csv"

        damp_set = self.results.damping

        # Create long-form CSV
        rows = []

        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        for matrix in damp_set.matrices:
            freq = matrix.frequency
            for i, dof_i in enumerate(dof_names):
                for j, dof_j in enumerate(dof_names):
                    rows.append({
                        'Frequency (rad/s)': freq,
                        'Period (s)': rad_per_s_to_period_s(freq),
                        'DOF_i': dof_i,
                        'DOF_j': dof_j,
                        'Damping': matrix.matrix[i, j],
                        'Unit': self._get_damping_unit(i, j)
                    })

        df = pd.DataFrame(rows)
        df.to_csv(output_file, index=False, float_format='%.6e')

        print(f"Damping CSV exported: {output_file}")
        return output_file

    def export_excel_workbook(self) -> Path:
        """
        Export comprehensive Excel workbook with all results

        Sheets:
        - Summary: Overview and metadata
        - RAOs: All RAO data
        - AddedMass: Added mass matrices
        - Damping: Damping matrices
        - Frequencies: Frequency discretization

        Returns:
            Path to created Excel file
        """
        output_file = self.output_dir / f"{self.vessel_name}_hydrodynamics.xlsx"

        with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
            # Summary sheet
            self._write_summary_sheet(writer)

            # RAO data sheet
            self._write_rao_sheet(writer)

            # Added mass sheet
            self._write_added_mass_sheet(writer)

            # Damping sheet
            self._write_damping_sheet(writer)

            # Frequency/heading info sheet
            self._write_discretization_sheet(writer)

        print(f"Excel workbook exported: {output_file}")
        return output_file

    def export_summary_report(self) -> Path:
        """
        Export text summary report

        Returns:
            Path to created report file
        """
        output_file = self.output_dir / f"{self.vessel_name}_summary.txt"

        lines = []
        lines.append("=" * 80)
        lines.append(f"Diffraction Analysis Results Summary")
        lines.append("=" * 80)
        lines.append(f"Vessel: {self.results.vessel_name}")
        lines.append(f"Analysis Tool: {self.results.analysis_tool}")
        lines.append(f"Water Depth: {self.results.water_depth} m")
        lines.append(f"Created: {self.results.created_date}")
        lines.append("=" * 80)
        lines.append("")

        # Frequency coverage
        freqs = self.results.raos.surge.frequencies
        lines.append("Frequency Coverage:")
        lines.append(f"  Count: {freqs.count}")
        lines.append(f"  Range: {freqs.min_freq:.4f} - {freqs.max_freq:.4f} rad/s")
        lines.append(f"  Periods: {2*np.pi/freqs.max_freq:.2f} - {2*np.pi/freqs.min_freq:.2f} s")
        lines.append("")

        # Heading coverage
        heads = self.results.raos.surge.headings
        lines.append("Heading Coverage:")
        lines.append(f"  Count: {heads.count}")
        lines.append(f"  Range: {heads.min_heading:.1f} - {heads.max_heading:.1f} deg")
        lines.append("")

        # Output files
        lines.append("Generated Files:")
        lines.append(f"  - {self.vessel_name}_vessel_type.yml")
        lines.append(f"  - {self.vessel_name}_raos.csv")
        lines.append(f"  - {self.vessel_name}_added_mass.csv")
        lines.append(f"  - {self.vessel_name}_damping.csv")
        lines.append(f"  - {self.vessel_name}_hydrodynamics.xlsx")
        lines.append("")

        lines.append("=" * 80)
        lines.append("Ready for OrcaFlex import")
        lines.append("=" * 80)

        with open(output_file, 'w') as f:
            f.write('\n'.join(lines))

        print(f"Summary report exported: {output_file}")
        return output_file

    # Helper methods

    def _write_summary_sheet(self, writer):
        """Write summary sheet to Excel workbook"""
        summary_data = {
            'Property': [
                'Vessel Name',
                'Analysis Tool',
                'Water Depth (m)',
                'Number of Frequencies',
                'Frequency Range (rad/s)',
                'Number of Headings',
                'Heading Range (deg)',
                'Created Date',
                'Analysis Date',
                'Source Files'
            ],
            'Value': [
                self.results.vessel_name,
                self.results.analysis_tool,
                self.results.water_depth,
                self.results.raos.surge.frequencies.count,
                f"{self.results.raos.surge.frequencies.min_freq:.4f} - {self.results.raos.surge.frequencies.max_freq:.4f}",
                self.results.raos.surge.headings.count,
                f"{self.results.raos.surge.headings.min_heading:.1f} - {self.results.raos.surge.headings.max_heading:.1f}",
                self.results.created_date,
                self.results.analysis_date or 'N/A',
                ', '.join(self.results.source_files) if self.results.source_files else 'N/A'
            ]
        }

        df = pd.DataFrame(summary_data)
        df.to_excel(writer, sheet_name='Summary', index=False)

    def _write_rao_sheet(self, writer):
        """Write RAO data to Excel sheet"""
        # Reuse CSV export logic but write to Excel
        raos = self.results.raos
        freqs = raos.surge.frequencies.values
        periods = raos.surge.frequencies.periods
        headings = raos.surge.headings.values

        data_dict = {
            'Frequency (rad/s)': freqs,
            'Period (s)': periods
        }

        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        for dof_name in dof_names:
            dof_enum = DOF[dof_name.upper()]
            component = raos.get_component(dof_enum)

            for h_idx, heading in enumerate(headings):
                data_dict[f"{dof_name}_Mag_H{heading:.1f}"] = component.magnitude[:, h_idx]
                data_dict[f"{dof_name}_Phase_H{heading:.1f}"] = component.phase[:, h_idx]

        df = pd.DataFrame(data_dict)
        df.to_excel(writer, sheet_name='RAOs', index=False)

    def _write_added_mass_sheet(self, writer):
        """Write added mass matrices to Excel sheet"""
        am_set = self.results.added_mass
        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        # Create pivot table format: Frequency x (DOF_i x DOF_j)
        data = []

        for matrix in am_set.matrices:
            row = {'Frequency (rad/s)': matrix.frequency, 'Period (s)': 2*np.pi/matrix.frequency}
            for i, dof_i in enumerate(dof_names):
                for j, dof_j in enumerate(dof_names):
                    row[f"{dof_i}_{dof_j}"] = matrix.matrix[i, j]
            data.append(row)

        df = pd.DataFrame(data)
        df.to_excel(writer, sheet_name='AddedMass', index=False)

    def _write_damping_sheet(self, writer):
        """Write damping matrices to Excel sheet"""
        damp_set = self.results.damping
        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']

        data = []

        for matrix in damp_set.matrices:
            row = {'Frequency (rad/s)': matrix.frequency, 'Period (s)': 2*np.pi/matrix.frequency}
            for i, dof_i in enumerate(dof_names):
                for j, dof_j in enumerate(dof_names):
                    row[f"{dof_i}_{dof_j}"] = matrix.matrix[i, j]
            data.append(row)

        df = pd.DataFrame(data)
        df.to_excel(writer, sheet_name='Damping', index=False)

    def _write_discretization_sheet(self, writer):
        """Write frequency and heading discretization to Excel"""
        freqs = self.results.raos.surge.frequencies
        heads = self.results.raos.surge.headings

        data = {
            'Frequency (rad/s)': freqs.values,
            'Period (s)': freqs.periods
        }

        df_freq = pd.DataFrame(data)

        df_head = pd.DataFrame({
            'Heading (deg)': heads.values
        })

        # Write to same sheet
        df_freq.to_excel(writer, sheet_name='Discretization', index=False, startrow=0)
        df_head.to_excel(writer, sheet_name='Discretization', index=False, startrow=len(df_freq)+3)

    @staticmethod
    def _get_added_mass_unit(i: int, j: int) -> str:
        """Get appropriate unit for added mass coupling"""
        # i, j are 0-indexed DOF indices
        # 0-2: translation, 3-5: rotation

        if i < 3 and j < 3:
            return "kg"
        elif (i < 3 and j >= 3) or (i >= 3 and j < 3):
            return "kg.m"
        else:  # both rotational
            return "kg.m^2"

    @staticmethod
    def _get_damping_unit(i: int, j: int) -> str:
        """Get appropriate unit for damping coupling"""
        if i < 3 and j < 3:
            return "N.s/m"
        elif (i < 3 and j >= 3) or (i >= 3 and j < 3):
            return "N.s or N.m.s/rad"
        else:  # both rotational
            return "N.m.s/rad"
