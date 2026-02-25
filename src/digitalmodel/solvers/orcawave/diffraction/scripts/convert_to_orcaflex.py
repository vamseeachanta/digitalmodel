#!/usr/bin/env python3
"""
OrcaWave to OrcaFlex Converter
Converts OrcaWave diffraction analysis results to OrcaFlex vessel type format
"""

import os
import sys
import yaml
import json
import h5py
import logging
import argparse
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
import re

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class VesselData:
    """Container for vessel hydrodynamic data"""
    name: str
    frequencies: np.ndarray
    directions: np.ndarray
    added_mass: np.ndarray  # Shape: (n_freq, 6, 6)
    damping: np.ndarray      # Shape: (n_freq, 6, 6)
    excitation_force: np.ndarray   # Shape: (n_freq, n_dir, 6) complex
    excitation_moment: np.ndarray  # Shape: (n_freq, n_dir, 6) complex
    displacement: float
    centre_of_gravity: List[float]
    centre_of_buoyancy: List[float]
    waterplane_area: float
    metacentric_height: float

class OrcaFlexConverter:
    """Converts OrcaWave results to OrcaFlex format"""
    
    def __init__(self, input_dir: str, output_dir: str):
        self.input_dir = Path(input_dir)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.vessel_data = None
        
    def load_orcawave_results(self) -> VesselData:
        """Load OrcaWave results from various formats"""
        
        logger.info(f"Loading OrcaWave results from: {self.input_dir}")
        
        # Try loading from HDF5 first (most complete)
        h5_files = list(self.input_dir.glob("*.h5"))
        if h5_files:
            return self._load_from_hdf5(h5_files[0])
        
        # Try loading from CSV files
        csv_dir = self.input_dir / "csv_outputs"
        if csv_dir.exists():
            return self._load_from_csv(csv_dir)
        
        # Try loading from Excel
        xlsx_files = list(self.input_dir.glob("*.xlsx"))
        if xlsx_files:
            return self._load_from_excel(xlsx_files[0])
        
        raise FileNotFoundError("No valid OrcaWave results found")
    
    def _load_from_hdf5(self, h5_file: Path) -> VesselData:
        """Load results from HDF5 database"""
        
        logger.info(f"Loading from HDF5: {h5_file}")
        
        with h5py.File(h5_file, 'r') as f:
            # Extract data
            frequencies = f['frequencies'][:]
            directions = f['directions'][:]
            added_mass = f['added_mass'][:]
            damping = f['damping'][:]
            
            # Complex excitation forces
            force_real = f['excitation_force_real'][:]
            force_imag = f['excitation_force_imag'][:]
            excitation_force = force_real + 1j * force_imag
            
            moment_real = f['excitation_moment_real'][:]
            moment_imag = f['excitation_moment_imag'][:]
            excitation_moment = moment_real + 1j * moment_imag
            
            # Vessel properties
            attrs = f.attrs
            displacement = attrs.get('displacement', 1000.0)
            cog = list(attrs.get('centre_of_gravity', [0, 0, 0]))
            cob = list(attrs.get('centre_of_buoyancy', [0, 0, -1]))
            waterplane = attrs.get('waterplane_area', 100.0)
            gm = attrs.get('metacentric_height', 1.0)
            name = attrs.get('vessel_name', 'Sea Cypress')
        
        return VesselData(
            name=name,
            frequencies=frequencies,
            directions=directions,
            added_mass=added_mass,
            damping=damping,
            excitation_force=excitation_force,
            excitation_moment=excitation_moment,
            displacement=displacement,
            centre_of_gravity=cog,
            centre_of_buoyancy=cob,
            waterplane_area=waterplane,
            metacentric_height=gm
        )
    
    def _load_from_csv(self, csv_dir: Path) -> VesselData:
        """Load results from CSV files"""
        
        logger.info(f"Loading from CSV directory: {csv_dir}")
        
        # Load frequency and direction data
        freq_file = csv_dir / "frequencies.csv"
        dir_file = csv_dir / "directions.csv"
        
        if freq_file.exists():
            frequencies = pd.read_csv(freq_file)['frequency'].values
        else:
            frequencies = np.logspace(-2, np.log10(3), 100)
        
        if dir_file.exists():
            directions = pd.read_csv(dir_file)['direction'].values
        else:
            directions = np.arange(0, 195, 15)
        
        n_freq = len(frequencies)
        n_dir = len(directions)
        
        # Initialize arrays
        added_mass = np.zeros((n_freq, 6, 6))
        damping = np.zeros((n_freq, 6, 6))
        excitation_force = np.zeros((n_freq, n_dir, 6), dtype=complex)
        excitation_moment = np.zeros((n_freq, n_dir, 6), dtype=complex)
        
        # Load added mass matrices
        for i in range(6):
            for j in range(6):
                file = csv_dir / f"added_mass_{i+1}{j+1}.csv"
                if file.exists():
                    data = pd.read_csv(file)
                    added_mass[:, i, j] = data['value'].values
        
        # Load damping matrices
        for i in range(6):
            for j in range(6):
                file = csv_dir / f"damping_{i+1}{j+1}.csv"
                if file.exists():
                    data = pd.read_csv(file)
                    damping[:, i, j] = data['value'].values
        
        # Load excitation forces
        for i in range(6):
            file = csv_dir / f"excitation_force_{i+1}.csv"
            if file.exists():
                data = pd.read_csv(file)
                # Reshape data assuming it's organized by frequency then direction
                real_part = data['real'].values.reshape(n_freq, n_dir)
                imag_part = data['imag'].values.reshape(n_freq, n_dir)
                excitation_force[:, :, i] = real_part + 1j * imag_part
        
        # Default vessel properties
        displacement = 1000.0
        cog = [0, 0, 5.0]
        cob = [0, 0, -1.0]
        waterplane = 200.0
        gm = 1.5
        
        # Try to load vessel properties
        props_file = csv_dir / "vessel_properties.csv"
        if props_file.exists():
            props = pd.read_csv(props_file, index_col=0)
            displacement = props.loc['displacement', 'value']
            cog = [props.loc[f'cog_{x}', 'value'] for x in ['x', 'y', 'z']]
            cob = [props.loc[f'cob_{x}', 'value'] for x in ['x', 'y', 'z']]
            waterplane = props.loc['waterplane_area', 'value']
            gm = props.loc['metacentric_height', 'value']
        
        return VesselData(
            name="Sea Cypress",
            frequencies=frequencies,
            directions=directions,
            added_mass=added_mass,
            damping=damping,
            excitation_force=excitation_force,
            excitation_moment=excitation_moment,
            displacement=displacement,
            centre_of_gravity=cog,
            centre_of_buoyancy=cob,
            waterplane_area=waterplane,
            metacentric_height=gm
        )
    
    def _load_from_excel(self, excel_file: Path) -> VesselData:
        """Load results from Excel file"""
        
        logger.info(f"Loading from Excel: {excel_file}")
        
        # Read all sheets
        xl = pd.ExcelFile(excel_file)
        
        # Load frequency and direction data
        if 'Frequencies' in xl.sheet_names:
            frequencies = pd.read_excel(xl, 'Frequencies')['Frequency'].values
        else:
            frequencies = np.logspace(-2, np.log10(3), 100)
        
        if 'Directions' in xl.sheet_names:
            directions = pd.read_excel(xl, 'Directions')['Direction'].values
        else:
            directions = np.arange(0, 195, 15)
        
        n_freq = len(frequencies)
        n_dir = len(directions)
        
        # Initialize arrays
        added_mass = np.zeros((n_freq, 6, 6))
        damping = np.zeros((n_freq, 6, 6))
        excitation_force = np.zeros((n_freq, n_dir, 6), dtype=complex)
        
        # Load matrices from sheets
        if 'Added_Mass' in xl.sheet_names:
            am_data = pd.read_excel(xl, 'Added_Mass')
            # Parse added mass data (assuming specific format)
            for idx, row in am_data.iterrows():
                freq_idx = int(row.get('freq_idx', idx))
                for i in range(6):
                    for j in range(6):
                        col_name = f'A{i+1}{j+1}'
                        if col_name in row:
                            added_mass[freq_idx, i, j] = row[col_name]
        
        if 'Damping' in xl.sheet_names:
            damp_data = pd.read_excel(xl, 'Damping')
            # Parse damping data
            for idx, row in damp_data.iterrows():
                freq_idx = int(row.get('freq_idx', idx))
                for i in range(6):
                    for j in range(6):
                        col_name = f'B{i+1}{j+1}'
                        if col_name in row:
                            damping[freq_idx, i, j] = row[col_name]
        
        # Default vessel properties
        vessel_props = {
            'displacement': 1000.0,
            'cog': [0, 0, 5.0],
            'cob': [0, 0, -1.0],
            'waterplane': 200.0,
            'gm': 1.5
        }
        
        if 'Vessel_Properties' in xl.sheet_names:
            props_df = pd.read_excel(xl, 'Vessel_Properties', index_col=0)
            vessel_props['displacement'] = props_df.loc['Displacement', 'Value']
            vessel_props['waterplane'] = props_df.loc['Waterplane Area', 'Value']
            vessel_props['gm'] = props_df.loc['GM', 'Value']
        
        return VesselData(
            name="Sea Cypress",
            frequencies=frequencies,
            directions=directions,
            added_mass=added_mass,
            damping=damping,
            excitation_force=excitation_force,
            excitation_moment=excitation_force,  # Use force as approximation
            displacement=vessel_props['displacement'],
            centre_of_gravity=vessel_props['cog'],
            centre_of_buoyancy=vessel_props['cob'],
            waterplane_area=vessel_props['waterplane'],
            metacentric_height=vessel_props['gm']
        )
    
    def convert_to_orcaflex(self, vessel_data: VesselData) -> Dict[str, Any]:
        """Convert vessel data to OrcaFlex YAML format"""
        
        logger.info("Converting to OrcaFlex vessel type format...")
        
        # OrcaFlex vessel type structure
        vessel_type = {
            'VesselType': {
                'Name': vessel_data.name,
                'DraughtsCalculatedFromRAOs': 'Displacement RAOs',
                
                # General data
                'Mass': vessel_data.displacement,
                'CentreOfMass': vessel_data.centre_of_gravity,
                'CentreOfBuoyancy': vessel_data.centre_of_buoyancy,
                'DisplacedVolume': vessel_data.displacement / 1.025,  # Assuming seawater
                'WaterplaneArea': vessel_data.waterplane_area,
                'GMt': vessel_data.metacentric_height,
                
                # Frequency dependent data
                'FrequencyDependentData': {
                    'NumberOfFrequencies': len(vessel_data.frequencies),
                    'Frequencies': vessel_data.frequencies.tolist(),
                    'FrequencyUnits': 'rad/s',
                    
                    'NumberOfDirections': len(vessel_data.directions),
                    'Directions': vessel_data.directions.tolist(),
                    'DirectionUnits': 'deg',
                    
                    'AddedMassAndDamping': self._format_matrices(
                        vessel_data.added_mass, 
                        vessel_data.damping
                    ),
                    
                    'LoadRAOs': self._format_load_raos(
                        vessel_data.excitation_force,
                        vessel_data.excitation_moment
                    )
                },
                
                # Additional properties
                'IncludeWaveLoad': 'Yes',
                'IncludeAddedMassAndDamping': 'Yes',
                'IncludeManoeuvringLoad': 'No',
                'IncludeOtherDamping': 'No',
                
                # Conventions
                'ConventionForDirection': 'Coming from',
                'ConventionForPhase': 'Lag',
                
                # Reference axes
                'ReferenceOrigin': [0, 0, 0],
                'XAxis': [1, 0, 0],
                'ZAxis': [0, 0, 1]
            }
        }
        
        return vessel_type
    
    def _format_matrices(self, added_mass: np.ndarray, damping: np.ndarray) -> List[Dict]:
        """Format added mass and damping matrices for OrcaFlex"""
        
        matrices = []
        
        for freq_idx in range(len(added_mass)):
            matrix_data = {
                'FrequencyIndex': freq_idx,
                'AddedMass': {},
                'Damping': {}
            }
            
            # Format as upper triangular (OrcaFlex convention)
            dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
            
            for i in range(6):
                for j in range(i, 6):  # Upper triangular only
                    key = f'{dof_names[i]}{dof_names[j]}'
                    matrix_data['AddedMass'][key] = float(added_mass[freq_idx, i, j])
                    matrix_data['Damping'][key] = float(damping[freq_idx, i, j])
            
            matrices.append(matrix_data)
        
        return matrices
    
    def _format_load_raos(self, force: np.ndarray, moment: np.ndarray) -> List[Dict]:
        """Format wave load RAOs for OrcaFlex"""
        
        raos = []
        dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
        
        for freq_idx in range(force.shape[0]):
            for dir_idx in range(force.shape[1]):
                rao_data = {
                    'FrequencyIndex': freq_idx,
                    'DirectionIndex': dir_idx,
                    'LoadRAO': {}
                }
                
                for dof_idx in range(6):
                    dof_name = dof_names[dof_idx]
                    
                    if dof_idx < 3:  # Forces
                        value = force[freq_idx, dir_idx, dof_idx]
                    else:  # Moments
                        value = moment[freq_idx, dir_idx, dof_idx - 3]
                    
                    # Store magnitude and phase
                    rao_data['LoadRAO'][dof_name] = {
                        'Amplitude': float(np.abs(value)),
                        'Phase': float(np.angle(value, deg=True))
                    }
                
                raos.append(rao_data)
        
        return raos
    
    def save_orcaflex_file(self, vessel_type: Dict[str, Any], filename: str = None):
        """Save OrcaFlex vessel type to YAML file"""
        
        if filename is None:
            filename = "sea_cypress_vessel_type.yml"
        
        output_file = self.output_dir / filename
        
        logger.info(f"Saving OrcaFlex vessel type to: {output_file}")
        
        with open(output_file, 'w') as f:
            yaml.dump(vessel_type, f, default_flow_style=False, sort_keys=False)
        
        # Also save a compact version for direct import
        compact_file = self.output_dir / f"compact_{filename}"
        self._save_compact_format(vessel_type, compact_file)
        
        logger.info(f"Files saved successfully")
        return output_file
    
    def _save_compact_format(self, vessel_type: Dict, output_file: Path):
        """Save in compact format optimized for OrcaFlex import"""
        
        # Create simplified structure
        compact = {
            'General': {
                'Name': vessel_type['VesselType']['Name'],
                'Mass': vessel_type['VesselType']['Mass'],
                'CentreOfMass': vessel_type['VesselType']['CentreOfMass']
            },
            'HydrodynamicData': {
                'DataSource': 'OrcaWave Diffraction Analysis',
                'Frequencies': vessel_type['VesselType']['FrequencyDependentData']['Frequencies'],
                'Directions': vessel_type['VesselType']['FrequencyDependentData']['Directions']
            }
        }
        
        # Save matrices in separate arrays for easier parsing
        freq_data = vessel_type['VesselType']['FrequencyDependentData']
        n_freq = freq_data['NumberOfFrequencies']
        
        # Flatten matrices
        added_mass_flat = []
        damping_flat = []
        
        for matrix in freq_data['AddedMassAndDamping']:
            am_row = []
            d_row = []
            for key in ['SurgeSurge', 'SurgeSway', 'SurgeHeave', 'SurgeRoll', 'SurgePitch', 'SurgeYaw',
                       'SwaySway', 'SwayHeave', 'SwayRoll', 'SwayPitch', 'SwayYaw',
                       'HeaveHeave', 'HeaveRoll', 'HeavePitch', 'HeaveYaw',
                       'RollRoll', 'RollPitch', 'RollYaw',
                       'PitchPitch', 'PitchYaw',
                       'YawYaw']:
                am_row.append(matrix['AddedMass'].get(key, 0.0))
                d_row.append(matrix['Damping'].get(key, 0.0))
            added_mass_flat.append(am_row)
            damping_flat.append(d_row)
        
        compact['HydrodynamicData']['AddedMass'] = added_mass_flat
        compact['HydrodynamicData']['Damping'] = damping_flat
        
        with open(output_file, 'w') as f:
            yaml.dump(compact, f, default_flow_style=False)
    
    def generate_summary_report(self, vessel_data: VesselData) -> str:
        """Generate summary report of conversion"""
        
        report = []
        report.append("=" * 60)
        report.append("ORCAFLEX CONVERSION SUMMARY")
        report.append("=" * 60)
        report.append(f"Vessel Name: {vessel_data.name}")
        report.append(f"Displacement: {vessel_data.displacement:.1f} tonnes")
        report.append(f"Waterplane Area: {vessel_data.waterplane_area:.1f} m²")
        report.append(f"GM: {vessel_data.metacentric_height:.2f} m")
        report.append("")
        report.append("Frequency Range:")
        report.append(f"  Min: {vessel_data.frequencies.min():.3f} rad/s")
        report.append(f"  Max: {vessel_data.frequencies.max():.3f} rad/s")
        report.append(f"  Points: {len(vessel_data.frequencies)}")
        report.append("")
        report.append("Direction Range:")
        report.append(f"  Min: {vessel_data.directions.min():.1f}°")
        report.append(f"  Max: {vessel_data.directions.max():.1f}°")
        report.append(f"  Points: {len(vessel_data.directions)}")
        report.append("")
        report.append("Data Components:")
        report.append(f"  Added Mass: {vessel_data.added_mass.shape}")
        report.append(f"  Damping: {vessel_data.damping.shape}")
        report.append(f"  Excitation Forces: {vessel_data.excitation_force.shape}")
        report.append("=" * 60)
        
        return "\n".join(report)

def main():
    """Main execution function"""
    
    parser = argparse.ArgumentParser(
        description="Convert OrcaWave results to OrcaFlex format"
    )
    parser.add_argument(
        '--input', '-i',
        required=True,
        help='Input directory containing OrcaWave results'
    )
    parser.add_argument(
        '--output', '-o',
        required=True,
        help='Output directory for OrcaFlex files'
    )
    parser.add_argument(
        '--vessel-name',
        default='Sea Cypress',
        help='Vessel name for OrcaFlex type'
    )
    
    args = parser.parse_args()
    
    try:
        # Initialize converter
        converter = OrcaFlexConverter(args.input, args.output)
        
        # Load OrcaWave results
        vessel_data = converter.load_orcawave_results()
        vessel_data.name = args.vessel_name
        
        # Convert to OrcaFlex format
        vessel_type = converter.convert_to_orcaflex(vessel_data)
        
        # Save files
        output_file = converter.save_orcaflex_file(vessel_type)
        
        # Generate report
        report = converter.generate_summary_report(vessel_data)
        print(report)
        
        # Save report
        report_file = converter.output_dir / "conversion_report.txt"
        with open(report_file, 'w') as f:
            f.write(report)
        
        logger.info(f"Conversion complete. Files saved to: {converter.output_dir}")
        
    except Exception as e:
        logger.error(f"Conversion failed: {e}")
        return 1
    
    return 0

if __name__ == "__main__":
    sys.exit(main())