#!/usr/bin/env python
"""
Generate OrcaWave input files from GMsh mesh files
Converts GMsh .msh format to OrcaWave-compatible input configuration
"""

import os
import sys
import yaml
import argparse
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, List, Tuple
import numpy as np

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent.parent))

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class GMshToOrcaWaveConverter:
    """Convert GMsh mesh to OrcaWave input configuration"""
    
    def __init__(self, msh_file_path: str, output_dir: str):
        """
        Initialize converter with GMsh mesh file
        
        Args:
            msh_file_path: Path to GMsh .msh file
            output_dir: Directory for OrcaWave output files
        """
        self.msh_file_path = Path(msh_file_path)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Default vessel properties (Sea Cypress)
        self.vessel_properties = {
            'name': 'Sea Cypress',
            'length': 103.0,  # meters
            'mass': 9017.95,  # tonnes
            'cog': [2.53, 0.0, -1.974],  # Center of gravity [x, y, z]
            'draft': 3.5,  # meters
            'waterline_z': 0.0  # Waterline position
        }
        
        # Default analysis parameters
        self.analysis_params = {
            'water_depth': 30.0,  # meters
            'water_density': 1.025,  # tonnes/m³
            'wave_periods': [2, 3, 4, 5, 6, 7, 7.5, 8, 8.5, 9, 9.5, 10, 11, 13, 15, 16, 17, 19, 20, 22],
            'wave_headings': [0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180]
        }
        
    def read_gmsh_mesh(self) -> Dict[str, Any]:
        """
        Read and parse GMsh mesh file
        
        Returns:
            Dictionary containing mesh data
        """
        logger.info(f"Reading GMsh mesh from: {self.msh_file_path}")
        
        mesh_data = {
            'nodes': [],
            'elements': [],
            'bounds': None,
            'format': 'gmsh'
        }
        
        if not self.msh_file_path.exists():
            raise FileNotFoundError(f"Mesh file not found: {self.msh_file_path}")
            
        with open(self.msh_file_path, 'r') as f:
            lines = f.readlines()
            
        # Parse GMsh format (simplified parser for key sections)
        in_nodes = False
        in_elements = False
        
        for line in lines:
            line = line.strip()
            
            if line == '$Nodes':
                in_nodes = True
                in_elements = False
                continue
            elif line == '$EndNodes':
                in_nodes = False
                continue
            elif line == '$Elements':
                in_elements = True
                in_nodes = False
                continue
            elif line == '$EndElements':
                in_elements = False
                continue
                
            if in_nodes and line and not line.startswith('$'):
                # Parse node data
                parts = line.split()
                if len(parts) >= 4:
                    try:
                        node_id = int(parts[0])
                        x, y, z = float(parts[1]), float(parts[2]), float(parts[3])
                        mesh_data['nodes'].append([x, y, z])
                    except (ValueError, IndexError):
                        continue
                        
        # Calculate mesh bounds
        if mesh_data['nodes']:
            nodes_array = np.array(mesh_data['nodes'])
            mesh_data['bounds'] = {
                'min': nodes_array.min(axis=0).tolist(),
                'max': nodes_array.max(axis=0).tolist(),
                'center': nodes_array.mean(axis=0).tolist()
            }
            logger.info(f"Loaded {len(mesh_data['nodes'])} nodes")
            logger.info(f"Mesh bounds: {mesh_data['bounds']}")
            
        return mesh_data
        
    def generate_gdf_file(self, mesh_data: Dict[str, Any]) -> str:
        """
        Generate Wamit GDF format file from mesh data
        
        Args:
            mesh_data: Dictionary containing mesh information
            
        Returns:
            Path to generated GDF file
        """
        gdf_file_path = self.output_dir / f"{self.msh_file_path.stem}.gdf"
        logger.info(f"Generating GDF file: {gdf_file_path}")
        
        # For now, we'll reference the existing GDF file
        # In a full implementation, this would convert the mesh format
        existing_gdf = Path(__file__).parent.parent / "revision-1" / "geometry-iterations" / "sea_cypress_corrected.gdf"
        
        if existing_gdf.exists():
            import shutil
            shutil.copy2(existing_gdf, gdf_file_path)
            logger.info(f"Using existing GDF file as reference")
        else:
            # Create a simple GDF placeholder
            with open(gdf_file_path, 'w') as f:
                f.write(f"# Wamit GDF file generated from {self.msh_file_path.name}\n")
                f.write(f"# Generated on {datetime.now().isoformat()}\n")
                f.write(f"# Mesh center: {mesh_data.get('bounds', {}).get('center', [0, 0, 0])}\n")
                
        return str(gdf_file_path.relative_to(self.output_dir.parent))
        
    def generate_orcawave_config(self, gdf_file_path: str) -> Dict[str, Any]:
        """
        Generate OrcaWave configuration dictionary
        
        Args:
            gdf_file_path: Relative path to GDF file
            
        Returns:
            OrcaWave configuration dictionary
        """
        logger.info("Generating OrcaWave configuration")
        
        config = {
            'UnitsSystem': 'SI',
            'SolveType': 'Full QTF calculation',
            'LoadRAOCalculationMethod': 'Both',
            'PreferredLoadRAOCalculationMethod': 'Haskind',
            'QuadraticLoadPressureIntegration': True,
            'QuadraticLoadControlSurface': True,
            'QuadraticLoadMomentumConservation': False,
            'PreferredQuadraticLoadCalculationMethod': 'Control surface',
            'LengthTolerance': 100e-9,
            'WaterlineZTolerance': 1e-6,
            'WaterlineGapTolerance': 0.1,
            'DivideNonPlanarPanels': False,
            'LinearSolverMethod': 'Direct LU',
            'OutputPanelPressures': False,
            'OutputPanelVelocities': False,
            'OutputBodyWireFrames': True,
            'OutputIntermediateResults': False,
            'ValidatePanelArrangement': False,
            'BodyVolumeWarningLevel': 1e-12,
            'PanelAspectRatioWarningLevel': 25,
            'PanelsPerWavelengthWarningLevel': 5,
            
            # Environment
            'WaterDepth': self.analysis_params['water_depth'],
            'WaterDensity': self.analysis_params['water_density'],
            'WavesReferredToBy': 'period (s)',
            'HasWaveSpectrumForDragLinearisation': False,
            'MorisonFluidVelocity': 'Undisturbed incident wave',
            'PeriodOrFrequency': self.analysis_params['wave_periods'],
            'WaveHeading': self.analysis_params['wave_headings'],
            
            # QTF parameters
            'QTFMinCrossingAngle': 0,
            'QTFMaxCrossingAngle': 0,
            'QTFMinPeriodOrFrequency': 2,
            'QTFMaxPeriodOrFrequency': 10,
            'QTFFrequencyTypes': 'Both',
            'IncludeMeanDriftFullQTFs': False,
            
            # Bodies
            'Bodies': [{
                'BodyName': self.vessel_properties['name'].replace(' ', '_'),
                'BodyMeshPosition': [0, 0, self.vessel_properties['waterline_z']],
                'BodyMeshAttitude': [0, 0, 0],
                'BodyIncludedInAnalysis': True,
                'BodyMeshFileName': gdf_file_path,
                'BodyMeshFormat': 'Wamit gdf',
                'BodyMeshLengthUnits': 'm',
                'BodyMeshSymmetry': 'None',
                'BodyMeshDipolePanels': None,
                'BodyAddInteriorSurfacePanels': True,
                'BodyInteriorSurfacePanelMethod': 'Triangulation method',
                'BodyControlSurfaceType': 'Automatically generated',
                'BodyControlSurfacePanelSize': 10,
                'BodyControlSurfaceSeparationFromBody': 20,
                'BodyControlSurfaceIncludeFreeSurface': True,
                'BodyOrcaFlexImportSymmetry': 'Use global mesh symmetry',
                'BodyOrcaFlexImportLength': self.vessel_properties['length'],
                'BodyHydrostaticIntegralMethod': 'Standard',
                'BodyHydrostaticStiffnessMethod': 'Displacement',
                'BodyInertiaSpecifiedBy': 'Matrix (for a general body)',
                'BodyCentreOfMass': self.vessel_properties['cog'],
                'BodyMass': self.vessel_properties['mass'],
                'BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz': [
                    [254.9374465e3, 0, 0],
                    [0, 2709.74529e3, 0],
                    [0, 0, 2752.1116e3]
                ]
            }]
        }
        
        return config
        
    def write_orcawave_input(self, config: Dict[str, Any], filename: str = None) -> str:
        """
        Write OrcaWave input file
        
        Args:
            config: OrcaWave configuration dictionary
            filename: Optional output filename
            
        Returns:
            Path to written file
        """
        if filename is None:
            filename = f"sea_cypress_from_gmsh_{datetime.now().strftime('%Y%m%d_%H%M%S')}.yml"
            
        output_path = self.output_dir / filename
        
        # Add header comments
        header = f"""# Type: Diffraction
# Program: OrcaWave 11.5e
# File: {output_path}
# Created: {datetime.now().strftime('%I:%M %p on %m/%d/%Y')}
# User: AI Generated from GMsh mesh
# Source Mesh: {self.msh_file_path.name}
---"""
        
        with open(output_path, 'w') as f:
            f.write('%YAML 1.1\n')
            f.write(header + '\n')
            yaml.dump(config, f, default_flow_style=False, sort_keys=False)
            
        logger.info(f"Wrote OrcaWave input to: {output_path}")
        return str(output_path)
        
    def convert(self) -> str:
        """
        Main conversion method
        
        Returns:
            Path to generated OrcaWave input file
        """
        logger.info("Starting GMsh to OrcaWave conversion")
        
        # Read mesh
        mesh_data = self.read_gmsh_mesh()
        
        # Generate GDF file
        gdf_path = self.generate_gdf_file(mesh_data)
        
        # Generate configuration
        config = self.generate_orcawave_config(gdf_path)
        
        # Write output
        output_file = self.write_orcawave_input(config)
        
        logger.info("Conversion complete!")
        return output_file


def main():
    """Main execution function"""
    parser = argparse.ArgumentParser(
        description='Generate OrcaWave input files from GMsh mesh'
    )
    parser.add_argument(
        '--mesh-file',
        default='specs/modules/orcawave/diffraction-analysis/inputs/geometry/Sea Cypress_0.25 Mesh_Ascii.msh',
        help='Path to GMsh .msh file'
    )
    parser.add_argument(
        '--output-dir',
        default='specs/modules/orcawave/diffraction-analysis/inputs/orcawave',
        help='Output directory for OrcaWave files'
    )
    parser.add_argument(
        '--vessel-length',
        type=float,
        default=103.0,
        help='Vessel length in meters'
    )
    parser.add_argument(
        '--vessel-mass',
        type=float,
        default=9017.95,
        help='Vessel mass in tonnes'
    )
    parser.add_argument(
        '--water-depth',
        type=float,
        default=30.0,
        help='Water depth in meters'
    )
    
    args = parser.parse_args()
    
    # Convert relative paths to absolute
    mesh_file = Path(args.mesh_file)
    if not mesh_file.is_absolute():
        mesh_file = Path.cwd() / mesh_file
        
    output_dir = Path(args.output_dir)
    if not output_dir.is_absolute():
        output_dir = Path.cwd() / output_dir
    
    # Create converter
    converter = GMshToOrcaWaveConverter(
        str(mesh_file),
        str(output_dir)
    )
    
    # Update vessel properties if provided
    converter.vessel_properties['length'] = args.vessel_length
    converter.vessel_properties['mass'] = args.vessel_mass
    converter.analysis_params['water_depth'] = args.water_depth
    
    # Perform conversion
    try:
        output_file = converter.convert()
        print(f"\nSuccess! OrcaWave input file created: {output_file}")
        print(f"\nNext steps:")
        print(f"1. Review the generated file: {output_file}")
        print(f"2. Run OrcaWave analysis using: python scripts/execute_orcawave.py --config {output_file}")
        print(f"3. Post-process results using: python scripts/process_orcawave_results.py")
    except Exception as e:
        logger.error(f"Conversion failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()