"""
Extract vessel data from Excel files for OrcaWave input generation.
"""

import pandas as pd
import numpy as np
from pathlib import Path
import yaml
import logging
from typing import Dict, Any, Optional, List
import argparse

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class VesselDataExtractor:
    """Extract vessel parameters from Excel files."""
    
    def __init__(self, excel_path: Path):
        """Initialize with Excel file path."""
        self.excel_path = Path(excel_path)
        self.data = {}
        
    def extract_parameters(self, sheet_name: Optional[str] = None) -> Dict[str, Any]:
        """
        Extract vessel parameters from Excel.
        
        Args:
            sheet_name: Specific sheet to read, or None for all sheets
            
        Returns:
            Dictionary of vessel parameters
        """
        try:
            # Read Excel file
            if sheet_name:
                df = pd.read_excel(self.excel_path, sheet_name=sheet_name)
                self._process_sheet(df, sheet_name)
            else:
                # Read all sheets
                excel_file = pd.ExcelFile(self.excel_path)
                for sheet in excel_file.sheet_names:
                    df = pd.read_excel(self.excel_path, sheet_name=sheet)
                    self._process_sheet(df, sheet)
                    
            return self.data
            
        except Exception as e:
            logger.error(f"Error reading Excel file: {e}")
            raise
            
    def _process_sheet(self, df: pd.DataFrame, sheet_name: str):
        """Process a single sheet to extract parameters."""
        logger.info(f"Processing sheet: {sheet_name}")
        
        # Common parameter mappings
        param_mappings = {
            # Mass properties
            'mass': ['mass', 'weight', 'displacement'],
            'centre_of_mass': ['cog', 'centre_of_mass', 'com', 'vcg'],
            
            # Inertia properties
            'ixx': ['ixx', 'rxx', 'roll_inertia'],
            'iyy': ['iyy', 'ryy', 'pitch_inertia'],
            'izz': ['izz', 'rzz', 'yaw_inertia'],
            
            # Dimensions
            'length': ['length', 'loa', 'lbp'],
            'beam': ['beam', 'breadth', 'width'],
            'draft': ['draft', 'draught', 'depth'],
            
            # Hydrostatic
            'waterplane_area': ['waterplane', 'awp', 'waterplane_area'],
            'displacement_volume': ['volume', 'displacement_volume'],
            
            # Environment
            'water_depth': ['water_depth', 'depth', 'wd'],
            'water_density': ['density', 'rho', 'water_density'],
        }
        
        # Search for parameters in DataFrame
        for param, keywords in param_mappings.items():
            value = self._find_parameter(df, keywords)
            if value is not None:
                self.data[param] = value
                logger.info(f"Found {param}: {value}")
                
    def _find_parameter(self, df: pd.DataFrame, keywords: List[str]) -> Optional[float]:
        """Find parameter value by searching for keywords."""
        # Search in column names
        for col in df.columns:
            col_lower = str(col).lower()
            for keyword in keywords:
                if keyword.lower() in col_lower:
                    # Get non-null values
                    values = df[col].dropna()
                    if len(values) > 0:
                        try:
                            return float(values.iloc[0])
                        except:
                            pass
                            
        # Search in first column as labels
        if len(df.columns) >= 2:
            first_col = df.iloc[:, 0].astype(str).str.lower()
            for keyword in keywords:
                mask = first_col.str.contains(keyword.lower(), na=False)
                if mask.any():
                    idx = mask.idxmax()
                    # Try to get value from second column
                    try:
                        return float(df.iloc[idx, 1])
                    except:
                        pass
                        
        return None
        
    def calculate_inertia_tensor(self) -> Dict[str, List[List[float]]]:
        """Calculate inertia tensor from extracted properties."""
        tensor = {
            'BodyInertiaTensorRx': [],
            'BodyInertiaTensorRy': [],
            'BodyInertiaTensorRz': []
        }
        
        # Get inertia values or use defaults
        ixx = self.data.get('ixx', 0)
        iyy = self.data.get('iyy', 0)
        izz = self.data.get('izz', 0)
        
        # Create inertia tensor (assuming principal axes)
        tensor['BodyInertiaTensorRx'] = [ixx, 0, 0]
        tensor['BodyInertiaTensorRy'] = [0, iyy, 0]
        tensor['BodyInertiaTensorRz'] = [0, 0, izz]
        
        return tensor
        
    def format_for_orcawave(self) -> Dict[str, Any]:
        """Format extracted data for OrcaWave YAML input."""
        orcawave_data = {
            'BodyMass': self.data.get('mass', 1000),
            'BodyCentreOfMass': [
                self.data.get('centre_of_mass_x', 0),
                self.data.get('centre_of_mass_y', 0),
                self.data.get('centre_of_mass_z', 0)
            ]
        }
        
        # Add inertia tensor
        tensor = self.calculate_inertia_tensor()
        orcawave_data.update(tensor)
        
        # Add dimensions if available
        if 'length' in self.data:
            orcawave_data['BodyOrcaFlexImportLength'] = self.data['length']
            
        return orcawave_data
        

def merge_with_template(vessel_data: Dict, template_path: Path, output_path: Path):
    """Merge extracted vessel data with OrcaWave template."""
    try:
        # Load template
        with open(template_path, 'r') as f:
            template = yaml.safe_load(f)
            
        # Find Bodies section and update
        if 'Bodies' in template and len(template['Bodies']) > 0:
            body = template['Bodies'][0]
            
            # Update with extracted data
            for key, value in vessel_data.items():
                if key.startswith('Body'):
                    body[key] = value
                    
            logger.info(f"Updated body parameters in template")
            
        # Write output
        with open(output_path, 'w') as f:
            yaml.dump(template, f, default_flow_style=False, sort_keys=False)
            
        logger.info(f"Wrote OrcaWave input to: {output_path}")
        
    except Exception as e:
        logger.error(f"Error merging with template: {e}")
        raise
        

def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(description='Extract vessel data from Excel for OrcaWave')
    parser.add_argument('excel_file', help='Path to Excel file with vessel data')
    parser.add_argument('--sheet', help='Specific sheet name to process')
    parser.add_argument('--template', help='Path to OrcaWave template YAML')
    parser.add_argument('--output', help='Output YAML file path')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
        
    # Extract data
    extractor = VesselDataExtractor(args.excel_file)
    vessel_data = extractor.extract_parameters(args.sheet)
    
    # Format for OrcaWave
    orcawave_data = extractor.format_for_orcawave()
    
    # Print extracted data
    print("\n=== Extracted Vessel Data ===")
    for key, value in vessel_data.items():
        print(f"{key}: {value}")
        
    print("\n=== OrcaWave Formatted Data ===")
    for key, value in orcawave_data.items():
        print(f"{key}: {value}")
        
    # Merge with template if provided
    if args.template and args.output:
        template_path = Path(args.template)
        output_path = Path(args.output)
        merge_with_template(orcawave_data, template_path, output_path)
        print(f"\n✅ Generated OrcaWave input: {output_path}")
        

if __name__ == "__main__":
    main()