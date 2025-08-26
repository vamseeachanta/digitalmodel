"""
Post-process OrcaWave results and extract key output data.
"""

import pandas as pd
import numpy as np
from pathlib import Path
import logging
import argparse
from typing import Dict, Any, List, Tuple
import yaml
import json

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaWavePostProcessor:
    """Process OrcaWave output files and extract key data."""
    
    def __init__(self, results_dir: Path):
        """Initialize with results directory path."""
        self.results_dir = Path(results_dir)
        self.data = {
            'raos': {},
            'added_mass': {},
            'damping': {},
            'excitation': {},
            'mean_drift': {},
            'qtf': {}
        }
        
    def find_result_files(self) -> Dict[str, Path]:
        """Find all OrcaWave result files."""
        files = {}
        
        # Common OrcaWave output file patterns
        patterns = {
            'raos': ['*RAOs.csv', '*_raos.csv', '*RAO*.csv'],
            'added_mass': ['*added_mass.csv', '*AddedMass*.csv'],
            'damping': ['*damping.csv', '*Damping*.csv'],
            'excitation': ['*excitation.csv', '*Excitation*.csv'],
            'mean_drift': ['*mean_drift.csv', '*MeanDrift*.csv'],
            'qtf_sum': ['*QTF_sum.csv', '*_qtf_sum.csv'],
            'qtf_diff': ['*QTF_diff.csv', '*_qtf_diff.csv']
        }
        
        for key, patterns_list in patterns.items():
            for pattern in patterns_list:
                matches = list(self.results_dir.glob(pattern))
                if matches:
                    files[key] = matches[0]
                    logger.info(f"Found {key} file: {matches[0].name}")
                    break
                    
        return files
        
    def extract_raos(self, rao_file: Path) -> pd.DataFrame:
        """Extract RAO data from CSV file."""
        try:
            # Read RAO data
            df = pd.read_csv(rao_file)
            
            # Expected columns: Period, Heading, DOF, Amplitude, Phase
            if 'Period' in df.columns:
                # Pivot to create matrix format
                rao_pivot = df.pivot_table(
                    index='Period',
                    columns=['Heading', 'DOF'],
                    values='Amplitude'
                )
                self.data['raos'] = rao_pivot
                logger.info(f"Extracted RAOs: {rao_pivot.shape}")
                return rao_pivot
            else:
                logger.warning("RAO file format not recognized")
                return pd.DataFrame()
                
        except Exception as e:
            logger.error(f"Error extracting RAOs: {e}")
            return pd.DataFrame()
            
    def extract_hydrodynamic_coefficients(self, files: Dict[str, Path]):
        """Extract hydrodynamic coefficients from result files."""
        
        # Extract added mass
        if 'added_mass' in files:
            try:
                df = pd.read_csv(files['added_mass'])
                self.data['added_mass'] = df
                logger.info(f"Extracted added mass: {df.shape}")
            except Exception as e:
                logger.error(f"Error reading added mass: {e}")
                
        # Extract damping
        if 'damping' in files:
            try:
                df = pd.read_csv(files['damping'])
                self.data['damping'] = df
                logger.info(f"Extracted damping: {df.shape}")
            except Exception as e:
                logger.error(f"Error reading damping: {e}")
                
        # Extract excitation
        if 'excitation' in files:
            try:
                df = pd.read_csv(files['excitation'])
                self.data['excitation'] = df
                logger.info(f"Extracted excitation: {df.shape}")
            except Exception as e:
                logger.error(f"Error reading excitation: {e}")
                
    def extract_qtf_data(self, files: Dict[str, Path]):
        """Extract QTF data if available."""
        
        # Sum-frequency QTF
        if 'qtf_sum' in files:
            try:
                df = pd.read_csv(files['qtf_sum'])
                self.data['qtf']['sum'] = df
                logger.info(f"Extracted sum-frequency QTF: {df.shape}")
            except Exception as e:
                logger.error(f"Error reading QTF sum: {e}")
                
        # Difference-frequency QTF
        if 'qtf_diff' in files:
            try:
                df = pd.read_csv(files['qtf_diff'])
                self.data['qtf']['diff'] = df
                logger.info(f"Extracted difference-frequency QTF: {df.shape}")
            except Exception as e:
                logger.error(f"Error reading QTF diff: {e}")
                
    def export_to_excel(self, output_path: Path):
        """Export all results to Excel workbook."""
        try:
            with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
                # Write RAOs
                if isinstance(self.data['raos'], pd.DataFrame) and not self.data['raos'].empty:
                    self.data['raos'].to_excel(writer, sheet_name='RAOs')
                    
                # Write hydrodynamic coefficients
                if isinstance(self.data['added_mass'], pd.DataFrame) and not self.data['added_mass'].empty:
                    self.data['added_mass'].to_excel(writer, sheet_name='Added Mass', index=False)
                    
                if isinstance(self.data['damping'], pd.DataFrame) and not self.data['damping'].empty:
                    self.data['damping'].to_excel(writer, sheet_name='Damping', index=False)
                    
                if isinstance(self.data['excitation'], pd.DataFrame) and not self.data['excitation'].empty:
                    self.data['excitation'].to_excel(writer, sheet_name='Excitation', index=False)
                    
                # Write QTF data if available
                if 'sum' in self.data['qtf'] and not self.data['qtf']['sum'].empty:
                    self.data['qtf']['sum'].to_excel(writer, sheet_name='QTF Sum', index=False)
                    
                if 'diff' in self.data['qtf'] and not self.data['qtf']['diff'].empty:
                    self.data['qtf']['diff'].to_excel(writer, sheet_name='QTF Diff', index=False)
                    
                # Add summary sheet
                summary = self._create_summary()
                summary_df = pd.DataFrame([summary])
                summary_df.to_excel(writer, sheet_name='Summary', index=False)
                
            logger.info(f"Exported results to Excel: {output_path}")
            return True
            
        except Exception as e:
            logger.error(f"Error exporting to Excel: {e}")
            return False
            
    def export_to_orcaflex(self, output_path: Path):
        """Export results in OrcaFlex-compatible format."""
        try:
            orcaflex_data = {
                'VesselType': {
                    'Name': 'Imported_Vessel',
                    'DraughtsAndAreas': {},
                    'RAOs': {},
                    'LoadRAOs': {},
                    'HydrodynamicCoefficients': {}
                }
            }
            
            # Convert RAOs to OrcaFlex format
            if isinstance(self.data['raos'], pd.DataFrame) and not self.data['raos'].empty:
                # Convert to nested dict structure
                rao_dict = {}
                for col in self.data['raos'].columns:
                    heading, dof = col
                    if heading not in rao_dict:
                        rao_dict[heading] = {}
                    rao_dict[heading][dof] = self.data['raos'][col].tolist()
                orcaflex_data['VesselType']['RAOs'] = rao_dict
                
            # Add hydrodynamic coefficients
            if isinstance(self.data['added_mass'], pd.DataFrame) and not self.data['added_mass'].empty:
                orcaflex_data['VesselType']['HydrodynamicCoefficients']['AddedMass'] = \
                    self.data['added_mass'].to_dict('records')
                    
            if isinstance(self.data['damping'], pd.DataFrame) and not self.data['damping'].empty:
                orcaflex_data['VesselType']['HydrodynamicCoefficients']['Damping'] = \
                    self.data['damping'].to_dict('records')
                    
            # Write YAML file
            with open(output_path, 'w') as f:
                yaml.dump(orcaflex_data, f, default_flow_style=False, sort_keys=False)
                
            logger.info(f"Exported to OrcaFlex format: {output_path}")
            return True
            
        except Exception as e:
            logger.error(f"Error exporting to OrcaFlex: {e}")
            return False
            
    def _create_summary(self) -> Dict[str, Any]:
        """Create summary statistics of results."""
        summary = {
            'Total_Files_Processed': 0,
            'RAO_Data_Points': 0,
            'Frequency_Range': '',
            'Heading_Range': '',
            'DOFs_Analyzed': 0
        }
        
        # Count processed files
        for key, value in self.data.items():
            if isinstance(value, pd.DataFrame) and not value.empty:
                summary['Total_Files_Processed'] += 1
            elif isinstance(value, dict):
                summary['Total_Files_Processed'] += len([v for v in value.values() if not v.empty])
                
        # RAO statistics
        if isinstance(self.data['raos'], pd.DataFrame) and not self.data['raos'].empty:
            summary['RAO_Data_Points'] = self.data['raos'].size
            
            # Get frequency range
            if 'Period' in self.data['raos'].index.names:
                periods = self.data['raos'].index.get_level_values('Period').unique()
                summary['Frequency_Range'] = f"{periods.min():.1f} - {periods.max():.1f} s"
                
            # Count DOFs
            if len(self.data['raos'].columns.names) > 1:
                dofs = set()
                for col in self.data['raos'].columns:
                    if isinstance(col, tuple) and len(col) > 1:
                        dofs.add(col[1])
                summary['DOFs_Analyzed'] = len(dofs)
                
        return summary
        
    def validate_results(self) -> Dict[str, Any]:
        """Validate extracted results."""
        validation = {
            'is_valid': True,
            'warnings': [],
            'errors': []
        }
        
        # Check if any data was extracted
        has_data = False
        for key, value in self.data.items():
            if isinstance(value, pd.DataFrame) and not value.empty:
                has_data = True
                break
            elif isinstance(value, dict) and any(not v.empty for v in value.values()):
                has_data = True
                break
                
        if not has_data:
            validation['errors'].append("No data extracted from result files")
            validation['is_valid'] = False
            
        # Validate RAOs
        if isinstance(self.data['raos'], pd.DataFrame) and not self.data['raos'].empty:
            # Check for NaN values
            if self.data['raos'].isna().any().any():
                validation['warnings'].append("RAO data contains NaN values")
                
            # Check for negative values
            if (self.data['raos'] < 0).any().any():
                validation['warnings'].append("RAO data contains negative values")
                
        # Validate hydrodynamic coefficients
        if isinstance(self.data['added_mass'], pd.DataFrame) and not self.data['added_mass'].empty:
            # Added mass should be positive
            numeric_cols = self.data['added_mass'].select_dtypes(include=[np.number]).columns
            if (self.data['added_mass'][numeric_cols] < 0).any().any():
                validation['warnings'].append("Added mass contains negative values")
                
        return validation


def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(description='Post-process OrcaWave results')
    parser.add_argument('results_dir', help='Directory containing OrcaWave results')
    parser.add_argument('--excel', help='Output Excel file path')
    parser.add_argument('--orcaflex', help='Output OrcaFlex YAML file path')
    parser.add_argument('--validate', action='store_true', help='Validate results')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose logging')
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
        
    # Process results
    processor = OrcaWavePostProcessor(Path(args.results_dir))
    
    # Find and extract result files
    files = processor.find_result_files()
    
    if not files:
        print("❌ No OrcaWave result files found")
        return
        
    print(f"\n✅ Found {len(files)} result file types")
    
    # Extract data
    if 'raos' in files:
        processor.extract_raos(files['raos'])
        
    processor.extract_hydrodynamic_coefficients(files)
    processor.extract_qtf_data(files)
    
    # Validate if requested
    if args.validate:
        validation = processor.validate_results()
        print("\n=== Results Validation ===")
        print(f"Valid: {validation['is_valid']}")
        
        if validation['errors']:
            print("\nErrors:")
            for error in validation['errors']:
                print(f"  ❌ {error}")
                
        if validation['warnings']:
            print("\nWarnings:")
            for warning in validation['warnings']:
                print(f"  ⚠️ {warning}")
                
    # Export to Excel
    if args.excel:
        if processor.export_to_excel(Path(args.excel)):
            print(f"\n✅ Exported to Excel: {args.excel}")
        else:
            print(f"\n❌ Failed to export to Excel")
            
    # Export to OrcaFlex
    if args.orcaflex:
        if processor.export_to_orcaflex(Path(args.orcaflex)):
            print(f"\n✅ Exported to OrcaFlex: {args.orcaflex}")
        else:
            print(f"\n❌ Failed to export to OrcaFlex")
            

if __name__ == "__main__":
    main()