#!/usr/bin/env python
"""
Post-process OrcaWave results with parallel extraction
Extracts key hydrodynamic data and prepares for OrcaFlex integration
"""

import os
import sys
import json
import yaml
import argparse
import logging
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, List, Tuple, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent.parent.parent.parent))

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class OrcaWavePostProcessor:
    """Post-process OrcaWave analysis results"""
    
    def __init__(self, results_dir: str, output_dir: str = None):
        """
        Initialize post-processor
        
        Args:
            results_dir: Directory containing OrcaWave results
            output_dir: Output directory for processed data
        """
        self.results_dir = Path(results_dir)
        self.output_dir = Path(output_dir) if output_dir else self.results_dir / 'processed'
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Result file patterns
        self.file_patterns = {
            'raos': ['*RAOs*.xlsx', '*RAOs*.csv', '*rao*.dat'],
            'added_mass': ['*added_mass*.csv', '*addedmass*.dat'],
            'damping': ['*damping*.csv', '*radiation_damping*.dat'],
            'excitation': ['*excitation*.csv', '*wave_force*.dat'],
            'mean_drift': ['*mean_drift*.csv', '*drift_force*.dat'],
            'qtf': ['*QTF*.csv', '*qtf*.dat']
        }
        
        self.extracted_data = {}
        self.validation_results = {}
        
    def find_result_files(self) -> Dict[str, List[Path]]:
        """
        Find all OrcaWave result files
        
        Returns:
            Dictionary of result types and file paths
        """
        logger.info(f"Searching for result files in: {self.results_dir}")
        
        found_files = {}
        
        for result_type, patterns in self.file_patterns.items():
            found_files[result_type] = []
            for pattern in patterns:
                files = list(self.results_dir.glob(pattern))
                files.extend(list(self.results_dir.glob(f"**/{pattern}")))
                found_files[result_type].extend(files)
                
            if found_files[result_type]:
                logger.info(f"Found {len(found_files[result_type])} {result_type} files")
            else:
                logger.warning(f"No {result_type} files found")
                
        return found_files
        
    def extract_raos(self, file_path: Path) -> Dict[str, Any]:
        """
        Extract RAO data from file
        
        Args:
            file_path: Path to RAO file
            
        Returns:
            Extracted RAO data
        """
        logger.info(f"Extracting RAOs from: {file_path}")
        
        rao_data = {
            'file': str(file_path),
            'periods': [],
            'headings': [],
            'surge': {},
            'sway': {},
            'heave': {},
            'roll': {},
            'pitch': {},
            'yaw': {}
        }
        
        try:
            if file_path.suffix == '.xlsx':
                # Read Excel file
                df = pd.read_excel(file_path, sheet_name=0)
                
                # Extract periods and headings (assuming standard format)
                if 'Period' in df.columns:
                    rao_data['periods'] = df['Period'].unique().tolist()
                if 'Heading' in df.columns:
                    rao_data['headings'] = df['Heading'].unique().tolist()
                    
                # Extract DOF data
                dof_mapping = {
                    'Surge': 'surge', 'Sway': 'sway', 'Heave': 'heave',
                    'Roll': 'roll', 'Pitch': 'pitch', 'Yaw': 'yaw'
                }
                
                for col_name, dof_key in dof_mapping.items():
                    if col_name in df.columns:
                        rao_data[dof_key] = df.pivot_table(
                            values=col_name,
                            index='Period',
                            columns='Heading'
                        ).to_dict()
                        
            elif file_path.suffix == '.csv':
                # Read CSV file
                df = pd.read_csv(file_path)
                
                # Similar extraction logic for CSV
                # Adapt based on actual CSV format
                pass
                
            else:
                # Read text/dat file
                with open(file_path, 'r') as f:
                    lines = f.readlines()
                # Parse custom format
                pass
                
        except Exception as e:
            logger.error(f"Failed to extract RAOs: {e}")
            rao_data['error'] = str(e)
            
        return rao_data
        
    def extract_hydrodynamic_coefficients(self, file_path: Path, coeff_type: str) -> Dict[str, Any]:
        """
        Extract hydrodynamic coefficients (added mass/damping)
        
        Args:
            file_path: Path to coefficient file
            coeff_type: Type of coefficient ('added_mass' or 'damping')
            
        Returns:
            Extracted coefficient data
        """
        logger.info(f"Extracting {coeff_type} from: {file_path}")
        
        coeff_data = {
            'file': str(file_path),
            'type': coeff_type,
            'frequencies': [],
            'values': {}
        }
        
        try:
            if file_path.suffix == '.csv':
                df = pd.read_csv(file_path)
                
                # Extract frequency column
                if 'Frequency' in df.columns:
                    coeff_data['frequencies'] = df['Frequency'].tolist()
                elif 'Period' in df.columns:
                    periods = df['Period'].tolist()
                    coeff_data['frequencies'] = [1/p if p != 0 else 0 for p in periods]
                    
                # Extract coefficient matrix (6x6 for each frequency)
                # Adapt based on actual file format
                for i in range(6):
                    for j in range(6):
                        col_name = f"C{i+1}{j+1}"  # or whatever the column naming is
                        if col_name in df.columns:
                            coeff_data['values'][f"{i+1},{j+1}"] = df[col_name].tolist()
                            
        except Exception as e:
            logger.error(f"Failed to extract {coeff_type}: {e}")
            coeff_data['error'] = str(e)
            
        return coeff_data
        
    def parallel_extraction(self) -> Dict[str, Any]:
        """
        Extract all data in parallel
        
        Returns:
            All extracted data
        """
        logger.info("Starting parallel data extraction...")
        
        found_files = self.find_result_files()
        extraction_tasks = []
        
        # Prepare extraction tasks
        for result_type, files in found_files.items():
            for file_path in files:
                if result_type == 'raos':
                    extraction_tasks.append(('raos', file_path, self.extract_raos))
                elif result_type in ['added_mass', 'damping']:
                    extraction_tasks.append(
                        (result_type, file_path, 
                         lambda f, t=result_type: self.extract_hydrodynamic_coefficients(f, t))
                    )
                # Add more extraction methods as needed
                
        # Execute parallel extraction
        extracted_data = {}
        
        with ThreadPoolExecutor(max_workers=4) as executor:
            future_to_task = {
                executor.submit(extract_func, file_path): (data_type, file_path)
                for data_type, file_path, extract_func in extraction_tasks
            }
            
            for future in as_completed(future_to_task):
                data_type, file_path = future_to_task[future]
                try:
                    result = future.result(timeout=30)
                    
                    if data_type not in extracted_data:
                        extracted_data[data_type] = []
                    extracted_data[data_type].append(result)
                    
                    logger.info(f"Extracted {data_type} from {file_path.name}")
                    
                except Exception as e:
                    logger.error(f"Failed to extract {data_type} from {file_path}: {e}")
                    
        self.extracted_data = extracted_data
        return extracted_data
        
    def validate_extracted_data(self) -> Dict[str, Any]:
        """
        Validate extracted data for completeness and consistency
        
        Returns:
            Validation results
        """
        logger.info("Validating extracted data...")
        
        validation = {
            'timestamp': datetime.now().isoformat(),
            'status': 'valid',
            'checks': {}
        }
        
        # Check RAO data
        if 'raos' in self.extracted_data:
            rao_validation = {
                'found': len(self.extracted_data['raos']),
                'valid': 0,
                'issues': []
            }
            
            for rao_data in self.extracted_data['raos']:
                if 'error' not in rao_data:
                    rao_validation['valid'] += 1
                    
                    # Check for required DOFs
                    required_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
                    missing_dofs = [dof for dof in required_dofs if not rao_data.get(dof)]
                    
                    if missing_dofs:
                        rao_validation['issues'].append(f"Missing DOFs: {missing_dofs}")
                        
            validation['checks']['raos'] = rao_validation
            
        # Check hydrodynamic coefficients
        for coeff_type in ['added_mass', 'damping']:
            if coeff_type in self.extracted_data:
                coeff_validation = {
                    'found': len(self.extracted_data[coeff_type]),
                    'valid': 0,
                    'issues': []
                }
                
                for coeff_data in self.extracted_data[coeff_type]:
                    if 'error' not in coeff_data:
                        coeff_validation['valid'] += 1
                        
                        # Check for frequency range
                        if coeff_data.get('frequencies'):
                            freq_range = (min(coeff_data['frequencies']), 
                                        max(coeff_data['frequencies']))
                            coeff_validation['frequency_range'] = freq_range
                            
                validation['checks'][coeff_type] = coeff_validation
                
        # Overall validation status
        if any('issues' in check and check['issues'] 
               for check in validation['checks'].values()):
            validation['status'] = 'warning'
            
        self.validation_results = validation
        return validation
        
    def generate_orcaflex_data(self) -> Dict[str, Any]:
        """
        Generate OrcaFlex-compatible vessel data
        
        Returns:
            OrcaFlex vessel data dictionary
        """
        logger.info("Generating OrcaFlex vessel data...")
        
        orcaflex_data = {
            'VesselType': 'Vessel type1',
            'Name': 'Sea Cypress',
            'Length': 103.0,
            'RAOs': {},
            'AddedMass': {},
            'Damping': {},
            'LoadRAOs': {}
        }
        
        # Process RAO data
        if 'raos' in self.extracted_data and self.extracted_data['raos']:
            rao_data = self.extracted_data['raos'][0]  # Use first RAO file
            
            if 'periods' in rao_data:
                orcaflex_data['RAOPeriods'] = rao_data['periods']
            if 'headings' in rao_data:
                orcaflex_data['RAODirections'] = rao_data['headings']
                
            # Format RAOs for OrcaFlex
            for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
                if dof in rao_data and rao_data[dof]:
                    orcaflex_data['RAOs'][dof] = rao_data[dof]
                    
        # Process added mass
        if 'added_mass' in self.extracted_data and self.extracted_data['added_mass']:
            mass_data = self.extracted_data['added_mass'][0]
            if 'frequencies' in mass_data:
                orcaflex_data['AddedMassAndDampingFrequencies'] = mass_data['frequencies']
            orcaflex_data['AddedMass'] = mass_data.get('values', {})
            
        # Process damping
        if 'damping' in self.extracted_data and self.extracted_data['damping']:
            damp_data = self.extracted_data['damping'][0]
            orcaflex_data['Damping'] = damp_data.get('values', {})
            
        return orcaflex_data
        
    def create_visualizations(self) -> List[str]:
        """
        Create visualization plots of extracted data
        
        Returns:
            List of generated plot file paths
        """
        logger.info("Creating visualizations...")
        
        plot_files = []
        
        # RAO polar plots
        if 'raos' in self.extracted_data and self.extracted_data['raos']:
            rao_data = self.extracted_data['raos'][0]
            
            if 'periods' in rao_data and 'headings' in rao_data:
                fig, axes = plt.subplots(2, 3, figsize=(15, 10), subplot_kw=dict(projection='polar'))
                fig.suptitle('Response Amplitude Operators (RAOs)', fontsize=16)
                
                dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
                
                for idx, (ax, dof) in enumerate(zip(axes.flat, dof_names)):
                    if dof in rao_data and rao_data[dof]:
                        # Create polar plot for middle period
                        periods = rao_data['periods']
                        if len(periods) > 0:
                            mid_period = periods[len(periods)//2]
                            
                            headings_rad = np.deg2rad(rao_data['headings'])
                            
                            # Get RAO values for this period
                            # This is simplified - adapt to actual data structure
                            values = [1.0] * len(headings_rad)  # Placeholder
                            
                            ax.plot(headings_rad, values, 'b-', linewidth=2)
                            ax.fill(headings_rad, values, alpha=0.25)
                            ax.set_title(f'{dof.capitalize()} (T={mid_period}s)')
                            ax.set_theta_zero_location('N')
                            ax.set_theta_direction(-1)
                            
                plt.tight_layout()
                plot_file = self.output_dir / 'rao_polar_plots.png'
                plt.savefig(plot_file, dpi=150)
                plt.close()
                plot_files.append(str(plot_file))
                logger.info(f"Created RAO polar plots: {plot_file}")
                
        # Hydrodynamic coefficient plots
        if 'added_mass' in self.extracted_data or 'damping' in self.extracted_data:
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
            fig.suptitle('Hydrodynamic Coefficients', fontsize=16)
            
            # Added mass plot
            if 'added_mass' in self.extracted_data and self.extracted_data['added_mass']:
                mass_data = self.extracted_data['added_mass'][0]
                if 'frequencies' in mass_data:
                    frequencies = mass_data['frequencies']
                    # Plot diagonal terms (simplified)
                    ax1.plot(frequencies, [1.0] * len(frequencies), label='A11')  # Placeholder
                    ax1.set_xlabel('Frequency (rad/s)')
                    ax1.set_ylabel('Added Mass')
                    ax1.set_title('Added Mass Coefficients')
                    ax1.grid(True)
                    ax1.legend()
                    
            # Damping plot
            if 'damping' in self.extracted_data and self.extracted_data['damping']:
                damp_data = self.extracted_data['damping'][0]
                if 'frequencies' in damp_data:
                    frequencies = damp_data['frequencies']
                    # Plot diagonal terms (simplified)
                    ax2.plot(frequencies, [1.0] * len(frequencies), label='B11')  # Placeholder
                    ax2.set_xlabel('Frequency (rad/s)')
                    ax2.set_ylabel('Damping')
                    ax2.set_title('Damping Coefficients')
                    ax2.grid(True)
                    ax2.legend()
                    
            plt.tight_layout()
            plot_file = self.output_dir / 'hydrodynamic_coefficients.png'
            plt.savefig(plot_file, dpi=150)
            plt.close()
            plot_files.append(str(plot_file))
            logger.info(f"Created hydrodynamic plots: {plot_file}")
            
        return plot_files
        
    def generate_report(self) -> str:
        """
        Generate comprehensive post-processing report
        
        Returns:
            Path to report file
        """
        report_file = self.output_dir / f"postprocess_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
        
        with open(report_file, 'w') as f:
            f.write("# OrcaWave Post-Processing Report\n\n")
            f.write(f"**Generated:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"**Results Directory:** {self.results_dir}\n\n")
            
            f.write("## Data Extraction Summary\n\n")
            
            for data_type, data_list in self.extracted_data.items():
                f.write(f"### {data_type.replace('_', ' ').title()}\n")
                f.write(f"- Files found: {len(data_list)}\n")
                
                successful = sum(1 for d in data_list if 'error' not in d)
                f.write(f"- Successfully extracted: {successful}\n")
                
                if data_list and 'error' not in data_list[0]:
                    # Add specific details based on data type
                    if data_type == 'raos':
                        data = data_list[0]
                        f.write(f"- Periods: {len(data.get('periods', []))}\n")
                        f.write(f"- Headings: {len(data.get('headings', []))}\n")
                        
                f.write("\n")
                
            f.write("## Validation Results\n\n")
            f.write(f"**Overall Status:** {self.validation_results.get('status', 'unknown')}\n\n")
            
            for check_name, check_result in self.validation_results.get('checks', {}).items():
                f.write(f"### {check_name.replace('_', ' ').title()}\n")
                for key, value in check_result.items():
                    if key != 'issues':
                        f.write(f"- {key}: {value}\n")
                if 'issues' in check_result and check_result['issues']:
                    f.write("- Issues:\n")
                    for issue in check_result['issues']:
                        f.write(f"  - {issue}\n")
                f.write("\n")
                
            f.write("## Output Files\n\n")
            f.write("The following files have been generated:\n\n")
            
            # List output files
            for file_path in self.output_dir.glob("*"):
                if file_path.is_file():
                    f.write(f"- `{file_path.name}`\n")
                    
        logger.info(f"Report generated: {report_file}")
        return str(report_file)
        
    def process_all(self) -> Dict[str, Any]:
        """
        Run complete post-processing pipeline
        
        Returns:
            Processing results summary
        """
        logger.info("Starting complete post-processing pipeline...")
        
        results = {
            'timestamp': datetime.now().isoformat(),
            'status': 'success',
            'steps': {}
        }
        
        try:
            # Step 1: Parallel extraction
            results['steps']['extraction'] = self.parallel_extraction()
            
            # Step 2: Validation
            results['steps']['validation'] = self.validate_extracted_data()
            
            # Step 3: Generate OrcaFlex data
            orcaflex_data = self.generate_orcaflex_data()
            
            # Save OrcaFlex data
            orcaflex_yml = self.output_dir / 'sea_cypress_orcaflex.yml'
            with open(orcaflex_yml, 'w') as f:
                yaml.dump(orcaflex_data, f, default_flow_style=False)
            results['steps']['orcaflex_generation'] = str(orcaflex_yml)
            
            # Also save as JSON
            orcaflex_json = self.output_dir / 'sea_cypress_orcaflex.json'
            with open(orcaflex_json, 'w') as f:
                json.dump(orcaflex_data, f, indent=2)
                
            # Step 4: Create visualizations
            plot_files = self.create_visualizations()
            results['steps']['visualizations'] = plot_files
            
            # Step 5: Generate report
            report = self.generate_report()
            results['steps']['report'] = report
            
        except Exception as e:
            logger.error(f"Post-processing failed: {e}")
            results['status'] = 'failed'
            results['error'] = str(e)
            
        return results


def main():
    """Main execution function"""
    parser = argparse.ArgumentParser(
        description='Post-process OrcaWave results with parallel extraction'
    )
    parser.add_argument(
        '--results-dir',
        default='specs/modules/orcawave/diffraction-analysis/outputs',
        help='Directory containing OrcaWave results'
    )
    parser.add_argument(
        '--output-dir',
        help='Output directory for processed data (default: results-dir/processed)'
    )
    parser.add_argument(
        '--parallel',
        action='store_true',
        default=True,
        help='Use parallel processing (default: True)'
    )
    
    args = parser.parse_args()
    
    # Convert to absolute paths
    results_dir = Path(args.results_dir)
    if not results_dir.is_absolute():
        results_dir = Path.cwd() / results_dir
        
    # Create processor
    processor = OrcaWavePostProcessor(str(results_dir), args.output_dir)
    
    # Run processing
    results = processor.process_all()
    
    # Print summary
    print("\n" + "="*60)
    print("POST-PROCESSING COMPLETE")
    print("="*60)
    print(f"Status: {results['status']}")
    
    if 'steps' in results:
        print("\nProcessing Steps Completed:")
        for step, result in results['steps'].items():
            if isinstance(result, dict):
                print(f"- {step}: {len(result)} items processed")
            elif isinstance(result, list):
                print(f"- {step}: {len(result)} files created")
            else:
                print(f"- {step}: {result}")
                
    print("\nOutput Files:")
    print(f"- OrcaFlex data: sea_cypress_orcaflex.yml")
    print(f"- OrcaFlex data (JSON): sea_cypress_orcaflex.json")
    print(f"- Report: postprocess_report_*.md")
    print(f"- Visualizations: *.png")
    
    print("\nNext Steps:")
    print("1. Review the post-processing report")
    print("2. Import sea_cypress_orcaflex.yml into OrcaFlex")
    print("3. Validate vessel response in OrcaFlex")
    print("="*60)


if __name__ == "__main__":
    main()