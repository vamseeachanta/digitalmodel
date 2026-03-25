#!/usr/bin/env python
"""
Tension to Stress Conversion Module for Fatigue Analysis
========================================================

This module converts tension ranges from rainflow counting to stress ranges
using FEA (Finite Element Analysis) lookup tables. It supports linear interpolation
and extrapolation for values outside the lookup table range.

Key Features:
- FEA-based tension to stress conversion
- Linear and polynomial interpolation methods
- Support for multiple strut locations
- Stress concentration factor (SCF) application
- Unit load normalization

Input Files Required:
- tension_stress_lookup.csv: FEA results mapping tension to stress
- rainflow_cycles/FC###_cycles.csv: Cycle count data from rainflow module
- stress_concentration_factors.csv: SCF values per location (optional)
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Union
import logging
from datetime import datetime
import json
from scipy import interpolate

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class TensionStressConverter:
    """Converts tension ranges to stress ranges using FEA lookup tables"""
    
    def __init__(self,
                 lookup_table_path: str = "input/tension_stress_lookup.csv",
                 scf_path: Optional[str] = None,
                 output_dir: str = "output/stress_ranges"):
        """
        Initialize the tension to stress converter
        
        Args:
            lookup_table_path: Path to FEA lookup table
            scf_path: Path to stress concentration factors file (optional)
            output_dir: Directory for stress range outputs
        """
        self.lookup_table_path = lookup_table_path
        self.scf_path = scf_path
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Load lookup table and SCF data
        self.lookup_table = None
        self.scf_data = None
        self.interpolator = None
        
        self._load_lookup_table()
        self._load_scf_data()
        self._create_interpolator()
    
    def _load_lookup_table(self):
        """Load FEA tension to stress lookup table"""
        logger.info(f"Loading lookup table from {self.lookup_table_path}")
        
        if Path(self.lookup_table_path).exists():
            self.lookup_table = pd.read_csv(self.lookup_table_path)
            
            # Validate required columns
            required_cols = ['tension_kn', 'stress_mpa']
            if not all(col in self.lookup_table.columns for col in required_cols):
                # Try alternative column names
                tension_cols = [c for c in self.lookup_table.columns if 'tension' in c.lower()]
                stress_cols = [c for c in self.lookup_table.columns if 'stress' in c.lower()]
                
                if tension_cols and stress_cols:
                    self.lookup_table = self.lookup_table.rename(columns={
                        tension_cols[0]: 'tension_kn',
                        stress_cols[0]: 'stress_mpa'
                    })
                else:
                    logger.warning("Could not find tension/stress columns in lookup table")
                    self._create_default_lookup_table()
            
            logger.info(f"Loaded {len(self.lookup_table)} lookup points")
        else:
            logger.warning(f"Lookup table not found: {self.lookup_table_path}")
            self._create_default_lookup_table()
    
    def _create_default_lookup_table(self):
        """Create default lookup table based on typical relationships"""
        logger.info("Creating default tension-stress lookup table")
        
        # Create linear relationship: Stress = Tension / 4 (typical for steel)
        # This assumes a cross-sectional area that gives this relationship
        tensions = np.linspace(0, 2000, 21)  # 0 to 2000 kN
        stresses = tensions / 4  # Linear relationship
        
        self.lookup_table = pd.DataFrame({
            'tension_kn': tensions,
            'stress_mpa': stresses
        })
        
        # Save the default table
        self.lookup_table.to_csv(self.lookup_table_path, index=False)
        logger.info(f"Saved default lookup table to: {self.lookup_table_path}")
    
    def _load_scf_data(self):
        """Load stress concentration factors if available"""
        if self.scf_path and Path(self.scf_path).exists():
            logger.info(f"Loading SCF data from {self.scf_path}")
            self.scf_data = pd.read_csv(self.scf_path)
            
            # Create SCF lookup dictionary
            if 'location' in self.scf_data.columns and 'scf' in self.scf_data.columns:
                self.scf_dict = dict(zip(
                    self.scf_data['location'],
                    self.scf_data['scf']
                ))
            else:
                self.scf_dict = {}
        else:
            logger.info("No SCF data provided, using SCF = 1.0")
            self.scf_dict = {}
    
    def _create_interpolator(self):
        """Create interpolation function from lookup table"""
        if self.lookup_table is not None and len(self.lookup_table) > 1:
            # Sort by tension for proper interpolation
            self.lookup_table = self.lookup_table.sort_values('tension_kn')
            
            # Create interpolator
            self.interpolator = interpolate.interp1d(
                self.lookup_table['tension_kn'].values,
                self.lookup_table['stress_mpa'].values,
                kind='linear',
                bounds_error=False,
                fill_value='extrapolate'
            )
            
            logger.info("Created interpolation function for tension-stress conversion")
        else:
            # Fallback to simple linear relationship
            self.interpolator = lambda x: x / 4
            logger.warning("Using fallback linear relationship: Stress = Tension / 4")
    
    def convert_tension_to_stress(self, 
                                 tension_range: Union[float, np.ndarray],
                                 location: Optional[str] = None) -> Union[float, np.ndarray]:
        """
        Convert tension range to stress range
        
        Args:
            tension_range: Tension range(s) in kN
            location: Location identifier for SCF lookup
        
        Returns:
            Stress range(s) in MPa
        """
        # Apply interpolation
        stress_range = self.interpolator(tension_range)
        
        # Apply SCF if available
        if location and location in self.scf_dict:
            scf = self.scf_dict[location]
            stress_range = stress_range * scf
            logger.debug(f"Applied SCF={scf} for location {location}")
        
        return stress_range
    
    def process_rainflow_results(self,
                                cycles_file: Union[str, Path],
                                location: Optional[str] = None) -> Dict:
        """
        Process rainflow counting results to convert tension to stress
        
        Args:
            cycles_file: Path to rainflow cycles CSV file
            location: Location identifier for SCF lookup
        
        Returns:
            Dictionary with stress ranges and cycle counts
        """
        cycles_file = Path(cycles_file)
        
        # Read cycle data
        # Skip comment lines
        with open(cycles_file, 'r') as f:
            lines = f.readlines()
            data_start = 0
            for i, line in enumerate(lines):
                if not line.startswith('#'):
                    data_start = i
                    break
        
        df = pd.read_csv(cycles_file, skiprows=data_start)
        
        if len(df) == 0:
            logger.warning(f"No cycles found in {cycles_file}")
            return {
                'file': cycles_file.name,
                'location': location,
                'stress_ranges': np.array([]),
                'cycle_counts': np.array([]),
                'statistics': {}
            }
        
        # Convert tension ranges to stress ranges
        tension_ranges = df['range'].values
        cycle_counts = df['cycles'].values
        
        stress_ranges = self.convert_tension_to_stress(tension_ranges, location)
        
        # Calculate statistics
        statistics = {
            'num_ranges': len(stress_ranges),
            'max_stress': float(np.max(stress_ranges)) if len(stress_ranges) > 0 else 0,
            'min_stress': float(np.min(stress_ranges)) if len(stress_ranges) > 0 else 0,
            'mean_stress': float(np.mean(stress_ranges)) if len(stress_ranges) > 0 else 0,
            'weighted_mean_stress': float(np.average(stress_ranges, weights=cycle_counts)) if len(stress_ranges) > 0 else 0,
            'total_cycles': float(np.sum(cycle_counts)) if len(cycle_counts) > 0 else 0
        }
        
        result = {
            'file': cycles_file.name,
            'location': location,
            'stress_ranges': stress_ranges,
            'cycle_counts': cycle_counts,
            'statistics': statistics
        }
        
        return result
    
    def save_stress_data(self, result: Dict, output_path: Union[str, Path]):
        """
        Save stress range data to file
        
        Args:
            result: Stress conversion results
            output_path: Path to save the results
        """
        output_path = Path(output_path)
        
        # Create DataFrame with results
        if len(result['stress_ranges']) > 0:
            df = pd.DataFrame({
                'stress_mpa': result['stress_ranges'],
                'cycles': result['cycle_counts']
            })
        else:
            df = pd.DataFrame(columns=['stress_mpa', 'cycles'])
        
        # Add metadata as comments
        with open(output_path, 'w') as f:
            f.write(f"# Stress Range Data\n")
            f.write(f"# Source: {result['file']}\n")
            f.write(f"# Location: {result.get('location', 'default')}\n")
            f.write(f"# Max Stress: {result['statistics'].get('max_stress', 0):.2f} MPa\n")
            f.write(f"# Mean Stress: {result['statistics'].get('mean_stress', 0):.2f} MPa\n")
            f.write(f"# Total Cycles: {result['statistics'].get('total_cycles', 0):.2f}\n")
            df.to_csv(f, index=False)
    
    def process_batch(self,
                     input_dir: Union[str, Path],
                     pattern: str = "*_cycles.csv",
                     locations: Optional[Dict[str, str]] = None) -> pd.DataFrame:
        """
        Process multiple cycle files in batch
        
        Args:
            input_dir: Directory containing cycle count files
            pattern: File pattern to match
            locations: Dictionary mapping file patterns to locations
        
        Returns:
            Summary DataFrame with all results
        """
        input_dir = Path(input_dir)
        files = sorted(input_dir.glob(pattern))
        
        logger.info(f"Processing {len(files)} cycle files from {input_dir}")
        
        results = []
        
        for i, file_path in enumerate(files):
            # Determine location if provided
            location = None
            if locations:
                for pattern, loc in locations.items():
                    if pattern in file_path.name:
                        location = loc
                        break
            
            # Process file
            result = self.process_rainflow_results(file_path, location)
            
            # Save individual results
            output_file = self.output_dir / f"{file_path.stem}_stress.csv"
            self.save_stress_data(result, output_file)
            
            # Add to summary
            results.append({
                'file': file_path.name,
                'location': location or 'default',
                'num_ranges': result['statistics']['num_ranges'],
                'max_stress_mpa': result['statistics']['max_stress'],
                'mean_stress_mpa': result['statistics']['mean_stress'],
                'weighted_mean_mpa': result['statistics']['weighted_mean_stress'],
                'total_cycles': result['statistics']['total_cycles']
            })
            
            if (i + 1) % 10 == 0:
                logger.info(f"Processed {i + 1}/{len(files)} files")
        
        # Create summary DataFrame
        summary_df = pd.DataFrame(results)
        
        # Save summary
        summary_path = self.output_dir / "stress_conversion_summary.csv"
        summary_df.to_csv(summary_path, index=False)
        logger.info(f"Saved summary to: {summary_path}")
        
        # Generate report
        self._generate_report(summary_df)
        
        return summary_df
    
    def visualize_lookup_table(self, save_path: Optional[str] = None):
        """
        Create visualization of the tension-stress relationship
        
        Args:
            save_path: Path to save the plot (optional)
        """
        try:
            import matplotlib.pyplot as plt
        except ImportError:
            logger.warning("Matplotlib not available for visualization")
            return
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # Plot lookup table points
        ax1.scatter(self.lookup_table['tension_kn'],
                   self.lookup_table['stress_mpa'],
                   label='Lookup Points', color='blue', s=50)
        
        # Plot interpolation line
        tension_range = np.linspace(0, self.lookup_table['tension_kn'].max() * 1.2, 100)
        stress_range = self.interpolator(tension_range)
        ax1.plot(tension_range, stress_range, 'r-', label='Interpolation', alpha=0.7)
        
        ax1.set_xlabel('Tension Range (kN)')
        ax1.set_ylabel('Stress Range (MPa)')
        ax1.set_title('Tension to Stress Conversion')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        
        # Plot derivative (stress per unit tension)
        if len(self.lookup_table) > 1:
            tensions = self.lookup_table['tension_kn'].values
            stresses = self.lookup_table['stress_mpa'].values
            derivatives = np.gradient(stresses, tensions)
            
            ax2.plot(tensions[:-1], derivatives[:-1], 'g-', linewidth=2)
            ax2.set_xlabel('Tension Range (kN)')
            ax2.set_ylabel('dStress/dTension (MPa/kN)')
            ax2.set_title('Conversion Rate')
            ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=150, bbox_inches='tight')
            logger.info(f"Saved visualization to: {save_path}")
        else:
            plt.show()
    
    def _generate_report(self, summary_df: pd.DataFrame):
        """Generate processing report"""
        report_path = self.output_dir / "stress_conversion_report.txt"
        
        with open(report_path, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("TENSION TO STRESS CONVERSION REPORT\n")
            f.write("=" * 80 + "\n\n")
            
            f.write(f"Generated: {datetime.now().isoformat()}\n")
            f.write(f"Total Files Processed: {len(summary_df)}\n\n")
            
            f.write("LOOKUP TABLE INFO:\n")
            if self.lookup_table is not None:
                f.write(f"  Lookup Points: {len(self.lookup_table)}\n")
                f.write(f"  Tension Range: {self.lookup_table['tension_kn'].min():.1f} - "
                       f"{self.lookup_table['tension_kn'].max():.1f} kN\n")
                f.write(f"  Stress Range: {self.lookup_table['stress_mpa'].min():.1f} - "
                       f"{self.lookup_table['stress_mpa'].max():.1f} MPa\n")
            
            if self.scf_dict:
                f.write(f"  SCF Locations: {list(self.scf_dict.keys())}\n")
                f.write(f"  SCF Range: {min(self.scf_dict.values()):.2f} - "
                       f"{max(self.scf_dict.values()):.2f}\n")
            else:
                f.write("  SCF: 1.0 (default)\n")
            f.write("\n")
            
            f.write("STRESS STATISTICS:\n")
            f.write(f"  Maximum Stress: {summary_df['max_stress_mpa'].max():.2f} MPa\n")
            f.write(f"  Average Max Stress: {summary_df['max_stress_mpa'].mean():.2f} MPa\n")
            f.write(f"  Average Mean Stress: {summary_df['mean_stress_mpa'].mean():.2f} MPa\n\n")
            
            f.write("TOP 5 HIGHEST STRESS FILES:\n")
            top5 = summary_df.nlargest(5, 'max_stress_mpa')
            for _, row in top5.iterrows():
                f.write(f"  {row['file']}: {row['max_stress_mpa']:.2f} MPa\n")
        
        logger.info(f"Generated report: {report_path}")


def main():
    """Main execution function"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Tension to Stress Conversion")
    parser.add_argument('--lookup-table', default='input/tension_stress_lookup.csv',
                       help='Path to FEA lookup table')
    parser.add_argument('--scf-file', default=None,
                       help='Path to stress concentration factors file')
    parser.add_argument('--input-dir', default='output/rainflow_cycles',
                       help='Directory containing cycle count files')
    parser.add_argument('--output-dir', default='output/stress_ranges',
                       help='Output directory for stress ranges')
    parser.add_argument('--pattern', default='*_cycles.csv',
                       help='File pattern to match')
    parser.add_argument('--visualize', action='store_true',
                       help='Create visualization of lookup table')
    parser.add_argument('--single-file', default=None,
                       help='Process a single file instead of batch')
    
    args = parser.parse_args()
    
    # Create converter
    converter = TensionStressConverter(
        lookup_table_path=args.lookup_table,
        scf_path=args.scf_file,
        output_dir=args.output_dir
    )
    
    # Visualize if requested
    if args.visualize:
        viz_path = Path(args.output_dir) / "lookup_visualization.png"
        converter.visualize_lookup_table(viz_path)
    
    if args.single_file:
        # Process single file
        result = converter.process_rainflow_results(args.single_file)
        output_path = Path(args.output_dir) / f"{Path(args.single_file).stem}_stress.csv"
        converter.save_stress_data(result, output_path)
        logger.info(f"Processed {args.single_file}")
        logger.info(f"Results saved to: {output_path}")
    else:
        # Process batch
        summary = converter.process_batch(args.input_dir, args.pattern)
        logger.info("Stress conversion complete!")
        logger.info(f"Processed {len(summary)} files")
        logger.info(f"Results saved to: {args.output_dir}")


if __name__ == "__main__":
    main()