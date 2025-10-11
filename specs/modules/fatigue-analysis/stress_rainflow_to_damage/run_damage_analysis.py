#!/usr/bin/env python3
"""
Stress Rainflow to Damage Analysis
Calculates fatigue damage rate (1/year) from stress rainflow cycle counts
using S-N curves and Miner's rule

Each input stress rainflow file generates a corresponding damage rate output file.
"""

import os
import sys
import yaml
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple, Optional
import argparse
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm

class DamageAnalyzer:
    """Calculate fatigue damage from stress rainflow data using S-N curves"""
    
    def __init__(self, config_path: str):
        """Initialize analyzer with configuration file"""
        with open(config_path, 'r') as f:
            self.config = yaml.safe_load(f)
        
        self.setup_logging()
        self.logger.info(f"Initialized DamageAnalyzer with config: {config_path}")
        
    def setup_logging(self):
        """Setup logging configuration"""
        log_level = getattr(logging, self.config['logging']['level'])
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        
        # Create logger
        self.logger = logging.getLogger('DamageAnalyzer')
        self.logger.setLevel(log_level)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(log_level)
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        console_handler.setFormatter(formatter)
        self.logger.addHandler(console_handler)
        
        # File handler if enabled
        if self.config['logging']['log_to_file']:
            log_file = self.config['logging']['log_file'].replace('{timestamp}', timestamp)
            file_handler = logging.FileHandler(log_file)
            file_handler.setLevel(log_level)
            file_handler.setFormatter(formatter)
            self.logger.addHandler(file_handler)
    
    def calculate_cycles_to_failure(self, stress_range: float) -> float:
        """
        Calculate number of cycles to failure using two-segment S-N curve

        Two-Segment S-N Curve:
        - Segment 1 (High Stress): log10(N) = log_a - m * log10(S)  [N < N_transition]
        - Segment 2 (Low Stress):  log10(N) = log_c - r * log10(S)  [N >= N_transition]

        Where: N = cycles to failure, S = stress range
        """
        params = self.config['sn_curve']['parameters']
        damage_opts = self.config['damage_calculation']['options']

        # Check if we should ignore below fatigue limit (infinite life)
        if damage_opts.get('ignore_below_fatigue_limit', False):
            if stress_range <= params['fatigue_limit_stress']:
                return float('inf')  # Infinite life below fatigue limit

        # Calculate N using Segment 1 (high stress region)
        log_N1 = params['log_a'] - params['m'] * np.log10(stress_range)
        N1 = 10 ** log_N1

        # Check if two-segment curve is enabled and if we're in Segment 2 region
        two_seg = params.get('two_segment', {})
        if two_seg.get('enabled', False):
            transition_cycles = float(params.get('fatigue_limit_cycles', 1.0e7))

            # If N1 >= transition cycles, use Segment 2 instead
            if N1 >= transition_cycles:
                log_N2 = two_seg['log_c'] - two_seg['r'] * np.log10(stress_range)
                N2 = 10 ** log_N2
                return N2

        return N1
    
    def get_thickness_correction_factor(self, location_id: str, thickness_mm: float) -> float:
        """
        Calculate thickness correction factor
        TCF = (t / t_ref)^tk
        """
        if not self.config['thickness_correction']['enabled']:
            return 1.0
        
        t_ref = self.config['thickness_correction']['reference_thickness_mm']
        tk = self.config['thickness_correction']['thickness_exponent_tk']
        
        tcf = (thickness_mm / t_ref) ** tk
        return tcf
    
    def get_scf(self, location_id: str) -> float:
        """Get Stress Concentration Factor for location"""
        if not self.config['stress_factors']['stress_concentration']['enabled']:
            return 1.0
        
        scf_dict = self.config['stress_factors']['stress_concentration']['location_scf']
        return scf_dict.get(location_id, scf_dict.get('default', 1.0))
    
    def get_thickness(self, location_id: str) -> float:
        """Get thickness for location"""
        thickness_dict = self.config['thickness_correction']['location_thickness_mm']
        return thickness_dict.get(location_id, thickness_dict.get('default', 22.0))
    
    def process_single_file(self, input_file: Path) -> Dict:
        """
        Process a single stress rainflow file and calculate damage
        Returns dictionary with results
        """
        # Parse filename to extract metadata
        filename = input_file.stem  # Remove .csv
        parts = filename.split('_')
        
        # Extract metadata from filename pattern
        # Pattern: {config}_FC{###}_Strut{#}_{location_id}_stress_rainflow
        # Find location_id (should be like loc02, loc03, etc.)
        location_id = None
        strut_num = None
        fc_num = None
        config_name = None
        
        # Find the parts
        for i, part in enumerate(parts):
            if part.startswith('loc'):
                location_id = part
            if part.startswith('Strut'):
                strut_num = part[5:]  # Remove 'Strut' prefix
            if part.startswith('fc') or part.startswith('FC'):
                fc_num = part[2:]  # Remove 'fc' or 'FC' prefix
                config_name = '_'.join(parts[:i])
        
        if not location_id:
            # Fallback: assume location_id is between Strut and 'stress'
            for i, part in enumerate(parts):
                if part == 'stress' and i > 0:
                    location_id = parts[i-1]
                    break
        
        self.logger.info(f"Processing: {filename}")
        self.logger.debug(f"  Config: {config_name}, FC: {fc_num}, Strut: {strut_num}, Location: {location_id}")
        
        # Read input data
        df = pd.read_csv(input_file)
        stress_col = self.config['input_data']['stress_rainflow']['columns']['stress_range']
        cycles_col = self.config['input_data']['stress_rainflow']['columns']['annual_cycles']
        
        # Rename columns for consistency
        df = df.rename(columns={
            stress_col: 'stress_range_mpa',
            cycles_col: 'cycles_annual'
        })
        
        # Get location-specific parameters
        scf = self.get_scf(location_id)
        thickness_mm = self.get_thickness(location_id)
        tcf = self.get_thickness_correction_factor(location_id, thickness_mm)
        
        self.logger.debug(f"  SCF: {scf}, Thickness: {thickness_mm}mm, TCF: {tcf:.4f}")
        
        # Apply SCF to stress ranges
        df['stress_range_corrected'] = df['stress_range_mpa'] * scf
        
        # Apply thickness correction to S-N curve (affects allowable stress)
        # TCF > 1 for thicker sections (higher allowable stress)
        # TCF < 1 for thinner sections (lower allowable stress)
        df['stress_range_for_sn'] = df['stress_range_corrected'] / tcf
        
        # Calculate cycles to failure for each stress range
        df['cycles_to_failure'] = df['stress_range_for_sn'].apply(self.calculate_cycles_to_failure)
        
        # Calculate damage per stress range bin (Miner's rule: ni/Ni)
        df['damage_per_bin'] = df.apply(
            lambda row: row['cycles_annual'] / row['cycles_to_failure'] 
            if row['cycles_to_failure'] != float('inf') else 0.0,
            axis=1
        )
        
        # Damage rate per year is same as damage per bin (since cycles are annual)
        df['damage_rate_per_year'] = df['damage_per_bin']
        
        # Calculate total damage rate (sum of all bins)
        total_damage_rate = df['damage_rate_per_year'].sum()
        
        # Calculate fatigue life
        design_factor = self.config['damage_calculation']['safety_factors']['design_factor']
        if total_damage_rate > 0:
            fatigue_life_years = 1.0 / total_damage_rate
            design_life_years = fatigue_life_years / design_factor
        else:
            fatigue_life_years = float('inf')
            design_life_years = float('inf')
        
        self.logger.info(f"  Total Damage Rate: {total_damage_rate:.6e} /year")
        self.logger.info(f"  Fatigue Life: {fatigue_life_years:.1f} years")
        self.logger.info(f"  Design Life (DF={design_factor}): {design_life_years:.1f} years")
        
        # Prepare output DataFrame
        output_df = pd.DataFrame({
            'stress_range_mpa': df['stress_range_mpa'],
            'cycles_annual': df['cycles_annual'],
            'stress_corrected_mpa': df['stress_range_corrected'],
            'cycles_to_failure': df['cycles_to_failure'],
            'damage_per_bin': df['damage_per_bin'],
            'damage_rate_per_year': df['damage_rate_per_year']
        })
        
        # Add total row
        total_row = pd.DataFrame({
            'stress_range_mpa': ['TOTAL'],
            'cycles_annual': [df['cycles_annual'].sum()],
            'stress_corrected_mpa': ['-'],
            'cycles_to_failure': ['-'],
            'damage_per_bin': [total_damage_rate],
            'damage_rate_per_year': [total_damage_rate]
        })
        output_df = pd.concat([output_df, total_row], ignore_index=True)
        
        # Generate output filename
        output_pattern = self.config['output']['file_naming']['pattern']
        output_name = output_pattern.format(
            config=config_name,
            fc_number=int(fc_num),
            strut_number=strut_num,
            location_id=location_id
        )
        
        # Create output path
        output_base = Path(self.config['output']['folders']['base'])
        output_results = output_base / self.config['output']['folders']['results']
        output_results.mkdir(parents=True, exist_ok=True)
        
        output_file = output_results / f"{output_name}.csv"
        
        # Save results
        output_df.to_csv(output_file, index=False)
        self.logger.info(f"  Saved: {output_file}")
        
        # Create visualization if enabled
        if self.config['visualization']['damage_plot']['enabled']:
            self.create_damage_plot(df, output_name, config_name, fc_num, strut_num, location_id,
                                   total_damage_rate, fatigue_life_years)
        
        # Return summary data
        return {
            'filename': filename,
            'config': config_name,
            'fc_number': fc_num,
            'strut_number': strut_num,
            'location_id': location_id,
            'thickness_mm': thickness_mm,
            'scf': scf,
            'thickness_correction_factor': tcf,
            'total_cycles_per_year': df['cycles_annual'].sum(),
            'total_damage_rate_per_year': total_damage_rate,
            'fatigue_life_years': fatigue_life_years,
            'design_life_years': design_life_years,
            'safety_margin': fatigue_life_years / (25 * design_factor)  # Against 25-year design life
        }
    
    def create_damage_plot(self, df, output_name, config, fc_num, strut_num, location_id,
                           total_damage, fatigue_life):
        """Create damage vs stress range visualization"""
        # Filter out rows with zero cycles
        plot_df = df[df['cycles_annual'] > 0].copy()
        
        if len(plot_df) == 0:
            self.logger.warning(f"  No non-zero cycles to plot for {output_name}")
            return
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=self.config['visualization']['damage_plot']['figure_size'])
        
        # Left plot: Damage rate vs stress range
        ax1.bar(plot_df['stress_range_mpa'], plot_df['damage_rate_per_year'], 
                width=plot_df['stress_range_mpa'].diff().mean() * 0.8,
                color='steelblue', edgecolor='black', alpha=0.7)
        ax1.set_xlabel(self.config['visualization']['damage_plot']['styling']['xlabel'])
        ax1.set_ylabel('Annual Damage Rate [1/year]')
        ax1.set_title(f'Damage Distribution - {location_id}')
        ax1.grid(True, alpha=0.3)
        if self.config['visualization']['damage_plot']['styling']['log_scale_y']:
            ax1.set_yscale('log')
        
        # Add total damage annotation
        ax1.text(0.95, 0.95, f'Total Damage: {total_damage:.3e}/year\nFatigue Life: {fatigue_life:.1f} years',
                transform=ax1.transAxes, ha='right', va='top',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        
        # Right plot: S-N curve with data points
        stress_range = np.logspace(1, 3, 100)  # 10 to 1000 MPa
        cycles = [self.calculate_cycles_to_failure(s) for s in stress_range]
        cycles = [c if c != float('inf') else 1e10 for c in cycles]  # Cap infinity for plotting
        
        ax2.loglog(stress_range, cycles, 'b-', linewidth=2, label='S-N Curve (ABS E)')
        
        # Plot actual stress ranges with cycle counts
        if len(plot_df) > 0:
            ax2.scatter(plot_df['stress_range_corrected'], plot_df['cycles_annual'],
                       color='red', s=50, alpha=0.6, label='Annual Cycles')
        
        # Mark fatigue limit
        fatigue_limit = self.config['sn_curve']['parameters']['fatigue_limit_stress']
        ax2.axvline(x=fatigue_limit, color='green', linestyle='--', alpha=0.5,
                   label=f'Fatigue Limit ({fatigue_limit:.1f} MPa)')
        
        ax2.set_xlabel('Stress Range [MPa]')
        ax2.set_ylabel('Cycles')
        ax2.set_title(f'S-N Curve - {location_id}')
        ax2.grid(True, alpha=0.3, which='both')
        ax2.legend()
        
        # Overall title
        title = self.config['visualization']['damage_plot']['styling']['title'].format(
            config=config, fc_number=int(fc_num), strut_number=strut_num, location_id=location_id
        )
        fig.suptitle(title, fontsize=14, fontweight='bold')
        
        # Save plot
        output_base = Path(self.config['output']['folders']['base'])
        output_plots = output_base / self.config['output']['folders']['plots']
        output_plots.mkdir(parents=True, exist_ok=True)
        
        plot_file = output_plots / f"{output_name}.png"
        plt.tight_layout()
        plt.savefig(plot_file, dpi=self.config['visualization']['damage_plot']['dpi'])
        plt.close()
        
        self.logger.info(f"  Plot saved: {plot_file}")
    
    def get_input_files(self) -> List[Path]:
        """Get list of input files to process based on configuration"""
        base_folder = Path(self.config['input_data']['stress_rainflow']['data_folder'])
        
        if self.config['input_data']['processing']['file_selection']['all_files']:
            # Process all matching files
            pattern = self.config['input_data']['stress_rainflow']['filter_pattern']
            files = list(base_folder.glob(pattern))
        else:
            # Process specific files based on filters
            files = []
            selection = self.config['input_data']['processing']['file_selection']
            
            for file in base_folder.glob('*_stress_rainflow.csv'):
                filename = file.stem  # removes .csv, keeping _stress_rainflow
                parts = filename.split('_')
                
                # Extract components
                # Location is third from end (before 'stress' and 'rainflow')
                location_id = parts[-3] if len(parts) >= 3 else None
                strut_part = None
                fc_part = None
                config_name = None
                
                for i, part in enumerate(parts):
                    if part.startswith('fc') or part.startswith('FC'):
                        fc_part = part
                        fc_num = int(part[2:])
                        config_name = '_'.join(parts[:i])
                    if part.startswith('Strut'):
                        strut_part = part
                        strut_num = int(part[5:])
                
                # Check if file matches selection criteria
                match = True
                
                if 'location_ids' in selection and selection['location_ids']:
                    if location_id not in selection['location_ids']:
                        match = False
                
                if 'configs' in selection and selection['configs']:
                    if config_name not in selection['configs']:
                        match = False
                
                if 'fc_numbers' in selection and selection['fc_numbers']:
                    if fc_num not in selection['fc_numbers']:
                        match = False
                
                if 'strut_numbers' in selection and selection['strut_numbers']:
                    if strut_num not in selection['strut_numbers']:
                        match = False
                
                if match:
                    files.append(file)
        
        return sorted(files)
    
    def run(self):
        """Main execution method"""
        self.logger.info("="*80)
        self.logger.info(f"Starting Damage Analysis - {self.config['project']['name']}")
        self.logger.info("="*80)
        
        # Get input files
        input_files = self.get_input_files()
        self.logger.info(f"Found {len(input_files)} files to process")
        
        if len(input_files) == 0:
            self.logger.error("No input files found!")
            return
        
        # Process files
        summaries = []
        
        if self.config['input_data']['processing']['parallel_processing']:
            # Parallel processing
            max_workers = self.config['input_data']['processing']['max_workers']
            self.logger.info(f"Processing in parallel with {max_workers} workers")
            
            with ProcessPoolExecutor(max_workers=max_workers) as executor:
                futures = {executor.submit(self.process_single_file, f): f for f in input_files}
                
                with tqdm(total=len(input_files), desc="Processing files") as pbar:
                    for future in as_completed(futures):
                        try:
                            summary = future.result()
                            summaries.append(summary)
                            pbar.update(1)
                        except Exception as e:
                            file = futures[future]
                            self.logger.error(f"Error processing {file}: {e}")
                            if not self.config['input_data']['processing']['continue_on_error']:
                                raise
                            pbar.update(1)
        else:
            # Sequential processing
            for file in tqdm(input_files, desc="Processing files"):
                try:
                    summary = self.process_single_file(file)
                    summaries.append(summary)
                except Exception as e:
                    self.logger.error(f"Error processing {file}: {e}")
                    if not self.config['input_data']['processing']['continue_on_error']:
                        raise
        
        # Generate summary report
        if self.config['output']['formats']['summary_report']['enabled']:
            self.generate_summary_report(summaries)
        
        self.logger.info("="*80)
        self.logger.info("Analysis Complete!")
        self.logger.info(f"Processed {len(summaries)} files successfully")
        self.logger.info("="*80)
    
    def generate_summary_report(self, summaries: List[Dict]):
        """Generate summary CSV of all processed files"""
        df = pd.DataFrame(summaries)
        
        # Sort by damage rate (highest first)
        df = df.sort_values('total_damage_rate_per_year', ascending=False)
        
        # Save summary
        output_base = Path(self.config['output']['folders']['base'])
        output_reports = output_base / self.config['output']['folders']['reports']
        output_reports.mkdir(parents=True, exist_ok=True)
        
        summary_file = output_reports / self.config['output']['formats']['summary_report']['filename']
        df.to_csv(summary_file, index=False)
        
        self.logger.info(f"Summary report saved: {summary_file}")
        
        # Print top 10 highest damage locations
        self.logger.info("\nTop 10 Highest Damage Locations:")
        self.logger.info("-" * 80)
        for i, row in df.head(10).iterrows():
            self.logger.info(
                f"{row['filename']}: "
                f"Damage={row['total_damage_rate_per_year']:.3e}/year, "
                f"Life={row['fatigue_life_years']:.1f} years"
            )


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description='Stress Rainflow to Damage Analysis')
    parser.add_argument('--config', type=str, required=True,
                       help='Path to configuration YAML file')
    
    args = parser.parse_args()
    
    # Check config file exists
    if not os.path.exists(args.config):
        print(f"Error: Configuration file not found: {args.config}")
        sys.exit(1)
    
    # Run analysis
    analyzer = DamageAnalyzer(args.config)
    analyzer.run()


if __name__ == "__main__":
    main()