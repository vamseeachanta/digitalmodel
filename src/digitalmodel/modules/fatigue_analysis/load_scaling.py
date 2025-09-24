"""
Load Scaling Module for Fatigue Analysis
=========================================
Main module for performing load scaling based on reference seastates.

This module implements the core load scaling methodology:
1. Reads reference seastate data and fatigue seastate definitions
2. Selects appropriate reference conditions for each fatigue seastate
3. Applies wind and wave scaling factors
4. Generates scaled tension time series
5. Outputs results in specified format
"""

import os
import logging
import yaml
import pandas as pd
import numpy as np
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
from pathlib import Path
import warnings

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


@dataclass
class ScalingFactors:
    """Container for wind and wave scaling factors"""
    wind_factor: float
    wave_factor: float
    wind_reference: str
    wave_reference: str
    wind_match_quality: str
    wave_match_quality: str


@dataclass
class LoadCase:
    """Container for load case data"""
    config: str
    fatigue_condition: str
    strut: int
    scaled_tension: np.ndarray
    scaling_factors: ScalingFactors
    time_vector: np.ndarray


class LoadScalingProcessor:
    """Main processor for load scaling calculations"""
    
    def __init__(self, config_file: str):
        """
        Initialize the load scaling processor
        
        Args:
            config_file: Path to YAML configuration file
        """
        self.config_file = config_file
        self.config = self._load_config()
        self.reference_metadata = None
        self.fatigue_seastates = None
        self.reference_data = {}
        self.results = []
        
    def _load_config(self) -> Dict[str, Any]:
        """Load and validate configuration from YAML file"""
        try:
            with open(self.config_file, 'r') as f:
                config = yaml.safe_load(f)
            logger.info(f"Configuration loaded from {self.config_file}")
            return config
        except Exception as e:
            logger.error(f"Failed to load configuration: {e}")
            raise
            
    def load_input_data(self):
        """Load all input data files"""
        logger.info("Loading input data...")
        
        # Load reference seastate metadata
        ref_meta_file = self.config['input_data']['reference_seastate']['metadata_file']
        self.reference_metadata = pd.read_csv(ref_meta_file)
        logger.info(f"Loaded {len(self.reference_metadata)} reference seastate definitions")
        
        # Load fatigue seastates
        fatigue_file = self.config['input_data']['fatigue_seastates']['metadata_file']
        self.fatigue_seastates = pd.read_csv(fatigue_file)
        logger.info(f"Loaded {len(self.fatigue_seastates)} fatigue seastates")
        
        # Validate occurrence sums to 100%
        total_occurrence = self.fatigue_seastates['Occurrence (%)'].sum()
        if abs(total_occurrence - 100.0) > 0.1:
            logger.warning(f"Fatigue seastate occurrences sum to {total_occurrence}%, expected 100%")
            
    def load_reference_data(self, config: str, env_ref: str, strut: int) -> np.ndarray:
        """
        Load reference time series data
        
        Args:
            config: Vessel configuration (e.g., 'fsts_l015')
            env_ref: Environment reference (e.g., 'wave01', 'wind01')
            strut: Strut number
            
        Returns:
            Time series array of effective tensions
        """
        # Build file path
        data_folder = self.config['input_data']['reference_seastate']['data_folder']
        
        # Handle different naming patterns based on config
        if '125km3' in config:
            # Extended configuration name
            file_name = f"{config}_mwl_{env_ref}_Strut{strut}.csv"
        else:
            # Standard configuration name
            file_name = f"{config}_mwl_{env_ref}_Strut{strut}.csv"
            
        file_path = os.path.join(data_folder, file_name)
        
        # Check cache first
        cache_key = f"{config}_{env_ref}_Strut{strut}"
        if cache_key in self.reference_data:
            return self.reference_data[cache_key]
            
        try:
            # Load CSV file
            df = pd.read_csv(file_path)
            tension_column = self.config['input_data']['reference_seastate']['tension_column']
            
            if tension_column not in df.columns:
                # Try alternative column names
                alt_columns = ['Effective Tension at Vessel End', 'Effective Tension (kN)', 
                              'Tension (kN)', 'EffectiveTension', 'Effective Tension']
                for col in alt_columns:
                    if col in df.columns:
                        tension_column = col
                        break
                else:
                    available_cols = ', '.join(df.columns.tolist())
                    raise ValueError(f"Tension column '{tension_column}' not found in {file_path}. Available columns: {available_cols}")
                    
            tensions = df[tension_column].values
            self.reference_data[cache_key] = tensions
            return tensions
            
        except Exception as e:
            logger.error(f"Failed to load reference data from {file_path}: {e}")
            raise
            
    def select_reference_conditions(self, fatigue_state: pd.Series) -> Tuple[str, str]:
        """
        Select best matching reference conditions for a fatigue seastate
        
        Args:
            fatigue_state: Row from fatigue seastates dataframe
            
        Returns:
            Tuple of (wind_reference, wave_reference)
        """
        # Extract fatigue parameters
        wind_dir = fatigue_state.get('Wind Dir (deg)', fatigue_state.get('Wind Dir (°)', None))
        if wind_dir is None:
            raise KeyError(f"Wind direction column not found in fatigue state: {fatigue_state.keys()}")
        wind_speed = fatigue_state['Wind Speed (m/s)']
        wave_dir = fatigue_state.get('Wave Dir (deg)', fatigue_state.get('Wave Dir (°)', None))
        if wave_dir is None:
            raise KeyError(f"Wave direction column not found in fatigue state: {fatigue_state.keys()}")
        wave_hs = fatigue_state['Hs (m)']
        wave_tp = fatigue_state['Tp (s)']
        
        # Filter reference metadata for wind and wave conditions
        wind_refs = self.reference_metadata[
            self.reference_metadata['env reference'].str.contains('wind', na=False)
        ].copy()
        
        wave_refs = self.reference_metadata[
            self.reference_metadata['env reference'].str.contains('wave', na=False)
        ].copy()
        
        # Select closest wind reference by direction
        wind_reference = self._select_closest_by_direction(
            wind_refs, wind_dir, 'Wind Dir [deg]'
        )
        
        # Select closest wave reference by direction and period
        wave_reference = self._select_closest_wave_reference(
            wave_refs, wave_dir, wave_tp
        )
        
        return wind_reference, wave_reference
        
    def _select_closest_by_direction(self, refs: pd.DataFrame, target_dir: float, 
                                     dir_column: str) -> str:
        """Select reference with closest direction match"""
        if refs.empty:
            return None
            
        # Calculate angular differences - handle non-numeric values
        refs = refs.copy()
        refs['dir_diff'] = refs[dir_column].apply(
            lambda x: min(abs(float(x) - target_dir), 360 - abs(float(x) - target_dir)) 
            if pd.notna(x) and str(x).replace('.','',1).replace('-','',1).isdigit() 
            else float('inf')
        )
        
        # Find minimum difference
        best_match = refs.loc[refs['dir_diff'].idxmin()]
        return best_match['env reference']
        
    def _select_closest_wave_reference(self, refs: pd.DataFrame, 
                                       target_dir: float, target_tp: float) -> str:
        """Select wave reference by direction and period"""
        if refs.empty:
            return None
            
        # Make a copy to avoid modifying original
        refs = refs.copy()
        
        # Calculate direction differences - handle non-numeric values
        refs['dir_diff'] = refs['Wave Dir [deg]'].apply(
            lambda x: min(abs(float(x) - target_dir), 360 - abs(float(x) - target_dir))
            if pd.notna(x) and str(x).replace('.','',1).replace('-','',1).isdigit()
            else float('inf')
        )
        
        # Calculate period differences - handle non-numeric values
        refs['tp_diff'] = refs['Tp [s]'].apply(
            lambda x: abs(float(x) - target_tp)
            if pd.notna(x) and str(x).replace('.','',1).replace('-','',1).isdigit()
            else float('inf')
        )
        
        # Combined score (weighted)
        dir_weight = 0.7
        tp_weight = 0.3
        refs['score'] = (refs['dir_diff'] / 180) * dir_weight + \
                       (refs['tp_diff'] / 10) * tp_weight
        
        # Find best match
        best_match = refs.loc[refs['score'].idxmin()]
        return best_match['env reference']
        
    def calculate_scaling_factors(self, fatigue_state: pd.Series, 
                                  wind_ref: str, wave_ref: str) -> ScalingFactors:
        """
        Calculate wind and wave scaling factors
        
        Args:
            fatigue_state: Fatigue seastate parameters
            wind_ref: Selected wind reference
            wave_ref: Selected wave reference
            
        Returns:
            ScalingFactors object with calculated factors
        """
        # Get reference values
        wind_ref_data = self.reference_metadata[
            self.reference_metadata['env reference'] == wind_ref
        ].iloc[0]
        
        wave_ref_data = self.reference_metadata[
            self.reference_metadata['env reference'] == wave_ref
        ].iloc[0]
        
        # Wind scaling: (V_fatigue / V_reference)^2
        v_fatigue = float(fatigue_state['Wind Speed (m/s)'])
        v_reference = float(wind_ref_data['Vw [m/s]']) if pd.notna(wind_ref_data['Vw [m/s]']) else 0
        wind_factor = (v_fatigue / v_reference) ** 2 if v_reference > 0 else 0
        
        # Wave scaling: Hs_fatigue / Hs_reference  
        hs_fatigue = float(fatigue_state['Hs (m)'])
        hs_reference = float(wave_ref_data['Hs [m]']) if pd.notna(wave_ref_data['Hs [m]']) else 0
        wave_factor = hs_fatigue / hs_reference if hs_reference > 0 else 0
        
        # Apply scaling limits
        validation = self.config['load_scaling']['validation']
        wind_factor = np.clip(wind_factor, 
                              validation['min_wind_scaling_factor'],
                              validation['max_wind_scaling_factor'])
        wave_factor = np.clip(wave_factor,
                              validation['min_wave_scaling_factor'],
                              validation['max_wave_scaling_factor'])
        
        # Determine match quality
        wind_match = "exact" if abs(v_fatigue - v_reference) < 1.0 else "approximate"
        wave_match = "exact" if abs(hs_fatigue - hs_reference) < 0.05 else "approximate"
        
        return ScalingFactors(
            wind_factor=wind_factor,
            wave_factor=wave_factor,
            wind_reference=wind_ref,
            wave_reference=wave_ref,
            wind_match_quality=wind_match,
            wave_match_quality=wave_match
        )
        
    def apply_scaling(self, config: str, fatigue_idx: int, strut: int) -> LoadCase:
        """
        Apply scaling to generate scaled tension time series
        
        Args:
            config: Vessel configuration
            fatigue_idx: Index of fatigue seastate
            strut: Strut number
            
        Returns:
            LoadCase object with scaled results
        """
        fatigue_state = self.fatigue_seastates.iloc[fatigue_idx]
        
        # Select reference conditions
        wind_ref, wave_ref = self.select_reference_conditions(fatigue_state)
        
        # Load reference time series
        wind_tensions = self.load_reference_data(config, wind_ref, strut)
        wave_tensions = self.load_reference_data(config, wave_ref, strut)
        
        # Calculate scaling factors
        scaling_factors = self.calculate_scaling_factors(
            fatigue_state, wind_ref, wave_ref
        )
        
        # Apply scaling: Effective_tension = scaled_wind + scaled_wave
        scaled_wind = wind_tensions * scaling_factors.wind_factor
        scaled_wave = wave_tensions * scaling_factors.wave_factor
        scaled_tension = scaled_wind + scaled_wave
        
        # Create time vector
        time_step = self.config.get('processing', {}).get('time_series', {}).get('time_step_seconds', 0.1)
        time_vector = np.arange(len(scaled_tension)) * time_step
        
        # Create load case result
        fc_number = fatigue_idx + 1
        return LoadCase(
            config=config,
            fatigue_condition=f"FC{fc_number:03d}",
            strut=strut,
            scaled_tension=scaled_tension,
            scaling_factors=scaling_factors,
            time_vector=time_vector
        )
        
    def process_all_cases(self):
        """Process all combinations of configurations, fatigue states, and struts"""
        configs = self.config['input_data']['vessel_configurations']['configs']
        struts = self.config['input_data']['vessel_configurations']['struts']
        
        total_cases = len(configs) * len(self.fatigue_seastates) * len(struts)
        processed = 0
        
        logger.info(f"Processing {total_cases} load cases...")
        
        for config_data in configs:
            config = config_data['id']
            logger.info(f"Processing configuration: {config}")
            
            for fatigue_idx in range(len(self.fatigue_seastates)):
                for strut in struts:
                    try:
                        # Process single load case
                        load_case = self.apply_scaling(config, fatigue_idx, strut)
                        self.results.append(load_case)
                        
                        processed += 1
                        if processed % 10 == 0:
                            logger.info(f"Processed {processed}/{total_cases} cases")
                            
                    except Exception as e:
                        logger.error(f"Failed to process {config}_FC{fatigue_idx+1:03d}_Strut{strut}: {e}")
                        if not self.config.get('execution', {}).get('error_handling', {}).get('continue_on_error', False):
                            raise
                            
        logger.info(f"Completed processing {len(self.results)} load cases")
        
    def save_outputs(self):
        """Save processed results to output files"""
        output_config = self.config['output']
        base_folder = output_config['base_folder']
        
        # Try to use efficient output format
        import sys
        module_dir = os.path.dirname(os.path.abspath(__file__))
        parent_dir = os.path.dirname(os.path.dirname(os.path.dirname(module_dir)))
        efficient_module_path = os.path.join(parent_dir, 'digitalmodel', 'modules', 'fatigue_analysis')
        if efficient_module_path not in sys.path:
            sys.path.insert(0, efficient_module_path)
        
        try:
            from load_scaling_efficient_v2 import EfficientOutputWriterV2
            use_efficient = True
        except ImportError:
            try:
                from load_scaling_efficient import EfficientOutputWriter
                use_efficient = True
            except ImportError:
                logger.warning("Efficient output module not found, using standard format")
                use_efficient = False
        
        if use_efficient:
            # Use efficient output format with metadata extraction
            logger.info(f"Saving outputs to {base_folder} using enhanced efficient format")
            try:
                writer = EfficientOutputWriterV2(base_folder)
            except NameError:
                writer = EfficientOutputWriter(base_folder)
            
            # Save all results using efficient format
            for load_case in self.results:
                writer.write_scaled_tension(
                    config_id=load_case.config,
                    fc_number=int(load_case.fatigue_condition[2:]),
                    strut_number=load_case.strut,
                    time_series=load_case.time_vector,
                    scaled_tension=load_case.scaled_tension,
                    wind_factor=load_case.scaling_factors.wind_factor,
                    wave_factor=load_case.scaling_factors.wave_factor,
                    wind_reference=load_case.scaling_factors.wind_reference,
                    wave_reference=load_case.scaling_factors.wave_reference
                )
            
            # Finalize and write metadata
            writer.finalize()
        else:
            # Fallback to standard format
            os.makedirs(base_folder, exist_ok=True)
            logger.info(f"Saving outputs to {base_folder}")
            
            for load_case in self.results:
                # Generate filename
                file_name = output_config['file_naming']['pattern'].format(
                    config=load_case.config,
                    fc_number=int(load_case.fatigue_condition[2:]),
                    strut_number=load_case.strut,
                    type='scaled_tension'
                )
                
                file_path = os.path.join(base_folder, file_name)
                
                # Prepare data for saving
                data = pd.DataFrame({
                    'Time (s)': load_case.time_vector,
                    'Scaled Tension (kN)': load_case.scaled_tension,
                    'Wind Factor': load_case.scaling_factors.wind_factor,
                    'Wave Factor': load_case.scaling_factors.wave_factor,
                    'Wind Reference': load_case.scaling_factors.wind_reference,
                    'Wave Reference': load_case.scaling_factors.wave_reference
                })
                
                # Save to CSV
                data.to_csv(file_path, index=False)
            
        # Save summary report
        if output_config.get('generate_summary', True):
            self._save_summary_report()
            
        logger.info("Output files saved successfully")
        
    def _save_summary_report(self):
        """Generate and save summary report"""
        output_folder = self.config['output']['base_folder']
        summary_file = os.path.join(output_folder, 'scaling_factors_applied.csv')
        
        summary_data = []
        for load_case in self.results:
            summary_data.append({
                'Configuration': load_case.config,
                'Fatigue Condition': load_case.fatigue_condition,
                'Strut': load_case.strut,
                'Wind Factor': load_case.scaling_factors.wind_factor,
                'Wave Factor': load_case.scaling_factors.wave_factor,
                'Wind Reference': load_case.scaling_factors.wind_reference,
                'Wave Reference': load_case.scaling_factors.wave_reference,
                'Wind Match': load_case.scaling_factors.wind_match_quality,
                'Wave Match': load_case.scaling_factors.wave_match_quality
            })
            
        summary_df = pd.DataFrame(summary_data)
        summary_df.to_csv(summary_file, index=False)
        logger.info(f"Summary report saved to {summary_file}")
        
    def run(self):
        """Execute the complete load scaling pipeline"""
        logger.info("Starting load scaling analysis...")
        
        try:
            # Load input data
            self.load_input_data()
            
            # Process all load cases
            self.process_all_cases()
            
            # Save outputs
            self.save_outputs()
            
            logger.info("Load scaling analysis completed successfully")
            
        except Exception as e:
            logger.error(f"Load scaling analysis failed: {e}")
            raise


def main():
    """
    Main entry point for load scaling with argument parsing
    """
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Load Scaling Analysis for Fatigue Assessment',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Run with configuration file
    python -m digitalmodel.modules.fatigue_analysis.load_scaling config.yml
    
    # Run with verbose output
    python load_scaling.py config.yml --verbose
    
    # Override output directory
    python load_scaling.py config.yml -o custom_output/
        """
    )
    
    parser.add_argument(
        'config_file',
        type=str,
        help='Path to YAML configuration file'
    )
    
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )
    
    parser.add_argument(
        '-o', '--output-dir',
        type=str,
        help='Override output directory from configuration'
    )
    
    args = parser.parse_args()
    
    # Set logging level
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    
    # Create processor
    processor = LoadScalingProcessor(args.config_file)
    
    # Override output directory if specified
    if args.output_dir:
        processor.config['output']['base_folder'] = args.output_dir
        logger.info(f"Output directory overridden to: {args.output_dir}")
    
    # Run processing
    processor.run()


if __name__ == "__main__":
    import sys
    main()
    sys.exit(0)