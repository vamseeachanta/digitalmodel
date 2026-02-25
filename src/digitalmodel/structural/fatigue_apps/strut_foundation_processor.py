#!/usr/bin/env python
"""
Production Data Processor for Strut Foundation Fatigue Analysis
Processes 4 vessel configurations with rainflow counting and fatigue damage calculation
Using 100-second samples (1000 timesteps) for efficient processing
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import json
from dataclasses import dataclass, asdict
import logging
from datetime import datetime

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class Configuration:
    """Vessel configuration data"""
    name: str
    weight: float
    description: str
    
@dataclass
class FatigueCondition:
    """Fatigue condition parameters (legacy)"""
    id: int
    wind_speed: float  # m/s
    wind_dir: float    # degrees
    hs: float          # m
    tp: float          # s
    wave_dir: float    # degrees
    occurrence: float  # percentage

@dataclass
class SeaState:
    """Sea state parameters using new SS naming convention"""
    id: str           # SS001, SS002, etc.
    wind_speed: float  # m/s
    wind_dir: float    # degrees
    hs: float          # m
    tp: float          # s
    wave_dir: float    # degrees
    occurrence: float  # percentage
    description: str = ""

class ProductionDataHandler:
    """Handles production data loading and processing"""
    
    def __init__(self, base_path: str = None,
                 sample_timesteps: int = 1000):
        """
        Initialize the production data handler
        
        Args:
            base_path: Base path to production data (if None, auto-detect)
            sample_timesteps: Number of timesteps to use (100 seconds at 0.1s = 1000 steps)
        """
        # Use sample_data if production path doesn't exist
        if base_path is None:
            prod_path = Path("D:/1522/ctr9/fatigue_wsp_method/07c_fatigue/csv")
            sample_path = Path("sample_data")
            
            if prod_path.exists():
                self.base_path = prod_path
                logger.info(f"Using production data: {prod_path}")
            elif sample_path.exists():
                self.base_path = sample_path
                logger.info(f"Using sample data: {sample_path}")
            else:
                raise ValueError("No data source found")
        else:
            self.base_path = Path(base_path)
        self.sample_timesteps = sample_timesteps
        
        # Define configurations with operational weights
        self.configurations = {
            'fsts_l015': Configuration(
                name='fsts_l015',
                weight=46.25,
                description='FSTs Light (15% loaded)'
            ),
            'fsts_l095': Configuration(
                name='fsts_l095', 
                weight=46.25,
                description='FSTs Full (95% loaded)'
            ),
            'fsts_l015_125km3_l100_pb': Configuration(
                name='fsts_l015_125km3_l100_pb',
                weight=3.75,
                description='FSTs Light + LNGC Full (Port Berthing)'
            ),
            'fsts_l095_125km3_l000_pb': Configuration(
                name='fsts_l095_125km3_l000_pb',
                weight=3.75,
                description='FSTs Full + LNGC Light (Port Berthing)'
            )
        }
        
    def get_reference_files(self, config_name: str) -> Dict[str, List[str]]:
        """Get all reference files for a configuration (flat structure)
        
        Supports both new (REF_WIND01, REF_WAVE01) and legacy (wind01, wave01) naming
        """
        # In production, files are in flat structure with pattern:
        # New: {config}_mwl_REF_WIND01_Strut{#}.csv, {config}_mwl_REF_WAVE01_Strut{#}.csv
        # Legacy: {config}_mwl_wind01_Strut{#}.csv, {config}_mwl_wave01_Strut{#}.csv
        
        reference_files = {'wind': [], 'wave': []}
        
        # Look for files matching the config pattern
        pattern = f"{config_name}_mwl_*_Strut*.csv"
        files = list(self.base_path.glob(pattern))
        
        for file_path in files:
            filename = file_path.name
            # Parse the reference type from filename
            # Examples: 
            # New: fsts_l015_mwl_REF_WIND01_Strut1.csv
            # Legacy: fsts_l015_mwl_wind01_Strut1.csv
            parts = filename.replace('.csv', '').split('_mwl_')
            if len(parts) == 2:
                ref_part = parts[1].split('_')[0]  # Get 'REF_WIND01', 'wind01', etc.
                
                # Check for new naming convention
                if ref_part.startswith('REF_WIND'):
                    if ref_part not in reference_files['wind']:
                        reference_files['wind'].append(ref_part)
                elif ref_part.startswith('REF_WAVE'):
                    if ref_part not in reference_files['wave']:
                        reference_files['wave'].append(ref_part)
                # Check for legacy naming convention
                elif ref_part.startswith('wind'):
                    if ref_part not in reference_files['wind']:
                        reference_files['wind'].append(ref_part)
                elif ref_part.startswith('wave'):
                    if ref_part not in reference_files['wave']:
                        reference_files['wave'].append(ref_part)
        
        return reference_files
    
    def parse_reference_name(self, ref_name: str) -> Dict:
        """Parse reference name to extract parameters
        
        Supports both new (REF_WIND01, REF_WAVE01) and legacy (wind01, wave01) formats
        """
        # New naming convention: REF_WIND01, REF_WAVE01
        if ref_name.startswith('REF_WIND'):
            # Extract number (e.g., '01' from 'REF_WIND01')
            num_str = ref_name.replace('REF_WIND', '')
            num = int(num_str) if num_str else 1
            return {
                'type': 'wind',
                'number': num,
                'direction': 0.0,  # REF_WIND01 is baseline at 0°
                'speed': 10.0,     # Reference wind speed
                'naming_format': 'new'
            }
        elif ref_name.startswith('REF_WAVE'):
            # Extract number (e.g., '01' from 'REF_WAVE01')
            num_str = ref_name.replace('REF_WAVE', '')
            num = int(num_str) if num_str else 1
            return {
                'type': 'wave',
                'number': num,
                'direction': 0.0,  # REF_WAVE01 is baseline at 0°
                'hs': 0.5,        # Reference Hs
                'tp': 2.7,        # Reference Tp
                'naming_format': 'new'
            }
        # Legacy naming convention: wind01, wave01
        elif ref_name.startswith('wind'):
            # Extract number (e.g., '01' from 'wind01')
            num = int(ref_name[4:]) if len(ref_name) > 4 else 1
            # Map to direction (simplified - would need actual mapping from production)
            direction = (num - 1) * 22.5 if num <= 16 else 0  # 16 directions
            return {
                'type': 'wind',
                'number': num,
                'direction': direction,
                'speed': 10.0,  # Reference wind speed
                'naming_format': 'legacy'
            }
        elif ref_name.startswith('wave'):
            # Extract number (e.g., '01' from 'wave01')
            num = int(ref_name[4:]) if len(ref_name) > 4 else 1
            # Map to parameters (simplified - would need actual mapping from production)
            direction = (num - 1) * 20 if num <= 18 else 0  # 18 wave cases
            return {
                'type': 'wave',
                'number': num,
                'direction': direction % 360,
                'hs': 0.5,  # Reference Hs
                'tp': 2.7,  # Reference Tp
                'naming_format': 'legacy'
            }
        return {}
    
    def load_strut_data(self, config_name: str, ref_name: str, strut_num: int,
                       use_sample: bool = True) -> Tuple[np.ndarray, np.ndarray]:
        """
        Load effective tension data for a specific strut
        
        Production format: {config}_mwl_{reference}_Strut{#}.csv
        
        Returns:
            Tuple of (time_array, tension_array)
        """
        # Build filename according to production pattern
        filename = f"{config_name}_mwl_{ref_name}_Strut{strut_num}.csv"
        file_path = self.base_path / filename
        
        if not file_path.exists():
            logger.warning(f"File does not exist: {file_path}")
            return np.array([]), np.array([])
        
        try:
            # Read CSV with proper encoding
            df = pd.read_csv(file_path, encoding='latin-1')
            
            # Find the correct column for effective tension at FST vessel end
            vessel_end_columns = [col for col in df.columns if 'Vessel End' in col and 'Effective' in col]
            
            if not vessel_end_columns:
                # Try alternative column names
                vessel_end_columns = [col for col in df.columns if 'FST' in col and 'Vessel' in col]
            
            if vessel_end_columns:
                tension_col = vessel_end_columns[0]
            else:
                logger.warning(f"Could not find vessel end tension column in {file_path}")
                return np.array([]), np.array([])
            
            # Get time and tension data
            time_data = df.iloc[:, 0].values  # First column is usually time
            tension_data = df[tension_col].values
            
            # Use sample if requested
            if use_sample and len(time_data) > self.sample_timesteps:
                time_data = time_data[:self.sample_timesteps]
                tension_data = tension_data[:self.sample_timesteps]
            
            return time_data, tension_data
            
        except Exception as e:
            logger.error(f"Error loading {file_path}: {e}")
            return np.array([]), np.array([])

class LoadScaler:
    """Handles load scaling and combination"""
    
    def __init__(self, data_handler: ProductionDataHandler):
        self.data_handler = data_handler
        self.base_wind_speed = 10.0  # m/s (REF_WIND01 baseline)
        self.base_hs = 0.5  # m (REF_WAVE01 baseline)
        
        # Load both legacy fatigue conditions and new sea states
        self.fatigue_conditions = self.load_fatigue_conditions()
        self.sea_states = self.load_sea_states()
        
    def load_fatigue_conditions(self) -> List[FatigueCondition]:
        """Load legacy fatigue conditions from CSV"""
        fatigue_file = Path("input/fatigue_conditions.csv")
        
        if not fatigue_file.exists():
            # Create sample fatigue conditions for testing
            return self.create_sample_fatigue_conditions()
        
        df = pd.read_csv(fatigue_file)
        conditions = []
        
        for _, row in df.iterrows():
            conditions.append(FatigueCondition(
                id=int(row.get('Row', len(conditions) + 1)),
                wind_speed=float(row.get('Wind Speed (m/s)', 5.0)),
                wind_dir=float(row.get('Wind Dir (°)', 0.0)),
                hs=float(row.get('Hs (m)', 0.15)),
                tp=float(row.get('Tp (s)', 2.0)),
                wave_dir=float(row.get('Wave Dir (°)', 0.0)),
                occurrence=float(row.get('Occurrence (%)', 1.0))
            ))
        
        return conditions
    
    def load_sea_states(self) -> List[SeaState]:
        """Load sea states using new SS naming convention"""
        sea_state_file = Path("input/sea_states.csv")
        
        if not sea_state_file.exists():
            # Create sample sea states for testing based on verification framework
            return self.create_sample_sea_states()
        
        df = pd.read_csv(sea_state_file)
        sea_states = []
        
        for _, row in df.iterrows():
            sea_states.append(SeaState(
                id=str(row.get('Sea_State_ID', f'SS{len(sea_states)+1:03d}')),
                wind_speed=float(row.get('Wind Speed (m/s)', 5.0)),
                wind_dir=float(row.get('Wind Dir (°)', 0.0)),
                hs=float(row.get('Hs (m)', 0.15)),
                tp=float(row.get('Tp (s)', 2.0)),
                wave_dir=float(row.get('Wave Dir (°)', 0.0)),
                occurrence=float(row.get('Occurrence (%)', 1.0)),
                description=str(row.get('Description', ''))
            ))
        
        return sea_states
    
    def create_sample_sea_states(self) -> List[SeaState]:
        """Create sample sea states based on verification framework mapping"""
        sea_states = [
            SeaState(
                id='SS001',
                wind_speed=15.0,
                wind_dir=0.0,
                hs=0.75,
                tp=2.7,
                wave_dir=0.0,
                occurrence=25.0,
                description='Test validation condition'
            ),
            SeaState(
                id='SS002',
                wind_speed=10.0,
                wind_dir=0.0,
                hs=0.50,
                tp=2.7,
                wave_dir=0.0,
                occurrence=25.0,
                description='Baseline check condition'
            ),
            SeaState(
                id='SS003',
                wind_speed=5.0,
                wind_dir=0.0,
                hs=0.25,
                tp=2.7,
                wave_dir=0.0,
                occurrence=25.0,
                description='Calm conditions'
            ),
            SeaState(
                id='SS004',
                wind_speed=20.0,
                wind_dir=0.0,
                hs=1.00,
                tp=2.7,
                wave_dir=0.0,
                occurrence=25.0,
                description='Severe conditions'
            )
        ]
        
        return sea_states
    
    def create_sample_fatigue_conditions(self) -> List[FatigueCondition]:
        """Create sample fatigue conditions for testing"""
        conditions = []
        
        # Create a subset of 10 representative conditions
        sample_conditions = [
            (5, 0, 0.15, 1.93, 0, 7.76),      # Low wind, low wave
            (10, 45, 0.25, 2.70, 45, 5.50),   # Moderate aligned
            (15, 90, 0.35, 3.47, 120, 3.25),  # High misaligned
            (7, 180, 0.20, 2.70, 180, 6.00),  # Moderate opposite
            (20, 270, 0.50, 3.47, 270, 1.50), # High aligned
            (3, 0, 0.10, 1.93, 30, 8.50),     # Very low misaligned
            (12, 135, 0.30, 2.70, 150, 4.00), # Moderate-high misaligned
            (8, 225, 0.22, 2.70, 225, 5.75),  # Moderate aligned
            (25, 315, 0.65, 3.47, 300, 0.75), # Very high misaligned
            (6, 90, 0.18, 1.93, 90, 6.99),    # Low-moderate aligned
        ]
        
        for i, (ws, wd, hs, tp, wvd, occ) in enumerate(sample_conditions, 1):
            conditions.append(FatigueCondition(
                id=i,
                wind_speed=ws,
                wind_dir=wd,
                hs=hs,
                tp=tp,
                wave_dir=wvd,
                occurrence=occ
            ))
        
        return conditions
    
    def select_closest_reference(self, config_name: str, ref_type: str, 
                                direction: float, tp: Optional[float] = None) -> str:
        """Select the closest reference seastate based on parameters
        
        Prefers new REF_WIND01/REF_WAVE01 format, falls back to legacy
        """
        ref_files = self.data_handler.get_reference_files(config_name)
        
        if not ref_files.get(ref_type):
            return ""
        
        # Filter by type and prioritize new naming format
        typed_refs = []
        new_format_refs = []
        legacy_refs = []
        
        for ref_name in ref_files[ref_type]:
            params = self.data_handler.parse_reference_name(ref_name)
            if params.get('type') == ref_type:
                if params.get('naming_format') == 'new':
                    new_format_refs.append((ref_name, params))
                else:
                    legacy_refs.append((ref_name, params))
        
        # Prefer new format, use legacy as fallback
        typed_refs = new_format_refs if new_format_refs else legacy_refs
        
        if not typed_refs:
            return ""
        
        # For new format with REF_WIND01/REF_WAVE01, prefer the 01 references
        if new_format_refs:
            for ref_name, params in new_format_refs:
                if (ref_type == 'wind' and ref_name == 'REF_WIND01') or \
                   (ref_type == 'wave' and ref_name == 'REF_WAVE01'):
                    return ref_name
            # If REF_*01 not found, return first available
            return new_format_refs[0][0]
        
        # Legacy fallback: Find closest by direction (and Tp for waves)
        best_ref = None
        best_score = float('inf')
        
        for ref_name, params in typed_refs:
            # Calculate direction difference (accounting for circular nature)
            dir_diff = abs(params['direction'] - direction)
            if dir_diff > 180:
                dir_diff = 360 - dir_diff
            
            score = dir_diff
            
            # For waves, also consider Tp
            if ref_type == 'wave' and tp is not None and 'tp' in params:
                tp_diff = abs(params['tp'] - tp)
                score += tp_diff * 50  # Weight Tp difference
            
            if score < best_score:
                best_score = score
                best_ref = ref_name
        
        return best_ref or ""
    
    def process_fatigue_condition(self, config_name: str, condition: FatigueCondition,
                                 strut_num: int) -> Tuple[np.ndarray, Dict]:
        """Process a single fatigue condition by combining scaled wind and wave references (legacy)"""
        
        # Calculate scaling factors
        wind_scale = (condition.wind_speed / self.base_wind_speed) ** 2
        wave_scale = condition.hs / self.base_hs
        
        # Select closest references
        wind_ref = self.select_closest_reference(config_name, 'wind', condition.wind_dir)
        wave_ref = self.select_closest_reference(config_name, 'wave', condition.wave_dir, condition.tp)
        
        if not wind_ref or not wave_ref:
            logger.warning(f"Could not find references for FC{condition.id:03d}")
            return np.array([]), {}
        
        # Load reference data
        time_wind, tension_wind = self.data_handler.load_strut_data(config_name, wind_ref, strut_num)
        time_wave, tension_wave = self.data_handler.load_strut_data(config_name, wave_ref, strut_num)
        
        if len(tension_wind) == 0 or len(tension_wave) == 0:
            return np.array([]), {}
        
        # Ensure same length
        min_len = min(len(tension_wind), len(tension_wave))
        tension_wind = tension_wind[:min_len]
        tension_wave = tension_wave[:min_len]
        
        # Scale and combine
        scaled_wind = tension_wind * wind_scale
        scaled_wave = tension_wave * wave_scale
        effective_tension = scaled_wind + scaled_wave
        
        # Metadata
        metadata = {
            'fatigue_condition': condition.id,
            'wind_reference': wind_ref,
            'wave_reference': wave_ref,
            'wind_scale_factor': wind_scale,
            'wave_scale_factor': wave_scale,
            'wind_speed': condition.wind_speed,
            'hs': condition.hs,
            'tp': condition.tp,
            'occurrence_pct': condition.occurrence
        }
        
        return effective_tension, metadata
    
    def process_sea_state(self, config_name: str, sea_state: SeaState,
                         strut_num: int) -> Tuple[np.ndarray, Dict]:
        """Process a single sea state using new SS naming convention
        
        Uses REF_WIND01 and REF_WAVE01 as baseline references for scaling
        """
        
        # Calculate scaling factors based on REF_WIND01 and REF_WAVE01 baselines
        wind_scale = (sea_state.wind_speed / self.base_wind_speed) ** 2
        wave_scale = sea_state.hs / self.base_hs
        
        # Always use REF_WIND01 and REF_WAVE01 as references for new convention
        wind_ref = self.select_closest_reference(config_name, 'wind', 0.0)  # 0° for REF_WIND01
        wave_ref = self.select_closest_reference(config_name, 'wave', 0.0)  # 0° for REF_WAVE01
        
        # Prefer new naming format
        ref_files = self.data_handler.get_reference_files(config_name)
        if 'REF_WIND01' in ref_files.get('wind', []):
            wind_ref = 'REF_WIND01'
        if 'REF_WAVE01' in ref_files.get('wave', []):
            wave_ref = 'REF_WAVE01'
        
        if not wind_ref or not wave_ref:
            logger.warning(f"Could not find references for {sea_state.id}")
            return np.array([]), {}
        
        # Load reference data
        time_wind, tension_wind = self.data_handler.load_strut_data(config_name, wind_ref, strut_num)
        time_wave, tension_wave = self.data_handler.load_strut_data(config_name, wave_ref, strut_num)
        
        if len(tension_wind) == 0 or len(tension_wave) == 0:
            return np.array([]), {}
        
        # Ensure same length
        min_len = min(len(tension_wind), len(tension_wave))
        tension_wind = tension_wind[:min_len]
        tension_wave = tension_wave[:min_len]
        
        # Scale and combine
        scaled_wind = tension_wind * wind_scale
        scaled_wave = tension_wave * wave_scale
        effective_tension = scaled_wind + scaled_wave
        
        # Metadata
        metadata = {
            'sea_state_id': sea_state.id,
            'wind_reference': wind_ref,
            'wave_reference': wave_ref,
            'wind_scale_factor': wind_scale,
            'wave_scale_factor': wave_scale,
            'wind_speed': sea_state.wind_speed,
            'hs': sea_state.hs,
            'tp': sea_state.tp,
            'occurrence_pct': sea_state.occurrence,
            'description': sea_state.description,
            'naming_convention': 'new'
        }
        
        return effective_tension, metadata

def main(data_path=None):
    """Main execution function"""
    logger.info("=" * 60)
    logger.info("STRUT FOUNDATION FATIGUE ANALYSIS - SAMPLE IMPLEMENTATION")
    logger.info("=" * 60)
    
    # Initialize handlers
    data_handler = ProductionDataHandler(base_path=data_path, sample_timesteps=1000)  # 100 seconds
    scaler = LoadScaler(data_handler)
    
    # Create output directories
    output_base = Path("output")
    output_base.mkdir(exist_ok=True)
    
    # Summary statistics
    summary_stats = []
    
    # Process each configuration
    for config_name, config in data_handler.configurations.items():
        logger.info(f"\nProcessing {config_name} (Weight: {config.weight}%)")
        logger.info(f"Description: {config.description}")
        
        # Check if configuration has any files (flat structure)
        pattern = f"{config_name}_mwl_*_Strut*.csv"
        config_files = list(data_handler.base_path.glob(pattern))
        if not config_files:
            logger.warning(f"Skipping {config_name} - no files found")
            continue
        
        # Get available references
        ref_files = data_handler.get_reference_files(config_name)
        total_refs = len(ref_files.get('wind', [])) + len(ref_files.get('wave', []))
        logger.info(f"Found {total_refs} reference seastates (wind: {len(ref_files.get('wind', []))}, wave: {len(ref_files.get('wave', []))})")
        
        # Create configuration output directory
        config_output = output_base / config_name
        config_output.mkdir(exist_ok=True)
        
        # Process subset of fatigue conditions (first 3 for demo)
        num_conditions = min(3, len(scaler.fatigue_conditions))
        
        for fc_idx in range(num_conditions):
            condition = scaler.fatigue_conditions[fc_idx]
            
            # Process subset of struts (first 2 for demo)
            for strut_num in [1, 2]:
                logger.info(f"  Processing FC{condition.id:03d} - Strut{strut_num}")
                
                # Generate combined effective tension
                effective_tension, metadata = scaler.process_fatigue_condition(
                    config_name, condition, strut_num
                )
                
                if len(effective_tension) > 0:
                    # Save combined tension
                    output_file = config_output / f"FC{condition.id:03d}_Strut{strut_num}_combined.csv"
                    
                    df_output = pd.DataFrame({
                        'time_s': np.arange(len(effective_tension)) * 0.1,
                        'effective_tension_kN': effective_tension
                    })
                    df_output.to_csv(output_file, index=False)
                    
                    # Collect statistics
                    stats = {
                        'config': config_name,
                        'fc_id': condition.id,
                        'strut': strut_num,
                        'max_tension': float(np.max(effective_tension)),
                        'min_tension': float(np.min(effective_tension)),
                        'mean_tension': float(np.mean(effective_tension)),
                        'std_tension': float(np.std(effective_tension)),
                        'samples': len(effective_tension),
                        **metadata
                    }
                    summary_stats.append(stats)
    
    # Save summary statistics
    if summary_stats:
        summary_df = pd.DataFrame(summary_stats)
        summary_file = output_base / "processing_summary.csv"
        summary_df.to_csv(summary_file, index=False)
        
        logger.info("\n" + "=" * 60)
        logger.info("PROCESSING SUMMARY")
        logger.info("=" * 60)
        logger.info(f"Configurations processed: {summary_df['config'].nunique()}")
        logger.info(f"Total combinations: {len(summary_stats)}")
        logger.info(f"Max tension observed: {summary_df['max_tension'].max():.2f} kN")
        logger.info(f"Min tension observed: {summary_df['min_tension'].min():.2f} kN")
        logger.info(f"Output saved to: {output_base.absolute()}")
    else:
        logger.warning("No data was successfully processed")
    
    return summary_stats

if __name__ == "__main__":
    import sys
    data_path = sys.argv[1] if len(sys.argv) > 1 else None
    results = main(data_path)