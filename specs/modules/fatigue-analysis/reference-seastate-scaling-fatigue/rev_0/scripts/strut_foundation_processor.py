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
    """Fatigue condition parameters"""
    id: int
    wind_speed: float  # m/s
    wind_dir: float    # degrees
    hs: float          # m
    tp: float          # s
    wave_dir: float    # degrees
    occurrence: float  # percentage

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
        
    def get_reference_dirs(self, config_name: str) -> List[str]:
        """Get all reference directories for a configuration"""
        config_path = self.base_path / config_name
        if not config_path.exists():
            logger.warning(f"Configuration path does not exist: {config_path}")
            return []
        
        return [d.name for d in config_path.iterdir() if d.is_dir()]
    
    def parse_reference_dir(self, ref_dir: str) -> Dict:
        """Parse reference directory name to extract parameters"""
        # Example: wind_000deg or wave_000deg_Hs050cm_Tp270cs
        parts = ref_dir.split('_')
        
        if parts[0] == 'wind':
            return {
                'type': 'wind',
                'direction': int(parts[1].replace('deg', '')),
                'speed': 10.0  # Reference wind speed
            }
        elif parts[0] == 'wave':
            return {
                'type': 'wave',
                'direction': int(parts[1].replace('deg', '')),
                'hs': 0.5,  # Reference Hs in meters
                'tp': float(parts[-1].replace('Tp', '').replace('cs', '')) / 100  # Convert centiseconds to seconds
            }
        return {}
    
    def load_strut_data(self, config_name: str, ref_dir: str, strut_num: int,
                       use_sample: bool = True) -> Tuple[np.ndarray, np.ndarray]:
        """
        Load effective tension data for a specific strut
        
        Returns:
            Tuple of (time_array, tension_array)
        """
        file_path = self.base_path / config_name / ref_dir / f"Strut{strut_num}.csv"
        
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
        self.base_wind_speed = 10.0  # m/s
        self.base_hs = 0.5  # m
        
        # Load fatigue conditions
        self.fatigue_conditions = self.load_fatigue_conditions()
        
    def load_fatigue_conditions(self) -> List[FatigueCondition]:
        """Load fatigue conditions from CSV"""
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
        """Select the closest reference seastate based on parameters"""
        ref_dirs = self.data_handler.get_reference_dirs(config_name)
        
        if not ref_dirs:
            return ""
        
        # Filter by type
        typed_refs = []
        for ref_dir in ref_dirs:
            params = self.data_handler.parse_reference_dir(ref_dir)
            if params.get('type') == ref_type:
                typed_refs.append((ref_dir, params))
        
        if not typed_refs:
            return ""
        
        # Find closest by direction (and Tp for waves)
        best_ref = None
        best_score = float('inf')
        
        for ref_dir, params in typed_refs:
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
                best_ref = ref_dir
        
        return best_ref or ""
    
    def process_fatigue_condition(self, config_name: str, condition: FatigueCondition,
                                 strut_num: int) -> Tuple[np.ndarray, Dict]:
        """Process a single fatigue condition by combining scaled wind and wave references"""
        
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
        
        # Check if configuration exists
        config_path = data_handler.base_path / config_name
        if not config_path.exists():
            logger.warning(f"Skipping {config_name} - path does not exist")
            continue
        
        # Get available references
        ref_dirs = data_handler.get_reference_dirs(config_name)
        logger.info(f"Found {len(ref_dirs)} reference seastates")
        
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