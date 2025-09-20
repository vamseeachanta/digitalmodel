#!/usr/bin/env python
"""
Integrated Production Data Processor with Rainflow Counting and Fatigue Analysis
Processes 4 vessel configurations with complete fatigue life estimation
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict
import logging
from datetime import datetime

# Add parent directory to path to import modules
sys.path.append(str(Path(__file__).parent.parent.parent.parent))

# Import existing modules
from digitalmodel.modules.fatigue_analysis.rainflow_counter import RainflowCounter
from digitalmodel.modules.fatigue_analysis.fatigue_damage_calculator import (
    FatigueDamageCalculator, SNCurveParameters
)

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

class TensionToStressConverter:
    """Convert tension ranges to stress ranges using lookup table"""
    
    def __init__(self, lookup_table_path: str = None):
        """Initialize the converter with lookup table"""
        if lookup_table_path and Path(lookup_table_path).exists():
            self.lookup_df = pd.read_csv(lookup_table_path)
            self.tension_values = self.lookup_df['Tension Range (kN)'].values
            self.stress_values = self.lookup_df['Stress Range (Mpa)'].values
        else:
            # Default linear relationship: Stress = Tension / 4
            self.tension_values = np.array([0, 2000])
            self.stress_values = np.array([0, 500])
    
    def convert_to_stress(self, tension_range: float) -> float:
        """Convert tension range to stress range using interpolation"""
        return np.interp(tension_range, self.tension_values, self.stress_values)

class ProductionDataHandler:
    """Handles production data loading and processing"""
    
    def __init__(self, base_path: str = None, sample_timesteps: int = 1000):
        """Initialize the production data handler"""
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
        parts = ref_dir.split('_')
        
        if parts[0] == 'wind':
            return {
                'type': 'wind',
                'direction': int(parts[1].replace('deg', '')),
                'speed': 10.0
            }
        elif parts[0] == 'wave':
            return {
                'type': 'wave',
                'direction': int(parts[1].replace('deg', '')),
                'hs': 0.5,
                'tp': float(parts[-1].replace('Tp', '').replace('cs', '')) / 100 if 'Tp' in parts[-1] else 2.7
            }
        return {}
    
    def load_strut_data(self, config_name: str, ref_dir: str, strut_num: int,
                       use_sample: bool = True) -> Tuple[np.ndarray, np.ndarray]:
        """Load effective tension data for a specific strut"""
        file_path = self.base_path / config_name / ref_dir / f"Strut{strut_num}.csv"
        
        if not file_path.exists():
            logger.warning(f"File does not exist: {file_path}")
            return np.array([]), np.array([])
        
        try:
            df = pd.read_csv(file_path, encoding='latin-1')
            
            # Find vessel end tension column
            vessel_end_columns = [col for col in df.columns if 'Vessel End' in col and 'Effective' in col]
            if not vessel_end_columns:
                vessel_end_columns = [col for col in df.columns if 'FST' in col and 'Vessel' in col]
            
            if vessel_end_columns:
                tension_col = vessel_end_columns[0]
            else:
                logger.warning(f"Could not find vessel end tension column in {file_path}")
                return np.array([]), np.array([])
            
            time_data = df.iloc[:, 0].values
            tension_data = df[tension_col].values
            
            if use_sample and len(time_data) > self.sample_timesteps:
                time_data = time_data[:self.sample_timesteps]
                tension_data = tension_data[:self.sample_timesteps]
            
            return time_data, tension_data
            
        except Exception as e:
            logger.error(f"Error loading {file_path}: {e}")
            return np.array([]), np.array([])

class IntegratedFatigueProcessor:
    """Integrated processor with rainflow counting and fatigue damage calculation"""
    
    def __init__(self, data_handler: ProductionDataHandler):
        self.data_handler = data_handler
        self.base_wind_speed = 10.0  # m/s
        self.base_hs = 0.5  # m
        
        # Initialize components
        self.rainflow_counter = RainflowCounter(gate_value=0.0001)  # 0.01% gate
        self.stress_converter = TensionToStressConverter("input/tension_range_to_stress_range_function.csv")
        
        # Initialize fatigue calculator with ABS E in Air curve
        self.sn_curve = SNCurveParameters(
            name="ABS E in Air",
            log_a1=12.018,
            m1=3.0,
            log_a2=11.170,
            m2=5.0,
            threshold=1e6
        )
        self.fatigue_calculator = FatigueDamageCalculator(
            sn_curve=self.sn_curve,
            scf=1.0,
            design_life_years=20.0
        )
        
        # Load fatigue conditions
        self.fatigue_conditions = self.load_fatigue_conditions()
    
    def load_fatigue_conditions(self) -> List[FatigueCondition]:
        """Load fatigue conditions from CSV"""
        fatigue_file = Path("input/fatigue_conditions.csv")
        
        if not fatigue_file.exists():
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
        sample_conditions = [
            (5, 0, 0.15, 1.93, 0, 7.76),
            (10, 45, 0.25, 2.70, 45, 5.50),
            (15, 90, 0.35, 3.47, 120, 3.25),
        ]
        
        conditions = []
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
        
        typed_refs = []
        for ref_dir in ref_dirs:
            params = self.data_handler.parse_reference_dir(ref_dir)
            if params.get('type') == ref_type:
                typed_refs.append((ref_dir, params))
        
        if not typed_refs:
            return ""
        
        best_ref = None
        best_score = float('inf')
        
        for ref_dir, params in typed_refs:
            dir_diff = abs(params['direction'] - direction)
            if dir_diff > 180:
                dir_diff = 360 - dir_diff
            
            score = dir_diff
            
            if ref_type == 'wave' and tp is not None and 'tp' in params:
                tp_diff = abs(params['tp'] - tp)
                score += tp_diff * 50
            
            if score < best_score:
                best_score = score
                best_ref = ref_dir
        
        return best_ref or ""
    
    def process_fatigue_condition(self, config_name: str, condition: FatigueCondition,
                                 strut_num: int) -> Dict:
        """Process a single fatigue condition with full fatigue analysis"""
        
        # Calculate scaling factors
        wind_scale = (condition.wind_speed / self.base_wind_speed) ** 2
        wave_scale = condition.hs / self.base_hs
        
        # Select closest references
        wind_ref = self.select_closest_reference(config_name, 'wind', condition.wind_dir)
        wave_ref = self.select_closest_reference(config_name, 'wave', condition.wave_dir, condition.tp)
        
        if not wind_ref or not wave_ref:
            logger.warning(f"Could not find references for FC{condition.id:03d}")
            return {}
        
        # Load reference data
        _, wind_tension = self.data_handler.load_strut_data(config_name, wind_ref, strut_num)
        _, wave_tension = self.data_handler.load_strut_data(config_name, wave_ref, strut_num)
        
        if len(wind_tension) == 0 or len(wave_tension) == 0:
            return {}
        
        # Ensure same length
        min_len = min(len(wind_tension), len(wave_tension))
        wind_tension = wind_tension[:min_len]
        wave_tension = wave_tension[:min_len]
        
        # Scale and combine
        scaled_wind = wind_tension * wind_scale
        scaled_wave = wave_tension * wave_scale
        effective_tension = scaled_wind + scaled_wave
        
        # Apply rainflow counting
        tension_ranges, cycle_counts = self.rainflow_counter.count_cycles(effective_tension)
        
        # Convert tension ranges to stress ranges
        stress_ranges = np.array([self.stress_converter.convert_to_stress(t) for t in tension_ranges])
        
        # Calculate fatigue damage
        damage = 0.0
        for stress_range, cycles in zip(stress_ranges, cycle_counts):
            if stress_range > 0:
                # Calculate cycles to failure
                n_failure = self.fatigue_calculator.cycles_to_failure(stress_range)
                # Accumulate damage
                damage += cycles / n_failure
        
        # Weight by occurrence percentage
        weighted_damage = damage * (condition.occurrence / 100.0)
        
        # Scale to annual damage
        # Sample represents 100 seconds of data, need to scale to full year
        seconds_per_year = 365.25 * 24 * 3600
        sample_seconds = min_len * 0.1  # 0.1s timestep
        annual_damage = weighted_damage * (seconds_per_year / sample_seconds)
        
        # Calculate fatigue life
        fatigue_life = 1.0 / annual_damage if annual_damage > 0 else float('inf')
        
        return {
            'config': config_name,
            'condition_id': condition.id,
            'strut': strut_num,
            'tension_ranges': len(tension_ranges),
            'max_tension': float(np.max(effective_tension)),
            'min_tension': float(np.min(effective_tension)),
            'mean_tension': float(np.mean(effective_tension)),
            'max_stress_range': float(np.max(stress_ranges)) if len(stress_ranges) > 0 else 0,
            'damage': damage,
            'weighted_damage': weighted_damage,
            'annual_damage': annual_damage,
            'fatigue_life_years': fatigue_life,
            'occurrence_pct': condition.occurrence
        }

def main(data_path=None):
    """Main execution function with integrated fatigue analysis"""
    logger.info("=" * 60)
    logger.info("INTEGRATED FATIGUE ANALYSIS WITH RAINFLOW & DAMAGE")
    logger.info("=" * 60)
    
    # Initialize handlers
    data_handler = ProductionDataHandler(base_path=data_path, sample_timesteps=1000)
    processor = IntegratedFatigueProcessor(data_handler)
    
    # Create output directories
    output_base = Path("output")
    output_base.mkdir(exist_ok=True)
    
    # Results storage
    all_results = []
    config_summary = {}
    
    # Process each configuration
    for config_name, config in data_handler.configurations.items():
        logger.info(f"\nProcessing {config_name} (Weight: {config.weight}%)")
        logger.info(f"Description: {config.description}")
        
        config_path = data_handler.base_path / config_name
        if not config_path.exists():
            logger.warning(f"Skipping {config_name} - path does not exist")
            continue
        
        ref_dirs = data_handler.get_reference_dirs(config_name)
        logger.info(f"Found {len(ref_dirs)} reference seastates")
        
        config_output = output_base / config_name
        config_output.mkdir(exist_ok=True)
        
        # Store damage accumulation for this configuration
        strut_damages = {f"Strut{i}": 0.0 for i in range(1, 9)}
        
        # Process fatigue conditions
        num_conditions = min(3, len(processor.fatigue_conditions))  # Use 3 for demo
        
        for fc_idx in range(num_conditions):
            condition = processor.fatigue_conditions[fc_idx]
            
            # Process struts
            for strut_num in [1, 2]:  # Use 2 struts for demo
                logger.info(f"  Processing FC{condition.id:03d} - Strut{strut_num}")
                
                # Process with full fatigue analysis
                result = processor.process_fatigue_condition(
                    config_name, condition, strut_num
                )
                
                if result:
                    all_results.append(result)
                    strut_damages[f"Strut{strut_num}"] += result['annual_damage']
        
        # Calculate configuration fatigue life
        config_life = {}
        for strut, damage in strut_damages.items():
            if damage > 0:
                config_life[strut] = 1.0 / damage
            else:
                config_life[strut] = float('inf')
        
        if config_life:
            min_life = min(config_life.values())
            critical_strut = min(config_life, key=config_life.get)
            
            config_summary[config_name] = {
                'weight_pct': config.weight,
                'critical_strut': critical_strut,
                'min_fatigue_life': min_life,
                'strut_lives': config_life
            }
    
    # Save detailed results
    if all_results:
        results_df = pd.DataFrame(all_results)
        results_file = output_base / "integrated_fatigue_results.csv"
        results_df.to_csv(results_file, index=False)
        
        # Calculate weighted overall fatigue life
        if config_summary:
            weighted_damage = sum(
                1.0 / summary['min_fatigue_life'] * summary['weight_pct'] / 100
                for summary in config_summary.values()
                if summary['min_fatigue_life'] < float('inf')
            )
            overall_life = 1.0 / weighted_damage if weighted_damage > 0 else float('inf')
        else:
            overall_life = float('inf')
        
        # Print summary
        logger.info("\n" + "=" * 60)
        logger.info("FATIGUE LIFE SUMMARY")
        logger.info("=" * 60)
        
        for config_name, summary in config_summary.items():
            logger.info(f"\n{config_name} (Weight: {summary['weight_pct']:.2f}%)")
            logger.info(f"  Critical Strut: {summary['critical_strut']}")
            logger.info(f"  Min Fatigue Life: {summary['min_fatigue_life']:.1f} years")
            
            for strut, life in summary['strut_lives'].items():
                if life < float('inf'):
                    logger.info(f"    {strut}: {life:.1f} years")
        
        logger.info(f"\nWeighted Overall Fatigue Life: {overall_life:.1f} years")
        logger.info(f"Results saved to: {results_file}")
        
        # Save configuration summary
        summary_file = output_base / "configuration_summary.json"
        import json
        with open(summary_file, 'w') as f:
            json.dump(config_summary, f, indent=2, default=str)
        logger.info(f"Configuration summary saved to: {summary_file}")
    else:
        logger.warning("No results generated")
    
    return all_results, config_summary

if __name__ == "__main__":
    import sys
    data_path = sys.argv[1] if len(sys.argv) > 1 else None
    results, summary = main(data_path)