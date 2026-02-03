#!/usr/bin/env python3
"""
Reference Seastate Scaling Processor for Fatigue Analysis

This module implements the reference seastate scaling methodology for
fatigue analysis of strut foundations, combining:
- Reference seastate data (wind @ 10m/s, wave @ Hs=0.5m)
- Scaling factors for actual conditions
- Rainflow counting for cycle identification
- S-N curve fatigue damage calculation
- Configuration weighting for overall fatigue life

Author: DigitalModel Team
Date: 2025-01-20
"""

import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass, asdict
import logging
from datetime import datetime
import json

# Import existing modules
from .rainflow_counter import RainflowCounter
from .fatigue_damage_calculator import FatigueDamageCalculator, SNCurveParameters

logger = logging.getLogger(__name__)

@dataclass
class Configuration:
    """Vessel configuration with operational weighting"""
    name: str
    weight: float  # Percentage of operational time
    description: str
    
@dataclass
class FatigueCondition:
    """Environmental condition for fatigue analysis"""
    id: int
    wind_speed: float  # m/s
    wind_dir: float    # degrees
    hs: float          # significant wave height (m)
    tp: float          # peak period (s)
    wave_dir: float    # degrees
    occurrence: float  # percentage occurrence

@dataclass
class FatigueResult:
    """Results from fatigue analysis"""
    config: str
    strut: int
    condition_id: int
    tension_ranges: int
    max_tension: float
    min_tension: float
    mean_tension: float
    max_stress_range: float
    damage: float
    weighted_damage: float
    annual_damage: float
    fatigue_life_years: float
    occurrence_pct: float

class TensionToStressConverter:
    """Convert tension ranges to stress ranges using lookup table or formula"""
    
    def __init__(self, lookup_table_path: Optional[str] = None):
        """
        Initialize converter with optional lookup table
        
        Args:
            lookup_table_path: Path to CSV with tension-stress mapping
        """
        if lookup_table_path and Path(lookup_table_path).exists():
            self.lookup_df = pd.read_csv(lookup_table_path)
            self.tension_values = self.lookup_df['Tension Range (kN)'].values
            self.stress_values = self.lookup_df['Stress Range (Mpa)'].values
            logger.info(f"Loaded tension-stress lookup table from {lookup_table_path}")
        else:
            # Default linear relationship: Stress = Tension / 4
            self.tension_values = np.array([0, 2000])  # kN
            self.stress_values = np.array([0, 500])    # MPa
            logger.info("Using default linear tension-stress relationship")
    
    def convert_to_stress(self, tension_range: float) -> float:
        """
        Convert tension range to stress range
        
        Args:
            tension_range: Tension range in kN
            
        Returns:
            Stress range in MPa
        """
        return np.interp(tension_range, self.tension_values, self.stress_values)
    
    def convert_array(self, tension_ranges: np.ndarray) -> np.ndarray:
        """Convert array of tension ranges to stress ranges"""
        return np.array([self.convert_to_stress(t) for t in tension_ranges])

class ReferenceSeaStateProcessor:
    """
    Main processor for reference seastate scaling fatigue analysis
    """
    
    # Default configurations
    DEFAULT_CONFIGS = {
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
    
    def __init__(self,
                 data_path: str,
                 output_path: str = "output",
                 sn_curve: Optional[SNCurveParameters] = None,
                 scf: float = 1.0,
                 design_life_years: float = 20.0,
                 sample_timesteps: Optional[int] = None):
        """
        Initialize the processor
        
        Args:
            data_path: Path to data directory
            output_path: Path for output files
            sn_curve: S-N curve parameters (default: ABS E in Air)
            scf: Stress Concentration Factor
            design_life_years: Design life for fatigue assessment
            sample_timesteps: Limit timesteps for testing (None = use all)
        """
        self.data_path = Path(data_path)
        self.output_path = Path(output_path)
        self.sample_timesteps = sample_timesteps
        
        # Reference seastate parameters
        self.base_wind_speed = 10.0  # m/s
        self.base_hs = 0.5           # m
        
        # Initialize components
        self.rainflow_counter = RainflowCounter(gate_value=0.0001)  # 0.01% gate
        self.stress_converter = TensionToStressConverter()
        
        # Initialize fatigue calculator
        if sn_curve is None:
            sn_curve = FatigueDamageCalculator.CURVES['ABS_E_AIR']
        
        self.fatigue_calculator = FatigueDamageCalculator(
            sn_curve=sn_curve,
            scf=scf,
            design_life_years=design_life_years
        )
        
        # Load configurations
        self.configurations = self.DEFAULT_CONFIGS.copy()
        
        # Results storage
        self.results: List[FatigueResult] = []
        self.summary: Dict[str, Any] = {}
        
        logger.info(f"Initialized processor with data path: {data_path}")
        logger.info(f"S-N Curve: {sn_curve.name}, SCF: {scf}")
    
    def load_fatigue_conditions(self, conditions_file: Optional[str] = None) -> List[FatigueCondition]:
        """
        Load fatigue conditions from CSV file
        
        Args:
            conditions_file: Path to conditions CSV (optional)
            
        Returns:
            List of fatigue conditions
        """
        if conditions_file and Path(conditions_file).exists():
            df = pd.read_csv(conditions_file)
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
            
            logger.info(f"Loaded {len(conditions)} fatigue conditions from {conditions_file}")
            return conditions
        else:
            # Create sample conditions for testing
            return self._create_sample_conditions()
    
    def _create_sample_conditions(self) -> List[FatigueCondition]:
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
        
        logger.info(f"Created {len(conditions)} sample fatigue conditions")
        return conditions
    
    def load_strut_data(self, config_name: str, ref_dir: str, strut_num: int) -> Tuple[np.ndarray, np.ndarray]:
        """
        Load effective tension data for a specific strut
        
        Args:
            config_name: Configuration name
            ref_dir: Reference directory name
            strut_num: Strut number (1-8)
            
        Returns:
            Tuple of (time, tension) arrays
        """
        file_path = self.data_path / config_name / ref_dir / f"Strut{strut_num}.csv"
        
        if not file_path.exists():
            logger.warning(f"File not found: {file_path}")
            return np.array([]), np.array([])
        
        try:
            df = pd.read_csv(file_path, encoding='latin-1')
            
            # Find vessel end tension column
            vessel_columns = [col for col in df.columns 
                            if 'Vessel End' in col and 'Effective' in col]
            if not vessel_columns:
                vessel_columns = [col for col in df.columns 
                                if 'FST' in col and 'Vessel' in col]
            
            if vessel_columns:
                tension_col = vessel_columns[0]
                time_data = df.iloc[:, 0].values
                tension_data = df[tension_col].values
                
                # Apply timestep limit if specified
                if self.sample_timesteps:
                    time_data = time_data[:self.sample_timesteps]
                    tension_data = tension_data[:self.sample_timesteps]
                
                return time_data, tension_data
            else:
                logger.error(f"No vessel end tension column found in {file_path}")
                return np.array([]), np.array([])
                
        except Exception as e:
            logger.error(f"Error loading {file_path}: {e}")
            return np.array([]), np.array([])
    
    def select_reference_seastate(self, config_name: str, condition: FatigueCondition) -> Tuple[str, str]:
        """
        Select closest reference seastates for wind and wave
        
        Args:
            config_name: Configuration name
            condition: Fatigue condition with target parameters
            
        Returns:
            Tuple of (wind_ref_dir, wave_ref_dir)
        """
        config_path = self.data_path / config_name
        if not config_path.exists():
            return "", ""
        
        ref_dirs = [d.name for d in config_path.iterdir() if d.is_dir()]
        
        # Find best wind reference
        wind_refs = [d for d in ref_dirs if d.startswith('wind_')]
        best_wind = self._find_closest_direction(wind_refs, condition.wind_dir)
        
        # Find best wave reference
        wave_refs = [d for d in ref_dirs if d.startswith('wave_')]
        best_wave = self._find_closest_wave(wave_refs, condition.wave_dir, condition.tp)
        
        return best_wind, best_wave
    
    def _find_closest_direction(self, refs: List[str], target_dir: float) -> str:
        """Find reference with closest direction"""
        if not refs:
            return ""
        
        best_ref = ""
        best_diff = float('inf')
        
        for ref in refs:
            # Extract direction from name (e.g., "wind_045deg_...")
            parts = ref.split('_')
            if len(parts) >= 2:
                try:
                    ref_dir = float(parts[1].replace('deg', ''))
                    diff = abs(ref_dir - target_dir)
                    if diff > 180:
                        diff = 360 - diff
                    
                    if diff < best_diff:
                        best_diff = diff
                        best_ref = ref
                except ValueError:
                    continue
        
        return best_ref
    
    def _find_closest_wave(self, refs: List[str], target_dir: float, target_tp: float) -> str:
        """Find wave reference with closest direction and Tp"""
        if not refs:
            return ""
        
        # Available Tp values in references
        available_tps = [1.93, 2.70, 3.47]
        closest_tp = min(available_tps, key=lambda x: abs(x - target_tp))
        
        # Filter by Tp first
        tp_str = f"Tp{int(closest_tp * 100):03d}cs"
        tp_refs = [r for r in refs if tp_str in r]
        
        if tp_refs:
            return self._find_closest_direction(tp_refs, target_dir)
        else:
            return self._find_closest_direction(refs, target_dir)
    
    def process_fatigue_condition(self,
                                 config_name: str,
                                 condition: FatigueCondition,
                                 strut_num: int) -> Optional[FatigueResult]:
        """
        Process a single fatigue condition for one strut
        
        Args:
            config_name: Configuration name
            condition: Fatigue condition
            strut_num: Strut number
            
        Returns:
            FatigueResult or None if processing fails
        """
        # Calculate scaling factors
        wind_scale = (condition.wind_speed / self.base_wind_speed) ** 2
        wave_scale = condition.hs / self.base_hs
        
        # Select reference seastates
        wind_ref, wave_ref = self.select_reference_seastate(config_name, condition)
        
        if not wind_ref or not wave_ref:
            logger.warning(f"Could not find references for FC{condition.id:03d}")
            return None
        
        # Load reference data
        _, wind_tension = self.load_strut_data(config_name, wind_ref, strut_num)
        _, wave_tension = self.load_strut_data(config_name, wave_ref, strut_num)
        
        if len(wind_tension) == 0 or len(wave_tension) == 0:
            return None
        
        # Ensure same length
        min_len = min(len(wind_tension), len(wave_tension))
        wind_tension = wind_tension[:min_len]
        wave_tension = wave_tension[:min_len]
        
        # Scale and combine tensions
        scaled_wind = wind_tension * wind_scale
        scaled_wave = wave_tension * wave_scale
        effective_tension = scaled_wind + scaled_wave
        
        # Apply rainflow counting
        tension_ranges, cycle_counts = self.rainflow_counter.count_cycles(effective_tension)
        
        if len(tension_ranges) == 0:
            return None
        
        # Convert to stress
        stress_ranges = self.stress_converter.convert_array(tension_ranges)
        
        # Calculate fatigue damage
        damage_result = self.fatigue_calculator.calculate_fatigue_life(
            stress_ranges=stress_ranges,
            cycle_counts=cycle_counts,
            time_duration=min_len * 0.1,  # 0.1s timestep
            occurrence_weight=condition.occurrence / 100.0
        )
        
        return FatigueResult(
            config=config_name,
            strut=strut_num,
            condition_id=condition.id,
            tension_ranges=len(tension_ranges),
            max_tension=float(np.max(effective_tension)),
            min_tension=float(np.min(effective_tension)),
            mean_tension=float(np.mean(effective_tension)),
            max_stress_range=float(np.max(stress_ranges)) if len(stress_ranges) > 0 else 0,
            damage=damage_result['damage'],
            weighted_damage=damage_result['weighted_damage'],
            annual_damage=damage_result['annual_damage'],
            fatigue_life_years=damage_result['fatigue_life_years'],
            occurrence_pct=condition.occurrence
        )
    
    def process_configuration(self,
                            config_name: str,
                            conditions: List[FatigueCondition],
                            strut_nums: List[int]) -> Dict[str, Any]:
        """
        Process all conditions and struts for a configuration
        
        Args:
            config_name: Configuration name
            conditions: List of fatigue conditions
            strut_nums: List of strut numbers to process
            
        Returns:
            Configuration summary dictionary
        """
        logger.info(f"Processing configuration: {config_name}")
        config = self.configurations[config_name]
        
        # Check if configuration path exists
        config_path = self.data_path / config_name
        if not config_path.exists():
            logger.warning(f"Configuration path does not exist: {config_path}")
            return {}
        
        # Process each condition and strut
        config_results = []
        strut_damages = {f"Strut{i}": 0.0 for i in strut_nums}
        
        for condition in conditions:
            for strut_num in strut_nums:
                logger.debug(f"  Processing FC{condition.id:03d} - Strut{strut_num}")
                
                result = self.process_fatigue_condition(config_name, condition, strut_num)
                if result:
                    config_results.append(result)
                    self.results.append(result)
                    strut_damages[f"Strut{strut_num}"] += result.annual_damage
        
        # Calculate configuration fatigue life
        strut_lives = {}
        for strut, damage in strut_damages.items():
            if damage > 0:
                strut_lives[strut] = 1.0 / damage
            else:
                strut_lives[strut] = float('inf')
        
        # Find critical strut
        if strut_lives:
            min_life = min(strut_lives.values())
            critical_strut = min(strut_lives, key=strut_lives.get)
        else:
            min_life = float('inf')
            critical_strut = "None"
        
        return {
            'config_name': config_name,
            'weight_pct': config.weight,
            'description': config.description,
            'critical_strut': critical_strut,
            'min_fatigue_life': min_life,
            'strut_lives': strut_lives,
            'num_results': len(config_results)
        }
    
    def calculate_overall_fatigue_life(self, config_summaries: List[Dict[str, Any]]) -> float:
        """
        Calculate weighted overall fatigue life
        
        Args:
            config_summaries: List of configuration summaries
            
        Returns:
            Overall fatigue life in years
        """
        weighted_damage = 0.0
        
        for summary in config_summaries:
            if summary['min_fatigue_life'] < float('inf'):
                config_damage = 1.0 / summary['min_fatigue_life']
                weighted_damage += config_damage * (summary['weight_pct'] / 100.0)
        
        if weighted_damage > 0:
            return 1.0 / weighted_damage
        else:
            return float('inf')
    
    def run_analysis(self,
                    conditions: Optional[List[FatigueCondition]] = None,
                    strut_nums: Optional[List[int]] = None,
                    config_names: Optional[List[str]] = None) -> bool:
        """
        Run complete fatigue analysis
        
        Args:
            conditions: List of fatigue conditions (None = use defaults)
            strut_nums: List of strut numbers (None = [1,2,3,4,5,6,7,8])
            config_names: List of configurations to process (None = all)
            
        Returns:
            True if successful
        """
        # Set defaults
        if conditions is None:
            conditions = self.load_fatigue_conditions()
        
        if strut_nums is None:
            strut_nums = list(range(1, 9))
        
        if config_names is None:
            config_names = list(self.configurations.keys())
        
        logger.info("="*60)
        logger.info("REFERENCE SEASTATE SCALING FATIGUE ANALYSIS")
        logger.info("="*60)
        logger.info(f"Configurations: {len(config_names)}")
        logger.info(f"Fatigue conditions: {len(conditions)}")
        logger.info(f"Struts: {strut_nums}")
        
        # Process each configuration
        config_summaries = []
        
        for config_name in config_names:
            if config_name not in self.configurations:
                logger.warning(f"Unknown configuration: {config_name}")
                continue
            
            summary = self.process_configuration(config_name, conditions, strut_nums)
            if summary:
                config_summaries.append(summary)
        
        # Calculate overall fatigue life
        overall_life = self.calculate_overall_fatigue_life(config_summaries)
        
        # Store summary
        self.summary = {
            'timestamp': datetime.now().isoformat(),
            'configurations': config_summaries,
            'overall_fatigue_life': overall_life,
            'num_conditions': len(conditions),
            'num_struts': len(strut_nums),
            'total_results': len(self.results)
        }
        
        # Print results
        logger.info("\n" + "="*60)
        logger.info("ANALYSIS SUMMARY")
        logger.info("="*60)
        
        for summary in config_summaries:
            logger.info(f"\n{summary['config_name']} (Weight: {summary['weight_pct']:.2f}%)")
            logger.info(f"  Critical Strut: {summary['critical_strut']}")
            logger.info(f"  Min Fatigue Life: {summary['min_fatigue_life']:.1f} years")
        
        logger.info(f"\nWeighted Overall Fatigue Life: {overall_life:.1f} years")
        
        # Save results
        self.save_results()
        
        return True
    
    def save_results(self):
        """Save analysis results to files"""
        self.output_path.mkdir(parents=True, exist_ok=True)
        
        # Save detailed results
        if self.results:
            results_df = pd.DataFrame([asdict(r) for r in self.results])
            results_file = self.output_path / "fatigue_analysis_results.csv"
            results_df.to_csv(results_file, index=False)
            logger.info(f"Detailed results saved to: {results_file}")
        
        # Save summary
        summary_file = self.output_path / "analysis_summary.json"
        with open(summary_file, 'w') as f:
            json.dump(self.summary, f, indent=2, default=str)
        logger.info(f"Summary saved to: {summary_file}")
        
        # Create report
        self._create_report()
    
    def _create_report(self):
        """Create markdown report of results"""
        report_file = self.output_path / "fatigue_analysis_report.md"
        
        with open(report_file, 'w') as f:
            f.write("# Reference Seastate Scaling Fatigue Analysis Report\n\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            f.write("## Analysis Parameters\n\n")
            f.write(f"- S-N Curve: {self.fatigue_calculator.sn_curve.name}\n")
            f.write(f"- SCF: {self.fatigue_calculator.scf}\n")
            f.write(f"- Design Life: {self.fatigue_calculator.design_life_years} years\n")
            f.write(f"- Data Path: {self.data_path}\n\n")
            
            f.write("## Configuration Summary\n\n")
            f.write("| Configuration | Weight (%) | Critical Strut | Min Life (years) |\n")
            f.write("|---------------|------------|----------------|------------------|\n")
            
            for config in self.summary.get('configurations', []):
                f.write(f"| {config['config_name']} | {config['weight_pct']:.2f} | "
                       f"{config['critical_strut']} | {config['min_fatigue_life']:.1f} |\n")
            
            f.write(f"\n**Overall Weighted Fatigue Life: {self.summary.get('overall_fatigue_life', 0):.1f} years**\n")
        
        logger.info(f"Report saved to: {report_file}")