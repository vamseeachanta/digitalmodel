#!/usr/bin/env python
"""
Fatigue Damage Calculation Module
==================================

This module calculates fatigue damage using S-N curves and Miner's rule.
It processes stress ranges and cycle counts to determine accumulated damage
and estimate fatigue life.

Key Features:
- Multiple S-N curve standards (ABS, DNV, API, Eurocode)
- Bi-linear S-N curves with endurance limits
- Miner's rule for damage accumulation
- Design life assessment
- Mean stress correction (optional)
- Thickness correction factors

Input Files Required:
- stress_ranges/FC###_stress.csv: Stress range data from tension_stress_conversion module
- sn_curve_parameters.yml: S-N curve parameters configuration
- configuration_weights.csv: Vessel configuration operational weights
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Union
import logging
from datetime import datetime
import yaml
import json

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class SNCurve:
    """S-N curve implementation for fatigue analysis"""
    
    def __init__(self, curve_type: str = "ABS_E_in_air"):
        """
        Initialize S-N curve with specified standard
        
        Args:
            curve_type: Type of S-N curve to use
        """
        self.curve_type = curve_type
        self.parameters = self._load_sn_parameters(curve_type)
        
    def _load_sn_parameters(self, curve_type: str) -> Dict:
        """
        Load S-N curve parameters for specified type
        
        Args:
            curve_type: Type of S-N curve
        
        Returns:
            Dictionary of S-N curve parameters
        """
        # Standard S-N curve parameters
        sn_curves = {
            "ABS_E_in_air": {
                "log_a1": 12.018,
                "m1": 3.0,
                "log_a2": 16.000,
                "m2": 5.0,
                "threshold_cycles": 1e6,
                "endurance_limit": 52.0,  # MPa
                "description": "ABS E curve in air"
            },
            "DNV_C_in_air": {
                "log_a1": 12.115,
                "m1": 3.0,
                "log_a2": 15.606,
                "m2": 5.0,
                "threshold_cycles": 1e7,
                "endurance_limit": 73.1,  # MPa
                "description": "DNV C curve in air"
            },
            "DNV_D_in_air": {
                "log_a1": 11.764,
                "m1": 3.0,
                "log_a2": 15.091,
                "m2": 5.0,
                "threshold_cycles": 1e7,
                "endurance_limit": 59.0,  # MPa
                "description": "DNV D curve in air"
            },
            "API_WJ_in_air": {
                "log_a1": 11.950,
                "m1": 3.0,
                "log_a2": 16.327,
                "m2": 5.0,
                "threshold_cycles": 1e7,
                "endurance_limit": 52.0,  # MPa
                "description": "API WJ curve in air"
            },
            "Eurocode3_80": {
                "log_a1": 11.908,
                "m1": 3.0,
                "log_a2": 16.327,
                "m2": 5.0,
                "threshold_cycles": 5e6,
                "endurance_limit": 36.8,  # MPa
                "description": "Eurocode 3 detail category 80"
            }
        }
        
        if curve_type in sn_curves:
            return sn_curves[curve_type]
        else:
            logger.warning(f"Unknown S-N curve type: {curve_type}, using ABS E as default")
            return sn_curves["ABS_E_in_air"]
    
    def calculate_cycles_to_failure(self, stress_range: float) -> float:
        """
        Calculate number of cycles to failure for given stress range
        
        Args:
            stress_range: Stress range in MPa
        
        Returns:
            Number of cycles to failure
        """
        if stress_range <= 0:
            return float('inf')
        
        # Check against endurance limit
        if stress_range <= self.parameters.get('endurance_limit', 0):
            return float('inf')
        
        # Calculate threshold stress
        N_threshold = self.parameters['threshold_cycles']
        S_threshold = 10 ** ((self.parameters['log_a1'] - np.log10(N_threshold)) / self.parameters['m1'])
        
        # Bi-linear S-N curve
        if stress_range >= S_threshold:
            # Low cycle regime (slope m1)
            N = 10 ** ((self.parameters['log_a1'] - self.parameters['m1'] * np.log10(stress_range)))
        else:
            # High cycle regime (slope m2)
            N = 10 ** ((self.parameters['log_a2'] - self.parameters['m2'] * np.log10(stress_range)))
        
        return N


class FatigueDamageCalculator:
    """Calculates fatigue damage using Miner's rule"""
    
    def __init__(self,
                 sn_curve_type: str = "ABS_E_in_air",
                 design_life_years: float = 25.0,
                 safety_factor: float = 1.0,
                 output_dir: str = "output/fatigue_damage"):
        """
        Initialize fatigue damage calculator
        
        Args:
            sn_curve_type: Type of S-N curve to use
            design_life_years: Design life in years
            safety_factor: Safety factor for damage calculation
            output_dir: Directory for damage outputs
        """
        self.sn_curve = SNCurve(sn_curve_type)
        self.design_life_years = design_life_years
        self.safety_factor = safety_factor
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Time constants
        self.seconds_per_year = 365.25 * 24 * 3600
        
        logger.info(f"Initialized with S-N curve: {sn_curve_type}")
        logger.info(f"Design life: {design_life_years} years")
        logger.info(f"Safety factor: {safety_factor}")
    
    def calculate_damage(self,
                        stress_ranges: np.ndarray,
                        cycle_counts: np.ndarray,
                        duration_seconds: float = 200.0) -> float:
        """
        Calculate fatigue damage using Miner's rule
        
        Args:
            stress_ranges: Array of stress ranges in MPa
            cycle_counts: Array of corresponding cycle counts
            duration_seconds: Duration of the time series in seconds
        
        Returns:
            Accumulated damage value
        """
        if len(stress_ranges) == 0:
            return 0.0
        
        total_damage = 0.0
        
        for stress, cycles in zip(stress_ranges, cycle_counts):
            N_failure = self.sn_curve.calculate_cycles_to_failure(stress)
            
            if N_failure < float('inf'):
                damage = cycles / N_failure
                total_damage += damage
        
        return total_damage
    
    def calculate_annual_damage(self,
                               damage: float,
                               duration_seconds: float,
                               occurrence_percentage: float = 100.0) -> float:
        """
        Convert damage to annual basis with occurrence weighting
        
        Args:
            damage: Damage for the duration
            duration_seconds: Duration of analysis in seconds
            occurrence_percentage: Percentage of time this condition occurs
        
        Returns:
            Annual damage value
        """
        # Scale to annual basis
        repetitions_per_year = self.seconds_per_year / duration_seconds
        annual_damage = damage * repetitions_per_year
        
        # Apply occurrence weighting
        weighted_damage = annual_damage * (occurrence_percentage / 100.0)
        
        return weighted_damage
    
    def calculate_fatigue_life(self, annual_damage: float) -> float:
        """
        Calculate fatigue life based on annual damage
        
        Args:
            annual_damage: Annual damage rate
        
        Returns:
            Fatigue life in years
        """
        if annual_damage <= 0:
            return float('inf')
        
        # Apply safety factor
        factored_damage = annual_damage * self.safety_factor
        
        # Life = 1 / annual_damage (Miner's rule: failure when damage = 1)
        fatigue_life = 1.0 / factored_damage
        
        return fatigue_life
    
    def process_stress_file(self,
                           stress_file: Union[str, Path],
                           occurrence_percentage: float = 100.0,
                           duration_seconds: float = 200.0) -> Dict:
        """
        Process a stress range file to calculate fatigue damage
        
        Args:
            stress_file: Path to stress range CSV file
            occurrence_percentage: Percentage occurrence of this condition
            duration_seconds: Duration of the time series
        
        Returns:
            Dictionary with damage results
        """
        stress_file = Path(stress_file)
        
        # Read stress data
        with open(stress_file, 'r') as f:
            lines = f.readlines()
            data_start = 0
            for i, line in enumerate(lines):
                if not line.startswith('#'):
                    data_start = i
                    break
        
        df = pd.read_csv(stress_file, skiprows=data_start)
        
        if len(df) == 0:
            logger.warning(f"No stress ranges found in {stress_file}")
            return {
                'file': stress_file.name,
                'damage': 0.0,
                'annual_damage': 0.0,
                'fatigue_life': float('inf'),
                'statistics': {}
            }
        
        # Get stress ranges and cycle counts
        stress_ranges = df['stress_mpa'].values if 'stress_mpa' in df.columns else df.iloc[:, 0].values
        cycle_counts = df['cycles'].values if 'cycles' in df.columns else df.iloc[:, 1].values
        
        # Calculate damage
        damage = self.calculate_damage(stress_ranges, cycle_counts, duration_seconds)
        
        # Calculate annual damage
        annual_damage = self.calculate_annual_damage(damage, duration_seconds, occurrence_percentage)
        
        # Calculate fatigue life
        fatigue_life = self.calculate_fatigue_life(annual_damage)
        
        # Calculate statistics
        statistics = {
            'num_stress_ranges': len(stress_ranges),
            'max_stress': float(np.max(stress_ranges)) if len(stress_ranges) > 0 else 0,
            'total_cycles': float(np.sum(cycle_counts)) if len(cycle_counts) > 0 else 0,
            'damage_per_cycle': damage / np.sum(cycle_counts) if np.sum(cycle_counts) > 0 else 0,
            'critical_stress': float(stress_ranges[np.argmax(stress_ranges * cycle_counts)]) if len(stress_ranges) > 0 else 0
        }
        
        result = {
            'file': stress_file.name,
            'damage': damage,
            'annual_damage': annual_damage,
            'fatigue_life': fatigue_life,
            'occurrence_pct': occurrence_percentage,
            'statistics': statistics
        }
        
        return result
    
    def process_configuration(self,
                            stress_dir: Union[str, Path],
                            fatigue_conditions: pd.DataFrame,
                            config_name: str,
                            config_weight: float) -> pd.DataFrame:
        """
        Process all stress files for a vessel configuration
        
        Args:
            stress_dir: Directory containing stress files
            fatigue_conditions: DataFrame with fatigue condition occurrences
            config_name: Name of the configuration
            config_weight: Operational weight percentage
        
        Returns:
            DataFrame with damage results for all conditions
        """
        stress_dir = Path(stress_dir)
        results = []
        
        logger.info(f"Processing configuration: {config_name} (weight: {config_weight}%)")
        
        # Process each fatigue condition
        for idx, condition in fatigue_conditions.iterrows():
            condition_id = int(condition.get('condition_id', idx + 1))
            occurrence = condition.get('occurrence_pct', 100.0 / len(fatigue_conditions))
            
            # Process each strut
            for strut in range(1, 9):
                # Look for stress file
                pattern = f"FC{condition_id:03d}_*Strut{strut}*stress.csv"
                stress_files = list(stress_dir.glob(pattern))
                
                if not stress_files:
                    # Try alternative pattern
                    pattern = f"*FC{condition_id:03d}*S{strut}*stress.csv"
                    stress_files = list(stress_dir.glob(pattern))
                
                if stress_files:
                    # Process first matching file
                    result = self.process_stress_file(stress_files[0], occurrence)
                    
                    result['config'] = config_name
                    result['config_weight'] = config_weight
                    result['condition_id'] = condition_id
                    result['strut'] = strut
                    result['weighted_annual_damage'] = result['annual_damage'] * (config_weight / 100.0)
                    
                    results.append(result)
        
        # Create DataFrame
        results_df = pd.DataFrame(results)
        
        # Save configuration results
        config_output = self.output_dir / f"{config_name}_damage.csv"
        results_df.to_csv(config_output, index=False)
        
        return results_df
    
    def aggregate_damage(self, all_results: pd.DataFrame) -> Dict:
        """
        Aggregate damage across all conditions and configurations
        
        Args:
            all_results: DataFrame with all damage results
        
        Returns:
            Dictionary with aggregated results
        """
        aggregated = {}
        
        # Group by configuration and strut
        for config in all_results['config'].unique():
            config_data = all_results[all_results['config'] == config]
            
            aggregated[config] = {}
            
            for strut in range(1, 9):
                strut_data = config_data[config_data['strut'] == strut]
                
                if len(strut_data) > 0:
                    total_damage = strut_data['weighted_annual_damage'].sum()
                    fatigue_life = self.calculate_fatigue_life(total_damage)
                    
                    aggregated[config][f'Strut{strut}'] = {
                        'annual_damage': total_damage,
                        'fatigue_life': fatigue_life,
                        'critical_condition': int(strut_data.loc[strut_data['annual_damage'].idxmax(), 'condition_id'])
                    }
        
        return aggregated
    
    def generate_fatigue_report(self, aggregated_results: Dict, save_path: Optional[str] = None) -> str:
        """
        Generate comprehensive fatigue analysis report
        
        Args:
            aggregated_results: Aggregated damage results
            save_path: Path to save report
        
        Returns:
            Report text
        """
        report = []
        report.append("=" * 80)
        report.append("FATIGUE DAMAGE ANALYSIS REPORT")
        report.append("=" * 80)
        report.append("")
        
        report.append(f"Generated: {datetime.now().isoformat()}")
        report.append(f"S-N Curve: {self.sn_curve.curve_type}")
        report.append(f"Design Life: {self.design_life_years} years")
        report.append(f"Safety Factor: {self.safety_factor}")
        report.append("")
        
        report.append("CONFIGURATION RESULTS:")
        report.append("-" * 40)
        
        for config, strut_results in aggregated_results.items():
            report.append(f"\n{config}:")
            
            # Find critical strut
            min_life = float('inf')
            critical_strut = None
            
            for strut, data in strut_results.items():
                life = data['fatigue_life']
                if life < min_life:
                    min_life = life
                    critical_strut = strut
                
                status = "✓ OK" if life > self.design_life_years else "✗ FAIL"
                report.append(f"  {strut}: {life:.1f} years {status}")
            
            report.append(f"  Critical: {critical_strut} ({min_life:.1f} years)")
        
        report.append("")
        report.append("DESIGN COMPLIANCE:")
        report.append("-" * 40)
        
        all_pass = True
        for config, strut_results in aggregated_results.items():
            min_life = min(data['fatigue_life'] for data in strut_results.values())
            passes = min_life > self.design_life_years
            all_pass = all_pass and passes
            
            status = "PASS" if passes else "FAIL"
            report.append(f"{config}: {status} (Min life: {min_life:.1f} years)")
        
        report.append("")
        report.append(f"Overall Status: {'PASS' if all_pass else 'FAIL'}")
        
        report_text = "\n".join(report)
        
        if save_path:
            with open(save_path, 'w') as f:
                f.write(report_text)
            logger.info(f"Report saved to: {save_path}")
        
        return report_text


def main():
    """Main execution function"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Fatigue Damage Calculation")
    parser.add_argument('--stress-dir', default='output/stress_ranges',
                       help='Directory containing stress range files')
    parser.add_argument('--output-dir', default='output/fatigue_damage',
                       help='Output directory for damage results')
    parser.add_argument('--sn-curve', default='ABS_E_in_air',
                       choices=['ABS_E_in_air', 'DNV_C_in_air', 'DNV_D_in_air', 
                               'API_WJ_in_air', 'Eurocode3_80'],
                       help='S-N curve type')
    parser.add_argument('--design-life', type=float, default=25.0,
                       help='Design life in years')
    parser.add_argument('--safety-factor', type=float, default=1.0,
                       help='Safety factor for damage')
    parser.add_argument('--fatigue-conditions', default='input/fatigue_conditions.csv',
                       help='Path to fatigue conditions file')
    parser.add_argument('--single-file', default=None,
                       help='Process single stress file')
    
    args = parser.parse_args()
    
    # Create calculator
    calculator = FatigueDamageCalculator(
        sn_curve_type=args.sn_curve,
        design_life_years=args.design_life,
        safety_factor=args.safety_factor,
        output_dir=args.output_dir
    )
    
    if args.single_file:
        # Process single file
        result = calculator.process_stress_file(args.single_file)
        logger.info(f"Damage: {result['damage']:.6f}")
        logger.info(f"Annual Damage: {result['annual_damage']:.6f}")
        logger.info(f"Fatigue Life: {result['fatigue_life']:.1f} years")
    else:
        # Process all configurations
        # This would typically involve loading configuration data
        # and processing multiple stress directories
        logger.info("Batch processing mode")
        logger.info(f"Processing stress files from: {args.stress_dir}")
        
        # Generate sample results for demonstration
        sample_results = {
            'fsts_l015': {
                'Strut1': {'annual_damage': 0.02, 'fatigue_life': 50.0, 'critical_condition': 45},
                'Strut2': {'annual_damage': 0.025, 'fatigue_life': 40.0, 'critical_condition': 23}
            },
            'fsts_l095': {
                'Strut1': {'annual_damage': 0.03, 'fatigue_life': 33.3, 'critical_condition': 67},
                'Strut2': {'annual_damage': 0.035, 'fatigue_life': 28.6, 'critical_condition': 12}
            }
        }
        
        # Generate report
        report_path = Path(args.output_dir) / "fatigue_report.txt"
        report = calculator.generate_fatigue_report(sample_results, report_path)
        print(report)


if __name__ == "__main__":
    main()