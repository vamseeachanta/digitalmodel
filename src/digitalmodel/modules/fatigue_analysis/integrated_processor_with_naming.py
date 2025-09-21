#!/usr/bin/env python3
"""
Integrated Fatigue Analysis Processor with Proper Naming Convention

This module integrates all fatigue analysis components with the agreed
naming convention: {config}_FC{###}_Strut{#}_{type}.csv
"""

import numpy as np
import pandas as pd
import logging
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

from .strut_foundation_processor import ProductionDataHandler, LoadScaler, FatigueCondition
from .rainflow_counter import RainflowCounter
from .fatigue_damage_calculator import FatigueDamageCalculator, SNCurveParameters
from .file_namer import FatigueFileNamer, OverallFileNamer, create_output_structure

logger = logging.getLogger(__name__)


class IntegratedFatigueProcessorWithNaming:
    """
    Integrated fatigue analysis processor with proper file naming
    """
    
    def __init__(self, data_handler: ProductionDataHandler, output_path: str = "output"):
        self.data_handler = data_handler
        self.load_scaler = LoadScaler(data_handler)
        
        # Initialize analysis components
        self.rainflow_counter = RainflowCounter(gate_value=0.0001)
        
        # Create tension-to-stress converter
        from .integrated_processor import TensionToStressConverter
        stress_table_path = Path("input/tension_range_to_stress_range_function.csv")
        if stress_table_path.exists():
            self.stress_converter = TensionToStressConverter(str(stress_table_path))
        else:
            self.stress_converter = TensionToStressConverter()
        
        # Initialize fatigue calculator with ABS E in Air S-N curve
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
        
        logger.info(f"Initialized fatigue calculator with {self.sn_curve.name} curve, SCF=1.0")
        
        # Load fatigue conditions
        self.fatigue_conditions = self.load_scaler.fatigue_conditions
        
        # Create file naming structure
        configs = list(data_handler.configurations.keys())
        self.namers = create_output_structure(output_path, configs)
        self.output_path = Path(output_path)
    
    def process_single_condition(self, config_name: str, condition: FatigueCondition, 
                                strut_num: int, save_intermediate: bool = True) -> dict:
        """
        Process a single fatigue condition with proper file naming
        """
        # Get file namer for this configuration
        namer = self.namers[config_name]
        
        # Step 1: Get scaled tensions
        effective_tension, metadata = self.load_scaler.process_fatigue_condition(
            config_name, condition, strut_num
        )
        
        if len(effective_tension) == 0:
            logger.warning(f"No data for {config_name} FC{condition.id:03d} Strut{strut_num}")
            return None
        
        # Save combined tension if requested
        if save_intermediate:
            combined_file = namer.get_combined_filename(condition.id, strut_num)
            df_combined = pd.DataFrame({
                'Time': np.arange(len(effective_tension)) * 0.1,
                'Effective_Tension_kN': effective_tension,
                'Wind_Scale': metadata['wind_scale_factor'],
                'Wave_Scale': metadata['wave_scale_factor']
            })
            df_combined.to_csv(combined_file, index=False)
        
        # Step 2: Rainflow counting
        tension_ranges, cycle_counts = self.rainflow_counter.count_cycles(effective_tension)
        
        if save_intermediate and len(tension_ranges) > 0:
            rainflow_file = namer.get_rainflow_filename(condition.id, strut_num)
            df_rainflow = pd.DataFrame({
                'Tension_Range_kN': tension_ranges,
                'Cycle_Count': cycle_counts
            })
            df_rainflow.to_csv(rainflow_file, index=False)
        
        # Step 3: Convert tension to stress
        stress_ranges = self.stress_converter.convert_to_stress(tension_ranges)
        
        # Step 4: Calculate damage
        damage = self.fatigue_calculator.calculate_damage(stress_ranges, cycle_counts)
        weighted_damage = damage * (condition.occurrence / 100.0)
        
        # Scale to annual damage
        seconds_per_year = 365.25 * 24 * 3600
        sample_seconds = len(effective_tension) * 0.1  # 0.1s timestep
        annual_damage = weighted_damage * (seconds_per_year / sample_seconds)
        fatigue_life = 1 / annual_damage if annual_damage > 0 else float('inf')
        
        # Save damage results if requested
        if save_intermediate and len(stress_ranges) > 0:
            damage_file = namer.get_damage_filename(condition.id, strut_num)
            df_damage = pd.DataFrame({
                'Stress_Range_MPa': stress_ranges,
                'Cycle_Count': cycle_counts,
                'Damage_Per_Cycle': damage / np.sum(cycle_counts) if np.sum(cycle_counts) > 0 else 0,
                'Total_Damage': damage,
                'Weighted_Damage': weighted_damage,
                'Annual_Damage': annual_damage,
                'Fatigue_Life_Years': fatigue_life
            })
            df_damage.to_csv(damage_file, index=False)
        
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
    
    def process_configuration(self, config_name: str, struts: List[int] = None,
                            conditions: List[FatigueCondition] = None,
                            save_intermediate: bool = True) -> Tuple[List[dict], dict]:
        """
        Process a complete configuration with proper file naming
        """
        if struts is None:
            struts = list(range(1, 9))
        
        if conditions is None:
            conditions = self.fatigue_conditions[:3]  # First 3 for testing
        
        logger.info(f"\nProcessing {config_name}")
        config = self.data_handler.configurations[config_name]
        logger.info(f"Description: {config.description}")
        logger.info(f"Weight: {config.weight}%")
        
        # Get file namer
        namer = self.namers[config_name]
        
        # Process all combinations
        results = []
        for condition in conditions:
            for strut_num in struts:
                logger.info(f"  Processing FC{condition.id:03d} - Strut{strut_num}")
                result = self.process_single_condition(
                    config_name, condition, strut_num, save_intermediate
                )
                if result:
                    results.append(result)
        
        # Create configuration summary
        if results:
            df_results = pd.DataFrame(results)
            
            # Save configuration summary with proper naming
            summary_file = namer.get_summary_filename()
            
            # Group by strut to find critical strut
            strut_summary = df_results.groupby('strut').agg({
                'annual_damage': 'sum',
                'fatigue_life_years': 'min'
            }).reset_index()
            
            # Find critical strut
            critical_strut_idx = strut_summary['fatigue_life_years'].idxmin()
            critical_strut = strut_summary.loc[critical_strut_idx, 'strut']
            min_life = strut_summary.loc[critical_strut_idx, 'fatigue_life_years']
            
            # Save configuration summary
            strut_summary.to_csv(summary_file, index=False)
            
            config_summary = {
                'weight_pct': config.weight,
                'critical_strut': f"Strut{critical_strut}",
                'min_fatigue_life': min_life,
                'strut_lives': {f"Strut{s}": life for s, life in 
                              zip(strut_summary['strut'], strut_summary['fatigue_life_years'])}
            }
            
            return results, config_summary
        
        return [], {}
    
    def run_complete_analysis(self, struts: List[int] = None, 
                            max_conditions: int = None,
                            save_intermediate: bool = True) -> Tuple[pd.DataFrame, dict]:
        """
        Run complete fatigue analysis with proper file naming
        """
        if struts is None:
            struts = [1, 2]  # Default to first 2 struts for testing
        
        # Limit conditions for testing
        conditions = self.fatigue_conditions
        if max_conditions:
            conditions = conditions[:max_conditions]
        
        all_results = []
        config_summaries = {}
        
        # Process each configuration
        for config_name in self.data_handler.configurations:
            config_path = self.data_handler.base_path / config_name
            if not config_path.exists():
                logger.warning(f"Skipping {config_name} - path does not exist")
                continue
            
            results, summary = self.process_configuration(
                config_name, struts, conditions, save_intermediate
            )
            
            if results:
                all_results.extend(results)
                config_summaries[config_name] = summary
        
        # Create overall summary
        if all_results:
            df_all = pd.DataFrame(all_results)
            
            # Save overall summary
            overall_namer = self.namers['overall']
            overall_file = overall_namer.get_overall_summary_filename()
            
            # Calculate weighted overall fatigue life
            weighted_life = 0
            for config_name, summary in config_summaries.items():
                weight = summary['weight_pct'] / 100.0
                min_life = summary['min_fatigue_life']
                if min_life != float('inf'):
                    weighted_life += weight / min_life
            
            overall_fatigue_life = 1 / weighted_life if weighted_life > 0 else float('inf')
            
            # Save overall summary
            overall_summary = pd.DataFrame([{
                'Overall_Fatigue_Life_Years': overall_fatigue_life,
                'Total_Configurations': len(config_summaries),
                'Total_Conditions': len(conditions),
                'Total_Struts': len(struts)
            }])
            overall_summary.to_csv(overall_file, index=False)
            
            # Save configuration comparison
            comparison_file = overall_namer.get_configuration_comparison_filename()
            comparison_data = []
            for config_name, summary in config_summaries.items():
                comparison_data.append({
                    'Configuration': config_name,
                    'Weight_Pct': summary['weight_pct'],
                    'Critical_Strut': summary['critical_strut'],
                    'Min_Fatigue_Life_Years': summary['min_fatigue_life']
                })
            
            df_comparison = pd.DataFrame(comparison_data)
            df_comparison.to_csv(comparison_file, index=False)
            
            logger.info(f"\nOverall Weighted Fatigue Life: {overall_fatigue_life:.2f} years")
            logger.info(f"Results saved with proper naming convention in {self.output_path}")
            
            return df_all, config_summaries
        
        return pd.DataFrame(), {}


def main(data_path=None):
    """Main execution with proper naming convention"""
    logger.info("=" * 60)
    logger.info("FATIGUE ANALYSIS WITH PROPER NAMING CONVENTION")
    logger.info("=" * 60)
    
    # Initialize handlers
    data_handler = ProductionDataHandler(base_path=data_path, sample_timesteps=1000)
    processor = IntegratedFatigueProcessorWithNaming(data_handler)
    
    # Run complete analysis
    df_results, config_summaries = processor.run_complete_analysis(
        struts=[1, 2],  # Process first 2 struts
        max_conditions=3,  # Process first 3 conditions
        save_intermediate=True  # Save all intermediate files
    )
    
    # Display summary
    for config_name, summary in config_summaries.items():
        logger.info(f"\n{config_name}:")
        logger.info(f"  Critical Strut: {summary['critical_strut']}")
        logger.info(f"  Min Fatigue Life: {summary['min_fatigue_life']:.2f} years")
        for strut, life in summary['strut_lives'].items():
            logger.info(f"    {strut}: {life:.2f} years")
    
    return df_results, config_summaries


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    results, summary = main("sample_data")