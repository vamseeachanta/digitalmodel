"""
Overall Damage by Configuration Calculator

Calculates total fatigue damage across all configurations using Miner's rule.
Generates detailed reports and visualizations including polar plots.
"""

import os
import sys
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import yaml
from typing import Dict, List, Tuple


class OverallDamageCalculator:
    """Calculate overall damage by configuration using Miner's rule"""
    
    def __init__(self, config_path: str):
        """Initialize calculator with configuration file"""
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self.base_folder = Path(self.config['paths']['base_folder'])
        
        self.fatigue_data = None
        self.weightage_data = None
        self.results = {}
        
    def _load_config(self) -> dict:
        """Load YAML configuration file"""
        with open(self.config_path, 'r') as f:
            return yaml.safe_load(f)
    
    def load_data(self):
        """Load fatigue life summary and configuration weightage data"""
        print("Loading input data...")
        
        fatigue_path = self.base_folder / self.config['paths']['fatigue_life_summary']
        weightage_path = self.base_folder / self.config['paths']['config_weightage']
        
        self.fatigue_data = pd.read_csv(fatigue_path)
        self.weightage_data = pd.read_csv(weightage_path)
        
        self._validate_data()
        print(f"  Loaded {len(self.fatigue_data)} fatigue records")
        print(f"  Loaded {len(self.weightage_data)} configuration weights")
        
    def _validate_data(self):
        """Validate input data"""
        config_map = self.config['configuration_mapping']
        
        for config_with_suffix in self.weightage_data['Configuration']:
            if config_with_suffix not in config_map:
                raise ValueError(f"Configuration '{config_with_suffix}' not found in mapping")
            
            mapped_config = config_map[config_with_suffix]
            if mapped_config not in self.fatigue_data['configuration'].values:
                raise ValueError(f"Mapped configuration '{mapped_config}' not found in fatigue data")
        
        if self.config['processing']['validate_weightage_sum']:
            total_weight = self.weightage_data['Duration Weight (%)'].sum()
            tolerance = self.config['processing']['weightage_tolerance']
            if abs(total_weight - 100.0) > tolerance:
                raise ValueError(f"Duration weights sum to {total_weight}%, expected 100%")
        
        print("  Data validation passed")
    
    def calculate_weighted_damage(self) -> pd.DataFrame:
        """Calculate weighted damage rates for all configurations"""
        print("Calculating weighted damage rates...")
        
        results_list = []
        config_map = self.config['configuration_mapping']
        
        for _, weight_row in self.weightage_data.iterrows():
            config_with_suffix = weight_row['Configuration']
            config_name = config_map[config_with_suffix]
            duration_weight = weight_row['Duration Weight (%)']
            
            config_fatigue = self.fatigue_data[
                self.fatigue_data['configuration'] == config_name
            ].copy()
            
            config_fatigue['configuration_original'] = config_with_suffix
            config_fatigue['duration_weight_percent'] = duration_weight
            config_fatigue['weighted_damage_rate'] = (
                config_fatigue['total_damage_rate'] * duration_weight / 100.0
            )
            
            results_list.append(config_fatigue)
        
        results_df = pd.concat(results_list, ignore_index=True)
        
        print(f"  Calculated weighted damage for {len(results_df)} records")
        return results_df
    
    def apply_miners_rule(self, weighted_df: pd.DataFrame) -> pd.DataFrame:
        """Apply Miner's rule to calculate total damage by strut and location"""
        print("Applying Miner's rule...")
        
        total_damage = weighted_df.groupby(['strut', 'location']).agg({
            'weighted_damage_rate': 'sum'
        }).reset_index()
        
        total_damage.rename(
            columns={'weighted_damage_rate': 'total_damage_rate_miners_rule'},
            inplace=True
        )
        
        inf_threshold = float(self.config['processing']['inf_life_threshold'])
        total_damage['fatigue_life_years'] = np.where(
            total_damage['total_damage_rate_miners_rule'].values > inf_threshold,
            1.0 / total_damage['total_damage_rate_miners_rule'].values,
            np.inf
        )
        
        print(f"  Calculated total damage for {len(total_damage)} strut-location combinations")
        return total_damage
    
    def calculate_contributions(self, weighted_df: pd.DataFrame, 
                               total_damage: pd.DataFrame) -> pd.DataFrame:
        """Calculate contribution percentage for each configuration"""
        print("Calculating configuration contributions...")
        
        merged = weighted_df.merge(
            total_damage[['strut', 'location', 'total_damage_rate_miners_rule']],
            on=['strut', 'location']
        )
        
        merged['contribution_to_total_percent'] = np.where(
            merged['total_damage_rate_miners_rule'] > 0,
            (merged['weighted_damage_rate'] / merged['total_damage_rate_miners_rule']) * 100.0,
            0.0
        )
        
        return merged
    
    def identify_worst_case(self, total_damage: pd.DataFrame) -> Tuple[int, str, float]:
        """Identify worst-case strut-location (minimum fatigue life)"""
        finite_life = total_damage[total_damage['fatigue_life_years'] != np.inf]
        
        if len(finite_life) == 0:
            return None, None, np.inf
        
        worst_case = finite_life.loc[finite_life['fatigue_life_years'].idxmin()]
        return worst_case['strut'], worst_case['location'], worst_case['fatigue_life_years']
    
    def generate_outputs(self, weighted_df: pd.DataFrame, total_damage: pd.DataFrame,
                        contributions_df: pd.DataFrame):
        """Generate all CSV output files"""
        print("Generating output files...")
        
        output_folder = self.base_folder / self.config['paths']['output_folder']
        output_folder.mkdir(parents=True, exist_ok=True)
        
        decimal_prec = self.config['processing']['decimal_precision']
        
        damage_by_config = contributions_df[[
            'strut', 'location', 'configuration_original', 'total_damage_rate',
            'duration_weight_percent', 'weighted_damage_rate', 'contribution_to_total_percent'
        ]].copy()
        damage_by_config.rename(columns={'configuration_original': 'configuration'}, inplace=True)
        
        damage_by_config.to_csv(
            output_folder / 'damage_by_configuration.csv',
            index=False,
            float_format=f'%.{decimal_prec["damage_rate"]}f'
        )
        print(f"  Created: damage_by_configuration.csv")
        
        total_damage.to_csv(
            output_folder / 'overall_damage_summary.csv',
            index=False,
            float_format=f'%.{decimal_prec["damage_rate"]}f'
        )
        print(f"  Created: overall_damage_summary.csv")
        
        config_contrib = contributions_df.groupby('configuration_original').agg({
            'contribution_to_total_percent': ['mean', 'min', 'max']
        }).reset_index()
        config_contrib.columns = [
            'configuration', 'average_contribution_percent',
            'min_contribution_percent', 'max_contribution_percent'
        ]
        
        config_contrib.to_csv(
            output_folder / 'configuration_contribution.csv',
            index=False,
            float_format=f'%.{decimal_prec["contribution_percent"]}f'
        )
        print(f"  Created: configuration_contribution.csv")
        
        self.results['damage_by_config'] = damage_by_config
        self.results['total_damage'] = total_damage
        self.results['config_contrib'] = config_contrib
        self.results['contributions_df'] = contributions_df
    
    def create_plots(self):
        """Generate all visualization plots"""
        if not self.config['plotting']['enabled']:
            return
        
        print("Creating visualization plots...")
        output_folder = self.base_folder / self.config['paths']['output_folder']
        
        self._create_bar_cumulative_plot(output_folder)
        self._create_polar_summary_plot(output_folder)
        self._create_polar_worst_case_plot(output_folder)
        self._create_stacked_bar_plot(output_folder)
    
    def _create_bar_cumulative_plot(self, output_folder: Path):
        """Create bar chart with cumulative overlay"""
        config_contrib = self.results['config_contrib'].copy()
        
        fig, ax1 = plt.subplots(figsize=(12, 6))
        
        x_pos = np.arange(len(config_contrib))
        ax1.bar(x_pos, config_contrib['average_contribution_percent'], 
                alpha=0.7, color='steelblue', label='Average Contribution')
        ax1.set_xlabel('Configuration', fontsize=12)
        ax1.set_ylabel('Average Contribution (%)', fontsize=12, color='steelblue')
        ax1.set_xticks(x_pos)
        ax1.set_xticklabels(config_contrib['configuration'], rotation=45, ha='right')
        ax1.tick_params(axis='y', labelcolor='steelblue')
        
        ax2 = ax1.twinx()
        cumulative = config_contrib['average_contribution_percent'].cumsum()
        ax2.plot(x_pos, cumulative, color='red', marker='o', 
                linewidth=2, label='Cumulative')
        ax2.set_ylabel('Cumulative Contribution (%)', fontsize=12, color='red')
        ax2.tick_params(axis='y', labelcolor='red')
        
        plt.title('Damage Contribution by Configuration', fontsize=14, fontweight='bold')
        fig.legend(loc='upper left', bbox_to_anchor=(0.1, 0.9))
        plt.tight_layout()
        plt.savefig(output_folder / 'damage_by_config_bar.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"  Created: damage_by_config_bar.png")
    
    def _create_polar_summary_plot(self, output_folder: Path):
        """Create summary polar plot for average contributions"""
        config_contrib = self.results['config_contrib'].copy()
        
        fig = plt.figure(figsize=(10, 10))
        ax = fig.add_subplot(111, projection='polar')
        
        theta = np.linspace(0, 2 * np.pi, len(config_contrib), endpoint=False)
        radii = config_contrib['average_contribution_percent'].values
        width = 2 * np.pi / len(config_contrib) * 0.8
        
        bars = ax.bar(theta, radii, width=width, alpha=0.7)
        
        colors = plt.cm.viridis(np.linspace(0, 1, len(config_contrib)))
        for bar, color in zip(bars, colors):
            bar.set_facecolor(color)
        
        ax.set_xticks(theta)
        ax.set_xticklabels(config_contrib['configuration'], fontsize=10)
        ax.set_ylabel('Average Contribution (%)', fontsize=11)
        ax.set_title('Configuration Damage Contribution\n(Average Across All Strut-Locations)',
                    fontsize=13, fontweight='bold', pad=20)
        
        plt.tight_layout()
        plt.savefig(output_folder / 'damage_contribution_polar_summary.png', 
                   dpi=300, bbox_inches='tight')
        plt.close()
        print(f"  Created: damage_contribution_polar_summary.png")
    
    def _create_polar_worst_case_plot(self, output_folder: Path):
        """Create polar plot for worst-case strut-location"""
        total_damage = self.results['total_damage']
        contributions_df = self.results['contributions_df']
        
        worst_strut, worst_loc, worst_life = self.identify_worst_case(total_damage)
        
        if worst_strut is None:
            print("  Skipped: worst-case polar plot (no finite life cases)")
            return
        
        worst_case_data = contributions_df[
            (contributions_df['strut'] == worst_strut) &
            (contributions_df['location'] == worst_loc)
        ].copy()
        
        fig = plt.figure(figsize=(10, 10))
        ax = fig.add_subplot(111, projection='polar')
        
        theta = np.linspace(0, 2 * np.pi, len(worst_case_data), endpoint=False)
        radii = worst_case_data['contribution_to_total_percent'].values
        width = 2 * np.pi / len(worst_case_data) * 0.8
        
        bars = ax.bar(theta, radii, width=width, alpha=0.7)
        
        colors = plt.cm.plasma(np.linspace(0, 1, len(worst_case_data)))
        for bar, color in zip(bars, colors):
            bar.set_facecolor(color)
        
        ax.set_xticks(theta)
        ax.set_xticklabels(worst_case_data['configuration_original'], fontsize=10)
        ax.set_ylabel('Contribution (%)', fontsize=11)
        ax.set_title(f'Worst-Case Configuration Contributions\n' +
                    f'Strut {worst_strut}, {worst_loc} ' +
                    f'(Fatigue Life: {worst_life:.1f} years)',
                    fontsize=13, fontweight='bold', pad=20)
        
        plt.tight_layout()
        plt.savefig(output_folder / 'damage_contribution_polar_worst_case.png',
                   dpi=300, bbox_inches='tight')
        plt.close()
        print(f"  Created: damage_contribution_polar_worst_case.png")
    
    def _create_stacked_bar_plot(self, output_folder: Path):
        """Create stacked bar plot by strut-location"""
        contributions_df = self.results['contributions_df']
        
        contributions_df['strut_location'] = (
            contributions_df['strut'].astype(str) + '-' + contributions_df['location']
        )
        
        pivot_data = contributions_df.pivot_table(
            index='strut_location',
            columns='configuration_original',
            values='weighted_damage_rate',
            aggfunc='sum'
        )
        
        fig, ax = plt.subplots(figsize=(14, 8))
        pivot_data.plot(kind='bar', stacked=True, ax=ax, 
                       colormap='tab10', alpha=0.8)
        
        ax.set_xlabel('Strut-Location', fontsize=12)
        ax.set_ylabel('Weighted Damage Rate (1/year)', fontsize=12)
        ax.set_title('Damage Contribution by Configuration for Each Strut-Location',
                    fontsize=14, fontweight='bold')
        ax.legend(title='Configuration', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.xticks(rotation=45, ha='right')
        plt.tight_layout()
        plt.savefig(output_folder / 'damage_by_strut_location.png',
                   dpi=300, bbox_inches='tight')
        plt.close()
        print(f"  Created: damage_by_strut_location.png")
    
    def print_summary(self):
        """Print calculation summary"""
        total_damage = self.results['total_damage']
        
        print("\n" + "="*70)
        print("OVERALL DAMAGE CALCULATION SUMMARY")
        print("="*70)
        
        worst_strut, worst_loc, worst_life = self.identify_worst_case(total_damage)
        
        if worst_strut is not None:
            print(f"\nWorst-Case Scenario:")
            print(f"  Strut {worst_strut}, {worst_loc}")
            print(f"  Fatigue Life: {worst_life:.2f} years")
            
            worst_damage = total_damage[
                (total_damage['strut'] == worst_strut) &
                (total_damage['location'] == worst_loc)
            ]['total_damage_rate_miners_rule'].values[0]
            print(f"  Total Damage Rate: {worst_damage:.10f} 1/year")
        
        print(f"\nConfiguration Contributions:")
        config_contrib = self.results['config_contrib']
        for _, row in config_contrib.iterrows():
            print(f"  {row['configuration']}: {row['average_contribution_percent']:.2f}% " +
                  f"(range: {row['min_contribution_percent']:.2f}% - " +
                  f"{row['max_contribution_percent']:.2f}%)")
        
        print("\n" + "="*70)
    
    def run(self):
        """Execute full calculation workflow"""
        self.load_data()
        weighted_df = self.calculate_weighted_damage()
        total_damage = self.apply_miners_rule(weighted_df)
        contributions_df = self.calculate_contributions(weighted_df, total_damage)
        self.generate_outputs(weighted_df, total_damage, contributions_df)
        self.create_plots()
        self.print_summary()
        
        print("\nCalculation completed successfully!")


def main():
    """Main entry point"""
    if len(sys.argv) < 2:
        config_path = Path(__file__).parent / "input" / "config.yml"
    else:
        config_path = sys.argv[1]
    
    calculator = OverallDamageCalculator(config_path)
    calculator.run()


if __name__ == "__main__":
    main()