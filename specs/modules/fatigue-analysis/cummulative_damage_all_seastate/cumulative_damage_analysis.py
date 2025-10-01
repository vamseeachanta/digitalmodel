#!/usr/bin/env python3
"""
Cumulative Damage Analysis Module
==================================
Calculates fatigue life by combining stress rainflow damage rates with sea state occurrence probabilities.
Uses Miner's rule for damage accumulation.

Author: Engineering Analysis Team
Date: 2025-01-25
Version: 1.0.0
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import yaml
import logging
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from concurrent.futures import ProcessPoolExecutor, as_completed
import warnings
warnings.filterwarnings('ignore')

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('cumulative_damage_analysis.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

@dataclass
class DamageResult:
    """Container for damage calculation results"""
    configuration: str
    strut: int
    location: str
    fatigue_conditions: List[str]
    damage_rates: np.ndarray
    weighted_damages: np.ndarray
    total_damage_rate: float
    fatigue_life: float
    contributions: np.ndarray
    critical_fc: str
    critical_contribution: float


class CumulativeDamageAnalyzer:
    """Main class for cumulative fatigue damage analysis"""
    
    def __init__(self, config_file: str):
        """
        Initialize the analyzer with configuration file
        
        Args:
            config_file: Path to YAML configuration file
        """
        self.config = self._load_config(config_file)
        self.setup_directories()
        self.fatigue_conditions_df = None
        self.results = []
        
    def _load_config(self, config_file: str) -> Dict:
        """Load configuration from YAML file"""
        logger.info(f"Loading configuration from {config_file}")
        with open(config_file, 'r') as f:
            return yaml.safe_load(f)
    
    def setup_directories(self):
        """Create output directories if they don't exist"""
        base_output = Path(self.config['output']['base_folder'])
        base_output.mkdir(parents=True, exist_ok=True)
        
        if self.config['output']['individual_files']['enabled']:
            (base_output / self.config['output']['individual_files']['folder']).mkdir(exist_ok=True)
        
        if self.config['output']['plots']['enabled']:
            (base_output / self.config['output']['plots']['folder']).mkdir(exist_ok=True)
    
    def load_fatigue_conditions(self) -> pd.DataFrame:
        """Load fatigue condition occurrence data"""
        fc_file = self.config['input']['fatigue_conditions']['file']
        logger.info(f"Loading fatigue conditions from {fc_file}")
        
        df = pd.read_csv(fc_file)
        # Convert occurrence percentage to fraction
        df['occurrence_fraction'] = df['Occurrence (%)'] / 100.0
        
        # Validate occurrence sum
        total_occurrence = df['Occurrence (%)'].sum()
        logger.info(f"Total occurrence probability: {total_occurrence:.2f}%")
        
        if self.config['quality_checks']['enabled']:
            tolerance = 0.1
            if abs(total_occurrence - 100.0) > tolerance:
                logger.warning(f"Occurrence probabilities sum to {total_occurrence:.2f}%, expected ~100%")
        
        self.fatigue_conditions_df = df
        return df
    
    def get_available_files(self) -> List[Tuple[str, int, str]]:
        """
        Scan input folder for available damage rate files
        
        Returns:
            List of tuples (configuration, strut, location)
        """
        base_folder = Path(self.config['input']['base_folder'])
        input_folder = base_folder / self.config['input']['damage_rates']['folder']
        pattern = self.config['input']['damage_rates']['pattern']
        
        available = []
        for file_path in input_folder.glob("*.csv"):
            filename = file_path.name
            # Parse filename to extract components
            if '_damage_rate.csv' in filename:
                parts = filename.replace('_damage_rate.csv', '').split('_')
                
                # Find FC index
                fc_idx = next((i for i, p in enumerate(parts) if p.startswith('FC')), None)
                if fc_idx is not None:
                    # Configuration is everything before FC
                    config = '_'.join(parts[:fc_idx])
                    # Extract FC, Strut, and location
                    fc = parts[fc_idx]
                    strut = int(parts[fc_idx+1].replace('Strut', ''))
                    location = parts[fc_idx+2]
                    
                    available.append((config, strut, location, fc))
        
        # Get unique combinations
        unique_combinations = set()
        for config, strut, location, fc in available:
            unique_combinations.add((config, strut, location))
        
        logger.info(f"Found {len(unique_combinations)} unique config/strut/location combinations")
        return sorted(list(unique_combinations))
    
    def load_damage_data(self, config: str, strut: int, location: str, fc: str) -> Optional[pd.DataFrame]:
        """
        Load damage rate data for specific combination
        
        Args:
            config: Configuration name
            strut: Strut number
            location: Location ID
            fc: Fatigue condition
            
        Returns:
            DataFrame with damage data or None if file doesn't exist
        """
        base_folder = Path(self.config['input']['base_folder'])
        input_folder = base_folder / self.config['input']['damage_rates']['folder']
        filename = f"{config}_{fc}_Strut{strut}_{location}_damage_rate.csv"
        file_path = input_folder / filename
        
        if file_path.exists():
            return pd.read_csv(file_path)
        else:
            if self.config['processing']['missing_data']['log_missing']:
                logger.debug(f"Missing file: {filename}")
            return None
    
    def calculate_cumulative_damage(self, config: str, strut: int, location: str) -> Optional[DamageResult]:
        """
        Calculate cumulative damage for a specific configuration/strut/location
        
        Args:
            config: Configuration name
            strut: Strut number
            location: Location ID
            
        Returns:
            DamageResult object or None if calculation fails
        """
        logger.info(f"Processing: {config} - Strut{strut} - {location}")
        
        if self.fatigue_conditions_df is None:
            self.load_fatigue_conditions()
        
        fatigue_conditions = []
        damage_rates = []
        weighted_damages = []
        
        # Process each fatigue condition
        for _, fc_row in self.fatigue_conditions_df.iterrows():
            fc_id = fc_row['Row']
            occurrence = fc_row['occurrence_fraction']
            
            # Load damage data for this FC
            damage_df = self.load_damage_data(config, strut, location, fc_id)
            
            if damage_df is not None:
                # Get total damage rate for this FC
                total_damage = damage_df['damage_rate_per_year'].sum()
                
                # Apply occurrence weighting
                weighted_damage = total_damage * occurrence
                
                fatigue_conditions.append(fc_id)
                damage_rates.append(total_damage)
                weighted_damages.append(weighted_damage)
            else:
                # Handle missing data based on configuration
                if self.config['processing']['missing_data']['handling'] == 'skip':
                    continue
                else:
                    fatigue_conditions.append(fc_id)
                    damage_rates.append(0.0)
                    weighted_damages.append(0.0)
        
        if not fatigue_conditions:
            logger.warning(f"No data found for {config} - Strut{strut} - {location}")
            return None
        
        # Convert to arrays
        damage_rates = np.array(damage_rates)
        weighted_damages = np.array(weighted_damages)
        
        # Calculate total damage rate (Miner's rule)
        total_damage_rate = weighted_damages.sum()
        
        # Calculate fatigue life
        if total_damage_rate > 0:
            fatigue_life = 1.0 / total_damage_rate
        else:
            fatigue_life = np.inf
        
        # Calculate contributions
        if total_damage_rate > 0:
            contributions = (weighted_damages / total_damage_rate) * 100
        else:
            contributions = np.zeros_like(weighted_damages)
        
        # Find critical FC
        critical_idx = np.argmax(weighted_damages)
        critical_fc = fatigue_conditions[critical_idx] if fatigue_conditions else "N/A"
        critical_contribution = contributions[critical_idx] if len(contributions) > 0 else 0.0
        
        # Quality checks
        if self.config['quality_checks']['enabled']:
            self._perform_quality_checks(total_damage_rate, fatigue_life)
        
        return DamageResult(
            configuration=config,
            strut=strut,
            location=location,
            fatigue_conditions=fatigue_conditions,
            damage_rates=damage_rates,
            weighted_damages=weighted_damages,
            total_damage_rate=total_damage_rate,
            fatigue_life=fatigue_life,
            contributions=contributions,
            critical_fc=critical_fc,
            critical_contribution=critical_contribution
        )
    
    def _perform_quality_checks(self, damage_rate: float, fatigue_life: float):
        """Perform quality checks on calculated values"""
        for check in self.config['quality_checks']['checks']:
            if check['name'] == 'damage_rate_bounds':
                if not (check['min_value'] <= damage_rate <= check['max_value']):
                    logger.warning(f"Damage rate {damage_rate:.6f} outside bounds [{check['min_value']}, {check['max_value']}]")
            
            elif check['name'] == 'fatigue_life_bounds':
                if not (check['min_years'] <= fatigue_life <= check['max_years']):
                    logger.warning(f"Fatigue life {fatigue_life:.1f} years outside bounds [{check['min_years']}, {check['max_years']}]")
    
    def save_individual_result(self, result: DamageResult):
        """Save individual result to CSV file"""
        if not self.config['output']['individual_files']['enabled']:
            return
        
        output_folder = Path(self.config['output']['base_folder']) / self.config['output']['individual_files']['folder']
        filename = f"{result.configuration}_FC_all_Strut{result.strut}_{result.location}_fatigue_life.csv"
        file_path = output_folder / filename
        
        # Create DataFrame for output
        data = []
        for i, fc in enumerate(result.fatigue_conditions):
            fc_data = self.fatigue_conditions_df[self.fatigue_conditions_df['Row'] == fc].iloc[0]
            data.append({
                'fatigue_condition': fc,
                'occurrence_percent': fc_data['Occurrence (%)'],
                'damage_rate_annual': result.damage_rates[i],
                'weighted_damage_rate': result.weighted_damages[i],
                'contribution_percent': result.contributions[i]
            })
        
        df = pd.DataFrame(data)
        
        # Add summary rows
        summary = pd.DataFrame([
            {'fatigue_condition': 'TOTAL', 'occurrence_percent': 100.0, 
             'damage_rate_annual': np.nan, 'weighted_damage_rate': result.total_damage_rate,
             'contribution_percent': 100.0},
            {'fatigue_condition': 'FATIGUE_LIFE_YEARS', 'occurrence_percent': np.nan,
             'damage_rate_annual': np.nan, 'weighted_damage_rate': np.nan,
             'contribution_percent': result.fatigue_life}
        ])
        
        df = pd.concat([df, summary], ignore_index=True)
        df.to_csv(file_path, index=False)
        logger.info(f"Saved individual result to {file_path}")
    
    def create_individual_plots(self, result: DamageResult):
        """Create plots for individual result"""
        if not self.config['output']['plots']['enabled'] or not self.config['output']['plots']['individual']['enabled']:
            return
        
        # Skip plotting if there's no damage (infinite life)
        if result.total_damage_rate == 0 or np.isinf(result.fatigue_life):
            return
        
        output_folder = Path(self.config['output']['base_folder']) / self.config['output']['plots']['folder']
        
        for plot_config in self.config['output']['plots']['individual']['types']:
            if plot_config['type'] == 'bar_chart':
                self._create_damage_contribution_bar(result, output_folder)
            elif plot_config['type'] == 'stacked_bar':
                self._create_percentage_breakdown(result, output_folder)
    
    def _create_damage_contribution_bar(self, result: DamageResult, output_folder: Path):
        """Create bar chart showing damage contribution by FC"""
        fig, ax1 = plt.subplots(figsize=(12, 6))
        
        x = np.arange(len(result.fatigue_conditions))
        
        # Bar chart for weighted damage
        bars = ax1.bar(x, result.weighted_damages, color='steelblue', alpha=0.7, label='Weighted Damage Rate')
        ax1.set_xlabel('Fatigue Condition')
        ax1.set_ylabel('Weighted Damage Rate (1/year)', color='steelblue')
        ax1.set_xticks(x)
        ax1.set_xticklabels(result.fatigue_conditions, rotation=45)
        ax1.tick_params(axis='y', labelcolor='steelblue')
        
        # Add cumulative damage line
        ax2 = ax1.twinx()
        cumulative = np.cumsum(result.weighted_damages)
        ax2.plot(x, cumulative, 'r-', marker='o', label='Cumulative Damage', linewidth=2)
        ax2.set_ylabel('Cumulative Damage Rate (1/year)', color='red')
        ax2.tick_params(axis='y', labelcolor='red')
        
        # Add value labels on bars
        for bar, value in zip(bars, result.weighted_damages):
            if value > 0:
                height = bar.get_height()
                ax1.text(bar.get_x() + bar.get_width()/2., height,
                        f'{value:.2e}', ha='center', va='bottom', fontsize=8)
        
        plt.title(f'Damage Contribution by Fatigue Condition\n{result.configuration} - Strut {result.strut} - {result.location}\nFatigue Life: {result.fatigue_life:.1f} years')
        
        # Add legends
        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper left')
        
        plt.tight_layout()
        filename = f"{result.configuration}_Strut{result.strut}_{result.location}_damage_contribution.png"
        plt.savefig(output_folder / filename, dpi=300, bbox_inches='tight')
        plt.close()
    
    def _create_percentage_breakdown(self, result: DamageResult, output_folder: Path):
        """Create stacked bar chart showing percentage contribution"""
        fig, ax = plt.subplots(figsize=(10, 8))
        
        # Filter out zero contributions for cleaner visualization
        non_zero_idx = result.contributions > 0.1  # Only show contributions > 0.1%
        filtered_fc = [fc for i, fc in enumerate(result.fatigue_conditions) if non_zero_idx[i]]
        filtered_contributions = result.contributions[non_zero_idx]
        
        # Create pie chart for better visualization of percentages
        colors = plt.cm.Set3(np.linspace(0, 1, len(filtered_fc)))
        wedges, texts, autotexts = ax.pie(filtered_contributions, labels=filtered_fc, 
                                           colors=colors, autopct='%1.1f%%',
                                           startangle=90)
        
        # Beautify the plot
        for autotext in autotexts:
            autotext.set_color('white')
            autotext.set_fontweight('bold')
            autotext.set_fontsize(9)
        
        plt.title(f'Damage Contribution Breakdown\n{result.configuration} - Strut {result.strut} - {result.location}\nCritical FC: {result.critical_fc} ({result.critical_contribution:.1f}%)')
        
        filename = f"{result.configuration}_Strut{result.strut}_{result.location}_percentage_breakdown.png"
        plt.savefig(output_folder / filename, dpi=300, bbox_inches='tight')
        plt.close()
    
    def create_aggregated_plots(self, results_df: pd.DataFrame):
        """Create aggregated plots for all results"""
        if not self.config['output']['plots']['enabled'] or not self.config['output']['plots']['aggregated']['enabled']:
            return
        
        output_folder = Path(self.config['output']['base_folder']) / self.config['output']['plots']['folder']
        
        for plot_config in self.config['output']['plots']['aggregated']['types']:
            if plot_config['type'] == 'heatmap':
                self._create_fatigue_life_heatmap(results_df, output_folder)
            elif plot_config['type'] == 'grouped_bar':
                self._create_fatigue_life_comparison(results_df, output_folder)
            elif plot_config['type'] == 'line_plot':
                self._create_damage_trends(results_df, output_folder)
    
    def _create_fatigue_life_heatmap(self, results_df: pd.DataFrame, output_folder: Path):
        """Create heatmap of fatigue life by strut and location"""
        for config in results_df['configuration'].unique():
            config_df = results_df[results_df['configuration'] == config]
            
            # Pivot data for heatmap
            pivot_table = config_df.pivot(index='strut', columns='location', values='fatigue_life_years')
            
            plt.figure(figsize=(12, 8))
            sns.heatmap(pivot_table, annot=True, fmt='.1f', cmap='RdYlGn', 
                       cbar_kws={'label': 'Fatigue Life (years)'})
            plt.title(f'Fatigue Life Heatmap - {config}')
            plt.xlabel('Location')
            plt.ylabel('Strut')
            
            filename = f"{config}_fatigue_life_heatmap.png"
            plt.savefig(output_folder / filename, dpi=300, bbox_inches='tight')
            plt.close()
    
    def _create_fatigue_life_comparison(self, results_df: pd.DataFrame, output_folder: Path):
        """Create grouped bar chart comparing fatigue life"""
        for config in results_df['configuration'].unique():
            config_df = results_df[results_df['configuration'] == config]
            
            fig, ax = plt.subplots(figsize=(14, 6))
            
            # Group by strut
            struts = sorted(config_df['strut'].unique())
            locations = sorted(config_df['location'].unique())
            
            x = np.arange(len(struts))
            width = 0.8 / len(locations)
            
            colors = plt.cm.Set2(np.linspace(0, 1, len(locations)))
            
            for i, location in enumerate(locations):
                loc_data = config_df[config_df['location'] == location]
                fatigue_lives = []
                for strut in struts:
                    strut_loc_data = loc_data[loc_data['strut'] == strut]
                    if not strut_loc_data.empty:
                        fatigue_lives.append(strut_loc_data['fatigue_life_years'].values[0])
                    else:
                        fatigue_lives.append(0)
                
                offset = (i - len(locations)/2 + 0.5) * width
                bars = ax.bar(x + offset, fatigue_lives, width, label=location, color=colors[i])
                
                # Add value labels
                for bar in bars:
                    height = bar.get_height()
                    if height > 0:
                        ax.text(bar.get_x() + bar.get_width()/2., height,
                               f'{height:.0f}', ha='center', va='bottom', fontsize=8)
            
            ax.set_xlabel('Strut Number')
            ax.set_ylabel('Fatigue Life (years)')
            ax.set_title(f'Fatigue Life Comparison by Strut and Location - {config}')
            ax.set_xticks(x)
            ax.set_xticklabels([f'Strut {s}' for s in struts])
            ax.legend(title='Location', bbox_to_anchor=(1.05, 1), loc='upper left')
            ax.grid(axis='y', alpha=0.3)
            
            plt.tight_layout()
            filename = f"{config}_fatigue_life_comparison.png"
            plt.savefig(output_folder / filename, dpi=300, bbox_inches='tight')
            plt.close()
    
    def _create_damage_trends(self, results_df: pd.DataFrame, output_folder: Path):
        """Create line plot showing damage rate trends"""
        for config in results_df['configuration'].unique():
            config_df = results_df[results_df['configuration'] == config]
            
            fig, ax = plt.subplots(figsize=(12, 6))
            
            locations = sorted(config_df['location'].unique())
            colors = plt.cm.tab10(np.linspace(0, 1, len(locations)))
            
            for i, location in enumerate(locations):
                loc_data = config_df[config_df['location'] == location].sort_values('strut')
                ax.plot(loc_data['strut'], loc_data['total_damage_rate'], 
                       marker='o', label=location, color=colors[i], linewidth=2)
            
            ax.set_xlabel('Strut Number')
            ax.set_ylabel('Total Damage Rate (1/year)')
            ax.set_title(f'Damage Rate Trends Across Struts - {config}')
            ax.legend(title='Location', bbox_to_anchor=(1.05, 1), loc='upper left')
            ax.grid(True, alpha=0.3)
            ax.set_yscale('log')
            
            plt.tight_layout()
            filename = f"{config}_damage_rate_trends.png"
            plt.savefig(output_folder / filename, dpi=300, bbox_inches='tight')
            plt.close()
    
    def save_summary(self, results: List[DamageResult]):
        """Save summary of all results"""
        if not self.config['output']['summary']['enabled']:
            return
        
        output_file = Path(self.config['output']['base_folder']) / self.config['output']['summary']['filename']
        
        summary_data = []
        for result in results:
            summary_data.append({
                'configuration': result.configuration,
                'strut': result.strut,
                'location': result.location,
                'total_damage_rate': result.total_damage_rate,
                'fatigue_life_years': result.fatigue_life,
                'critical_fc': result.critical_fc,
                'critical_fc_contribution': result.critical_contribution
            })
        
        df = pd.DataFrame(summary_data)
        df = df.sort_values(['configuration', 'strut', 'location'])
        df.to_csv(output_file, index=False)
        logger.info(f"Saved summary to {output_file}")
        
        # Generate statistics
        logger.info("\n=== Analysis Summary ===")
        logger.info(f"Total combinations analyzed: {len(results)}")
        logger.info(f"Average fatigue life: {df['fatigue_life_years'].mean():.1f} years")
        logger.info(f"Minimum fatigue life: {df['fatigue_life_years'].min():.1f} years")
        logger.info(f"Maximum fatigue life: {df['fatigue_life_years'].max():.1f} years")
        
        # Find critical components
        critical = df.nsmallest(5, 'fatigue_life_years')
        logger.info("\nCritical components (lowest fatigue life):")
        for _, row in critical.iterrows():
            logger.info(f"  {row['configuration']} - Strut {row['strut']} - {row['location']}: {row['fatigue_life_years']:.1f} years")
        
        return df
    
    def generate_report(self, results_df: pd.DataFrame):
        """Generate markdown report"""
        if not self.config['reporting']['generate_report']:
            return
        
        report_file = Path(self.config['output']['base_folder']) / 'analysis_report.md'
        
        with open(report_file, 'w') as f:
            f.write("# Cumulative Fatigue Damage Analysis Report\n\n")
            f.write(f"**Date:** {pd.Timestamp.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"**Version:** {self.config['analysis']['version']}\n\n")
            
            # Executive Summary
            f.write("## Executive Summary\n\n")
            f.write(f"- **Total Combinations Analyzed:** {len(results_df)}\n")
            f.write(f"- **Average Fatigue Life:** {results_df['fatigue_life_years'].mean():.1f} years\n")
            f.write(f"- **Fatigue Life Range:** {results_df['fatigue_life_years'].min():.1f} - {results_df['fatigue_life_years'].max():.1f} years\n")
            f.write(f"- **Standard Deviation:** {results_df['fatigue_life_years'].std():.1f} years\n\n")
            
            # Methodology
            f.write("## Methodology\n\n")
            f.write("This analysis uses Miner's rule for cumulative damage calculation:\n")
            f.write("1. Individual damage rates from stress rainflow analysis\n")
            f.write("2. Weighted by sea state occurrence probabilities\n")
            f.write("3. Summed to get total annual damage rate\n")
            f.write("4. Fatigue life = 1 / total damage rate\n\n")
            
            # Results Overview
            f.write("## Results Overview\n\n")
            
            # By configuration
            f.write("### By Configuration\n\n")
            config_summary = results_df.groupby('configuration').agg({
                'fatigue_life_years': ['mean', 'min', 'max', 'std']
            }).round(1)
            f.write(config_summary.to_markdown())
            f.write("\n\n")
            
            # Critical Findings
            f.write("## Critical Findings\n\n")
            f.write("### Components with Shortest Fatigue Life\n\n")
            critical = results_df.nsmallest(10, 'fatigue_life_years')
            critical_table = critical[['configuration', 'strut', 'location', 
                                      'fatigue_life_years', 'critical_fc', 'critical_fc_contribution']]
            f.write(critical_table.to_markdown(index=False))
            f.write("\n\n")
            
            # Most influential fatigue conditions
            f.write("### Most Influential Fatigue Conditions\n\n")
            fc_influence = results_df.groupby('critical_fc')['critical_fc'].count().sort_values(ascending=False).head(5)
            f.write("| Fatigue Condition | Count as Critical |\n")
            f.write("|-------------------|------------------|\n")
            for fc, count in fc_influence.items():
                f.write(f"| {fc} | {count} |\n")
            f.write("\n")
            
            # Recommendations
            f.write("## Recommendations\n\n")
            f.write("1. **Priority Inspection:** Focus on components with fatigue life < 20 years\n")
            f.write("2. **Design Review:** Consider strengthening locations with high damage rates\n")
            f.write("3. **Monitoring:** Implement continuous monitoring for critical components\n")
            f.write("4. **Maintenance Planning:** Schedule based on calculated fatigue life\n")
            f.write("5. **Further Analysis:** Consider more detailed FEA for critical locations\n\n")
        
        logger.info(f"Report generated: {report_file}")
    
    def process_single_combination(self, combo: Tuple[str, int, str]) -> Optional[DamageResult]:
        """Process a single configuration/strut/location combination"""
        config, strut, location = combo
        result = self.calculate_cumulative_damage(config, strut, location)
        
        if result:
            self.save_individual_result(result)
            self.create_individual_plots(result)
        
        return result
    
    def run(self):
        """Main execution method"""
        logger.info("Starting cumulative damage analysis")
        
        # Load fatigue conditions
        self.load_fatigue_conditions()
        
        # Get available file combinations
        combinations = self.get_available_files()
        
        if not combinations:
            logger.error("No input files found")
            return
        
        # Process combinations
        results = []
        
        if self.config['execution']['parallel_processing']:
            max_workers = self.config['execution']['max_workers']
            with ProcessPoolExecutor(max_workers=max_workers) as executor:
                futures = {executor.submit(self.process_single_combination, combo): combo 
                          for combo in combinations}
                
                for future in as_completed(futures):
                    result = future.result()
                    if result:
                        results.append(result)
        else:
            for combo in combinations:
                result = self.process_single_combination(combo)
                if result:
                    results.append(result)
        
        # Save summary and create aggregated plots
        if results:
            summary_df = self.save_summary(results)
            self.create_aggregated_plots(summary_df)
            self.generate_report(summary_df)
        
        logger.info("Analysis complete")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Cumulative Fatigue Damage Analysis')
    parser.add_argument('--config', type=str, default='cumulative_damage_config.yml',
                       help='Configuration file path')
    parser.add_argument('--log-level', type=str, default='INFO',
                       choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
                       help='Logging level')
    
    args = parser.parse_args()
    
    # Update logging level
    logging.getLogger().setLevel(getattr(logging, args.log_level))
    
    # Run analysis
    analyzer = CumulativeDamageAnalyzer(args.config)
    analyzer.run()


if __name__ == "__main__":
    main()