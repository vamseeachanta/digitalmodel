#!/usr/bin/env python3
"""
Visualization Module for Fatigue Analysis Results

This module provides comprehensive visualization capabilities for fatigue analysis,
including damage histograms, S-N curves, fatigue life plots, and configuration comparisons.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
import json
import logging

# Set style for professional plots
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")

logger = logging.getLogger(__name__)


class FatigueVisualizer:
    """Create visualizations for fatigue analysis results"""
    
    def __init__(self, output_path: str = "output/visualizations"):
        """
        Initialize the visualizer
        
        Args:
            output_path: Directory for saving visualization outputs
        """
        self.output_path = Path(output_path)
        self.output_path.mkdir(parents=True, exist_ok=True)
        
        # Set default figure parameters
        self.fig_size = (12, 8)
        self.dpi = 100
        
    def plot_sn_curve(self, sn_curve_params: Dict[str, Any], 
                     stress_ranges: Optional[np.ndarray] = None,
                     save_file: str = "sn_curve.png") -> plt.Figure:
        """
        Plot S-N curve with optional data points
        
        Args:
            sn_curve_params: S-N curve parameters dictionary
            stress_ranges: Optional stress ranges to overlay
            save_file: Filename for saving plot
            
        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=self.fig_size)
        
        # Generate S-N curve
        stress_values = np.logspace(1, 3, 100)  # 10 to 1000 MPa
        
        # Calculate cycles to failure
        if 'threshold' in sn_curve_params and sn_curve_params['threshold']:
            # Bi-linear S-N curve
            cycles = []
            for s in stress_values:
                if s <= sn_curve_params['threshold']:
                    n = 10**sn_curve_params['log_a2'] * (s ** -sn_curve_params['m2'])
                else:
                    n = 10**sn_curve_params['log_a1'] * (s ** -sn_curve_params['m1'])
                cycles.append(n)
            cycles = np.array(cycles)
        else:
            # Single slope S-N curve
            cycles = 10**sn_curve_params['log_a1'] * (stress_values ** -sn_curve_params['m1'])
        
        # Plot S-N curve
        ax.loglog(cycles, stress_values, 'b-', linewidth=2, 
                 label=f"{sn_curve_params.get('name', 'S-N Curve')}")
        
        # Overlay data points if provided
        if stress_ranges is not None and len(stress_ranges) > 0:
            # Calculate corresponding cycles for data points
            data_cycles = []
            for s in stress_ranges:
                if 'threshold' in sn_curve_params and sn_curve_params['threshold']:
                    if s <= sn_curve_params['threshold']:
                        n = 10**sn_curve_params['log_a2'] * (s ** -sn_curve_params['m2'])
                    else:
                        n = 10**sn_curve_params['log_a1'] * (s ** -sn_curve_params['m1'])
                else:
                    n = 10**sn_curve_params['log_a1'] * (s ** -sn_curve_params['m1'])
                data_cycles.append(n)
            
            ax.scatter(data_cycles, stress_ranges, c='red', alpha=0.5, 
                      s=30, label='Analysis Points')
        
        # Formatting
        ax.set_xlabel('Number of Cycles to Failure (N)', fontsize=12)
        ax.set_ylabel('Stress Range (MPa)', fontsize=12)
        ax.set_title('S-N Fatigue Curve', fontsize=14, fontweight='bold')
        ax.grid(True, which="both", ls="-", alpha=0.2)
        ax.legend(loc='upper right')
        
        # Add reference lines
        ax.axvline(x=1e6, color='gray', linestyle='--', alpha=0.5, label='1M cycles')
        ax.axvline(x=1e7, color='gray', linestyle='--', alpha=0.5, label='10M cycles')
        
        plt.tight_layout()
        
        # Save figure
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"S-N curve saved to {save_path}")
        
        return fig
    
    def plot_damage_histogram(self, results_df: pd.DataFrame,
                             config_name: str = "All",
                             save_file: str = "damage_histogram.png") -> plt.Figure:
        """
        Plot damage contribution histogram
        
        Args:
            results_df: DataFrame with fatigue analysis results
            config_name: Configuration name for filtering
            save_file: Filename for saving plot
            
        Returns:
            Matplotlib figure object
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
        
        # Filter data if specific config
        if config_name != "All" and 'config' in results_df.columns:
            data = results_df[results_df['config'] == config_name]
        else:
            data = results_df
        
        if len(data) == 0:
            logger.warning(f"No data found for config: {config_name}")
            return fig
        
        # Plot 1: Damage by stress range
        if 'max_stress_range' in data.columns and 'damage' in data.columns:
            stress_bins = np.linspace(0, data['max_stress_range'].max(), 20)
            data['stress_bin'] = pd.cut(data['max_stress_range'], bins=stress_bins)
            
            damage_by_stress = data.groupby('stress_bin')['damage'].sum()
            
            ax1.bar(range(len(damage_by_stress)), damage_by_stress.values, 
                   color='steelblue', alpha=0.7)
            ax1.set_xlabel('Stress Range Bin', fontsize=11)
            ax1.set_ylabel('Cumulative Damage', fontsize=11)
            ax1.set_title('Damage Distribution by Stress Range', fontsize=12)
            ax1.grid(True, alpha=0.3)
        
        # Plot 2: Damage by condition
        if 'condition_id' in data.columns:
            damage_by_condition = data.groupby('condition_id')['annual_damage'].sum()
            
            ax2.bar(damage_by_condition.index, damage_by_condition.values,
                   color='coral', alpha=0.7)
            ax2.set_xlabel('Fatigue Condition ID', fontsize=11)
            ax2.set_ylabel('Annual Damage', fontsize=11)
            ax2.set_title('Annual Damage by Condition', fontsize=12)
            ax2.grid(True, alpha=0.3)
        
        fig.suptitle(f'Damage Distribution Analysis - {config_name}', 
                    fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save figure
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"Damage histogram saved to {save_path}")
        
        return fig
    
    def plot_configuration_comparison(self, summary_data: Dict[str, Any],
                                     save_file: str = "config_comparison.png") -> plt.Figure:
        """
        Plot comparison between configurations
        
        Args:
            summary_data: Summary dictionary with configuration data
            save_file: Filename for saving plot
            
        Returns:
            Matplotlib figure object
        """
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))
        
        configs = summary_data.get('configurations', [])
        if not configs:
            logger.warning("No configuration data found")
            return fig
        
        # Extract data
        config_names = [c['config_name'] for c in configs]
        weights = [c['weight_pct'] for c in configs]
        fatigue_lives = [c['min_fatigue_life'] for c in configs]
        
        # Plot 1: Configuration weights (pie chart)
        ax1 = axes[0, 0]
        colors = plt.cm.Set3(range(len(config_names)))
        wedges, texts, autotexts = ax1.pie(weights, labels=config_names, 
                                           colors=colors, autopct='%1.1f%%',
                                           startangle=90)
        ax1.set_title('Operational Time Distribution', fontsize=12, fontweight='bold')
        
        # Plot 2: Fatigue life comparison (bar chart)
        ax2 = axes[0, 1]
        bars = ax2.bar(config_names, fatigue_lives, color=colors, alpha=0.7)
        ax2.set_xlabel('Configuration', fontsize=11)
        ax2.set_ylabel('Fatigue Life (years)', fontsize=11)
        ax2.set_title('Minimum Fatigue Life by Configuration', fontsize=12, fontweight='bold')
        ax2.grid(True, alpha=0.3)
        
        # Rotate x-labels if needed
        plt.setp(ax2.xaxis.get_majorticklabels(), rotation=45, ha='right')
        
        # Add value labels on bars
        for bar, life in zip(bars, fatigue_lives):
            height = bar.get_height()
            if height < float('inf'):
                ax2.text(bar.get_x() + bar.get_width()/2., height,
                        f'{height:.1f}', ha='center', va='bottom')
        
        # Plot 3: Strut-wise fatigue life heatmap
        ax3 = axes[1, 0]
        
        # Create strut life matrix
        strut_names = set()
        for config in configs:
            if 'strut_lives' in config:
                strut_names.update(config['strut_lives'].keys())
        
        strut_names = sorted(list(strut_names))
        
        if strut_names:
            life_matrix = []
            for config in configs:
                strut_lives = config.get('strut_lives', {})
                row = [strut_lives.get(strut, float('inf')) for strut in strut_names]
                # Cap infinite values for visualization
                row = [min(v, 1000) for v in row]
                life_matrix.append(row)
            
            life_matrix = np.array(life_matrix)
            
            im = ax3.imshow(life_matrix, cmap='RdYlGn', aspect='auto')
            ax3.set_xticks(range(len(strut_names)))
            ax3.set_yticks(range(len(config_names)))
            ax3.set_xticklabels(strut_names)
            ax3.set_yticklabels(config_names)
            ax3.set_xlabel('Strut', fontsize=11)
            ax3.set_ylabel('Configuration', fontsize=11)
            ax3.set_title('Fatigue Life Heatmap (years)', fontsize=12, fontweight='bold')
            
            # Add colorbar
            cbar = plt.colorbar(im, ax=ax3)
            cbar.set_label('Fatigue Life (years)', rotation=270, labelpad=20)
        
        # Plot 4: Overall summary
        ax4 = axes[1, 1]
        ax4.axis('off')
        
        # Create summary text
        overall_life = summary_data.get('overall_fatigue_life', 0)
        num_conditions = summary_data.get('num_conditions', 0)
        num_struts = summary_data.get('num_struts', 0)
        
        summary_text = f"""
        OVERALL FATIGUE ANALYSIS SUMMARY
        
        Weighted Overall Fatigue Life: {overall_life:.1f} years
        Number of Configurations: {len(configs)}
        Number of Fatigue Conditions: {num_conditions}
        Number of Struts Analyzed: {num_struts}
        
        Critical Configuration: {min(configs, key=lambda x: x.get('min_fatigue_life', float('inf')))['config_name']}
        Design Life Target: 20 years
        Safety Factor: {overall_life / 20:.2f}
        """
        
        ax4.text(0.1, 0.5, summary_text, fontsize=11, 
                verticalalignment='center', fontfamily='monospace')
        
        plt.suptitle('Fatigue Analysis Configuration Comparison', 
                    fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save figure
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"Configuration comparison saved to {save_path}")
        
        return fig
    
    def plot_rainflow_results(self, ranges: np.ndarray, counts: np.ndarray,
                            title: str = "Rainflow Counting Results",
                            save_file: str = "rainflow_results.png") -> plt.Figure:
        """
        Plot rainflow counting results
        
        Args:
            ranges: Array of stress/load ranges
            counts: Array of cycle counts
            title: Plot title
            save_file: Filename for saving plot
            
        Returns:
            Matplotlib figure object
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
        
        # Plot 1: Range-Count histogram
        ax1.bar(ranges, counts, width=0.8*(ranges[1]-ranges[0]) if len(ranges) > 1 else 1,
               color='teal', alpha=0.7, edgecolor='black')
        ax1.set_xlabel('Stress Range (MPa)', fontsize=11)
        ax1.set_ylabel('Number of Cycles', fontsize=11)
        ax1.set_title('Cycle Count Distribution', fontsize=12)
        ax1.grid(True, alpha=0.3)
        
        # Plot 2: Cumulative distribution
        cumulative = np.cumsum(counts)
        cumulative_pct = cumulative / cumulative[-1] * 100 if cumulative[-1] > 0 else cumulative
        
        ax2.plot(ranges, cumulative_pct, 'b-', linewidth=2, marker='o', markersize=4)
        ax2.set_xlabel('Stress Range (MPa)', fontsize=11)
        ax2.set_ylabel('Cumulative Percentage (%)', fontsize=11)
        ax2.set_title('Cumulative Cycle Distribution', fontsize=12)
        ax2.grid(True, alpha=0.3)
        ax2.set_ylim([0, 105])
        
        # Add 50% and 90% reference lines
        ax2.axhline(y=50, color='r', linestyle='--', alpha=0.5, label='50%')
        ax2.axhline(y=90, color='r', linestyle='--', alpha=0.5, label='90%')
        ax2.legend()
        
        fig.suptitle(title, fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save figure
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"Rainflow results saved to {save_path}")
        
        return fig
    
    def plot_time_series_with_cycles(self, time_series: np.ndarray,
                                    time: Optional[np.ndarray] = None,
                                    reversals: Optional[List[int]] = None,
                                    title: str = "Time Series with Cycles",
                                    save_file: str = "time_series_cycles.png") -> plt.Figure:
        """
        Plot time series with identified cycles
        
        Args:
            time_series: Stress/load time series
            time: Time array (optional)
            reversals: Indices of reversal points
            title: Plot title
            save_file: Filename for saving plot
            
        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=self.fig_size)
        
        if time is None:
            time = np.arange(len(time_series))
        
        # Plot time series
        ax.plot(time, time_series, 'b-', linewidth=1, alpha=0.7, label='Time Series')
        
        # Mark reversals if provided
        if reversals:
            reversal_times = [time[i] for i in reversals if i < len(time)]
            reversal_values = [time_series[i] for i in reversals if i < len(time_series)]
            ax.scatter(reversal_times, reversal_values, c='red', s=30, 
                      zorder=5, label='Reversals')
        
        ax.set_xlabel('Time (s)', fontsize=11)
        ax.set_ylabel('Stress/Load', fontsize=11)
        ax.set_title(title, fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        plt.tight_layout()
        
        # Save figure
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"Time series plot saved to {save_path}")
        
        return fig
    
    def create_dashboard(self, results_file: str, summary_file: str,
                        save_file: str = "fatigue_dashboard.png"):
        """
        Create comprehensive dashboard from analysis results
        
        Args:
            results_file: Path to detailed results CSV
            summary_file: Path to summary JSON
            save_file: Filename for saving dashboard
        """
        # Load data
        try:
            results_df = pd.read_csv(results_file)
            with open(summary_file, 'r') as f:
                summary_data = json.load(f)
        except Exception as e:
            logger.error(f"Error loading data files: {e}")
            return None
        
        # Create multi-panel dashboard
        fig = plt.figure(figsize=(20, 12))
        gs = fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)
        
        # Panel 1: Configuration weights
        ax1 = fig.add_subplot(gs[0, 0])
        configs = summary_data.get('configurations', [])
        if configs:
            names = [c['config_name'] for c in configs]
            weights = [c['weight_pct'] for c in configs]
            colors = plt.cm.Set3(range(len(names)))
            ax1.pie(weights, labels=names, autopct='%1.1f%%', colors=colors)
            ax1.set_title('Configuration Weights', fontsize=11, fontweight='bold')
        
        # Panel 2: Fatigue lives
        ax2 = fig.add_subplot(gs[0, 1])
        if configs:
            lives = [c['min_fatigue_life'] for c in configs]
            bars = ax2.bar(names, lives, color=colors, alpha=0.7)
            ax2.set_ylabel('Fatigue Life (years)', fontsize=10)
            ax2.set_title('Minimum Fatigue Lives', fontsize=11, fontweight='bold')
            plt.setp(ax2.xaxis.get_majorticklabels(), rotation=45, ha='right')
        
        # Panel 3: Damage by condition
        ax3 = fig.add_subplot(gs[0, 2])
        if 'condition_id' in results_df.columns:
            damage_by_cond = results_df.groupby('condition_id')['annual_damage'].mean()
            ax3.plot(damage_by_cond.index, damage_by_cond.values, 'o-', color='coral')
            ax3.set_xlabel('Condition ID', fontsize=10)
            ax3.set_ylabel('Mean Annual Damage', fontsize=10)
            ax3.set_title('Damage by Condition', fontsize=11, fontweight='bold')
            ax3.grid(True, alpha=0.3)
        
        # Panel 4: Stress range distribution
        ax4 = fig.add_subplot(gs[1, :2])
        if 'max_stress_range' in results_df.columns:
            ax4.hist(results_df['max_stress_range'], bins=30, color='steelblue', 
                    alpha=0.7, edgecolor='black')
            ax4.set_xlabel('Maximum Stress Range (MPa)', fontsize=10)
            ax4.set_ylabel('Frequency', fontsize=10)
            ax4.set_title('Stress Range Distribution', fontsize=11, fontweight='bold')
            ax4.grid(True, alpha=0.3)
        
        # Panel 5: Summary statistics
        ax5 = fig.add_subplot(gs[1, 2])
        ax5.axis('off')
        
        overall_life = summary_data.get('overall_fatigue_life', 0)
        num_results = summary_data.get('total_results', 0)
        
        stats_text = f"""
        SUMMARY STATISTICS
        
        Overall Life: {overall_life:.1f} yrs
        Total Results: {num_results}
        Configs: {len(configs)}
        
        Mean Damage: {results_df['damage'].mean():.3e}
        Max Stress: {results_df['max_stress_range'].max():.1f} MPa
        """
        
        ax5.text(0.1, 0.5, stats_text, fontsize=10, 
                verticalalignment='center', fontfamily='monospace')
        
        # Panel 6: Damage vs stress scatter
        ax6 = fig.add_subplot(gs[2, :])
        if 'max_stress_range' in results_df.columns and 'damage' in results_df.columns:
            for config in results_df['config'].unique():
                config_data = results_df[results_df['config'] == config]
                ax6.scatter(config_data['max_stress_range'], config_data['damage'],
                          alpha=0.5, label=config, s=20)
            ax6.set_xlabel('Maximum Stress Range (MPa)', fontsize=10)
            ax6.set_ylabel('Damage', fontsize=10)
            ax6.set_title('Damage vs Stress Range', fontsize=11, fontweight='bold')
            ax6.set_yscale('log')
            ax6.grid(True, alpha=0.3)
            ax6.legend(loc='best', fontsize=8)
        
        # Main title
        fig.suptitle('Fatigue Analysis Dashboard', fontsize=16, fontweight='bold')
        
        # Save dashboard
        save_path = self.output_path / save_file
        plt.savefig(save_path, dpi=self.dpi, bbox_inches='tight')
        logger.info(f"Dashboard saved to {save_path}")
        
        return fig


def generate_all_visualizations(results_path: str = "output",
                               viz_path: str = "output/visualizations"):
    """
    Generate all standard visualizations from fatigue analysis results
    
    Args:
        results_path: Path to results directory
        viz_path: Path for visualization outputs
    """
    visualizer = FatigueVisualizer(viz_path)
    
    results_path = Path(results_path)
    
    # Check for required files
    results_file = results_path / "fatigue_analysis_results.csv"
    summary_file = results_path / "analysis_summary.json"
    
    if not results_file.exists():
        results_file = results_path / "integrated_fatigue_results.csv"
    
    if not summary_file.exists():
        summary_file = results_path / "configuration_summary.json"
    
    if not results_file.exists() or not summary_file.exists():
        logger.error(f"Required files not found in {results_path}")
        return
    
    # Load data
    try:
        results_df = pd.read_csv(results_file)
        with open(summary_file, 'r') as f:
            summary_data = json.load(f)
        
        logger.info("Generating visualizations...")
        
        # 1. S-N Curve
        sn_params = {
            'name': 'ABS E in Air',
            'log_a1': 12.018,
            'm1': 3.0,
            'log_a2': 11.170,
            'm2': 5.0,
            'threshold': 1e6
        }
        
        stress_ranges = results_df['max_stress_range'].values if 'max_stress_range' in results_df.columns else None
        visualizer.plot_sn_curve(sn_params, stress_ranges)
        
        # 2. Configuration comparison
        visualizer.plot_configuration_comparison(summary_data)
        
        # 3. Damage histograms for each configuration
        if 'config' in results_df.columns:
            for config in results_df['config'].unique():
                visualizer.plot_damage_histogram(results_df, config, 
                                               f"damage_histogram_{config}.png")
        
        # 4. Overall dashboard
        visualizer.create_dashboard(str(results_file), str(summary_file))
        
        logger.info(f"All visualizations saved to {viz_path}")
        
    except Exception as e:
        logger.error(f"Error generating visualizations: {e}")


if __name__ == "__main__":
    # Example usage
    import sys
    
    if len(sys.argv) > 1:
        results_path = sys.argv[1]
    else:
        results_path = "output"
    
    generate_all_visualizations(results_path)