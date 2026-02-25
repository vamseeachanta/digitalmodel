"""
Comparative Analysis Module for Mooring System Configurations

This module provides comprehensive analysis capabilities for comparing multiple
mooring system configurations including pretension, stiffness, and force distributions.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import warnings
from datetime import datetime
from typing import Optional, Dict, List, Union

warnings.filterwarnings('ignore')

# Set style for better visualizations
try:
    plt.style.use('seaborn-v0_8-darkgrid')
except:
    plt.style.use('ggplot')


class MooringComparativeAnalysis:
    """
    Comprehensive comparative analysis for mooring system configurations.
    
    This class provides methods to:
    - Load and process mooring analysis CSV files
    - Perform statistical comparisons across configurations
    - Generate visualizations
    - Create detailed analysis reports
    """
    
    def __init__(self, csv_directory: Union[str, Path]):
        """
        Initialize the analysis with the directory containing CSV files.
        
        Args:
            csv_directory: Path to directory containing analysis CSV files
        """
        self.csv_dir = Path(csv_directory)
        self.pretension_files = []
        self.stiffness_analysis_files = []
        self.stiffness_summary_files = []
        self.fender_force_files = []
        self.analysis_results = {}
        
        # Automatically load files on initialization
        self.load_files()
        
    def load_files(self) -> Dict[str, int]:
        """
        Load all relevant CSV files for analysis.
        
        Returns:
            Dictionary with counts of each file type found
        """
        print("Loading CSV files...")
        
        # Find all relevant files
        for csv_file in self.csv_dir.glob("*.csv"):
            if "pretension_analysis" in csv_file.name:
                self.pretension_files.append(csv_file)
            elif "mooring_stiffness_analysis" in csv_file.name and "summary" not in csv_file.name:
                self.stiffness_analysis_files.append(csv_file)
            elif "mooring_stiffness_summary" in csv_file.name:
                self.stiffness_summary_files.append(csv_file)
            elif "fender_force_analysis" in csv_file.name:
                self.fender_force_files.append(csv_file)
        
        file_counts = {
            'pretension': len(self.pretension_files),
            'stiffness_analysis': len(self.stiffness_analysis_files),
            'stiffness_summary': len(self.stiffness_summary_files),
            'fender_force': len(self.fender_force_files)
        }
        
        for file_type, count in file_counts.items():
            print(f"Found {count} {file_type.replace('_', ' ')} files")
            
        return file_counts
        
    def analyze_pretension(self) -> Optional[pd.DataFrame]:
        """
        Analyze pretension data across configurations.
        
        Returns:
            DataFrame containing pretension analysis results
        """
        print("\n=== PRETENSION ANALYSIS ===")
        
        if not self.pretension_files:
            print("No pretension files found")
            return None
            
        pretension_data = []
        
        for file in self.pretension_files:
            df = pd.read_csv(file)
            config_name = df['filename_stem'].iloc[0] if 'filename_stem' in df.columns else file.stem
            
            # Calculate statistics
            stats = {
                'configuration': config_name,
                'file': file.name,
                'num_lines': len(df),
                'avg_target_tension': df['target_tension'].mean(),
                'avg_current_tension': df['current_tension'].mean() if 'current_tension' in df.columns else None,
                'max_tension': df['current_tension'].max() if 'current_tension' in df.columns else None,
                'min_tension': df['current_tension'].min() if 'current_tension' in df.columns else None,
                'avg_tension_diff_percent': df['tension_diff_percent'].mean() if 'tension_diff_percent' in df.columns else None,
                'max_tension_diff_percent': df['tension_diff_percent'].max() if 'tension_diff_percent' in df.columns else None,
                'converged_lines': len(df[df['tension_diff_percent'].abs() < 10]) if 'tension_diff_percent' in df.columns else None,
                'total_force_x': df['end_Gx_force'].sum() if 'end_Gx_force' in df.columns else None,
                'total_force_y': df['end_Gy_force'].sum() if 'end_Gy_force' in df.columns else None,
                'total_force_z': df['end_Gz_force'].sum() if 'end_Gz_force' in df.columns else None,
            }
            pretension_data.append(stats)
            
        self.analysis_results['pretension'] = pd.DataFrame(pretension_data)
        return self.analysis_results['pretension']
    
    def analyze_stiffness(self) -> Optional[pd.DataFrame]:
        """
        Analyze mooring stiffness data across configurations.
        
        Returns:
            DataFrame containing stiffness analysis results
        """
        print("\n=== STIFFNESS ANALYSIS ===")
        
        if not self.stiffness_summary_files:
            print("No stiffness summary files found")
            return None
            
        stiffness_data = []
        
        for file in self.stiffness_summary_files:
            df = pd.read_csv(file)
            
            if len(df) > 0:
                row = df.iloc[0]
                config_name = row['filename_stem'] if 'filename_stem' in df.columns else file.stem
                
                stats = {
                    'configuration': config_name,
                    'file': file.name,
                    'K_xx_total': row['K_xx_total'] if 'K_xx_total' in df.columns else None,
                    'K_xx_net': row['K_xx_net'] if 'K_xx_net' in df.columns else None,
                    'K_yy_total': row['K_yy_total'] if 'K_yy_total' in df.columns else None,
                    'K_zz_total': row['K_zz_total'] if 'K_zz_total' in df.columns else None,
                    'K_xy_coupling': row['K_xy_total'] if 'K_xy_total' in df.columns else None,
                    'K_xz_coupling': row['K_xz_total'] if 'K_xz_total' in df.columns else None,
                    'K_yz_coupling': row['K_yz_total'] if 'K_yz_total' in df.columns else None,
                    'T_surge': row['T_surge'] if 'T_surge' in df.columns else None,
                    'T_sway': row['T_sway'] if 'T_sway' in df.columns else None,
                    'T_heave': row['T_heave'] if 'T_heave' in df.columns else None,
                }
                stiffness_data.append(stats)
                
        self.analysis_results['stiffness'] = pd.DataFrame(stiffness_data)
        return self.analysis_results['stiffness']
    
    def analyze_line_stiffness_distribution(self) -> Optional[pd.DataFrame]:
        """
        Analyze stiffness distribution across mooring lines.
        
        Returns:
            DataFrame containing line-by-line stiffness analysis
        """
        print("\n=== LINE STIFFNESS DISTRIBUTION ===")
        
        if not self.stiffness_analysis_files:
            print("No stiffness analysis files found")
            return None
            
        line_data = []
        
        for file in self.stiffness_analysis_files:
            df = pd.read_csv(file)
            config_name = df['filename_stem'].iloc[0] if 'filename_stem' in df.columns else file.stem
            
            # Group analysis by line groups (bow, stern, breast, spring)
            for _, row in df.iterrows():
                line_name = row['ObjectName']
                line_num = int(line_name.replace('Line', ''))
                
                # Categorize lines
                if line_num <= 4:
                    group = 'Bow'
                elif line_num <= 8:
                    group = 'Bow-Spring'
                elif line_num <= 12:
                    group = 'Stern-Spring'
                else:
                    group = 'Stern'
                    
                line_stats = {
                    'configuration': config_name,
                    'line': line_name,
                    'line_group': group,
                    'k_axial': row['k_axial'] if 'k_axial' in df.columns else None,
                    'k_x': row['k_x'] if 'k_x' in df.columns else None,
                    'k_y': row['k_y'] if 'k_y' in df.columns else None,
                    'k_z': row['k_z'] if 'k_z' in df.columns else None,
                    'force_magnitude': np.sqrt(row['Fx']**2 + row['Fy']**2 + row['Fz']**2) if all(col in df.columns for col in ['Fx', 'Fy', 'Fz']) else None
                }
                line_data.append(line_stats)
                
        self.analysis_results['line_stiffness'] = pd.DataFrame(line_data)
        return self.analysis_results['line_stiffness']
    
    def analyze_fender_forces(self) -> Optional[pd.DataFrame]:
        """
        Analyze fender force data across configurations.
        
        Returns:
            DataFrame containing fender force analysis results
        """
        print("\n=== FENDER FORCE ANALYSIS ===")
        
        if not self.fender_force_files:
            print("No fender force files found")
            return None
            
        fender_data = []
        
        for file in self.fender_force_files:
            try:
                df = pd.read_csv(file)
                if len(df) > 0:
                    config_name = df['filename_stem'].iloc[0] if 'filename_stem' in df.columns else file.stem
                    
                    stats = {
                        'configuration': config_name,
                        'file': file.name,
                        'num_fenders': len(df),
                        'total_compression_force': df['current_fender_force'].sum() if 'current_fender_force' in df.columns else None,
                        'max_fender_force': df['current_fender_force'].max() if 'current_fender_force' in df.columns else None,
                        'avg_fender_force': df['current_fender_force'].mean() if 'current_fender_force' in df.columns else None,
                        'max_compression': df['current_compression'].max() if 'current_compression' in df.columns else None,
                    }
                    fender_data.append(stats)
            except Exception as e:
                print(f"Error reading {file.name}: {e}")
                
        if fender_data:
            self.analysis_results['fender'] = pd.DataFrame(fender_data)
            return self.analysis_results['fender']
        return None
    
    def run_all_analyses(self) -> Dict[str, pd.DataFrame]:
        """
        Run all available analyses.
        
        Returns:
            Dictionary containing all analysis results
        """
        self.analyze_pretension()
        self.analyze_stiffness()
        self.analyze_line_stiffness_distribution()
        self.analyze_fender_forces()
        return self.analysis_results
    
    def create_visualizations(self, output_dir: Optional[Union[str, Path]] = None) -> Path:
        """
        Create comparative visualizations.
        
        Args:
            output_dir: Directory to save visualizations (default: csv_dir/report)
            
        Returns:
            Path to output directory
        """
        print("\n=== CREATING VISUALIZATIONS ===")
        
        if output_dir is None:
            output_dir = self.csv_dir.parent / 'report'
        output_dir = Path(output_dir)
        output_dir.mkdir(exist_ok=True, parents=True)
        
        # Create stiffness comparison visualization
        self._create_stiffness_plots(output_dir)
        
        # Create pretension comparison visualization
        self._create_pretension_plots(output_dir)
        
        # Create line group analysis visualization
        self._create_line_group_plots(output_dir)
        
        print(f"Visualizations saved to {output_dir}")
        return output_dir
    
    def _create_stiffness_plots(self, output_dir: Path):
        """Create stiffness comparison plots."""
        if 'stiffness' not in self.analysis_results or self.analysis_results['stiffness'].empty:
            return
            
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        df = self.analysis_results['stiffness']
        
        # Extract configuration labels
        df['config_label'] = df['configuration'].apply(lambda x: 'PB' if 'pb' in x.lower() else 'SB')
        
        # Plot directional stiffness
        ax = axes[0, 0]
        x = np.arange(len(df))
        width = 0.25
        ax.bar(x - width, df['K_xx_total'], width, label='Surge (Kxx)', alpha=0.8)
        ax.bar(x, df['K_yy_total'], width, label='Sway (Kyy)', alpha=0.8)
        ax.bar(x + width, df['K_zz_total'], width, label='Heave (Kzz)', alpha=0.8)
        ax.set_xlabel('Configuration')
        ax.set_ylabel('Stiffness (kN/m)')
        ax.set_title('Directional Stiffness Comparison')
        ax.set_xticks(x)
        ax.set_xticklabels(df['config_label'])
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot natural periods
        ax = axes[0, 1]
        ax.bar(x - width, df['T_surge'], width, label='Surge', alpha=0.8)
        ax.bar(x, df['T_sway'], width, label='Sway', alpha=0.8)
        ax.bar(x + width, df['T_heave'], width, label='Heave', alpha=0.8)
        ax.set_xlabel('Configuration')
        ax.set_ylabel('Natural Period (s)')
        ax.set_title('Natural Period Comparison')
        ax.set_xticks(x)
        ax.set_xticklabels(df['config_label'])
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot coupling terms
        ax = axes[1, 0]
        ax.bar(x - width, df['K_xy_coupling'], width, label='Kxy', alpha=0.8)
        ax.bar(x, df['K_xz_coupling'], width, label='Kxz', alpha=0.8)
        ax.bar(x + width, df['K_yz_coupling'], width, label='Kyz', alpha=0.8)
        ax.set_xlabel('Configuration')
        ax.set_ylabel('Coupling Stiffness (kN/m)')
        ax.set_title('Cross-Coupling Terms')
        ax.set_xticks(x)
        ax.set_xticklabels(df['config_label'])
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot stiffness ratios
        ax = axes[1, 1]
        if len(df) > 0:
            df['sway_surge_ratio'] = df['K_yy_total'] / df['K_xx_total']
            df['heave_surge_ratio'] = df['K_zz_total'] / df['K_xx_total']
            
            x = np.arange(len(df))
            ax.bar(x - width/2, df['sway_surge_ratio'], width, label='Sway/Surge', alpha=0.8)
            ax.bar(x + width/2, df['heave_surge_ratio'], width, label='Heave/Surge', alpha=0.8)
            ax.set_xlabel('Configuration')
            ax.set_ylabel('Stiffness Ratio')
            ax.set_title('Stiffness Ratios (relative to surge)')
            ax.set_xticks(x)
            ax.set_xticklabels(df['config_label'])
            ax.legend()
            ax.grid(True, alpha=0.3)
        
        plt.suptitle('Mooring System Stiffness Comparative Analysis', fontsize=16, fontweight='bold')
        plt.tight_layout()
        plt.savefig(output_dir / 'stiffness_comparison.png', dpi=150, bbox_inches='tight')
        plt.close()
    
    def _create_pretension_plots(self, output_dir: Path):
        """Create pretension comparison plots."""
        if 'pretension' not in self.analysis_results or self.analysis_results['pretension'].empty:
            return
            
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        df = self.analysis_results['pretension']
        df['config_label'] = df['configuration'].apply(lambda x: 'PB' if 'pb' in x.lower() else 'SB')
        
        # Plot tension comparison
        ax = axes[0, 0]
        x = np.arange(len(df))
        width = 0.35
        ax.bar(x - width/2, df['avg_target_tension'], width, label='Target', alpha=0.8)
        if 'avg_current_tension' in df.columns:
            ax.bar(x + width/2, df['avg_current_tension'], width, label='Actual', alpha=0.8)
        ax.set_xlabel('Configuration')
        ax.set_ylabel('Tension (kN)')
        ax.set_title('Average Pretension Comparison')
        ax.set_xticks(x)
        ax.set_xticklabels(df['config_label'])
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot force balance
        ax = axes[0, 1]
        if all(col in df.columns for col in ['total_force_x', 'total_force_y', 'total_force_z']):
            width = 0.25
            ax.bar(x - width, df['total_force_x'], width, label='Fx', alpha=0.8)
            ax.bar(x, df['total_force_y'], width, label='Fy', alpha=0.8)
            ax.bar(x + width, df['total_force_z'], width, label='Fz', alpha=0.8)
            ax.set_xlabel('Configuration')
            ax.set_ylabel('Total Force (kN)')
            ax.set_title('Total Force Components')
            ax.set_xticks(x)
            ax.set_xticklabels(df['config_label'])
            ax.legend()
            ax.grid(True, alpha=0.3)
        
        # Plot tension distribution range
        ax = axes[1, 0]
        if 'max_tension' in df.columns and 'min_tension' in df.columns:
            ax.bar(x, df['max_tension'] - df['min_tension'], alpha=0.8)
            ax.set_xlabel('Configuration')
            ax.set_ylabel('Tension Range (kN)')
            ax.set_title('Tension Distribution Range (Max - Min)')
            ax.set_xticks(x)
            ax.set_xticklabels(df['config_label'])
            ax.grid(True, alpha=0.3)
        
        # Plot convergence status
        ax = axes[1, 1]
        if 'converged_lines' in df.columns and 'num_lines' in df.columns:
            df['convergence_rate'] = (df['converged_lines'] / df['num_lines']) * 100
            ax.bar(x, df['convergence_rate'], alpha=0.8)
            ax.set_xlabel('Configuration')
            ax.set_ylabel('Convergence Rate (%)')
            ax.set_title('Pretension Convergence Status')
            ax.set_xticks(x)
            ax.set_xticklabels(df['config_label'])
            ax.axhline(y=90, color='r', linestyle='--', label='90% Target')
            ax.legend()
            ax.grid(True, alpha=0.3)
        
        plt.suptitle('Mooring Pretension Comparative Analysis', fontsize=16, fontweight='bold')
        plt.tight_layout()
        plt.savefig(output_dir / 'pretension_comparison.png', dpi=150, bbox_inches='tight')
        plt.close()
    
    def _create_line_group_plots(self, output_dir: Path):
        """Create line group analysis plots."""
        if 'line_stiffness' not in self.analysis_results or self.analysis_results['line_stiffness'].empty:
            return
            
        fig, axes = plt.subplots(2, 2, figsize=(15, 12))
        df = self.analysis_results['line_stiffness']
        
        # Group statistics
        group_stats = df.groupby(['configuration', 'line_group']).agg({
            'k_axial': 'mean',
            'k_x': 'mean',
            'k_y': 'mean',
            'k_z': 'mean',
            'force_magnitude': 'mean'
        }).reset_index()
        
        # Plot axial stiffness by group
        ax = axes[0, 0]
        for config in group_stats['configuration'].unique():
            config_data = group_stats[group_stats['configuration'] == config]
            config_label = 'PB' if 'pb' in config.lower() else 'SB'
            ax.bar(config_data['line_group'], config_data['k_axial'], alpha=0.7, label=config_label)
        ax.set_xlabel('Line Group')
        ax.set_ylabel('Axial Stiffness (kN/m)')
        ax.set_title('Axial Stiffness by Line Group')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot force distribution
        ax = axes[0, 1]
        for config in group_stats['configuration'].unique():
            config_data = group_stats[group_stats['configuration'] == config]
            config_label = 'PB' if 'pb' in config.lower() else 'SB'
            ax.bar(config_data['line_group'], config_data['force_magnitude'], alpha=0.7, label=config_label)
        ax.set_xlabel('Line Group')
        ax.set_ylabel('Force Magnitude (kN)')
        ax.set_title('Average Force by Line Group')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot X-direction stiffness
        ax = axes[1, 0]
        line_groups = group_stats['line_group'].unique()
        x = np.arange(len(line_groups))
        width = 0.2
        
        for i, config in enumerate(group_stats['configuration'].unique()):
            config_data = group_stats[group_stats['configuration'] == config]
            config_label = 'PB' if 'pb' in config.lower() else 'SB'
            offset = (i - 0.5) * width
            ax.bar(x + offset, config_data['k_x'].abs(), width, label=f'{config_label} - X', alpha=0.7)
        
        ax.set_xlabel('Line Group')
        ax.set_ylabel('Stiffness Contribution (kN/m)')
        ax.set_title('X-Direction Stiffness by Line Group')
        ax.set_xticks(x)
        ax.set_xticklabels(line_groups)
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Plot Y-direction stiffness
        ax = axes[1, 1]
        for i, config in enumerate(group_stats['configuration'].unique()):
            config_data = group_stats[group_stats['configuration'] == config]
            config_label = 'PB' if 'pb' in config.lower() else 'SB'
            offset = (i - 0.5) * width
            ax.bar(x + offset, config_data['k_y'], width, label=f'{config_label} - Y', alpha=0.7)
        
        ax.set_xlabel('Line Group')
        ax.set_ylabel('Stiffness Contribution (kN/m)')
        ax.set_title('Y-Direction Stiffness by Line Group')
        ax.set_xticks(x)
        ax.set_xticklabels(line_groups)
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        plt.suptitle('Line Group Stiffness Analysis', fontsize=16, fontweight='bold')
        plt.tight_layout()
        plt.savefig(output_dir / 'line_group_analysis.png', dpi=150, bbox_inches='tight')
        plt.close()
    
    def generate_report(self, output_file: Optional[Union[str, Path]] = None) -> str:
        """
        Generate a comprehensive markdown report of the analysis.
        
        Args:
            output_file: Path to save report (default: csv_dir/report/analysis_report.md)
            
        Returns:
            The generated report as a string
        """
        from .report_generator import ReportGenerator
        
        generator = ReportGenerator(self.analysis_results, self.csv_dir)
        return generator.generate(output_file)