"""
Report Generator for Mooring Analysis

Generates comprehensive markdown reports from analysis results.
"""

import numpy as np
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import Dict, Optional, Union, List


class ReportGenerator:
    """Generate comprehensive analysis reports in markdown format."""
    
    def __init__(self, analysis_results: Dict[str, pd.DataFrame], csv_dir: Path):
        """
        Initialize report generator.
        
        Args:
            analysis_results: Dictionary of analysis DataFrames
            csv_dir: Path to CSV directory
        """
        self.analysis_results = analysis_results
        self.csv_dir = csv_dir
        self.report = []
        
    def generate(self, output_file: Optional[Union[str, Path]] = None) -> str:
        """
        Generate the complete report.
        
        Args:
            output_file: Path to save report
            
        Returns:
            Generated report as string
        """
        print("\n=== GENERATING REPORT ===")
        
        if output_file is None:
            output_file = self.csv_dir.parent / 'report' / 'analysis_report.md'
        output_file = Path(output_file)
        output_file.parent.mkdir(exist_ok=True, parents=True)
        
        # Build report sections
        self._add_header()
        self._add_executive_summary()
        self._add_stiffness_analysis()
        self._add_pretension_analysis()
        self._add_line_group_analysis()
        self._add_fender_analysis()
        self._add_conclusions()
        self._add_footer()
        
        # Write report to file
        report_text = '\n'.join(self.report)
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(report_text)
        
        print(f"Report saved to {output_file}")
        return report_text
    
    def _add_header(self):
        """Add report header."""
        self.report.append("# Mooring System Comparative Analysis Report")
        self.report.append(f"\n**Generated:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        self.report.append(f"\n**Data Directory:** `{self.csv_dir}`")
        self.report.append("\n---\n")
    
    def _add_executive_summary(self):
        """Add executive summary section."""
        self.report.append("## Executive Summary\n")
        self.report.append("This report presents a comparative analysis of mooring system configurations, ")
        self.report.append("evaluating pretension characteristics, stiffness properties, and force distributions.\n")
    
    def _add_stiffness_analysis(self):
        """Add stiffness analysis section."""
        if 'stiffness' not in self.analysis_results or self.analysis_results['stiffness'].empty:
            return
            
        self.report.append("\n## 1. Mooring Stiffness Analysis\n")
        df = self.analysis_results['stiffness']
        
        # System stiffness comparison table
        self.report.append("### 1.1 System Stiffness Comparison\n")
        self.report.append("| Configuration | Kxx (kN/m) | Kyy (kN/m) | Kzz (kN/m) | T_surge (s) | T_sway (s) | T_heave (s) |")
        self.report.append("|--------------|------------|------------|------------|-------------|------------|-------------|")
        
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            self.report.append(f"| {config} | {row['K_xx_total']:.1f} | {row['K_yy_total']:.1f} | "
                             f"{row['K_zz_total']:.1f} | {row['T_surge']:.1f} | "
                             f"{row['T_sway']:.1f} | {row['T_heave']:.1f} |")
        
        # Key findings
        self._add_stiffness_findings(df)
        
        # Cross-coupling effects
        self._add_coupling_analysis(df)
    
    def _add_stiffness_findings(self, df: pd.DataFrame):
        """Add stiffness key findings."""
        self.report.append("\n### 1.2 Key Findings\n")
        
        if len(df) == 2:
            pb_data = df[df['configuration'].str.contains('pb', case=False)].iloc[0]
            sb_data = df[df['configuration'].str.contains('sb', case=False)].iloc[0]
            
            kxx_diff = ((pb_data['K_xx_total'] - sb_data['K_xx_total']) / sb_data['K_xx_total']) * 100
            kyy_diff = ((pb_data['K_yy_total'] - sb_data['K_yy_total']) / sb_data['K_yy_total']) * 100
            kzz_diff = ((pb_data['K_zz_total'] - sb_data['K_zz_total']) / sb_data['K_zz_total']) * 100
            
            self.report.append(f"- **Surge Stiffness Difference:** PB is {kxx_diff:+.1f}% compared to SB")
            self.report.append(f"- **Sway Stiffness Difference:** PB is {kyy_diff:+.1f}% compared to SB")
            self.report.append(f"- **Heave Stiffness Difference:** PB is {kzz_diff:+.1f}% compared to SB")
            
            self.report.append(f"\n- **Natural Period - Surge:** PB = {pb_data['T_surge']:.1f}s, SB = {sb_data['T_surge']:.1f}s")
            self.report.append(f"- **Natural Period - Sway:** PB = {pb_data['T_sway']:.1f}s, SB = {sb_data['T_sway']:.1f}s")
            self.report.append(f"- **Natural Period - Heave:** PB = {pb_data['T_heave']:.1f}s, SB = {sb_data['T_heave']:.1f}s")
        
        # General characteristics
        self.report.append("\n### 1.3 Stiffness Characteristics\n")
        avg_kxx = df['K_xx_total'].mean()
        avg_kyy = df['K_yy_total'].mean()
        avg_kzz = df['K_zz_total'].mean()
        
        self.report.append(f"- **Average System Stiffness:** Kxx = {avg_kxx:.1f} kN/m, Kyy = {avg_kyy:.1f} kN/m, Kzz = {avg_kzz:.1f} kN/m")
        self.report.append(f"- **Stiffness Ratios:** Kyy/Kxx = {avg_kyy/avg_kxx:.2f}, Kzz/Kxx = {avg_kzz/avg_kxx:.2f}")
    
    def _add_coupling_analysis(self, df: pd.DataFrame):
        """Add cross-coupling analysis."""
        self.report.append("\n### 1.4 Cross-Coupling Effects\n")
        self.report.append("| Configuration | Kxy (kN/m) | Kxz (kN/m) | Kyz (kN/m) |")
        self.report.append("|--------------|------------|------------|------------|")
        
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            self.report.append(f"| {config} | {row['K_xy_coupling']:.1f} | "
                             f"{row['K_xz_coupling']:.1f} | {row['K_yz_coupling']:.1f} |")
    
    def _add_pretension_analysis(self):
        """Add pretension analysis section."""
        if 'pretension' not in self.analysis_results or self.analysis_results['pretension'].empty:
            return
            
        self.report.append("\n## 2. Pretension Analysis\n")
        df = self.analysis_results['pretension']
        
        # Summary table
        self.report.append("### 2.1 Pretension Summary\n")
        self.report.append("| Configuration | Avg Target (kN) | Avg Actual (kN) | Max (kN) | Min (kN) | Convergence Rate |")
        self.report.append("|--------------|-----------------|-----------------|----------|----------|------------------|")
        
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            conv_rate = (row['converged_lines'] / row['num_lines'] * 100) if row['converged_lines'] else 0
            self.report.append(f"| {config} | {row['avg_target_tension']:.1f} | "
                             f"{row['avg_current_tension']:.1f} | {row['max_tension']:.1f} | "
                             f"{row['min_tension']:.1f} | {conv_rate:.1f}% |")
        
        # Force balance
        self._add_force_balance(df)
        
        # Tension distribution
        self._add_tension_distribution(df)
    
    def _add_force_balance(self, df: pd.DataFrame):
        """Add force balance analysis."""
        self.report.append("\n### 2.2 Force Balance\n")
        if all(col in df.columns for col in ['total_force_x', 'total_force_y', 'total_force_z']):
            self.report.append("| Configuration | Fx Total (kN) | Fy Total (kN) | Fz Total (kN) | Resultant (kN) |")
            self.report.append("|--------------|---------------|---------------|---------------|----------------|")
            
            for _, row in df.iterrows():
                config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
                resultant = np.sqrt(row['total_force_x']**2 + row['total_force_y']**2 + row['total_force_z']**2)
                self.report.append(f"| {config} | {row['total_force_x']:.1f} | {row['total_force_y']:.1f} | "
                                 f"{row['total_force_z']:.1f} | {resultant:.1f} |")
    
    def _add_tension_distribution(self, df: pd.DataFrame):
        """Add tension distribution analysis."""
        self.report.append("\n### 2.3 Tension Distribution\n")
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            tension_range = row['max_tension'] - row['min_tension']
            self.report.append(f"- **{config} Configuration:**")
            self.report.append(f"  - Tension Range: {tension_range:.1f} kN ({row['min_tension']:.1f} - {row['max_tension']:.1f} kN)")
            self.report.append(f"  - Average Deviation from Target: {row['avg_tension_diff_percent']:.1f}%")
            self.report.append(f"  - Maximum Deviation: {row['max_tension_diff_percent']:.1f}%")
    
    def _add_line_group_analysis(self):
        """Add line group analysis section."""
        if 'line_stiffness' not in self.analysis_results or self.analysis_results['line_stiffness'].empty:
            return
            
        self.report.append("\n## 3. Line Group Analysis\n")
        df = self.analysis_results['line_stiffness']
        
        group_stats = df.groupby(['configuration', 'line_group']).agg({
            'k_axial': 'mean',
            'force_magnitude': 'mean',
            'k_x': 'mean',
            'k_y': 'mean'
        }).round(1)
        
        self.report.append("### 3.1 Average Properties by Line Group\n")
        
        for config in df['configuration'].unique():
            config_label = 'PB' if 'pb' in config.lower() else 'SB'
            self.report.append(f"\n**{config_label} Configuration:**\n")
            self.report.append("| Line Group | Avg Axial Stiffness (kN/m) | Avg Force (kN) | Avg Kx (kN/m) | Avg Ky (kN/m) |")
            self.report.append("|------------|---------------------------|----------------|---------------|---------------|")
            
            config_data = group_stats.loc[config]
            for group in config_data.index:
                row = config_data.loc[group]
                self.report.append(f"| {group} | {row['k_axial']:.1f} | {row['force_magnitude']:.1f} | "
                                 f"{row['k_x']:.1f} | {row['k_y']:.1f} |")
    
    def _add_fender_analysis(self):
        """Add fender force analysis section."""
        if 'fender' not in self.analysis_results or self.analysis_results['fender'] is None:
            return
            
        df = self.analysis_results['fender']
        if df.empty:
            return
            
        self.report.append("\n## 4. Fender Force Analysis\n")
        self.report.append("### 4.1 Fender Loading Summary\n")
        self.report.append("| Configuration | Total Force (kN) | Max Force (kN) | Avg Force (kN) | Max Compression (m) |")
        self.report.append("|--------------|------------------|----------------|----------------|-------------------|")
        
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            total_force = f"{row['total_compression_force']:.1f}" if row['total_compression_force'] is not None else "N/A"
            max_force = f"{row['max_fender_force']:.1f}" if row['max_fender_force'] is not None else "N/A"
            avg_force = f"{row['avg_fender_force']:.1f}" if row['avg_fender_force'] is not None else "N/A"
            max_comp = f"{row['max_compression']:.3f}" if row['max_compression'] is not None else "N/A"
            self.report.append(f"| {config} | {total_force} | {max_force} | {avg_force} | {max_comp} |")
    
    def _add_conclusions(self):
        """Add conclusions and recommendations section."""
        self.report.append("\n## 5. Conclusions and Recommendations\n")
        
        self._add_performance_summary()
        self._add_pretension_performance()
        self._add_recommendations()
        self._add_next_steps()
    
    def _add_performance_summary(self):
        """Add system performance summary."""
        self.report.append("### 5.1 System Performance Summary\n")
        
        if 'stiffness' in self.analysis_results and len(self.analysis_results['stiffness']) > 0:
            df = self.analysis_results['stiffness']
            
            best_surge = df.loc[df['T_surge'].idxmax()]
            best_sway = df.loc[df['T_sway'].idxmax()]
            
            self.report.append(f"- **Optimal Surge Response:** {'PB' if 'pb' in best_surge['configuration'].lower() else 'SB'} "
                             f"configuration with T = {best_surge['T_surge']:.1f}s")
            self.report.append(f"- **Optimal Sway Response:** {'PB' if 'pb' in best_sway['configuration'].lower() else 'SB'} "
                             f"configuration with T = {best_sway['T_sway']:.1f}s")
            
            # Check coupling
            max_coupling = df[['K_xy_coupling', 'K_xz_coupling', 'K_yz_coupling']].abs().max().max()
            avg_main_stiffness = df[['K_xx_total', 'K_yy_total', 'K_zz_total']].mean().mean()
            coupling_ratio = max_coupling / avg_main_stiffness
            
            if coupling_ratio > 0.2:
                self.report.append(f"\n⚠️ **Warning:** Significant cross-coupling detected (max {coupling_ratio:.1%} of main stiffness)")
            else:
                self.report.append(f"\n✓ **Cross-coupling is within acceptable limits** ({coupling_ratio:.1%} of main stiffness)")
    
    def _add_pretension_performance(self):
        """Add pretension performance analysis."""
        if 'pretension' not in self.analysis_results or len(self.analysis_results['pretension']) == 0:
            return
            
        self.report.append("\n### 5.2 Pretension Performance\n")
        df = self.analysis_results['pretension']
        
        for _, row in df.iterrows():
            config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
            conv_rate = (row['converged_lines'] / row['num_lines'] * 100) if row['converged_lines'] else 0
            
            if conv_rate > 90:
                self.report.append(f"- ✓ **{config}:** Good convergence ({conv_rate:.1f}% of lines within tolerance)")
            else:
                self.report.append(f"- ⚠️ **{config}:** Poor convergence ({conv_rate:.1f}% of lines within tolerance)")
            
            # Check force imbalance
            if all(col in row.index for col in ['total_force_x', 'total_force_y', 'total_force_z']):
                total_force = np.sqrt(row['total_force_x']**2 + row['total_force_y']**2 + row['total_force_z']**2)
                if total_force < 50:  # Threshold
                    self.report.append(f"  - ✓ Force balance acceptable (resultant = {total_force:.1f} kN)")
                else:
                    self.report.append(f"  - ⚠️ Significant force imbalance (resultant = {total_force:.1f} kN)")
    
    def _add_recommendations(self):
        """Add recommendations based on analysis."""
        self.report.append("\n### 5.3 Recommendations\n")
        
        recommendations = []
        
        # Check natural periods
        if 'stiffness' in self.analysis_results:
            df = self.analysis_results['stiffness']
            min_period = df[['T_surge', 'T_sway', 'T_heave']].min().min()
            max_period = df[['T_surge', 'T_sway', 'T_heave']].max().max()
            
            if min_period < 20:
                recommendations.append(f"- Consider reducing mooring stiffness to increase natural periods (current min: {min_period:.1f}s)")
            if max_period > 100:
                recommendations.append(f"- Consider increasing mooring stiffness to reduce excessive natural periods (current max: {max_period:.1f}s)")
        
        # Check pretension distribution
        if 'pretension' in self.analysis_results:
            df = self.analysis_results['pretension']
            for _, row in df.iterrows():
                config = 'PB' if 'pb' in row['configuration'].lower() else 'SB'
                tension_range = row['max_tension'] - row['min_tension']
                avg_tension = row['avg_current_tension']
                
                if avg_tension > 0 and tension_range / avg_tension > 0.5:
                    recommendations.append(f"- **{config}:** Review line arrangement to improve tension distribution "
                                         f"(current range: {tension_range/avg_tension:.1%} of average)")
        
        if recommendations:
            self.report.extend(recommendations)
        else:
            self.report.append("- All parameters are within acceptable ranges")
    
    def _add_next_steps(self):
        """Add next steps section."""
        self.report.append("\n### 5.4 Next Steps\n")
        self.report.append("1. Verify mooring line specifications against design requirements")
        self.report.append("2. Conduct dynamic analysis for critical configurations")
        self.report.append("3. Review fender capacity for maximum observed loads")
        self.report.append("4. Consider optimization of line pretensions for better force distribution")
    
    def _add_footer(self):
        """Add report footer."""
        self.report.append("\n---\n")
        self.report.append("*End of Report*")