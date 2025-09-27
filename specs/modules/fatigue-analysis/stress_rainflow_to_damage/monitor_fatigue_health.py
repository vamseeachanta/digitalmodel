#!/usr/bin/env python3
"""
Fatigue Health Monitoring System
Automated script to monitor fatigue damage trends and generate alerts
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
import json
import argparse

class FatigueMonitor:
    """Monitor fatigue health and generate alerts"""
    
    def __init__(self, summary_file, thresholds_file=None):
        """Initialize monitor with summary data and thresholds"""
        self.summary_file = summary_file
        self.df = pd.read_csv(summary_file)
        
        # Default thresholds
        self.thresholds = {
            'critical_damage_rate': 0.1,      # 10 years life
            'warning_damage_rate': 0.04,      # 25 years life
            'notice_damage_rate': 0.02,       # 50 years life
            'critical_stress_mpa': 45.0,      # Near fatigue limit
            'warning_stress_mpa': 40.0,       # Approaching limit
            'min_safety_margin': 5.0,          # Minimum acceptable safety factor
            'design_life_years': 25.0         # Target design life
        }
        
        # Load custom thresholds if provided
        if thresholds_file and os.path.exists(thresholds_file):
            with open(thresholds_file, 'r') as f:
                custom = json.load(f)
                self.thresholds.update(custom)
    
    def analyze_health(self):
        """Perform comprehensive health analysis"""
        report = {
            'timestamp': datetime.now().isoformat(),
            'total_locations': len(self.df),
            'status': 'HEALTHY',
            'alerts': [],
            'statistics': {},
            'risk_locations': [],
            'trends': {}
        }
        
        # Calculate statistics
        report['statistics'] = {
            'files_analyzed': len(self.df),
            'min_fatigue_life': self._safe_min(self.df['fatigue_life_years']),
            'avg_damage_rate': self.df['total_damage_rate_per_year'].mean(),
            'max_damage_rate': self.df['total_damage_rate_per_year'].max(),
            'locations_with_damage': (self.df['total_damage_rate_per_year'] > 0).sum(),
            'infinite_life_locations': (self.df['fatigue_life_years'] == float('inf')).sum()
        }
        
        # Check for critical conditions
        critical = self.df[self.df['total_damage_rate_per_year'] > self.thresholds['critical_damage_rate']]
        if len(critical) > 0:
            report['status'] = 'CRITICAL'
            for _, row in critical.iterrows():
                report['alerts'].append({
                    'level': 'CRITICAL',
                    'location': row['filename'],
                    'message': f"Fatigue life {row['fatigue_life_years']:.1f} years < 10 years",
                    'damage_rate': row['total_damage_rate_per_year']
                })
                report['risk_locations'].append(row['filename'])
        
        # Check for warnings
        warning = self.df[
            (self.df['total_damage_rate_per_year'] > self.thresholds['warning_damage_rate']) &
            (self.df['total_damage_rate_per_year'] <= self.thresholds['critical_damage_rate'])
        ]
        if len(warning) > 0 and report['status'] != 'CRITICAL':
            report['status'] = 'WARNING'
            for _, row in warning.iterrows():
                report['alerts'].append({
                    'level': 'WARNING',
                    'location': row['filename'],
                    'message': f"Fatigue life {row['fatigue_life_years']:.1f} years < design life",
                    'damage_rate': row['total_damage_rate_per_year']
                })
                report['risk_locations'].append(row['filename'])
        
        # Check for notices
        notice = self.df[
            (self.df['total_damage_rate_per_year'] > self.thresholds['notice_damage_rate']) &
            (self.df['total_damage_rate_per_year'] <= self.thresholds['warning_damage_rate'])
        ]
        if len(notice) > 0 and report['status'] == 'HEALTHY':
            report['status'] = 'NOTICE'
            for _, row in notice.iterrows():
                report['alerts'].append({
                    'level': 'NOTICE',
                    'location': row['filename'],
                    'message': f"Monitor: Fatigue life {row['fatigue_life_years']:.1f} years",
                    'damage_rate': row['total_damage_rate_per_year']
                })
        
        # Identify top risk locations
        if self.df['total_damage_rate_per_year'].max() > 0:
            top_risk = self.df.nlargest(10, 'total_damage_rate_per_year')
            report['risk_locations'] = top_risk['filename'].tolist()
        
        # Location-specific analysis
        report['location_analysis'] = self.analyze_by_location()
        
        # Configuration analysis
        report['config_analysis'] = self.analyze_by_config()
        
        return report
    
    def analyze_by_location(self):
        """Analyze damage by location ID"""
        location_stats = {}
        
        for location in self.df['location_id'].unique():
            loc_df = self.df[self.df['location_id'] == location]
            location_stats[location] = {
                'count': len(loc_df),
                'avg_damage_rate': loc_df['total_damage_rate_per_year'].mean(),
                'max_damage_rate': loc_df['total_damage_rate_per_year'].max(),
                'min_life_years': self._safe_min(loc_df['fatigue_life_years']),
                'thickness_mm': loc_df['thickness_mm'].iloc[0] if len(loc_df) > 0 else None,
                'scf': loc_df['scf'].iloc[0] if len(loc_df) > 0 else None
            }
        
        return location_stats
    
    def analyze_by_config(self):
        """Analyze damage by configuration"""
        config_stats = {}
        
        for config in self.df['config'].unique():
            cfg_df = self.df[self.df['config'] == config]
            config_stats[config] = {
                'count': len(cfg_df),
                'avg_damage_rate': cfg_df['total_damage_rate_per_year'].mean(),
                'max_damage_rate': cfg_df['total_damage_rate_per_year'].max(),
                'locations_with_damage': (cfg_df['total_damage_rate_per_year'] > 0).sum()
            }
        
        return config_stats
    
    def _safe_min(self, series):
        """Calculate minimum excluding infinity"""
        finite_values = series[series != float('inf')]
        if len(finite_values) > 0:
            return finite_values.min()
        return float('inf')
    
    def generate_dashboard(self, report):
        """Generate text dashboard"""
        dashboard = []
        dashboard.append("="*80)
        dashboard.append("FATIGUE HEALTH MONITORING DASHBOARD")
        dashboard.append("="*80)
        dashboard.append(f"Timestamp: {report['timestamp']}")
        dashboard.append(f"Status: {report['status']}")
        dashboard.append("")
        
        # Overall statistics
        dashboard.append("OVERALL STATISTICS")
        dashboard.append("-"*40)
        stats = report['statistics']
        dashboard.append(f"Total Locations Analyzed: {stats['files_analyzed']}")
        dashboard.append(f"Locations with Infinite Life: {stats['infinite_life_locations']}")
        dashboard.append(f"Locations with Damage: {stats['locations_with_damage']}")
        
        if stats['max_damage_rate'] > 0:
            dashboard.append(f"Maximum Damage Rate: {stats['max_damage_rate']:.3e} /year")
            if stats['min_fatigue_life'] != float('inf'):
                dashboard.append(f"Minimum Fatigue Life: {stats['min_fatigue_life']:.1f} years")
        dashboard.append("")
        
        # Alerts
        if report['alerts']:
            dashboard.append("ALERTS")
            dashboard.append("-"*40)
            for alert in report['alerts']:
                dashboard.append(f"[{alert['level']}] {alert['location']}")
                dashboard.append(f"  {alert['message']}")
            dashboard.append("")
        
        # Location analysis
        dashboard.append("LOCATION ANALYSIS")
        dashboard.append("-"*40)
        loc_analysis = report['location_analysis']
        dashboard.append(f"{'Location':<10} {'SCF':<6} {'Thickness':<10} {'Max Damage':<12} {'Min Life'}")
        dashboard.append("-"*50)
        
        for loc, stats in sorted(loc_analysis.items()):
            scf = f"{stats['scf']:.2f}" if stats['scf'] else "N/A"
            thickness = f"{stats['thickness_mm']:.0f}mm" if stats['thickness_mm'] else "N/A"
            damage = f"{stats['max_damage_rate']:.2e}" if stats['max_damage_rate'] > 0 else "0.0"
            life = f"{stats['min_life_years']:.1f} yrs" if stats['min_life_years'] != float('inf') else "Infinite"
            dashboard.append(f"{loc:<10} {scf:<6} {thickness:<10} {damage:<12} {life}")
        dashboard.append("")
        
        # Risk assessment
        dashboard.append("RISK ASSESSMENT")
        dashboard.append("-"*40)
        if report['risk_locations']:
            dashboard.append("High Risk Locations (Top 10):")
            for i, loc in enumerate(report['risk_locations'][:10], 1):
                dashboard.append(f"  {i}. {loc}")
        else:
            dashboard.append("No locations with measurable fatigue damage")
        dashboard.append("")
        
        # Recommendations
        dashboard.append("RECOMMENDATIONS")
        dashboard.append("-"*40)
        if report['status'] == 'CRITICAL':
            dashboard.append("[!] IMMEDIATE ACTION REQUIRED")
            dashboard.append("- Inspect critical locations immediately")
            dashboard.append("- Review operational parameters")
            dashboard.append("- Consider emergency maintenance")
        elif report['status'] == 'WARNING':
            dashboard.append("[!] ATTENTION NEEDED")
            dashboard.append("- Schedule inspection of warning locations")
            dashboard.append("- Review stress history trends")
            dashboard.append("- Plan preventive maintenance")
        elif report['status'] == 'NOTICE':
            dashboard.append("[i] MONITORING RECOMMENDED")
            dashboard.append("- Continue regular monitoring")
            dashboard.append("- Track damage accumulation trends")
            dashboard.append("- Maintain inspection schedule")
        else:
            dashboard.append("[OK] SYSTEM HEALTHY")
            dashboard.append("- Continue normal operations")
            dashboard.append("- Maintain quarterly monitoring")
            dashboard.append("- No immediate action required")
        
        dashboard.append("="*80)
        
        return "\n".join(dashboard)
    
    def save_report(self, report, output_dir="monitoring_reports"):
        """Save monitoring report"""
        Path(output_dir).mkdir(exist_ok=True)
        
        # Save JSON report
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        json_file = Path(output_dir) / f"fatigue_monitor_{timestamp}.json"
        with open(json_file, 'w') as f:
            # Convert infinity to string for JSON serialization
            report_json = json.dumps(report, default=str, indent=2)
            f.write(report_json)
        
        # Save dashboard text
        dashboard_file = Path(output_dir) / f"fatigue_dashboard_{timestamp}.txt"
        dashboard = self.generate_dashboard(report)
        with open(dashboard_file, 'w', encoding='utf-8') as f:
            f.write(dashboard)
        
        return json_file, dashboard_file


def main():
    """Main execution"""
    parser = argparse.ArgumentParser(description='Fatigue Health Monitoring System')
    parser.add_argument('--summary', type=str, required=True,
                       help='Path to damage_analysis_summary.csv')
    parser.add_argument('--thresholds', type=str,
                       help='Path to custom thresholds JSON file')
    parser.add_argument('--output', type=str, default='monitoring_reports',
                       help='Output directory for reports')
    parser.add_argument('--display', action='store_true',
                       help='Display dashboard to console')
    
    args = parser.parse_args()
    
    # Check if summary file exists
    if not os.path.exists(args.summary):
        print(f"Error: Summary file not found: {args.summary}")
        sys.exit(1)
    
    # Initialize monitor
    monitor = FatigueMonitor(args.summary, args.thresholds)
    
    # Analyze health
    report = monitor.analyze_health()
    
    # Save reports
    json_file, dashboard_file = monitor.save_report(report, args.output)
    
    print(f"Reports saved:")
    print(f"  JSON: {json_file}")
    print(f"  Dashboard: {dashboard_file}")
    
    # Display dashboard if requested
    if args.display:
        print("\n" + monitor.generate_dashboard(report))
    
    # Exit with appropriate code
    if report['status'] == 'CRITICAL':
        sys.exit(2)
    elif report['status'] == 'WARNING':
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()