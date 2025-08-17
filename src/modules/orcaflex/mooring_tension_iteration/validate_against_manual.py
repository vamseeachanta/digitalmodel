"""
Validation Script - Compare Automated Results Against Manual Process
This script helps validate the automated system against manual iteration results
"""

import pandas as pd
import numpy as np
from pathlib import Path
import json
import yaml
from typing import Dict, List, Tuple
import matplotlib.pyplot as plt
from datetime import datetime


class ManualProcessValidator:
    """Validate automated results against manual process"""
    
    def __init__(self, manual_results_path: Path, automated_results_path: Path):
        """
        Initialize validator with paths to results
        
        Args:
            manual_results_path: Path to manual process results
            automated_results_path: Path to automated iteration output
        """
        self.manual_path = Path(manual_results_path)
        self.automated_path = Path(automated_results_path)
        self.validation_results = {}
        
    def load_manual_results(self) -> pd.DataFrame:
        """
        Load manual iteration results from CSV or Excel
        
        Expected format:
        Iteration | Line01_Tension | Line02_Tension | ... | Max_Error
        """
        # Try different file formats
        if (self.manual_path / "manual_results.csv").exists():
            return pd.read_csv(self.manual_path / "manual_results.csv")
        elif (self.manual_path / "manual_results.xlsx").exists():
            return pd.read_excel(self.manual_path / "manual_results.xlsx")
        else:
            print("Manual results file not found. Creating template...")
            self._create_manual_template()
            return None
    
    def load_automated_results(self) -> Dict:
        """Load automated iteration history"""
        # Find most recent history file
        history_files = list(self.automated_path.glob("iteration_history_*.json"))
        
        if not history_files:
            raise FileNotFoundError("No automated results found")
        
        latest = max(history_files, key=lambda p: p.stat().st_mtime)
        
        with open(latest, 'r') as f:
            return json.load(f)
    
    def _create_manual_template(self):
        """Create template for manual results entry"""
        template = pd.DataFrame({
            'Iteration': [1, 2, 3, 4, 5],
            'Line01_Tension': [0.0] * 5,
            'Line02_Tension': [0.0] * 5,
            'Line03_Tension': [0.0] * 5,
            'Max_Error_%': [0.0] * 5,
            'Time_Minutes': [0.0] * 5,
            'Notes': [''] * 5
        })
        
        template_path = self.manual_path / "manual_results_template.csv"
        template.to_csv(template_path, index=False)
        print(f"Template created: {template_path}")
        print("Please fill in manual results and run validation again")
    
    def compare_convergence(self) -> Dict:
        """Compare convergence behavior between manual and automated"""
        manual_df = self.load_manual_results()
        if manual_df is None:
            return None
        
        automated = self.load_automated_results()
        
        # Extract convergence data
        manual_errors = manual_df['Max_Error_%'].values
        automated_errors = [entry['max_error'] for entry in automated]
        
        # Calculate metrics
        results = {
            'manual_iterations': len(manual_errors),
            'automated_iterations': len(automated_errors),
            'manual_final_error': manual_errors[-1] if len(manual_errors) > 0 else None,
            'automated_final_error': automated_errors[-1] if len(automated_errors) > 0 else None,
            'iterations_saved': len(manual_errors) - len(automated_errors),
            'convergence_rate_improvement': None
        }
        
        # Calculate convergence rate improvement
        if len(manual_errors) > 1 and len(automated_errors) > 1:
            manual_rate = (manual_errors[0] - manual_errors[-1]) / len(manual_errors)
            auto_rate = (automated_errors[0] - automated_errors[-1]) / len(automated_errors)
            results['convergence_rate_improvement'] = (auto_rate / manual_rate - 1) * 100
        
        self.validation_results['convergence'] = results
        return results
    
    def compare_final_tensions(self, tolerance: float = 1.0) -> Dict:
        """
        Compare final tensions between manual and automated
        
        Args:
            tolerance: Acceptable difference in tensions (kN)
        """
        manual_df = self.load_manual_results()
        if manual_df is None:
            return None
        
        automated = self.load_automated_results()
        
        # Get final tensions
        last_manual = manual_df.iloc[-1]
        last_auto = automated[-1] if automated else None
        
        if not last_auto:
            return None
        
        # Compare line tensions
        differences = {}
        tension_cols = [col for col in manual_df.columns if 'Tension' in col]
        
        for col in tension_cols:
            line_name = col.replace('_Tension', '')
            manual_tension = last_manual[col]
            auto_tension = last_auto['tensions'].get(line_name, 0)
            
            diff = abs(manual_tension - auto_tension)
            differences[line_name] = {
                'manual': manual_tension,
                'automated': auto_tension,
                'difference': diff,
                'within_tolerance': diff <= tolerance
            }
        
        # Summary statistics
        all_diffs = [d['difference'] for d in differences.values()]
        results = {
            'line_differences': differences,
            'max_difference': max(all_diffs) if all_diffs else 0,
            'avg_difference': np.mean(all_diffs) if all_diffs else 0,
            'all_within_tolerance': all(d['within_tolerance'] for d in differences.values())
        }
        
        self.validation_results['tensions'] = results
        return results
    
    def compare_time_efficiency(self) -> Dict:
        """Compare time taken between manual and automated processes"""
        manual_df = self.load_manual_results()
        if manual_df is None:
            return None
        
        automated = self.load_automated_results()
        
        # Get time data
        manual_time = manual_df['Time_Minutes'].sum() if 'Time_Minutes' in manual_df else None
        
        # Calculate automated time (in minutes)
        auto_time = sum(entry.get('analysis_time', 0) for entry in automated) / 60
        
        results = {
            'manual_time_minutes': manual_time,
            'automated_time_minutes': auto_time,
            'time_saved_minutes': manual_time - auto_time if manual_time else None,
            'time_reduction_percent': ((manual_time - auto_time) / manual_time * 100) if manual_time else None
        }
        
        self.validation_results['efficiency'] = results
        return results
    
    def generate_validation_report(self) -> str:
        """Generate comprehensive validation report"""
        report = []
        report.append("="*60)
        report.append("VALIDATION REPORT - Manual vs Automated Process")
        report.append("="*60)
        report.append(f"Generated: {datetime.now():%Y-%m-%d %H:%M:%S}")
        report.append("")
        
        # Convergence comparison
        conv = self.validation_results.get('convergence', {})
        if conv:
            report.append("CONVERGENCE COMPARISON")
            report.append("-"*30)
            report.append(f"Manual iterations: {conv['manual_iterations']}")
            report.append(f"Automated iterations: {conv['automated_iterations']}")
            report.append(f"Iterations saved: {conv['iterations_saved']}")
            report.append(f"Manual final error: {conv['manual_final_error']:.2f}%")
            report.append(f"Automated final error: {conv['automated_final_error']:.2f}%")
            
            if conv['convergence_rate_improvement']:
                report.append(f"Convergence rate improvement: {conv['convergence_rate_improvement']:.1f}%")
            report.append("")
        
        # Tension comparison
        tensions = self.validation_results.get('tensions', {})
        if tensions:
            report.append("FINAL TENSION COMPARISON")
            report.append("-"*30)
            report.append(f"Maximum difference: {tensions['max_difference']:.2f} kN")
            report.append(f"Average difference: {tensions['avg_difference']:.2f} kN")
            report.append(f"All within tolerance: {'YES' if tensions['all_within_tolerance'] else 'NO'}")
            
            # Show lines with largest differences
            if tensions['line_differences']:
                report.append("\nLargest differences:")
                sorted_diffs = sorted(
                    tensions['line_differences'].items(),
                    key=lambda x: x[1]['difference'],
                    reverse=True
                )[:5]
                
                for line, data in sorted_diffs:
                    report.append(f"  {line}: {data['difference']:.2f} kN "
                                f"(Manual: {data['manual']:.1f}, Auto: {data['automated']:.1f})")
            report.append("")
        
        # Efficiency comparison
        eff = self.validation_results.get('efficiency', {})
        if eff and eff['manual_time_minutes']:
            report.append("TIME EFFICIENCY")
            report.append("-"*30)
            report.append(f"Manual time: {eff['manual_time_minutes']:.1f} minutes")
            report.append(f"Automated time: {eff['automated_time_minutes']:.1f} minutes")
            report.append(f"Time saved: {eff['time_saved_minutes']:.1f} minutes")
            report.append(f"Time reduction: {eff['time_reduction_percent']:.1f}%")
            report.append("")
        
        # Overall validation
        report.append("OVERALL VALIDATION")
        report.append("-"*30)
        
        validation_passed = True
        validation_notes = []
        
        if conv and conv['automated_final_error'] and conv['manual_final_error']:
            if abs(conv['automated_final_error'] - conv['manual_final_error']) > 0.5:
                validation_notes.append("⚠ Final errors differ by more than 0.5%")
                validation_passed = False
        
        if tensions and not tensions['all_within_tolerance']:
            validation_notes.append("⚠ Some tensions exceed tolerance")
            validation_passed = False
        
        if validation_passed:
            report.append("✅ VALIDATION PASSED")
            report.append("Automated system produces equivalent results")
        else:
            report.append("⚠ VALIDATION REQUIRES REVIEW")
            for note in validation_notes:
                report.append(f"  {note}")
        
        return "\n".join(report)
    
    def plot_comparison(self):
        """Generate comparison plots"""
        manual_df = self.load_manual_results()
        if manual_df is None:
            return
        
        automated = self.load_automated_results()
        
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        
        # Convergence comparison
        ax = axes[0, 0]
        manual_errors = manual_df['Max_Error_%'].values
        auto_errors = [entry['max_error'] for entry in automated]
        
        ax.plot(range(1, len(manual_errors)+1), manual_errors, 'b-o', label='Manual')
        ax.plot(range(1, len(auto_errors)+1), auto_errors, 'r-s', label='Automated')
        ax.set_xlabel('Iteration')
        ax.set_ylabel('Max Error (%)')
        ax.set_title('Convergence Comparison')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Time comparison
        ax = axes[0, 1]
        if 'Time_Minutes' in manual_df:
            manual_times = manual_df['Time_Minutes'].cumsum()
            auto_times = np.cumsum([e.get('analysis_time', 0)/60 for e in automated])
            
            ax.plot(range(1, len(manual_times)+1), manual_times, 'b-o', label='Manual')
            ax.plot(range(1, len(auto_times)+1), auto_times, 'r-s', label='Automated')
            ax.set_xlabel('Iteration')
            ax.set_ylabel('Cumulative Time (minutes)')
            ax.set_title('Time Efficiency')
            ax.legend()
            ax.grid(True, alpha=0.3)
        
        # Final tension comparison
        ax = axes[1, 0]
        tensions = self.validation_results.get('tensions', {})
        if tensions and tensions['line_differences']:
            lines = list(tensions['line_differences'].keys())[:10]  # First 10 lines
            manual_vals = [tensions['line_differences'][l]['manual'] for l in lines]
            auto_vals = [tensions['line_differences'][l]['automated'] for l in lines]
            
            x = np.arange(len(lines))
            width = 0.35
            
            ax.bar(x - width/2, manual_vals, width, label='Manual', color='blue', alpha=0.7)
            ax.bar(x + width/2, auto_vals, width, label='Automated', color='red', alpha=0.7)
            ax.set_xlabel('Line')
            ax.set_ylabel('Final Tension (kN)')
            ax.set_title('Final Tension Comparison')
            ax.set_xticks(x)
            ax.set_xticklabels(lines, rotation=45)
            ax.legend()
        
        # Summary statistics
        ax = axes[1, 1]
        ax.axis('off')
        
        summary_text = self.generate_validation_report().split('\n')[25:35]  # Key metrics
        ax.text(0.1, 0.5, '\n'.join(summary_text), fontsize=10, family='monospace')
        
        plt.suptitle('Manual vs Automated Process Validation', fontsize=14, fontweight='bold')
        plt.tight_layout()
        
        # Save plot
        plot_path = self.automated_path / f"validation_comparison_{datetime.now():%Y%m%d_%H%M%S}.png"
        plt.savefig(plot_path, dpi=150, bbox_inches='tight')
        plt.show()
        
        print(f"Comparison plot saved to: {plot_path}")


def main():
    """Run validation"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Validate automated system against manual process')
    parser.add_argument('--manual', '-m', required=True, help='Path to manual results')
    parser.add_argument('--automated', '-a', required=True, help='Path to automated output')
    parser.add_argument('--tolerance', '-t', type=float, default=1.0, help='Tension tolerance (kN)')
    parser.add_argument('--plot', '-p', action='store_true', help='Generate comparison plots')
    
    args = parser.parse_args()
    
    # Create validator
    validator = ManualProcessValidator(
        manual_results_path=Path(args.manual),
        automated_results_path=Path(args.automated)
    )
    
    # Run comparisons
    print("Running validation...")
    validator.compare_convergence()
    validator.compare_final_tensions(tolerance=args.tolerance)
    validator.compare_time_efficiency()
    
    # Generate report
    report = validator.generate_validation_report()
    print("\n" + report)
    
    # Save report
    report_path = Path(args.automated) / f"validation_report_{datetime.now():%Y%m%d_%H%M%S}.txt"
    with open(report_path, 'w') as f:
        f.write(report)
    print(f"\nReport saved to: {report_path}")
    
    # Generate plots if requested
    if args.plot:
        validator.plot_comparison()


if __name__ == "__main__":
    main()