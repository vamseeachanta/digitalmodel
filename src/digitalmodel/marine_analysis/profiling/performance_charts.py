"""
Performance Visualization Script

Generates comprehensive performance charts from profiling data including:
- Runtime comparison bar charts
- Memory usage plots
- Bottleneck analysis
- Speedup factors
- Scaling analysis
- Performance trends

Usage:
    python scripts/generate_performance_charts.py --input outputs/profiling/metrics
    python scripts/generate_performance_charts.py --baseline baseline_metrics.json
"""

import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from typing import Dict, List, Optional
import argparse
from datetime import datetime

# Set style
sns.set_style("whitegrid")
plt.rcParams['figure.figsize'] = (12, 8)
plt.rcParams['font.size'] = 10


class PerformanceVisualizer:
    """Generate performance visualization charts."""

    def __init__(self, metrics_file: str, output_dir: str = "docs/charts/phase3/performance"):
        """Initialize visualizer.

        Args:
            metrics_file: Path to metrics JSON file
            output_dir: Output directory for charts
        """
        self.metrics_file = Path(metrics_file)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Load metrics
        with open(self.metrics_file, 'r') as f:
            self.data = json.load(f)

        self.metrics = pd.DataFrame(self.data['metrics'])
        self.timestamp = self.data['timestamp']

    def plot_runtime_comparison(self):
        """Generate runtime comparison bar chart."""
        fig, ax = plt.subplots(figsize=(12, 8))

        # Prepare data
        x = np.arange(len(self.metrics))
        width = 0.35

        actual_times = self.metrics['execution_time_ms'].values
        target_times = self.metrics['target_time_ms'].values

        # Create bars
        bars1 = ax.bar(x - width/2, actual_times, width, label='Actual Time',
                      color=['green' if m else 'red'
                             for m in self.metrics['meets_target']], alpha=0.7)
        bars2 = ax.bar(x + width/2, target_times, width, label='Target Time',
                      color='blue', alpha=0.5)

        # Labels and formatting
        ax.set_xlabel('Test Case', fontsize=12, fontweight='bold')
        ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
        ax.set_title('Performance: Actual vs Target Execution Time', fontsize=14, fontweight='bold')
        ax.set_xticks(x)
        ax.set_xticklabels(self.metrics['test_name'], rotation=45, ha='right')
        ax.legend(fontsize=10)
        ax.grid(True, alpha=0.3, axis='y')

        # Add value labels on bars
        for bar in bars1:
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{height:.2f}',
                   ha='center', va='bottom', fontsize=8)

        plt.tight_layout()
        plt.savefig(self.output_dir / '01_runtime_comparison.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 01_runtime_comparison.png")

    def plot_memory_usage(self):
        """Generate memory usage plot."""
        fig, ax = plt.subplots(figsize=(12, 8))

        modules = self.metrics['module_name'].unique()
        colors = plt.cm.Set3(np.linspace(0, 1, len(modules)))

        for i, module in enumerate(modules):
            module_data = self.metrics[self.metrics['module_name'] == module]
            ax.bar(module_data['test_name'], module_data['memory_usage_mb'],
                  label=module, color=colors[i], alpha=0.7)

        ax.set_xlabel('Test Case', fontsize=12, fontweight='bold')
        ax.set_ylabel('Memory Usage (MB)', fontsize=12, fontweight='bold')
        ax.set_title('Memory Usage by Test Case', fontsize=14, fontweight='bold')
        ax.legend(fontsize=10)
        ax.grid(True, alpha=0.3, axis='y')
        plt.xticks(rotation=45, ha='right')
        plt.tight_layout()
        plt.savefig(self.output_dir / '02_memory_usage.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 02_memory_usage.png")

    def plot_performance_ratio(self):
        """Generate performance ratio chart (actual/target)."""
        fig, ax = plt.subplots(figsize=(12, 8))

        # Calculate ratio
        ratio = self.metrics['execution_time_ms'] / self.metrics['target_time_ms']

        # Create bars
        colors = ['green' if r < 1.0 else 'orange' if r < 2.0 else 'red' for r in ratio]
        bars = ax.barh(self.metrics['test_name'], ratio, color=colors, alpha=0.7)

        # Add reference line at 1.0
        ax.axvline(x=1.0, color='blue', linestyle='--', linewidth=2, label='Target (1.0x)')

        # Labels and formatting
        ax.set_xlabel('Performance Ratio (Actual / Target)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Test Case', fontsize=12, fontweight='bold')
        ax.set_title('Performance Ratio: Actual Time vs Target\n(<1.0 = Better than target, >1.0 = Slower than target)',
                    fontsize=14, fontweight='bold')
        ax.legend(fontsize=10)
        ax.grid(True, alpha=0.3, axis='x')

        # Add value labels
        for bar, r in zip(bars, ratio):
            width = bar.get_width()
            ax.text(width, bar.get_y() + bar.get_height()/2.,
                   f'{r:.2f}x',
                   ha='left', va='center', fontsize=8, fontweight='bold')

        plt.tight_layout()
        plt.savefig(self.output_dir / '03_performance_ratio.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 03_performance_ratio.png")

    def plot_module_summary(self):
        """Generate module-level summary chart."""
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        summary = self.data['summary']['by_module']
        modules = list(summary.keys())

        # Chart 1: Average execution time by module
        avg_times = [summary[m]['avg_time_ms'] for m in modules]
        bars1 = ax1.bar(modules, avg_times, color='steelblue', alpha=0.7)
        ax1.set_xlabel('Module', fontsize=11, fontweight='bold')
        ax1.set_ylabel('Average Time (ms)', fontsize=11, fontweight='bold')
        ax1.set_title('Average Execution Time by Module', fontsize=12, fontweight='bold')
        ax1.grid(True, alpha=0.3, axis='y')
        plt.setp(ax1.xaxis.get_majorticklabels(), rotation=45, ha='right')

        for bar in bars1:
            height = bar.get_height()
            ax1.text(bar.get_x() + bar.get_width()/2., height,
                    f'{height:.2f}',
                    ha='center', va='bottom', fontsize=8)

        # Chart 2: Pass rate by module
        pass_rates = [summary[m]['passed'] / summary[m]['tests'] * 100 for m in modules]
        bars2 = ax2.bar(modules, pass_rates, color='green', alpha=0.7)
        ax2.set_xlabel('Module', fontsize=11, fontweight='bold')
        ax2.set_ylabel('Pass Rate (%)', fontsize=11, fontweight='bold')
        ax2.set_title('Performance Target Pass Rate by Module', fontsize=12, fontweight='bold')
        ax2.set_ylim(0, 105)
        ax2.grid(True, alpha=0.3, axis='y')
        ax2.axhline(y=100, color='blue', linestyle='--', linewidth=1, alpha=0.5)
        plt.setp(ax2.xaxis.get_majorticklabels(), rotation=45, ha='right')

        for bar in bars2:
            height = bar.get_height()
            ax2.text(bar.get_x() + bar.get_width()/2., height,
                    f'{height:.0f}%',
                    ha='center', va='bottom', fontsize=8)

        plt.tight_layout()
        plt.savefig(self.output_dir / '04_module_summary.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 04_module_summary.png")

    def plot_scaling_analysis(self):
        """Generate scaling analysis chart."""
        fig, ax = plt.subplots(figsize=(12, 8))

        # Group by module
        for module in self.metrics['module_name'].unique():
            module_data = self.metrics[self.metrics['module_name'] == module]

            # Plot time vs iterations (log scale)
            iterations = module_data['iterations'].values
            times = module_data['execution_time_ms'].values

            if len(iterations) > 0:
                ax.scatter(iterations, times, label=module, s=100, alpha=0.6)

        ax.set_xscale('log')
        ax.set_xlabel('Number of Iterations', fontsize=12, fontweight='bold')
        ax.set_ylabel('Execution Time (ms)', fontsize=12, fontweight='bold')
        ax.set_title('Scaling Analysis: Execution Time vs Problem Size', fontsize=14, fontweight='bold')
        ax.legend(fontsize=10)
        ax.grid(True, alpha=0.3, which='both')

        plt.tight_layout()
        plt.savefig(self.output_dir / '05_scaling_analysis.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 05_scaling_analysis.png")

    def plot_performance_heatmap(self):
        """Generate performance heatmap."""
        fig, ax = plt.subplots(figsize=(10, 8))

        # Create matrix: modules × metrics
        modules = self.metrics['module_name'].unique()
        metric_types = ['execution_time_ms', 'memory_usage_mb', 'target_time_ms']

        matrix = np.zeros((len(modules), len(metric_types)))

        for i, module in enumerate(modules):
            module_data = self.metrics[self.metrics['module_name'] == module]
            matrix[i, 0] = module_data['execution_time_ms'].mean()
            matrix[i, 1] = module_data['memory_usage_mb'].mean()
            matrix[i, 2] = module_data['target_time_ms'].mean()

        # Normalize for visualization
        matrix_normalized = matrix / matrix.max(axis=0)

        # Create heatmap
        im = ax.imshow(matrix_normalized, cmap='RdYlGn_r', aspect='auto', vmin=0, vmax=1)

        # Labels
        ax.set_xticks(np.arange(len(metric_types)))
        ax.set_yticks(np.arange(len(modules)))
        ax.set_xticklabels(['Execution Time', 'Memory Usage', 'Target Time'])
        ax.set_yticklabels(modules)

        # Rotate labels
        plt.setp(ax.get_xticklabels(), rotation=45, ha="right", rotation_mode="anchor")

        # Add values
        for i in range(len(modules)):
            for j in range(len(metric_types)):
                text = ax.text(j, i, f'{matrix[i, j]:.1f}',
                             ha="center", va="center", color="black", fontsize=8)

        ax.set_title('Performance Heatmap (Normalized)', fontsize=14, fontweight='bold')
        fig.colorbar(im, ax=ax, label='Normalized Value')

        plt.tight_layout()
        plt.savefig(self.output_dir / '06_performance_heatmap.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 06_performance_heatmap.png")

    def plot_target_achievement(self):
        """Generate target achievement chart."""
        fig, ax = plt.subplots(figsize=(10, 6))

        # Count pass/fail
        pass_count = self.metrics['meets_target'].sum()
        fail_count = len(self.metrics) - pass_count

        # Pie chart
        sizes = [pass_count, fail_count]
        labels = [f'Pass ({pass_count})', f'Fail ({fail_count})']
        colors = ['#90EE90', '#FFB6C1']
        explode = (0.05, 0)

        ax.pie(sizes, explode=explode, labels=labels, colors=colors,
               autopct='%1.1f%%', shadow=True, startangle=90,
               textprops={'fontsize': 12, 'fontweight': 'bold'})

        ax.set_title(f'Performance Target Achievement\n({pass_count}/{len(self.metrics)} tests passed)',
                    fontsize=14, fontweight='bold')

        plt.tight_layout()
        plt.savefig(self.output_dir / '07_target_achievement.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 07_target_achievement.png")

    def plot_bottleneck_analysis(self):
        """Generate bottleneck analysis chart."""
        fig, ax = plt.subplots(figsize=(12, 8))

        # Identify bottlenecks (tests that exceed target)
        bottlenecks = self.metrics[~self.metrics['meets_target']].copy()

        if len(bottlenecks) > 0:
            # Calculate how much slower than target
            bottlenecks['slowdown'] = (bottlenecks['execution_time_ms'] /
                                       bottlenecks['target_time_ms'])

            # Sort by slowdown
            bottlenecks = bottlenecks.sort_values('slowdown', ascending=True)

            # Plot
            colors = ['red' if s > 2.0 else 'orange' for s in bottlenecks['slowdown']]
            bars = ax.barh(bottlenecks['test_name'], bottlenecks['slowdown'],
                          color=colors, alpha=0.7)

            ax.axvline(x=1.0, color='blue', linestyle='--', linewidth=2, label='Target')
            ax.axvline(x=2.0, color='red', linestyle=':', linewidth=2, label='2x Slowdown', alpha=0.5)

            ax.set_xlabel('Slowdown Factor (Actual / Target)', fontsize=12, fontweight='bold')
            ax.set_ylabel('Test Case', fontsize=12, fontweight='bold')
            ax.set_title('Bottleneck Analysis: Tests Exceeding Target Performance',
                        fontsize=14, fontweight='bold')
            ax.legend(fontsize=10)
            ax.grid(True, alpha=0.3, axis='x')

            # Add values
            for bar, s in zip(bars, bottlenecks['slowdown']):
                width = bar.get_width()
                ax.text(width, bar.get_y() + bar.get_height()/2.,
                       f'{s:.2f}x',
                       ha='left', va='center', fontsize=9, fontweight='bold')
        else:
            ax.text(0.5, 0.5, 'No Bottlenecks Found!\nAll tests meet performance targets.',
                   ha='center', va='center', fontsize=16, fontweight='bold', color='green',
                   transform=ax.transAxes)
            ax.axis('off')

        plt.tight_layout()
        plt.savefig(self.output_dir / '08_bottleneck_analysis.png', dpi=300, bbox_inches='tight')
        plt.close()
        print(f"[OK] Generated: 08_bottleneck_analysis.png")

    def generate_all_charts(self):
        """Generate all performance charts."""
        print("\n" + "="*60)
        print("GENERATING PERFORMANCE CHARTS")
        print("="*60)

        self.plot_runtime_comparison()
        self.plot_memory_usage()
        self.plot_performance_ratio()
        self.plot_module_summary()
        self.plot_scaling_analysis()
        self.plot_performance_heatmap()
        self.plot_target_achievement()
        self.plot_bottleneck_analysis()

        print(f"\n[OK] All charts saved to: {self.output_dir}")
        print("="*60)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Generate performance charts")
    parser.add_argument('--input', required=True, help='Path to metrics JSON file')
    parser.add_argument('--output', default='docs/charts/phase3/performance',
                       help='Output directory for charts')

    args = parser.parse_args()

    visualizer = PerformanceVisualizer(args.input, args.output)
    visualizer.generate_all_charts()


if __name__ == '__main__':
    main()
