"""
Comprehensive Benchmark Harness for OrcaFlex Optimization
Tests performance across different file sizes and thread configurations
"""

import os
import json
import time
import logging
import psutil
import numpy as np
from pathlib import Path
from typing import Dict, List, Any, Tuple
from datetime import datetime
from dataclasses import dataclass, asdict
import matplotlib.pyplot as plt

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available - using mock mode")

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


@dataclass
class BenchmarkResult:
    """Benchmark result data structure."""
    file_size_mb: float
    file_count: int
    thread_count: int
    total_time: float
    avg_time_per_file: float
    throughput_files_per_sec: float
    memory_used_gb: float
    cpu_utilization: float
    optimization_type: str  # 'baseline', 'fixed', 'dynamic'
    speedup_vs_baseline: float = 1.0
    

class ComprehensiveBenchmark:
    """
    Comprehensive benchmark harness for OrcaFlex optimization validation.
    """
    
    def __init__(self, output_dir: str = "./benchmark_results"):
        """
        Initialize benchmark harness.
        
        Args:
            output_dir: Directory for benchmark results
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.results = []
        self.baseline_times = {}
        
    def create_test_datasets(self) -> Dict[str, List[str]]:
        """
        Create or identify test datasets by size category.
        
        Returns:
            Dictionary of file lists by size category
        """
        datasets = {
            'small': [],   # <100MB files
            'medium': [],  # 100-500MB files
            'large': [],   # 500MB-1GB files
            'xlarge': []   # >1GB files
        }
        
        # First check repository for test .sim files
        repo_dirs = [
            Path("D:/github/digitalmodel/src/baseline_results"),
            Path("D:/github/digitalmodel/src/optimized_results"),
            Path("D:/github/digitalmodel/src/single_file_test_results"),
            Path("D:/github/digitalmodel/tests/modules/orcaflex")
        ]
        
        for repo_dir in repo_dirs:
            if repo_dir.exists():
                for sim_file in repo_dir.glob("*.sim"):
                    size_mb = sim_file.stat().st_size / 1e6
                    if size_mb < 100:
                        datasets['small'].append(str(sim_file))
                    elif size_mb < 500:
                        datasets['medium'].append(str(sim_file))
                    elif size_mb < 1000:
                        datasets['large'].append(str(sim_file))
                    else:
                        datasets['xlarge'].append(str(sim_file))
        
        # Also check runtime test directory for real production files
        runtime_dir = Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test")
        if runtime_dir.exists():
            for sim_file in runtime_dir.glob("*.sim"):
                size_mb = sim_file.stat().st_size / 1e6
                # Add to appropriate category if not already present
                file_str = str(sim_file)
                if size_mb < 100 and file_str not in datasets['small']:
                    datasets['small'].append(file_str)
                elif size_mb < 500 and file_str not in datasets['medium']:
                    datasets['medium'].append(file_str)
                elif size_mb < 1000 and file_str not in datasets['large']:
                    datasets['large'].append(file_str)
                elif size_mb >= 1000 and file_str not in datasets['xlarge']:
                    datasets['xlarge'].append(file_str)
        
        # Log dataset composition
        logger.info("Test datasets prepared:")
        for category, files in datasets.items():
            if files:
                avg_size = sum(os.path.getsize(f)/1e6 for f in files) / len(files)
                logger.info(f"  {category}: {len(files)} files, avg {avg_size:.1f}MB")
            else:
                logger.info(f"  {category}: No files found")
                
        return datasets
    
    def run_benchmark(self, 
                     files: List[str], 
                     thread_count: int = None,
                     optimization_type: str = "baseline") -> BenchmarkResult:
        """
        Run a single benchmark test.
        
        Args:
            files: List of files to process
            thread_count: Number of threads to use
            optimization_type: Type of optimization being tested
            
        Returns:
            Benchmark result
        """
        if not files:
            return None
        
        # Determine actual thread count for dynamic optimization
        actual_thread_count = thread_count
        if thread_count is None:
            from digitalmodel.solvers.orcaflex.performance_monitor import ResourceManager
            rm = ResourceManager()
            actual_thread_count = rm.calculate_optimal_threads(files)
            logger.info(f"Running {optimization_type} benchmark with dynamically selected {actual_thread_count} threads on {len(files)} files")
        else:
            logger.info(f"Running {optimization_type} benchmark with {thread_count} threads on {len(files)} files")
        
        # Calculate file statistics
        total_size_mb = sum(os.path.getsize(f)/1e6 for f in files if os.path.exists(f))
        avg_size_mb = total_size_mb / len(files) if files else 0
        
        # Record initial state
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1e9
        
        # Run the benchmark
        start_time = time.time()
        
        if ORCAFLEX_AVAILABLE:
            # Use actual OrcaFlex processing
            from digitalmodel.solvers.orcaflex.orcaflex_optimized_parallel_v2 import (
                OrcaFlexOptimizedParallelAnalysis
            )
            
            # Create analyzer with fixed thread count
            analyzer = OrcaFlexOptimizedParallelAnalysis(num_threads=thread_count)
            
            try:
                results = analyzer.process_files_parallel(files)
                successful = results.get('successful', 0)
            except Exception as e:
                logger.error(f"Benchmark failed: {e}")
                successful = 0
        else:
            # Mock mode - simulate processing
            time.sleep(len(files) * 0.1 / max(actual_thread_count, 1))  # Simulate work
            successful = len(files)
        
        # Calculate metrics
        total_time = time.time() - start_time
        final_memory = process.memory_info().rss / 1e9
        memory_used = final_memory - initial_memory
        
        # Create result with actual thread count used
        result = BenchmarkResult(
            file_size_mb=avg_size_mb,
            file_count=len(files),
            thread_count=actual_thread_count,  # Use actual count (important for dynamic)
            total_time=total_time,
            avg_time_per_file=total_time / len(files) if files else 0,
            throughput_files_per_sec=len(files) / total_time if total_time > 0 else 0,
            memory_used_gb=memory_used,
            cpu_utilization=psutil.cpu_percent(interval=0.1),
            optimization_type=optimization_type
        )
        
        logger.info(f"  Completed in {total_time:.2f}s, throughput: {result.throughput_files_per_sec:.2f} files/s")
        
        return result
    
    def run_comparison_suite(self, datasets: Dict[str, List[str]]) -> Dict[str, Any]:
        """
        Run complete comparison suite across all configurations.
        
        Args:
            datasets: Dictionary of file lists by category
            
        Returns:
            Comparison results
        """
        logger.info("\n" + "="*60)
        logger.info("COMPREHENSIVE BENCHMARK SUITE")
        logger.info("="*60)
        
        all_results = []
        
        # Test configurations
        test_configs = [
            # Baseline (old default)
            ('baseline', 30, False),
            # New fixed default
            ('fixed_optimized', 15, False),
            # Dynamic optimization
            ('dynamic', None, True),
            # Extreme configurations for validation
            ('minimal_threads', 5, False),
            ('maximal_threads', 45, False),
        ]
        
        # Run benchmarks for each dataset and configuration
        for category, files in datasets.items():
            if not files:
                continue
                
            logger.info(f"\nTesting {category} files ({len(files)} files)...")
            category_results = []
            
            for config_name, thread_count, use_dynamic in test_configs:
                if use_dynamic:
                    # Use dynamic optimization - pass None to enable auto-detection
                    thread_count = None
                    logger.info(f"  Using dynamic optimization (auto-detect threads)")
                
                result = self.run_benchmark(files, thread_count, config_name)
                if result:
                    # Calculate speedup vs baseline
                    baseline_key = f"{category}_baseline"
                    if baseline_key not in self.baseline_times and config_name == 'baseline':
                        self.baseline_times[baseline_key] = result.total_time
                    
                    if baseline_key in self.baseline_times:
                        result.speedup_vs_baseline = self.baseline_times[baseline_key] / result.total_time
                    
                    category_results.append(result)
                    all_results.append(result)
            
            # Find best configuration for this category
            if category_results:
                best = min(category_results, key=lambda r: r.total_time)
                logger.info(f"  Best for {category}: {best.optimization_type} "
                          f"({best.thread_count} threads, {best.speedup_vs_baseline:.2f}x speedup)")
        
        # Generate summary
        summary = self.generate_summary(all_results)
        
        # Save results
        self.save_results(all_results, summary)
        
        # Create visualizations
        self.create_visualizations(all_results)
        
        return summary
    
    def generate_summary(self, results: List[BenchmarkResult]) -> Dict[str, Any]:
        """
        Generate summary statistics from benchmark results.
        
        Args:
            results: List of benchmark results
            
        Returns:
            Summary dictionary
        """
        if not results:
            return {}
        
        # Group by optimization type
        by_type = {}
        for result in results:
            if result.optimization_type not in by_type:
                by_type[result.optimization_type] = []
            by_type[result.optimization_type].append(result)
        
        summary = {
            'timestamp': datetime.now().isoformat(),
            'total_benchmarks': len(results),
            'configurations_tested': len(by_type),
            'by_optimization': {}
        }
        
        # Analyze each optimization type
        for opt_type, type_results in by_type.items():
            avg_speedup = np.mean([r.speedup_vs_baseline for r in type_results])
            avg_throughput = np.mean([r.throughput_files_per_sec for r in type_results])
            
            summary['by_optimization'][opt_type] = {
                'avg_speedup': avg_speedup,
                'avg_throughput': avg_throughput,
                'total_time': sum(r.total_time for r in type_results),
                'tests_run': len(type_results)
            }
        
        # Find overall best
        best_result = max(results, key=lambda r: r.speedup_vs_baseline)
        summary['best_configuration'] = {
            'optimization': best_result.optimization_type,
            'threads': best_result.thread_count,
            'speedup': best_result.speedup_vs_baseline,
            'file_size_category': self._get_size_category(best_result.file_size_mb)
        }
        
        return summary
    
    def _get_size_category(self, size_mb: float) -> str:
        """Get size category for a file size."""
        if size_mb < 100:
            return 'small'
        elif size_mb < 500:
            return 'medium'
        elif size_mb < 1000:
            return 'large'
        else:
            return 'xlarge'
    
    def save_results(self, results: List[BenchmarkResult], summary: Dict[str, Any]):
        """
        Save benchmark results to files.
        
        Args:
            results: List of benchmark results
            summary: Summary dictionary
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save detailed results as JSON
        results_file = self.output_dir / f"benchmark_results_{timestamp}.json"
        results_data = [asdict(r) for r in results]
        
        with open(results_file, 'w') as f:
            json.dump({
                'results': results_data,
                'summary': summary
            }, f, indent=2)
        
        logger.info(f"Results saved to {results_file}")
        
        # Save summary as markdown
        summary_file = self.output_dir / f"benchmark_summary_{timestamp}.md"
        self._write_markdown_summary(summary_file, summary, results)
        
        logger.info(f"Summary saved to {summary_file}")
    
    def _write_markdown_summary(self, filepath: Path, summary: Dict[str, Any], results: List[BenchmarkResult]):
        """Write markdown summary report."""
        with open(filepath, 'w') as f:
            f.write("# Benchmark Results Summary\n\n")
            f.write(f"**Date**: {summary['timestamp']}\n")
            f.write(f"**Total Benchmarks**: {summary['total_benchmarks']}\n\n")
            
            f.write("## Best Configuration\n")
            best = summary['best_configuration']
            f.write(f"- **Optimization**: {best['optimization']}\n")
            f.write(f"- **Threads**: {best['threads']}\n")
            f.write(f"- **Speedup**: {best['speedup']:.2f}x\n")
            f.write(f"- **File Category**: {best['file_size_category']}\n\n")
            
            f.write("## Performance by Optimization Type\n\n")
            f.write("| Optimization | Avg Speedup | Avg Throughput | Total Time |\n")
            f.write("|-------------|-------------|----------------|------------|\n")
            
            for opt_type, stats in summary['by_optimization'].items():
                f.write(f"| {opt_type} | {stats['avg_speedup']:.2f}x | "
                       f"{stats['avg_throughput']:.2f} files/s | "
                       f"{stats['total_time']:.2f}s |\n")
            
            f.write("\n## Detailed Results\n\n")
            f.write("| File Size | Threads | Optimization | Time (s) | Speedup |\n")
            f.write("|-----------|---------|--------------|----------|----------|\n")
            
            for r in sorted(results, key=lambda x: (x.file_size_mb, x.thread_count)):
                f.write(f"| {r.file_size_mb:.1f}MB | {r.thread_count} | "
                       f"{r.optimization_type} | {r.total_time:.2f} | "
                       f"{r.speedup_vs_baseline:.2f}x |\n")
    
    def create_visualizations(self, results: List[BenchmarkResult]):
        """
        Create visualization charts from results.
        
        Args:
            results: List of benchmark results
        """
        if not results:
            return
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Create figure with subplots
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        fig.suptitle('OrcaFlex Optimization Benchmark Results', fontsize=16)
        
        # 1. Speedup by optimization type
        ax1 = axes[0, 0]
        by_type = {}
        for r in results:
            if r.optimization_type not in by_type:
                by_type[r.optimization_type] = []
            by_type[r.optimization_type].append(r.speedup_vs_baseline)
        
        opt_types = list(by_type.keys())
        speedups = [np.mean(by_type[t]) for t in opt_types]
        
        ax1.bar(opt_types, speedups)
        ax1.set_xlabel('Optimization Type')
        ax1.set_ylabel('Average Speedup vs Baseline')
        ax1.set_title('Performance Improvement by Optimization')
        ax1.axhline(y=1.0, color='r', linestyle='--', alpha=0.5)
        
        # 2. Thread count vs performance for different file sizes
        ax2 = axes[0, 1]
        
        # Group by file size category
        size_categories = {}
        for r in results:
            category = self._get_size_category(r.file_size_mb)
            if category not in size_categories:
                size_categories[category] = {'threads': [], 'times': []}
            size_categories[category]['threads'].append(r.thread_count)
            size_categories[category]['times'].append(r.total_time)
        
        for category, data in size_categories.items():
            ax2.scatter(data['threads'], data['times'], label=category, alpha=0.6)
        
        ax2.set_xlabel('Thread Count')
        ax2.set_ylabel('Total Time (s)')
        ax2.set_title('Thread Count vs Performance by File Size')
        ax2.legend()
        
        # 3. Throughput comparison
        ax3 = axes[1, 0]
        
        throughputs = {}
        for r in results:
            if r.optimization_type not in throughputs:
                throughputs[r.optimization_type] = []
            throughputs[r.optimization_type].append(r.throughput_files_per_sec)
        
        data_to_plot = [throughputs[t] for t in opt_types]
        ax3.boxplot(data_to_plot, labels=opt_types)
        ax3.set_xlabel('Optimization Type')
        ax3.set_ylabel('Throughput (files/sec)')
        ax3.set_title('Throughput Distribution by Optimization')
        
        # 4. Memory usage
        ax4 = axes[1, 1]
        
        thread_counts = sorted(set(r.thread_count for r in results))
        avg_memory = []
        for tc in thread_counts:
            mem_usage = [r.memory_used_gb for r in results if r.thread_count == tc]
            avg_memory.append(np.mean(mem_usage) if mem_usage else 0)
        
        ax4.plot(thread_counts, avg_memory, 'bo-')
        ax4.set_xlabel('Thread Count')
        ax4.set_ylabel('Memory Usage (GB)')
        ax4.set_title('Memory Usage vs Thread Count')
        ax4.grid(True, alpha=0.3)
        
        # Adjust layout and save
        plt.tight_layout()
        
        chart_file = self.output_dir / f"benchmark_charts_{timestamp}.png"
        plt.savefig(chart_file, dpi=150, bbox_inches='tight')
        plt.close()
        
        logger.info(f"Visualizations saved to {chart_file}")


def run_comprehensive_benchmark():
    """Run the comprehensive benchmark suite."""
    
    print("\n" + "="*60)
    print("ORCAFLEX OPTIMIZATION BENCHMARK HARNESS")
    print("="*60)
    
    # Create benchmark harness
    benchmark = ComprehensiveBenchmark()
    
    # Create or identify test datasets
    datasets = benchmark.create_test_datasets()
    
    if not any(files for files in datasets.values()):
        print("\nNo test files found. Creating mock datasets...")
        # Create mock file lists for testing
        datasets = {
            'small': ['mock_small_1.sim', 'mock_small_2.sim'],
            'medium': ['mock_medium_1.sim'],
            'large': ['mock_large_1.sim'],
            'xlarge': ['mock_xlarge_1.sim']
        }
    
    # Run comprehensive benchmark suite
    summary = benchmark.run_comparison_suite(datasets)
    
    # Print summary
    print("\n" + "="*60)
    print("BENCHMARK COMPLETE")
    print("="*60)
    
    if summary:
        best = summary.get('best_configuration', {})
        print(f"\nBest Configuration:")
        print(f"  Optimization: {best.get('optimization')}")
        print(f"  Thread Count: {best.get('threads')}")
        print(f"  Speedup: {best.get('speedup', 1):.2f}x")
        
        print(f"\nResults saved to: {benchmark.output_dir}")
    
    return summary


if __name__ == "__main__":
    results = run_comprehensive_benchmark()