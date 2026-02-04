#!/usr/bin/env python
"""
Performance Benchmark for Fatigue Analysis Processing
"""

import sys
import time
import json
from pathlib import Path
from datetime import datetime
from typing import Dict
import pandas as pd
import numpy as np

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.structural.fatigue_apps.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler
)
from digitalmodel.structural.fatigue_apps.strut_foundation_processor_with_progress import (
    BatchProcessor
)

class PerformanceBenchmark:
    """Benchmark the fatigue analysis performance"""
    
    def __init__(self, base_path: str = None):
        self.base_path = Path(base_path) if base_path else Path(__file__).parent / "sample_data"
        self.results = {}
    
    def benchmark_data_loading(self, iterations: int = 10) -> Dict:
        """Benchmark data loading performance"""
        
        print("\n" + "=" * 60)
        print("BENCHMARK: Data Loading Performance")
        print("=" * 60)
        
        handler = ProductionDataHandler(base_path=str(self.base_path))
        
        # Test loading different file sizes
        configs = ['fsts_l015', 'fsts_l095']
        struts = [1, 2, 3, 4]
        
        timings = []
        
        for config in configs:
            for strut in struts:
                # Time loading wind reference
                start = time.perf_counter()
                for _ in range(iterations):
                    time_data, tension_data = handler.load_strut_data(
                        config, 'wind01', strut, use_sample=True
                    )
                end = time.perf_counter()
                
                avg_time = (end - start) / iterations * 1000  # Convert to ms
                timings.append({
                    'config': config,
                    'strut': strut,
                    'reference': 'wind01',
                    'avg_time_ms': avg_time,
                    'samples_loaded': len(tension_data)
                })
        
        df_timings = pd.DataFrame(timings)
        
        print(f"\nData Loading Results ({iterations} iterations each):")
        print(f"  - Average load time: {df_timings['avg_time_ms'].mean():.2f} ms")
        print(f"  - Min load time: {df_timings['avg_time_ms'].min():.2f} ms")
        print(f"  - Max load time: {df_timings['avg_time_ms'].max():.2f} ms")
        print(f"  - Samples per load: {df_timings['samples_loaded'].iloc[0]}")
        
        self.results['data_loading'] = {
            'avg_ms': float(df_timings['avg_time_ms'].mean()),
            'min_ms': float(df_timings['avg_time_ms'].min()),
            'max_ms': float(df_timings['avg_time_ms'].max()),
            'iterations': iterations
        }
        
        return df_timings
    
    def benchmark_processing_speed(self, num_conditions: int = 5) -> Dict:
        """Benchmark processing speed for fatigue analysis"""
        
        print("\n" + "=" * 60)
        print("BENCHMARK: Processing Speed")
        print("=" * 60)
        
        handler = ProductionDataHandler(base_path=str(self.base_path))
        scaler = LoadScaler(handler)
        
        # Create test conditions
        test_conditions = scaler.create_sample_fatigue_conditions()[:num_conditions]
        
        processing_times = []
        
        for config_name in ['fsts_l015', 'fsts_l095']:
            for fc in test_conditions:
                for strut in [1, 2]:
                    start = time.perf_counter()
                    
                    # Process single fatigue condition
                    effective_tension, metadata = scaler.process_fatigue_condition(
                        config_name, fc, strut
                    )
                    
                    end = time.perf_counter()
                    
                    processing_times.append({
                        'config': config_name,
                        'fc_id': fc.id,
                        'strut': strut,
                        'time_ms': (end - start) * 1000,
                        'samples': len(effective_tension)
                    })
        
        df_times = pd.DataFrame(processing_times)
        
        print(f"\nProcessing Speed Results:")
        print(f"  - Total analyses: {len(processing_times)}")
        print(f"  - Average time per analysis: {df_times['time_ms'].mean():.2f} ms")
        print(f"  - Total time: {df_times['time_ms'].sum():.2f} ms")
        
        # Calculate throughput
        total_samples = df_times['samples'].sum()
        total_time_s = df_times['time_ms'].sum() / 1000
        throughput = total_samples / total_time_s if total_time_s > 0 else 0
        
        print(f"  - Throughput: {throughput:.0f} samples/second")
        
        self.results['processing_speed'] = {
            'analyses': len(processing_times),
            'avg_ms': float(df_times['time_ms'].mean()),
            'total_ms': float(df_times['time_ms'].sum()),
            'throughput_samples_per_sec': float(throughput)
        }
        
        return df_times
    
    def benchmark_batch_processing(self) -> None:
        """Benchmark batch processing with parallelization potential"""
        
        print("\n" + "=" * 60)
        print("BENCHMARK: Batch Processing")
        print("=" * 60)
        
        # Create small batch for benchmarking
        from digitalmodel.structural.fatigue_apps.strut_foundation_processor import FatigueCondition
        
        batch_config = {
            "fatigue_conditions": [
                FatigueCondition(
                    id=i, wind_speed=10.0, wind_dir=0.0,
                    hs=0.25, tp=2.7, wave_dir=0.0, occurrence=1.0
                )
                for i in range(1, 4)
            ],
            "struts": [1, 2],
            "output_dir": "output_benchmark"
        }
        
        # Time batch processing
        start = time.perf_counter()
        
        processor = BatchProcessor(base_path=str(self.base_path))
        
        # Temporarily redirect stdout to suppress output
        import io
        import contextlib
        
        f = io.StringIO()
        with contextlib.redirect_stdout(f):
            processor.handler.process_all_configurations(
                batch_config['fatigue_conditions'],
                batch_config['struts'],
                Path(batch_config['output_dir'])
            )
        
        end = time.perf_counter()
        
        batch_time = (end - start) * 1000
        
        # Calculate metrics
        num_configs = 4
        num_conditions = len(batch_config['fatigue_conditions'])
        num_struts = len(batch_config['struts'])
        total_analyses = num_configs * num_conditions * num_struts
        
        print(f"\nBatch Processing Results:")
        print(f"  - Total analyses: {total_analyses}")
        print(f"  - Total time: {batch_time:.2f} ms")
        print(f"  - Time per analysis: {batch_time/total_analyses:.2f} ms")
        print(f"  - Analyses per second: {total_analyses/(batch_time/1000):.1f}")
        
        self.results['batch_processing'] = {
            'total_analyses': total_analyses,
            'total_ms': float(batch_time),
            'per_analysis_ms': float(batch_time/total_analyses),
            'analyses_per_sec': float(total_analyses/(batch_time/1000))
        }
    
    def benchmark_memory_usage(self) -> None:
        """Estimate memory usage"""
        
        print("\n" + "=" * 60)
        print("BENCHMARK: Memory Usage Estimation")
        print("=" * 60)
        
        handler = ProductionDataHandler(base_path=str(self.base_path))
        
        # Load sample data to estimate memory
        time_data, tension_data = handler.load_strut_data('fsts_l015', 'wind01', 1)
        
        # Estimate memory usage
        samples = len(tension_data)
        bytes_per_sample = tension_data.nbytes / samples if samples > 0 else 0
        
        # Estimate for full dataset
        total_configs = 4
        total_struts = 8
        total_references = 34  # 16 wind + 18 wave
        samples_per_file = 1000  # Full file
        
        total_samples = total_configs * total_struts * total_references * samples_per_file
        estimated_memory_mb = (total_samples * bytes_per_sample) / (1024 * 1024)
        
        print(f"\nMemory Usage Estimates:")
        print(f"  - Bytes per sample: {bytes_per_sample:.1f}")
        print(f"  - Samples per file: {samples_per_file}")
        print(f"  - Total dataset samples: {total_samples:,}")
        print(f"  - Estimated total memory: {estimated_memory_mb:.1f} MB")
        
        self.results['memory_usage'] = {
            'bytes_per_sample': float(bytes_per_sample),
            'total_samples': total_samples,
            'estimated_mb': float(estimated_memory_mb)
        }
    
    def save_results(self) -> None:
        """Save benchmark results"""
        
        output_file = Path(__file__).parent / "benchmark_results.json"
        
        self.results['metadata'] = {
            'timestamp': datetime.now().isoformat(),
            'base_path': str(self.base_path)
        }
        
        with open(output_file, 'w') as f:
            json.dump(self.results, f, indent=2)
        
        print(f"\nResults saved to: {output_file}")
    
    def run_all_benchmarks(self) -> None:
        """Run all benchmarks"""
        
        print("\n" + "="*60)
        print("RUNNING PERFORMANCE BENCHMARKS")
        print("="*60)
        
        self.benchmark_data_loading(iterations=10)
        self.benchmark_processing_speed(num_conditions=5)
        self.benchmark_batch_processing()
        self.benchmark_memory_usage()
        
        print("\n" + "="*60)
        print("PERFORMANCE SUMMARY")
        print("="*60)
        
        print("\nKey Metrics:")
        print(f"  - Data loading: {self.results['data_loading']['avg_ms']:.2f} ms average")
        print(f"  - Processing: {self.results['processing_speed']['avg_ms']:.2f} ms per analysis")
        print(f"  - Throughput: {self.results['processing_speed']['throughput_samples_per_sec']:.0f} samples/sec")
        print(f"  - Batch rate: {self.results['batch_processing']['analyses_per_sec']:.1f} analyses/sec")
        print(f"  - Memory estimate: {self.results['memory_usage']['estimated_mb']:.1f} MB for full dataset")
        
        self.save_results()

def main():
    """Main benchmark execution"""
    
    benchmark = PerformanceBenchmark()
    benchmark.run_all_benchmarks()
    
    print("\nBenchmarking complete!")
    return 0

if __name__ == "__main__":
    sys.exit(main())