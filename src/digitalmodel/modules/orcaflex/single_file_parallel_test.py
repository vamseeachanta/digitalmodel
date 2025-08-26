"""
Test Single File Loading with Parallelized Analysis Loops
Alternative strategy: Load one file at a time, parallelize the analysis within
"""

import os
import time
import logging
import psutil
import multiprocessing as mp
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False
    print("Warning: OrcFxAPI not available")

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class SingleFileParallelStrategy:
    """
    Alternative strategy: Load files sequentially, parallelize analysis within each file
    """
    
    def __init__(self):
        self.results = {}
        
    def process_file_sequential_load_parallel_analysis(self, 
                                                      file_path: str,
                                                      num_analysis_threads: int = 15) -> Dict[str, Any]:
        """
        Load file with single thread, then parallelize independent analysis operations.
        
        Args:
            file_path: Path to .sim file
            num_analysis_threads: Number of threads for parallel analysis
            
        Returns:
            Processing results
        """
        result = {
            'file': file_path,
            'file_size_mb': os.path.getsize(file_path) / 1e6 if os.path.exists(file_path) else 0,
            'load_time': 0,
            'analysis_time': 0,
            'save_time': 0,
            'total_time': 0,
            'status': 'pending'
        }
        
        start_total = time.time()
        
        try:
            # STEP 1: Load file with single thread (no competition for I/O)
            logger.info(f"Loading {Path(file_path).name} with SINGLE thread...")
            start_load = time.time()
            model = OrcFxAPI.Model()
            model.LoadData(str(file_path))
            result['load_time'] = time.time() - start_load
            logger.info(f"  Load completed in {result['load_time']:.2f}s")
            
            # STEP 2: Parallelize independent analysis operations
            logger.info(f"Running parallel analysis with {num_analysis_threads} threads...")
            start_analysis = time.time()
            
            # Simulate independent analysis tasks that could be parallelized
            analysis_tasks = [
                ('static_analysis', self._run_static_analysis),
                ('get_line_results', self._get_line_results),
                ('get_vessel_results', self._get_vessel_results),
                ('calculate_statistics', self._calculate_statistics),
            ]
            
            # Run analyses in parallel
            with ThreadPoolExecutor(max_workers=num_analysis_threads) as executor:
                futures = {}
                for task_name, task_func in analysis_tasks:
                    future = executor.submit(task_func, model)
                    futures[future] = task_name
                
                analysis_results = {}
                for future in as_completed(futures):
                    task_name = futures[future]
                    try:
                        analysis_results[task_name] = future.result()
                        logger.info(f"    {task_name} completed")
                    except Exception as e:
                        logger.error(f"    {task_name} failed: {e}")
                        analysis_results[task_name] = {'error': str(e)}
            
            result['analysis_time'] = time.time() - start_analysis
            logger.info(f"  Analysis completed in {result['analysis_time']:.2f}s")
            
            # STEP 3: Save results (single thread again for I/O)
            logger.info("Saving results with SINGLE thread...")
            start_save = time.time()
            output_dir = Path("./single_file_test_results")
            output_dir.mkdir(exist_ok=True)
            output_file = output_dir / f"{Path(file_path).stem}_processed.sim"
            model.SaveSimulation(str(output_file))
            result['save_time'] = time.time() - start_save
            logger.info(f"  Save completed in {result['save_time']:.2f}s")
            
            result['status'] = 'success'
            result['analysis_results'] = analysis_results
            
        except Exception as e:
            logger.error(f"Failed to process {file_path}: {e}")
            result['status'] = 'failed'
            result['error'] = str(e)
        
        result['total_time'] = time.time() - start_total
        return result
    
    def _run_static_analysis(self, model) -> Dict[str, Any]:
        """Run static analysis on model."""
        start = time.time()
        if ORCAFLEX_AVAILABLE:
            model.CalculateStatics()
        else:
            time.sleep(0.5)  # Simulate
        return {'duration': time.time() - start}
    
    def _get_line_results(self, model) -> Dict[str, Any]:
        """Extract line results from model."""
        start = time.time()
        results = {}
        if ORCAFLEX_AVAILABLE:
            # Get results for all lines
            for obj in model.objects:
                if obj.type == OrcFxAPI.otLine:
                    # Simulate getting results
                    results[obj.name] = {'tension': 'calculated'}
        else:
            time.sleep(0.3)  # Simulate
        return {'duration': time.time() - start, 'lines': len(results)}
    
    def _get_vessel_results(self, model) -> Dict[str, Any]:
        """Extract vessel results from model."""
        start = time.time()
        results = {}
        if ORCAFLEX_AVAILABLE:
            for obj in model.objects:
                if obj.type == OrcFxAPI.otVessel:
                    results[obj.name] = {'position': 'calculated'}
        else:
            time.sleep(0.3)  # Simulate
        return {'duration': time.time() - start, 'vessels': len(results)}
    
    def _calculate_statistics(self, model) -> Dict[str, Any]:
        """Calculate statistics from model."""
        start = time.time()
        # Simulate statistical calculations
        time.sleep(0.2)
        return {'duration': time.time() - start, 'stats': 'calculated'}
    
    def compare_strategies(self, file_paths: List[str]) -> Dict[str, Any]:
        """
        Compare different parallelization strategies.
        
        Args:
            file_paths: List of files to test
            
        Returns:
            Comparison results
        """
        logger.info("\n" + "="*60)
        logger.info("COMPARING PARALLELIZATION STRATEGIES")
        logger.info("="*60)
        
        results = {
            'traditional_parallel_files': {},
            'sequential_load_parallel_analysis': {},
            'hybrid_approach': {},
            'comparison': {}
        }
        
        # Test 1: Traditional approach - parallel file processing (15 threads total)
        logger.info("\nTest 1: Traditional Parallel File Processing (15 threads total)")
        start = time.time()
        
        from digitalmodel.modules.orcaflex.orcaflex_parallel_analysis import OrcaFlexParallelAnalysis
        traditional = OrcaFlexParallelAnalysis(num_threads=15)
        trad_results = traditional.process_files_parallel(file_paths)
        
        results['traditional_parallel_files'] = {
            'total_time': time.time() - start,
            'parallel_speedup': trad_results.get('parallel_speedup', 1),
            'successful': trad_results.get('successful', 0),
            'threads_per_file': 15 // len(file_paths) if len(file_paths) > 0 else 15
        }
        
        # Test 2: Sequential load with parallel analysis (1 file at a time, 30 threads each)
        logger.info("\nTest 2: Sequential Load + Parallel Analysis (1 file, 30 threads)")
        start = time.time()
        
        seq_results = []
        for file_path in file_paths:
            logger.info(f"\nProcessing {Path(file_path).name} with 30 threads")
            file_result = self.process_file_sequential_load_parallel_analysis(file_path, num_analysis_threads=30)
            seq_results.append(file_result)
        
        total_time = time.time() - start
        
        results['sequential_load_parallel_analysis'] = {
            'total_time': total_time,
            'avg_load_time': sum(r['load_time'] for r in seq_results) / len(seq_results),
            'avg_analysis_time': sum(r['analysis_time'] for r in seq_results) / len(seq_results),
            'avg_save_time': sum(r['save_time'] for r in seq_results) / len(seq_results),
            'successful': sum(1 for r in seq_results if r['status'] == 'success'),
            'threads_per_file': 30,
            'concurrent_files': 1,
            'details': seq_results
        }
        
        # Test 3: Hybrid approach - 2 files at a time, 20 threads each
        logger.info("\nTest 3: Hybrid Approach (2 concurrent files, 20 threads each)")
        start = time.time()
        
        hybrid_results = []
        # Process files in pairs
        from concurrent.futures import ThreadPoolExecutor, as_completed
        with ThreadPoolExecutor(max_workers=2) as executor:
            futures = []
            for file_path in file_paths:
                future = executor.submit(
                    self.process_file_sequential_load_parallel_analysis,
                    file_path,
                    num_analysis_threads=20
                )
                futures.append((future, file_path))
            
            for future, file_path in futures:
                try:
                    result = future.result(timeout=120)
                    hybrid_results.append(result)
                    logger.info(f"Completed {Path(file_path).name}")
                except Exception as e:
                    logger.error(f"Failed to process {file_path}: {e}")
                    hybrid_results.append({
                        'file': file_path,
                        'status': 'failed',
                        'error': str(e),
                        'load_time': 0,
                        'analysis_time': 0,
                        'save_time': 0
                    })
        
        hybrid_time = time.time() - start
        
        results['hybrid_approach'] = {
            'total_time': hybrid_time,
            'avg_load_time': sum(r['load_time'] for r in hybrid_results) / len(hybrid_results) if hybrid_results else 0,
            'avg_analysis_time': sum(r['analysis_time'] for r in hybrid_results) / len(hybrid_results) if hybrid_results else 0,
            'avg_save_time': sum(r['save_time'] for r in hybrid_results) / len(hybrid_results) if hybrid_results else 0,
            'successful': sum(1 for r in hybrid_results if r.get('status') == 'success'),
            'threads_per_file': 20,
            'concurrent_files': 2,
            'details': hybrid_results
        }
        
        # Compare all three approaches
        trad_time = results['traditional_parallel_files']['total_time']
        seq_time = results['sequential_load_parallel_analysis']['total_time']
        hybrid_time = results['hybrid_approach']['total_time']
        
        # Find the best approach
        times = {
            'Traditional (15 threads)': trad_time,
            'Sequential (1 file, 30 threads)': seq_time,
            'Hybrid (2 files, 20 threads each)': hybrid_time
        }
        best_approach = min(times, key=times.get)
        
        results['comparison'] = {
            'traditional_time': trad_time,
            'sequential_time': seq_time,
            'hybrid_time': hybrid_time,
            'best_approach': best_approach,
            'best_time': times[best_approach],
            'traditional_vs_best': ((trad_time - times[best_approach]) / trad_time * 100) if trad_time > 0 else 0,
            'recommendation': self._generate_recommendation(results)
        }
        
        return results
    
    def _generate_recommendation(self, results: Dict[str, Any]) -> str:
        """Generate recommendation based on test results."""
        comp = results['comparison']
        best = comp['best_approach']
        
        if 'Hybrid' in best:
            return "Hybrid approach optimal: Balances I/O contention (2 files) with CPU parallelization (20 threads/file)"
        elif 'Sequential' in best:
            return "Sequential loading optimal: Eliminates I/O contention, maximizes analysis parallelization (30 threads)"
        else:
            return "Traditional parallel files optimal: Current file sizes benefit from distributed loading"


def run_strategy_comparison():
    """Run comparison of parallelization strategies."""
    
    # Use the real test files
    test_dir = Path("D:/1522/ctr7/orcaflex/rev_a08/runtime_test")
    test_files = [
        str(test_dir / "fsts_l015_hwl_125km3_l100_pb_005yr__cl_015deg.sim"),
        str(test_dir / "fsts_l015_hwl_ncl_000deg.sim")
    ]
    
    # Check files exist
    existing_files = [f for f in test_files if Path(f).exists()]
    
    if not existing_files:
        logger.error("No test files found!")
        return
    
    logger.info(f"Testing with {len(existing_files)} files")
    
    # Run comparison
    tester = SingleFileParallelStrategy()
    results = tester.compare_strategies(existing_files)
    
    # Print summary
    print("\n" + "="*60)
    print("STRATEGY COMPARISON RESULTS")
    print("="*60)
    
    print(f"\n1. Traditional (Parallel Files - 15 threads total):")
    trad = results['traditional_parallel_files']
    print(f"   Total Time: {trad['total_time']:.2f}s")
    print(f"   Threads per file: ~{trad.get('threads_per_file', 'N/A')}")
    print(f"   Speedup: {trad['parallel_speedup']:.2f}x")
    
    print(f"\n2. Sequential Load + Parallel Analysis (1 file, 30 threads):")
    seq = results['sequential_load_parallel_analysis']
    print(f"   Total Time: {seq['total_time']:.2f}s")
    print(f"   Concurrent files: {seq['concurrent_files']}")
    print(f"   Threads per file: {seq['threads_per_file']}")
    print(f"   Avg Load Time: {seq['avg_load_time']:.2f}s")
    print(f"   Avg Analysis Time: {seq['avg_analysis_time']:.2f}s")
    print(f"   Avg Save Time: {seq['avg_save_time']:.2f}s")
    
    print(f"\n3. Hybrid Approach (2 files, 20 threads each):")
    hybrid = results['hybrid_approach']
    print(f"   Total Time: {hybrid['total_time']:.2f}s")
    print(f"   Concurrent files: {hybrid['concurrent_files']}")
    print(f"   Threads per file: {hybrid['threads_per_file']}")
    print(f"   Avg Load Time: {hybrid['avg_load_time']:.2f}s")
    print(f"   Avg Analysis Time: {hybrid['avg_analysis_time']:.2f}s")
    print(f"   Avg Save Time: {hybrid['avg_save_time']:.2f}s")
    
    print(f"\nComparison:")
    comp = results['comparison']
    print(f"  BEST APPROACH: {comp['best_approach']}")
    print(f"  Best Time: {comp['best_time']:.2f}s")
    print(f"  Improvement over Traditional: {comp['traditional_vs_best']:.1f}%")
    print(f"  Recommendation: {comp['recommendation']}")
    print("="*60)
    
    return results


if __name__ == "__main__":
    results = run_strategy_comparison()