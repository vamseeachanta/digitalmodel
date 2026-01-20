#!/usr/bin/env python3
"""
Generic Background Test Runner for All Repositories
====================================================
This command runs tests in parallel background processes to verify code changes
before reporting success to users. This addresses the issue of error-prone tests
that take long time to complete.

MEMORY NOTE: Always run tests before telling users about successful features or enhancements.
Run tests using parallel process concepts in the background.
"""

import os
import sys
import time
import json
import subprocess
import threading
from pathlib import Path
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Dict, List, Tuple, Optional, Any
import hashlib

class BackgroundTestRunner:
    """
    Generic background test runner that can be used across all repositories.
    Runs tests in parallel and caches results for efficiency.
    """
    
    def __init__(self, repo_path: Path = None):
        """Initialize the background test runner."""
        self.repo_path = repo_path or Path.cwd()
        self.cache_dir = self.repo_path / '.test-cache'
        self.cache_dir.mkdir(exist_ok=True)
        self.results = {}
        self.test_threads = []
        
    def get_file_hash(self, file_path: Path) -> str:
        """Get hash of a file for cache invalidation."""
        if not file_path.exists():
            return ""
        
        hasher = hashlib.md5()
        with open(file_path, 'rb') as f:
            hasher.update(f.read())
        return hasher.hexdigest()
    
    def get_test_cache_key(self, test_file: Path) -> str:
        """Generate cache key for test results."""
        # Include file hash and modification time
        file_hash = self.get_file_hash(test_file)
        mtime = test_file.stat().st_mtime if test_file.exists() else 0
        return f"{test_file.name}_{file_hash}_{mtime}"
    
    def is_cache_valid(self, test_file: Path, max_age_seconds: int = 3600) -> bool:
        """Check if cached test result is still valid."""
        cache_key = self.get_test_cache_key(test_file)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        if not cache_file.exists():
            return False
        
        # Check age
        age = time.time() - cache_file.stat().st_mtime
        return age < max_age_seconds
    
    def get_cached_result(self, test_file: Path) -> Optional[Dict]:
        """Get cached test result if valid."""
        if not self.is_cache_valid(test_file):
            return None
        
        cache_key = self.get_test_cache_key(test_file)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        try:
            with open(cache_file, 'r') as f:
                return json.load(f)
        except:
            return None
    
    def save_cache_result(self, test_file: Path, result: Dict):
        """Save test result to cache."""
        cache_key = self.get_test_cache_key(test_file)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        with open(cache_file, 'w') as f:
            json.dump(result, f)
    
    def find_test_files(self) -> List[Path]:
        """Find all test files in the repository."""
        test_patterns = [
            '**/test_*.py',
            '**/*_test.py',
            '**/tests.py',
            '**/run_tests.py'
        ]
        
        test_files = []
        for pattern in test_patterns:
            test_files.extend(self.repo_path.glob(pattern))
        
        # Filter out virtual environments and cache directories
        excluded_dirs = {'venv', '.venv', 'env', '__pycache__', '.pytest_cache', 'node_modules'}
        test_files = [
            f for f in test_files 
            if not any(excluded in f.parts for excluded in excluded_dirs)
        ]
        
        return test_files
    
    def run_single_test(self, test_file: Path) -> Dict[str, Any]:
        """Run a single test file and return results."""
        # Check cache first
        cached = self.get_cached_result(test_file)
        if cached:
            cached['from_cache'] = True
            return cached
        
        start_time = time.time()
        result = {
            'test_file': str(test_file),
            'start_time': datetime.now().isoformat(),
            'from_cache': False
        }
        
        try:
            # Determine test runner based on file and available tools
            if 'pytest' in test_file.name or (test_file.parent / 'pytest.ini').exists():
                cmd = [sys.executable, '-m', 'pytest', str(test_file), '-v', '--tb=short']
            else:
                cmd = [sys.executable, str(test_file)]
            
            # Run test
            process = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=60,  # 60 second timeout per test
                cwd=str(test_file.parent)
            )
            
            result['success'] = process.returncode == 0
            result['stdout'] = process.stdout[-1000:]  # Last 1000 chars
            result['stderr'] = process.stderr[-1000:]
            result['return_code'] = process.returncode
            
        except subprocess.TimeoutExpired:
            result['success'] = False
            result['error'] = 'Test timed out after 60 seconds'
            
        except Exception as e:
            result['success'] = False
            result['error'] = str(e)
        
        result['duration'] = time.time() - start_time
        result['end_time'] = datetime.now().isoformat()
        
        # Cache the result
        self.save_cache_result(test_file, result)
        
        return result
    
    def run_tests_parallel(self, test_files: List[Path], max_workers: int = 4) -> Dict[str, Any]:
        """Run multiple test files in parallel."""
        results = {
            'total_tests': len(test_files),
            'passed': 0,
            'failed': 0,
            'from_cache': 0,
            'test_results': [],
            'start_time': datetime.now().isoformat()
        }
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all test jobs
            future_to_test = {
                executor.submit(self.run_single_test, test_file): test_file
                for test_file in test_files
            }
            
            # Collect results as they complete
            for future in as_completed(future_to_test):
                test_file = future_to_test[future]
                try:
                    result = future.result()
                    results['test_results'].append(result)
                    
                    if result['success']:
                        results['passed'] += 1
                    else:
                        results['failed'] += 1
                    
                    if result.get('from_cache'):
                        results['from_cache'] += 1
                        
                except Exception as e:
                    results['test_results'].append({
                        'test_file': str(test_file),
                        'success': False,
                        'error': str(e)
                    })
                    results['failed'] += 1
        
        results['end_time'] = datetime.now().isoformat()
        results['success_rate'] = (results['passed'] / results['total_tests'] * 100) if results['total_tests'] > 0 else 0
        
        return results
    
    def run_tests_background(self, callback=None) -> threading.Thread:
        """
        Run tests in background thread.
        
        Args:
            callback: Optional function to call with results when complete
        """
        def background_worker():
            test_files = self.find_test_files()
            results = self.run_tests_parallel(test_files)
            self.results = results
            
            if callback:
                callback(results)
        
        thread = threading.Thread(target=background_worker, daemon=True)
        thread.start()
        self.test_threads.append(thread)
        return thread
    
    def wait_for_tests(self, timeout: float = None) -> bool:
        """
        Wait for background tests to complete.
        
        Returns:
            True if all tests completed, False if timeout
        """
        for thread in self.test_threads:
            thread.join(timeout)
            if thread.is_alive():
                return False
        return True
    
    def get_quick_validation(self, files_changed: List[Path]) -> Dict[str, Any]:
        """
        Quick validation by running only tests related to changed files.
        """
        # Find tests related to changed files
        relevant_tests = []
        
        for changed_file in files_changed:
            # Look for test file for this module
            if 'test' not in changed_file.name:
                test_name = f"test_{changed_file.stem}.py"
                test_file = changed_file.parent / test_name
                if test_file.exists():
                    relevant_tests.append(test_file)
                
                # Also check tests directory
                tests_dir = changed_file.parent / 'tests'
                if tests_dir.exists():
                    relevant_tests.extend(tests_dir.glob('test_*.py'))
        
        if not relevant_tests:
            # If no specific tests found, run a subset of critical tests
            all_tests = self.find_test_files()
            relevant_tests = all_tests[:5]  # Run first 5 tests as smoke test
        
        return self.run_tests_parallel(relevant_tests, max_workers=2)


class TestResultReporter:
    """Reports test results in a user-friendly format."""
    
    @staticmethod
    def format_results(results: Dict[str, Any]) -> str:
        """Format test results for display."""
        lines = []
        lines.append("=" * 60)
        lines.append("Background Test Results")
        lines.append("=" * 60)
        
        lines.append(f"Total Tests: {results['total_tests']}")
        lines.append(f"Passed: {results['passed']} ✓")
        lines.append(f"Failed: {results['failed']} ✗")
        lines.append(f"From Cache: {results['from_cache']}")
        lines.append(f"Success Rate: {results['success_rate']:.1f}%")
        
        if results['failed'] > 0:
            lines.append("\nFailed Tests:")
            for test_result in results['test_results']:
                if not test_result['success']:
                    lines.append(f"  - {Path(test_result['test_file']).name}")
                    if 'error' in test_result:
                        lines.append(f"    Error: {test_result['error']}")
        
        lines.append("=" * 60)
        return "\n".join(lines)


def main():
    """Main entry point for the background test runner."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Run tests in background with parallel processing'
    )
    parser.add_argument(
        '--path',
        default='.',
        help='Repository path (default: current directory)'
    )
    parser.add_argument(
        '--workers',
        type=int,
        default=4,
        help='Number of parallel workers (default: 4)'
    )
    parser.add_argument(
        '--quick',
        action='store_true',
        help='Run quick validation tests only'
    )
    parser.add_argument(
        '--wait',
        action='store_true',
        help='Wait for tests to complete'
    )
    parser.add_argument(
        '--timeout',
        type=float,
        default=300,
        help='Timeout in seconds (default: 300)'
    )
    
    args = parser.parse_args()
    
    repo_path = Path(args.path).resolve()
    runner = BackgroundTestRunner(repo_path)
    
    print(f"Starting background test runner in: {repo_path}")
    print(f"Using {args.workers} parallel workers")
    
    if args.quick:
        # Quick validation mode
        print("Running quick validation tests...")
        # For demo, use recently modified files
        recent_files = list(repo_path.glob('**/*.py'))[:5]
        results = runner.get_quick_validation(recent_files)
    else:
        # Full test run
        test_files = runner.find_test_files()
        print(f"Found {len(test_files)} test files")
        
        if args.wait:
            # Run and wait
            results = runner.run_tests_parallel(test_files, max_workers=args.workers)
        else:
            # Run in background
            def on_complete(results):
                print("\n" + TestResultReporter.format_results(results))
            
            thread = runner.run_tests_background(callback=on_complete)
            print("Tests running in background...")
            print(f"Thread ID: {thread.ident}")
            
            if args.wait:
                print(f"Waiting up to {args.timeout} seconds for completion...")
                if runner.wait_for_tests(timeout=args.timeout):
                    results = runner.results
                else:
                    print("Tests still running after timeout")
                    return
    
    if 'results' in locals():
        print("\n" + TestResultReporter.format_results(results))
        
        # Exit with appropriate code
        sys.exit(0 if results['failed'] == 0 else 1)


if __name__ == "__main__":
    main()