"""
Module Test Runner for executing tests with subprocess management and result collection.
"""

import os
import sys
import json
import subprocess
import psutil
import time
import tempfile
import threading
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple, Union
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
from concurrent.futures import ThreadPoolExecutor, as_completed, Future
from contextlib import contextmanager

from test_automation.config import config
from test_automation.logging_config import get_logger, log_performance
from test_automation.core.discovery import TestDiscoveryEngine, ModuleInfo, TestFile

logger = get_logger('test_automation.runner')

@dataclass
class TestResult:
    """Represents the result of a single test execution."""
    test_file: str
    module: str
    status: str  # 'passed', 'failed', 'skipped', 'error', 'timeout'
    duration: float
    output: str
    error_output: str
    exit_code: int
    test_count: int = 0
    passed_count: int = 0
    failed_count: int = 0
    skipped_count: int = 0
    collected_count: int = 0
    start_time: Optional[str] = None
    end_time: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return asdict(self)

@dataclass
class ModuleResult:
    """Represents the aggregated results for a module."""
    module_name: str
    status: str  # 'passed', 'failed', 'mixed', 'skipped', 'error'
    total_duration: float
    test_results: List[TestResult]
    total_tests: int = 0
    passed_tests: int = 0
    failed_tests: int = 0
    skipped_tests: int = 0
    error_tests: int = 0
    timeout_tests: int = 0
    start_time: Optional[str] = None
    end_time: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        data = asdict(self)
        data['test_results'] = [tr.to_dict() for tr in self.test_results]
        return data

class ResourceMonitor:
    """Monitor system resources during test execution."""
    
    def __init__(self):
        self.monitoring = False
        self.process = None
        self.peak_memory_mb = 0
        self.peak_cpu_percent = 0
        self.monitor_thread = None
    
    def start_monitoring(self, process: subprocess.Popen):
        """Start monitoring a process."""
        self.process = process
        self.monitoring = True
        self.peak_memory_mb = 0
        self.peak_cpu_percent = 0
        self.monitor_thread = threading.Thread(target=self._monitor_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()
    
    def stop_monitoring(self):
        """Stop monitoring."""
        self.monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=1.0)
    
    def _monitor_loop(self):
        """Monitor loop running in separate thread."""
        try:
            if not self.process:
                return
            
            ps_process = psutil.Process(self.process.pid)
            while self.monitoring and self.process.poll() is None:
                try:
                    # Get memory usage
                    memory_info = ps_process.memory_info()
                    memory_mb = memory_info.rss / (1024 * 1024)
                    self.peak_memory_mb = max(self.peak_memory_mb, memory_mb)
                    
                    # Get CPU usage
                    cpu_percent = ps_process.cpu_percent()
                    self.peak_cpu_percent = max(self.peak_cpu_percent, cpu_percent)
                    
                    time.sleep(0.5)  # Monitor every 500ms
                    
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    break
                    
        except Exception as e:
            logger.warning(f"Error monitoring process: {e}")
    
    def get_stats(self) -> Dict[str, float]:
        """Get monitoring statistics."""
        return {
            'peak_memory_mb': self.peak_memory_mb,
            'peak_cpu_percent': self.peak_cpu_percent
        }

class ModuleTestRunner:
    """
    Test runner for executing tests with subprocess management and result collection.
    
    This runner executes pytest on individual test files or modules, collects
    comprehensive results, handles timeouts, and provides resource isolation.
    """
    
    def __init__(self, parallel: bool = True):
        """
        Initialize the test runner.
        
        Args:
            parallel: Whether to enable parallel execution
        """
        self.parallel = parallel
        self.discovery_engine = TestDiscoveryEngine()
        self.results: Dict[str, ModuleResult] = {}
        self.execution_stats = {
            'total_modules': 0,
            'total_tests': 0,
            'execution_start': None,
            'execution_end': None,
            'total_duration': 0.0,
            'peak_memory_mb': 0.0,
            'peak_cpu_percent': 0.0
        }
        
        # Resource limits
        self.max_workers = config.execution.max_workers
        self.timeout_seconds = config.execution.timeout_seconds
        self.licensed_software_limit = config.execution.licensed_software_limit
        self.retry_count = config.execution.retry_count
        
        # Active licensed software processes counter
        self._licensed_processes = 0
        self._licensed_lock = threading.Lock()
    
    @log_performance
    def run_module_tests(self, module_name: str, verbose: bool = False) -> ModuleResult:
        """
        Run all tests for a specific module.
        
        Args:
            module_name: Name of the module to test
            verbose: Show detailed output
            
        Returns:
            ModuleResult with aggregated results
        """
        logger.info(f"Running tests for module: {module_name}")
        
        # Get module information from discovery
        modules = self.discovery_engine.discover_modules()
        module_info = modules.get(module_name)
        
        if not module_info:
            logger.error(f"Module '{module_name}' not found")
            return self._create_error_result(module_name, f"Module '{module_name}' not found")
        
        if not module_info.test_files:
            logger.warning(f"Module '{module_name}' has no test files")
            return self._create_empty_result(module_name)
        
        start_time = datetime.now(timezone.utc)
        test_results = []
        
        # Execute tests for each file in the module
        for test_file in module_info.test_files:
            if not test_file.is_runnable:
                logger.info(f"Skipping non-runnable test: {test_file.path}")
                result = self._create_skipped_result(test_file, "Not runnable (licensed software or development status)")
                test_results.append(result)
                continue
            
            # Run individual test file
            result = self._run_single_test(test_file, verbose)
            test_results.append(result)
        
        end_time = datetime.now(timezone.utc)
        total_duration = (end_time - start_time).total_seconds()
        
        # Aggregate results
        module_result = self._aggregate_module_results(
            module_name, test_results, total_duration, start_time, end_time
        )
        
        self.results[module_name] = module_result
        logger.info(f"Module '{module_name}' completed: {module_result.status}")
        
        return module_result
    
    @log_performance
    def run_all_modules(self, verbose: bool = False) -> Dict[str, ModuleResult]:
        """
        Run tests for all discovered modules.
        
        Args:
            verbose: Show detailed output
            
        Returns:
            Dictionary mapping module names to ModuleResult objects
        """
        logger.info("Starting test execution for all modules")
        
        # Discover runnable modules (force fresh discovery)
        self.discovery_engine.discover_modules(force_refresh=True)
        modules = self.discovery_engine.get_runnable_tests()
        if not modules:
            logger.warning("No runnable modules found")
            return {}
        
        self.execution_stats['execution_start'] = datetime.now(timezone.utc).isoformat()
        self.execution_stats['total_modules'] = len(modules)
        start_time = time.time()
        
        if self.parallel and len(modules) > 1:
            logger.info(f"Running {len(modules)} modules in parallel with {self.max_workers} workers")
            results = self._run_modules_parallel(modules, verbose)
        else:
            logger.info(f"Running {len(modules)} modules sequentially")
            results = self._run_modules_sequential(modules, verbose)
        
        end_time = time.time()
        self.execution_stats['execution_end'] = datetime.now(timezone.utc).isoformat()
        self.execution_stats['total_duration'] = end_time - start_time
        
        # Update statistics
        self._update_execution_stats(results)
        
        logger.info(f"All modules completed in {self.execution_stats['total_duration']:.1f}s")
        return results
    
    def _run_modules_parallel(self, modules: Dict[str, ModuleInfo], verbose: bool) -> Dict[str, ModuleResult]:
        """Run modules in parallel using ThreadPoolExecutor."""
        results = {}
        
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # Submit tasks
            future_to_module = {
                executor.submit(self.run_module_tests, module_name, verbose): module_name
                for module_name in modules.keys()
            }
            
            # Collect results as they complete
            for future in as_completed(future_to_module):
                module_name = future_to_module[future]
                try:
                    result = future.result()
                    results[module_name] = result
                    logger.info(f"Completed module: {module_name} ({result.status})")
                except Exception as e:
                    logger.error(f"Error running module {module_name}: {e}", exc_info=True)
                    results[module_name] = self._create_error_result(module_name, str(e))
        
        return results
    
    def _run_modules_sequential(self, modules: Dict[str, ModuleInfo], verbose: bool) -> Dict[str, ModuleResult]:
        """Run modules sequentially."""
        results = {}
        
        for module_name in modules.keys():
            try:
                result = self.run_module_tests(module_name, verbose)
                results[module_name] = result
                logger.info(f"Completed module: {module_name} ({result.status})")
            except Exception as e:
                logger.error(f"Error running module {module_name}: {e}", exc_info=True)
                results[module_name] = self._create_error_result(module_name, str(e))
        
        return results
    
    def _run_single_test(self, test_file: TestFile, verbose: bool) -> TestResult:
        """
        Execute a single test file using subprocess.
        
        Args:
            test_file: TestFile object to execute
            verbose: Show detailed output
            
        Returns:
            TestResult with execution results
        """
        logger.debug(f"Executing test file: {test_file.path}")
        
        # Handle licensed software limits
        if test_file.requires_license:
            if not self._acquire_license_slot():
                return self._create_skipped_result(test_file, "Licensed software limit reached")
        
        try:
            result = self._execute_pytest(test_file, verbose)
        finally:
            if test_file.requires_license:
                self._release_license_slot()
        
        return result
    
    def _execute_pytest(self, test_file: TestFile, verbose: bool) -> TestResult:
        """Execute pytest on a test file with resource monitoring and timeout handling."""
        
        start_time = datetime.now(timezone.utc)
        
        # Prepare pytest command (normalize path for cross-platform compatibility)
        test_path = str(Path(test_file.path)).replace('\\', '/')
        cmd = [sys.executable, '-m', 'pytest', test_path, '-v', '--tb=short']
        
        if verbose:
            cmd.extend(['--capture=no', '-s'])
        
        # Set up environment
        env = os.environ.copy()
        env['PYTHONPATH'] = str(Path.cwd())
        
        resource_monitor = ResourceMonitor()
        
        try:
            # Execute subprocess with timeout
            logger.debug(f"Executing command: {' '.join(cmd)}")
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env=env,
                cwd=Path.cwd()
            )
            
            # Start resource monitoring
            resource_monitor.start_monitoring(process)
            
            try:
                stdout, stderr = process.communicate(timeout=self.timeout_seconds)
                exit_code = process.returncode
                timed_out = False
            except subprocess.TimeoutExpired:
                logger.warning(f"Test timed out after {self.timeout_seconds}s: {test_file.path}")
                process.kill()
                stdout, stderr = process.communicate()
                exit_code = -1
                timed_out = True
            
        except Exception as e:
            logger.error(f"Error executing test {test_file.path}: {e}")
            return self._create_error_result_for_file(test_file, str(e), start_time)
        
        finally:
            resource_monitor.stop_monitoring()
        
        end_time = datetime.now(timezone.utc)
        duration = (end_time - start_time).total_seconds()
        
        # Parse results
        if timed_out:
            return TestResult(
                test_file=test_file.path,
                module=test_file.module,
                status='timeout',
                duration=duration,
                output=stdout or '',
                error_output=stderr or '',
                exit_code=exit_code,
                start_time=start_time.isoformat(),
                end_time=end_time.isoformat()
            )
        
        # Parse pytest JSON output if available
        test_result = self._parse_pytest_output(
            test_file, stdout, stderr, exit_code, duration, start_time, end_time
        )
        
        # Add resource statistics
        stats = resource_monitor.get_stats()
        logger.debug(f"Test resource usage - Memory: {stats['peak_memory_mb']:.1f}MB, CPU: {stats['peak_cpu_percent']:.1f}%")
        
        return test_result
    
    def _parse_pytest_output(self, test_file: TestFile, stdout: str, stderr: str, 
                           exit_code: int, duration: float, start_time: datetime, 
                           end_time: datetime) -> TestResult:
        """Parse pytest output and extract test results."""
        
        # Try to parse JSON report
        test_count = 0
        passed_count = 0
        failed_count = 0
        skipped_count = 0
        collected_count = 0
        
        # Look for JSON report in stdout
        json_report = None
        try:
            lines = stdout.split('\n')
            for line in lines:
                if line.startswith('{') and '"tests"' in line:
                    json_report = json.loads(line)
                    break
        except (json.JSONDecodeError, ValueError):
            pass
        
        if json_report:
            # Extract counts from JSON report
            summary = json_report.get('summary', {})
            test_count = summary.get('total', 0)
            passed_count = summary.get('passed', 0)
            failed_count = summary.get('failed', 0)
            skipped_count = summary.get('skipped', 0)
            collected_count = summary.get('collected', 0)
        else:
            # Fallback: parse text output
            collected_count = self._extract_collected_count(stdout)
            test_count, passed_count, failed_count, skipped_count = self._extract_test_counts(stdout)
        
        # Determine overall status
        if exit_code == 0:
            if test_count == 0:
                status = 'skipped'
            elif failed_count == 0:
                status = 'passed'
            else:
                status = 'failed'
        elif exit_code == 5:  # pytest exit code for no tests collected
            status = 'skipped'
        else:
            status = 'error'
        
        return TestResult(
            test_file=test_file.path,
            module=test_file.module,
            status=status,
            duration=duration,
            output=stdout,
            error_output=stderr,
            exit_code=exit_code,
            test_count=test_count,
            passed_count=passed_count,
            failed_count=failed_count,
            skipped_count=skipped_count,
            collected_count=collected_count,
            start_time=start_time.isoformat(),
            end_time=end_time.isoformat()
        )
    
    def _extract_collected_count(self, output: str) -> int:
        """Extract collected test count from pytest output."""
        import re
        match = re.search(r'collected (\d+) item', output)
        return int(match.group(1)) if match else 0
    
    def _extract_test_counts(self, output: str) -> Tuple[int, int, int, int]:
        """Extract test counts from pytest summary."""
        import re
        
        # Look for summary line like "1 passed, 2 failed, 1 skipped"
        summary_match = re.search(r'=+ (.+) in \d+\.\d+s =+', output)
        if not summary_match:
            return 0, 0, 0, 0
        
        summary_text = summary_match.group(1)
        
        # Extract individual counts
        passed_match = re.search(r'(\d+) passed', summary_text)
        failed_match = re.search(r'(\d+) failed', summary_text)
        skipped_match = re.search(r'(\d+) skipped', summary_text)
        
        passed_count = int(passed_match.group(1)) if passed_match else 0
        failed_count = int(failed_match.group(1)) if failed_match else 0
        skipped_count = int(skipped_match.group(1)) if skipped_match else 0
        
        total_count = passed_count + failed_count + skipped_count
        
        return total_count, passed_count, failed_count, skipped_count
    
    @contextmanager
    def _license_slot_manager(self):
        """Context manager for licensed software execution slots."""
        acquired = self._acquire_license_slot()
        try:
            yield acquired
        finally:
            if acquired:
                self._release_license_slot()
    
    def _acquire_license_slot(self) -> bool:
        """Acquire a slot for licensed software execution."""
        with self._licensed_lock:
            if self._licensed_processes < self.licensed_software_limit:
                self._licensed_processes += 1
                return True
            return False
    
    def _release_license_slot(self):
        """Release a licensed software execution slot."""
        with self._licensed_lock:
            self._licensed_processes = max(0, self._licensed_processes - 1)
    
    def _aggregate_module_results(self, module_name: str, test_results: List[TestResult],
                                total_duration: float, start_time: datetime, 
                                end_time: datetime) -> ModuleResult:
        """Aggregate individual test results into module result."""
        
        total_tests = len(test_results)
        passed_tests = sum(1 for r in test_results if r.status == 'passed')
        failed_tests = sum(1 for r in test_results if r.status == 'failed')
        skipped_tests = sum(1 for r in test_results if r.status == 'skipped')
        error_tests = sum(1 for r in test_results if r.status == 'error')
        timeout_tests = sum(1 for r in test_results if r.status == 'timeout')
        
        # Determine overall module status
        if error_tests > 0 or timeout_tests > 0:
            status = 'error'
        elif failed_tests > 0:
            if passed_tests > 0:
                status = 'mixed'
            else:
                status = 'failed'
        elif passed_tests > 0:
            status = 'passed'
        else:
            status = 'skipped'
        
        return ModuleResult(
            module_name=module_name,
            status=status,
            total_duration=total_duration,
            test_results=test_results,
            total_tests=total_tests,
            passed_tests=passed_tests,
            failed_tests=failed_tests,
            skipped_tests=skipped_tests,
            error_tests=error_tests,
            timeout_tests=timeout_tests,
            start_time=start_time.isoformat(),
            end_time=end_time.isoformat()
        )
    
    def _create_error_result(self, module_name: str, error_msg: str) -> ModuleResult:
        """Create an error result for a module."""
        now = datetime.now(timezone.utc)
        return ModuleResult(
            module_name=module_name,
            status='error',
            total_duration=0.0,
            test_results=[],
            error_tests=1,
            start_time=now.isoformat(),
            end_time=now.isoformat()
        )
    
    def _create_empty_result(self, module_name: str) -> ModuleResult:
        """Create an empty result for a module with no tests."""
        now = datetime.now(timezone.utc)
        return ModuleResult(
            module_name=module_name,
            status='skipped',
            total_duration=0.0,
            test_results=[],
            start_time=now.isoformat(),
            end_time=now.isoformat()
        )
    
    def _create_skipped_result(self, test_file: TestFile, reason: str) -> TestResult:
        """Create a skipped result for a test file."""
        now = datetime.now(timezone.utc)
        return TestResult(
            test_file=test_file.path,
            module=test_file.module,
            status='skipped',
            duration=0.0,
            output=f"Skipped: {reason}",
            error_output='',
            exit_code=0,
            start_time=now.isoformat(),
            end_time=now.isoformat()
        )
    
    def _create_error_result_for_file(self, test_file: TestFile, error_msg: str, 
                                    start_time: datetime) -> TestResult:
        """Create an error result for a test file."""
        end_time = datetime.now(timezone.utc)
        duration = (end_time - start_time).total_seconds()
        
        return TestResult(
            test_file=test_file.path,
            module=test_file.module,
            status='error',
            duration=duration,
            output='',
            error_output=error_msg,
            exit_code=-1,
            start_time=start_time.isoformat(),
            end_time=end_time.isoformat()
        )
    
    def _update_execution_stats(self, results: Dict[str, ModuleResult]):
        """Update execution statistics from results."""
        total_tests = sum(r.total_tests for r in results.values())
        self.execution_stats['total_tests'] = total_tests
        
        # Find peak resource usage (would need to be tracked during execution)
        # For now, these remain at 0 - could be enhanced with global monitoring
    
    def get_execution_summary(self) -> Dict[str, Any]:
        """Get summary of execution results."""
        if not self.results:
            return {'status': 'no_results'}
        
        total_modules = len(self.results)
        total_tests = sum(r.total_tests for r in self.results.values())
        passed_tests = sum(r.passed_tests for r in self.results.values())
        failed_tests = sum(r.failed_tests for r in self.results.values())
        skipped_tests = sum(r.skipped_tests for r in self.results.values())
        error_tests = sum(r.error_tests for r in self.results.values())
        timeout_tests = sum(r.timeout_tests for r in self.results.values())
        
        passed_modules = sum(1 for r in self.results.values() if r.status == 'passed')
        failed_modules = sum(1 for r in self.results.values() if r.status in ['failed', 'mixed'])
        error_modules = sum(1 for r in self.results.values() if r.status == 'error')
        
        return {
            'modules': {
                'total': total_modules,
                'passed': passed_modules,
                'failed': failed_modules,
                'error': error_modules
            },
            'tests': {
                'total': total_tests,
                'passed': passed_tests,
                'failed': failed_tests,
                'skipped': skipped_tests,
                'error': error_tests,
                'timeout': timeout_tests
            },
            'execution_stats': self.execution_stats,
            'success_rate': (passed_tests / max(total_tests, 1)) * 100
        }
    
    def save_results(self, output_path: str):
        """Save execution results to file."""
        results_data = {
            'execution_stats': self.execution_stats,
            'summary': self.get_execution_summary(),
            'modules': {name: result.to_dict() for name, result in self.results.items()}
        }
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(results_data, f, indent=2)
        
        logger.info(f"Results saved to {output_path}")
    
    def load_results(self, input_path: str):
        """Load execution results from file."""
        with open(input_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        self.execution_stats = data.get('execution_stats', {})
        
        # Reconstruct results from saved data
        self.results = {}
        for name, module_data in data.get('modules', {}).items():
            # Reconstruct test results
            test_results = []
            for tr_data in module_data.get('test_results', []):
                test_results.append(TestResult(**tr_data))
            
            # Reconstruct module result
            module_result = ModuleResult(
                module_name=module_data['module_name'],
                status=module_data['status'],
                total_duration=module_data['total_duration'],
                test_results=test_results,
                total_tests=module_data.get('total_tests', 0),
                passed_tests=module_data.get('passed_tests', 0),
                failed_tests=module_data.get('failed_tests', 0),
                skipped_tests=module_data.get('skipped_tests', 0),
                error_tests=module_data.get('error_tests', 0),
                timeout_tests=module_data.get('timeout_tests', 0),
                start_time=module_data.get('start_time'),
                end_time=module_data.get('end_time')
            )
            
            self.results[name] = module_result
        
        logger.info(f"Results loaded from {input_path}")