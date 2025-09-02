"""
Comprehensive Reporting and Dashboard System for test automation.

This module provides detailed test status reporting, historical trend analysis,
and dashboard generation for continuous monitoring.
"""

import json
import os
import csv
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from datetime import datetime, timezone, timedelta
from collections import defaultdict

from test_automation.config import config
from test_automation.logging_config import get_logger
from test_automation.core.runner import ModuleResult, TestResult
from test_automation.core.discovery import TestDiscoveryEngine

logger = get_logger('test_automation.reporting')


@dataclass
class TestTrendPoint:
    """Single point in test trend data."""
    timestamp: str
    total_modules: int
    passed_modules: int
    failed_modules: int
    total_tests: int
    passed_tests: int
    failed_tests: int
    success_rate: float
    duration: float


@dataclass
class ModuleTrend:
    """Trend data for a specific module."""
    module_name: str
    trend_points: List[TestTrendPoint]
    current_status: str
    stability_score: float  # 0-100, higher is more stable


class ReportStorage:
    """Handles storage and retrieval of test execution history."""
    
    def __init__(self):
        self.reports_dir = Path("test_reports")
        self.history_file = self.reports_dir / "execution_history.json"
        self.reports_dir.mkdir(exist_ok=True)
    
    def save_execution_results(self, results: Dict[str, ModuleResult], 
                             execution_summary: Dict[str, Any]) -> str:
        """Save execution results to persistent storage."""
        timestamp = datetime.now(timezone.utc).isoformat()
        
        # Create execution record
        execution_record = {
            'timestamp': timestamp,
            'summary': execution_summary,
            'modules': {name: result.to_dict() for name, result in results.items()}
        }
        
        # Load existing history
        history = self._load_history()
        
        # Add new record
        history.append(execution_record)
        
        # Keep only last 100 records to prevent unbounded growth
        if len(history) > 100:
            history = history[-100:]
        
        # Save updated history
        self._save_history(history)
        
        # Save detailed report for this execution
        report_file = self.reports_dir / f"execution_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_file, 'w', encoding='utf-8') as f:
            json.dump(execution_record, f, indent=2)
        
        logger.info(f"Execution results saved to {report_file}")
        return str(report_file)
    
    def _load_history(self) -> List[Dict[str, Any]]:
        """Load execution history from file."""
        if self.history_file.exists():
            try:
                with open(self.history_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
            except Exception as e:
                logger.warning(f"Error loading history: {e}")
        return []
    
    def _save_history(self, history: List[Dict[str, Any]]):
        """Save execution history to file."""
        try:
            with open(self.history_file, 'w', encoding='utf-8') as f:
                json.dump(history, f, indent=2)
        except Exception as e:
            logger.error(f"Error saving history: {e}")
    
    def get_execution_history(self, days: int = 30) -> List[Dict[str, Any]]:
        """Get execution history for the last N days."""
        history = self._load_history()
        
        if not history:
            return []
        
        # Filter by date
        cutoff_date = datetime.now(timezone.utc) - timedelta(days=days)
        filtered_history = []
        
        for record in history:
            try:
                record_date = datetime.fromisoformat(record['timestamp'].replace('Z', '+00:00'))
                if record_date >= cutoff_date:
                    filtered_history.append(record)
            except Exception:
                continue
        
        return filtered_history


class TrendAnalyzer:
    """Analyzes test execution trends over time."""
    
    def __init__(self, storage: ReportStorage):
        self.storage = storage
    
    def calculate_overall_trends(self, days: int = 30) -> Dict[str, Any]:
        """Calculate overall test suite trends."""
        history = self.storage.get_execution_history(days)
        
        if len(history) < 2:
            return {'status': 'insufficient_data', 'message': 'Need at least 2 execution records'}
        
        # Convert to trend points
        trend_points = []
        for record in history:
            summary = record.get('summary', {})
            
            # Handle different summary formats
            if 'tests' in summary and 'modules' in summary:
                # New format
                tests = summary['tests']
                modules = summary['modules']
                total_tests = tests.get('total', 0)
                passed_tests = tests.get('passed', 0)
                failed_tests = tests.get('failed', 0)
                total_modules = modules.get('total', 0)
                passed_modules = modules.get('passed', 0)
                failed_modules = modules.get('failed', 0)
            else:
                # Legacy format - calculate from module data
                modules_data = record.get('modules', {})
                total_modules = len(modules_data)
                passed_modules = sum(1 for m in modules_data.values() if m.get('status') == 'passed')
                failed_modules = sum(1 for m in modules_data.values() if m.get('status') in ['failed', 'error', 'mixed'])
                total_tests = sum(m.get('total_tests', 0) for m in modules_data.values())
                passed_tests = sum(m.get('passed_tests', 0) for m in modules_data.values())
                failed_tests = sum(m.get('failed_tests', 0) for m in modules_data.values())
            
            success_rate = (passed_tests / max(total_tests, 1)) * 100
            duration = summary.get('total_duration', 0)
            
            trend_point = TestTrendPoint(
                timestamp=record['timestamp'],
                total_modules=total_modules,
                passed_modules=passed_modules,
                failed_modules=failed_modules,
                total_tests=total_tests,
                passed_tests=passed_tests,
                failed_tests=failed_tests,
                success_rate=success_rate,
                duration=duration
            )
            trend_points.append(trend_point)
        
        # Calculate trends
        if len(trend_points) >= 2:
            recent = trend_points[-1]
            previous = trend_points[-2]
            
            success_rate_change = recent.success_rate - previous.success_rate
            duration_change = recent.duration - previous.duration
            test_count_change = recent.total_tests - previous.total_tests
        else:
            success_rate_change = 0
            duration_change = 0
            test_count_change = 0
        
        return {
            'trend_points': [asdict(tp) for tp in trend_points],
            'current_status': {
                'success_rate': trend_points[-1].success_rate,
                'total_tests': trend_points[-1].total_tests,
                'total_modules': trend_points[-1].total_modules,
                'duration': trend_points[-1].duration
            },
            'trends': {
                'success_rate_change': success_rate_change,
                'duration_change': duration_change,
                'test_count_change': test_count_change,
                'direction': 'improving' if success_rate_change > 0 else 'declining' if success_rate_change < 0 else 'stable'
            },
            'analysis_period': f'{days} days',
            'data_points': len(trend_points)
        }
    
    def calculate_module_trends(self, days: int = 30) -> Dict[str, ModuleTrend]:
        """Calculate trends for individual modules."""
        history = self.storage.get_execution_history(days)
        
        if not history:
            return {}
        
        module_trends = {}
        
        # Group data by module
        module_data = defaultdict(list)
        
        for record in history:
            modules = record.get('modules', {})
            timestamp = record['timestamp']
            
            for module_name, module_info in modules.items():
                module_data[module_name].append({
                    'timestamp': timestamp,
                    'status': module_info.get('status', 'unknown'),
                    'total_tests': module_info.get('total_tests', 0),
                    'passed_tests': module_info.get('passed_tests', 0),
                    'failed_tests': module_info.get('failed_tests', 0),
                    'duration': module_info.get('total_duration', 0)
                })
        
        # Calculate trends for each module
        for module_name, data_points in module_data.items():
            if len(data_points) < 2:
                continue
            
            # Calculate stability score (percentage of time module passed)
            passing_runs = sum(1 for dp in data_points if dp['status'] == 'passed')
            stability_score = (passing_runs / len(data_points)) * 100
            
            # Create trend points
            trend_points = []
            for dp in data_points:
                success_rate = (dp['passed_tests'] / max(dp['total_tests'], 1)) * 100
                trend_point = TestTrendPoint(
                    timestamp=dp['timestamp'],
                    total_modules=1,  # Single module
                    passed_modules=1 if dp['status'] == 'passed' else 0,
                    failed_modules=0 if dp['status'] == 'passed' else 1,
                    total_tests=dp['total_tests'],
                    passed_tests=dp['passed_tests'],
                    failed_tests=dp['failed_tests'],
                    success_rate=success_rate,
                    duration=dp['duration']
                )
                trend_points.append(trend_point)
            
            module_trends[module_name] = ModuleTrend(
                module_name=module_name,
                trend_points=trend_points,
                current_status=data_points[-1]['status'],
                stability_score=stability_score
            )
        
        return module_trends


class ReportGenerator:
    """Generates various types of reports and dashboards."""
    
    def __init__(self):
        self.storage = ReportStorage()
        self.trend_analyzer = TrendAnalyzer(self.storage)
        self.discovery_engine = TestDiscoveryEngine()
    
    def generate_status_report(self, results: Dict[str, ModuleResult] = None) -> Dict[str, Any]:
        """Generate comprehensive status report."""
        logger.info("Generating comprehensive status report")
        
        # Get current test results
        if not results:
            from test_automation.core.runner import ModuleTestRunner
            runner = ModuleTestRunner(parallel=False)
            results = runner.run_all_modules(verbose=False)
        
        # Get discovery information
        modules = self.discovery_engine.discover_modules()
        discovery_summary = self.discovery_engine.get_discovery_summary()
        
        # Calculate current statistics
        total_modules = len(results)
        passed_modules = sum(1 for r in results.values() if r.status == 'passed')
        failed_modules = sum(1 for r in results.values() if r.status in ['failed', 'mixed', 'error'])
        skipped_modules = sum(1 for r in results.values() if r.status == 'skipped')
        
        total_tests = sum(r.total_tests for r in results.values())
        passed_tests = sum(r.passed_tests for r in results.values())
        failed_tests = sum(r.failed_tests for r in results.values())
        error_tests = sum(r.error_tests for r in results.values())
        
        success_rate = (passed_tests / max(total_tests, 1)) * 100
        
        # Get trend analysis
        trends = self.trend_analyzer.calculate_overall_trends()
        module_trends = self.trend_analyzer.calculate_module_trends()
        
        # Categorize modules by status and performance
        high_performers = []
        problem_modules = []
        unstable_modules = []
        
        for module_name, trend in module_trends.items():
            if trend.stability_score >= 90:
                high_performers.append(module_name)
            elif trend.stability_score < 50:
                problem_modules.append(module_name)
            elif trend.current_status != 'passed':
                unstable_modules.append(module_name)
        
        return {
            'generated_at': datetime.now(timezone.utc).isoformat(),
            'current_status': {
                'modules': {
                    'total': total_modules,
                    'passed': passed_modules,
                    'failed': failed_modules,
                    'skipped': skipped_modules
                },
                'tests': {
                    'total': total_tests,
                    'passed': passed_tests,
                    'failed': failed_tests,
                    'errors': error_tests
                },
                'success_rate': success_rate
            },
            'discovery_info': discovery_summary,
            'trends': trends,
            'module_performance': {
                'high_performers': high_performers,
                'problem_modules': problem_modules,
                'unstable_modules': unstable_modules,
                'stability_scores': {name: trend.stability_score 
                                   for name, trend in module_trends.items()}
            },
            'detailed_results': {name: result.to_dict() for name, result in results.items()}
        }
    
    def save_html_report(self, report_data: Dict[str, Any], output_path: str = None) -> str:
        """Generate and save HTML dashboard report."""
        if not output_path:
            output_path = self.storage.reports_dir / f"dashboard_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
        
        html_content = self._generate_html_dashboard(report_data)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html_content)
        
        logger.info(f"HTML report saved to {output_path}")
        return str(output_path)
    
    def save_csv_report(self, report_data: Dict[str, Any], output_path: str = None) -> str:
        """Generate and save CSV report."""
        if not output_path:
            output_path = self.storage.reports_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
        
        with open(output_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            
            # Write header
            writer.writerow(['Module', 'Status', 'Total Tests', 'Passed', 'Failed', 'Success Rate', 'Duration'])
            
            # Write module data
            for module_name, module_data in report_data['detailed_results'].items():
                success_rate = (module_data['passed_tests'] / max(module_data['total_tests'], 1)) * 100
                writer.writerow([
                    module_name,
                    module_data['status'],
                    module_data['total_tests'],
                    module_data['passed_tests'],
                    module_data['failed_tests'],
                    f"{success_rate:.1f}%",
                    f"{module_data['total_duration']:.1f}s"
                ])
        
        logger.info(f"CSV report saved to {output_path}")
        return str(output_path)
    
    def save_json_report(self, report_data: Dict[str, Any], output_path: str = None) -> str:
        """Save JSON report."""
        if not output_path:
            output_path = self.storage.reports_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(report_data, f, indent=2)
        
        logger.info(f"JSON report saved to {output_path}")
        return str(output_path)
    
    def _generate_html_dashboard(self, report_data: Dict[str, Any]) -> str:
        """Generate HTML dashboard content."""
        current = report_data['current_status']
        trends = report_data.get('trends', {})
        performance = report_data.get('module_performance', {})
        
        html = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Test Suite Dashboard</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }}
        .container {{ max-width: 1200px; margin: 0 auto; }}
        .header {{ background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }}
        .header h1 {{ margin: 0; }}
        .stats-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-bottom: 30px; }}
        .stat-card {{ background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}
        .stat-number {{ font-size: 2em; font-weight: bold; margin-bottom: 5px; }}
        .stat-label {{ color: #666; text-transform: uppercase; font-size: 0.8em; }}
        .success {{ color: #28a745; }}
        .warning {{ color: #ffc107; }}
        .danger {{ color: #dc3545; }}
        .info {{ color: #17a2b8; }}
        .section {{ background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }}
        .section h2 {{ margin-top: 0; }}
        .module-list {{ list-style: none; padding: 0; }}
        .module-item {{ padding: 10px; margin: 5px 0; background: #f8f9fa; border-radius: 4px; display: flex; justify-content: between; }}
        .module-status {{ padding: 4px 8px; border-radius: 4px; color: white; font-size: 0.8em; }}
        .status-passed {{ background-color: #28a745; }}
        .status-failed {{ background-color: #dc3545; }}
        .status-mixed {{ background-color: #ffc107; }}
        .status-skipped {{ background-color: #6c757d; }}
        .trend-indicator {{ font-size: 1.2em; }}
        .trend-up {{ color: #28a745; }}
        .trend-down {{ color: #dc3545; }}
        .trend-stable {{ color: #6c757d; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Test Suite Dashboard</h1>
            <p>Generated: {report_data['generated_at']}</p>
        </div>
        
        <div class="stats-grid">
            <div class="stat-card">
                <div class="stat-number success">{current['modules']['passed']}/{current['modules']['total']}</div>
                <div class="stat-label">Modules Passing</div>
            </div>
            
            <div class="stat-card">
                <div class="stat-number info">{current['tests']['passed']}/{current['tests']['total']}</div>
                <div class="stat-label">Tests Passing</div>
            </div>
            
            <div class="stat-card">
                <div class="stat-number {'success' if current['success_rate'] >= 80 else 'warning' if current['success_rate'] >= 60 else 'danger'}">{current['success_rate']:.1f}%</div>
                <div class="stat-label">Success Rate</div>
            </div>
            
            <div class="stat-card">
                <div class="stat-number info">{len(performance.get('high_performers', []))}</div>
                <div class="stat-label">High Performers</div>
            </div>
        </div>
        
        {"" if trends.get('status') == 'insufficient_data' else f'''
        <div class="section">
            <h2>Trends 
                <span class="trend-indicator {'trend-up' if trends.get('trends', {}).get('direction') == 'improving' else 'trend-down' if trends.get('trends', {}).get('direction') == 'declining' else 'trend-stable'}">
                    {'↗' if trends.get('trends', {}).get('direction') == 'improving' else '↘' if trends.get('trends', {}).get('direction') == 'declining' else '→'}
                </span>
            </h2>
            <p>Success Rate Change: <strong>{trends.get('trends', {}).get('success_rate_change', 0):+.1f}%</strong></p>
            <p>Analysis Period: {trends.get('analysis_period', 'N/A')} ({trends.get('data_points', 0)} data points)</p>
        </div>
        '''}
        
        <div class="section">
            <h2>Module Performance</h2>
            
            {"<h3>High Performers</h3>" if performance.get('high_performers') else ""}
            {"<ul class='module-list'>" if performance.get('high_performers') else ""}
            {"".join(f"<li class='module-item'>{module} <span class='module-status status-passed'>Stable</span></li>" for module in performance.get('high_performers', []))}
            {"</ul>" if performance.get('high_performers') else ""}
            
            {"<h3>Problem Modules</h3>" if performance.get('problem_modules') else ""}
            {"<ul class='module-list'>" if performance.get('problem_modules') else ""}
            {"".join(f"<li class='module-item'>{module} <span class='module-status status-failed'>Needs Attention</span></li>" for module in performance.get('problem_modules', []))}
            {"</ul>" if performance.get('problem_modules') else ""}
            
            {"<h3>Unstable Modules</h3>" if performance.get('unstable_modules') else ""}
            {"<ul class='module-list'>" if performance.get('unstable_modules') else ""}
            {"".join(f"<li class='module-item'>{module} <span class='module-status status-mixed'>Unstable</span></li>" for module in performance.get('unstable_modules', []))}
            {"</ul>" if performance.get('unstable_modules') else ""}
        </div>
        
        <div class="section">
            <h2>Detailed Module Results</h2>
            <ul class="module-list">
"""
        
        # Add detailed module results
        for module_name, module_data in report_data['detailed_results'].items():
            status_class = f"status-{module_data['status']}"
            success_rate = (module_data['passed_tests'] / max(module_data['total_tests'], 1)) * 100
            
            html += f"""
                <li class="module-item">
                    <div>
                        <strong>{module_name}</strong><br>
                        <small>{module_data['passed_tests']}/{module_data['total_tests']} tests ({success_rate:.1f}%) - {module_data['total_duration']:.1f}s</small>
                    </div>
                    <span class="module-status {status_class}">{module_data['status'].upper()}</span>
                </li>
"""
        
        html += """
            </ul>
        </div>
    </div>
</body>
</html>
"""
        return html


class ContinuousMonitor:
    """Continuous monitoring and alerting system."""
    
    def __init__(self):
        self.storage = ReportStorage()
        self.trend_analyzer = TrendAnalyzer(self.storage)
        self.thresholds = {
            'success_rate_minimum': 70.0,
            'success_rate_decline_threshold': -10.0,
            'duration_increase_threshold': 50.0  # Percentage increase
        }
    
    def check_health_status(self) -> Dict[str, Any]:
        """Check current health status against thresholds."""
        trends = self.trend_analyzer.calculate_overall_trends(days=7)
        
        if trends.get('status') == 'insufficient_data':
            return {'status': 'unknown', 'message': 'Insufficient data for health check'}
        
        alerts = []
        status = 'healthy'
        
        current = trends['current_status']
        trend_data = trends.get('trends', {})
        
        # Check success rate
        if current['success_rate'] < self.thresholds['success_rate_minimum']:
            alerts.append({
                'type': 'success_rate_low',
                'severity': 'high',
                'message': f"Success rate {current['success_rate']:.1f}% is below threshold {self.thresholds['success_rate_minimum']}%"
            })
            status = 'critical'
        
        # Check success rate trend
        if trend_data.get('success_rate_change', 0) < self.thresholds['success_rate_decline_threshold']:
            alerts.append({
                'type': 'success_rate_declining', 
                'severity': 'medium',
                'message': f"Success rate declined by {abs(trend_data['success_rate_change']):.1f}% recently"
            })
            if status == 'healthy':
                status = 'warning'
        
        # Check execution time increases
        if trend_data.get('duration_change', 0) > 0:
            duration_increase_pct = (trend_data['duration_change'] / max(current['duration'] - trend_data['duration_change'], 1)) * 100
            if duration_increase_pct > self.thresholds['duration_increase_threshold']:
                alerts.append({
                    'type': 'duration_increased',
                    'severity': 'low', 
                    'message': f"Execution time increased by {duration_increase_pct:.1f}%"
                })
                if status == 'healthy':
                    status = 'warning'
        
        return {
            'status': status,
            'checked_at': datetime.now(timezone.utc).isoformat(),
            'current_metrics': current,
            'trends': trend_data,
            'alerts': alerts,
            'thresholds': self.thresholds
        }