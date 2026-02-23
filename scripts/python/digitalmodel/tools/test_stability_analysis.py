#!/usr/bin/env python3
"""
Test Stability Analysis - Run before new development
Identifies flaky tests, performance bottlenecks, and critical path coverage.
"""

import subprocess
import json
import time
from pathlib import Path
from collections import defaultdict, Counter
import sys

class TestStabilityAnalyzer:
    def __init__(self):
        self.results = {
            'test_runs': [],
            'flaky_tests': {},
            'consistent_passes': [],
            'consistent_failures': [],
            'performance_data': {},
            'critical_validations': [],
            'recommendation_summary': {}
        }
        
    def run_stability_analysis(self, iterations=3):
        """Run test suite multiple times to identify flaky tests."""
        print(f"Running test stability analysis ({iterations} iterations)...")
        
        test_commands = [
            # Core validated test directories  
            "python -m pytest tests/no_license/ tests/in_progress/ --tb=no --quiet",
            "python -m pytest tests/domains/transformation/ tests/domains/pipeline/ --tb=no --quiet", 
            "python -m pytest tests/domains/aqwa/ tests/domains/time_series/ --tb=no --quiet",
            "python -m pytest tests/domains/viv_analysis/ tests/domains/catenary_riser/ --tb=no --quiet",
            "python -m pytest tests/core/ --tb=no --quiet"
        ]
        
        for iteration in range(iterations):
            print(f"\nIteration {iteration + 1}/{iterations}")
            iteration_results = {}
            
            for i, cmd in enumerate(test_commands):
                print(f"  Running test group {i+1}/5...")
                start_time = time.time()
                
                try:
                    result = subprocess.run(
                        cmd.split(), 
                        capture_output=True, 
                        text=True, 
                        timeout=300  # 5 minute timeout
                    )
                    
                    duration = time.time() - start_time
                    
                    # Parse results
                    output = result.stdout
                    if "passed" in output:
                        # Extract pass/fail counts
                        lines = output.split('\n')
                        summary_line = next((line for line in lines if 'passed' in line and ('failed' in line or 'error' in line or line.strip().endswith('passed'))), '')
                        
                        iteration_results[f'group_{i+1}'] = {
                            'command': cmd,
                            'summary': summary_line.strip(),
                            'duration': duration,
                            'returncode': result.returncode,
                            'stdout_lines': len(output.split('\n'))
                        }
                    else:
                        iteration_results[f'group_{i+1}'] = {
                            'command': cmd,
                            'summary': 'No results found',
                            'duration': duration,
                            'returncode': result.returncode,
                            'stdout_lines': len(output.split('\n'))
                        }
                        
                except subprocess.TimeoutExpired:
                    iteration_results[f'group_{i+1}'] = {
                        'command': cmd,
                        'summary': 'TIMEOUT',
                        'duration': 300,
                        'returncode': -1,
                        'stdout_lines': 0
                    }
                except Exception as e:
                    iteration_results[f'group_{i+1}'] = {
                        'command': cmd,
                        'summary': f'ERROR: {str(e)}',
                        'duration': 0,
                        'returncode': -1,
                        'stdout_lines': 0
                    }
            
            self.results['test_runs'].append({
                'iteration': iteration + 1,
                'timestamp': time.time(),
                'results': iteration_results
            })
            
        self.analyze_stability()
        return self.results
    
    def analyze_stability(self):
        """Analyze results for stability patterns."""
        print("\nAnalyzing test stability patterns...")
        
        # Track result consistency
        group_results = defaultdict(list)
        group_durations = defaultdict(list)
        
        for run in self.results['test_runs']:
            for group, data in run['results'].items():
                group_results[group].append(data['summary'])
                group_durations[group].append(data['duration'])
        
        # Identify flaky vs consistent tests
        for group, summaries in group_results.items():
            unique_results = set(summaries)
            
            if len(unique_results) == 1:
                # Consistent results
                result = list(unique_results)[0]
                if 'passed' in result and 'failed' not in result:
                    self.results['consistent_passes'].append({
                        'group': group,
                        'result': result,
                        'avg_duration': sum(group_durations[group]) / len(group_durations[group])
                    })
                else:
                    self.results['consistent_failures'].append({
                        'group': group,  
                        'result': result,
                        'avg_duration': sum(group_durations[group]) / len(group_durations[group])
                    })
            else:
                # Flaky tests
                self.results['flaky_tests'][group] = {
                    'results': list(unique_results),
                    'occurrences': dict(Counter(summaries)),
                    'avg_duration': sum(group_durations[group]) / len(group_durations[group])
                }
        
        # Performance analysis
        for group, durations in group_durations.items():
            avg_duration = sum(durations) / len(durations)
            max_duration = max(durations)
            min_duration = min(durations)
            
            self.results['performance_data'][group] = {
                'avg_duration': avg_duration,
                'max_duration': max_duration,
                'min_duration': min_duration,
                'duration_variance': max_duration - min_duration,
                'performance_flag': 'SLOW' if avg_duration > 60 else 'NORMAL'
            }
    
    def generate_recommendations(self):
        """Generate testing recommendations before new development."""
        print("\nGenerating pre-development recommendations...")
        
        recommendations = {
            'critical_actions': [],
            'performance_improvements': [],
            'stability_improvements': [],
            'validation_coverage': [],
            'ci_cd_setup': []
        }
        
        # Critical actions
        if self.results['flaky_tests']:
            recommendations['critical_actions'].append(
                f"CRITICAL: Fix {len(self.results['flaky_tests'])} flaky test groups before new development"
            )
        
        if len(self.results['consistent_failures']) > 0:
            recommendations['critical_actions'].append(
                f"URGENT: Investigate {len(self.results['consistent_failures'])} consistently failing test groups"
            )
            
        # Performance recommendations
        slow_groups = [g for g, data in self.results['performance_data'].items() 
                      if data['performance_flag'] == 'SLOW']
        if slow_groups:
            recommendations['performance_improvements'].append(
                f"PERFORMANCE: Optimize {len(slow_groups)} slow test groups (>60s each)"
            )
        
        # Stability recommendations  
        if not self.results['flaky_tests'] and len(self.results['consistent_passes']) > 3:
            recommendations['stability_improvements'].append(
                "STABLE: Test suite shows good stability - ready for development"
            )
        else:
            recommendations['stability_improvements'].append(
                "UNSTABLE: Stabilize flaky tests before adding new features"
            )
            
        # Validation coverage
        recommendations['validation_coverage'].extend([
            "Add key output validations to remaining 34 failed tests",
            "Implement performance regression detection",
            "Add integration test coverage for critical paths"
        ])
        
        # CI/CD setup
        recommendations['ci_cd_setup'].extend([
            "Set up automated test runs on commits",
            "Implement test result trending/monitoring", 
            "Add test gates for merge requests",
            "Schedule nightly stability runs"
        ])
        
        self.results['recommendation_summary'] = recommendations
        return recommendations
    
    def save_results(self, filename='test_stability_report.json'):
        """Save analysis results to file."""
        with open(filename, 'w') as f:
            json.dump(self.results, f, indent=2)
        print(f"\nResults saved to {filename}")
    
    def print_summary(self):
        """Print analysis summary."""
        print("\n" + "="*80)
        print("TEST STABILITY ANALYSIS SUMMARY")
        print("="*80)
        
        print(f"\nRESULTS OVERVIEW:")
        print(f"  - Consistent Passing Groups: {len(self.results['consistent_passes'])}")
        print(f"  - Consistent Failing Groups: {len(self.results['consistent_failures'])}")
        print(f"  - Flaky Test Groups: {len(self.results['flaky_tests'])}")
        
        if self.results['flaky_tests']:
            print(f"\nFLAKY TESTS DETECTED:")
            for group, data in self.results['flaky_tests'].items():
                print(f"  - {group}: {data['occurrences']}")
        
        print(f"\nPERFORMANCE SUMMARY:")
        for group, perf in self.results['performance_data'].items():
            flag = perf['performance_flag']
            duration = perf['avg_duration']
            print(f"  - {group}: {duration:.1f}s avg {flag}")
        
        print(f"\nRECOMMENDATIONS:")
        recs = self.results['recommendation_summary']
        
        for category, items in recs.items():
            if items:
                print(f"\n  {category.upper().replace('_', ' ')}:")
                for item in items:
                    print(f"    - {item}")
        
        print("\n" + "="*80)

def main():
    """Run test stability analysis."""
    analyzer = TestStabilityAnalyzer()
    
    print("Starting Digital Model Test Stability Analysis")
    print("This will help establish a solid testing baseline before new development.\n")
    
    # Run analysis
    results = analyzer.run_stability_analysis(iterations=3)
    
    # Generate recommendations
    recommendations = analyzer.generate_recommendations()
    
    # Print summary
    analyzer.print_summary()
    
    # Save results
    analyzer.save_results()
    
    # Exit with appropriate code
    if analyzer.results['flaky_tests'] or len(analyzer.results['consistent_failures']) > 2:
        print("\nRECOMMENDATION: Address stability issues before new development")
        return 1
    else:
        print("\nTest suite stability looks good for new development!")
        return 0

if __name__ == "__main__":
    sys.exit(main())