"""
Before/After Implementation Comparison Tracker.

This module provides automated tracking and comparison of test status
before and after code/spec implementations, generating executive summaries
for decision makers and maintaining historical implementation impact records.
"""

import json
import os
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
from collections import defaultdict

from test_automation.config import config
from test_automation.logging_config import get_logger
from test_automation.core.runner import ModuleResult
from test_automation.core.reporting import ReportGenerator

logger = get_logger('test_automation.comparison_tracker')


@dataclass
class TestBaseline:
    """Represents a test baseline snapshot."""
    timestamp: str
    label: str
    description: str
    modules_summary: Dict[str, Any]
    tests_summary: Dict[str, Any]
    success_rate: float
    total_duration: float
    detailed_results: Dict[str, Dict[str, Any]]
    environment_info: Dict[str, Any]


@dataclass
class ComparisonResult:
    """Results of comparing two test baselines."""
    baseline_before: TestBaseline
    baseline_after: TestBaseline
    comparison_timestamp: str
    
    # Summary changes
    modules_change: Dict[str, int]
    tests_change: Dict[str, int]
    success_rate_change: float
    duration_change: float
    
    # Detailed changes
    improved_modules: List[str]
    degraded_modules: List[str]
    new_failing_modules: List[str]
    newly_passing_modules: List[str]
    
    # Impact analysis
    impact_score: float  # 0-100, higher means more significant impact
    impact_category: str  # 'positive', 'negative', 'mixed', 'neutral'
    key_findings: List[str]


class BaselineManager:
    """Manages test baselines for before/after comparisons."""
    
    def __init__(self):
        self.baselines_dir = Path("test_baselines")
        self.baselines_dir.mkdir(exist_ok=True)
        self.current_baseline_file = self.baselines_dir / "current_baseline.json"
    
    def capture_baseline(self, label: str = None, description: str = None) -> TestBaseline:
        """Capture current test status as a baseline."""
        logger.info(f"Capturing test baseline: {label or 'unnamed'}")
        
        # Generate test status
        report_generator = ReportGenerator()
        report_data = report_generator.generate_status_report()
        
        timestamp = datetime.now(timezone.utc).isoformat()
        
        # Create baseline
        baseline = TestBaseline(
            timestamp=timestamp,
            label=label or f"baseline_{datetime.now().strftime('%Y%m%d_%H%M%S')}",
            description=description or "Automated baseline capture",
            modules_summary=report_data['current_status']['modules'].copy(),
            tests_summary=report_data['current_status']['tests'].copy(),
            success_rate=report_data['current_status']['success_rate'],
            total_duration=sum(
                result['total_duration'] 
                for result in report_data['detailed_results'].values()
            ),
            detailed_results=report_data['detailed_results'].copy(),
            environment_info={
                'discovery_info': report_data['discovery_info'],
                'timestamp': timestamp
            }
        )
        
        # Save baseline
        baseline_file = self.baselines_dir / f"{baseline.label}.json"
        with open(baseline_file, 'w', encoding='utf-8') as f:
            json.dump(asdict(baseline), f, indent=2)
        
        logger.info(f"Baseline saved: {baseline_file}")
        return baseline
    
    def save_current_baseline(self, baseline: TestBaseline):
        """Save as the current baseline for comparison."""
        with open(self.current_baseline_file, 'w', encoding='utf-8') as f:
            json.dump(asdict(baseline), f, indent=2)
    
    def load_current_baseline(self) -> Optional[TestBaseline]:
        """Load the current baseline."""
        if not self.current_baseline_file.exists():
            return None
        
        try:
            with open(self.current_baseline_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            return TestBaseline(**data)
        except Exception as e:
            logger.warning(f"Could not load current baseline: {e}")
            return None
    
    def load_baseline(self, label: str) -> Optional[TestBaseline]:
        """Load a specific baseline by label."""
        baseline_file = self.baselines_dir / f"{label}.json"
        
        if not baseline_file.exists():
            return None
        
        try:
            with open(baseline_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            return TestBaseline(**data)
        except Exception as e:
            logger.warning(f"Could not load baseline {label}: {e}")
            return None


class ComparisonAnalyzer:
    """Analyzes differences between test baselines."""
    
    def compare_baselines(self, before: TestBaseline, after: TestBaseline) -> ComparisonResult:
        """Compare two baselines and generate analysis."""
        logger.info(f"Comparing baselines: {before.label} vs {after.label}")
        
        # Calculate summary changes
        modules_change = {
            'total': after.modules_summary['total'] - before.modules_summary['total'],
            'passed': after.modules_summary['passed'] - before.modules_summary['passed'],
            'failed': after.modules_summary['failed'] - before.modules_summary['failed'],
            'skipped': after.modules_summary.get('skipped', 0) - before.modules_summary.get('skipped', 0)
        }
        
        tests_change = {
            'total': after.tests_summary['total'] - before.tests_summary['total'],
            'passed': after.tests_summary['passed'] - before.tests_summary['passed'],
            'failed': after.tests_summary['failed'] - before.tests_summary['failed'],
            'errors': after.tests_summary.get('errors', 0) - before.tests_summary.get('errors', 0)
        }
        
        success_rate_change = after.success_rate - before.success_rate
        duration_change = after.total_duration - before.total_duration
        
        # Analyze module-level changes
        improved_modules = []
        degraded_modules = []
        new_failing_modules = []
        newly_passing_modules = []
        
        all_modules = set(before.detailed_results.keys()) | set(after.detailed_results.keys())
        
        for module_name in all_modules:
            before_result = before.detailed_results.get(module_name)
            after_result = after.detailed_results.get(module_name)
            
            if not before_result and after_result:
                # New module
                if after_result['status'] == 'passed':
                    newly_passing_modules.append(module_name)
                else:
                    new_failing_modules.append(module_name)
            elif before_result and not after_result:
                # Module removed (shouldn't happen often)
                pass
            elif before_result and after_result:
                before_status = before_result['status']
                after_status = after_result['status']
                
                # Status changes
                if before_status != 'passed' and after_status == 'passed':
                    newly_passing_modules.append(module_name)
                elif before_status == 'passed' and after_status != 'passed':
                    new_failing_modules.append(module_name)
                elif before_status != 'passed' and after_status != 'passed':
                    # Both failing, check if improved
                    before_success = before_result.get('passed_tests', 0) / max(before_result.get('total_tests', 1), 1)
                    after_success = after_result.get('passed_tests', 0) / max(after_result.get('total_tests', 1), 1)
                    
                    if after_success > before_success + 0.1:  # 10% improvement threshold
                        improved_modules.append(module_name)
                    elif after_success < before_success - 0.1:  # 10% degradation threshold
                        degraded_modules.append(module_name)
        
        # Calculate impact score and category
        impact_score, impact_category, key_findings = self._calculate_impact(
            modules_change, tests_change, success_rate_change, 
            improved_modules, degraded_modules, new_failing_modules, newly_passing_modules
        )
        
        return ComparisonResult(
            baseline_before=before,
            baseline_after=after,
            comparison_timestamp=datetime.now(timezone.utc).isoformat(),
            modules_change=modules_change,
            tests_change=tests_change,
            success_rate_change=success_rate_change,
            duration_change=duration_change,
            improved_modules=improved_modules,
            degraded_modules=degraded_modules,
            new_failing_modules=new_failing_modules,
            newly_passing_modules=newly_passing_modules,
            impact_score=impact_score,
            impact_category=impact_category,
            key_findings=key_findings
        )
    
    def _calculate_impact(self, modules_change: Dict, tests_change: Dict, success_rate_change: float,
                         improved: List[str], degraded: List[str], 
                         new_failing: List[str], newly_passing: List[str]) -> Tuple[float, str, List[str]]:
        """Calculate impact score, category, and key findings."""
        
        # Calculate weighted impact score (0-100)
        impact_score = 50.0  # Neutral baseline
        
        # Success rate impact (40% weight)
        impact_score += success_rate_change * 0.4
        
        # Module status changes (30% weight) 
        module_impact = (len(newly_passing) * 5 - len(new_failing) * 8 + 
                        len(improved) * 2 - len(degraded) * 3)
        impact_score += module_impact * 0.3
        
        # Test count changes (20% weight)
        test_impact = (tests_change.get('passed', 0) * 0.5 - 
                      tests_change.get('failed', 0) * 0.8)
        impact_score += test_impact * 0.2
        
        # Duration changes (10% weight) - improvements reduce score slightly
        if success_rate_change > 0:  # Only penalize duration if tests got worse
            duration_impact = min(0, -abs(tests_change.get('total', 1)) * 0.1 * 0.1)
            impact_score += duration_impact
        
        # Clamp to 0-100 range
        impact_score = max(0, min(100, impact_score))
        
        # Determine category
        if impact_score >= 65:
            impact_category = 'positive'
        elif impact_score <= 35:
            impact_category = 'negative'
        elif len(newly_passing) > 0 and len(new_failing) > 0:
            impact_category = 'mixed'
        else:
            impact_category = 'neutral'
        
        # Generate key findings
        key_findings = []
        
        if success_rate_change > 5:
            key_findings.append(f"Significant improvement in success rate (+{success_rate_change:.1f}%)")
        elif success_rate_change < -5:
            key_findings.append(f"Concerning decline in success rate ({success_rate_change:.1f}%)")
        
        if len(newly_passing) > 0:
            key_findings.append(f"{len(newly_passing)} modules now passing: {', '.join(newly_passing[:3])}")
        
        if len(new_failing) > 0:
            key_findings.append(f"{len(new_failing)} modules now failing: {', '.join(new_failing[:3])}")
        
        if tests_change.get('total', 0) > 0:
            key_findings.append(f"Added {tests_change['total']} new tests")
        
        if abs(tests_change.get('passed', 0)) > 5:
            direction = "more" if tests_change['passed'] > 0 else "fewer"
            key_findings.append(f"{abs(tests_change['passed'])} {direction} passing tests")
        
        if not key_findings:
            key_findings.append("No significant changes detected")
        
        return impact_score, impact_category, key_findings


class SpecComparisonReporter:
    """Generates comparison reports for spec implementations."""
    
    def __init__(self):
        self.baseline_manager = BaselineManager()
        self.analyzer = ComparisonAnalyzer()
    
    def generate_spec_summary(self, spec_path: str, comparison: ComparisonResult, 
                            implementation_notes: str = None) -> str:
        """Generate markdown summary for spec folder."""
        
        # Extract spec name from path
        spec_name = Path(spec_path).name
        
        markdown = f"""# Test Summary: {spec_name}

## Executive Summary

**Implementation Impact:** {comparison.impact_category.title()} (Score: {comparison.impact_score:.1f}/100)

**Key Changes:**
- Success Rate: {comparison.baseline_before.success_rate:.1f}% → {comparison.baseline_after.success_rate:.1f}% ({comparison.success_rate_change:+.1f}%)
- Passing Modules: {comparison.baseline_before.modules_summary['passed']} → {comparison.baseline_after.modules_summary['passed']} ({comparison.modules_change['passed']:+d})
- Passing Tests: {comparison.baseline_before.tests_summary['passed']} → {comparison.baseline_after.tests_summary['passed']} ({comparison.tests_change['passed']:+d})

## Implementation Impact Analysis

### Key Findings
{"".join(f"- {finding}\\n" for finding in comparison.key_findings)}

### Module Status Changes

"""
        
        if comparison.newly_passing_modules:
            markdown += f"""#### ✅ Newly Passing Modules ({len(comparison.newly_passing_modules)})
{"".join(f"- `{module}`\\n" for module in comparison.newly_passing_modules)}

"""
        
        if comparison.new_failing_modules:
            markdown += f"""#### ❌ Newly Failing Modules ({len(comparison.new_failing_modules)})
{"".join(f"- `{module}`\\n" for module in comparison.new_failing_modules)}

"""
        
        if comparison.improved_modules:
            markdown += f"""#### 📈 Improved Modules ({len(comparison.improved_modules)})
{"".join(f"- `{module}`\\n" for module in comparison.improved_modules)}

"""
        
        if comparison.degraded_modules:
            markdown += f"""#### 📉 Degraded Modules ({len(comparison.degraded_modules)})
{"".join(f"- `{module}`\\n" for module in comparison.degraded_modules)}

"""
        
        markdown += f"""## Detailed Metrics

### Before Implementation (`{comparison.baseline_before.label}`)
- **Modules:** {comparison.baseline_before.modules_summary['total']} total, {comparison.baseline_before.modules_summary['passed']} passed, {comparison.baseline_before.modules_summary['failed']} failed
- **Tests:** {comparison.baseline_before.tests_summary['total']} total, {comparison.baseline_before.tests_summary['passed']} passed, {comparison.baseline_before.tests_summary['failed']} failed
- **Success Rate:** {comparison.baseline_before.success_rate:.1f}%
- **Duration:** {comparison.baseline_before.total_duration:.1f}s

### After Implementation (`{comparison.baseline_after.label}`)
- **Modules:** {comparison.baseline_after.modules_summary['total']} total, {comparison.baseline_after.modules_summary['passed']} passed, {comparison.baseline_after.modules_summary['failed']} failed  
- **Tests:** {comparison.baseline_after.tests_summary['total']} total, {comparison.baseline_after.tests_summary['passed']} passed, {comparison.baseline_after.tests_summary['failed']} failed
- **Success Rate:** {comparison.baseline_after.success_rate:.1f}%
- **Duration:** {comparison.baseline_after.total_duration:.1f}s

### Changes Summary
- **Success Rate Change:** {comparison.success_rate_change:+.1f}%
- **Module Changes:** {comparison.modules_change['passed']:+d} passed, {comparison.modules_change['failed']:+d} failed
- **Test Changes:** {comparison.tests_change['passed']:+d} passed, {comparison.tests_change['failed']:+d} failed
- **Duration Change:** {comparison.duration_change:+.1f}s

"""
        
        if implementation_notes:
            markdown += f"""## Implementation Notes

{implementation_notes}

"""
        
        markdown += f"""## Timestamps

- **Before Baseline:** {comparison.baseline_before.timestamp}
- **After Baseline:** {comparison.baseline_after.timestamp}  
- **Comparison Generated:** {comparison.comparison_timestamp}

---
*This report was automatically generated by the test automation system.*
"""
        
        return markdown
    
    def save_spec_summary(self, spec_path: str, comparison: ComparisonResult, 
                         implementation_notes: str = None) -> str:
        """Save test summary to spec folder."""
        
        spec_dir = Path(spec_path)
        if spec_dir.is_file():
            spec_dir = spec_dir.parent
        
        summary_content = self.generate_spec_summary(str(spec_dir), comparison, implementation_notes)
        summary_file = spec_dir / "test_summary.md"
        
        with open(summary_file, 'w', encoding='utf-8') as f:
            f.write(summary_content)
        
        logger.info(f"Spec test summary saved: {summary_file}")
        return str(summary_file)


class ImplementationTracker:
    """Main tracker for before/after implementation comparisons."""
    
    def __init__(self):
        self.baseline_manager = BaselineManager()
        self.analyzer = ComparisonAnalyzer()
        self.reporter = SpecComparisonReporter()
    
    def start_implementation(self, spec_path: str, description: str = None) -> str:
        """Capture baseline before starting implementation."""
        spec_name = Path(spec_path).name if spec_path else "unknown_spec"
        label = f"before_{spec_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        description = description or f"Baseline before implementing {spec_name}"
        
        baseline = self.baseline_manager.capture_baseline(label, description)
        self.baseline_manager.save_current_baseline(baseline)
        
        logger.info(f"Implementation started - baseline captured: {label}")
        return label
    
    def complete_implementation(self, spec_path: str, implementation_notes: str = None) -> str:
        """Capture baseline after implementation and generate comparison."""
        
        # Load before baseline
        before_baseline = self.baseline_manager.load_current_baseline()
        if not before_baseline:
            logger.warning("No before baseline found - capturing current state as baseline")
            return self.start_implementation(spec_path, "Baseline capture (no before state)")
        
        # Capture after baseline
        spec_name = Path(spec_path).name if spec_path else "unknown_spec"
        after_label = f"after_{spec_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        after_description = f"Baseline after implementing {spec_name}"
        
        after_baseline = self.baseline_manager.capture_baseline(after_label, after_description)
        
        # Generate comparison
        comparison = self.analyzer.compare_baselines(before_baseline, after_baseline)
        
        # Save spec summary
        summary_file = self.reporter.save_spec_summary(spec_path, comparison, implementation_notes)
        
        # Save comparison for history
        comparison_file = Path("test_baselines") / f"comparison_{spec_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(comparison_file, 'w', encoding='utf-8') as f:
            json.dump(asdict(comparison), f, indent=2, default=str)
        
        logger.info(f"Implementation completed - comparison saved: {comparison_file}")
        logger.info(f"Spec summary generated: {summary_file}")
        
        return str(comparison_file)
    
    def generate_comparison_report(self, before_label: str, after_label: str, 
                                 spec_path: str = None) -> ComparisonResult:
        """Generate comparison between two specific baselines."""
        
        before_baseline = self.baseline_manager.load_baseline(before_label)
        after_baseline = self.baseline_manager.load_baseline(after_label)
        
        if not before_baseline:
            raise ValueError(f"Before baseline not found: {before_label}")
        if not after_baseline:
            raise ValueError(f"After baseline not found: {after_label}")
        
        comparison = self.analyzer.compare_baselines(before_baseline, after_baseline)
        
        if spec_path:
            self.reporter.save_spec_summary(spec_path, comparison)
        
        return comparison