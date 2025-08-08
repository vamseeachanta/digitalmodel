"""
Coverage Tracking Integration for Test Suite Automation

This module provides comprehensive coverage tracking, analysis, and reporting
capabilities integrated with the test automation framework.
"""

import json
import os
import sqlite3
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import subprocess
import xml.etree.ElementTree as ET


class CoverageTracker:
    """Comprehensive coverage tracking and analysis system."""
    
    def __init__(self, project_root: str = "."):
        """Initialize coverage tracker with project configuration."""
        self.project_root = Path(project_root)
        self.coverage_dir = self.project_root / "coverage_reports"
        self.coverage_dir.mkdir(exist_ok=True)
        self.db_path = self.coverage_dir / "coverage_trends.db"
        self.thresholds = {
            "overall": 80.0,
            "new_code": 90.0,
            "critical_modules": 85.0
        }
        self._init_database()
    
    def _init_database(self):
        """Initialize SQLite database for coverage trending."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS coverage_trends (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
                branch TEXT,
                commit_hash TEXT,
                overall_coverage REAL,
                line_coverage REAL,
                branch_coverage REAL,
                function_coverage REAL,
                total_lines INTEGER,
                covered_lines INTEGER,
                total_branches INTEGER,
                covered_branches INTEGER,
                module_coverage TEXT,
                uncovered_files TEXT,
                report_path TEXT
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS module_coverage (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                trend_id INTEGER,
                module_name TEXT,
                coverage_percentage REAL,
                lines_covered INTEGER,
                lines_total INTEGER,
                FOREIGN KEY (trend_id) REFERENCES coverage_trends (id)
            )
        """)
        
        conn.commit()
        conn.close()
    
    def run_coverage_analysis(self, test_path: str = "tests/", 
                             modules: Optional[List[str]] = None) -> Dict:
        """
        Run pytest with coverage analysis.
        
        Args:
            test_path: Path to test directory or specific test files
            modules: Specific modules to analyze
            
        Returns:
            Coverage analysis results dictionary
        """
        timestamp = datetime.now().isoformat()
        report_dir = self.coverage_dir / timestamp.replace(":", "-")
        report_dir.mkdir(exist_ok=True)
        
        # Build pytest command with coverage options
        cmd = [
            "python", "-m", "pytest",
            test_path,
            f"--cov=src/digitalmodel",
            f"--cov-report=html:{report_dir}/html",
            f"--cov-report=json:{report_dir}/coverage.json",
            f"--cov-report=xml:{report_dir}/coverage.xml",
            f"--cov-report=lcov:{report_dir}/coverage.lcov",
            "--cov-report=term-missing",
            "--tb=short",
            "-q"
        ]
        
        if modules:
            for module in modules:
                cmd.append(f"--cov={module}")
        
        # Run coverage analysis
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        # Parse coverage results
        coverage_data = self._parse_coverage_json(report_dir / "coverage.json")
        
        # Store in database
        self._store_coverage_trend(coverage_data, str(report_dir))
        
        # Check thresholds
        threshold_results = self._check_thresholds(coverage_data)
        
        return {
            "timestamp": timestamp,
            "report_path": str(report_dir),
            "coverage_data": coverage_data,
            "threshold_results": threshold_results,
            "command_output": result.stdout,
            "errors": result.stderr if result.returncode != 0 else None
        }
    
    def _parse_coverage_json(self, json_path: Path) -> Dict:
        """Parse coverage JSON report."""
        if not json_path.exists():
            return {}
        
        with open(json_path, 'r') as f:
            data = json.load(f)
        
        # Extract summary metrics
        summary = data.get("totals", {})
        files = data.get("files", {})
        
        # Calculate module-level coverage
        module_coverage = {}
        for file_path, file_data in files.items():
            module = self._get_module_name(file_path)
            if module not in module_coverage:
                module_coverage[module] = {
                    "covered_lines": 0,
                    "total_lines": 0,
                    "coverage": 0.0
                }
            
            file_summary = file_data.get("summary", {})
            module_coverage[module]["covered_lines"] += file_summary.get("covered_lines", 0)
            module_coverage[module]["total_lines"] += file_summary.get("num_statements", 0)
        
        # Calculate percentages
        for module in module_coverage.values():
            if module["total_lines"] > 0:
                module["coverage"] = (module["covered_lines"] / module["total_lines"]) * 100
        
        return {
            "overall_coverage": summary.get("percent_covered", 0),
            "line_coverage": summary.get("percent_covered", 0),
            "branch_coverage": summary.get("percent_covered_branches", 0),
            "total_lines": summary.get("num_statements", 0),
            "covered_lines": summary.get("covered_lines", 0),
            "total_branches": summary.get("num_branches", 0),
            "covered_branches": summary.get("covered_branches", 0),
            "missing_lines": summary.get("missing_lines", 0),
            "excluded_lines": summary.get("excluded_lines", 0),
            "module_coverage": module_coverage,
            "files": files
        }
    
    def _get_module_name(self, file_path: str) -> str:
        """Extract module name from file path."""
        parts = Path(file_path).parts
        if "modules" in parts:
            idx = parts.index("modules")
            if idx + 1 < len(parts):
                return parts[idx + 1]
        return "core"
    
    def _store_coverage_trend(self, coverage_data: Dict, report_path: str):
        """Store coverage data in trending database."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Get git information
        commit_hash = self._get_git_commit()
        branch = self._get_git_branch()
        
        # Insert main coverage record
        cursor.execute("""
            INSERT INTO coverage_trends (
                branch, commit_hash, overall_coverage, line_coverage,
                branch_coverage, total_lines, covered_lines,
                total_branches, covered_branches, module_coverage,
                report_path
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            branch,
            commit_hash,
            coverage_data.get("overall_coverage", 0),
            coverage_data.get("line_coverage", 0),
            coverage_data.get("branch_coverage", 0),
            coverage_data.get("total_lines", 0),
            coverage_data.get("covered_lines", 0),
            coverage_data.get("total_branches", 0),
            coverage_data.get("covered_branches", 0),
            json.dumps(coverage_data.get("module_coverage", {})),
            report_path
        ))
        
        trend_id = cursor.lastrowid
        
        # Insert module-level coverage
        for module_name, module_data in coverage_data.get("module_coverage", {}).items():
            cursor.execute("""
                INSERT INTO module_coverage (
                    trend_id, module_name, coverage_percentage,
                    lines_covered, lines_total
                ) VALUES (?, ?, ?, ?, ?)
            """, (
                trend_id,
                module_name,
                module_data.get("coverage", 0),
                module_data.get("covered_lines", 0),
                module_data.get("total_lines", 0)
            ))
        
        conn.commit()
        conn.close()
    
    def _get_git_commit(self) -> str:
        """Get current git commit hash."""
        try:
            result = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                text=True
            )
            return result.stdout.strip()[:8]
        except:
            return "unknown"
    
    def _get_git_branch(self) -> str:
        """Get current git branch."""
        try:
            result = subprocess.run(
                ["git", "branch", "--show-current"],
                capture_output=True,
                text=True
            )
            return result.stdout.strip()
        except:
            return "unknown"
    
    def _check_thresholds(self, coverage_data: Dict) -> Dict:
        """Check coverage against configured thresholds."""
        results = {
            "passed": True,
            "violations": [],
            "warnings": []
        }
        
        overall = coverage_data.get("overall_coverage", 0)
        
        # Check overall threshold
        if overall < self.thresholds["overall"]:
            results["passed"] = False
            results["violations"].append(
                f"Overall coverage {overall:.1f}% is below threshold {self.thresholds['overall']}%"
            )
        
        # Check critical modules
        critical_modules = ["aqwa", "pipeline", "fatigue_analysis"]
        for module in critical_modules:
            module_data = coverage_data.get("module_coverage", {}).get(module, {})
            module_cov = module_data.get("coverage", 0)
            if module_cov < self.thresholds["critical_modules"]:
                results["warnings"].append(
                    f"Critical module '{module}' coverage {module_cov:.1f}% "
                    f"is below threshold {self.thresholds['critical_modules']}%"
                )
        
        return results
    
    def get_coverage_trends(self, days: int = 30) -> List[Dict]:
        """Get coverage trends for the last N days."""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            SELECT * FROM coverage_trends
            WHERE timestamp > datetime('now', '-{} days')
            ORDER BY timestamp DESC
        """.format(days))
        
        columns = [desc[0] for desc in cursor.description]
        results = []
        for row in cursor.fetchall():
            results.append(dict(zip(columns, row)))
        
        conn.close()
        return results
    
    def generate_gap_analysis(self) -> Dict:
        """Generate coverage gap analysis report."""
        # Run current coverage analysis
        current = self.run_coverage_analysis()
        coverage_data = current["coverage_data"]
        
        gaps = {
            "uncovered_modules": [],
            "low_coverage_modules": [],
            "critical_gaps": [],
            "recommendations": []
        }
        
        # Identify modules with no coverage
        all_modules = set(self._discover_all_modules())
        covered_modules = set(coverage_data.get("module_coverage", {}).keys())
        gaps["uncovered_modules"] = list(all_modules - covered_modules)
        
        # Identify low coverage modules
        for module, data in coverage_data.get("module_coverage", {}).items():
            if data.get("coverage", 0) < 50:
                gaps["low_coverage_modules"].append({
                    "module": module,
                    "coverage": data.get("coverage", 0),
                    "missing_lines": data.get("total_lines", 0) - data.get("covered_lines", 0)
                })
        
        # Generate recommendations
        if gaps["uncovered_modules"]:
            gaps["recommendations"].append(
                f"Add tests for {len(gaps['uncovered_modules'])} uncovered modules"
            )
        
        if gaps["low_coverage_modules"]:
            gaps["recommendations"].append(
                f"Improve coverage for {len(gaps['low_coverage_modules'])} modules below 50%"
            )
        
        return gaps
    
    def _discover_all_modules(self) -> List[str]:
        """Discover all modules in the project."""
        modules_dir = self.project_root / "src" / "digitalmodel" / "modules"
        if not modules_dir.exists():
            return []
        
        return [d.name for d in modules_dir.iterdir() if d.is_dir() and not d.name.startswith("__")]
    
    def detect_coverage_regression(self) -> Optional[Dict]:
        """Detect coverage regression from previous run."""
        trends = self.get_coverage_trends(days=7)
        
        if len(trends) < 2:
            return None
        
        current = trends[0]
        previous = trends[1]
        
        regression = {
            "detected": False,
            "details": []
        }
        
        # Check overall coverage regression
        current_cov = current.get("overall_coverage", 0)
        previous_cov = previous.get("overall_coverage", 0)
        
        if current_cov < previous_cov - 1.0:  # Allow 1% variance
            regression["detected"] = True
            regression["details"].append(
                f"Overall coverage decreased from {previous_cov:.1f}% to {current_cov:.1f}%"
            )
        
        # Check module-level regressions
        current_modules = json.loads(current.get("module_coverage", "{}"))
        previous_modules = json.loads(previous.get("module_coverage", "{}"))
        
        for module in current_modules:
            if module in previous_modules:
                curr_cov = current_modules[module].get("coverage", 0)
                prev_cov = previous_modules[module].get("coverage", 0)
                if curr_cov < prev_cov - 2.0:  # Allow 2% variance for modules
                    regression["detected"] = True
                    regression["details"].append(
                        f"Module '{module}' coverage decreased from {prev_cov:.1f}% to {curr_cov:.1f}%"
                    )
        
        return regression if regression["detected"] else None


class CoverageQualityGates:
    """Implement coverage-based quality gates for CI/CD."""
    
    def __init__(self, tracker: CoverageTracker):
        """Initialize quality gates with coverage tracker."""
        self.tracker = tracker
    
    def check_pull_request(self, base_branch: str = "main") -> Dict:
        """Check coverage requirements for pull request."""
        # Run coverage analysis
        current = self.tracker.run_coverage_analysis()
        
        gate_results = {
            "passed": True,
            "checks": [],
            "coverage_summary": current["coverage_data"]
        }
        
        # Check overall coverage threshold
        overall = current["coverage_data"].get("overall_coverage", 0)
        if overall < self.tracker.thresholds["overall"]:
            gate_results["passed"] = False
            gate_results["checks"].append({
                "name": "Overall Coverage",
                "passed": False,
                "message": f"Coverage {overall:.1f}% is below required {self.tracker.thresholds['overall']}%"
            })
        else:
            gate_results["checks"].append({
                "name": "Overall Coverage",
                "passed": True,
                "message": f"Coverage {overall:.1f}% meets requirement"
            })
        
        # Check for regression
        regression = self.tracker.detect_coverage_regression()
        if regression:
            gate_results["passed"] = False
            gate_results["checks"].append({
                "name": "Coverage Regression",
                "passed": False,
                "message": "Coverage regression detected",
                "details": regression["details"]
            })
        else:
            gate_results["checks"].append({
                "name": "Coverage Regression",
                "passed": True,
                "message": "No coverage regression detected"
            })
        
        return gate_results
    
    def generate_badge_data(self) -> Dict:
        """Generate coverage badge data for README."""
        trends = self.tracker.get_coverage_trends(days=1)
        if not trends:
            return {"coverage": 0, "color": "red", "status": "unknown"}
        
        current = trends[0]
        coverage = current.get("overall_coverage", 0)
        
        # Determine badge color
        if coverage >= 80:
            color = "brightgreen"
        elif coverage >= 60:
            color = "yellow"
        elif coverage >= 40:
            color = "orange"
        else:
            color = "red"
        
        return {
            "coverage": coverage,
            "color": color,
            "status": f"{coverage:.1f}%"
        }


def main():
    """Main entry point for coverage tracking."""
    tracker = CoverageTracker()
    
    print("Running coverage analysis...")
    results = tracker.run_coverage_analysis()
    
    print(f"\nCoverage Report Generated: {results['report_path']}")
    print(f"Overall Coverage: {results['coverage_data'].get('overall_coverage', 0):.1f}%")
    
    # Check thresholds
    threshold_results = results["threshold_results"]
    if not threshold_results["passed"]:
        print("\n⚠️  Coverage Threshold Violations:")
        for violation in threshold_results["violations"]:
            print(f"  - {violation}")
    
    if threshold_results["warnings"]:
        print("\n⚠️  Coverage Warnings:")
        for warning in threshold_results["warnings"]:
            print(f"  - {warning}")
    
    # Check for regression
    regression = tracker.detect_coverage_regression()
    if regression:
        print("\n⚠️  Coverage Regression Detected:")
        for detail in regression["details"]:
            print(f"  - {detail}")
    
    # Generate gap analysis
    print("\nGenerating coverage gap analysis...")
    gaps = tracker.generate_gap_analysis()
    
    if gaps["uncovered_modules"]:
        print(f"\nUncovered Modules ({len(gaps['uncovered_modules'])}):")
        for module in gaps["uncovered_modules"][:5]:
            print(f"  - {module}")
    
    if gaps["recommendations"]:
        print("\nRecommendations:")
        for rec in gaps["recommendations"]:
            print(f"  - {rec}")
    
    # Quality gates check
    gates = CoverageQualityGates(tracker)
    gate_results = gates.check_pull_request()
    
    print(f"\n{'✅' if gate_results['passed'] else '❌'} Quality Gates: {'PASSED' if gate_results['passed'] else 'FAILED'}")
    for check in gate_results["checks"]:
        status = "✅" if check["passed"] else "❌"
        print(f"  {status} {check['name']}: {check['message']}")
    
    return 0 if gate_results["passed"] else 1


if __name__ == "__main__":
    exit(main())