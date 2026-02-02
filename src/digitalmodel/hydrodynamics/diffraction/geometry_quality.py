#!/usr/bin/env python3
"""
Geometry Quality Gates for Diffraction Analysis

ABOUTME: Quality validation for diffraction analysis geometry including watertightness, normals, and panel limits.

Provides quality checks for:
- Mesh watertightness (no holes)
- Consistent normal orientation
- Panel count limits
- Aspect ratio validation
- Element size consistency

Version: 3.0.0 (Phase 3)
Status: Geometry quality validation
"""

import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, field
import json


@dataclass
class GeometryQualityReport:
    """Quality check report for diffraction geometry"""
    geometry_file: str
    check_date: str

    # Mesh statistics
    num_nodes: int = 0
    num_panels: int = 0
    num_edges: int = 0

    # Quality checks
    is_watertight: bool = False
    watertight_issues: List[str] = field(default_factory=list)

    normals_consistent: bool = False
    normal_issues: List[str] = field(default_factory=list)

    panel_count_ok: bool = False
    panel_count_issues: List[str] = field(default_factory=list)

    aspect_ratio_ok: bool = False
    aspect_ratio_issues: List[str] = field(default_factory=list)

    element_size_ok: bool = False
    element_size_issues: List[str] = field(default_factory=list)

    # Overall status
    overall_status: str = "UNKNOWN"  # PASS, WARNING, FAIL
    passed_checks: int = 0
    total_checks: int = 5

    # Recommendations
    recommendations: List[str] = field(default_factory=list)


class GeometryQualityChecker:
    """Validate geometry quality for diffraction analysis"""

    def __init__(
        self,
        max_panels: int = 50000,
        min_aspect_ratio: float = 0.1,
        max_aspect_ratio: float = 10.0,
        element_size_tolerance: float = 0.5
    ):
        """
        Initialize quality checker

        Args:
            max_panels: Maximum recommended panel count
            min_aspect_ratio: Minimum acceptable aspect ratio
            max_aspect_ratio: Maximum acceptable aspect ratio
            element_size_tolerance: Tolerance for element size variation (0.5 = 50%)
        """
        self.max_panels = max_panels
        self.min_aspect_ratio = min_aspect_ratio
        self.max_aspect_ratio = max_aspect_ratio
        self.element_size_tolerance = element_size_tolerance

    def check_watertightness(
        self,
        nodes: np.ndarray,
        panels: np.ndarray
    ) -> Tuple[bool, List[str]]:
        """
        Check if mesh is watertight (no holes)

        Args:
            nodes: Nx3 array of node coordinates
            panels: Mx3 or Mx4 array of panel connectivity

        Returns:
            (is_watertight, list of issues)
        """
        issues = []

        # Check for duplicate nodes
        unique_nodes = np.unique(nodes, axis=0)
        if len(unique_nodes) != len(nodes):
            n_duplicates = len(nodes) - len(unique_nodes)
            issues.append(f"{n_duplicates} duplicate nodes found")

        # Check for degenerate panels (zero area)
        degenerate_count = 0
        for panel in panels:
            # Get panel nodes
            panel_nodes = nodes[panel[:3]]  # Use first 3 nodes

            # Calculate area using cross product
            v1 = panel_nodes[1] - panel_nodes[0]
            v2 = panel_nodes[2] - panel_nodes[0]
            area = 0.5 * np.linalg.norm(np.cross(v1, v2))

            if area < 1e-10:
                degenerate_count += 1

        if degenerate_count > 0:
            issues.append(f"{degenerate_count} degenerate panels (zero area)")

        # Check for open edges (simplified check)
        # In a watertight mesh, each edge should appear in exactly 2 panels
        edge_count = {}

        for panel in panels:
            # Get edges from panel
            n = len(panel)
            for i in range(n):
                edge = tuple(sorted([panel[i], panel[(i+1) % n]]))
                edge_count[edge] = edge_count.get(edge, 0) + 1

        # Find open edges
        open_edges = [edge for edge, count in edge_count.items() if count != 2]

        if open_edges:
            issues.append(f"{len(open_edges)} open edges found (not watertight)")

        is_watertight = len(issues) == 0

        if not is_watertight:
            issues.insert(0, "Mesh is not watertight")

        return is_watertight, issues

    def check_normals(
        self,
        nodes: np.ndarray,
        panels: np.ndarray
    ) -> Tuple[bool, List[str]]:
        """
        Check normal orientation consistency

        Args:
            nodes: Nx3 array of node coordinates
            panels: Mx3 or Mx4 array of panel connectivity

        Returns:
            (normals_consistent, list of issues)
        """
        issues = []

        # Calculate panel normals
        normals = []
        for panel in panels:
            panel_nodes = nodes[panel[:3]]

            v1 = panel_nodes[1] - panel_nodes[0]
            v2 = panel_nodes[2] - panel_nodes[0]
            normal = np.cross(v1, v2)

            norm_mag = np.linalg.norm(normal)
            if norm_mag > 1e-10:
                normal = normal / norm_mag
                normals.append(normal)

        normals = np.array(normals)

        # Check if most normals point outward (positive Z for simple check)
        avg_normal = np.mean(normals, axis=0)

        # Count panels with inconsistent normals
        inconsistent = 0
        for normal in normals:
            if np.dot(normal, avg_normal) < 0:
                inconsistent += 1

        if inconsistent > 0:
            pct = (inconsistent / len(normals)) * 100
            issues.append(f"{inconsistent} panels ({pct:.1f}%) have inconsistent normals")

            if pct > 10:
                issues.append("More than 10% inconsistent - check normal orientation")

        normals_consistent = len(issues) == 0

        return normals_consistent, issues

    def check_panel_count(
        self,
        num_panels: int
    ) -> Tuple[bool, List[str]]:
        """
        Check if panel count is within recommended limits

        Returns:
            (count_ok, list of issues)
        """
        issues = []

        if num_panels > self.max_panels:
            issues.append(
                f"Panel count ({num_panels}) exceeds recommended maximum ({self.max_panels})"
            )
            issues.append("High panel count may increase computation time significantly")

        if num_panels < 100:
            issues.append(
                f"Panel count ({num_panels}) is very low - results may be inaccurate"
            )

        count_ok = len(issues) == 0

        return count_ok, issues

    def check_aspect_ratios(
        self,
        nodes: np.ndarray,
        panels: np.ndarray
    ) -> Tuple[bool, List[str]]:
        """
        Check panel aspect ratios

        Returns:
            (aspect_ok, list of issues)
        """
        issues = []

        bad_aspect_count = 0
        max_aspect = 0.0
        min_aspect = float('inf')

        for panel in panels:
            panel_nodes = nodes[panel[:3]]

            # Calculate edge lengths
            edges = []
            n = len(panel)
            for i in range(n):
                edge_len = np.linalg.norm(panel_nodes[(i+1) % n] - panel_nodes[i])
                edges.append(edge_len)

            # Aspect ratio = max_edge / min_edge
            aspect = max(edges) / (min(edges) + 1e-10)

            max_aspect = max(max_aspect, aspect)
            min_aspect = min(min_aspect, aspect)

            if aspect < self.min_aspect_ratio or aspect > self.max_aspect_ratio:
                bad_aspect_count += 1

        if bad_aspect_count > 0:
            pct = (bad_aspect_count / len(panels)) * 100
            issues.append(
                f"{bad_aspect_count} panels ({pct:.1f}%) have poor aspect ratios "
                f"(outside [{self.min_aspect_ratio}, {self.max_aspect_ratio}])"
            )

        issues.append(f"Aspect ratio range: [{min_aspect:.2f}, {max_aspect:.2f}]")

        aspect_ok = bad_aspect_count == 0

        return aspect_ok, issues

    def check_element_sizes(
        self,
        nodes: np.ndarray,
        panels: np.ndarray
    ) -> Tuple[bool, List[str]]:
        """
        Check element size consistency

        Returns:
            (size_ok, list of issues)
        """
        issues = []

        # Calculate characteristic size for each panel
        sizes = []
        for panel in panels:
            panel_nodes = nodes[panel[:3]]

            # Use edge lengths
            edges = []
            n = len(panel)
            for i in range(n):
                edge_len = np.linalg.norm(panel_nodes[(i+1) % n] - panel_nodes[i])
                edges.append(edge_len)

            avg_edge = np.mean(edges)
            sizes.append(avg_edge)

        sizes = np.array(sizes)

        # Statistics
        mean_size = np.mean(sizes)
        std_size = np.std(sizes)
        min_size = np.min(sizes)
        max_size = np.max(sizes)

        # Check variation
        coeff_var = std_size / mean_size

        if coeff_var > self.element_size_tolerance:
            issues.append(
                f"Large element size variation (CV={coeff_var:.2f}, "
                f"tolerance={self.element_size_tolerance})"
            )

        issues.append(
            f"Element size range: [{min_size:.3f}, {max_size:.3f}] m (mean={mean_size:.3f} m)"
        )

        size_ok = coeff_var <= self.element_size_tolerance

        return size_ok, issues

    def generate_report(
        self,
        geometry_file: str,
        nodes: np.ndarray,
        panels: np.ndarray
    ) -> GeometryQualityReport:
        """
        Generate complete quality report

        Args:
            geometry_file: Path to geometry file
            nodes: Nx3 array of node coordinates
            panels: Mx3 or Mx4 array of panel connectivity

        Returns:
            GeometryQualityReport object
        """
        from datetime import datetime

        print("Running geometry quality checks...")
        print("=" * 80)

        report = GeometryQualityReport(
            geometry_file=geometry_file,
            check_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            num_nodes=len(nodes),
            num_panels=len(panels)
        )

        # Run checks
        print("1. Checking watertightness...")
        report.is_watertight, report.watertight_issues = self.check_watertightness(nodes, panels)
        if report.is_watertight:
            report.passed_checks += 1
            print("   [PASS] Mesh is watertight")
        else:
            print(f"   [FAIL] {len(report.watertight_issues)} issue(s) found")

        print("2. Checking normal orientation...")
        report.normals_consistent, report.normal_issues = self.check_normals(nodes, panels)
        if report.normals_consistent:
            report.passed_checks += 1
            print("   [PASS] Normals are consistent")
        else:
            print(f"   [WARNING] {len(report.normal_issues)} issue(s) found")

        print("3. Checking panel count...")
        report.panel_count_ok, report.panel_count_issues = self.check_panel_count(len(panels))
        if report.panel_count_ok:
            report.passed_checks += 1
            print("   [PASS] Panel count is acceptable")
        else:
            print(f"   [WARNING] {len(report.panel_count_issues)} issue(s) found")

        print("4. Checking aspect ratios...")
        report.aspect_ratio_ok, report.aspect_ratio_issues = self.check_aspect_ratios(nodes, panels)
        if report.aspect_ratio_ok:
            report.passed_checks += 1
            print("   [PASS] Aspect ratios are acceptable")
        else:
            print(f"   [WARNING] {len(report.aspect_ratio_issues)} issue(s) found")

        print("5. Checking element sizes...")
        report.element_size_ok, report.element_size_issues = self.check_element_sizes(nodes, panels)
        if report.element_size_ok:
            report.passed_checks += 1
            print("   [PASS] Element sizes are consistent")
        else:
            print(f"   [WARNING] {len(report.element_size_issues)} issue(s) found")

        # Overall status
        if report.passed_checks == report.total_checks:
            report.overall_status = "PASS"
        elif report.passed_checks >= 3:
            report.overall_status = "WARNING"
        else:
            report.overall_status = "FAIL"

        # Generate recommendations
        report.recommendations = self._generate_recommendations(report)

        print()
        print(f"Overall Status: {report.overall_status}")
        print(f"Passed Checks: {report.passed_checks}/{report.total_checks}")
        print("=" * 80)

        return report

    def _generate_recommendations(self, report: GeometryQualityReport) -> List[str]:
        """Generate recommendations based on quality check results"""
        recommendations = []

        if not report.is_watertight:
            recommendations.append("Fix watertight issues before running diffraction analysis")
            recommendations.append("Check for open edges and duplicate nodes in CAD model")

        if not report.normals_consistent:
            recommendations.append("Review and fix normal orientation in mesh")
            recommendations.append("Use CAD tools to unify normal direction")

        if not report.panel_count_ok:
            if report.num_panels > self.max_panels:
                recommendations.append("Consider coarsening mesh to reduce computation time")
            else:
                recommendations.append("Refine mesh for better accuracy")

        if not report.aspect_ratio_ok:
            recommendations.append("Improve panel aspect ratios for better numerical stability")
            recommendations.append("Target aspect ratios between 0.5 and 2.0")

        if not report.element_size_ok:
            recommendations.append("Create more uniform element size distribution")
            recommendations.append("Use mesh sizing controls in CAD software")

        return recommendations

    def export_report(self, report: GeometryQualityReport, output_file: Path):
        """Export quality report to JSON"""
        report_dict = {
            'geometry_file': report.geometry_file,
            'check_date': report.check_date,
            'mesh_statistics': {
                'num_nodes': report.num_nodes,
                'num_panels': report.num_panels
            },
            'quality_checks': {
                'watertight': {
                    'status': 'PASS' if report.is_watertight else 'FAIL',
                    'issues': report.watertight_issues
                },
                'normals': {
                    'status': 'PASS' if report.normals_consistent else 'WARNING',
                    'issues': report.normal_issues
                },
                'panel_count': {
                    'status': 'PASS' if report.panel_count_ok else 'WARNING',
                    'issues': report.panel_count_issues
                },
                'aspect_ratios': {
                    'status': 'PASS' if report.aspect_ratio_ok else 'WARNING',
                    'issues': report.aspect_ratio_issues
                },
                'element_sizes': {
                    'status': 'PASS' if report.element_size_ok else 'WARNING',
                    'issues': report.element_size_issues
                }
            },
            'overall_status': report.overall_status,
            'passed_checks': report.passed_checks,
            'total_checks': report.total_checks,
            'recommendations': report.recommendations
        }

        with open(output_file, 'w') as f:
            json.dump(report_dict, f, indent=2)

        print(f"\n[OK] Quality report exported to: {output_file}")
