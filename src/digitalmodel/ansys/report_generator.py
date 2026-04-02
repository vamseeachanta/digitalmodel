# ABOUTME: Auto-generate FEA report sections — model summary, mesh, BCs, results
# ABOUTME: Outputs markdown report text from analysis configuration and results

"""
Report Generator
================

Automatically generates FEA report sections in markdown format from
ANSYS analysis configuration and results data:

    - Model summary (geometry, materials, element types)
    - Mesh quality metrics (element count, aspect ratio, Jacobian)
    - Boundary conditions table
    - Results summary table (max stress, displacement, reactions)
    - Convergence check status
    - ASME/DNV code compliance summary
    - Load case matrix

The report generator consumes dataclasses from other modules in this
package and produces markdown-formatted text suitable for inclusion
in engineering reports.

No ANSYS installation required.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Optional

import pandas as pd


# ---------------------------------------------------------------------------
# Report data containers
# ---------------------------------------------------------------------------

@dataclass
class ModelInfo:
    """FEA model summary information."""
    project_name: str = "Pressure Vessel Analysis"
    analyst: str = ""
    date: str = ""
    software_version: str = "ANSYS Mechanical APDL 2024 R2"
    description: str = ""
    units: str = "mm, N, MPa, tonne, s"


@dataclass
class MeshMetrics:
    """Mesh quality metrics."""
    total_nodes: int = 0
    total_elements: int = 0
    element_types: list[str] = field(default_factory=list)
    min_element_size_mm: float = 0.0
    max_element_size_mm: float = 0.0
    avg_element_size_mm: float = 0.0
    min_aspect_ratio: float = 1.0
    max_aspect_ratio: float = 1.0
    avg_aspect_ratio: float = 1.0
    min_jacobian_ratio: float = 1.0
    elements_above_ar_limit: int = 0  # elements above aspect ratio limit
    aspect_ratio_limit: float = 10.0


@dataclass
class BoundaryConditionSummary:
    """Summary of a boundary condition for reporting."""
    bc_id: str = ""
    bc_type: str = ""  # Fixed support, Pressure, Force, Convection
    location: str = ""
    magnitude: str = ""
    direction: str = ""
    load_step: int = 1


@dataclass
class ResultEntry:
    """Single result entry for the results summary table."""
    result_type: str = ""  # von Mises stress, displacement, reaction force
    location: str = ""
    max_value: float = 0.0
    min_value: float = 0.0
    allowable: float = 0.0
    utilization: float = 0.0
    unit: str = "MPa"
    status: str = "PASS"


@dataclass
class ConvergenceInfo:
    """Convergence status information."""
    converged: bool = True
    num_substeps: int = 1
    num_iterations: int = 1
    final_force_residual: float = 0.0
    force_criterion: float = 0.001
    final_displacement_residual: float = 0.0
    displacement_criterion: float = 0.001


@dataclass
class ReportConfig:
    """Complete report configuration."""
    model_info: ModelInfo = field(default_factory=ModelInfo)
    mesh_metrics: MeshMetrics = field(default_factory=MeshMetrics)
    boundary_conditions: list[BoundaryConditionSummary] = field(
        default_factory=list
    )
    results: list[ResultEntry] = field(default_factory=list)
    convergence: ConvergenceInfo = field(default_factory=ConvergenceInfo)
    load_cases: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Report generator
# ---------------------------------------------------------------------------

class ReportGenerator:
    """Generate FEA report sections in markdown format."""

    def generate_model_summary(self, info: ModelInfo) -> str:
        """Generate the model summary section.

        Returns
        -------
        str
            Markdown text for model summary.
        """
        date = info.date or datetime.now().strftime("%Y-%m-%d")
        lines = [
            "## 1. Model Summary",
            "",
            f"| Parameter | Value |",
            f"|-----------|-------|",
            f"| Project | {info.project_name} |",
            f"| Analyst | {info.analyst} |",
            f"| Date | {date} |",
            f"| Software | {info.software_version} |",
            f"| Units | {info.units} |",
            "",
        ]
        if info.description:
            lines.extend([
                f"**Description:** {info.description}",
                "",
            ])
        return "\n".join(lines)

    def generate_mesh_summary(self, mesh: MeshMetrics) -> str:
        """Generate the mesh quality summary section.

        Returns
        -------
        str
            Markdown text for mesh quality metrics.
        """
        et_str = ", ".join(mesh.element_types) if mesh.element_types else "N/A"

        # Determine mesh quality status
        ar_ok = mesh.max_aspect_ratio <= mesh.aspect_ratio_limit
        jac_ok = mesh.min_jacobian_ratio > 0

        lines = [
            "## 2. Mesh Quality",
            "",
            f"| Metric | Value | Criterion | Status |",
            f"|--------|-------|-----------|--------|",
            f"| Total nodes | {mesh.total_nodes:,} | - | - |",
            f"| Total elements | {mesh.total_elements:,} | - | - |",
            f"| Element types | {et_str} | - | - |",
            f"| Element size range | "
            f"{mesh.min_element_size_mm:.1f} – {mesh.max_element_size_mm:.1f} mm"
            f" | - | - |",
            f"| Average element size | {mesh.avg_element_size_mm:.1f} mm"
            f" | - | - |",
            f"| Max aspect ratio | {mesh.max_aspect_ratio:.1f}"
            f" | < {mesh.aspect_ratio_limit:.0f}"
            f" | {'PASS' if ar_ok else 'FAIL'} |",
            f"| Min Jacobian ratio | {mesh.min_jacobian_ratio:.3f}"
            f" | > 0 | {'PASS' if jac_ok else 'FAIL'} |",
            f"| Elements above AR limit | {mesh.elements_above_ar_limit}"
            f" | 0 | {'PASS' if mesh.elements_above_ar_limit == 0 else 'REVIEW'} |",
            "",
        ]
        return "\n".join(lines)

    def generate_boundary_conditions_table(
        self, bcs: list[BoundaryConditionSummary]
    ) -> str:
        """Generate the boundary conditions table.

        Returns
        -------
        str
            Markdown text for BC table.
        """
        lines = [
            "## 3. Boundary Conditions",
            "",
            f"| ID | Type | Location | Magnitude | Direction | Load Step |",
            f"|----|------|----------|-----------|-----------|-----------|",
        ]
        for bc in bcs:
            lines.append(
                f"| {bc.bc_id} | {bc.bc_type} | {bc.location} "
                f"| {bc.magnitude} | {bc.direction} | {bc.load_step} |"
            )
        lines.append("")
        return "\n".join(lines)

    def generate_results_summary(self, results: list[ResultEntry]) -> str:
        """Generate the results summary table.

        Returns
        -------
        str
            Markdown text for results table.
        """
        lines = [
            "## 4. Results Summary",
            "",
            f"| Result | Location | Max Value | Allowable"
            f" | Utilization | Status |",
            f"|--------|----------|-----------|-----------|"
            f"-------------|--------|",
        ]
        for r in results:
            util_pct = f"{r.utilization * 100:.1f}%" if r.utilization > 0 else "N/A"
            lines.append(
                f"| {r.result_type} | {r.location} "
                f"| {r.max_value:.1f} {r.unit} "
                f"| {r.allowable:.1f} {r.unit} "
                f"| {util_pct} | {r.status} |"
            )
        lines.append("")
        return "\n".join(lines)

    def generate_convergence_section(
        self, conv: ConvergenceInfo
    ) -> str:
        """Generate the convergence check section.

        Returns
        -------
        str
            Markdown text for convergence info.
        """
        status = "CONVERGED" if conv.converged else "NOT CONVERGED"
        lines = [
            "## 5. Solution Convergence",
            "",
            f"| Parameter | Value |",
            f"|-----------|-------|",
            f"| Status | **{status}** |",
            f"| Substeps | {conv.num_substeps} |",
            f"| Total iterations | {conv.num_iterations} |",
            f"| Force residual | {conv.final_force_residual:.2E} |",
            f"| Force criterion | {conv.force_criterion:.2E} |",
            f"| Displacement residual | {conv.final_displacement_residual:.2E} |",
            f"| Displacement criterion | {conv.displacement_criterion:.2E} |",
            "",
        ]
        return "\n".join(lines)

    def generate_load_case_matrix(
        self, load_cases: list[str]
    ) -> str:
        """Generate the load case matrix section.

        Returns
        -------
        str
            Markdown text for load case list.
        """
        lines = [
            "## 6. Load Cases",
            "",
        ]
        for i, lc in enumerate(load_cases, start=1):
            lines.append(f"{i}. {lc}")
        lines.append("")
        return "\n".join(lines)

    def generate_notes_section(self, notes: list[str]) -> str:
        """Generate the notes and assumptions section.

        Returns
        -------
        str
            Markdown text for notes.
        """
        lines = [
            "## 7. Notes and Assumptions",
            "",
        ]
        for note in notes:
            lines.append(f"- {note}")
        lines.append("")
        return "\n".join(lines)

    def generate_full_report(self, config: ReportConfig) -> str:
        """Generate a complete FEA report in markdown format.

        Parameters
        ----------
        config : ReportConfig
            Complete report configuration.

        Returns
        -------
        str
            Full markdown report text.
        """
        title = config.model_info.project_name

        sections = [
            f"# FEA Report: {title}",
            "",
            "---",
            "",
            self.generate_model_summary(config.model_info),
            self.generate_mesh_summary(config.mesh_metrics),
            self.generate_boundary_conditions_table(config.boundary_conditions),
            self.generate_results_summary(config.results),
            self.generate_convergence_section(config.convergence),
        ]

        if config.load_cases:
            sections.append(self.generate_load_case_matrix(config.load_cases))

        if config.notes:
            sections.append(self.generate_notes_section(config.notes))

        sections.extend([
            "---",
            "",
            f"*Report generated on {datetime.now().strftime('%Y-%m-%d %H:%M')}*",
            "",
        ])

        return "\n".join(sections)

    def results_to_dataframe(
        self, results: list[ResultEntry]
    ) -> pd.DataFrame:
        """Convert result entries to a pandas DataFrame.

        Parameters
        ----------
        results : list[ResultEntry]
            Result entries.

        Returns
        -------
        pd.DataFrame
            Results as tabular data.
        """
        records = []
        for r in results:
            records.append({
                "result_type": r.result_type,
                "location": r.location,
                "max_value": r.max_value,
                "min_value": r.min_value,
                "allowable": r.allowable,
                "utilization": r.utilization,
                "unit": r.unit,
                "status": r.status,
            })
        return pd.DataFrame(records)
