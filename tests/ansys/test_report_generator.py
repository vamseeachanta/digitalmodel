# ABOUTME: Tests for ReportGenerator — FEA report markdown generation
# ABOUTME: Verifies report section content, table formatting, status fields

"""Tests for report_generator — ReportGenerator methods."""

import pytest
import pandas as pd

from digitalmodel.ansys.report_generator import (
    BoundaryConditionSummary,
    ConvergenceInfo,
    MeshMetrics,
    ModelInfo,
    ReportConfig,
    ReportGenerator,
    ResultEntry,
)


def _gen() -> ReportGenerator:
    return ReportGenerator()


# ---------------------------------------------------------------------------
# Model summary
# ---------------------------------------------------------------------------

class TestModelSummary:
    def test_contains_project_name(self):
        info = ModelInfo(project_name="PV Analysis Rev A")
        text = _gen().generate_model_summary(info)
        assert "PV Analysis Rev A" in text

    def test_contains_software_version(self):
        info = ModelInfo(software_version="ANSYS 2024 R2")
        text = _gen().generate_model_summary(info)
        assert "ANSYS 2024 R2" in text

    def test_markdown_table_format(self):
        info = ModelInfo()
        text = _gen().generate_model_summary(info)
        assert "|" in text
        assert "---" in text

    def test_includes_description_when_set(self):
        info = ModelInfo(description="Test description")
        text = _gen().generate_model_summary(info)
        assert "Test description" in text


# ---------------------------------------------------------------------------
# Mesh summary
# ---------------------------------------------------------------------------

class TestMeshSummary:
    def test_contains_node_count(self):
        mesh = MeshMetrics(total_nodes=15000, total_elements=8000)
        text = _gen().generate_mesh_summary(mesh)
        assert "15,000" in text

    def test_aspect_ratio_pass(self):
        mesh = MeshMetrics(max_aspect_ratio=5.0, aspect_ratio_limit=10.0)
        text = _gen().generate_mesh_summary(mesh)
        assert "PASS" in text

    def test_aspect_ratio_fail(self):
        mesh = MeshMetrics(max_aspect_ratio=15.0, aspect_ratio_limit=10.0)
        text = _gen().generate_mesh_summary(mesh)
        assert "FAIL" in text

    def test_jacobian_pass(self):
        mesh = MeshMetrics(min_jacobian_ratio=0.5)
        text = _gen().generate_mesh_summary(mesh)
        assert "PASS" in text


# ---------------------------------------------------------------------------
# Boundary conditions table
# ---------------------------------------------------------------------------

class TestBCTable:
    def test_contains_bc_entries(self):
        bcs = [
            BoundaryConditionSummary(
                bc_id="BC1", bc_type="Fixed Support",
                location="Base", magnitude="0 mm", direction="All DOF",
            ),
        ]
        text = _gen().generate_boundary_conditions_table(bcs)
        assert "BC1" in text
        assert "Fixed Support" in text

    def test_table_header_present(self):
        text = _gen().generate_boundary_conditions_table([])
        assert "Type" in text
        assert "Location" in text

    def test_multiple_bcs(self):
        bcs = [
            BoundaryConditionSummary(bc_id="BC1", bc_type="Support"),
            BoundaryConditionSummary(bc_id="BC2", bc_type="Pressure"),
        ]
        text = _gen().generate_boundary_conditions_table(bcs)
        assert "BC1" in text
        assert "BC2" in text


# ---------------------------------------------------------------------------
# Results summary
# ---------------------------------------------------------------------------

class TestResultsSummary:
    def test_contains_result_values(self):
        results = [
            ResultEntry(
                result_type="von Mises Stress",
                location="Shell mid-length",
                max_value=125.0,
                allowable=138.0,
                utilization=0.91,
                unit="MPa",
                status="PASS",
            ),
        ]
        text = _gen().generate_results_summary(results)
        assert "125.0" in text
        assert "138.0" in text
        assert "PASS" in text

    def test_utilization_as_percentage(self):
        results = [
            ResultEntry(utilization=0.85),
        ]
        text = _gen().generate_results_summary(results)
        assert "85.0%" in text

    def test_fail_status_shown(self):
        results = [
            ResultEntry(
                result_type="Stress",
                max_value=200.0,
                allowable=138.0,
                utilization=1.45,
                status="FAIL",
            ),
        ]
        text = _gen().generate_results_summary(results)
        assert "FAIL" in text


# ---------------------------------------------------------------------------
# Convergence section
# ---------------------------------------------------------------------------

class TestConvergenceSection:
    def test_converged_status(self):
        conv = ConvergenceInfo(converged=True)
        text = _gen().generate_convergence_section(conv)
        assert "CONVERGED" in text

    def test_not_converged_status(self):
        conv = ConvergenceInfo(converged=False)
        text = _gen().generate_convergence_section(conv)
        assert "NOT CONVERGED" in text

    def test_substep_count(self):
        conv = ConvergenceInfo(num_substeps=20)
        text = _gen().generate_convergence_section(conv)
        assert "20" in text


# ---------------------------------------------------------------------------
# Full report
# ---------------------------------------------------------------------------

class TestFullReport:
    def test_report_has_title(self):
        config = ReportConfig(
            model_info=ModelInfo(project_name="Test Project"),
        )
        text = _gen().generate_full_report(config)
        assert "# FEA Report: Test Project" in text

    def test_report_has_all_sections(self):
        config = ReportConfig(
            model_info=ModelInfo(),
            mesh_metrics=MeshMetrics(),
            boundary_conditions=[BoundaryConditionSummary()],
            results=[ResultEntry()],
            convergence=ConvergenceInfo(),
            load_cases=["LC1: Design pressure"],
            notes=["Linear elastic material"],
        )
        text = _gen().generate_full_report(config)
        assert "## 1. Model Summary" in text
        assert "## 2. Mesh Quality" in text
        assert "## 3. Boundary Conditions" in text
        assert "## 4. Results Summary" in text
        assert "## 5. Solution Convergence" in text
        assert "## 6. Load Cases" in text
        assert "## 7. Notes and Assumptions" in text

    def test_report_ends_with_timestamp(self):
        config = ReportConfig()
        text = _gen().generate_full_report(config)
        assert "Report generated on" in text


# ---------------------------------------------------------------------------
# DataFrame conversion
# ---------------------------------------------------------------------------

class TestResultsToDataFrame:
    def test_returns_dataframe(self):
        results = [ResultEntry(result_type="Stress", max_value=100.0)]
        df = _gen().results_to_dataframe(results)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 1

    def test_columns_present(self):
        results = [ResultEntry()]
        df = _gen().results_to_dataframe(results)
        assert "result_type" in df.columns
        assert "max_value" in df.columns
        assert "status" in df.columns

    def test_empty_results(self):
        df = _gen().results_to_dataframe([])
        assert len(df) == 0
