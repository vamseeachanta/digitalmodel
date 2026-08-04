"""Tests for BenchmarkRunner (end-to-end benchmark orchestration)."""
from __future__ import annotations

import importlib.util
import json
from pathlib import Path
from typing import Dict

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkConfig,
    BenchmarkRunner,
    BenchmarkRunResult,
    SolverType,
    run_benchmark,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)


def _load_ship_benchmark_script():
    script_path = Path("scripts/run_benchmark_ship_raos.py").resolve()
    spec = importlib.util.spec_from_file_location("run_benchmark_ship_raos", script_path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {script_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _synthetic_ship_rao_data() -> dict:
    frequencies = np.geomspace(1.0, 2.0, 10)
    return {
        0.0: {
            dof: {
                "freq": frequencies.tolist(),
                "amp": (frequencies * dof.value).tolist(),
                "phase": np.zeros(frequencies.size).tolist(),
            }
            for dof in DOF
        }
    }


# ---------------------------------------------------------------------------
# 1. BenchmarkConfig defaults and custom values
# ---------------------------------------------------------------------------


class TestBenchmarkConfig:
    """Validate BenchmarkConfig default fields and custom overrides."""

    def test_config_default_solvers(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert len(config.solvers) == 3
        assert SolverType.AQWA in config.solvers
        assert SolverType.ORCAWAVE in config.solvers
        assert SolverType.BEMROSETTA in config.solvers

    def test_config_has_no_unjustified_default_tolerance(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert config.tolerance is None

    def test_config_default_output_dir(self) -> None:
        # Act
        config = BenchmarkConfig()

        # Assert
        assert config.output_dir == Path("benchmark_output")

    def test_config_custom_values(self) -> None:
        # Act
        config = BenchmarkConfig(
            solver_relative_uncertainty=0.05,
            response_absolute_resolution=2e-9,
            minimum_explained_variance=0.9604,
            comparison_justification="Synthetic test uncertainty budget.",
            output_dir=Path("/tmp/custom_output"),
            headings=[0.0, 90.0, 180.0],
        )

        # Assert
        assert config.solver_relative_uncertainty == pytest.approx(0.05)
        assert config.response_absolute_resolution == pytest.approx(2e-9)
        assert config.minimum_explained_variance == pytest.approx(0.9604)
        assert config.output_dir == Path("/tmp/custom_output")
        assert config.headings == [0.0, 90.0, 180.0]

    def test_runner_derives_policy_from_named_uncertainty_inputs(self) -> None:
        config = BenchmarkConfig(
            solver_relative_uncertainty=0.025,
            response_absolute_resolution=5e-11,
            minimum_explained_variance=0.9801,
            comparison_justification="Synthetic test uncertainty budget.",
        )

        policy = BenchmarkRunner(config)._build_comparison_policy()

        assert policy.relative_rms_tolerance == pytest.approx(
            2.0 * config.solver_relative_uncertainty,
        )
        assert policy.absolute_rms_floor == pytest.approx(
            2.0 * config.response_absolute_resolution,
        )
        assert policy.correlation_minimum == pytest.approx(
            np.sqrt(config.minimum_explained_variance),
        )
        assert policy.justification == "Synthetic test uncertainty budget."


# ---------------------------------------------------------------------------
# 2. BenchmarkRunResult defaults
# ---------------------------------------------------------------------------


class TestBenchmarkRunResult:
    """Validate BenchmarkRunResult default state."""

    def test_run_result_defaults_not_successful(self) -> None:
        # Act
        result = BenchmarkRunResult()

        # Assert
        assert result.success is False
        assert result.report is None
        assert result.error_message is None
        assert result.plot_paths == []
        assert result.solver_results == {}


# ---------------------------------------------------------------------------
# 3. BenchmarkRunner.run_from_results
# ---------------------------------------------------------------------------


class TestRunFromResults:
    """Verify BenchmarkRunner.run_from_results orchestration."""

    def test_run_from_results_success(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.success is True
        assert result.report is not None
        assert len(result.plot_paths) > 0

    def test_run_from_results_creates_json_report(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.report_json_path is not None
        assert result.report_json_path.exists()
        assert result.report_json_path.suffix == ".json"

    def test_run_from_results_creates_html_report(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert
        assert result.report_html_path is not None
        assert result.report_html_path.exists()
        assert result.report_html_path.suffix == ".html"

    def test_run_from_results_dry_run(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        config = BenchmarkConfig(output_dir=tmp_path, dry_run=True)
        runner = BenchmarkRunner(config)

        # Act
        result = runner.run_from_results(three_solver_results)

        # Assert - dry run still produces a report but no plot files
        assert result.success is True
        assert result.report is not None
        assert result.plot_paths == []

    def test_unavailable_matrix_correlation_is_recorded_not_crashed(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        for solver_name, scale in (("SolverA", 1000.0), ("SolverB", 100.0)):
            results = two_identical_results[solver_name]
            for coefficient_set in (results.added_mass, results.damping):
                for matrix in coefficient_set.matrices:
                    matrix.matrix = np.eye(6) * scale
                    matrix.source = "solver"

        result = BenchmarkRunner(
            BenchmarkConfig(output_dir=tmp_path, dry_run=True),
        ).run_from_results(two_identical_results)

        actual = None
        if result.report_json_path and result.report_html_path:
            data = json.loads(result.report_json_path.read_text(encoding="utf-8"))
            pair = data["pairwise_results"]["SolverA-vs-SolverB"]
            actual = {
                "status": data["comparison_status"],
                "overall": data["overall_consensus"],
                "correlation": pair["added_mass_correlations"]["1,1"],
                "quality": pair["added_mass_quality"]["1,1"],
                # The 30 off-diagonal cells are zero on BOTH legs — a
                # structurally absent coupling, now reported as NOT_APPLICABLE
                # rather than claiming IDENTICAL agreement. The 6 diagonal
                # cells are constant across frequency and differ between the
                # solvers (1000 vs 100), so they stay INSUFFICIENT_DATA and the
                # report still REFUSES — which is the point of #1633.
                # Qualities are listed alphabetically by the distribution
                # builder, so INSUFFICIENT_DATA precedes NOT_APPLICABLE.
                "visible": (
                    "Unavailable (INSUFFICIENT_DATA: 6, NOT_APPLICABLE: 30)"
                ) in (
                    result.report_html_path.read_text(encoding="utf-8")
                ),
            }

        assert actual == {
            "status": "REFUSED",
            "overall": None,
            "correlation": None,
            "quality": "INSUFFICIENT_DATA",
            "visible": True,
        }

    def test_ship_placeholder_matrices_refuse_end_to_end_artifact(
        self, tmp_path: Path,
    ) -> None:
        script = _load_ship_benchmark_script()
        first = script.create_diffraction_results(
            _synthetic_ship_rao_data(), "SyntheticShip", "AQWA",
        )
        second = script.create_diffraction_results(
            _synthetic_ship_rao_data(), "SyntheticShip", "OrcaWave",
        )

        result = BenchmarkRunner(
            BenchmarkConfig(output_dir=tmp_path, dry_run=True),
        ).run_from_results({"AQWA": first, "OrcaWave": second})

        actual = None
        if result.report_json_path:
            data = json.loads(result.report_json_path.read_text(encoding="utf-8"))
            pair = data["pairwise_results"]["AQWA-vs-OrcaWave"]
            actual = {
                "matrix_source": first.added_mass.matrices[0].source,
                "status": data["comparison_status"],
                "overall": data["overall_consensus"],
                "quality": pair["added_mass_quality"]["1,1"],
                "correlation": pair["added_mass_correlations"]["1,1"],
            }

        assert actual == {
            "matrix_source": "placeholder",
            "status": "REFUSED",
            "overall": None,
            "quality": "UNTRUSTED_SOURCE",
            "correlation": None,
        }


# ---------------------------------------------------------------------------
# 4. Convenience function
# ---------------------------------------------------------------------------


class TestRunBenchmarkConvenience:
    """Verify the run_benchmark top-level convenience function."""

    def test_run_benchmark_convenience_function(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Act
        result = run_benchmark(
            solver_results=three_solver_results,
            output_dir=tmp_path,
            solver_relative_uncertainty=0.025,
            response_absolute_resolution=5e-11,
            minimum_explained_variance=0.9801,
            comparison_justification="Synthetic test uncertainty budget.",
        )

        # Assert
        assert result.success is True
        assert result.report is not None


class TestBenchmarkExecutiveSemanticSummary:
    def test_build_benchmark_executive_uses_taxonomy_labels(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        config = BenchmarkConfig(output_dir=tmp_path)
        runner = BenchmarkRunner(config)
        result = runner.run_from_results(three_solver_results)

        html = runner._build_benchmark_executive_html(
            result.report,
            semantic={
                "match_count": 10,
                "cosmetic_count": 1,
                "convention_count": 1,
                "significant_count": 1,
                "taxonomy_counts": {
                    "physics_significant": 0,
                    "solver_mode_significant": 1,
                    "representation_normalization_only": 1,
                    "output_only": 1,
                    "gui_only": 0,
                    "internal_default_only": 0,
                    "known_non_configurable_in_spec": 0,
                },
                "diffs": [
                    {
                        "key": "SolveType",
                        "level": "significant",
                        "category": "solver_mode_significant",
                        "owd": "Potential",
                        "spec": "Full QTF",
                    },
                    {
                        "key": "WavesReferredToBy",
                        "level": "convention",
                        "category": "representation_normalization_only",
                        "owd": "frequency (rad/s)",
                        "spec": "period (s)",
                    },
                    {
                        "key": "OutputPanelPressures",
                        "level": "cosmetic",
                        "category": "output_only",
                        "owd": "Yes",
                        "spec": "No",
                    },
                ],
            },
        )

        assert "solver_mode_significant" in html
        assert "representation_normalization_only" in html
        assert "output_only" in html


# ---------------------------------------------------------------------------
# 5. Coefficient visibility: distribution partitioning and coverage (#1633)
# ---------------------------------------------------------------------------


def _hydro_row_cells(html: str, matrix_label: str) -> tuple:
    """Return the plain-text cells of one Hydrodynamic Coefficients row."""
    import html as html_mod
    import re

    row = re.search(
        rf"<tr><td>{re.escape(matrix_label)}</td>(.*?)</tr>",
        html,
        re.DOTALL,
    )
    if row is None:
        return ()
    cells = re.findall(r"<td[^>]*>(.*?)</td>", row.group(1), re.DOTALL)
    return tuple(
        html_mod.unescape(re.sub(r"<[^>]+>", "", cell)).strip()
        for cell in cells
    )


def _diagonal_only_matrices(
    results: Dict[str, DiffractionResults],
) -> Dict[str, DiffractionResults]:
    """Give each solver a distinct constant diagonal and zero off-diagonal.

    Diagonal cells differ between the solvers and never vary with frequency,
    so they refuse with INSUFFICIENT_DATA. Off-diagonal cells are zero on both
    legs, so they are NOT_APPLICABLE. That produces a matrix whose diagonal
    and off-diagonal partitions have completely different quality
    distributions -- exactly the case the shared 36-cell distribution
    misreported.
    """
    for solver_name, scale in (("SolverA", 1000.0), ("SolverB", 100.0)):
        for coefficient_set in (
            results[solver_name].added_mass,
            results[solver_name].damping,
        ):
            for matrix in coefficient_set.matrices:
                matrix.matrix = np.eye(6) * scale
                matrix.source = "solver"
    return results


class TestCoefficientVisibility:
    """The printed numbers must describe the column that labels them."""

    def test_each_column_distribution_describes_only_its_own_cells(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        """Min Diagonal counted all 36 cells, not its own 6 (#1633).

        The diagonal partition is 6 INSUFFICIENT_DATA cells and the
        off-diagonal partition is 30 NOT_APPLICABLE cells. Only the overall
        column may legitimately show both.
        """
        result = BenchmarkRunner(
            BenchmarkConfig(output_dir=tmp_path, dry_run=True),
        ).run_from_results(_diagonal_only_matrices(two_identical_results))

        html = result.report_html_path.read_text(encoding="utf-8")

        assert _hydro_row_cells(html, "Added Mass") == (
            "Unavailable (INSUFFICIENT_DATA)",
            "Not compared (NOT_APPLICABLE)",
            "Unavailable (INSUFFICIENT_DATA: 6, NOT_APPLICABLE: 30)",
            "0 of 36 cells compared "
            "(INSUFFICIENT_DATA: 6, NOT_APPLICABLE: 30)",
        )

    def test_json_records_coverage_per_matrix_per_pair(
        self,
        two_identical_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        """78% of the shipped coefficient evidence was uncompared and
        invisible; coverage must be machine-readable (#1633)."""
        result = BenchmarkRunner(
            BenchmarkConfig(output_dir=tmp_path, dry_run=True),
        ).run_from_results(_diagonal_only_matrices(two_identical_results))

        data = json.loads(
            result.report_json_path.read_text(encoding="utf-8"),
        )
        pair = data["pairwise_results"]["SolverA-vs-SolverB"]

        assert {
            "added_mass_coverage": pair.get("added_mass_coverage"),
            "damping_coverage": pair.get("damping_coverage"),
        } == {
            "added_mass_coverage": {
                "compared_cells": 0,
                "total_cells": 36,
                "quality_counts": {
                    "INSUFFICIENT_DATA": 6,
                    "NOT_APPLICABLE": 30,
                },
            },
            "damping_coverage": {
                "compared_cells": 0,
                "total_cells": 36,
                "quality_counts": {
                    "INSUFFICIENT_DATA": 6,
                    "NOT_APPLICABLE": 30,
                },
            },
        }

    def test_html_states_coverage_for_every_matrix_of_every_pair(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        """Three pairs times two matrices = six coverage statements, in the
        6x6 section, plus six more in the per-pair summary tables."""
        result = BenchmarkRunner(
            BenchmarkConfig(output_dir=tmp_path, dry_run=True),
        ).run_from_results(three_solver_results)

        html = result.report_html_path.read_text(encoding="utf-8")

        assert html.count("cells compared") == 12
