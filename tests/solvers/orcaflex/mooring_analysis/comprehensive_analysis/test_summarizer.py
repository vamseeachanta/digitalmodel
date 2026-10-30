# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regression tests for summaries without a compliance assessment."""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.orcaflex.mooring_analysis.comprehensive_analysis.models import (
    AnalysisResults,
    ContextInfo,
    FenderMetrics,
    PretensionMetrics,
    StiffnessMatrix,
    StiffnessMetrics,
)
from digitalmodel.solvers.orcaflex.mooring_analysis.comprehensive_analysis.summarizer import (
    ComprehensiveSummarizer,
)


@pytest.fixture(params=[False, True], ids=["empty", "populated"])
def unassessed_results(request):
    """Supply absent metrics or synthetic metrics without assessed evidence."""
    results = AnalysisResults(
        run_id="synthetic",
        filename=Path("synthetic.csv"),
        context=ContextInfo(),
    )
    if request.param:
        # Independent synthetic basis: one line at unit target and actual tension.
        # Deviation and spread are zero; one converged line / one line = 100%.
        results.pretension_metrics = PretensionMetrics(
            mean_convergence=1.0,
            std_convergence=0.0,
            max_deviation=0.0,
            min_deviation=0.0,
            converged_lines=["synthetic-line"],
            problem_lines=[],
            total_lines=1,
            tension_distribution={"mean": 1.0, "std": 0.0},
            convergence_percentage=100.0,
            average_tension=1.0,
            tension_range=(1.0, 1.0),
        )
        # Unit stiffness along three axes gives unit ratios and anisotropy.
        matrix = np.eye(3)
        results.stiffness_metrics = StiffnessMetrics(
            stiffness_matrix=StiffnessMatrix(matrix=matrix, translational=matrix),
            dominant_direction=(1.0, 0.0, 0.0),
            stiffness_ratios={"x/y": 1.0, "x/z": 1.0, "y/z": 1.0},
            anisotropy_factor=1.0,
            critical_lines=[],
        )
        # An unloaded synthetic fender has zero forces, utilization and contact;
        # remaining margin = 100% - 0% = 100%. No standard criteria are implied.
        results.fender_metrics = FenderMetrics(
            utilization_rates={"synthetic-fender": 0.0},
            max_forces={"synthetic-fender": 0.0},
            mean_forces={"synthetic-fender": 0.0},
            critical_fenders=[],
            load_sharing={"synthetic-fender": 0.0},
            contact_percentages={"synthetic-fender": 0.0},
            force_distribution_stats={"mean": 0.0, "std": 0.0},
            overloaded_fenders=[],
            design_margin={"synthetic-fender": 100.0},
        )
    return results


def test_individual_summary_refuses_unassessed_compliance(unassessed_results):
    """Metrics alone must not produce positive individual compliance assurance."""
    with pytest.raises(
        NotImplementedError,
        match=r"DNV-OS-E301 compliance assessment is unavailable; .*cannot be assured",
    ):
        ComprehensiveSummarizer().summarize_individual(unassessed_results)


@pytest.mark.parametrize("grouped", [False, True], ids=["no-groups", "grouped"])
def test_overall_summary_refuses_unassessed_compliance(unassessed_results, grouped):
    """Empty collections and populated groups both lack a compliance assessment."""
    all_results = {"synthetic-group": [unassessed_results]} if grouped else {}
    with pytest.raises(
        NotImplementedError,
        match=r"DNV-OS-E301 compliance assessment is unavailable; .*cannot be assured",
    ):
        ComprehensiveSummarizer().summarize_overall(all_results)
