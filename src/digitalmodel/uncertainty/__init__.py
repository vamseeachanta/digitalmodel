"""Monte Carlo uncertainty quantification harness for digitalmodel.

This package implements the #1427 model-agnostic UQ core, motivated by the
public RUNSPEC/Kostro Monte Carlo workflow pattern: distribution priors, Latin
Hypercube and random designs, evaluator dispatch, and common post-processing
metrics. It samples live evaluators under uncertainty; that is separate from
:mod:`digitalmodel.parametric`, which interpolates deterministic responses
from pre-computed grids.
"""

from digitalmodel.uncertainty.analytics import (
    fan,
    percentile_summary,
    spearman_ranks,
    tornado,
)
from digitalmodel.uncertainty.config import UncertaintyStudy, load_study
from digitalmodel.uncertainty.distributions import Prior
from digitalmodel.uncertainty.doe import sample_lhs, sample_random
from digitalmodel.uncertainty.evaluator import Evaluator, run_matrix

__all__ = [
    "Evaluator",
    "Prior",
    "UncertaintyStudy",
    "fan",
    "load_study",
    "percentile_summary",
    "run_matrix",
    "sample_lhs",
    "sample_random",
    "spearman_ranks",
    "tornado",
]
