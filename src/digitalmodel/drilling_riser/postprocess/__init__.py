"""Riser global-analysis post-processing (W5): aggregation of campaign results, extreme statistics over seeds,
coincident tension-moment-pressure checks and utilisation against a criteria register.

Reads the ``riser-w5-channels/1`` results written by the OrcaFlex campaign runner; needs no solver.
"""

from digitalmodel.drilling_riser.postprocess.aggregate import (
    CaseRecord,
    DigestMismatch,
    case_key,
    collect,
    load_verified,
    seed_groups,
)
from digitalmodel.drilling_riser.postprocess.channels import MissingChannel
from digitalmodel.drilling_riser.postprocess.checks import CHECKS, CheckValue, NotEvaluated, zero_crossing
from digitalmodel.drilling_riser.postprocess.evaluate import CaseCheck, evaluate_case, summarise
from digitalmodel.drilling_riser.postprocess.extremes import GumbelFit, gumbel_fit, gumbel_quantile

__all__ = [
    "CHECKS",
    "CaseCheck",
    "CaseRecord",
    "CheckValue",
    "DigestMismatch",
    "GumbelFit",
    "MissingChannel",
    "NotEvaluated",
    "case_key",
    "collect",
    "evaluate_case",
    "gumbel_fit",
    "gumbel_quantile",
    "load_verified",
    "seed_groups",
    "summarise",
    "zero_crossing",
]
