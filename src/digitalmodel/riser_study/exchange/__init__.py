# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Versioned, solver-free result records, readers and validation."""

from .records import CampaignRun, FatigueResult, StrengthResult
from .validation import (
    VERSION, exchange_schema, load_json, load_result, portable_path,
    validate_campaign_run, validate_result,
)
from .catalog import (
    CASE_IDS, SEA_STATE_IDS, assert_token_ids, load_campaign,
    load_case_matrix, load_token_registry, validate_matrix,
)

__all__ = [
    "VERSION", "CASE_IDS", "SEA_STATE_IDS", "CampaignRun", "FatigueResult",
    "StrengthResult", "exchange_schema", "load_json", "load_result", "portable_path",
    "validate_result", "validate_campaign_run", "load_case_matrix", "validate_matrix",
    "load_token_registry", "assert_token_ids", "load_campaign",
]
