"""Assumption provenance ledger (back-compat re-export).

The ledger was promoted to the domain-neutral
:mod:`digitalmodel.common.assumption_ledger` so the OrcaFlex inverse resolver
can share it without depending on the diffraction package. This module re-exports
it to preserve the original import path.
"""

from __future__ import annotations

from digitalmodel.common.assumption_ledger import (
    AssumptionLedger,
    AssumptionRecord,
    AssumptionSource,
    Confidence,
)

__all__ = [
    "AssumptionLedger",
    "AssumptionRecord",
    "AssumptionSource",
    "Confidence",
]
