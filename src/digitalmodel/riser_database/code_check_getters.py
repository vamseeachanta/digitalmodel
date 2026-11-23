# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Citation sidecars for legacy code-check factors; no calculation changes.

Metadata validation establishes citation identity, not independent validation
of the factor values or clause text against a licensed standard.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Final

from digitalmodel.citations.schema import Citation, CitedValue, validate_citation
from digitalmodel.riser_database.getters import _DNV_OS_F201_CITATION_TEMPLATE

# Retained from orcaflex.code_check_engine.DNVOSF201Input.safety_class_factor.
RISER_GAMMA_SC_LOW: Final[float] = 1.04
RISER_GAMMA_SC_NORMAL: Final[float] = 1.14
RISER_GAMMA_SC_HIGH: Final[float] = 1.26

_SAFETY_CLASS_FACTORS: Final = {
    "low": RISER_GAMMA_SC_LOW,
    "normal": RISER_GAMMA_SC_NORMAL,
    "high": RISER_GAMMA_SC_HIGH,
}


def get_dnv_safety_class_factor(
    safety_class: str = "normal",
    *,
    override: float | None = None,
    repo_root: Path | None = None,
) -> CitedValue:
    """Return a dimensionless legacy factor with validated DNV citation identity.

    Select ``low``, ``normal`` (default) or ``high``. A finite positive override
    wins and its note retains the selected legacy default. Unknown selectors
    raise ``ValueError`` rather than using the engine's silent normal fallback.
    Missing or mismatched citation metadata raises ``CitationResolutionError``;
    an override never bypasses validation and no standalone fallback is applied.
    """
    if not isinstance(safety_class, str) or safety_class not in _SAFETY_CLASS_FACTORS:
        raise ValueError("safety_class must be 'low', 'normal' or 'high'")
    default = _SAFETY_CLASS_FACTORS[safety_class]
    value = default
    note = (
        f"Safety class {safety_class}; legacy implementation default {default}; "
        "factor and clause text not independently validated against the licensed copy"
    )
    if override is not None:
        try:
            value = float(override)
        except (TypeError, ValueError, OverflowError) as exc:
            raise ValueError("override must be finite and positive") from exc
        if not math.isfinite(value) or value <= 0:
            raise ValueError("override must be finite and positive")
        note = f"user override {value}; {note}"
    citation = Citation(
        section="Sec.5 (safety class resistance factors)",
        note=note,
        source_sibling="generic",
        **_DNV_OS_F201_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")
