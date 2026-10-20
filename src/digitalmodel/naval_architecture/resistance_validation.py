# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Condition-matched benchmark checks and conservative lineage separation.

Comparison policy implements the benchmark contract in the issue plan
(2026-09-27), W4. All numerical acceptance limits must be supplied with
their basis; this module prescribes no physical accuracy threshold.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
import math
import re
from collections.abc import Sequence

from digitalmodel.naval_architecture.mesh_resistance import FlowConditions, _finite


@dataclass(frozen=True)
class BenchmarkCase:
    """A comparison input, not a data catalog or a qualification certificate.

    ``source_digest`` identifies normalized source content, independent of file
    name or revision label. Whole source snapshots and hull families are kept
    together by ``ensure_independent_split``. Callers own digest verification.
    A source being readable does not imply redistribution permission.
    """

    case_id: str
    family: str
    geometry_digest: str
    source_digest: str
    source_revision: str
    rights_basis: str
    redistribution_allowed: bool
    role: str
    conditions: FlowConditions
    length_m: float
    wetted_area_m2: float
    observable: str
    normalization: str
    units: str
    value: float

    def __post_init__(self) -> None:
        for name in (
            "case_id",
            "family",
            "source_revision",
            "rights_basis",
            "observable",
            "normalization",
            "units",
        ):
            if (
                not isinstance(getattr(self, name), str)
                or not getattr(self, name).strip()
            ):
                raise ValueError(f"{name} must be declared")
        for name in ("geometry_digest", "source_digest"):
            if not re.fullmatch(r"[0-9a-f]{64}", getattr(self, name)):
                raise ValueError(f"{name} must be a SHA-256 digest")
        if self.role not in {"train", "calibration", "validation", "test"}:
            raise ValueError("unknown benchmark role")
        if type(self.redistribution_allowed) is not bool:
            raise ValueError("redistribution permission must be explicit")
        if not isinstance(self.conditions, FlowConditions):
            raise ValueError("flow conditions must be supplied")
        for name in ("length_m", "wetted_area_m2"):
            object.__setattr__(
                self, name, _finite(name, getattr(self, name), positive=True)
            )
        object.__setattr__(self, "value", _finite("benchmark value", self.value))

    def to_public_dict(self) -> dict:
        """Export only a record explicitly permitted for redistribution."""
        if not self.redistribution_allowed:
            raise ValueError("source has no declared redistribution permission")
        return asdict(self)


def ensure_independent_split(cases: Sequence[BenchmarkCase]) -> None:
    """Refuse shared families, geometry or normalized sources across roles.

    Different revisions of a hull still share its family. Renaming files or
    changing case identifiers therefore does not evade the split policy.
    This does not detect deliberately falsified identities or unseen data.
    """
    seen: dict[tuple[str, str], str] = {}
    for case in cases:
        for name in ("family", "geometry_digest", "source_digest"):
            key = (name, getattr(case, name))
            if key in seen and seen[key] != case.role:
                raise ValueError(f"cross-role {name} leakage")
            seen[key] = case.role


def compare_cases(
    prediction: BenchmarkCase,
    reference: BenchmarkCase,
    *,
    absolute_tolerance: float,
    relative_tolerance: float,
    tolerance_source: str,
) -> dict:
    """Compare identical declared conditions; no hidden regime tolerance.

    Acceptance is |prediction-reference| <= atol + rtol*|reference|.
    This is a caller-selected arithmetic policy, not an ITTC acceptance rule.
    Exact condition equality is intentionally conservative: scale transfer and
    uncertainty matching must happen explicitly before this comparison.
    A passing comparison alone establishes no physical-validation status.
    """
    if not tolerance_source.strip():
        raise ValueError("tolerance source must be supplied")
    for name in (
        "conditions",
        "length_m",
        "wetted_area_m2",
        "geometry_digest",
        "observable",
        "normalization",
        "units",
    ):
        if getattr(prediction, name) != getattr(reference, name):
            raise ValueError(f"comparison {name} mismatch")
    atol = _finite("absolute tolerance", absolute_tolerance)
    rtol = _finite("relative tolerance", relative_tolerance)
    if atol < 0 or rtol < 0:
        raise ValueError("tolerances must be non-negative")
    error = abs(prediction.value - reference.value)
    limit = atol + rtol * abs(reference.value)
    if not math.isfinite(error) or not math.isfinite(limit):
        raise ValueError("comparison arithmetic must remain finite")
    return {
        "status": "PASS" if error <= limit else "FAIL",
        "absolute_error": error,
        "limit": limit,
        "units": reference.units,
        "tolerance_source": tolerance_source,
        "reference_revision": reference.source_revision,
        "reference_digest": reference.source_digest,
        "prediction_revision": prediction.source_revision,
    }
