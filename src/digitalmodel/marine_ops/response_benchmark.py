# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspect evidence for a significant-motion response benchmark.

This is a metadata and byte-identity check, not engineering qualification.
Declarations of independence, frequency dependence, reuse rights and acceptance
criteria require independent review. No source is downloaded or executed.
See docs/response-benchmark-basis.md for scope and outstanding evidence.
"""

from __future__ import annotations

from dataclasses import dataclass, field, fields
from hashlib import sha256
import re
from typing import Literal, TypeGuard


# Envelope calculation basis B5: translations in m, rotations in deg.
# These are significant-motion units, not the m/m or deg/m RAO input units.
_AMPLITUDE_UNITS = {
    "surge": "m",
    "sway": "m",
    "heave": "m",
    "roll": "deg",
    "pitch": "deg",
    "yaw": "deg",
}
# RFC 6234 (2011), section 6.2: SHA-256 yields 256 bits, hence 64 hex digits.
_SHA256_HEX = re.compile(r"[0-9a-f]{64}")


@dataclass(frozen=True)
class ResponseReference:
    """Caller-supplied evidence locators; blank fields mean missing evidence.

    ``citation`` identifies the document edition and table/example location.
    ``source_locator`` locates that source; ``reuse_basis`` locates authorization
    to use its data. Independence and frequency-dependence fields locate evidence
    for those claims. ``acceptance_criteria`` identifies the intended-use limits,
    their units and their source, rather than selecting a default tolerance.
    ``declared_sha256`` identifies the exact supplied reference bytes.
    """

    citation: str | None = None
    source_locator: str | None = None
    reuse_basis: str | None = None
    independence_basis: str | None = None
    frequency_dependence_basis: str | None = None
    dof: str | None = None
    frequency_unit: str | None = None
    significant_amplitude_unit: str | None = None
    acceptance_criteria: str | None = None
    declared_sha256: str | None = None


@dataclass(frozen=True)
class ReferenceIntake:
    """Observable intake gaps; complete metadata never implies a benchmark pass."""

    gaps: tuple[str, ...]
    observed_sha256: str | None
    benchmark_status: Literal["NOT_EVALUATED"] = field(
        default="NOT_EVALUATED", init=False
    )

    @property
    def intake_complete(self) -> bool:
        """Return whether required declarations and matching bytes are present."""
        return not self.gaps


def inspect_response_reference(
    reference: ResponseReference, *, artifact: bytes | None = None
) -> ReferenceIntake:
    """Check required metadata, exact units and digest without authenticating it.

    Canonical angular frequency is rad/s, following envelope basis B5. Response
    is significant single amplitude, following B1, in the corresponding B5
    motion unit. Other conventions require an explicitly reviewed later adapter.
    The bytes are hashed as supplied, without decoding or newline normalization.

    Even complete intake leaves physical accuracy NOT_EVALUATED: numerical
    comparison, source review and review-role/lesson registration are separate.
    """
    gaps = [
        f"missing:{item.name}"
        for item in fields(reference)
        if not _present(getattr(reference, item.name))
    ]
    if _present(reference.dof):
        if reference.dof not in _AMPLITUDE_UNITS:
            gaps.append("unsupported:dof")
        elif (
            _present(reference.significant_amplitude_unit)
            and reference.significant_amplitude_unit != _AMPLITUDE_UNITS[reference.dof]
        ):
            gaps.append("incompatible:significant_amplitude_unit")
    if _present(reference.frequency_unit) and reference.frequency_unit != "rad/s":
        gaps.append("unsupported:frequency_unit")

    declared_valid = (
        _present(reference.declared_sha256)
        and _SHA256_HEX.fullmatch(reference.declared_sha256) is not None
    )
    if _present(reference.declared_sha256) and not declared_valid:
        gaps.append("invalid:declared_sha256")

    observed = None
    if artifact is None:
        gaps.append("missing:artifact")
    else:
        observed = sha256(artifact).hexdigest()
        if not artifact:
            gaps.append("empty:artifact")
        if declared_valid and observed != reference.declared_sha256:
            gaps.append("mismatch:artifact_sha256")
    return ReferenceIntake(tuple(gaps), observed)


def _present(value: object) -> TypeGuard[str]:
    return isinstance(value, str) and bool(value.strip())
