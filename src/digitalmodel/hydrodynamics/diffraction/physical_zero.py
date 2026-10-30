# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Fail-closed diagnostics for possible rotational radiation zeros.

Source: Orcina, OrcaWave online help, undated edition accessed 2026-10-20,
Results: Added mass and damping; First-order equations: The radiation potential.
See docs/domains/diffraction/physical-zero-qualification.html for source links.
Exact zero describes data, never geometry, extraction lineage or acceptance.
This module uses the matrix-set protocol without importing solver adapters.
"""

from dataclasses import asdict, dataclass, field
import hashlib
import json
import math
from typing import Any


@dataclass(frozen=True)
class BodyFrame:
    """Claimed body origin in metres and body axes in the reference frame.

    Frame consistency does not certify orthogonality or a physical mode.
    """

    origin_m: tuple[float, float, float]
    axes: tuple[tuple[float, float, float], ...]

    def __post_init__(self) -> None:
        """Require finite coordinates; no geometric accuracy is inferred."""
        try:
            origin = tuple(float(value) for value in self.origin_m)
            axes = tuple(tuple(float(value) for value in axis) for axis in self.axes)
            # Three spatial coordinates follow the source's body-axis convention.
            valid = (
                len(origin) == 3 and len(axes) == 3 and all(len(a) == 3 for a in axes)
            )
            valid = valid and all(
                math.isfinite(v) for v in (*origin, *(v for a in axes for v in a))
            )
        except (TypeError, ValueError, OverflowError) as error:
            raise ValueError(
                "frame requires finite three-dimensional coordinates"
            ) from error
        if not valid:
            raise ValueError("frame requires finite three-dimensional coordinates")
        object.__setattr__(self, "origin_m", origin)
        object.__setattr__(self, "axes", axes)


@dataclass(frozen=True)
class ModelContext:
    """Model-side observation, supplied independently of extraction claims."""

    model_digest: str
    body_id: str
    frame: BodyFrame


@dataclass(frozen=True)
class ExtractionClaim:
    """Untrusted extraction claim, with no certificate or acceptance authority."""

    context: ModelContext
    native_output_digest: str
    coefficient_digest: str
    extractor_revision: str


@dataclass(frozen=True)
class ZeroAssessment:
    """Diagnostic result; consistency never grants a comparison exemption."""

    zero_mode_observed: bool
    reasons: tuple[str, ...]
    coefficient_digest: str | None = None
    status: str = field(default="NOT_EVALUATED", init=False)
    correlation: None = field(default=None, init=False)

    def to_dict(self) -> dict[str, Any]:
        """Return JSON-compatible diagnostics with explicit unknown correlation."""
        result = asdict(self)
        result["reasons"] = list(self.reasons)
        return result


def _matrix_payload(matrix_set: Any) -> dict[str, Any]:
    """Snapshot the existing output_schemas matrix-set protocol."""
    frequencies = [float(value) for value in matrix_set.frequencies.values]
    if (
        not frequencies
        or matrix_set.frequencies.unit != "rad/s"
        or matrix_set.frequencies.count != len(frequencies)
        or len(matrix_set.matrices) != len(frequencies)
        or any(not math.isfinite(f) or f <= 0 for f in frequencies)
        or any(a >= b for a, b in zip(frequencies, frequencies[1:]))
    ):
        raise ValueError("invalid frequency axis")
    matrices = []
    for frequency, matrix in zip(frequencies, matrix_set.matrices):
        values = [[float(value) for value in row] for row in matrix.matrix]
        # Six DOFs follow output_schemas.DOF; this is a schema dimension.
        if len(values) != 6 or any(len(row) != 6 for row in values):
            raise ValueError("invalid matrix shape")
        if any(not math.isfinite(value) for row in values for value in row):
            raise ValueError("nonfinite coefficients")
        if float(matrix.frequency) != frequency:
            raise ValueError("matrix and collection frequencies differ")
        if matrix.matrix_type not in {"added_mass", "damping"}:
            raise ValueError("unknown coefficient kind")
        # Canonical units are defined in output_schemas.ADDED_MASS_UNITS /
        # DAMPING_UNITS; no unit conversion is attempted by this diagnostic.
        expected_units = (
            {"linear": "kg", "coupling": "kg.m", "angular": "kg.m^2"}
            if matrix.matrix_type == "added_mass"
            else {"linear": "N.s/m", "coupling": "N.s/rad", "angular": "N.m.s/rad"}
        )
        if dict(matrix.units) != expected_units:
            raise ValueError("canonical coefficient units are required")
        matrices.append(
            {
                "frequency": frequency,
                "matrix": values,
                "matrix_type": matrix.matrix_type,
                "units": dict(matrix.units),
                "source": matrix.source,
            }
        )
    if len({matrix["matrix_type"] for matrix in matrices}) != 1:
        raise ValueError("mixed coefficient kinds")
    return {"frequencies_rad_s": frequencies, "matrices": matrices}


def _digest(payload: dict[str, Any]) -> str:
    data = json.dumps(payload, sort_keys=True, separators=(",", ":"), allow_nan=False)
    return "sha256:" + hashlib.sha256(data.encode("utf-8")).hexdigest()


def coefficient_digest(matrix_set: Any) -> str:
    """Identify coefficients, frequency axis, units, kind and source together."""
    return _digest(_matrix_payload(matrix_set))


def _binding_reasons(
    model_context: ModelContext | None,
    extraction: ExtractionClaim | None,
    model_input: bytes | None,
    native_output: bytes | None,
    digest: str,
) -> list[str]:
    reasons = []
    if model_context is None:
        reasons.append("MODEL_CONTEXT_MISSING")
    else:
        reasons.append("MODEL_GEOMETRY_UNVERIFIED")
        if model_input is None:
            reasons.append("MODEL_INPUT_MISSING")
        elif (
            model_context.model_digest
            != "sha256:" + hashlib.sha256(model_input).hexdigest()
        ):
            reasons.append("MODEL_DIGEST_MISMATCH")
    if extraction is None:
        reasons.append("EXTRACTION_RECORD_MISSING")
        return reasons
    if not extraction.extractor_revision.strip():
        reasons.append("EXTRACTOR_REVISION_MISSING")
    if native_output is None:
        reasons.append("NATIVE_OUTPUT_MISSING")
    elif (
        extraction.native_output_digest
        != "sha256:" + hashlib.sha256(native_output).hexdigest()
    ):
        reasons.append("NATIVE_OUTPUT_BINDING_MISMATCH")
    if extraction.coefficient_digest != digest:
        reasons.append("COEFFICIENT_BINDING_MISMATCH")
    if model_context is not None:
        comparisons = (
            (model_context.model_digest, extraction.context.model_digest, "MODEL"),
            (model_context.body_id, extraction.context.body_id, "BODY"),
            (model_context.frame.origin_m, extraction.context.frame.origin_m, "ORIGIN"),
            (model_context.frame.axes, extraction.context.frame.axes, "AXES"),
        )
        reasons.extend(
            f"{name}_BINDING_MISMATCH" for a, b, name in comparisons if a != b
        )
    return reasons


def assess_physical_zero(
    matrix_set: Any,
    *,
    rotational_dof: int,
    model_context: ModelContext | None = None,
    extraction: ExtractionClaim | None = None,
    model_input: bytes | None = None,
    native_output: bytes | None = None,
) -> ZeroAssessment:
    """Observe a rotational zero without authorizing physical acceptance.

    DOFs follow output_schemas.DOF: roll=4, pitch=5, yaw=6. No tolerance is
    inferred from a zero pattern. Near-zero qualification needs a sourced,
    case-specific criterion and is outside this diagnostic contract.
    """
    if type(rotational_dof) is not int or rotational_dof not in {4, 5, 6}:
        raise ValueError("rotational_dof must be roll=4, pitch=5 or yaw=6")
    reasons = ["EXTRACTION_LINEAGE_UNESTABLISHED", "PHYSICAL_MODE_UNESTABLISHED"]
    try:
        payload = _matrix_payload(matrix_set)
        digest = _digest(payload)
    except (AttributeError, TypeError, ValueError, OverflowError):
        return ZeroAssessment(False, ("INVALID_COEFFICIENT_DATA", *reasons))
    reasons = (
        _binding_reasons(
            model_context,
            extraction,
            model_input,
            native_output,
            digest,
        )
        + reasons
    )
    matrices = payload["matrices"]
    if any(matrix["source"] != "solver" for matrix in matrices):
        reasons.insert(0, "UNTRUSTED_SOURCE")
    if all(
        value == 0 for matrix in matrices for row in matrix["matrix"] for value in row
    ):
        reasons.insert(0, "ALL_ZERO_COEFFICIENTS")
    index = rotational_dof - 1
    zero_mode = all(
        value == 0
        for matrix in matrices
        for value in (
            *matrix["matrix"][index],
            *(row[index] for row in matrix["matrix"]),
        )
    )
    if not zero_mode:
        reasons.insert(0, "ROTATIONAL_ZERO_NOT_OBSERVED")
    return ZeroAssessment(zero_mode, tuple(reasons), digest)
