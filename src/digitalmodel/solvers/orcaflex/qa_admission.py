"""Solver-free evidence admission for the native parallel batch route.

Contract source: docs/agent-pack.md, section 5, queue refusals B1–B9 and
checks C15–C23, citing AQG r0 section 12 (2026). This implements record
admission, not the engineering checks which produce that record.

The caller supplies the current criteria revision, independently recomputed
closed-input fingerprint, required image roles and a trusted signature verifier.
These authorities must come from outside the submitted evidence. No signature
algorithm, revision currency or engineering tolerance is guessed here.
"""

from __future__ import annotations

import hashlib
import json
import re
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

# AQG r0 sections 4–9, as mapped in docs/agent-pack.md section 5, C01–C14.
REQUIRED_GATES = ("G0", "G1", "G2", "G3", "G4", "G5")


class AdmissionRefused(ValueError):
    """One deterministic B1–B9 refusal for a submitted case."""

    def __init__(self, condition: str, message: str):
        self.condition = condition
        super().__init__(f"{condition}: {message}")


@dataclass(frozen=True)
class AdmissionRequest:
    """Evidence and separately resolved identities for one candidate case.

    ``input_sha256`` must be recomputed from the complete input closure using
    the runner's fingerprint convention. External dependencies must first be
    materialised below the model directory; unresolved closure is not eligible.
    ``images`` maps required roles to the actual files being admitted.
    """

    record: Mapping[str, Any]
    input_sha256: str
    configuration: str
    sample: str
    images: Mapping[str, Path]


@dataclass(frozen=True)
class AdmissionBinding:
    """Immutable input and approved evidence identities from one validation."""

    input_sha256: str
    evidence_sha256: str


@dataclass(frozen=True)
class AdmissionContext:
    """Trusted admission policy, supplied independently of candidate records.

    ``verify_signature(payload, signature)`` must authenticate the exact UTF-8
    canonical JSON payload and the reviewer against an approved trust store.
    The payload contains every record field except ``signature``. Returning
    anything other than ``True``, or raising, refuses admission. A boolean
    stored inside the submitted record is not an authentication mechanism.
    ``required_images`` is the nonempty, analysis-specific image-role register.
    """

    criteria_revision: str
    required_images: tuple[str, ...]
    verify_signature: Callable[[bytes, Mapping[str, Any]], bool]
    requests: Mapping[str, AdmissionRequest]

    def validate(self, case: dict, expected_version: str) -> AdmissionBinding:
        """Refuse incomplete evidence, or bind both identities from this request."""
        request = self.requests.get(case["case_id"])
        if not isinstance(request, AdmissionRequest):
            raise AdmissionRefused("B1", "missing model evidence record")
        record = request.record
        if not isinstance(record, Mapping) or record.get("case_id") != case["case_id"]:
            raise AdmissionRefused("B1", "missing or mismatched model evidence record")

        if not _digest(request.input_sha256) or record.get("input_sha256") != request.input_sha256:
            raise AdmissionRefused("B2", "input fingerprint differs at admission")

        gates = record.get("gates")
        if not isinstance(gates, Mapping):
            raise AdmissionRefused("B3", "required gate records are missing")
        for name in REQUIRED_GATES:
            gate = gates.get(name)
            if not isinstance(gate, Mapping) or not (
                gate.get("status") == "PASS" or (
                    gate.get("status") == "NOT_APPLICABLE" and _text(gate.get("reason"))
                )
            ):
                raise AdmissionRefused("B3", f"{name} is unqualified or unexplained")

        if (not _text(expected_version) or record.get("program") != "OrcaFlex"
                or record.get("program_version") != expected_version):
            raise AdmissionRefused("B4", "evidence program version differs from request")

        images = record.get("images")
        if not self.required_images or not isinstance(images, Mapping):
            raise AdmissionRefused("B5", "required image register is missing")
        for role in self.required_images:
            if not _text(role) or not _digest(images.get(role)):
                raise AdmissionRefused("B5", "required image digest is missing")
            try:
                actual = hashlib.sha256(Path(request.images[role]).read_bytes()).hexdigest()
            except (KeyError, OSError, TypeError, ValueError):
                raise AdmissionRefused("B5", "required image is unavailable") from None
            if actual != images[role]:
                raise AdmissionRefused("B5", "required image differs from evidence")

        # Check content before signature verification so each isolated B condition
        # has its own diagnostic, even when a malformed record also breaks approval.
        signature = record.get("signature")
        if (not isinstance(signature, Mapping) or not _text(record.get("producer"))
                or not _text(signature.get("reviewer"))
                or signature["reviewer"] == record["producer"]):
            raise AdmissionRefused("B6", "missing or non-independent signature")

        deviations = record.get("deviations")
        if not isinstance(deviations, list) or any(
            not isinstance(item, Mapping) or not _text(item.get("disposition"))
            for item in deviations
        ):
            raise AdmissionRefused("B7", "deviation register is missing or lacks disposition")

        if (not _text(self.criteria_revision)
                or record.get("criteria_revision") != self.criteria_revision):
            raise AdmissionRefused("B8", "evidence criteria revision is not current")

        representatives = record.get("representatives")
        identity = {
            "configuration": request.configuration, "sample": request.sample,
            "program_version": expected_version, "criteria_revision": self.criteria_revision,
        }
        if (not all(_text(value) for value in identity.values())
                or any(record.get(key) != identity[key] for key in ("configuration", "sample"))
                or not isinstance(representatives, list)
                or not any(
                    isinstance(item, Mapping) and item.get("status") == "PASS"
                    and all(item.get(key) == value for key, value in identity.items())
                    and _text(item.get("evidence_ref")) and _digest(item.get("evidence_sha256"))
                    for item in representatives
                )):
            raise AdmissionRefused("B9", "no passing representative for configuration/sample")

        try:
            payload = record_payload(record)
            approved = self.verify_signature(payload, signature) is True
        except Exception:
            # An unavailable trust service must not expose its error details or
            # admit a case without authentication.
            approved = False
        if not approved:
            raise AdmissionRefused("B6", "exact evidence content has no approved signature")
        return AdmissionBinding(
            input_sha256=request.input_sha256,
            evidence_sha256=hashlib.sha256(payload).hexdigest(),
        )


def record_payload(record: Mapping[str, Any]) -> bytes:
    """Canonical bytes authenticated by the caller's signature verifier."""
    return json.dumps(
        {key: value for key, value in record.items() if key != "signature"},
        sort_keys=True, separators=(",", ":"), allow_nan=False,
    ).encode("utf-8")


def _text(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip())


def _digest(value: Any) -> bool:
    # SHA-256 representation follows the existing runner.sha256_file convention;
    # derive the hexadecimal width from hashlib rather than an engineering limit.
    width = hashlib.sha256().digest_size * len("ff")
    return isinstance(value, str) and re.fullmatch(r"[0-9a-f]{%d}" % width, value) is not None
