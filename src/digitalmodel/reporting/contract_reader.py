# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Opt-in, exact-source JSON reads for report sections (#1019).

Authoring example (``root`` is the trusted analysis store directory)::

    data = read_declared_json(root, section, source, provenance,
                              assumptions=["force.units"], ledger=ledger)
    html = assemble_report("Results", provenance=provenance,
                           sections=[section], data=data, ledger=ledger)

Import this helper from ``digitalmodel.reporting.contract_reader``. Declare the
source's full metadata in ``provenance.sources`` and its exact portable relative
identifier in ``section.reads``. Kinds and wildcard patterns do not grant access.
The source digest must be ``sha256:`` followed by 64 lowercase hexadecimal digits.
Supply nonblank units and list the exact ledger field names this read relies on.
For example, record the assumed force units under ``force.units`` before the
read and pass the same ledger to the assembler to render its value and basis.
Use ``assumptions=[]`` when the read relies on no assumed fields.

Only UTF-8 JSON up to 1 MiB is accepted. Duplicate object keys, non-finite
numbers and malformed JSON fail closed. A bounded binary read supplies both the
digest and decoder, so verified bytes are the bytes used as results.

Threat model: trusted local files, without hostile concurrent path or symlink
mutation. Resolved paths must stay under the supplied root; symlinks within that
root are permitted. This is no Python sandbox: arbitrary builder I/O and literal
results are not intercepted. Call this helper before invoking builders.
"""

from __future__ import annotations

import hashlib
import json
import math
import re
from collections.abc import Sequence
from pathlib import Path, PureWindowsPath
from typing import Any

from digitalmodel.common.assumption_ledger import AssumptionLedger
from digitalmodel.reporting._backbone import ReportSection
from digitalmodel.reporting.provenance import ContractError, DataSource, Provenance

MAX_JSON_BYTES = 1024 * 1024
_DIGEST = re.compile(r"sha256:[0-9a-f]{64}")


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """Reject duplicate keys, including keys in nested objects."""
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ContractError("JSON contains a duplicate object key.")
        result[key] = value
    return result


def _reject_constant(value: str) -> Any:
    """Reject the nonstandard JSON NaN and infinity tokens."""
    raise ContractError("JSON contains a non-finite constant.")


def _finite_float(value: str) -> float:
    """Reject finite JSON spellings that overflow a Python float."""
    number = float(value)
    if not math.isfinite(number):
        raise ContractError("JSON number exceeds the finite float range.")
    return number


def read_declared_json(
    root: Path | str,
    section: ReportSection,
    source: DataSource,
    provenance: Provenance,
    *,
    assumptions: Sequence[str],
    ledger: AssumptionLedger | None = None,
) -> Any:
    """Return verified JSON data, raising a provenance or contract error.

    All authorization and path checks precede opening the source. Read errors,
    invalid digests and invalid JSON raise ``ContractError``. The source is
    opened read-only, once, with at most ``MAX_JSON_BYTES + 1`` bytes requested.
    """
    provenance.require()
    if not provenance.units_system or not provenance.units_system.strip():
        raise ContractError("Strict JSON reads require nonblank units_system.")
    if (
        not isinstance(assumptions, Sequence)
        or isinstance(assumptions, (str, bytes, bytearray))
        or any(not isinstance(item, str) or not item.strip() for item in assumptions)
    ):
        raise ContractError(
            "Explicit assumptions must be a sequence of nonblank strings."
        )
    if assumptions and (
        ledger is None
        or not set(assumptions).issubset(record.field for record in ledger)
    ):
        raise ContractError("Every named assumption must match a field in the ledger.")
    if source not in provenance.sources:
        raise ContractError("Source must be an exact member of provenance.sources.")
    identifier = source.identifier
    if identifier not in (section.reads or []):
        raise ContractError(
            f"Section {section.key!r} requires an exactly declared source identifier."
        )
    if (
        not identifier
        or PureWindowsPath(identifier).drive
        or any(char in identifier for char in '\\<>:"|?*')
        or any(ord(char) < 32 for char in identifier)
        or any(part in ("", ".", "..") for part in identifier.split("/"))
    ):
        raise ContractError("Source identifier must be a portable relative path.")
    if not source.digest or not _DIGEST.fullmatch(source.digest):
        raise ContractError(
            "Source digest must be sha256: plus 64 lowercase hex digits."
        )
    try:
        resolved_root = Path(root).resolve(strict=True)
        path = (resolved_root / identifier).resolve(strict=True)
    except (OSError, RuntimeError) as error:
        raise ContractError("Cannot resolve declared JSON source or root.") from error
    if not path.is_relative_to(resolved_root):
        raise ContractError("Source symlink would escape the declared root.")
    try:
        with path.open("rb") as stream:
            raw = stream.read(MAX_JSON_BYTES + 1)
    except OSError as error:
        raise ContractError("Cannot read declared JSON source.") from error
    if len(raw) > MAX_JSON_BYTES:
        raise ContractError("Declared JSON source exceeds the 1 MiB limit.")
    if "sha256:" + hashlib.sha256(raw).hexdigest() != source.digest:
        raise ContractError("Declared source digest does not match its bytes.")
    try:
        return json.loads(
            raw.decode("utf-8"),
            object_pairs_hook=_unique_object,
            parse_constant=_reject_constant,
            parse_float=_finite_float,
        )
    except (UnicodeError, ValueError, RecursionError) as error:
        raise ContractError("Declared source is not supported UTF-8 JSON.") from error
