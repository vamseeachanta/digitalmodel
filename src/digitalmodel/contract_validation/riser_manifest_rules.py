# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Fail-closed manifest declaration and path rules for #1602."""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any

from .errors import ContractValidationError

EXPECTED_COMPONENTS = [
    ("host_motion_component", "issue-1602-riser-host-motion-contract-v3.yaml"),
    ("assurance_component", "issue-1602-riser-assurance-contract-v3.yaml"),
]
EXPECTED_LOAD_ORDER = [
    "solver_neutral_base_v2",
    "host_motion_component",
    "assurance_component",
]


def validate_manifest_declarations(root: dict[str, Any]) -> None:
    """Require the exact approved base, component set, paths, and load order."""

    composition = root.get("composition", {})
    base = composition.get("base", {})
    if (
        base.get("component_id") != "solver_neutral_base_v2"
        or base.get("path") != "issue-1602-riser-analysis-contract-v1.yaml"
    ):
        raise ContractValidationError("base component declaration is not exact")
    components = composition.get("components", [])
    observed = [(item.get("component_id"), item.get("path")) for item in components]
    if observed != EXPECTED_COMPONENTS:
        raise ContractValidationError("component declaration set is not exact")
    if composition.get("load_order") != EXPECTED_LOAD_ORDER:
        raise ContractValidationError("component load order is not exact")


def resolve_bound_path(base_dir: Path, relative: object) -> Path:
    """Resolve a declared path and reject lexical or symlink escapes."""

    if not isinstance(relative, str) or Path(relative).is_absolute():
        raise ContractValidationError("declared path must be relative")
    candidate = Path(os.path.abspath(base_dir / relative))
    allowed = Path(os.path.abspath(base_dir.parent))
    if os.path.commonpath((candidate, allowed)) != str(allowed):
        raise ContractValidationError(
            f"declared path escapes contract tree: {relative}"
        )
    try:
        resolved = candidate.resolve(strict=True)
    except OSError as exc:
        raise ContractValidationError(
            f"cannot resolve declared path: {relative}"
        ) from exc
    if os.path.commonpath((resolved, allowed)) != str(allowed):
        raise ContractValidationError(
            f"declared path escapes contract tree: {relative}"
        )
    cursor = candidate
    while cursor != allowed:
        if cursor.is_symlink():
            raise ContractValidationError(
                f"input must be regular non-symlink: {candidate.name}"
            )
        cursor = cursor.parent
    return resolved
