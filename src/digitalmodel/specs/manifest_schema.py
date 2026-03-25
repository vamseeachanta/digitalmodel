"""Pydantic schema for per-module manifest.yaml validation.

Each digitalmodel calculation module has a manifest.yaml that maps its
functions to standard clauses and equations (D-05/D-06 traceability).

Exports:
    ModuleManifest  -- top-level model for a module's manifest.yaml
    FunctionEntry   -- one function's traceability mapping
    StandardRef     -- reference to the governing standard
    validate_manifest_file -- load + validate a YAML file, returning ModuleManifest
"""

from pathlib import Path

import yaml
from pydantic import BaseModel, Field


class StandardRef(BaseModel):
    """Reference to the primary engineering standard governing a module."""

    id: str  # e.g. "DNV-RP-F109"
    edition: int | str  # e.g. 2021 or "2021-09"
    title: str  # Full title of the standard


class FunctionEntry(BaseModel):
    """One function's traceability mapping to a standard clause/equation."""

    name: str  # Python function name
    clause: str  # e.g. "S3.2.1"
    equation: str = ""  # e.g. "Eq 3.1" (optional for lookup/table functions)
    description: str  # Human-readable description
    inputs: list[str] = []  # Parameter names
    outputs: list[str] = []  # Return value descriptions


class ModuleManifest(BaseModel):
    """Top-level schema for a module's manifest.yaml file.

    Requires at least one function entry to ensure every manifest
    provides meaningful traceability data.
    """

    module: str  # Module path e.g. "subsea/on_bottom_stability"
    description: str  # Module description
    primary_standard: StandardRef
    functions: list[FunctionEntry] = Field(..., min_length=1)


def validate_manifest_file(path: Path | str) -> ModuleManifest:
    """Load and validate a manifest.yaml file.

    Args:
        path: Path to the manifest.yaml file.

    Returns:
        ModuleManifest instance if valid.

    Raises:
        ValueError: If the manifest is invalid, with the file path in the message.
        FileNotFoundError: If the file does not exist.
    """
    path = Path(path)
    with open(path) as f:
        data = yaml.safe_load(f)
    try:
        return ModuleManifest(**data)
    except Exception as e:
        raise ValueError(f"Invalid manifest at {path}: {e}") from e
