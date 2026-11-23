"""Auto-detect OrcaFlex file format (spec, modular, single)."""

from __future__ import annotations

from enum import Enum
from pathlib import Path

import yaml


class FormatType(str, Enum):
    """OrcaFlex file format types."""

    SPEC = "spec"
    MODULAR = "modular"
    SINGLE = "single"


# Top-level keys that indicate a single OrcaFlex model file
_ORCAFLEX_SECTION_KEYS = {
    'General', 'Environment', 'VesselTypes', 'LineTypes',
    'Vessels', 'Lines', '6DBuoys', 'Buoys', 'BuoyTypes',
    'Shapes', 'Constraints', 'Links', 'Winches', 'Supports',
    'SupportTypes', 'MorisonElementTypes', '3DBuoys', 'Groups',
    'VariableData', 'TurbineTypes', 'Turbines', 'DragChainTypes',
    'DragChains', 'FlexJoints', 'SolidFrictionCoefficients',
}


def detect_format(path: Path) -> FormatType:
    """Detect the OrcaFlex format of a file or directory.

    Detection rules:
    - Directory with master.yml + includes/ -> modular
    - File with 'metadata' + 'pipeline' top-level keys -> spec
    - File with OrcaFlex section keys (General, Environment, etc.) -> single
    - List with 'includefile' entries -> modular (master.yml itself)

    Args:
        path: Path to file or directory.

    Returns:
        Detected FormatType.

    Raises:
        ValueError: If format cannot be determined.
    """
    path = Path(path)

    # Directory detection
    if path.is_dir():
        master = path / 'master.yml'
        includes = path / 'includes'
        if master.exists() and includes.is_dir():
            return FormatType.MODULAR
        raise ValueError(
            f"Directory '{path}' does not contain master.yml + includes/"
        )

    # File detection
    if not path.is_file():
        raise ValueError(f"Path '{path}' does not exist")

    with open(path) as f:
        data = yaml.safe_load(f)

    if data is None:
        raise ValueError(f"File '{path}' is empty or invalid YAML")

    # Modular master.yml: list of dicts with 'includefile' keys
    if isinstance(data, list):
        if any(isinstance(item, dict) and 'includefile' in item for item in data):
            return FormatType.MODULAR
        raise ValueError(f"File '{path}' is a YAML list but not a modular master")

    if not isinstance(data, dict):
        raise ValueError(f"File '{path}' has unexpected YAML structure: {type(data)}")

    # Spec: has 'metadata' + 'pipeline' (or 'metadata' + 'environment')
    top_keys = set(data.keys())
    if 'metadata' in top_keys and ('pipeline' in top_keys or 'environment' in top_keys):
        return FormatType.SPEC

    # Single: has OrcaFlex section keys
    if top_keys & _ORCAFLEX_SECTION_KEYS:
        return FormatType.SINGLE

    raise ValueError(
        f"Cannot determine format of '{path}'. "
        f"Top-level keys: {sorted(top_keys)}"
    )
