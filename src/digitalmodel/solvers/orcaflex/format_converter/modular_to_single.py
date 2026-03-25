"""Convert modular OrcaFlex format to single monolithic YAML.

Extracted and enhanced from the merge logic in:
- modular_input_validation/level_3_physical.py (lines 130-201)
- modular_input_validation/utils.py (extract_includefiles, resolve_include_path)
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

from ..yaml_utils import orcaflex_dump, orcaflex_load
from .protocols import ConversionReport


class ModularToSingleConverter:
    """Convert modular OrcaFlex format (master.yml + includes/) to single file.

    Loads master.yml, resolves all includefile directives, deep-merges
    include contents, and writes a single flat YAML file.
    """

    def __init__(self, master_path: Path | None = None):
        self.master_path = Path(master_path) if master_path else None

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Execute the conversion.

        Args:
            source: Path to master.yml or modular directory.
            target: Path for output single YAML file.

        Returns:
            ConversionReport with conversion results.
        """
        src = Path(source) if source else self.master_path
        if src is None:
            raise ValueError("No source path provided")

        # Resolve master.yml path
        if src.is_dir():
            master = src / "master.yml"
        else:
            master = src

        if not master.exists():
            return ConversionReport(
                success=False,
                source_format="modular",
                target_format="single",
                source_path=master,
                target_path=Path(target) if target else Path(),
                warnings=[f"Master file not found: {master}"],
            )

        tgt = (
            Path(target) if target else master.parent / f"{master.parent.name}.yml"
        )

        warnings: list[str] = []

        # Load master.yml (can be a list of includefile dicts or a dict)
        master_data, header_lines = orcaflex_load(master)

        # Extract includefile paths
        include_paths = self._extract_includefiles(master_data)

        if not include_paths:
            warnings.append("No includefile directives found in master.yml")

        # Load and merge all includes
        merged: dict[str, Any] = {}
        for inc_path_str in include_paths:
            inc_path = self._resolve_include_path(master, inc_path_str)
            if not inc_path.exists():
                warnings.append(f"Include file not found: {inc_path_str}")
                continue

            with open(inc_path) as f:
                inc_data = yaml.safe_load(f)

            if inc_data and isinstance(inc_data, dict):
                merged = self._deep_merge(merged, inc_data)

        # Write output with preserved header
        header = "\n".join(header_lines) if header_lines else None
        orcaflex_dump(merged, tgt, header=header)

        return ConversionReport(
            success=True,
            source_format="modular",
            target_format="single",
            source_path=master,
            target_path=tgt,
            warnings=warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("modular", "single")

    def merge_to_dict(self, master_path: Path) -> dict[str, Any]:
        """Merge all includes into a single dict (useful for chaining).

        Args:
            master_path: Path to master.yml.

        Returns:
            Merged dictionary of all include contents.
        """
        master_data, _ = orcaflex_load(master_path)
        include_paths = self._extract_includefiles(master_data)

        merged: dict[str, Any] = {}
        for inc_path_str in include_paths:
            inc_path = self._resolve_include_path(master_path, inc_path_str)
            if not inc_path.exists():
                continue

            with open(inc_path) as f:
                inc_data = yaml.safe_load(f)

            if inc_data and isinstance(inc_data, dict):
                merged = self._deep_merge(merged, inc_data)

        return merged

    @staticmethod
    def _extract_includefiles(data: Any) -> list[str]:
        """Recursively extract all includefile references.

        Handles both list format (master.yml) and nested dict format.
        """
        includefiles: list[str] = []

        def traverse(obj: Any) -> None:
            if isinstance(obj, dict):
                for key, value in obj.items():
                    if key == "includefile" and isinstance(value, str):
                        includefiles.append(value)
                    else:
                        traverse(value)
            elif isinstance(obj, list):
                for item in obj:
                    traverse(item)

        if data:
            traverse(data)
        return includefiles

    @staticmethod
    def _resolve_include_path(master_file: Path, include_file: str) -> Path:
        """Resolve include path relative to master file directory."""
        inc = Path(include_file)
        if inc.is_absolute():
            return inc
        return (master_file.parent / inc).resolve()

    @staticmethod
    def _deep_merge(base: dict, override: dict) -> dict:
        """Deep merge two dictionaries.

        Rules:
        - dict + dict -> recursive merge
        - list + list -> concatenate
        - otherwise -> override wins
        """
        result = base.copy()

        for key, value in override.items():
            if key in result:
                if isinstance(result[key], dict) and isinstance(value, dict):
                    result[key] = ModularToSingleConverter._deep_merge(
                        result[key], value
                    )
                elif isinstance(result[key], list) and isinstance(value, list):
                    result[key] = result[key] + value
                else:
                    result[key] = value
            else:
                result[key] = value

        return result
