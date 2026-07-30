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

        # Merge master content and all includes
        merged = self._merge_master(master, master_data, warnings)

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
        return self._merge_master(master_path, master_data, [])

    def _merge_master(
        self, master: Path, master_data: Any, warnings: list[str]
    ) -> dict[str, Any]:
        """Merge master content and all includefile contents into one dict.

        Handles both master formats:
        - list format (canonical master.yml): ``- includefile: path`` items;
          each include's (section-wrapped) content is deep-merged at the
          top level.
        - dict format: the master's own sections are preserved.  An
          ``includefile`` directive inside a section pulls the include's
          content into THAT section (OrcaFlex interprets section-scoped
          includes in the section's context), while a top-level
          ``includefile`` key merges at the top level.
        """
        merged: dict[str, Any] = {}

        if isinstance(master_data, list):
            for item in master_data:
                if not isinstance(item, dict):
                    continue
                if isinstance(item.get("includefile"), str):
                    inc_data = self._load_include(
                        master, item["includefile"], warnings
                    )
                    if inc_data is not None:
                        merged = self._deep_merge(merged, inc_data)
                else:
                    # Hybrid list item carrying inline sections
                    merged = self._deep_merge(merged, item)
        elif isinstance(master_data, dict):
            for key, value in master_data.items():
                if key == "includefile" and isinstance(value, str):
                    inc_data = self._load_include(master, value, warnings)
                    if inc_data is not None:
                        merged = self._deep_merge(merged, inc_data)
                elif isinstance(value, dict) and isinstance(
                    value.get("includefile"), str
                ):
                    # Section-scoped include: content belongs INSIDE this
                    # section, not at the top level.
                    section = {
                        k: v for k, v in value.items() if k != "includefile"
                    }
                    inc_data = self._load_include(
                        master, value["includefile"], warnings
                    )
                    if inc_data is not None:
                        # Tolerate already-wrapped fragments ({Section: {...}})
                        if set(inc_data.keys()) == {key} and isinstance(
                            inc_data[key], dict
                        ):
                            inc_data = inc_data[key]
                        section = self._deep_merge(section, inc_data)
                    merged = self._deep_merge(merged, {key: section})
                else:
                    merged = self._deep_merge(merged, {key: value})

        return merged

    def _load_include(
        self, master: Path, inc_path_str: str, warnings: list[str]
    ) -> dict[str, Any] | None:
        """Load an include file, recording a warning if it is missing."""
        inc_path = self._resolve_include_path(master, inc_path_str)
        if not inc_path.exists():
            warnings.append(f"Include file not found: {inc_path_str}")
            return None

        with open(inc_path) as f:
            inc_data = yaml.safe_load(f)

        if inc_data and isinstance(inc_data, dict):
            return inc_data
        return None

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
