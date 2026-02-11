"""Extract spec.yml-compatible dicts from monolithic OrcaFlex YAML files.

Monolithic OrcaFlex YAML files are exported by OrcFxAPI (.dat -> .yml) as
multi-document YAML with ``---`` separators. This module converts them into
a dict structure that can be loaded into ``ProjectInputSpec``.

Usage::

    from pathlib import Path
    from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
        MonolithicExtractor,
    )

    extractor = MonolithicExtractor(Path("model.yml"))
    spec_dict = extractor.extract()
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any

import yaml

from .schema.generic import (
    FIELD_TO_SECTION,
    SINGLETON_SECTIONS,
    TYPED_FIELD_MAP,
)

# Reverse map: OrcaFlex YAML section key -> spec field name
SECTION_TO_FIELD: dict[str, str] = {v: k for k, v in FIELD_TO_SECTION.items()}

# Reverse map: OrcaFlex YAML key -> typed field name
ORCAFLEX_TO_TYPED: dict[str, str] = {v: k for k, v in TYPED_FIELD_MAP.items()}

# Keys extracted into the simulation section (excluded from general_properties)
_SIMULATION_KEYS = {"StageDuration", "ImplicitConstantTimeStep"}

# OrcaFlex YAML uses "VariableData" with sub-categories, but the generic
# schema uses "VariableDataSources" as a flat list section. Map both forms
# so the extractor can handle either key.
_VARIABLE_DATA_YAML_KEY = "VariableData"
_VARIABLE_DATA_SCHEMA_KEY = "VariableData"

# OrcaFlex section name aliases: YAML export names may differ from API names.
# The key is the canonical SINGLETON_SECTIONS key; values are fallback names.
_SECTION_ALIASES: dict[str, list[str]] = {
    "FrictionCoefficients": ["SolidFrictionCoefficients"],
    "Groups": ["BrowserGroups"],
}


def _sanitize_name(raw: str) -> str:
    """Convert a filename stem to a snake_case identifier.

    Replaces spaces, hyphens, and runs of non-alphanumeric characters with
    underscores, then collapses consecutive underscores and strips edges.

    Args:
        raw: The raw string to sanitize (typically a filename stem).

    Returns:
        A cleaned snake_case string.
    """
    cleaned = re.sub(r"[^a-zA-Z0-9]+", "_", raw)
    cleaned = re.sub(r"_+", "_", cleaned)
    return cleaned.strip("_").lower()


def _topo_sort_groups(structure: dict[str, str]) -> dict[str, str]:
    """Topologically sort Groups.Structure so parents precede children.

    OrcFxAPI.SaveData() alphabetically sorts group entries, but OrcaFlex
    requires parent groups to be defined before any child references them.

    Args:
        structure: Dict mapping group name -> parent name.

    Returns:
        New dict with the same entries, ordered parent-first.
    """
    if not structure:
        return structure

    # Build child -> parent mapping and collect all unique names
    all_names = set(structure.keys())
    ordered: list[str] = []
    visited: set[str] = set()

    def _visit(name: str) -> None:
        if name in visited or name not in all_names:
            return
        visited.add(name)
        # Visit parent first (if it's also a group entry)
        parent = structure.get(name)
        if parent and parent in all_names:
            _visit(parent)
        ordered.append(name)

    for name in structure:
        _visit(name)

    return {name: structure[name] for name in ordered}


class MonolithicExtractor:
    """Extract a spec.yml-compatible dict from a monolithic OrcaFlex YAML.

    The extractor reads a multi-document OrcaFlex YAML file, merges all
    documents into a single dict, and maps the OrcaFlex-native keys into
    the ``ProjectInputSpec`` schema structure.

    Attributes:
        yaml_path: Path to the monolithic OrcaFlex YAML file.
    """

    def __init__(self, yaml_path: Path) -> None:
        self.yaml_path = Path(yaml_path)
        self._raw: dict[str, Any] = self._load_yaml()

    def extract(self) -> dict[str, Any]:
        """Extract a spec-compatible dict from the loaded YAML.

        Returns:
            Dict with ``metadata``, ``environment``, ``simulation``, and
            ``generic`` keys suitable for ``ProjectInputSpec(**result)``.
        """
        return {
            "metadata": self._extract_metadata(),
            "environment": self._extract_environment(),
            "simulation": self._extract_simulation(),
            "generic": self._extract_generic_model(),
        }

    # ------------------------------------------------------------------
    # YAML loading
    # ------------------------------------------------------------------

    def _load_yaml(self) -> dict[str, Any]:
        """Load a multi-document OrcaFlex YAML, merging all documents.

        OrcaFlex exports may use ``---`` separators to split sections
        across documents. The first document usually contains everything,
        but subsequent documents are merged in to handle split files.

        Returns:
            A single merged dict of all YAML documents.

        Raises:
            FileNotFoundError: If the YAML file does not exist.
            yaml.YAMLError: If the file cannot be parsed.
        """
        try:
            text = self.yaml_path.read_text(encoding="utf-8-sig")
        except UnicodeDecodeError:
            text = self.yaml_path.read_text(encoding="latin-1")
        merged: dict[str, Any] = {}
        for doc in yaml.safe_load_all(text):
            if isinstance(doc, dict):
                merged.update(doc)
        return merged

    # ------------------------------------------------------------------
    # Metadata
    # ------------------------------------------------------------------

    def _extract_metadata(self) -> dict[str, Any]:
        """Build metadata from the source filename.

        Returns:
            Dict with ``name``, ``description``, ``structure``,
            ``operation``, and ``project`` keys.
        """
        stem = self.yaml_path.stem
        return {
            "name": _sanitize_name(stem),
            "description": f"Extracted from {self.yaml_path.name}",
            "structure": "generic",
            "operation": "generic",
            "project": "model_library",
        }

    # ------------------------------------------------------------------
    # Environment
    # ------------------------------------------------------------------

    def _extract_environment(self) -> dict[str, Any]:
        """Map OrcaFlex Environment keys to the environment schema.

        Captures ALL raw Environment properties into ``raw_properties`` for
        round-trip fidelity.  The typed schema fields (water, seabed, waves,
        current, wind) are also populated from specific keys so the spec
        remains human-readable.

        Returns:
            Dict compatible with the ``Environment`` Pydantic model.
        """
        env = self._raw.get("Environment", {})

        result: dict[str, Any] = {
            "water": self._extract_water(env),
            "seabed": self._extract_seabed(env),
        }

        waves = self._extract_waves(env)
        if waves:
            result["waves"] = waves

        current = self._extract_current(env)
        if current:
            result["current"] = current

        wind = self._extract_wind(env)
        if wind:
            result["wind"] = wind

        # Capture ALL raw OrcaFlex Environment properties for pass-through.
        # The EnvironmentBuilder uses this as a base layer, overlaying
        # spec-derived values on top.
        if env:
            result["raw_properties"] = dict(env)

        return result

    def _extract_water(self, env: dict[str, Any]) -> dict[str, Any]:
        """Extract water properties from the Environment section.

        OrcaFlex uses ``WaterDepth`` (or ``SeabedOriginDepth`` in some
        versions) for the depth value.
        """
        depth = env.get("WaterDepth")
        if depth is None:
            depth = env.get("SeabedOriginDepth", 100.0)
        density = env.get("Density", 1.025)
        return {
            "depth": depth,
            "density": density,
        }

    def _extract_seabed(self, env: dict[str, Any]) -> dict[str, Any]:
        """Extract seabed properties from the Environment section."""
        normal = env.get("SeabedNormalStiffness", 100.0)
        shear = env.get("SeabedShearStiffness", 100.0)
        slope = env.get("SeabedSlope", 0.0)
        return {
            "slope": slope,
            "stiffness": {
                "normal": normal,
                "shear": shear,
            },
        }

    def _extract_waves(self, env: dict[str, Any]) -> dict[str, Any] | None:
        """Extract wave parameters from the first wave train, if present."""
        wave_trains = env.get("WaveTrains")
        if not wave_trains:
            return None

        wt = wave_trains[0]
        result: dict[str, Any] = {}

        wave_type = wt.get("WaveType")
        if wave_type is not None:
            result["type"] = wave_type

        height = wt.get("WaveHs") or wt.get("WaveHeight")
        if height is not None:
            result["height"] = height

        period = wt.get("WaveTz") or wt.get("WaveTp") or wt.get("WavePeriod")
        if period is not None:
            result["period"] = period

        direction = wt.get("WaveDirection")
        if direction is not None:
            result["direction"] = direction

        return result if result else None

    def _extract_current(self, env: dict[str, Any]) -> dict[str, Any] | None:
        """Extract current parameters from the Environment section.

        Also captures the depth-varying current profile from the
        multi-column ``CurrentDepth, CurrentFactor, CurrentRotation`` key.
        """
        speed = env.get("RefCurrentSpeed")
        direction = env.get("RefCurrentDirection")

        if speed is None and direction is None:
            return None

        result: dict[str, Any] = {}
        if speed is not None:
            result["speed"] = speed
        if direction is not None:
            result["direction"] = direction

        # Extract current profile from multi-column key
        profile_key = "CurrentDepth, CurrentFactor, CurrentRotation"
        raw_profile = env.get(profile_key)
        if raw_profile and isinstance(raw_profile, list):
            # Convert [[depth, factor, rotation], ...] -> [[depth, factor], ...]
            profile = []
            for row in raw_profile:
                if isinstance(row, list) and len(row) >= 2:
                    profile.append([float(row[0]), float(row[1])])
            if profile:
                result["profile"] = profile

        return result

    def _extract_wind(self, env: dict[str, Any]) -> dict[str, Any] | None:
        """Extract wind parameters from the Environment section."""
        speed = env.get("WindSpeed")
        direction = env.get("WindDirection")

        if speed is None and direction is None:
            return None

        result: dict[str, Any] = {}
        if speed is not None:
            result["speed"] = speed
        if direction is not None:
            result["direction"] = direction
        return result

    # ------------------------------------------------------------------
    # Simulation
    # ------------------------------------------------------------------

    def _extract_simulation(self) -> dict[str, Any]:
        """Extract simulation control parameters from the General section.

        Returns:
            Dict with ``stages`` and optionally ``time_step`` keys.
        """
        general = self._raw.get("General", {})
        result: dict[str, Any] = {}

        stages = general.get("StageDuration")
        if stages is not None:
            result["stages"] = stages

        time_step = general.get("ImplicitConstantTimeStep")
        if time_step is not None:
            result["time_step"] = time_step

        return result

    # ------------------------------------------------------------------
    # Generic model
    # ------------------------------------------------------------------

    def _extract_generic_model(self) -> dict[str, Any]:
        """Extract all OrcaFlex object sections into a GenericModel dict.

        Processes:
        - List sections (LineTypes, Vessels, Lines, etc.)
        - Singleton sections (SolidFrictionCoefficients, etc.)
        - VariableData (special nested structure)
        - General properties (everything in General except simulation keys)

        Returns:
            Dict compatible with the ``GenericModel`` Pydantic model.
        """
        result: dict[str, Any] = {}

        # List-based sections
        for section_key, field_name in SECTION_TO_FIELD.items():
            # VariableDataSources handled separately below
            if section_key == _VARIABLE_DATA_SCHEMA_KEY:
                continue
            items = self._extract_list_section(section_key)
            if items:
                result[field_name] = items

        # VariableData: OrcaFlex exports as nested categories, flatten to list
        var_data_items = self._extract_variable_data()
        if var_data_items:
            result["variable_data_sources"] = var_data_items

        # Singleton sections
        for section_key, field_name in SINGLETON_SECTIONS.items():
            singleton = self._extract_singleton(section_key)
            if singleton is not None:
                result[field_name] = singleton

        # General properties (excluding simulation keys)
        general_props = self._extract_general_properties()
        if general_props:
            result["general_properties"] = general_props

        return result

    def _extract_list_section(self, section_key: str) -> list[dict[str, Any]]:
        """Extract a list-based OrcaFlex section into spec-compatible dicts.

        Args:
            section_key: The OrcaFlex YAML section name (e.g. "LineTypes").

        Returns:
            List of dicts, each with typed fields and a ``properties`` bag.
            Empty list if the section is absent or empty.
        """
        items = self._raw.get(section_key, [])
        if not items:
            return []
        if not isinstance(items, list):
            return []

        return [self._extract_object(item) for item in items if isinstance(item, dict)]

    def _extract_object(self, raw: dict[str, Any]) -> dict[str, Any]:
        """Convert a raw OrcaFlex object dict into a spec-compatible dict.

        Splits keys into typed fields (via ``ORCAFLEX_TO_TYPED``) and
        remaining properties that go into the ``properties`` bag.

        Args:
            raw: A single OrcaFlex object dict from the YAML.

        Returns:
            Dict with typed field keys and a ``properties`` dict.
        """
        typed: dict[str, Any] = {}
        properties: dict[str, Any] = {}

        for key, value in raw.items():
            typed_field = ORCAFLEX_TO_TYPED.get(key)
            if typed_field is not None:
                typed[typed_field] = value
            else:
                properties[key] = value

        typed["properties"] = properties
        return typed

    def _extract_variable_data(self) -> list[dict[str, Any]]:
        """Extract the VariableData section into a flat list of objects.

        OrcaFlex exports VariableData as a nested dict where each key is
        a data category (e.g. "Dragcoefficient", "Coatingsorlinings") and
        each value is a list of named entries. This method flattens all
        categories into a single list of spec-compatible objects.

        Returns:
            List of dicts with ``name``, ``data_type``, ``entries``, and
            ``properties`` keys. Empty list if no VariableData present.
        """
        var_data = self._raw.get(_VARIABLE_DATA_YAML_KEY, {})
        if not var_data or not isinstance(var_data, dict):
            return []

        result: list[dict[str, Any]] = []
        for category_key, entries in var_data.items():
            if not isinstance(entries, list):
                continue
            for entry in entries:
                if not isinstance(entry, dict):
                    continue

                name = entry.get("Name", "")
                properties = {
                    k: v for k, v in entry.items() if k != "Name"
                }
                result.append({
                    "name": name,
                    "data_type": category_key,
                    "properties": properties,
                })

        return result

    def _extract_singleton(self, section_key: str) -> dict[str, Any] | None:
        """Extract a singleton OrcaFlex section.

        Singleton sections (FrictionCoefficients, LineContactData,
        Groups, etc.) are single dicts rather than lists. Falls back to
        alias names if the primary key is not found.

        Args:
            section_key: The OrcaFlex YAML section name.

        Returns:
            Dict with a ``data`` key containing the section contents,
            or None if the section is absent or empty.
        """
        data = self._raw.get(section_key)
        if not data or not isinstance(data, (dict, list)):
            # Try alias names (e.g. SolidFrictionCoefficients for
            # FrictionCoefficients, BrowserGroups for Groups)
            for alias in _SECTION_ALIASES.get(section_key, []):
                data = self._raw.get(alias)
                if data and isinstance(data, (dict, list)):
                    break
            else:
                return None

        # Groups.Structure: reorder so parents are defined before children.
        # OrcFxAPI.SaveData() alphabetically sorts group entries, but OrcaFlex
        # requires parent groups to exist before they are referenced.
        if section_key == "Groups" and "Structure" in data:
            data = dict(data)
            data["Structure"] = _topo_sort_groups(data["Structure"])

        return {"data": data}

    def _extract_general_properties(self) -> dict[str, Any]:
        """Extract General section properties, excluding simulation keys.

        Keys that belong to the simulation section (``StageDuration``,
        ``ImplicitConstantTimeStep``) are excluded since they are handled
        by ``_extract_simulation()``.

        Returns:
            Dict of remaining General properties, empty if none.
        """
        general = self._raw.get("General", {})
        if not general:
            return {}
        return {
            k: v for k, v in general.items()
            if k not in _SIMULATION_KEYS
        }

    # ------------------------------------------------------------------
    # Public helpers
    # ------------------------------------------------------------------

    @property
    def raw(self) -> dict[str, Any]:
        """Access the raw merged YAML data (read-only)."""
        return dict(self._raw)

    def get_section_keys(self) -> list[str]:
        """List all top-level section keys found in the YAML.

        Returns:
            Sorted list of top-level keys present in the loaded YAML.
        """
        return sorted(self._raw.keys())

    def get_object_names(self, section_key: str) -> list[str]:
        """Get the names of all objects in a given section.

        Args:
            section_key: OrcaFlex YAML section name (e.g. "LineTypes").

        Returns:
            List of ``Name`` values, empty if section is missing or has
            no named objects.
        """
        items = self._raw.get(section_key, [])
        if not isinstance(items, list):
            return []
        return [
            item["Name"]
            for item in items
            if isinstance(item, dict) and "Name" in item
        ]
