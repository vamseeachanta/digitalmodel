#!/usr/bin/env python
"""Semantic validation of monolithic vs modular OrcaFlex YAML files.

Compares a monolithic OrcaFlex YAML (ground truth exported from .dat) against
generated modular YAML files section-by-section, identifying exactly which
properties differ per section category.

This saves time vs. running full OrcaFlex statics by catching spec.yml
discrepancies at the YAML level before any solver invocation.

Usage:
    # Single model comparison (console)
    uv run python scripts/semantic_validate.py \\
        "docs/domains/orcaflex/examples/raw/A01/A01 Catenary riser.yml" \\
        "docs/domains/orcaflex/library/tier2_fast/a01_catenary_riser/modular"

    # With HTML report
    uv run python scripts/semantic_validate.py \\
        "docs/domains/orcaflex/examples/raw/A01/A01 Catenary riser.yml" \\
        "docs/domains/orcaflex/library/tier2_fast/a01_catenary_riser/modular" \\
        --html validation_report.html

    # JSON output
    uv run python scripts/semantic_validate.py \\
        "docs/domains/orcaflex/examples/raw/A01/A01 Catenary riser.yml" \\
        "docs/domains/orcaflex/library/tier2_fast/a01_catenary_riser/modular" \\
        --json

    # Batch mode (all models in a directory)
    uv run python scripts/semantic_validate.py \\
        --batch docs/domains/orcaflex/library/tier2_fast \\
        --batch-report validation_batch_report.html
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import yaml


# ---------------------------------------------------------------------------
# Section classification
# ---------------------------------------------------------------------------

FLAT_SECTIONS = {"General", "Environment"}

LIST_SECTIONS = {
    "LineTypes",
    "VesselTypes",
    "ClumpTypes",
    "WingTypes",
    "FlexJointTypes",
    "DragChainTypes",
    "StiffenerTypes",
    "SupportTypes",
    "MorisonElementTypes",
    "Vessels",
    "Lines",
    "Shapes",
    "6DBuoys",
    "3DBuoys",
    "Constraints",
    "Links",
    "Winches",
    "FlexJoints",
    "DragChains",
    "Turbines",
    "AttachedBuoys",
    "PyModels",
    "WakeModels",
    "MultibodyGroups",
    "ExpansionTables",
}

SINGLETON_SECTIONS = {
    "SolidFrictionCoefficients",
    "LineContactData",
    "CodeChecks",
    "Shear7Data",
    "VIVAData",
    "RayleighDampingCoefficients",
}

NESTED_SECTIONS = {"VariableData"}

ALL_KNOWN_SECTIONS = FLAT_SECTIONS | LIST_SECTIONS | SINGLETON_SECTIONS | NESTED_SECTIONS


# ---------------------------------------------------------------------------
# Significance classification
# ---------------------------------------------------------------------------

class Significance:
    MATCH = "match"
    COSMETIC = "cosmetic"
    MINOR = "minor"
    SIGNIFICANT = "significant"
    TYPE_MISMATCH = "type_mismatch"
    MISSING = "missing"
    EXTRA = "extra"


# ---------------------------------------------------------------------------
# Cosmetic / allowed-diff exclusion list
# ---------------------------------------------------------------------------
# Properties in this set are view/display properties or dormant-mode defaults
# that OrcaFlex fills automatically and which have no effect on analysis
# results. Diffs for these are downgraded to COSMETIC significance.
ALLOWED_DIFF_PROPS: set[str] = {
    # View / display settings (General section)
    "DefaultViewAngle1",
    "DefaultViewAngle2",
    "DefaultViewCentre",
    "DefaultViewSize",
    "DefaultViewOrientation",
    "DefaultViewResetWhenConnectedObjectMoved",
    "DefaultViewDistortionX",
    "DefaultViewDistortionY",
    "DefaultViewDistortionZ",
    "DefaultViewAzimuth",
    "DefaultViewElevation",
    "DefaultViewMode",
    "DefaultShadedFillMode",
    "DefaultShadedProjectionMode",
    # Drawing / node display on objects
    "DrawNodes",
    "DrawNodesSize",
    "DrawNodesAsDiscs",
    "DrawShaded",
    "DrawShadedDiameter",
    "DrawShadedSections",
    "DrawShadedNodesAsSpheres",
    "DrawNodeSymbol",
    "DrawNodeSize",
    "DrawShadedModel",
    "ShadedDrawingCullingMode",
    # Contact / drawing cosmetics on lines
    "ContactPen",
    "ContactVisualisation",
    # Group state (open/closed in GUI)
    "State",
    "Structure",
    # Model state bookkeeping
    "ModelState",
    # Dormant mode-dependent object properties (filtered by _SKIP_OBJECT_KEYS
    # in generic_builder — setting these causes "Change not allowed" errors)
    "ApplySeabedContactLoadsAtCentreline",
    "SeabedDamping",
    # Sea surface / seabed rendering
    "SeaSurfaceTranslucency",
    "SeabedTranslucency",
    "SeaSurfaceGridDensity",
    "SeabedGridDensity",
    "SeaSurfacePen",
    # View-related display settings
    "DrawShadedSmoothShading",
    "DrawShadedOnSeabed",
    "DrawAxialColour",
    "DrawShadedColour",
    "DrawShadedFillColour",
    "DrawShadedWallThickness",
    "NodeColour",
    "Colour",
    "ShowDeflectedShape",
    "WireframeMode",
    "PenWidth",
    "BackgroundColour",
    # Temperature units — encoding of ° symbol causes false diffs
    "TemperatureUnits",
}


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class PropertyDiff:
    """A single property-level difference."""

    key: str
    mono_val: Any = None
    mod_val: Any = None
    pct_diff: float = 0.0
    significance: str = Significance.MATCH


@dataclass
class ObjectComparison:
    """Comparison result for a single named object in a list section."""

    name: str
    total_mono: int = 0
    total_mod: int = 0
    matches: int = 0
    diffs: list[PropertyDiff] = field(default_factory=list)
    missing_in_mod: list[PropertyDiff] = field(default_factory=list)
    extra_in_mod: list[PropertyDiff] = field(default_factory=list)


@dataclass
class SectionResult:
    """Comparison result for one OrcaFlex section."""

    name: str
    section_type: str  # "flat", "list", "singleton", "nested", "unknown"
    total_mono: int = 0
    total_mod: int = 0
    matches: int = 0
    diffs: list[PropertyDiff] = field(default_factory=list)
    missing_in_mod: list[PropertyDiff] = field(default_factory=list)
    extra_in_mod: list[PropertyDiff] = field(default_factory=list)
    objects: list[ObjectComparison] = field(default_factory=list)
    missing_objects: list[str] = field(default_factory=list)
    extra_objects: list[str] = field(default_factory=list)
    nested_categories: dict[str, ObjectComparison] = field(default_factory=dict)

    @property
    def has_diffs(self) -> bool:
        if self.diffs or self.missing_in_mod or self.extra_in_mod:
            return True
        if self.missing_objects or self.extra_objects:
            return True
        for obj in self.objects:
            if obj.diffs or obj.missing_in_mod or obj.extra_in_mod:
                return True
        for cat in self.nested_categories.values():
            if cat.diffs or cat.missing_in_mod or cat.extra_in_mod:
                return True
        return False

    @property
    def has_significant_diffs(self) -> bool:
        """True if any non-cosmetic differences exist."""
        _non_cosmetic = {
            Significance.MINOR, Significance.SIGNIFICANT,
            Significance.TYPE_MISMATCH, Significance.MISSING, Significance.EXTRA,
        }

        def _has_sig(diffs: list[PropertyDiff]) -> bool:
            return any(d.significance in _non_cosmetic for d in diffs)

        if _has_sig(self.diffs) or _has_sig(self.missing_in_mod) or _has_sig(self.extra_in_mod):
            return True
        if self.missing_objects or self.extra_objects:
            return True
        for obj in self.objects:
            if _has_sig(obj.diffs) or _has_sig(obj.missing_in_mod) or _has_sig(obj.extra_in_mod):
                return True
        for cat in self.nested_categories.values():
            if _has_sig(cat.diffs) or _has_sig(cat.missing_in_mod) or _has_sig(cat.extra_in_mod):
                return True
        return False


@dataclass
class ValidationResult:
    """Complete validation result for a monolithic vs modular comparison."""

    model_name: str
    monolithic_path: str
    modular_path: str
    timestamp: str
    sections: list[SectionResult] = field(default_factory=list)

    @property
    def sections_with_diffs(self) -> int:
        return sum(1 for s in self.sections if s.has_diffs)

    @property
    def sections_with_significant_diffs(self) -> int:
        return sum(1 for s in self.sections if s.has_significant_diffs)

    @property
    def total_sections(self) -> int:
        return len(self.sections)


# ---------------------------------------------------------------------------
# YAML loading
# ---------------------------------------------------------------------------

def load_monolithic(path: Path) -> dict:
    """Load multi-document monolithic YAML, merging all documents."""
    try:
        text = path.read_text(encoding="utf-8-sig")
    except UnicodeDecodeError:
        text = path.read_text(encoding="latin-1")
    merged: dict = {}
    for doc in yaml.safe_load_all(text):
        if isinstance(doc, dict):
            merged.update(doc)
    return merged


def load_modular(modular_dir: Path) -> dict:
    """Load and merge all include files from a modular output directory.

    Uses section-level deep merging: when the same top-level key (e.g.
    ``General``) appears in multiple include files, dict values are merged
    (later file wins per-property), list values are concatenated, and scalar
    values are overwritten.
    """
    merged: dict = {}
    includes = modular_dir / "includes"
    if not includes.exists():
        # Try modular_dir itself if it contains yml files directly
        includes = modular_dir
    for yml_path in sorted(includes.glob("*.yml")):
        try:
            text = yml_path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            text = yml_path.read_text(encoding="latin-1")
        data = yaml.safe_load(text)
        if isinstance(data, dict):
            for key, value in data.items():
                if key in merged:
                    existing = merged[key]
                    if isinstance(existing, dict) and isinstance(value, dict):
                        existing.update(value)
                        continue
                    if isinstance(existing, list) and isinstance(value, list):
                        existing.extend(value)
                        continue
                merged[key] = value
    return merged


# ---------------------------------------------------------------------------
# Value comparison
# ---------------------------------------------------------------------------

def _is_numeric(val: Any) -> bool:
    """Check if a value is numeric (int or float, not bool)."""
    return isinstance(val, (int, float)) and not isinstance(val, bool)


def _classify_diff(pct: float) -> str:
    """Classify a percentage difference into a significance level."""
    if pct == 0.0:
        return Significance.MATCH
    if pct < 0.01:
        return Significance.COSMETIC
    if pct <= 1.0:
        return Significance.MINOR
    return Significance.SIGNIFICANT


def values_equal(
    mono_val: Any,
    mod_val: Any,
    rtol: float = 1e-6,
    atol: float = 1e-10,
) -> tuple[bool, float, str]:
    """Compare two values with tolerance.

    Returns (is_equal, pct_diff, significance).
    """
    # Both None or both null-like
    if mono_val is None and mod_val is None:
        return True, 0.0, Significance.MATCH

    # One None, one not — treat as missing/extra elsewhere
    if mono_val is None or mod_val is None:
        return False, float("inf"), Significance.SIGNIFICANT

    # Numeric comparison
    if _is_numeric(mono_val) and _is_numeric(mod_val):
        mono_f = float(mono_val)
        mod_f = float(mod_val)

        # Both near zero
        if abs(mono_f) < atol and abs(mod_f) < atol:
            return True, 0.0, Significance.MATCH

        # Exact match
        if mono_f == mod_f:
            return True, 0.0, Significance.MATCH

        # Relative check
        denom = max(abs(mono_f), abs(mod_f))
        if denom < atol:
            return True, 0.0, Significance.MATCH

        pct = abs(mono_f - mod_f) / denom * 100.0
        is_eq = pct < rtol * 100.0
        sig = Significance.MATCH if is_eq else _classify_diff(pct)
        return is_eq, round(pct, 6), sig

    # Type mismatch (e.g. string vs number)
    if type(mono_val) != type(mod_val):
        # Special case: int vs float that are equal
        if _is_numeric(mono_val) and _is_numeric(mod_val):
            return values_equal(float(mono_val), float(mod_val), rtol, atol)
        return False, float("inf"), Significance.TYPE_MISMATCH

    # List comparison (element-wise)
    if isinstance(mono_val, list) and isinstance(mod_val, list):
        return _compare_lists(mono_val, mod_val, rtol, atol)

    # Dict comparison (recursive)
    if isinstance(mono_val, dict) and isinstance(mod_val, dict):
        return _compare_dicts(mono_val, mod_val, rtol, atol)

    # String and other exact comparisons
    if mono_val == mod_val:
        return True, 0.0, Significance.MATCH

    # String comparison — check if they differ only in whitespace or casing
    if isinstance(mono_val, str) and isinstance(mod_val, str):
        if mono_val.strip() == mod_val.strip():
            return True, 0.0, Significance.COSMETIC

    return False, float("inf"), Significance.SIGNIFICANT


def _compare_lists(
    mono_list: list,
    mod_list: list,
    rtol: float,
    atol: float,
) -> tuple[bool, float, str]:
    """Compare two lists element-wise."""
    if len(mono_list) != len(mod_list):
        return False, float("inf"), Significance.SIGNIFICANT

    if not mono_list:
        return True, 0.0, Significance.MATCH

    max_pct = 0.0
    worst_sig = Significance.MATCH
    all_equal = True

    for mv, dv in zip(mono_list, mod_list):
        eq, pct, sig = values_equal(mv, dv, rtol, atol)
        if not eq:
            all_equal = False
        if pct != float("inf"):
            max_pct = max(max_pct, pct)
        else:
            max_pct = float("inf")
        # Track worst significance
        if _sig_rank(sig) > _sig_rank(worst_sig):
            worst_sig = sig

    return all_equal, max_pct, worst_sig


def _compare_dicts(
    mono_dict: dict,
    mod_dict: dict,
    rtol: float,
    atol: float,
) -> tuple[bool, float, str]:
    """Compare two dicts key-by-key."""
    all_keys = set(mono_dict.keys()) | set(mod_dict.keys())
    if not all_keys:
        return True, 0.0, Significance.MATCH

    all_equal = True
    max_pct = 0.0
    worst_sig = Significance.MATCH

    for k in all_keys:
        if k not in mono_dict or k not in mod_dict:
            all_equal = False
            worst_sig = Significance.SIGNIFICANT
            continue
        eq, pct, sig = values_equal(mono_dict[k], mod_dict[k], rtol, atol)
        if not eq:
            all_equal = False
        if pct != float("inf"):
            max_pct = max(max_pct, pct)
        else:
            max_pct = float("inf")
        if _sig_rank(sig) > _sig_rank(worst_sig):
            worst_sig = sig

    return all_equal, max_pct, worst_sig


_SIG_RANK = {
    Significance.MATCH: 0,
    Significance.COSMETIC: 1,
    Significance.MINOR: 2,
    Significance.SIGNIFICANT: 3,
    Significance.TYPE_MISMATCH: 4,
    Significance.MISSING: 5,
    Significance.EXTRA: 5,
}


def _sig_rank(sig: str) -> int:
    return _SIG_RANK.get(sig, 3)


# ---------------------------------------------------------------------------
# Section comparators
# ---------------------------------------------------------------------------

def _format_val(val: Any, max_len: int = 60) -> str:
    """Format a value for display, truncating if too long."""
    s = repr(val)
    if len(s) > max_len:
        return s[: max_len - 3] + "..."
    return s


def compare_flat_section(
    name: str,
    mono_data: dict,
    mod_data: dict | None,
    rtol: float,
    atol: float,
    section_type: str = "flat",
) -> SectionResult:
    """Compare a flat dict section (General, Environment, singletons)."""
    result = SectionResult(name=name, section_type=section_type)

    if mod_data is None:
        # Entire section missing in modular
        result.total_mono = len(mono_data) if mono_data else 0
        for k, v in (mono_data or {}).items():
            result.missing_in_mod.append(
                PropertyDiff(key=k, mono_val=v, significance=Significance.MISSING)
            )
        return result

    if mono_data is None:
        result.total_mod = len(mod_data) if mod_data else 0
        for k, v in (mod_data or {}).items():
            result.extra_in_mod.append(
                PropertyDiff(key=k, mod_val=v, significance=Significance.EXTRA)
            )
        return result

    mono_keys = set(mono_data.keys())
    mod_keys = set(mod_data.keys())
    all_keys = mono_keys | mod_keys

    result.total_mono = len(mono_keys)
    result.total_mod = len(mod_keys)

    for key in sorted(all_keys):
        if key not in mod_data:
            result.missing_in_mod.append(
                PropertyDiff(
                    key=key,
                    mono_val=mono_data[key],
                    significance=Significance.MISSING,
                )
            )
            continue

        if key not in mono_data:
            result.extra_in_mod.append(
                PropertyDiff(
                    key=key,
                    mod_val=mod_data[key],
                    significance=Significance.EXTRA,
                )
            )
            continue

        mv = mono_data[key]
        dv = mod_data[key]

        # Special handling for WaveTrains (list-of-dicts within Environment)
        if key == "WaveTrains" and isinstance(mv, list) and isinstance(dv, list):
            eq, pct, sig = _compare_wave_trains(mv, dv, rtol, atol)
            if eq:
                result.matches += 1
            else:
                result.diffs.append(
                    PropertyDiff(key=key, mono_val=mv, mod_val=dv, pct_diff=pct, significance=sig)
                )
            continue

        eq, pct, sig = values_equal(mv, dv, rtol, atol)
        if eq:
            result.matches += 1
        else:
            result.diffs.append(
                PropertyDiff(key=key, mono_val=mv, mod_val=dv, pct_diff=pct, significance=sig)
            )

    return result


def _compare_wave_trains(
    mono_trains: list[dict],
    mod_trains: list[dict],
    rtol: float,
    atol: float,
) -> tuple[bool, float, str]:
    """Compare WaveTrains lists by matching on Name key."""
    mono_by_name = {t.get("Name", f"unnamed_{i}"): t for i, t in enumerate(mono_trains)}
    mod_by_name = {t.get("Name", f"unnamed_{i}"): t for i, t in enumerate(mod_trains)}

    all_names = set(mono_by_name.keys()) | set(mod_by_name.keys())
    all_equal = True
    max_pct = 0.0
    worst_sig = Significance.MATCH

    for train_name in all_names:
        if train_name not in mono_by_name or train_name not in mod_by_name:
            all_equal = False
            worst_sig = Significance.SIGNIFICANT
            continue

        # Compare the two wave train dicts
        eq, pct, sig = _compare_dicts(
            mono_by_name[train_name], mod_by_name[train_name], rtol, atol
        )
        if not eq:
            all_equal = False
        if pct != float("inf"):
            max_pct = max(max_pct, pct)
        else:
            max_pct = float("inf")
        if _sig_rank(sig) > _sig_rank(worst_sig):
            worst_sig = sig

    return all_equal, max_pct, worst_sig


def compare_list_section(
    name: str,
    mono_list: list[dict] | None,
    mod_list: list[dict] | None,
    rtol: float,
    atol: float,
) -> SectionResult:
    """Compare a list-of-dicts section (LineTypes, Lines, Vessels, etc.)."""
    result = SectionResult(name=name, section_type="list")

    mono_list = mono_list or []
    mod_list = mod_list or []

    result.total_mono = len(mono_list)
    result.total_mod = len(mod_list)

    # Index by Name
    mono_by_name: dict[str, dict] = {}
    for item in mono_list:
        obj_name = item.get("Name", None)
        if obj_name is not None:
            mono_by_name[obj_name] = item

    mod_by_name: dict[str, dict] = {}
    for item in mod_list:
        obj_name = item.get("Name", None)
        if obj_name is not None:
            mod_by_name[obj_name] = item

    # Find missing/extra objects
    mono_names = set(mono_by_name.keys())
    mod_names = set(mod_by_name.keys())

    result.missing_objects = sorted(mono_names - mod_names)
    result.extra_objects = sorted(mod_names - mono_names)

    # Compare matched objects
    for obj_name in sorted(mono_names & mod_names):
        mono_obj = mono_by_name[obj_name]
        mod_obj = mod_by_name[obj_name]
        obj_cmp = _compare_object(obj_name, mono_obj, mod_obj, rtol, atol)
        result.objects.append(obj_cmp)
        result.matches += obj_cmp.matches

    return result


def _compare_object(
    name: str,
    mono_obj: dict,
    mod_obj: dict,
    rtol: float,
    atol: float,
) -> ObjectComparison:
    """Compare two named objects property-by-property."""
    cmp = ObjectComparison(name=name)

    mono_keys = set(mono_obj.keys())
    mod_keys = set(mod_obj.keys())
    all_keys = mono_keys | mod_keys

    cmp.total_mono = len(mono_keys)
    cmp.total_mod = len(mod_keys)

    for key in sorted(all_keys):
        if key == "Name":
            # Already matched by name
            cmp.matches += 1
            continue

        if key not in mod_obj:
            cmp.missing_in_mod.append(
                PropertyDiff(
                    key=key,
                    mono_val=mono_obj[key],
                    significance=Significance.MISSING,
                )
            )
            continue

        if key not in mono_obj:
            cmp.extra_in_mod.append(
                PropertyDiff(
                    key=key,
                    mod_val=mod_obj[key],
                    significance=Significance.EXTRA,
                )
            )
            continue

        mv = mono_obj[key]
        dv = mod_obj[key]

        eq, pct, sig = values_equal(mv, dv, rtol, atol)
        if eq:
            cmp.matches += 1
        else:
            cmp.diffs.append(
                PropertyDiff(key=key, mono_val=mv, mod_val=dv, pct_diff=pct, significance=sig)
            )

    return cmp


def compare_nested_section(
    name: str,
    mono_data: dict | None,
    mod_data: dict | None,
    rtol: float,
    atol: float,
) -> SectionResult:
    """Compare a nested dict section (VariableData).

    VariableData is structured as:
        VariableData:
          CategoryName:
            - Name: somename
              key: value
              ...
    """
    result = SectionResult(name=name, section_type="nested")

    mono_data = mono_data or {}
    mod_data = mod_data or {}

    all_categories = set(mono_data.keys()) | set(mod_data.keys())
    result.total_mono = len(mono_data)
    result.total_mod = len(mod_data)

    for cat_name in sorted(all_categories):
        if cat_name not in mod_data:
            result.missing_in_mod.append(
                PropertyDiff(
                    key=cat_name,
                    mono_val=f"[{len(mono_data[cat_name])} items]" if isinstance(mono_data[cat_name], list) else mono_data[cat_name],
                    significance=Significance.MISSING,
                )
            )
            continue

        if cat_name not in mono_data:
            result.extra_in_mod.append(
                PropertyDiff(
                    key=cat_name,
                    mod_val=f"[{len(mod_data[cat_name])} items]" if isinstance(mod_data[cat_name], list) else mod_data[cat_name],
                    significance=Significance.EXTRA,
                )
            )
            continue

        mono_items = mono_data[cat_name]
        mod_items = mod_data[cat_name]

        # Both should be lists of dicts
        if isinstance(mono_items, list) and isinstance(mod_items, list):
            # Compare like a list section
            cat_cmp = ObjectComparison(name=cat_name)
            mono_by_name = {it.get("Name", f"item_{i}"): it for i, it in enumerate(mono_items)}
            mod_by_name = {it.get("Name", f"item_{i}"): it for i, it in enumerate(mod_items)}

            for item_name in sorted(set(mono_by_name.keys()) | set(mod_by_name.keys())):
                if item_name not in mod_by_name:
                    cat_cmp.missing_in_mod.append(
                        PropertyDiff(key=item_name, mono_val="(object)", significance=Significance.MISSING)
                    )
                    continue
                if item_name not in mono_by_name:
                    cat_cmp.extra_in_mod.append(
                        PropertyDiff(key=item_name, mod_val="(object)", significance=Significance.EXTRA)
                    )
                    continue

                # Compare item properties
                mono_item = mono_by_name[item_name]
                mod_item = mod_by_name[item_name]
                for prop_key in sorted(set(mono_item.keys()) | set(mod_item.keys())):
                    if prop_key == "Name":
                        cat_cmp.matches += 1
                        continue
                    if prop_key not in mod_item:
                        cat_cmp.missing_in_mod.append(
                            PropertyDiff(key=f"{item_name}.{prop_key}", mono_val=mono_item[prop_key], significance=Significance.MISSING)
                        )
                        continue
                    if prop_key not in mono_item:
                        cat_cmp.extra_in_mod.append(
                            PropertyDiff(key=f"{item_name}.{prop_key}", mod_val=mod_item[prop_key], significance=Significance.EXTRA)
                        )
                        continue

                    eq, pct, sig = values_equal(mono_item[prop_key], mod_item[prop_key], rtol, atol)
                    if eq:
                        cat_cmp.matches += 1
                    else:
                        cat_cmp.diffs.append(
                            PropertyDiff(
                                key=f"{item_name}.{prop_key}",
                                mono_val=mono_item[prop_key],
                                mod_val=mod_item[prop_key],
                                pct_diff=pct,
                                significance=sig,
                            )
                        )

            cat_cmp.total_mono = sum(len(it) for it in mono_items if isinstance(it, dict))
            cat_cmp.total_mod = sum(len(it) for it in mod_items if isinstance(it, dict))
            result.nested_categories[cat_name] = cat_cmp
            result.matches += cat_cmp.matches
        else:
            # Non-list nested data — compare directly
            eq, pct, sig = values_equal(mono_items, mod_items, rtol, atol)
            if eq:
                result.matches += 1
            else:
                result.diffs.append(
                    PropertyDiff(key=cat_name, mono_val=mono_items, mod_val=mod_items, pct_diff=pct, significance=sig)
                )

    return result


# ---------------------------------------------------------------------------
# Main comparison engine
# ---------------------------------------------------------------------------

def _downgrade_allowed_diffs(result: SectionResult) -> SectionResult:
    """Downgrade diffs for allowed (cosmetic) properties to COSMETIC significance.

    This applies to both top-level diffs and object/category-level diffs.
    Properties in ``ALLOWED_DIFF_PROPS`` are view/display settings that don't
    affect analysis results.
    """
    for d in result.diffs:
        if d.key in ALLOWED_DIFF_PROPS and d.significance not in (
            Significance.MATCH,
            Significance.COSMETIC,
        ):
            d.significance = Significance.COSMETIC

    for d in result.missing_in_mod:
        if d.key in ALLOWED_DIFF_PROPS:
            d.significance = Significance.COSMETIC

    for d in result.extra_in_mod:
        if d.key in ALLOWED_DIFF_PROPS:
            d.significance = Significance.COSMETIC

    for obj in result.objects:
        for d in obj.diffs:
            if d.key in ALLOWED_DIFF_PROPS and d.significance not in (
                Significance.MATCH,
                Significance.COSMETIC,
            ):
                d.significance = Significance.COSMETIC
        for d in obj.missing_in_mod:
            if d.key in ALLOWED_DIFF_PROPS:
                d.significance = Significance.COSMETIC
        for d in obj.extra_in_mod:
            if d.key in ALLOWED_DIFF_PROPS:
                d.significance = Significance.COSMETIC

    for cat in result.nested_categories.values():
        for d in cat.diffs:
            # Nested category keys are "itemName.propKey" — check the prop part
            prop_key = d.key.rsplit(".", 1)[-1] if "." in d.key else d.key
            if prop_key in ALLOWED_DIFF_PROPS and d.significance not in (
                Significance.MATCH,
                Significance.COSMETIC,
            ):
                d.significance = Significance.COSMETIC
        for d in cat.missing_in_mod:
            prop_key = d.key.rsplit(".", 1)[-1] if "." in d.key else d.key
            if prop_key in ALLOWED_DIFF_PROPS:
                d.significance = Significance.COSMETIC
        for d in cat.extra_in_mod:
            prop_key = d.key.rsplit(".", 1)[-1] if "." in d.key else d.key
            if prop_key in ALLOWED_DIFF_PROPS:
                d.significance = Significance.COSMETIC

    return result


def validate(
    mono_data: dict,
    mod_data: dict,
    rtol: float = 1e-6,
    atol: float = 1e-10,
    sections_filter: list[str] | None = None,
) -> list[SectionResult]:
    """Compare monolithic and modular YAML data section by section."""
    results: list[SectionResult] = []

    # Determine which sections to compare
    all_sections = set(mono_data.keys()) | set(mod_data.keys())
    if sections_filter:
        all_sections = all_sections & set(sections_filter)

    for section_name in sorted(all_sections):
        mono_section = mono_data.get(section_name)
        mod_section = mod_data.get(section_name)

        if section_name in FLAT_SECTIONS:
            result = compare_flat_section(
                section_name, mono_section, mod_section, rtol, atol, "flat"
            )
        elif section_name in LIST_SECTIONS:
            result = compare_list_section(section_name, mono_section, mod_section, rtol, atol)
        elif section_name in SINGLETON_SECTIONS:
            result = compare_flat_section(
                section_name, mono_section, mod_section, rtol, atol, "singleton"
            )
        elif section_name in NESTED_SECTIONS:
            result = compare_nested_section(section_name, mono_section, mod_section, rtol, atol)
        else:
            # Unknown section — treat as flat if dict, skip if not
            if isinstance(mono_section, dict) or isinstance(mod_section, dict):
                result = compare_flat_section(
                    section_name,
                    mono_section if isinstance(mono_section, dict) else None,
                    mod_section if isinstance(mod_section, dict) else None,
                    rtol,
                    atol,
                    "unknown",
                )
            elif isinstance(mono_section, list) or isinstance(mod_section, list):
                # Might be a list section we don't know about
                result = compare_list_section(
                    section_name,
                    mono_section if isinstance(mono_section, list) else None,
                    mod_section if isinstance(mod_section, list) else None,
                    rtol,
                    atol,
                )
            else:
                # Scalar or unrecognized — compare directly
                result = SectionResult(name=section_name, section_type="unknown")
                if mono_section is None:
                    result.extra_in_mod.append(
                        PropertyDiff(key=section_name, mod_val=mod_section, significance=Significance.EXTRA)
                    )
                elif mod_section is None:
                    result.missing_in_mod.append(
                        PropertyDiff(key=section_name, mono_val=mono_section, significance=Significance.MISSING)
                    )
                else:
                    eq, pct, sig = values_equal(mono_section, mod_section, rtol, atol)
                    if eq:
                        result.matches = 1
                    else:
                        result.diffs.append(
                            PropertyDiff(
                                key=section_name,
                                mono_val=mono_section,
                                mod_val=mod_section,
                                pct_diff=pct,
                                significance=sig,
                            )
                        )

        # Downgrade allowed (cosmetic) property diffs
        result = _downgrade_allowed_diffs(result)
        results.append(result)

    return results


# ---------------------------------------------------------------------------
# Console output
# ---------------------------------------------------------------------------

_SIG_COLORS = {
    Significance.MATCH: "",
    Significance.COSMETIC: "\033[33m",
    Significance.MINOR: "\033[33m",
    Significance.SIGNIFICANT: "\033[31m",
    Significance.TYPE_MISMATCH: "\033[35m",
    Significance.MISSING: "\033[36m",
    Significance.EXTRA: "\033[36m",
}
_RESET = "\033[0m"


def _sig_label(sig: str) -> str:
    """Human-readable label for significance."""
    labels = {
        Significance.MATCH: "equal",
        Significance.COSMETIC: "cosmetic",
        Significance.MINOR: "MINOR",
        Significance.SIGNIFICANT: "DIFF",
        Significance.TYPE_MISMATCH: "TYPE MISMATCH",
        Significance.MISSING: "MISSING in mod",
        Significance.EXTRA: "EXTRA in mod",
    }
    return labels.get(sig, sig)


def _pct_str(pct: float) -> str:
    """Format percentage for display."""
    if pct == float("inf") or math.isinf(pct):
        return ""
    if pct == 0.0:
        return ""
    return f"{pct:.4f}%"


def print_console(result: ValidationResult, use_color: bool = True) -> None:
    """Print validation results to console."""
    c = _SIG_COLORS if use_color else {k: "" for k in _SIG_COLORS}
    r = _RESET if use_color else ""

    print(f"\n{'=' * 60}")
    print(f"  Semantic Validation: {result.model_name}")
    print(f"{'=' * 60}")
    print(f"  Monolithic: {result.monolithic_path}")
    print(f"  Modular:    {result.modular_path}")
    print()

    for section in result.sections:
        _print_section_console(section, c, r)

    # Summary
    print(f"\n{'=' * 60}")
    print(
        f"  SUMMARY: {result.total_sections} sections compared, "
        f"{result.sections_with_diffs} with differences"
    )
    print(f"{'=' * 60}\n")


def _print_section_console(
    section: SectionResult,
    c: dict[str, str],
    r: str,
) -> None:
    """Print a single section's results to console."""
    type_label = f"({section.section_type})"
    header = f"Section: {section.name} {type_label}"

    if section.section_type == "list":
        obj_count = max(section.total_mono, section.total_mod)
        header += f" [{obj_count} object(s)]"

    print(f"\n{header}")
    print("-" * len(header))

    # Flat/singleton sections
    if section.section_type in ("flat", "singleton", "unknown"):
        total = section.matches + len(section.diffs)
        print(f"  MATCH: {section.matches}/{total} properties identical")

        if section.diffs:
            print(f"  DIFF:  {len(section.diffs)} properties differ")
            for d in section.diffs:
                pct = _pct_str(d.pct_diff)
                pct_part = f" ({pct})" if pct else ""
                color = c.get(d.significance, "")
                print(
                    f"    {d.key:40s} mono={_format_val(d.mono_val, 30):>30s}  "
                    f"mod={_format_val(d.mod_val, 30):>30s}  "
                    f"{color}({_sig_label(d.significance)}{pct_part}){r}"
                )

        if section.missing_in_mod:
            print(f"  MISSING in modular: {len(section.missing_in_mod)} properties")
            for d in section.missing_in_mod:
                print(f"    {c.get(Significance.MISSING, '')}{d.key}: {_format_val(d.mono_val, 50)}{r}")

        if section.extra_in_mod:
            print(f"  EXTRA in modular: {len(section.extra_in_mod)} properties")
            for d in section.extra_in_mod:
                print(f"    {c.get(Significance.EXTRA, '')}{d.key}: {_format_val(d.mod_val, 50)}{r}")

    # List sections
    elif section.section_type == "list":
        if section.missing_objects:
            print(f"  MISSING objects in modular: {section.missing_objects}")
        if section.extra_objects:
            print(f"  EXTRA objects in modular: {section.extra_objects}")

        for obj in section.objects:
            total = obj.matches + len(obj.diffs)
            print(f"\n  Object \"{obj.name}\":")
            print(f"    MATCH: {obj.matches}/{total} properties")

            if obj.diffs:
                print(f"    DIFF: {len(obj.diffs)} properties")
                for d in obj.diffs:
                    pct = _pct_str(d.pct_diff)
                    pct_part = f" ({pct})" if pct else ""
                    color = c.get(d.significance, "")
                    print(
                        f"      {d.key:38s} mono={_format_val(d.mono_val, 25):>25s}  "
                        f"mod={_format_val(d.mod_val, 25):>25s}  "
                        f"{color}({_sig_label(d.significance)}{pct_part}){r}"
                    )

            if obj.missing_in_mod:
                print(f"    MISSING in modular: {len(obj.missing_in_mod)}")
                for d in obj.missing_in_mod:
                    print(f"      {d.key}: {_format_val(d.mono_val, 50)}")

            if obj.extra_in_mod:
                print(f"    EXTRA in modular: {len(obj.extra_in_mod)}")
                for d in obj.extra_in_mod:
                    print(f"      {d.key}: {_format_val(d.mod_val, 50)}")

    # Nested sections
    elif section.section_type == "nested":
        if section.missing_in_mod:
            print(f"  MISSING categories in modular: {len(section.missing_in_mod)}")
            for d in section.missing_in_mod:
                print(f"    {d.key}: {_format_val(d.mono_val, 50)}")

        if section.extra_in_mod:
            print(f"  EXTRA categories in modular: {len(section.extra_in_mod)}")
            for d in section.extra_in_mod:
                print(f"    {d.key}: {_format_val(d.mod_val, 50)}")

        for cat_name, cat_cmp in section.nested_categories.items():
            total = cat_cmp.matches + len(cat_cmp.diffs)
            print(f"\n  Category \"{cat_name}\":")
            print(f"    MATCH: {cat_cmp.matches}/{total} properties")

            if cat_cmp.diffs:
                print(f"    DIFF: {len(cat_cmp.diffs)} properties")
                for d in cat_cmp.diffs:
                    pct = _pct_str(d.pct_diff)
                    pct_part = f" ({pct})" if pct else ""
                    color = c.get(d.significance, "")
                    print(
                        f"      {d.key:38s} mono={_format_val(d.mono_val, 25):>25s}  "
                        f"mod={_format_val(d.mod_val, 25):>25s}  "
                        f"{color}({_sig_label(d.significance)}{pct_part}){r}"
                    )

            if cat_cmp.missing_in_mod:
                print(f"    MISSING: {len(cat_cmp.missing_in_mod)}")
                for d in cat_cmp.missing_in_mod:
                    print(f"      {d.key}")

            if cat_cmp.extra_in_mod:
                print(f"    EXTRA: {len(cat_cmp.extra_in_mod)}")
                for d in cat_cmp.extra_in_mod:
                    print(f"      {d.key}")


# ---------------------------------------------------------------------------
# JSON output
# ---------------------------------------------------------------------------

def _serialize_val(val: Any) -> Any:
    """Make a value JSON-serializable."""
    if isinstance(val, float) and (math.isinf(val) or math.isnan(val)):
        return str(val)
    if isinstance(val, Path):
        return str(val)
    return val


def _diff_to_dict(d: PropertyDiff) -> dict:
    """Convert a PropertyDiff to a JSON-friendly dict."""
    out = {"key": d.key, "significance": d.significance}
    if d.mono_val is not None:
        out["mono"] = _serialize_val(d.mono_val)
    if d.mod_val is not None:
        out["mod"] = _serialize_val(d.mod_val)
    if d.pct_diff != 0.0:
        out["pct_diff"] = _serialize_val(d.pct_diff)
    return out


def to_json(result: ValidationResult) -> dict:
    """Convert ValidationResult to a JSON-serializable dict."""
    sections_out = {}
    for s in result.sections:
        sec: dict[str, Any] = {
            "type": s.section_type,
            "total_mono": s.total_mono,
            "total_mod": s.total_mod,
            "matches": s.matches,
            "diffs": [_diff_to_dict(d) for d in s.diffs],
            "missing_in_mod": [_diff_to_dict(d) for d in s.missing_in_mod],
            "extra_in_mod": [_diff_to_dict(d) for d in s.extra_in_mod],
        }

        if s.section_type == "list":
            sec["missing_objects"] = s.missing_objects
            sec["extra_objects"] = s.extra_objects
            sec["objects"] = {}
            for obj in s.objects:
                sec["objects"][obj.name] = {
                    "total_mono": obj.total_mono,
                    "total_mod": obj.total_mod,
                    "matches": obj.matches,
                    "diffs": [_diff_to_dict(d) for d in obj.diffs],
                    "missing_in_mod": [_diff_to_dict(d) for d in obj.missing_in_mod],
                    "extra_in_mod": [_diff_to_dict(d) for d in obj.extra_in_mod],
                }

        if s.section_type == "nested":
            sec["categories"] = {}
            for cat_name, cat_cmp in s.nested_categories.items():
                sec["categories"][cat_name] = {
                    "total_mono": cat_cmp.total_mono,
                    "total_mod": cat_cmp.total_mod,
                    "matches": cat_cmp.matches,
                    "diffs": [_diff_to_dict(d) for d in cat_cmp.diffs],
                    "missing_in_mod": [_diff_to_dict(d) for d in cat_cmp.missing_in_mod],
                    "extra_in_mod": [_diff_to_dict(d) for d in cat_cmp.extra_in_mod],
                }

        sections_out[s.name] = sec

    return {
        "model": result.model_name,
        "monolithic_path": result.monolithic_path,
        "modular_path": result.modular_path,
        "timestamp": result.timestamp,
        "sections": sections_out,
        "summary": {
            "total_sections": result.total_sections,
            "sections_with_diffs": result.sections_with_diffs,
        },
    }


def summarize(sections: list[SectionResult]) -> dict:
    """Produce a compact per-section summary dict for benchmark embedding.

    Returns a dict with top-level counts and a ``sections`` sub-dict keyed by
    section name, each containing match/diff/missing/extra counts and the
    worst significance level encountered.
    """
    total = len(sections)
    with_diffs = 0
    significant_count = 0
    sec_out: dict[str, dict] = {}

    for s in sections:
        # Count all diffs across objects and nested categories
        n_diffs = len(s.diffs)
        n_missing = len(s.missing_in_mod)
        n_extra = len(s.extra_in_mod)
        worst = Significance.MATCH

        # Collect significances from top-level diffs
        all_sigs = [d.significance for d in s.diffs]
        all_sigs += [d.significance for d in s.missing_in_mod]
        all_sigs += [d.significance for d in s.extra_in_mod]

        # Object-level (list sections)
        for obj in s.objects:
            n_diffs += len(obj.diffs)
            n_missing += len(obj.missing_in_mod)
            n_extra += len(obj.extra_in_mod)
            all_sigs += [d.significance for d in obj.diffs]
            all_sigs += [d.significance for d in obj.missing_in_mod]
            all_sigs += [d.significance for d in obj.extra_in_mod]

        # Nested categories (VariableData)
        for cat in s.nested_categories.values():
            n_diffs += len(cat.diffs)
            n_missing += len(cat.missing_in_mod)
            n_extra += len(cat.extra_in_mod)
            all_sigs += [d.significance for d in cat.diffs]
            all_sigs += [d.significance for d in cat.missing_in_mod]
            all_sigs += [d.significance for d in cat.extra_in_mod]

        # Missing/extra objects count as significant
        n_missing += len(s.missing_objects)
        n_extra += len(s.extra_objects)
        if s.missing_objects or s.extra_objects:
            all_sigs.append(Significance.SIGNIFICANT)

        # Determine worst significance
        sig_order = [
            Significance.MATCH, Significance.COSMETIC, Significance.MINOR,
            Significance.SIGNIFICANT, Significance.TYPE_MISMATCH,
            Significance.MISSING, Significance.EXTRA,
        ]
        for sig in all_sigs:
            if sig in sig_order:
                idx = sig_order.index(sig)
                if idx > sig_order.index(worst):
                    worst = sig

        has_diff = s.has_diffs
        if has_diff:
            with_diffs += 1
        if worst in (Significance.SIGNIFICANT, Significance.TYPE_MISMATCH,
                     Significance.MISSING, Significance.EXTRA):
            significant_count += 1

        sec_out[s.name] = {
            "matches": s.matches,
            "diffs": n_diffs,
            "missing": n_missing,
            "extra": n_extra,
            "worst_significance": worst,
        }

    return {
        "total_sections": total,
        "sections_with_diffs": with_diffs,
        "significant_diffs": significant_count,
        "sections": sec_out,
    }


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------

_HTML_TEMPLATE_HEAD = """\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{title}</title>
<style>
* {{ box-sizing: border-box; }}
body {{
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
               Roboto, Arial, sans-serif;
  margin: 0; padding: 0; color: #333; background: #f8f9fa;
  font-size: 14px; line-height: 1.5;
}}
.container {{ max-width: 1400px; margin: 0 auto; padding: 1.5em 2em; }}

/* Header */
.report-header {{
  background: #2c3e50; color: #fff; padding: 1.2em 2em;
  margin-bottom: 1.5em; border-radius: 6px;
}}
.report-header h1 {{ margin: 0 0 0.3em; font-size: 1.6em; }}
.report-header .meta {{ font-size: 0.9em; opacity: 0.85; }}

/* Section cards */
.section {{ background: #fff; border-radius: 6px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  margin-bottom: 1.5em; padding: 1.2em 1.5em;
}}
.section h2 {{ margin: 0 0 0.8em; font-size: 1.2em; color: #2c3e50;
  border-bottom: 2px solid #3498db; padding-bottom: 0.3em; }}
.section h3 {{ font-size: 1.0em; color: #2c3e50; margin: 1em 0 0.5em;
  border-bottom: 1px solid #ddd; padding-bottom: 0.15em; }}

/* Tables */
table {{ border-collapse: collapse; margin: 0.5em 0 1em; font-size: 0.85em; width: 100%; }}
th, td {{ border: 1px solid #ddd; padding: 0.45em 0.7em; text-align: left; }}
th {{ background: #34495e; color: #fff; font-weight: 600;
  font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px; }}
tbody tr:nth-child(even) {{ background: #f8f9fa; }}
tbody tr:nth-child(odd) {{ background: #fff; }}
tbody tr:hover {{ background: #ebf5fb; }}
td.mono, .mono {{ font-family: 'SF Mono', 'Cascadia Code', 'Consolas',
  'Fira Code', monospace; }}
td.num {{ text-align: right; font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace; font-size: 0.85em; }}

/* Badges */
.badge {{ display: inline-block; padding: 3px 10px; border-radius: 3px;
  color: #fff; font-size: 0.8em; font-weight: 700; }}
.badge-match {{ background: #27ae60; }}
.badge-cosmetic {{ background: #95a5a6; }}
.badge-minor {{ background: #f39c12; }}
.badge-significant {{ background: #e74c3c; }}
.badge-type-mismatch {{ background: #8e44ad; }}
.badge-missing {{ background: #3498db; }}
.badge-extra {{ background: #1abc9c; }}
.badge-pass {{ background: #27ae60; }}
.badge-warn {{ background: #f39c12; }}
.badge-fail {{ background: #e74c3c; }}

/* Collapsible */
details {{ margin: 0.5em 0; }}
details summary {{ cursor: pointer; font-weight: 600; color: #2c3e50;
  padding: 0.3em 0; user-select: none; }}
details summary:hover {{ color: #3498db; }}

/* TOC */
.toc {{ background: #fff; border: 1px solid #ddd; border-radius: 6px;
  padding: 1em 1.2em; margin-bottom: 1.5em;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08); }}
.toc a {{ color: #3498db; text-decoration: none; font-size: 0.85em; }}
.toc a:hover {{ text-decoration: underline; }}
.toc ol {{ padding-left: 1.5em; }}
.toc li {{ margin: 0.2em 0; }}

/* Value cells */
.val {{ font-family: 'SF Mono', 'Cascadia Code', 'Consolas', monospace;
  font-size: 0.82em; max-width: 280px; overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; }}
.val-wrap {{ white-space: normal; word-break: break-all; }}
</style>
</head>
<body>
<div class="container">
"""

_HTML_TEMPLATE_FOOT = """\
</div>
</body>
</html>
"""


def _sig_badge(sig: str) -> str:
    """Return an HTML badge for a significance level."""
    css_class = {
        Significance.MATCH: "badge-match",
        Significance.COSMETIC: "badge-cosmetic",
        Significance.MINOR: "badge-minor",
        Significance.SIGNIFICANT: "badge-significant",
        Significance.TYPE_MISMATCH: "badge-type-mismatch",
        Significance.MISSING: "badge-missing",
        Significance.EXTRA: "badge-extra",
    }.get(sig, "badge-warn")

    label = _sig_label(sig)
    return f'<span class="badge {css_class}">{label}</span>'


def _html_val(val: Any, max_len: int = 80) -> str:
    """Format a value for HTML display."""
    import html as html_mod
    s = repr(val)
    if len(s) > max_len:
        s = s[: max_len - 3] + "..."
    return f'<span class="val">{html_mod.escape(s)}</span>'


def _diff_table_html(diffs: list[PropertyDiff], title: str = "Differences") -> str:
    """Generate an HTML table for a list of property diffs."""
    if not diffs:
        return ""

    rows = []
    for d in diffs:
        pct = _pct_str(d.pct_diff)
        pct_cell = f'<td class="num">{pct}</td>' if pct else '<td class="num">-</td>'
        mono_cell = _html_val(d.mono_val) if d.mono_val is not None else "-"
        mod_cell = _html_val(d.mod_val) if d.mod_val is not None else "-"
        rows.append(
            f"<tr>"
            f'<td class="mono">{d.key}</td>'
            f"<td>{mono_cell}</td>"
            f"<td>{mod_cell}</td>"
            f"{pct_cell}"
            f"<td>{_sig_badge(d.significance)}</td>"
            f"</tr>"
        )

    return f"""
    <table>
    <thead><tr><th>Property</th><th>Monolithic</th><th>Modular</th><th>Diff %</th><th>Status</th></tr></thead>
    <tbody>
    {''.join(rows)}
    </tbody>
    </table>
    """


def generate_html(result: ValidationResult) -> str:
    """Generate a single-page HTML validation report."""
    title = f"Semantic Validation: {result.model_name}"
    parts = [_HTML_TEMPLATE_HEAD.format(title=title)]

    # Header
    parts.append(f"""
    <div class="report-header">
      <h1>{title}</h1>
      <div class="meta">
        Monolithic: {result.monolithic_path}<br>
        Modular: {result.modular_path}<br>
        Generated: {result.timestamp}
      </div>
    </div>
    """)

    # Summary table
    parts.append('<div class="section"><h2>Summary</h2>')
    parts.append("""
    <table>
    <thead><tr><th>Section</th><th>Type</th><th>Mono Props</th><th>Mod Props</th>
    <th>Matches</th><th>Diffs</th><th>Missing</th><th>Extra</th><th>Status</th></tr></thead>
    <tbody>
    """)

    for s in result.sections:
        diff_count = len(s.diffs)
        missing_count = len(s.missing_in_mod)
        extra_count = len(s.extra_in_mod)

        # For list sections, aggregate object-level counts
        for obj in s.objects:
            diff_count += len(obj.diffs)
            missing_count += len(obj.missing_in_mod)
            extra_count += len(obj.extra_in_mod)

        # For nested sections, aggregate category-level counts
        for cat in s.nested_categories.values():
            diff_count += len(cat.diffs)
            missing_count += len(cat.missing_in_mod)
            extra_count += len(cat.extra_in_mod)

        if not s.has_significant_diffs:
            status_badge = '<span class="badge badge-pass">PASS</span>'
        elif diff_count == 0 and missing_count > 0:
            status_badge = '<span class="badge badge-warn">PARTIAL</span>'
        else:
            status_badge = '<span class="badge badge-fail">DIFF</span>'

        parts.append(
            f"<tr>"
            f'<td><a href="#sec-{s.name}">{s.name}</a></td>'
            f"<td>{s.section_type}</td>"
            f'<td class="num">{s.total_mono}</td>'
            f'<td class="num">{s.total_mod}</td>'
            f'<td class="num">{s.matches}</td>'
            f'<td class="num">{diff_count}</td>'
            f'<td class="num">{missing_count}</td>'
            f'<td class="num">{extra_count}</td>'
            f"<td>{status_badge}</td>"
            f"</tr>"
        )

    parts.append("</tbody></table></div>")

    # Per-section details
    for s in result.sections:
        parts.append(f'<div class="section" id="sec-{s.name}">')
        parts.append(f"<h2>{s.name} ({s.section_type})</h2>")

        if not s.has_diffs:
            parts.append(f'<p><span class="badge badge-pass">ALL MATCH</span> {s.matches} properties identical</p>')
            parts.append("</div>")
            continue

        # Flat / singleton
        if s.section_type in ("flat", "singleton", "unknown"):
            total = s.matches + len(s.diffs)
            parts.append(f"<p>Matched: {s.matches}/{total} properties</p>")

            if s.diffs:
                parts.append("<details open><summary>Differences</summary>")
                parts.append(_diff_table_html(s.diffs))
                parts.append("</details>")

            if s.missing_in_mod:
                parts.append(f"<details><summary>Missing in modular ({len(s.missing_in_mod)})</summary>")
                parts.append(_diff_table_html(s.missing_in_mod, "Missing"))
                parts.append("</details>")

            if s.extra_in_mod:
                parts.append(f"<details><summary>Extra in modular ({len(s.extra_in_mod)})</summary>")
                parts.append(_diff_table_html(s.extra_in_mod, "Extra"))
                parts.append("</details>")

        # List sections
        elif s.section_type == "list":
            if s.missing_objects:
                parts.append(f'<p><span class="badge badge-missing">MISSING OBJECTS</span> {", ".join(s.missing_objects)}</p>')
            if s.extra_objects:
                parts.append(f'<p><span class="badge badge-extra">EXTRA OBJECTS</span> {", ".join(s.extra_objects)}</p>')

            for obj in s.objects:
                total = obj.matches + len(obj.diffs)
                has_issues = obj.diffs or obj.missing_in_mod or obj.extra_in_mod
                open_attr = " open" if has_issues else ""
                parts.append(f'<details{open_attr}><summary>Object: {obj.name} ({obj.matches}/{total} match)</summary>')

                if obj.diffs:
                    parts.append(_diff_table_html(obj.diffs))

                if obj.missing_in_mod:
                    parts.append(f"<h4>Missing in modular ({len(obj.missing_in_mod)})</h4>")
                    parts.append(_diff_table_html(obj.missing_in_mod, "Missing"))

                if obj.extra_in_mod:
                    parts.append(f"<h4>Extra in modular ({len(obj.extra_in_mod)})</h4>")
                    parts.append(_diff_table_html(obj.extra_in_mod, "Extra"))

                if not has_issues:
                    parts.append(f'<p><span class="badge badge-pass">ALL MATCH</span></p>')

                parts.append("</details>")

        # Nested sections
        elif s.section_type == "nested":
            if s.missing_in_mod:
                parts.append(f"<p>Missing categories: {len(s.missing_in_mod)}</p>")
                parts.append(_diff_table_html(s.missing_in_mod, "Missing categories"))

            if s.extra_in_mod:
                parts.append(f"<p>Extra categories: {len(s.extra_in_mod)}</p>")
                parts.append(_diff_table_html(s.extra_in_mod, "Extra categories"))

            for cat_name, cat_cmp in s.nested_categories.items():
                total = cat_cmp.matches + len(cat_cmp.diffs)
                has_issues = cat_cmp.diffs or cat_cmp.missing_in_mod or cat_cmp.extra_in_mod
                open_attr = " open" if has_issues else ""
                parts.append(f'<details{open_attr}><summary>Category: {cat_name} ({cat_cmp.matches}/{total} match)</summary>')

                if cat_cmp.diffs:
                    parts.append(_diff_table_html(cat_cmp.diffs))

                if cat_cmp.missing_in_mod:
                    parts.append(f"<h4>Missing ({len(cat_cmp.missing_in_mod)})</h4>")
                    parts.append(_diff_table_html(cat_cmp.missing_in_mod))

                if cat_cmp.extra_in_mod:
                    parts.append(f"<h4>Extra ({len(cat_cmp.extra_in_mod)})</h4>")
                    parts.append(_diff_table_html(cat_cmp.extra_in_mod))

                if not has_issues:
                    parts.append(f'<p><span class="badge badge-pass">ALL MATCH</span></p>')

                parts.append("</details>")

        parts.append("</div>")

    parts.append(_HTML_TEMPLATE_FOOT)
    return "\n".join(parts)


def generate_batch_html(results: list[ValidationResult]) -> str:
    """Generate an aggregate HTML report for batch validation."""
    title = "Batch Semantic Validation Report"
    parts = [_HTML_TEMPLATE_HEAD.format(title=title)]

    timestamp = datetime.now(tz=timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")

    # Header
    parts.append(f"""
    <div class="report-header">
      <h1>{title}</h1>
      <div class="meta">
        Models: {len(results)}<br>
        Generated: {timestamp}
      </div>
    </div>
    """)

    # Aggregate summary table
    parts.append('<div class="section"><h2>Aggregate Summary</h2>')
    parts.append("""
    <table>
    <thead><tr><th>Model</th><th>Sections</th><th>With Diffs</th><th>Status</th></tr></thead>
    <tbody>
    """)

    for r in results:
        if r.sections_with_diffs == 0:
            status = '<span class="badge badge-pass">PASS</span>'
        else:
            status = f'<span class="badge badge-fail">{r.sections_with_diffs} DIFF</span>'

        parts.append(
            f"<tr>"
            f'<td><a href="#model-{r.model_name}">{r.model_name}</a></td>'
            f'<td class="num">{r.total_sections}</td>'
            f'<td class="num">{r.sections_with_diffs}</td>'
            f"<td>{status}</td>"
            f"</tr>"
        )

    parts.append("</tbody></table></div>")

    # Cross-section diff matrix: which sections have diffs across all models
    all_section_names: set[str] = set()
    for r in results:
        for s in r.sections:
            all_section_names.add(s.name)

    if all_section_names:
        parts.append('<div class="section"><h2>Section Diff Matrix</h2>')
        parts.append("<table><thead><tr><th>Section</th>")
        for r in results:
            parts.append(f"<th>{r.model_name}</th>")
        parts.append("</tr></thead><tbody>")

        for sec_name in sorted(all_section_names):
            parts.append(f'<tr><td class="mono">{sec_name}</td>')
            for r in results:
                matching_section = next((s for s in r.sections if s.name == sec_name), None)
                if matching_section is None:
                    parts.append('<td>-</td>')
                elif not matching_section.has_significant_diffs:
                    parts.append('<td><span class="badge badge-pass">OK</span></td>')
                else:
                    parts.append('<td><span class="badge badge-fail">DIFF</span></td>')
            parts.append("</tr>")

        parts.append("</tbody></table></div>")

    # Per-model detail sections
    for r in results:
        parts.append(f'<div class="section" id="model-{r.model_name}">')
        parts.append(f"<h2>{r.model_name}</h2>")
        parts.append(f"<p>Monolithic: <code>{r.monolithic_path}</code><br>Modular: <code>{r.modular_path}</code></p>")

        # Summary table for this model
        parts.append("""
        <table>
        <thead><tr><th>Section</th><th>Type</th><th>Matches</th><th>Diffs</th><th>Status</th></tr></thead>
        <tbody>
        """)

        for s in r.sections:
            diff_count = len(s.diffs)
            for obj in s.objects:
                diff_count += len(obj.diffs)
            for cat in s.nested_categories.values():
                diff_count += len(cat.diffs)

            if not s.has_significant_diffs:
                status_badge = '<span class="badge badge-pass">PASS</span>'
            else:
                status_badge = '<span class="badge badge-fail">DIFF</span>'

            parts.append(
                f"<tr>"
                f'<td class="mono">{s.name}</td>'
                f"<td>{s.section_type}</td>"
                f'<td class="num">{s.matches}</td>'
                f'<td class="num">{diff_count}</td>'
                f"<td>{status_badge}</td>"
                f"</tr>"
            )

        parts.append("</tbody></table>")

        # Show diffs for sections that have them
        for s in r.sections:
            if not s.has_diffs:
                continue

            parts.append(f"<details><summary>{s.name} ({s.section_type}) - differences</summary>")

            all_diffs = list(s.diffs)
            for obj in s.objects:
                for d in obj.diffs:
                    all_diffs.append(PropertyDiff(
                        key=f"{obj.name}.{d.key}",
                        mono_val=d.mono_val,
                        mod_val=d.mod_val,
                        pct_diff=d.pct_diff,
                        significance=d.significance,
                    ))
                for d in obj.missing_in_mod:
                    all_diffs.append(PropertyDiff(
                        key=f"{obj.name}.{d.key}",
                        mono_val=d.mono_val,
                        significance=Significance.MISSING,
                    ))
                for d in obj.extra_in_mod:
                    all_diffs.append(PropertyDiff(
                        key=f"{obj.name}.{d.key}",
                        mod_val=d.mod_val,
                        significance=Significance.EXTRA,
                    ))
            for cat_name, cat_cmp in s.nested_categories.items():
                for d in cat_cmp.diffs:
                    all_diffs.append(PropertyDiff(
                        key=f"{cat_name}.{d.key}",
                        mono_val=d.mono_val,
                        mod_val=d.mod_val,
                        pct_diff=d.pct_diff,
                        significance=d.significance,
                    ))

            all_diffs.extend(s.missing_in_mod)
            all_diffs.extend(s.extra_in_mod)

            if all_diffs:
                parts.append(_diff_table_html(all_diffs))

            parts.append("</details>")

        parts.append("</div>")

    parts.append(_HTML_TEMPLATE_FOOT)
    return "\n".join(parts)


# ---------------------------------------------------------------------------
# Batch mode
# ---------------------------------------------------------------------------

def _find_monolithic_yml(model_dir: Path) -> Path | None:
    """Find the monolithic YAML file for a model directory.

    Looks in:
    1. model_dir/monolithic/*.yml
    2. Matching A01 examples by naming convention
    """
    mono_dir = model_dir / "monolithic"
    if mono_dir.exists():
        ymls = list(mono_dir.glob("*.yml"))
        if ymls:
            return ymls[0]
    return None


def _find_modular_dir(model_dir: Path) -> Path | None:
    """Find the modular output directory for a model.

    Looks in:
    1. model_dir/modular/ (with includes/ subdirectory)
    2. model_dir/analysis/modular/
    """
    for candidate in [model_dir / "modular", model_dir / "analysis" / "modular"]:
        includes = candidate / "includes"
        if includes.exists() and list(includes.glob("*.yml")):
            return candidate
    return None


def run_batch(
    batch_dir: Path,
    rtol: float,
    atol: float,
    sections_filter: list[str] | None = None,
) -> list[ValidationResult]:
    """Run validation across all model directories in a batch directory."""
    results: list[ValidationResult] = []

    model_dirs = sorted(
        d for d in batch_dir.iterdir()
        if d.is_dir() and not d.name.startswith(".") and d.name != "validation"
    )

    for model_dir in model_dirs:
        mono_path = _find_monolithic_yml(model_dir)
        mod_dir = _find_modular_dir(model_dir)

        if mono_path is None or mod_dir is None:
            # Skip models without both monolithic and modular
            if mono_path is None and mod_dir is None:
                continue
            print(f"  SKIP {model_dir.name}: ", end="")
            if mono_path is None:
                print("no monolithic YAML found")
            else:
                print("no modular output found")
            continue

        print(f"  Validating {model_dir.name}...")

        try:
            mono_data = load_monolithic(mono_path)
            mod_data = load_modular(mod_dir)

            sections = validate(mono_data, mod_data, rtol, atol, sections_filter)

            vr = ValidationResult(
                model_name=model_dir.name,
                monolithic_path=str(mono_path),
                modular_path=str(mod_dir),
                timestamp=datetime.now(tz=timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC"),
                sections=sections,
            )
            results.append(vr)

            diffs = vr.sections_with_diffs
            total = vr.total_sections
            status = "PASS" if diffs == 0 else f"{diffs}/{total} sections with diffs"
            print(f"    {status}")
        except Exception as e:
            print(f"    ERROR: {e}")

    return results


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Semantic validation of monolithic vs modular OrcaFlex YAML",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Single model (console)
  uv run python scripts/semantic_validate.py mono.yml modular_dir/

  # With HTML report
  uv run python scripts/semantic_validate.py mono.yml modular_dir/ --html report.html

  # JSON output
  uv run python scripts/semantic_validate.py mono.yml modular_dir/ --json

  # Batch mode
  uv run python scripts/semantic_validate.py --batch models_dir/ --batch-report report.html

  # Filter to specific sections
  uv run python scripts/semantic_validate.py mono.yml modular_dir/ --sections Environment LineTypes
        """,
    )

    # Single-model arguments
    parser.add_argument(
        "monolithic",
        nargs="?",
        help="Path to monolithic .yml file",
    )
    parser.add_argument(
        "modular",
        nargs="?",
        help="Path to modular output dir (containing includes/)",
    )

    # Output format
    parser.add_argument(
        "--json",
        action="store_true",
        help="Output JSON instead of text",
    )
    parser.add_argument(
        "--html",
        type=str,
        default=None,
        help="Generate HTML report at specified path",
    )

    # Tolerances
    parser.add_argument(
        "--rtol",
        type=float,
        default=1e-6,
        help="Relative tolerance for numeric comparison (default: 1e-6)",
    )
    parser.add_argument(
        "--atol",
        type=float,
        default=1e-10,
        help="Absolute tolerance for near-zero values (default: 1e-10)",
    )

    # Filtering
    parser.add_argument(
        "--ignore-order",
        action="store_true",
        default=True,
        help="Ignore property key ordering (default: True)",
    )
    parser.add_argument(
        "--sections",
        nargs="*",
        default=None,
        help="Only compare these sections (e.g. Environment LineTypes)",
    )

    # Batch mode
    parser.add_argument(
        "--batch",
        type=str,
        default=None,
        help="Path to models dir for batch validation",
    )
    parser.add_argument(
        "--batch-report",
        type=str,
        default=None,
        help="Generate aggregate HTML report for batch mode",
    )

    # Misc
    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable ANSI color in console output",
    )

    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    # Batch mode
    if args.batch:
        batch_dir = Path(args.batch)
        if not batch_dir.exists():
            print(f"ERROR: Batch directory not found: {batch_dir}", file=sys.stderr)
            return 1

        print(f"Batch validation: {batch_dir}")
        results = run_batch(batch_dir, args.rtol, args.atol, args.sections)

        if not results:
            print("No models found with both monolithic and modular YAML.")
            return 0

        # Summary
        pass_count = sum(1 for r in results if r.sections_with_significant_diffs == 0)
        total = len(results)
        print(f"\nBatch complete: {pass_count}/{total} models fully match")

        if args.batch_report:
            html = generate_batch_html(results)
            out_path = Path(args.batch_report)
            out_path.write_text(html, encoding="utf-8")
            print(f"Batch report: {out_path}")

        if args.json:
            output = {
                "batch": str(batch_dir),
                "models": [to_json(r) for r in results],
                "summary": {
                    "total_models": total,
                    "models_passing": pass_count,
                    "models_with_diffs": total - pass_count,
                },
            }
            print(json.dumps(output, indent=2, default=str))

        return 0 if pass_count == total else 1

    # Single-model mode
    if not args.monolithic or not args.modular:
        parser.error("monolithic and modular arguments required (or use --batch)")

    mono_path = Path(args.monolithic)
    mod_path = Path(args.modular)

    if not mono_path.exists():
        print(f"ERROR: Monolithic file not found: {mono_path}", file=sys.stderr)
        return 1

    if not mod_path.exists():
        print(f"ERROR: Modular directory not found: {mod_path}", file=sys.stderr)
        return 1

    # Load data
    mono_data = load_monolithic(mono_path)
    mod_data = load_modular(mod_path)

    # Run validation
    sections = validate(mono_data, mod_data, args.rtol, args.atol, args.sections)

    # Derive model name from monolithic filename
    model_name = mono_path.stem

    result = ValidationResult(
        model_name=model_name,
        monolithic_path=str(mono_path),
        modular_path=str(mod_path),
        timestamp=datetime.now(tz=timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC"),
        sections=sections,
    )

    # Output
    if args.json:
        print(json.dumps(to_json(result), indent=2, default=str))
    elif args.html:
        html = generate_html(result)
        out_path = Path(args.html)
        out_path.write_text(html, encoding="utf-8")
        print(f"HTML report written to: {out_path}")
        # Also print a brief summary
        print(f"\n{result.total_sections} sections compared, {result.sections_with_diffs} with differences")
    else:
        use_color = not args.no_color and sys.stdout.isatty()
        print_console(result, use_color=use_color)

    return 0 if result.sections_with_significant_diffs == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
