#!/usr/bin/env python3
"""Extract OrcaFlex property inventory from code registries and spec.yml files.

Generates a machine-readable YAML inventory of all OrcaFlex object types,
their properties, classification (priority/skip/typed), and usage statistics
from the spec library.

Usage:
    uv run python scripts/extract_property_inventory.py [--output PATH]
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Any

# Ensure project root is on path
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "src"))


def load_registries() -> dict[str, Any]:
    """Import and return all code registries as a structured dict."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import (
        FIELD_TO_SECTION,
        SECTION_REGISTRY,
        SINGLETON_SECTIONS,
        TYPED_FIELD_MAP,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
        _PRIORITY_KEYS,
        _SECTION_ORDER,
        _SKIP_GENERAL_KEYS,
        _SKIP_OBJECT_KEYS,
    )
    from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
        EnvironmentBuilder,
    )

    return {
        "section_registry": SECTION_REGISTRY,
        "field_to_section": FIELD_TO_SECTION,
        "typed_field_map": TYPED_FIELD_MAP,
        "singleton_sections": SINGLETON_SECTIONS,
        "priority_keys": _PRIORITY_KEYS,
        "section_order": _SECTION_ORDER,
        "skip_general_keys": _SKIP_GENERAL_KEYS,
        "skip_object_keys": _SKIP_OBJECT_KEYS,
        "wind_type_props": EnvironmentBuilder._WIND_TYPE_PROPS,
        "wind_speed_dormant": EnvironmentBuilder._WIND_SPEED_DORMANT,
        "safe_raw_overlay_keys": EnvironmentBuilder._SAFE_RAW_OVERLAY_KEYS,
    }


def extract_schema_fields(model_class: type) -> dict[str, dict[str, str]]:
    """Extract field info from a Pydantic model class.

    Returns dict of field_name -> {type, required, description}.
    """
    fields = {}
    if not hasattr(model_class, "model_fields"):
        return fields
    for name, field_info in model_class.model_fields.items():
        if name == "properties":
            continue  # Skip pass-through bag
        annotation = str(field_info.annotation) if field_info.annotation else "Any"
        # Clean up annotation string
        annotation = (
            annotation.replace("typing.", "")
            .replace("<class '", "")
            .replace("'>", "")
        )
        fields[name] = {
            "type": annotation,
            "required": field_info.is_required(),
            "description": field_info.description or "",
        }
    return fields


def scan_spec_library(spec_dir: Path) -> dict[str, dict[str, Any]]:
    """Walk spec.yml files and collect property usage per OrcaFlex section.

    Returns dict of section_name -> {spec_count, common_properties}.
    """
    import yaml

    usage: dict[str, dict[str, int]] = {}
    spec_count: dict[str, int] = {}

    spec_files = list(spec_dir.rglob("spec.yml")) + list(
        spec_dir.rglob("spec.yaml")
    )

    for spec_file in spec_files:
        try:
            with open(spec_file, encoding="utf-8") as f:
                content = f.read()
            # Handle multi-document YAML
            docs = list(yaml.safe_load_all(content))
            for doc in docs:
                if not isinstance(doc, dict):
                    continue
                # Look for generic.* fields that map to OrcaFlex sections
                generic = doc.get("generic", {})
                if not isinstance(generic, dict):
                    continue
                for field_name, items in generic.items():
                    if not isinstance(items, list):
                        continue
                    # Map field name to section name
                    section = _field_to_section_name(field_name)
                    if not section:
                        continue
                    spec_count[section] = spec_count.get(section, 0) + 1
                    for item in items:
                        if not isinstance(item, dict):
                            continue
                        props = item.get("properties", {})
                        if isinstance(props, dict):
                            for prop_key in props:
                                usage.setdefault(section, {})
                                usage[section][prop_key] = (
                                    usage[section].get(prop_key, 0) + 1
                                )
        except Exception:
            continue

    result = {}
    for section in set(list(usage.keys()) + list(spec_count.keys())):
        prop_counts = usage.get(section, {})
        # Sort properties by frequency
        common = sorted(
            prop_counts.keys(), key=lambda k: prop_counts[k], reverse=True
        )[:10]
        result[section] = {
            "spec_count": spec_count.get(section, 0),
            "common_properties": common,
        }
    return result


def _field_to_section_name(field_name: str) -> str | None:
    """Map a spec field name to OrcaFlex section name."""
    mapping = {
        "line_types": "LineTypes",
        "vessel_types": "VesselTypes",
        "vessels": "Vessels",
        "lines": "Lines",
        "buoys_6d": "6DBuoys",
        "buoys_3d": "3DBuoys",
        "shapes": "Shapes",
        "constraints": "Constraints",
        "links": "Links",
        "winches": "Winches",
        "clump_types": "ClumpTypes",
        "wing_types": "WingTypes",
        "flex_joint_types": "FlexJointTypes",
        "flex_joints": "FlexJoints",
        "drag_chain_types": "DragChainTypes",
        "drag_chains": "DragChains",
        "stiffener_types": "StiffenerTypes",
        "support_types": "SupportTypes",
        "morison_element_types": "MorisonElementTypes",
        "turbines": "Turbines",
        "attached_buoys": "AttachedBuoys",
        "variable_data_sources": "VariableData",
        "expansion_tables": "ExpansionTables",
        "py_models": "PyModels",
        "wake_models": "WakeModels",
        "multibody_groups": "MultibodyGroups",
        "browser_groups": "BrowserGroups",
    }
    return mapping.get(field_name)


def build_dormancy_rules(registries: dict[str, Any]) -> dict[str, dict[str, str]]:
    """Build dormancy rules from environment builder metadata."""
    rules: dict[str, dict[str, str]] = {}

    # Wind-type dependent properties
    wind_type_props = registries["wind_type_props"]
    for wind_type, props in wind_type_props.items():
        for prop in props:
            rules[prop] = {"dormant_when": f"WindType != {wind_type}"}

    # WindSpeed dormant for specific wind types
    for wind_type in registries["wind_speed_dormant"]:
        rules["WindSpeed"] = {"dormant_when": f"WindType == {wind_type}"}

    # Known dormancy from builder skip lists
    rules["WaveGamma"] = {"dormant_when": "WaveJONSWAPParameters == Automatic"}
    rules["CurrentExponent"] = {
        "dormant_when": "VerticalCurrentVariationMethod != Power law"
    }
    rules["CurrentSpeedAtSurface"] = {
        "dormant_when": "VerticalCurrentVariationMethod != Power law"
    }
    rules["CurrentSpeedAtSeabed"] = {
        "dormant_when": "VerticalCurrentVariationMethod != Power law"
    }

    return rules


def build_inventory(
    registries: dict[str, Any], usage: dict[str, dict[str, Any]]
) -> dict[str, Any]:
    """Build the complete inventory structure."""
    object_types = {}

    for section_name, (model_class, is_list) in registries[
        "section_registry"
    ].items():
        schema_fields = extract_schema_fields(model_class)

        # Map typed fields to OrcaFlex keys
        typed_fields = {}
        typed_field_map = registries["typed_field_map"]
        for py_field, ofx_key in typed_field_map.items():
            if py_field in schema_fields:
                typed_fields[py_field] = {
                    "orcaflex_key": ofx_key,
                    **schema_fields[py_field],
                }

        # Classify priority and skip keys for this section
        priority = [
            k
            for k in registries["priority_keys"]
            if k in [v for v in typed_field_map.values()] or k in ("Name",)
        ]

        skip = sorted(registries["skip_object_keys"])

        section_usage = usage.get(section_name, {})

        object_types[section_name] = {
            "section_type": "list" if is_list else "singleton",
            "schema_class": model_class.__name__,
            "typed_fields": typed_fields if typed_fields else None,
            "priority_keys": priority if priority else None,
            "skip_keys": skip if skip else None,
            "usage": section_usage if section_usage else None,
        }

    # Add singleton sections
    for section_name, field_name in registries["singleton_sections"].items():
        object_types[section_name] = {
            "section_type": "singleton",
            "field_name": field_name,
        }

    dormancy_rules = build_dormancy_rules(registries)

    return {
        "object_types": object_types,
        "dormancy_rules": dormancy_rules,
        "metadata": {
            "section_count": len(registries["section_registry"]),
            "singleton_count": len(registries["singleton_sections"]),
            "typed_field_count": len(registries["typed_field_map"]),
            "priority_key_count": len(registries["priority_keys"]),
            "skip_general_key_count": len(registries["skip_general_keys"]),
            "skip_object_key_count": len(registries["skip_object_keys"]),
            "section_order": registries["section_order"],
        },
    }


def write_yaml(inventory: dict[str, Any], output_path: Path) -> None:
    """Write inventory to YAML file."""
    import yaml

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as f:
        yaml.dump(
            inventory, f, default_flow_style=False, sort_keys=False, width=120
        )
    print(f"Inventory written to {output_path}")


def main() -> None:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Extract OrcaFlex property inventory"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=PROJECT_ROOT
        / "docs"
        / "modules"
        / "orcaflex"
        / "property_routing"
        / "object_inventory.yaml",
        help="Output YAML path",
    )
    parser.add_argument(
        "--spec-dir",
        type=Path,
        default=PROJECT_ROOT / "docs" / "modules" / "orcaflex" / "library",
        help="Spec library directory to scan",
    )
    args = parser.parse_args()

    print("Loading code registries...")
    registries = load_registries()

    print(f"Scanning spec library at {args.spec_dir}...")
    usage = scan_spec_library(args.spec_dir) if args.spec_dir.exists() else {}

    print("Building inventory...")
    inventory = build_inventory(registries, usage)

    write_yaml(inventory, args.output)

    # Summary
    ot = inventory["object_types"]
    dr = inventory["dormancy_rules"]
    print(f"\nSummary:")
    print(f"  Object types: {len(ot)}")
    print(f"  Dormancy rules: {len(dr)}")
    print(
        f"  Spec files scanned: "
        f"{sum(u.get('spec_count', 0) for u in usage.values() if isinstance(u, dict))}"
    )


if __name__ == "__main__":
    main()
