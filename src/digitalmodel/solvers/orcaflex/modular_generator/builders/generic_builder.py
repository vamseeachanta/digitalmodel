"""Builder for generic OrcaFlex model objects.

This builder handles generic (non-pipeline, non-riser) OrcaFlex models.
It iterates all fields in the GenericModel spec and emits the corresponding
OrcaFlex YAML sections, using typed fields for known properties and the
properties dict for pass-through values.

OrcaFlex loads YAML sections sequentially and validates references as it
goes. The builder must therefore emit sections in dependency order so that
definitions (LineTypes, ClumpTypes, StiffenerTypes, SupportTypes, etc.)
precede objects that reference them (Lines, Shapes, etc.).
"""

from __future__ import annotations

from typing import Any

from ..schema.generic import (
    FIELD_TO_SECTION,
    SINGLETON_SECTIONS,
    TYPED_FIELD_MAP,
    GenericObject,
    GenericVariableData,
)
from .base import BaseBuilder
from .registry import BuilderRegistry

# Map spec field name -> context registration key for known entity types.
# Only fields that have a corresponding attribute on BuilderContext are listed.
_FIELD_TO_CONTEXT_KEY: dict[str, str] = {
    "line_types": "line_type_names",
    "vessel_types": "vessel_type_names",
    "vessels": "vessel_names",
    "lines": "line_names",
    "buoys_6d": "buoy_names_6d",
    "buoys_3d": "buoy_names_3d",
    "shapes": "shape_names",
    "winches": "winch_names",
}

# OrcaFlex YAML section dependency order.  Sections appearing earlier in this
# list are emitted first so that definitions are available before references.
# Derived from monolithic model.SaveData() output order.
_SECTION_ORDER: list[str] = [
    "General",
    "VariableData",
    "ExpansionTables",
    # Singleton sections that define named references (must precede objects)
    "RayleighDampingCoefficients",
    "SolidFrictionCoefficients",
    "LineContactData",
    "CodeChecks",
    "Shear7Data",
    "VIVAData",
    # Type definitions (must precede instances)
    "LineTypes",
    "VesselTypes",
    "ClumpTypes",
    "WingTypes",
    "FlexJointTypes",
    "DragChainTypes",
    "StiffenerTypes",
    "SupportTypes",
    "MorisonElementTypes",
    # Object instances
    "PyModels",
    "WakeModels",
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
    "MultibodyGroups",
    "BrowserGroups",
]

# OrcaFlex properties that must appear before other properties within an
# object dict.  These "mode" properties change the available property set,
# so they must be set first.  Order matters within this list too.
_PRIORITY_KEYS: list[str] = [
    "Name",
    "Category",        # LineTypes: General vs Homogeneous pipe
    "ShapeType",       # Shapes: Drawing, Elastic solid, etc.
    "Shape",           # Shapes: Block, Cylinder, etc. (controls Size semantics)
    "BuoyType",        # 6DBuoys: Spar buoy, Lumped buoy, etc.
    "Connection",      # Various: affects available sub-properties
    "LinkType",        # Links: Tether, Spring/damper, etc.
    "Geometry",        # SupportTypes: U shaped, etc.
    "WaveType",        # Environment wave trains
    "DegreesOfFreedomInStatics",  # 6DBuoys: must precede stiffness props
]


@BuilderRegistry.register("20_generic_objects.yml", order=200)
class GenericModelBuilder(BaseBuilder):
    """Builds all OrcaFlex object sections for generic models.

    Iterates the GenericModel specification and emits:
    - List sections (LineTypes, Vessels, Lines, etc.) as lists of dicts
    - Singleton sections (SolidFrictionCoefficients, etc.) as flat dicts
    - General section overrides merged into a "General" key

    Each list object is merged via ``_merge_object`` which combines typed
    fields (e.g. ``name`` -> ``Name``) with the pass-through ``properties``
    dict, giving typed fields priority on conflict.

    Reference: OrcaFlex data model documentation.
    """

    def should_generate(self) -> bool:
        """Only generate for generic models."""
        return self.spec.is_generic()

    def build(self) -> dict[str, Any]:
        """Build all OrcaFlex sections from the generic model spec.

        Returns:
            Dictionary keyed by OrcaFlex section name (e.g. "LineTypes",
            "Vessels", "SolidFrictionCoefficients", "General") with the
            corresponding list-of-dicts or flat dict values.
        """
        generic = self.spec.generic
        result: dict[str, Any] = {}

        # Process list-based object sections
        for field_name, section_key in FIELD_TO_SECTION.items():
            # VariableData needs special nested-by-category handling
            if field_name == "variable_data_sources":
                continue

            objects: list[GenericObject] = getattr(generic, field_name, [])
            if not objects:
                continue

            merged = [self._merge_object(obj) for obj in objects]
            result[section_key] = merged

            # Register entity names into context where applicable
            context_key = _FIELD_TO_CONTEXT_KEY.get(field_name)
            if context_key is not None:
                names = [obj.name for obj in objects]
                self._register_entity(context_key, names)

        # VariableData: group by data_type into nested sub-categories
        # OrcaFlex expects: VariableData: { Dragcoefficient: [...], ... }
        if generic.variable_data_sources:
            var_data = self._build_variable_data(generic.variable_data_sources)
            if var_data:
                result["VariableData"] = var_data

        # Process singleton sections
        for section_key, field_name in SINGLETON_SECTIONS.items():
            singleton = getattr(generic, field_name, None)
            if singleton is None:
                continue
            if singleton.data:
                result[section_key] = dict(singleton.data)

        # Merge general_properties into "General" if non-empty
        if generic.general_properties:
            result["General"] = dict(generic.general_properties)

        return self._order_sections(result)

    @staticmethod
    def _order_sections(result: dict[str, Any]) -> dict[str, Any]:
        """Re-order result dict to match OrcaFlex section dependency order.

        Sections listed in ``_SECTION_ORDER`` are emitted first (in that
        order).  Any unlisted sections are appended at the end.
        """
        ordered: dict[str, Any] = {}
        for key in _SECTION_ORDER:
            if key in result:
                ordered[key] = result[key]
        # Append any sections not in the canonical order
        for key, val in result.items():
            if key not in ordered:
                ordered[key] = val
        return ordered

    @staticmethod
    def _build_variable_data(
        sources: list[GenericVariableData],
    ) -> dict[str, list[dict[str, Any]]]:
        """Build VariableData section grouped by data_type sub-category.

        OrcaFlex expects VariableData as a nested dict where keys are
        category names (e.g. "Dragcoefficient", "Linetypediameter") and
        values are lists of named entries.

        Args:
            sources: Flat list of GenericVariableData with data_type fields.

        Returns:
            Nested dict: {"Dragcoefficient": [{Name: ..., ...}], ...}
        """
        categories: dict[str, list[dict[str, Any]]] = {}
        for src in sources:
            category = src.data_type or "Unknown"
            entry: dict[str, Any] = dict(src.properties)
            entry["Name"] = src.name
            categories.setdefault(category, []).append(entry)
        return categories

    @staticmethod
    def _merge_object(obj: GenericObject) -> dict[str, Any]:
        """Merge typed fields and pass-through properties into a single dict.

        Strategy:
        1. Start from the properties dict (pass-through values).
        2. Overlay typed fields using TYPED_FIELD_MAP for key translation.
        3. Only include typed fields whose value is not None.
        4. Typed fields take priority over properties on key conflict.
        5. Re-order so priority keys (Name, Category) appear first.

        Args:
            obj: A GenericObject (or subclass) instance.

        Returns:
            Flat dictionary suitable for OrcaFlex YAML output.
        """
        merged: dict[str, Any] = dict(obj.properties)

        for py_field, ofx_key in TYPED_FIELD_MAP.items():
            value = getattr(obj, py_field, None)
            if value is not None:
                merged[ofx_key] = value

        # Ensure priority keys come first (e.g. Category before MaterialDensity)
        ordered: dict[str, Any] = {}
        for key in _PRIORITY_KEYS:
            if key in merged:
                ordered[key] = merged.pop(key)
        ordered.update(merged)
        return ordered
