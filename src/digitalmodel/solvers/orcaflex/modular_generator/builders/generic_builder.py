"""Builder for generic OrcaFlex model objects.

This builder handles generic (non-pipeline, non-riser) OrcaFlex models.
It iterates all fields in the GenericModel spec and emits the corresponding
OrcaFlex YAML sections, using typed fields for known properties and the
properties dict for pass-through values.
"""

from __future__ import annotations

from typing import Any

from ..schema.generic import (
    FIELD_TO_SECTION,
    SINGLETON_SECTIONS,
    TYPED_FIELD_MAP,
    GenericObject,
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

        return result

    @staticmethod
    def _merge_object(obj: GenericObject) -> dict[str, Any]:
        """Merge typed fields and pass-through properties into a single dict.

        Strategy:
        1. Start from the properties dict (pass-through values).
        2. Overlay typed fields using TYPED_FIELD_MAP for key translation.
        3. Only include typed fields whose value is not None.
        4. Typed fields take priority over properties on key conflict.

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

        return merged
