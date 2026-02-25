"""Builder for the SupportTypes section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("13_supports.yml", order=50)
class SupportsBuilder(BaseBuilder):
    """Builds the SupportTypes section of the OrcaFlex model.

    The SupportTypes section defines support type properties used by lines
    for pipe-soil or pipe-structure interaction. Each support type defines:
    - Geometry (Flat, Curved, etc.)
    - Stiffness values (normal, shear)
    - Dimensions (diameter, length)
    - Visual properties (pen style)

    Reference: 13_supports.yml in modular include format.
    """

    def should_generate(self) -> bool:
        """Only generate for pipeline models."""
        return self.spec.is_pipeline()

    def build(self) -> dict[str, Any]:
        """Build the SupportTypes section.

        Creates support type definitions based on roller configuration
        from the equipment spec. Generates default support types for
        pipeline installation scenarios.

        Returns:
            Dictionary with 'SupportTypes' key containing support definitions.
        """
        support_types = []
        support_type_names = []

        # Default support type 1 - larger diameter for main supports
        support_type1 = {
            "Name": "Support type1",
            "Geometry": "Flat",
            "NormalStiffness": 10e3,
            "ShearStiffness": 100,
            "Diameter": 5,
            "FlatSupportLength": 10,
            "Pen": [2, "Solid", "$40FF00"],
        }
        support_types.append(support_type1)
        support_type_names.append("Support type1")

        # Default support type 2 - smaller diameter for roller supports
        support_type2 = {
            "Name": "Support type2",
            "Geometry": "Flat",
            "NormalStiffness": 10e3,
            "ShearStiffness": 100,
            "Diameter": 2,
            "FlatSupportLength": 10,
        }
        support_types.append(support_type2)
        support_type_names.append("Support type2")

        # Add custom support type from rollers config if specified
        if self.spec.equipment.rollers:
            rollers = self.spec.equipment.rollers
            if rollers.support_type and rollers.support_type not in support_type_names:
                custom_support = {
                    "Name": rollers.support_type,
                    "Geometry": "Flat",
                    "NormalStiffness": 10e3,
                    "ShearStiffness": 100,
                    "Diameter": 2,
                    "FlatSupportLength": 10,
                }
                support_types.append(custom_support)
                support_type_names.append(rollers.support_type)

        # Register support type names for cross-builder reference
        self._register_entity("support_type_names", support_type_names)

        return {"SupportTypes": support_types}
