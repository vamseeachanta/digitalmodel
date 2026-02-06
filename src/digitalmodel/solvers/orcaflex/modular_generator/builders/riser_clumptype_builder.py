"""Builder for ClumpType definitions used as inline attachments on riser lines.

ClumpTypes are simple mass/volume/drag attachments that share the parent
line's DOFs. Unlike 6DBuoys, they do NOT add extra degrees of freedom to
the statics solver, making them suitable for large numbers of attachments.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("04_riser_clump_types.yml", order=38)
class RiserClumpTypeBuilder(BaseBuilder):
    """Builds ClumpType definitions for riser inline attachments.

    Generates ClumpTypes entries for each clump type defined in the spec.
    These are used as attachment types on riser lines via the multi-column
    attachment format.

    Reference: OrcaFlex ClumpTypes documentation.
    """

    def should_generate(self) -> bool:
        """Only generate for riser models with clump types."""
        return self.spec.is_riser() and len(self.spec.riser.clump_types) > 0

    def build(self) -> dict[str, Any]:
        """Build the ClumpTypes section from riser clump type definitions.

        Returns:
            Dictionary with 'ClumpTypes' key containing list of clump type
            definitions in OrcaFlex format.
        """
        riser = self.spec.riser
        clump_types_list = []
        clump_type_names = []

        for ct in riser.clump_types:
            clump_def = self._build_clump_type(ct)
            clump_types_list.append(clump_def)
            clump_type_names.append(ct.name)

        # Register names for cross-builder reference
        self._register_entity("clump_type_names", clump_type_names)

        return {"ClumpTypes": clump_types_list}

    def _build_clump_type(self, ct) -> dict[str, Any]:
        """Build a single ClumpType definition.

        OrcaFlex ClumpTypes format uses 3-component arrays for directional
        properties: [normal, ~, axial] where ~ (None) means "not specified".

        Args:
            ct: ClumpType object from spec.

        Returns:
            Dictionary representing an OrcaFlex ClumpType entry.
        """
        clump_def = {
            "Name": ct.name,
            "Mass": ct.mass,
            "Volume": ct.volume,
            "Height": ct.height,
            "AlignWith": "Line axes",
        }

        # Add drag area if specified [normal, ~, axial]
        if ct.drag_area:
            clump_def["DragArea"] = [ct.drag_area[0], None, ct.drag_area[1]]

        # Add drag coefficients [Cd_normal, ~, Cd_axial]
        if ct.drag_coefficient:
            clump_def["Cd"] = [ct.drag_coefficient[0], None, ct.drag_coefficient[1]]

        # Add added mass coefficients [Ca_normal, ~, Ca_axial]
        if ct.added_mass_coefficient:
            clump_def["Ca"] = [
                ct.added_mass_coefficient[0],
                None,
                ct.added_mass_coefficient[1],
            ]

        return clump_def
