"""Builder for the Groups section of OrcaFlex models.

This builder generates Groups organizing all model entities into
logical collections for easier model management and results processing.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("10_groups.yml", order=100)
class GroupsBuilder(BaseBuilder):
    """Builds the Groups section of the OrcaFlex model.

    Generates Groups that organize entities by category:
    - Structure: All physical entities (lines, buoys, shapes)
    - State: Collapsed/expanded view state for UI

    Each entity is mapped to 'Model' to indicate it belongs
    to the model hierarchy.

    Reference: 10_groups.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the Groups section from context entity names.

        Collects all entity names from context (populated by previous
        builders) and organizes them into the Structure group.

        Returns:
            Dictionary with 'Groups' key containing group definitions.
        """
        structure_group = {}

        # Collect line names from context
        line_names = self.context.line_names
        for name in line_names:
            structure_group[name] = "Model"

        # Collect 6D buoy names from context
        buoy_names_6d = self.context.buoy_names_6d
        for name in buoy_names_6d:
            structure_group[name] = "Model"

        # Collect 3D buoy names from context
        buoy_names_3d = self.context.buoy_names_3d
        for name in buoy_names_3d:
            structure_group[name] = "Model"

        # Collect shape names from context
        shape_names = self.context.shape_names
        for name in shape_names:
            structure_group[name] = "Model"

        # Add any additional entities that might have been registered
        additional_lines = self.context.additional_line_names
        for name in additional_lines:
            structure_group[name] = "Model"

        # Collect vessel names from context
        vessel_names = self.context.vessel_names
        for name in vessel_names:
            structure_group[name] = "Model"

        # Collect winch names from context
        winch_names = self.context.winch_names
        for name in winch_names:
            structure_group[name] = "Model"

        # If no entities from context, build from spec directly
        if not structure_group:
            structure_group = self._build_from_spec()

        # Register group names for reference
        self._register_entity("group_names", ["Structure"])

        return {
            "Groups": {
                "Structure": structure_group,
                "State": {
                    "Collapsed": [],
                },
            }
        }

    def _build_from_spec(self) -> dict[str, str]:
        """Build structure group directly from spec when context is empty.

        This is a fallback method used when the builder is run without
        prior context population from other builders.

        Returns:
            Dictionary mapping entity names to 'Model'.
        """
        structure_group = {}

        # Add pipeline line
        structure_group[self.spec.pipeline.name] = "Model"

        # Add equipment entities
        equipment = self.spec.equipment

        # Add tugs
        if equipment.tugs:
            for i in range(equipment.tugs.count):
                structure_group[f"Tug{i + 1}"] = "Model"

        # Add rollers
        if equipment.rollers:
            structure_group["Rollers"] = "Model"

        # Add buoyancy modules
        if equipment.buoyancy_modules:
            structure_group["BM"] = "Model"

        # Add ramps
        for ramp in equipment.ramps:
            structure_group[ramp.name] = "Model"

        # Add vessel (S-lay)
        if equipment.vessel:
            structure_group[equipment.vessel.name] = "Model"

        # Add tensioner (S-lay)
        if equipment.tensioner:
            structure_group[equipment.tensioner.name] = "Model"

        # Add default entities
        structure_group["6D buoy1"] = "Model"
        structure_group["Mid-pipe"] = "Model"

        # Add default ramps if none specified
        if not equipment.ramps:
            structure_group["Ramp inclined"] = "Model"
            structure_group["Ramp-curve"] = "Model"

        return structure_group

    def get_all_entity_names(self) -> list[str]:
        """Get list of all entity names that would be in the Structure group.

        Useful for validation and debugging.

        Returns:
            List of all entity names.
        """
        all_names = []

        # From context
        all_names.extend(self.context.line_names)
        all_names.extend(self.context.buoy_names_6d)
        all_names.extend(self.context.buoy_names_3d)
        all_names.extend(self.context.shape_names)
        all_names.extend(self.context.additional_line_names)
        all_names.extend(self.context.vessel_names)
        all_names.extend(self.context.winch_names)

        # Remove duplicates while preserving order
        seen = set()
        unique_names = []
        for name in all_names:
            if name not in seen:
                unique_names.append(name)
                seen.add(name)

        return unique_names
