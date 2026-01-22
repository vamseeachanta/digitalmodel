"""Builder for the Shapes section of OrcaFlex models.

This builder generates Shape definitions for seabed ramps and geometry
including Block and Curved plate shapes with stiffness properties.
"""

from typing import Any

from .base import BaseBuilder


# Default stiffness values for shapes
DEFAULT_STIFFNESS = {
    "NormalStiffness": 10e3,  # kN/m/m2
    "ShearStiffness": 100,  # kN/m/m2
}


class ShapesBuilder(BaseBuilder):
    """Builds the Shapes section of the OrcaFlex model.

    Generates Shape definitions for:
    - Block shapes (rectangular solids for inclined ramps)
    - Curved plate shapes (for smooth pipe routing)
    - Custom shapes from ramp specifications

    Each shape includes:
    - Position and orientation
    - Dimensions (size or profile)
    - Contact stiffness properties

    Reference: 09_shapes.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the Shapes section from equipment ramp configuration.

        Returns:
            Dictionary with 'Shapes' key containing list of shape definitions.
        """
        shapes_list = []
        shape_names = []

        # Build ramps from equipment configuration
        if self.spec.equipment.ramps:
            for ramp in self.spec.equipment.ramps:
                shape = self._build_shape_from_ramp(ramp)
                shapes_list.append(shape)
                shape_names.append(ramp.name)
        else:
            # Build default ramp shapes for installation model
            default_shapes = self._build_default_ramps()
            shapes_list.extend(default_shapes)
            shape_names.extend([s["Name"] for s in default_shapes])

        # Register shape names for cross-builder reference
        self._register_entity("shape_names", shape_names)

        return {"Shapes": shapes_list}

    def _build_shape_from_ramp(self, ramp) -> dict[str, Any]:
        """Build a shape definition from a Ramp specification.

        Args:
            ramp: Ramp object from spec.equipment.ramps

        Returns:
            Dictionary representing an OrcaFlex Shape definition.
        """
        shape_type = ramp.type.lower()

        if shape_type == "block":
            return self._build_block_shape(ramp)
        elif shape_type == "curved_plate":
            return self._build_curved_plate_shape(ramp)
        elif shape_type == "cylinder":
            return self._build_cylinder_shape(ramp)
        else:
            # Default to block shape
            return self._build_block_shape(ramp)

    def _build_block_shape(self, ramp) -> dict[str, Any]:
        """Build a block (rectangular solid) shape.

        Args:
            ramp: Ramp object with block parameters.

        Returns:
            Dictionary representing an OrcaFlex Block shape.
        """
        # Get dimensions from ramp or use defaults
        origin = ramp.origin or [0, 0, 0]
        size = ramp.size or [150, 50, 10]
        rotation = ramp.rotation or [0, 0, 0]

        return {
            "Name": ramp.name,
            "ShapeType": "Elastic solid",
            "Shape": "Block",
            "Connection": "Fixed",
            "Origin": origin,
            "Size": size,
            "Orientation": rotation,
            "NormalStiffness": DEFAULT_STIFFNESS["NormalStiffness"],
            "ShearStiffness": DEFAULT_STIFFNESS["ShearStiffness"],
        }

    def _build_curved_plate_shape(self, ramp) -> dict[str, Any]:
        """Build a curved plate shape.

        Args:
            ramp: Ramp object with curved plate parameters.

        Returns:
            Dictionary representing an OrcaFlex Curved plate shape.
        """
        origin = ramp.origin or [0, 0, 0]
        rotation = ramp.rotation or [90, 90, 0]

        # Default profile for curved ramp (semicircular cross-section)
        default_profile = [
            [0, 1000],  # Start: [distance_along_axis, diameter]
            [50, 1000],  # End
        ]

        return {
            "Name": ramp.name,
            "ShapeType": "Elastic solid",
            "Shape": "Curved plate",
            "Connection": "Fixed",
            "Origin": origin,
            "Orientation": rotation,
            "IncludedAngleOfRevolution": 10,
            "IsHollow": "No",
            "ProfileDistanceAlongAxis, ProfileDiameter": default_profile,
            "NormalStiffness": DEFAULT_STIFFNESS["NormalStiffness"],
            "ShearStiffness": DEFAULT_STIFFNESS["ShearStiffness"],
        }

    def _build_cylinder_shape(self, ramp) -> dict[str, Any]:
        """Build a cylinder shape.

        Args:
            ramp: Ramp object with cylinder parameters.

        Returns:
            Dictionary representing an OrcaFlex Cylinder shape.
        """
        origin = ramp.origin or [0, 0, 0]
        rotation = ramp.rotation or [0, 0, 0]

        # Extract cylinder dimensions from size if provided
        size = ramp.size or [10, 5, 5]  # [length, diameter, diameter]
        length = size[0]
        diameter = size[1]

        return {
            "Name": ramp.name,
            "ShapeType": "Elastic solid",
            "Shape": "Cylinder",
            "Connection": "Fixed",
            "Origin": origin,
            "Orientation": rotation,
            "CylinderOuterDiameter": diameter,
            "CylinderInnerDiameter": 0,
            "CylinderLength": length,
            "NormalStiffness": DEFAULT_STIFFNESS["NormalStiffness"],
            "ShearStiffness": DEFAULT_STIFFNESS["ShearStiffness"],
        }

    def _build_default_ramps(self) -> list[dict[str, Any]]:
        """Build default ramp shapes for floating pipeline installation.

        Creates:
        - Ramp inclined: Inclined block at seabed for pipe laydown
        - Ramp-curve: Curved section for smooth transition

        Returns:
            List of default shape definitions.
        """
        seabed_slope = self.spec.environment.seabed.slope

        # Calculate ramp orientation based on seabed slope
        ramp_angle = seabed_slope if seabed_slope != 0 else 2.27

        shapes = [
            # Inclined ramp block at pipe start
            {
                "Name": "Ramp inclined",
                "ShapeType": "Elastic solid",
                "Shape": "Block",
                "Connection": "Fixed",
                "Origin": [-126.35, -25, -5],
                "Size": [150, 50, 10],
                "Orientation": [0, ramp_angle, 0],
                "NormalStiffness": DEFAULT_STIFFNESS["NormalStiffness"],
                "ShearStiffness": DEFAULT_STIFFNESS["ShearStiffness"],
            },
            # Curved transition section
            {
                "Name": "Ramp-curve",
                "ShapeType": "Elastic solid",
                "Shape": "Curved plate",
                "Connection": "Fixed",
                "Origin": [4.1, -25, -500.55],
                "Orientation": [90, 90, 182.27],
                "IncludedAngleOfRevolution": 10,
                "IsHollow": "No",
                "ProfileDistanceAlongAxis, ProfileDiameter": [
                    [0, 1000],
                    [50, 1000],
                ],
                "NormalStiffness": DEFAULT_STIFFNESS["NormalStiffness"],
                "ShearStiffness": DEFAULT_STIFFNESS["ShearStiffness"],
            },
        ]

        return shapes

    def _calculate_ramp_position(self) -> tuple[list[float], list[float]]:
        """Calculate ramp position based on environment and pipeline configuration.

        Returns:
            Tuple of (origin, orientation) for the main ramp.
        """
        water_depth = self.spec.environment.water.depth
        seabed_slope = self.spec.environment.seabed.slope

        # Position ramp before pipeline start
        origin = [
            -126.35,  # X: Before pipeline origin
            -25,  # Y: Centered on pipeline path
            -water_depth + 5,  # Z: Near seabed
        ]

        # Orientation aligned with seabed slope
        orientation = [0, seabed_slope, 0]

        return origin, orientation
