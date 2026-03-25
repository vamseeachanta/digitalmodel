"""Builder for the Vessels section of OrcaFlex models.

Generates vessel definitions for S-lay installation, including
stinger support path geometry derived from stinger configuration.
"""

import math
from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("06_vessels.yml", order=45)
class VesselBuilder(BaseBuilder):
    """Builds the Vessels section for S-lay installation models.

    Generates vessel with support path based on stinger geometry.
    The stinger is modeled as a curved support path extending from
    the vessel stern.
    """

    def should_generate(self) -> bool:
        """Only generate for S-lay models with vessel defined."""
        return self.spec.equipment.vessel is not None

    def build(self) -> dict[str, Any]:
        """Build Vessels section from vessel and stinger configuration.

        Returns:
            Dictionary with 'Vessels' key containing vessel definitions.
        """
        vessel = self.spec.equipment.vessel
        stinger = self.spec.equipment.stinger
        vessel_type_name = (
            self.context.vessel_type_names[0]
            if self.context.vessel_type_names
            else f"{vessel.name} Type"
        )

        vessel_def = {
            "Name": vessel.name,
            "VesselType": vessel_type_name,
            "Length": vessel.properties.loa,
            "Connection": "Free",
            "InitialPosition": vessel.position,
            "InitialHeading": vessel.orientation[2] if vessel.orientation else 0,
            "PrimaryMotion": "Calculated (6 DOF)",
            "SuperimposedMotion": "None",
            "IncludedInStatics": "None",
            "WaveCalculationMethod": "Specified by environment",
        }

        # Add stinger support path if stinger is defined
        if stinger:
            support_path = self._build_support_path(stinger, vessel.properties.loa)
            vessel_def.update(support_path)

        vessel_name = vessel.name

        # Register vessel name for downstream builders
        self._register_entity("vessel_names", [vessel_name])
        self._register_entity("main_vessel_name", vessel_name)

        return {"Vessels": [vessel_def]}

    def _build_support_path(self, stinger, vessel_loa: float) -> dict[str, Any]:
        """Build support path geometry from stinger configuration.

        The stinger support path is modeled as a series of points along
        the stinger curve, defined in vessel-local coordinates.

        Args:
            stinger: Stinger configuration object.
            vessel_loa: Vessel length overall for position reference.

        Returns:
            Dictionary with support path keys for OrcaFlex vessel definition.
        """
        radius = stinger.radius
        origin = stinger.origin_position

        # Generate support path points along the stinger curve
        # Arc from stinger origin, curving downward with given radius
        num_points = 20  # Number of discretization points
        total_arc = 0
        for section in stinger.sections:
            total_arc += section.length

        if total_arc == 0:
            total_arc = 40  # Default stinger length

        path_x = []
        path_z = []
        for i in range(num_points + 1):
            arc_frac = i / num_points
            theta = (arc_frac * total_arc) / radius
            # Stinger curves downward from stern
            x = origin[0] - radius * math.sin(theta)
            z = origin[2] - radius * (1 - math.cos(theta))
            path_x.append(round(x, 3))
            path_z.append(round(z, 3))

        # Build support positions from rollers if defined
        supports = []
        if stinger.rollers:
            for roller in stinger.rollers:
                theta = roller.arc_length / radius
                sx = origin[0] - radius * math.sin(theta)
                sz = origin[2] - radius * (1 - math.cos(theta)) + roller.z_offset
                supports.append({
                    "SupportType": roller.support_type,
                    "SupportPosition": [round(sx, 3), 0, round(sz, 3)],
                    "SupportOrientation": [0, 0, 0],
                })

        result = {
            "SupportGeometrySpecification": "Explicit",
        }

        if supports:
            result["Supports"] = supports

        return result
