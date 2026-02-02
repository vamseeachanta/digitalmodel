"""Builder for the VesselTypes section of OrcaFlex models.

Generates vessel type definitions for S-lay installation vessels,
including mass, inertia, and RAO reference data.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("04_vessel_types.yml", order=35)
class VesselTypeBuilder(BaseBuilder):
    """Builds the VesselTypes section for S-lay installation models.

    Generates vessel type entries with physical properties derived
    from the vessel specification. Only active for S-lay models
    (when vessel is defined in equipment).
    """

    def should_generate(self) -> bool:
        """Only generate for S-lay models with vessel defined."""
        return self.spec.equipment.vessel is not None

    def build(self) -> dict[str, Any]:
        """Build VesselTypes section from vessel properties.

        Returns:
            Dictionary with 'VesselTypes' key containing vessel type list.
        """
        vessel = self.spec.equipment.vessel
        props = vessel.properties

        # Calculate mass from displacement or estimate from dimensions
        displacement = props.displacement
        if displacement is None:
            # Rough estimate: LOA * beam * draft * block_coefficient * water_density
            displacement = props.loa * props.beam * props.draft * 0.7 * 1.025

        # Calculate moments of inertia from gyration radii
        rxx, ryy, rzz = props.gyration_radii
        ixx = displacement * rxx ** 2
        iyy = displacement * ryy ** 2
        izz = displacement * rzz ** 2

        vessel_type_name = f"{vessel.name} Type"

        vessel_type = {
            "Name": vessel_type_name,
            "Length": props.loa,
            "Draughts": [props.draft],
            "Symmetry": "xz plane",
            "CalculationMethod": "Single draught",
            "Displacement": [displacement],
            "WaveReferencePoint": [0, 0, 0],
            "CentreOfGravityRelativeToCoG": [0, 0, 0],
            "MomentsOfInertia": [[ixx, iyy, izz]],
        }

        # Register vessel type name for downstream builders
        self._register_entity("vessel_type_names", [vessel_type_name])

        return {"VesselTypes": [vessel_type]}
