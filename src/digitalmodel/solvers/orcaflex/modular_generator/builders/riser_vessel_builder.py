"""Builder for the Vessels section of OrcaFlex riser models.

Generates vessel definitions for riser production models, typically
an FPSO or floating platform for top-side connection.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("06_riser_vessels.yml", order=42)
class RiserVesselBuilder(BaseBuilder):
    """Builds the Vessels section for riser models.

    Generates vessel with motion properties for riser top connection.
    Unlike S-lay vessels, riser vessels typically use RAO-based motion.
    """

    def should_generate(self) -> bool:
        """Only generate for riser models."""
        return self.spec.is_riser()

    def build(self) -> dict[str, Any]:
        """Build Vessels section from riser vessel configuration.

        Returns:
            Dictionary with 'Vessels' key containing vessel definitions.
        """
        riser = self.spec.riser
        vessel = riser.vessel

        vessel_def = {
            "Name": vessel.name,
            "VesselType": vessel.type_name,
            "Connection": "Free",
            "InitialPosition": vessel.position,
            "InitialHeading": vessel.orientation[2] if vessel.orientation else 0,
            "PrimaryMotion": vessel.primary_motion,
            "SuperimposedMotion": vessel.superimposed_motion,
            "IncludedInStatics": vessel.included_in_statics,
        }

        # Add length if specified
        if vessel.length:
            vessel_def["Length"] = vessel.length

        # Register vessel name for downstream builders
        self._register_entity("riser_vessel_name", vessel.name)
        self._register_entity("vessel_names", [vessel.name])
        self._register_entity("main_vessel_name", vessel.name)

        return {"Vessels": [vessel_def]}
