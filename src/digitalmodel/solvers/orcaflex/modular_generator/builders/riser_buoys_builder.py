"""Builder for lumped 6D buoys on riser models (#3974).

Buoy-supported risers, hybrid riser towers and arch (lazy-S) configurations
hang riser lines from a subsurface buoy. The buoy is emitted as an OrcaFlex
``6DBuoys`` entry of type ``Lumped buoy``; data names follow the public
example "A02 Lazy S simple" (``includes/08_buoys.yml``). It is written after
the vessel (a buoy may be connected to it) and before the riser lines and
links that connect to the buoy.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("06_riser_buoys.yml", order=43)
class RiserBuoysBuilder(BaseBuilder):
    """Builds the 6DBuoys section for riser models with typed buoys."""

    # Object sections emitted by build(); read by
    # writers.basefile.object_section_order() to derive a section
    # dependency order from this builder's registered order.
    _sections = (
        "6DBuoys",
    )

    def should_generate(self) -> bool:
        """Only generate for riser models that define buoys."""
        return self.spec.is_riser() and len(self.spec.riser.buoys) > 0

    def build(self) -> dict[str, Any]:
        buoys = [self._build_buoy(buoy) for buoy in self.spec.riser.buoys]
        names = [buoy.name for buoy in self.spec.riser.buoys]
        self._register_entity("buoy_names_6d", names)
        return {"6DBuoys": buoys}

    @staticmethod
    def _build_buoy(buoy) -> dict[str, Any]:
        return {
            "Name": buoy.name,
            "BuoyType": "Lumped buoy",
            "Connection": buoy.connection,
            "InitialPosition": list(buoy.position),
            "InitialAttitude": list(buoy.attitude),
            "Mass": buoy.mass,
            "MomentsOfInertia": list(buoy.moments_of_inertia),
            "CentreOfMass": list(buoy.centre_of_mass),
            "Volume": buoy.volume,
            "Height": buoy.height,
            "CentreOfVolume": list(buoy.centre_of_volume),
            "DragArea": list(buoy.drag_area),
            "DragForceCoefficient": list(buoy.drag_coefficient),
            "AddedMassCoefficient": list(buoy.added_mass_coefficient),
        }
