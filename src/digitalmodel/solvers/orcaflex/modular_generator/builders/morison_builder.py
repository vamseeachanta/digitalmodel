"""Builder for the MorisonElementTypes section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("14_morison.yml", order=60)
class MorisonBuilder(BaseBuilder):
    """Builds the MorisonElementTypes section of the OrcaFlex model.

    The MorisonElementTypes section defines hydrodynamic element types for
    Morison-equation based loading calculations. Each type defines:
    - Drag coefficients (Cd) for normal and axial directions
    - Added mass coefficients (Ca) for normal and axial directions
    - Inertia coefficients (Cm)
    - Reference diameters for drag and hydrodynamic calculations

    Reference: 14_morison.yml in modular include format.
    """

    def should_generate(self) -> bool:
        """Only generate for pipeline models."""
        return self.spec.is_pipeline()

    def build(self) -> dict[str, Any]:
        """Build the MorisonElementTypes section.

        Creates default Morison element type definitions for offshore
        analysis. The default type has zero coefficients (no hydrodynamic
        loading) which is typical for subsea elements where Morison loads
        are handled by the line type definition.

        Returns:
            Dictionary with 'MorisonElementTypes' key containing definitions.
        """
        morison_types = []
        morison_type_names = []

        # Default Morison element type - zero coefficients
        # This is used when hydrodynamic loading is defined elsewhere
        morison_type1 = {
            "Name": "Morison element type1",
            "NormalDragDiameter": 0,
            "AxialDragDiameter": None,
            "Cd": [0, None, 0],
            "NormalHydrodynamicDiameter": 0,
            "AxialHydrodynamicDiameter": None,
            "Ca": [0, None, 0],
            "Cm": [None, None, None],
        }
        morison_types.append(morison_type1)
        morison_type_names.append("Morison element type1")

        # Register Morison type names for cross-builder reference
        self._register_entity("morison_type_names", morison_type_names)

        return {"MorisonElementTypes": morison_types}
