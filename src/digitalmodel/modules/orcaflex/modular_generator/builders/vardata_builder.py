"""Builder for the VariableData section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("02_var_data.yml", order=20)
class VarDataBuilder(BaseBuilder):
    """Builds the VariableData section of the OrcaFlex model.

    The VariableData section contains coating/lining definitions that define
    additional layers on lines (pipes). Each coating entry specifies layer
    thickness and material density pairs.

    Coating names are generated as:
    - "coating" for corrosion coating only
    - "coating+CWC<thickness_mm>" for corrosion + concrete weight coating

    Reference: 02_var_data.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the VariableData section from pipeline coatings.

        Generates coating entries from spec.pipeline.coatings including:
        - Base corrosion coating entry
        - Combined entries for each weight coating variant

        Returns:
            Dictionary with 'VariableData' key containing coating definitions.
        """
        coatings = self.spec.pipeline.coatings
        corrosion = coatings.corrosion
        weight_coatings = coatings.weight

        coatings_list = []
        coating_names = []

        # Build combined coating entries (coating + weight coating)
        for wc in weight_coatings:
            # Generate name like "coating+CWC120" from weight coating name
            if wc.name:
                name = f"coating+{wc.name}"
            else:
                # Generate name from thickness in mm
                thickness_mm = int(wc.thickness * 1000)
                name = f"coating+CWC{thickness_mm}"

            # Layers: [corrosion_layer, weight_coating_layer]
            layers = [
                [corrosion.thickness, corrosion.density],
                [wc.thickness, wc.density],
            ]

            coating_entry = {
                "Name": name,
                "LayerThickness, LayerMaterialDensity": layers,
            }
            coatings_list.append(coating_entry)
            coating_names.append(name)

        # Add base corrosion coating entry (no weight coating)
        base_coating_name = "coating"
        base_coating_entry = {
            "Name": base_coating_name,
            "LayerThickness, LayerMaterialDensity": [
                [corrosion.thickness, corrosion.density],
            ],
        }
        coatings_list.append(base_coating_entry)
        coating_names.append(base_coating_name)

        # Register coating names for cross-builder reference
        self._register_entity("coating_names", coating_names)

        return {
            "VariableData": {
                "Coatingsorlinings": coatings_list,
            }
        }
