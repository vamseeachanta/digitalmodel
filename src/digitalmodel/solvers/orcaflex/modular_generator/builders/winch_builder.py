# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Builder for the Winches section of OrcaFlex models.

Generates winch definitions connecting vessel tensioner to pipeline
for S-lay installation operations with stage-controlled tension.
"""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("11_winches.yml", order=95)
class WinchBuilder(BaseBuilder):
    """Builds the Winches section for S-lay installation models.

    Generates winch definitions that model the tensioner connecting
    the vessel to the pipeline End A. Tension is applied via
    stage-controlled winch.
    """

    # Object sections emitted by build(); read by
    # writers.basefile.object_section_order() to derive a section
    # dependency order from this builder's registered order.
    _sections = (
        "Winches",
    )

    def should_generate(self) -> bool:
        """Only generate for S-lay models with tensioner defined."""
        return (
            self.spec.equipment.vessel is not None
            and self.spec.equipment.tensioner is not None
        )

    def build(self) -> dict[str, Any]:
        """Build Winches section from tensioner configuration.

        Returns:
            Dictionary with 'Winches' key containing winch definitions.
        """
        tensioner = self.spec.equipment.tensioner
        vessel = self.spec.equipment.vessel
        if tensioner is None or vessel is None:
            raise ValueError("Winch requires tensioner and vessel")
        pipeline_name = self.context.main_pipeline_name or self.spec.pipeline.name

        winch_name = tensioner.name

        # Tensioner position on vessel (default to stern area)
        origin = self.spec.equipment.stinger.origin_position if self.spec.equipment.stinger else [0, 0, 0]
        # Reuse the previous 10 m initial wire length as the forward lead.
        position = tensioner.position_on_vessel or [origin[0] + 10, origin[1], origin[2]]
        if position == origin:
            raise ValueError("Tensioner lead must be separated from the pipe upper end")

        winch_def: dict[str, Any] = {
            "Name": winch_name,
            "Connection": [vessel.name, pipeline_name],
            "ConnectionPoint": [position, [0, 0, 0]],
            "ConnectionzRelativeTo": [None, "End A"],
            "StageMode": [
                "Specified tension",  # Stage 0: build-up
                "Specified tension",  # Stage 1: operational
            ],
            "StageTension": [
                tensioner.tension_value,
                tensioner.tension_value,  # Stage 1: operational tension
            ],
            "StageValue": [
                None,
                None,  # Stage 1: tension-controlled
            ],
            "WireType": "Winch wire_LT",
            "StaticsStep1": "Specified tension",
        }

        # Stiffness and damping if provided
        if tensioner.stiffness is not None:
            winch_def["Stiffness"] = tensioner.stiffness
        if tensioner.damping is not None:
            winch_def["Damping"] = tensioner.damping

        # Register winch name for downstream builders
        self._register_entity("winch_names", [winch_name])

        return {"Winches": [winch_def]}
