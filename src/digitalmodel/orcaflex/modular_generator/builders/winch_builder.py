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
        pipeline_name = self.context.main_pipeline_name or self.spec.pipeline.name

        winch_name = tensioner.name

        # Tensioner position on vessel (default to stern area)
        position = tensioner.position_on_vessel or [0, 0, 0]

        winch_def = {
            "Name": winch_name,
            "Connection": [vessel.name, pipeline_name],
            "ConnectionPoint": [position, [0, 0, 0]],
            "ConnectionzRelativeTo": [None, "End A"],
            "StageMode": [
                "Specified length",  # Stage 0: build-up
                "Specified tension",  # Stage 1: operational
            ],
            "StageTension": [
                0,  # Stage 0: no tension during build-up
                tensioner.tension_value,  # Stage 1: operational tension
            ],
            "StageValue": [
                10,  # Stage 0: initial wire length
                None,  # Stage 1: tension-controlled
            ],
            "WireType": "Winch wire_LT",
            "StaticsStep1": "Specified length",
        }

        # Stiffness and damping if provided
        if tensioner.stiffness is not None:
            winch_def["Stiffness"] = tensioner.stiffness
        if tensioner.damping is not None:
            winch_def["Damping"] = tensioner.damping

        # Register winch name for downstream builders
        self._register_entity("winch_names", [winch_name])

        return {"Winches": [winch_def]}
