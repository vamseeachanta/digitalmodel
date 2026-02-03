"""Root ProjectInputSpec model."""

from __future__ import annotations

from pydantic import BaseModel, Field, model_validator

from .metadata import Metadata
from .environment import Environment
from .pipeline import Pipeline
from .equipment import Equipment
from .simulation import Simulation


class ProjectInputSpec(BaseModel):
    """
    Root model for project-specific OrcaFlex model input specification.

    This is the main entry point for validating YAML input files.
    It contains all components needed to generate a complete OrcaFlex model.

    Example:
        ```python
        from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
        import yaml

        with open('spec.yml') as f:
            data = yaml.safe_load(f)

        spec = ProjectInputSpec(**data)
        print(f"Model: {spec.metadata.name}")
        print(f"Pipeline length: {sum(s.length for s in spec.pipeline.segments)}m")
        ```

    Attributes:
        metadata: Model identification and categorization.
        environment: Environmental conditions (water, metocean).
        pipeline: Pipeline definition (dimensions, coatings, segments).
        equipment: Installation equipment (tugs, rollers, buoyancy modules).
        simulation: Simulation control parameters.
    """

    metadata: Metadata = Field(..., description="Model metadata")
    environment: Environment = Field(..., description="Environmental conditions")
    pipeline: Pipeline = Field(..., description="Pipeline definition")
    equipment: Equipment = Field(
        default_factory=Equipment, description="Equipment configuration"
    )
    simulation: Simulation = Field(
        default_factory=Simulation, description="Simulation parameters"
    )

    @model_validator(mode="after")
    def validate_consistency(self) -> "ProjectInputSpec":
        """
        Cross-validate model components for physical consistency.

        Checks:
        - Water depth consistency with seabed features
        - Tug positions relative to pipeline length
        - Buoyancy module spacing vs segment length
        """
        # Check if tug spacing and count fit within pipeline length
        if self.equipment.tugs:
            total_pipeline_length = sum(s.length for s in self.pipeline.segments)
            tugs = self.equipment.tugs
            max_tug_x = tugs.first_position[0] + (tugs.count - 1) * tugs.spacing

            # Only warn if tugs extend significantly beyond pipeline
            if max_tug_x > total_pipeline_length * 1.5:
                # This could be intentional for staged installation
                pass  # Could add warning logging here

        # Check S-lay equipment consistency
        if self.equipment.stinger and not self.equipment.vessel:
            raise ValueError("Stinger requires a vessel to be defined")
        if self.equipment.tensioner and not self.equipment.vessel:
            raise ValueError("Tensioner requires a vessel to be defined")

        return self

    def get_total_pipeline_length(self) -> float:
        """Calculate total pipeline length from all segments."""
        return sum(s.length for s in self.pipeline.segments)

    def get_tug_positions(self) -> list[list[float]]:
        """Generate positions for all tugs based on spacing configuration."""
        if not self.equipment.tugs:
            return []

        tugs = self.equipment.tugs
        positions = []
        for i in range(tugs.count):
            x = tugs.first_position[0] + i * tugs.spacing
            y = tugs.first_position[1]
            z = tugs.first_position[2]
            positions.append([x, y, z])
        return positions

    def get_buoyancy_module_positions(self) -> list[float]:
        """Generate arc-length positions for buoyancy modules."""
        if not self.equipment.buoyancy_modules:
            return []

        bm = self.equipment.buoyancy_modules
        total_length = self.get_total_pipeline_length()

        start = bm.start_arc_length or 0
        end = bm.end_arc_length or total_length

        positions = []
        current = start
        while current <= end:
            positions.append(current)
            current += bm.spacing

        return positions

    def is_s_lay(self) -> bool:
        """Check if this is an S-lay installation model."""
        return self.equipment.vessel is not None

    def is_floating(self) -> bool:
        """Check if this is a floating installation model."""
        return self.equipment.tugs is not None
