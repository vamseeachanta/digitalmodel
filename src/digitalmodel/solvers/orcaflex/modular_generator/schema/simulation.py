"""Simulation control models."""

from __future__ import annotations

from pydantic import BaseModel, Field, field_validator


class Simulation(BaseModel):
    """
    Simulation control parameters.

    Attributes:
        time_step: Outer time step for dynamics (s).
        stages: Stage durations [build-up, main] (s).
        north_direction: Direction of model North in degrees.
        inner_time_step: Optional inner time step (s).
        implicit_constant: Optional implicit integration constant.
    """

    time_step: float = Field(
        default=0.1, gt=0, le=1, description="Outer time step (s)"
    )
    stages: list[int | float] = Field(
        default_factory=lambda: [8, 16],
        min_length=1,
        description="Stage durations (s)",
    )
    north_direction: float = Field(
        default=0, ge=0, lt=360, description="North direction (deg)"
    )
    inner_time_step: float | None = Field(
        default=None, gt=0, le=0.1, description="Inner time step (s)"
    )
    implicit_constant: float | None = Field(
        default=None, ge=0, le=1, description="Implicit integration constant"
    )

    @field_validator("time_step")
    @classmethod
    def validate_time_step(cls, v: float) -> float:
        """Validate time step is reasonable for offshore analysis."""
        if v < 0.001:
            raise ValueError(
                f"Time step {v}s is too small. Minimum recommended is 0.001s"
            )
        return v

    @field_validator("stages")
    @classmethod
    def validate_stages(cls, v: list[int | float]) -> list[int | float]:
        """Validate stage durations are positive."""
        for i, stage in enumerate(v):
            if stage <= 0:
                raise ValueError(f"Stage {i} duration must be positive, got {stage}")
        return v
