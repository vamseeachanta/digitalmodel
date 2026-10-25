# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numeric length contract independent of the parent-contract composer."""

import math
from typing import Annotated, Literal, Self

from pydantic import BaseModel, ConfigDict, Field, model_validator

# Mathematical positivity/finiteness constraints, not engineering design limits.
PositiveLength = Annotated[float, Field(strict=True, allow_inf_nan=False, gt=0)]
AbsoluteTolerance = Annotated[float, Field(strict=True, allow_inf_nan=False, ge=0)]


class RiserLengthContract(BaseModel):
    """Admit segment lengths only when their sum matches the declared total.

    The geometric identity is L_total = sum(L_segment). An absolute tolerance
    in metres is mandatory caller input; no design acceptance limit is assumed.
    This does not check equilibrium, stretch, orientation, or suitability.
    """

    model_config = ConfigDict(extra="forbid", frozen=True)

    segments_m: tuple[PositiveLength, ...] = Field(min_length=1)
    total_m: PositiveLength
    absolute_tolerance_m: AbsoluteTolerance
    units: Literal["m"]

    @model_validator(mode="after")
    def check_total(self) -> Self:
        """Check length additivity without replacing the admitted total."""
        try:
            summed = math.fsum(self.segments_m)
        except OverflowError as exc:
            raise ValueError("segment total is not finite") from exc
        # Zero relative tolerance means only the supplied absolute tolerance acts.
        if not math.isclose(
            summed, self.total_m, rel_tol=0.0, abs_tol=self.absolute_tolerance_m
        ):
            raise ValueError("declared total differs from the segment length sum")
        return self
