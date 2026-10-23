# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Finite scalar result transport, independent of calculation manifests."""

from typing import Annotated

from pydantic import BaseModel, ConfigDict, Field, StringConstraints

NonblankText = Annotated[str, StringConstraints(strip_whitespace=True, min_length=1)]
FiniteNumber = Annotated[float, Field(strict=True, allow_inf_nan=False)]


class EngineeringResult(BaseModel):
    """Preserve a scalar with explicit units and its stated source.

    This is a transport contract, not an engineering acceptance check. The
    source is descriptive text; citation resolution remains a separate step.
    Unit strings are preserved without conversion or interpretation.
    """

    model_config = ConfigDict(extra="forbid", frozen=True)

    quantity: NonblankText
    value: FiniteNumber
    units: NonblankText
    source: NonblankText
