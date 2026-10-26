# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pure mesh/grid study inputs and deterministic case expansion for issue #612.

Schema-only scope follows the schema parsing allowance in
``docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md``.
Frequency coordinates are angular frequencies in rad/s, strictly above zero.
Headings use the plan's canonical interval: zero inclusive, one full turn
(360 degrees by the definition of degree) exclusive. Coordinates retain input
order; there is no interpolation, normalization or unit conversion here.

YAML is supplied as text. Mesh references are opaque strings, never resolved
or checked. This module performs no filesystem access, solver detection,
package preparation or execution. Importing it through the legacy package
initializer can still load that initializer's unrelated dependencies.
"""

from __future__ import annotations

from dataclasses import dataclass
from itertools import product
from typing import Annotated, Self

import yaml
from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    StringConstraints,
    field_validator,
    model_validator,
)

NonemptyText = Annotated[
    str, StringConstraints(strict=True, strip_whitespace=True, min_length=1)
]
Frequency = Annotated[float, Field(strict=True, gt=0, allow_inf_nan=False)]
Heading = Annotated[float, Field(strict=True, ge=0, lt=360, allow_inf_nan=False)]


class _StudyInput(BaseModel):
    """Reject unknown keys and retain immutable validated inputs."""

    model_config = ConfigDict(extra="forbid", frozen=True)


class MeshVariant(_StudyInput):
    """A labelled mesh reference; existence and preparation are out of scope."""

    label: NonemptyText
    mesh_file: NonemptyText


class FrequencyGrid(_StudyInput):
    """A labelled nonempty grid of distinct positive angular frequencies."""

    label: NonemptyText
    frequencies: Annotated[tuple[Frequency, ...], Field(min_length=1)]

    @field_validator("frequencies")
    @classmethod
    def unique_frequencies(cls, values: tuple[float, ...]) -> tuple[float, ...]:
        """Reject repeated coordinates without sorting or removing them."""
        if len(values) != len(set(values)):
            raise ValueError("frequencies must contain unique coordinates")
        return values


class HeadingGrid(_StudyInput):
    """A labelled nonempty grid of distinct canonical headings in degrees."""

    label: NonemptyText
    headings: Annotated[tuple[Heading, ...], Field(min_length=1)]

    @field_validator("headings")
    @classmethod
    def unique_headings(cls, values: tuple[float, ...]) -> tuple[float, ...]:
        """Reject repeated coordinates within the canonical interval."""
        if len(values) != len(set(values)):
            raise ValueError("headings must contain unique coordinates")
        return values


class ConvergenceStudy(_StudyInput):
    """Three nonempty axes, each with unique labels in declaration order."""

    mesh_variants: Annotated[tuple[MeshVariant, ...], Field(min_length=1)]
    frequency_grids: Annotated[tuple[FrequencyGrid, ...], Field(min_length=1)]
    heading_grids: Annotated[tuple[HeadingGrid, ...], Field(min_length=1)]

    @model_validator(mode="after")
    def unique_axis_labels(self) -> Self:
        """Scope label uniqueness to each axis, after whitespace stripping."""
        for name in ("mesh_variants", "frequency_grids", "heading_grids"):
            labels = [item.label for item in getattr(self, name)]
            if len(labels) != len(set(labels)):
                raise ValueError(f"{name} must have unique labels")
        return self

    @classmethod
    def from_yaml(cls, text: str) -> Self:
        """Parse YAML text without opening paths or generating artifacts.

        Raises:
            ValueError: If YAML syntax or any study input is invalid.
        """
        try:
            raw = yaml.safe_load(text)
        except yaml.YAMLError as error:
            raise ValueError("invalid study YAML") from error
        return cls.model_validate(raw)


@dataclass(frozen=True)
class StudyCase:
    """Immutable descriptor identified by its three axis labels together."""

    mesh_label: str
    frequency_label: str
    heading_label: str
    mesh_file: str
    frequencies: tuple[float, ...]
    headings: tuple[float, ...]


def expand_study(study: ConvergenceStudy, *, max_cases: int) -> list[StudyCase]:
    """Expand validated axes in mesh, frequency-grid, heading-grid order.

    Headings vary fastest. Cardinality is the product of the three axis
    lengths, not the number of individual frequency/heading coordinates.
    The caller must provide a positive integer cap; there is no default or
    override. The cap is checked before constructing the Cartesian product
    or allocating any case descriptors.

    Raises:
        ValueError: If the cap is invalid or the product exceeds it.
    """
    if type(max_cases) is not int or max_cases <= 0:
        raise ValueError("max_cases must be a positive integer")
    case_count = (
        len(study.mesh_variants)
        * len(study.frequency_grids)
        * len(study.heading_grids)
    )
    if case_count > max_cases:
        raise ValueError(
            f"study has {case_count} cases, exceeding max_cases={max_cases}"
        )
    return [
        StudyCase(
            mesh_label=mesh.label,
            frequency_label=frequency.label,
            heading_label=heading.label,
            mesh_file=mesh.mesh_file,
            frequencies=frequency.frequencies,
            headings=heading.headings,
        )
        for mesh, frequency, heading in product(
            study.mesh_variants, study.frequency_grids, study.heading_grids
        )
    ]
