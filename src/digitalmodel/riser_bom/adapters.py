# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Adapt analytical lazy-wave configurations or solved lengths to BOM quantities."""

from __future__ import annotations

from decimal import localcontext
from typing import TYPE_CHECKING, Protocol

from digitalmodel.citations import Citation

from .costing import Configuration, arithmetic_precision, number

if TYPE_CHECKING:
    from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
        LazyWaveConfiguration,
    )


class LazyWaveLength(Protocol):
    """Length interface of the analytical LazyWaveResults object."""

    total_arc_length: float


def configuration_from_lazy_wave(
    result: LazyWaveLength | LazyWaveConfiguration,
    *,
    grounded_length_m: object,
    joint_length_m: object,
    coated_length_m: object,
    buoyancy_module_count: object,
    ancillary_count: object,
    installation_days: object,
    citation: Citation,
) -> Configuration:
    """Map lazy-wave geometry plus supplied grounded length to an installed BOM.

    A LazyWaveConfiguration is solved with the existing analytical calculation;
    solved length inputs are used directly, without importing the marine package.
    Module count, coating scope and installation duration remain required inputs;
    static geometry alone cannot establish them. The citation shall identify the
    quantity schedule supporting these supplied inputs and the solved length.
    """
    if not hasattr(result, "total_arc_length"):
        from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
            LazyWaveConfiguration,
            LazyWaveSolver,
        )

        if isinstance(result, LazyWaveConfiguration):
            result = LazyWaveSolver().solve(result)
    suspended = number(result.total_arc_length, "total_arc_length", positive=True)
    grounded = number(grounded_length_m, "grounded_length_m")
    with localcontext() as context:
        context.prec = arithmetic_precision([suspended, grounded])
        installed = suspended + grounded
    return Configuration(
        riser_length_m=installed,
        joint_length_m=joint_length_m,
        coated_length_m=coated_length_m,
        buoyancy_module_count=buoyancy_module_count,
        ancillary_count=ancillary_count,
        installation_days=installation_days,
        citation=citation,
    )
