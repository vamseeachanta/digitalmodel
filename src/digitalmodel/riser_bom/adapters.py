# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Adapter for the analytical lazy-wave result, without importing any solver."""

from __future__ import annotations

from decimal import localcontext
from typing import Protocol

from digitalmodel.citations import Citation

from .costing import Configuration, arithmetic_precision, number


class LazyWaveLength(Protocol):
    """Length interface of the analytical LazyWaveResults object."""

    total_arc_length: float


def configuration_from_lazy_wave(
    result: LazyWaveLength,
    *,
    grounded_length_m: object,
    joint_length_m: object,
    coated_length_m: object,
    buoyancy_module_count: object,
    ancillary_count: object,
    installation_days: object,
    citation: Citation,
) -> Configuration:
    """Map solved suspended length plus supplied grounded length to an installed BOM.

    Module count, coating scope and installation duration remain required inputs;
    static geometry alone cannot establish them. The citation shall identify the
    quantity schedule supporting these supplied inputs and the solved length.
    """
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
