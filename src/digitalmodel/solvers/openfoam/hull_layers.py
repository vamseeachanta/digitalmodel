# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Absolute layer inputs and requested-stack diagnostics, without a mesh verdict.

Source: OpenFOAM v2312 (2023), snappyHexMesh layer guide, keyword sections
relativeSizes, expansionRatio, firstLayerThickness, minThickness, nSurfaceLayers.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from typing import TYPE_CHECKING, Sequence

if TYPE_CHECKING:
    from .hull_case_regions import SurfaceRegion

LAYER_GUIDE = (
    "https://doc.openfoam.com/2312/tools/pre-processing/mesh/generation/"
    "snappyhexmesh/layers/"
)


def _positive_finite(name: str, value: float) -> None:
    if isinstance(value, bool) or not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be a positive finite scalar")


@dataclass(frozen=True)
class AbsoluteLayerSpec:
    """Caller-supplied lengths in metres; no empirical sizing limit is implied.

    min_thickness_m is the minimum OVERALL stack, not the minimum first layer
    (OpenFOAM v2312 layer guide, minThickness). Counts remain per surface.
    """

    first_layer_thickness_m: float
    expansion_ratio: float
    min_thickness_m: float

    def __post_init__(self) -> None:
        for name, value in asdict(self).items():
            _positive_finite(name, value)

    def sizing(
        self, n_surface_layers: int, outside_cell_size_m: float | None = None
    ) -> dict:
        """Return requested lengths, with optional cell-to-layer ratios.

        The guide's expansionRatio gives h_i = h_first*r**i. Summation gives
        H = h_first*(r**n - 1)/(r - 1), or n*h_first when r=1.
        expm1 avoids cancellation near unity. These are geometric identities,
        not a correlation predicting layer survival. A supplied cell length
        must represent the local undistorted cell normal to the patch.
        """
        if (
            isinstance(n_surface_layers, bool)
            or not isinstance(n_surface_layers, int)
            or n_surface_layers < 0
        ):
            raise ValueError("n_surface_layers must be a nonnegative integer")
        if outside_cell_size_m is not None:
            _positive_finite("outside_cell_size_m", outside_cell_size_m)
        first = self.first_layer_thickness_m if n_surface_layers else None
        final, total = self._stack(n_surface_layers)
        return {
            "n_surface_layers": n_surface_layers,
            "first_layer_thickness_m": first,
            "final_layer_thickness_m": final,
            "total_thickness_m": total,
            "outside_cell_size_m": outside_cell_size_m,
            "cell_to_first_layer_ratio": (
                _finite_ratio(outside_cell_size_m, first) if first else None
            ),
            "cell_to_stack_ratio": (
                _finite_ratio(outside_cell_size_m, total) if total else None
            ),
            "minimum_stack_exceeds_request": total < self.min_thickness_m
            if first
            else None,
            "coverage_status": "NOT_EVALUATED",
            "y_plus_status": "NOT_EVALUATED",
        }

    def _stack(self, count: int) -> tuple[float | None, float]:
        if not count:
            return None, 0.0
        first, ratio = self.first_layer_thickness_m, self.expansion_ratio
        try:
            final = first * ratio ** (count - 1)
            total = (
                first * count
                if ratio == 1
                else first * (math.expm1(count * math.log(ratio)) / (ratio - 1))
            )
        except OverflowError as exc:
            raise ValueError("layer stack is not representable") from exc
        if not all(math.isfinite(v) and v > 0 for v in (final, total)):
            raise ValueError("layer stack is not representable")
        return final, total


def _finite_ratio(numerator: float | None, denominator: float) -> float | None:
    if numerator is None:
        return None
    ratio = numerator / denominator
    if not math.isfinite(ratio) or ratio <= 0:
        raise ValueError("cell-to-layer ratio is not representable")
    return ratio


def layer_tokens(
    spec: AbsoluteLayerSpec | None, regions: Sequence[SurfaceRegion]
) -> dict[str, str]:
    """Preserve relative defaults unless absolute lengths are supplied."""
    if spec is None:
        # Inherited values, not sizing recommendations: shipped hull_resistance
        # and hull_double_body system/snappyHexMeshDict at revision 6476218e.
        return {
            "RELATIVESIZES": "true",
            "EXPANSIONRATIO": "1.5",
            "LAYERTHICKNESS": "finalLayerThickness 0.7;",
            "MINTHICKNESS": "0.25",
        }
    # Validate every patch before either builder writes the first dictionary.
    for region in regions:
        spec.sizing(region.n_surface_layers)
    return {
        "RELATIVESIZES": "false",
        "EXPANSIONRATIO": str(spec.expansion_ratio),
        "LAYERTHICKNESS": f"firstLayerThickness {spec.first_layer_thickness_m};",
        "MINTHICKNESS": str(spec.min_thickness_m),
    }


def layer_provenance(
    spec: AbsoluteLayerSpec | None, regions: Sequence[SurfaceRegion]
) -> dict:
    """Record requests; actual cell lengths, coverage and y+ need native output."""
    return {
        "mode": "inherited_relative" if spec is None else "absolute",
        "source": LAYER_GUIDE,
        "specification": None if spec is None else asdict(spec),
        "patches": {}
        if spec is None
        else {region.name: spec.sizing(region.n_surface_layers) for region in regions},
        "coverage_status": "NOT_EVALUATED",
        "y_plus_status": "NOT_EVALUATED",
        "limitation": (
            "Requested stack only. Local undistorted normal cell size, achieved "
            "layers, coverage and y+ require native mesh and flow readback. "
            "No universal cell-to-layer acceptance ratio is established."
        ),
    }
