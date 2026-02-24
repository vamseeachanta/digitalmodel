#!/usr/bin/env python3
"""
ABOUTME: Domain builder helpers for constructing OpenFOAM computational domain
configurations from hull geometry dimensions or explicit bounding boxes.
"""

from __future__ import annotations

import math
from typing import Optional

import numpy as np

from .models import DomainConfig


class DomainBuilder:
    """Factory methods for creating DomainConfig objects.

    Provides parametric domain sizing based on hull geometry conventions
    standard in marine CFD (ITTC guidelines). All methods return a
    DomainConfig that can be used directly by OpenFOAMCaseBuilder.
    """

    @staticmethod
    def from_hull_dims(
        length: float,
        beam: float,
        draft: float,
        upstream_factor: float = 2.5,
        downstream_factor: float = 6.0,
        lateral_factor: float = 3.5,
        depth_factor: float = 2.5,
        freeboard_factor: float = 0.5,
        base_cell_size: Optional[float] = None,
    ) -> DomainConfig:
        """Create a DomainConfig from hull principal dimensions.

        Domain is centred on the hull midship section. Sizing follows
        ITTC recommended practices for resistance/seakeeping simulations.

        Args:
            length: Hull length between perpendiculars (m).
            beam: Hull maximum beam (m).
            draft: Hull draught (m).
            upstream_factor: Multiple of L for upstream extent.
            downstream_factor: Multiple of L for downstream extent.
            lateral_factor: Multiple of B for lateral half-extent.
            depth_factor: Multiple of T for depth below keel.
            freeboard_factor: Multiple of L for height above waterplane.
            base_cell_size: Background mesh cell size (m). Defaults to L/20.

        Returns:
            DomainConfig with appropriate extents and cell sizing.
        """
        if base_cell_size is None:
            base_cell_size = length / 20.0

        x_min = -upstream_factor * length
        x_max = downstream_factor * length
        y_min = -lateral_factor * beam
        y_max = lateral_factor * beam
        z_min = -depth_factor * draft
        z_max = freeboard_factor * length

        return DomainConfig(
            min_coords=[x_min, y_min, z_min],
            max_coords=[x_max, y_max, z_max],
            base_cell_size=base_cell_size,
        )

    @staticmethod
    def from_bounding_box(
        bb_min: np.ndarray,
        bb_max: np.ndarray,
        upstream_factor: float = 3.0,
        downstream_factor: float = 6.0,
        lateral_factor: float = 3.0,
        depth_factor: float = 2.5,
        freeboard_factor: float = 0.3,
        base_cell_size: float = 5.0,
        symmetry_y: bool = False,
    ) -> DomainConfig:
        """Create a DomainConfig from a mesh bounding box.

        Expands the supplied geometry bounding box by specified factors
        to form the outer computational domain boundary.

        Args:
            bb_min: Geometry bounding box minimum [x, y, z] (m).
            bb_max: Geometry bounding box maximum [x, y, z] (m).
            upstream_factor: Extent upstream of geometry x-min.
            downstream_factor: Extent downstream of geometry x-max.
            lateral_factor: Lateral extent factor from geometry y-extent.
            depth_factor: Depth factor from geometry z-min.
            freeboard_factor: Height factor from geometry z-max.
            base_cell_size: Background cell size (m).
            symmetry_y: If True, set y_min=0 for symmetry plane.

        Returns:
            DomainConfig with extents enclosing the supplied bounding box.
        """
        geo_length_x = bb_max[0] - bb_min[0]
        geo_half_y = (bb_max[1] - bb_min[1]) / 2.0
        geo_depth = abs(bb_min[2])
        geo_height = abs(bb_max[2])

        x_min = bb_min[0] - upstream_factor * geo_length_x
        x_max = bb_max[0] + downstream_factor * geo_length_x
        y_max = bb_max[1] + lateral_factor * geo_half_y
        y_min = -(bb_max[1] + lateral_factor * geo_half_y)
        z_min = bb_min[2] - depth_factor * max(geo_depth, 1.0)
        z_max = bb_max[2] + freeboard_factor * max(geo_height, geo_length_x)

        if symmetry_y:
            y_min = 0.0

        return DomainConfig(
            min_coords=[float(x_min), float(y_min), float(z_min)],
            max_coords=[float(x_max), float(y_max), float(z_max)],
            base_cell_size=base_cell_size,
        )
