#!/usr/bin/env python3
"""
ABOUTME: Tests for OpenFOAM mesh pipeline covering domain building, blockMeshDict
generation, and STL geometry import bridge.
"""

import pytest
import numpy as np
from pathlib import Path

from digitalmodel.solvers.openfoam.domain_builder import DomainBuilder
from digitalmodel.solvers.openfoam.models import DomainConfig


# ============================================================================
# DomainBuilder tests
# ============================================================================


class TestDomainBuilderFromHullGeometry:
    """Test automatic domain sizing from hull principal dimensions."""

    def test_domain_builder_creates_domain_config(self):
        """DomainBuilder.from_hull_dims returns a DomainConfig."""
        dc = DomainBuilder.from_hull_dims(
            length=200.0, beam=30.0, draft=15.0
        )
        assert isinstance(dc, DomainConfig)

    def test_upstream_extent_at_least_two_lengths(self):
        """Upstream domain extent is at least 2L from bow."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0
        )
        upstream = abs(dc.min_coords[0])
        assert upstream >= 2.0 * 100.0

    def test_downstream_extent_at_least_five_lengths(self):
        """Downstream domain extent is at least 5L from stern."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0
        )
        downstream = dc.max_coords[0]
        assert downstream >= 5.0 * 100.0

    def test_lateral_extent_at_least_three_beams(self):
        """Lateral domain extent is at least 3B from centreline."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0
        )
        lateral = dc.max_coords[1]
        assert lateral >= 3.0 * 20.0

    def test_depth_extent_below_keel(self):
        """Domain extends at least 2 draughts below keel."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0
        )
        depth = abs(dc.min_coords[2])
        assert depth >= 2.0 * 10.0

    def test_domain_extends_above_waterplane(self):
        """Domain extends above waterplane for free-surface simulation."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0
        )
        assert dc.max_coords[2] > 0.0


class TestDomainBuilderCellSizing:
    """Test cell count and grading calculations."""

    def test_cell_counts_positive(self):
        """Cell counts in each direction are positive integers."""
        dc = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0,
            base_cell_size=5.0,
        )
        nx, ny, nz = dc.cell_counts()
        assert nx > 0
        assert ny > 0
        assert nz > 0

    def test_finer_cell_size_gives_more_cells(self):
        """Smaller base_cell_size results in more cells."""
        dc_coarse = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0,
            base_cell_size=10.0,
        )
        dc_fine = DomainBuilder.from_hull_dims(
            length=100.0, beam=20.0, draft=10.0,
            base_cell_size=5.0,
        )
        nx_c, ny_c, nz_c = dc_coarse.cell_counts()
        nx_f, ny_f, nz_f = dc_fine.cell_counts()
        assert nx_f > nx_c


class TestDomainBuilderFromBoundingBox:
    """Test domain creation from explicit bounding box."""

    def test_from_bounding_box_creates_domain(self):
        """DomainBuilder.from_bounding_box wraps supplied coords."""
        bb_min = np.array([-50.0, -25.0, -20.0])
        bb_max = np.array([50.0, 25.0, 0.0])
        dc = DomainBuilder.from_bounding_box(
            bb_min=bb_min, bb_max=bb_max,
            upstream_factor=3.0, downstream_factor=6.0,
        )
        assert isinstance(dc, DomainConfig)
        assert dc.min_coords[0] < bb_min[0]
        assert dc.max_coords[0] > bb_max[0]

    def test_from_bounding_box_symmetry_flag(self):
        """Symmetric domain halves lateral extent."""
        bb_min = np.array([-50.0, -25.0, -20.0])
        bb_max = np.array([50.0, 25.0, 0.0])
        dc_full = DomainBuilder.from_bounding_box(
            bb_min=bb_min, bb_max=bb_max
        )
        dc_sym = DomainBuilder.from_bounding_box(
            bb_min=bb_min, bb_max=bb_max, symmetry_y=True
        )
        # Symmetric case: ymin should be 0
        assert dc_sym.min_coords[1] == pytest.approx(0.0)
        # Full case: ymin should be negative
        assert dc_full.min_coords[1] < 0.0
