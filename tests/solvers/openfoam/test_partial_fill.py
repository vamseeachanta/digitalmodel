#!/usr/bin/env python3
"""
ABOUTME: Tests for the VOF partial-fill initialisation helpers (#659) — fill
fraction -> box height, free-surface-on-cell-face snapping (and warning), the
setFieldsDict rendering, and end-to-end that SloshingSetup emits a partial-fill
setFieldsDict through the case builder.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.partial_fill import (
    FillSnap,
    partial_fill_box,
    render_set_fields_dict_body,
    snap_fill_to_cell_face,
)
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.marine_solvers import SloshingSetup
from digitalmodel.solvers.openfoam.models import DomainConfig


# ============================================================================
# Snapping the free surface onto a cell face
# ============================================================================


class TestSnapFillToCellFace:
    def test_exact_fill_is_not_adjusted(self):
        # 0.5 of a 10-cell tank = exactly 5 cells -> no adjustment.
        snap = snap_fill_to_cell_face(tank_height=1.0, n_cells=10, fill_level=0.5)
        assert snap.n_fill_cells == 5
        assert snap.fill_height == pytest.approx(0.5)
        assert snap.fill_level == pytest.approx(0.5)
        assert snap.adjusted is False
        assert snap.adjustment == pytest.approx(0.0)

    def test_fraction_maps_to_box_height(self):
        # 0.3 of a 2.0 m tank with 20 cells = 6 cells, h = 0.6 m.
        snap = snap_fill_to_cell_face(tank_height=2.0, n_cells=20, fill_level=0.3)
        assert snap.n_fill_cells == 6
        assert snap.fill_height == pytest.approx(0.6)
        assert snap.cell_height == pytest.approx(0.1)

    def test_off_face_fill_is_snapped_to_nearest_face(self):
        # 0.53 of a 10-cell tank -> rounds to 5 cells (0.50), adjusted.
        snap = snap_fill_to_cell_face(tank_height=1.0, n_cells=10, fill_level=0.53)
        assert snap.n_fill_cells == 5
        assert snap.fill_level == pytest.approx(0.5)
        assert snap.adjusted is True
        assert snap.adjustment == pytest.approx(-0.03)

    def test_snap_rounds_up_when_closer(self):
        # 0.57 -> nearest face is 6 cells (0.60).
        snap = snap_fill_to_cell_face(tank_height=1.0, n_cells=10, fill_level=0.57)
        assert snap.n_fill_cells == 6
        assert snap.fill_level == pytest.approx(0.6)

    def test_warns_when_adjusted(self, caplog):
        import logging

        from loguru import logger

        # Bridge loguru -> stdlib so pytest's caplog captures the warning.
        handler_id = logger.add(caplog.handler, format="{message}", level="WARNING")
        try:
            with caplog.at_level(logging.WARNING):
                snap_fill_to_cell_face(tank_height=1.0, n_cells=10, fill_level=0.53)
        finally:
            logger.remove(handler_id)
        assert any("snapped" in r.message for r in caplog.records)

    def test_no_warning_when_exact(self, caplog):
        import logging

        from loguru import logger

        handler_id = logger.add(caplog.handler, format="{message}", level="WARNING")
        try:
            with caplog.at_level(logging.WARNING):
                snap_fill_to_cell_face(tank_height=1.0, n_cells=10, fill_level=0.5)
        finally:
            logger.remove(handler_id)
        assert not any("snapped" in r.message for r in caplog.records)

    @pytest.mark.parametrize(
        "tank_height,n_cells,fill_level",
        [(0.0, 10, 0.5), (-1.0, 10, 0.5)],
    )
    def test_bad_tank_height_rejected(self, tank_height, n_cells, fill_level):
        with pytest.raises(ValueError):
            snap_fill_to_cell_face(tank_height, n_cells, fill_level)

    def test_bad_cell_count_rejected(self):
        with pytest.raises(ValueError):
            snap_fill_to_cell_face(1.0, 0, 0.5)

    @pytest.mark.parametrize("fill_level", [-0.01, 1.01])
    def test_fill_level_out_of_range_rejected(self, fill_level):
        with pytest.raises(ValueError):
            snap_fill_to_cell_face(1.0, 10, fill_level)

    def test_empty_and_full_snaps(self):
        assert snap_fill_to_cell_face(1.0, 10, 0.0).n_fill_cells == 0
        assert snap_fill_to_cell_face(1.0, 10, 1.0).n_fill_cells == 10


# ============================================================================
# Fill-box geometry
# ============================================================================


class TestPartialFillBox:
    def test_box_top_is_fill_height_on_z(self):
        # z-up: box runs from below the floor up to zmin + h.
        box_min, box_max = partial_fill_box(
            [0.0, 0.0, 0.0], [1.0, 0.06, 0.5], fill_height=0.3, vertical_axis=2
        )
        assert box_max[2] == pytest.approx(0.3)
        # brackets the tank in x and y
        assert box_min[0] < 0.0 and box_max[0] > 1.0
        assert box_min[1] < 0.0 and box_max[1] > 0.06
        assert box_min[2] < 0.0  # below the floor

    def test_vertical_axis_y(self):
        box_min, box_max = partial_fill_box(
            [0.0, 0.0, 0.0], [1.0, 0.6, 0.01], fill_height=0.3, vertical_axis=1
        )
        assert box_max[1] == pytest.approx(0.3)
        assert box_min[1] < 0.0

    def test_bad_axis_rejected(self):
        with pytest.raises(ValueError):
            partial_fill_box([0, 0, 0], [1, 1, 1], 0.5, vertical_axis=3)


# ============================================================================
# setFieldsDict rendering
# ============================================================================


class TestRenderSetFieldsDict:
    def test_default_air_liquid_box(self):
        body = render_set_fields_dict_body((-1, -1, -1), (2, 2, 0.3))
        assert "defaultFieldValues ( volScalarFieldValue alpha.water 0 );" in body
        assert "boxToCell" in body
        assert "box (-1 -1 -1) (2 2 0.3);" in body
        assert "fieldValues ( volScalarFieldValue alpha.water 1 );" in body


# ============================================================================
# End-to-end: SloshingSetup -> case builder -> setFieldsDict
# ============================================================================


class TestSloshingSetupIntegration:
    def _sloshing_case(self, fill_level, tmp_path, n_cells_z=20):
        setup = SloshingSetup(fill_level=fill_level, name="slosh_fill")
        # Give the case a concrete rectangular tank so the fill height is
        # deterministic: 1.0 x 0.06 x 0.5 m, z-up, 20 cells over the height.
        setup.case.domain = DomainConfig(
            min_coords=[0.0, 0.0, 0.0],
            max_coords=[1.0, 0.06, 0.5],
            n_cells=[50, 1, n_cells_z],
        )
        return OpenFOAMCaseBuilder(setup.case).build(tmp_path)

    def test_emits_set_fields_dict(self, tmp_path):
        case_dir = self._sloshing_case(0.5, tmp_path)
        sfd = case_dir / "system" / "setFieldsDict"
        assert sfd.exists()
        content = sfd.read_text()
        assert "FoamFile" in content
        assert "object      setFieldsDict;" in content
        assert "boxToCell" in content

    def test_fill_height_matches_fraction(self, tmp_path):
        # 0.5 of a 0.5 m tank with 20 z-cells = 0.25 m (exactly on a face).
        case_dir = self._sloshing_case(0.5, tmp_path)
        content = (case_dir / "system" / "setFieldsDict").read_text()
        # box top on z should be 0.25
        assert "0.25)" in content

    def test_snapped_fill_height(self, tmp_path):
        # 0.53 of a 0.5 m tank, 20 cells (dz=0.025): 0.265 -> 11 cells -> 0.275
        case_dir = self._sloshing_case(0.53, tmp_path)
        content = (case_dir / "system" / "setFieldsDict").read_text()
        assert "0.275)" in content

    def test_no_fill_level_no_set_fields(self, tmp_path):
        # fill_level=None on the underlying case -> no setFieldsDict.
        setup = SloshingSetup(name="slosh_nofill")
        setup.case.fill_level = None
        case_dir = OpenFOAMCaseBuilder(setup.case).build(tmp_path)
        assert not (case_dir / "system" / "setFieldsDict").exists()
