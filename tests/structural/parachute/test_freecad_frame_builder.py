"""
Tests for FreeCAD frame builder — creates 3D tube model from geometry.
Requires FreeCAD — skipped if not available.
"""
import os
import sys
import tempfile

import pytest

for p in ["/usr/lib/freecad-python3/lib", "/usr/lib/freecad/lib"]:
    if os.path.exists(p) and p not in sys.path:
        sys.path.append(p)

try:
    import FreeCAD

    FREECAD_AVAILABLE = True
except ImportError:
    FREECAD_AVAILABLE = False

pytestmark = pytest.mark.skipif(
    not FREECAD_AVAILABLE, reason="FreeCAD not installed"
)

if FREECAD_AVAILABLE:
    from digitalmodel.structural.parachute.frame_geometry_3d import (
        build_gt1r_frame_3d,
    )
    from digitalmodel.structural.parachute.freecad_frame_builder import (
        build_freecad_model,
        export_iges,
        export_step,
    )


class TestFreeCADModelCreation:
    def setup_method(self):
        self.geo = build_gt1r_frame_3d()
        self.doc = build_freecad_model(self.geo)

    def teardown_method(self):
        if self.doc:
            FreeCAD.closeDocument(self.doc.Name)

    def test_document_created(self):
        assert self.doc is not None
        assert self.doc.Name == "GT1R_ParachuteFrame"

    def test_tube_objects_created(self):
        tubes = [
            o for o in self.doc.Objects if "tube_" in o.Name.lower()
        ]
        assert len(tubes) == 16  # 12 straight + 4 bends

    def test_tube_has_shape_with_volume(self):
        tubes = [
            o for o in self.doc.Objects if "tube_" in o.Name.lower()
        ]
        for t in tubes:
            assert hasattr(t, "Shape")
            assert t.Shape.Volume > 0

    def test_connection_markers_created(self):
        markers = [
            o for o in self.doc.Objects if "conn_" in o.Name.lower()
        ]
        assert len(markers) == 15  # 15 connections


class TestSTEPExport:
    def setup_method(self):
        self.geo = build_gt1r_frame_3d()
        self.doc = build_freecad_model(self.geo)

    def teardown_method(self):
        if self.doc:
            FreeCAD.closeDocument(self.doc.Name)

    def test_step_export_creates_file(self):
        with tempfile.NamedTemporaryFile(
            suffix=".step", delete=False
        ) as f:
            path = f.name
        try:
            result = export_step(self.doc, path)
            assert os.path.exists(result)
            assert os.path.getsize(result) > 0
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_step_export_returns_path(self):
        with tempfile.NamedTemporaryFile(
            suffix=".step", delete=False
        ) as f:
            path = f.name
        try:
            result = export_step(self.doc, path)
            assert result == path
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_iges_export_creates_file(self):
        with tempfile.NamedTemporaryFile(
            suffix=".iges", delete=False
        ) as f:
            path = f.name
        try:
            result = export_iges(self.doc, path)
            assert os.path.exists(result)
            assert os.path.getsize(result) > 0
        finally:
            if os.path.exists(path):
                os.unlink(path)
