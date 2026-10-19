# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic DXF inputs; no external drawings or geometry kernels."""

import math

import ezdxf
import pytest

from digitalmodel.cad import CadReadError, UnitsError, read_cad


def drawing(tmp_path, units=4):
    """ezdxf 1.4.5 DXF Units, INSUNITS table: 4=mm; 0=unitless."""
    doc = ezdxf.new()
    doc.units = units
    return doc, tmp_path / "drawing.dxf"


def test_line_point_and_units(tmp_path):
    """Hand: (1000,2000,3000) mm /1000 = (1,2,3) m.

    NIST SI Prefixes, milli row (accessed 2026-10-19).
    """
    doc, path = drawing(tmp_path)
    doc.modelspace().add_line(
        (0, 0, 0), (1000, 2000, 3000), dxfattribs={"layer": "EDGES"}
    )
    doc.modelspace().add_point((2000, 0, 0))
    doc.saveas(path)
    result = read_cad(path)
    assert result.format == "dxf"
    assert result.units == "m"
    assert result.unit_decision.source_units == "mm"
    assert result.unit_decision.basis == "file"
    assert result.points == ((0, 0, 0), (1, 2, 3), (2, 0, 0))
    assert result.entities[0].kind == "line"
    assert result.entities[0].indices == (0, 1)
    assert result.entities[0].layer == "EDGES"
    assert result.entities[0].source_id
    assert result.entities[1].kind == "point"
    assert result.entities[1].indices == (2,)


def test_unitless_drawing_needs_explicit_units(tmp_path):
    """Hand: 100 cm /100 = 1 m (NIST SI Prefixes, centi row)."""
    doc, path = drawing(tmp_path, units=0)
    doc.modelspace().add_line((0, 0), (100, 0))
    doc.saveas(path)
    with pytest.raises(UnitsError, match="State units"):
        read_cad(path)
    result = read_cad(path, units="cm")
    assert result.points[-1] == (1, 0, 0)
    assert result.unit_decision.basis == "caller"


def test_conflicting_units_are_not_silently_overridden(tmp_path):
    doc, path = drawing(tmp_path)
    doc.modelspace().add_point((0, 0))
    doc.saveas(path)
    with pytest.raises(UnitsError, match="conflict"):
        read_cad(path, units="m")


def test_unknown_unit_code_is_not_unitless(tmp_path):
    """ezdxf 1.4.5 DXF Units: INSUNITS=21 is US Survey Feet, unsupported."""
    doc, path = drawing(tmp_path, units=21)
    doc.modelspace().add_point((0, 0))
    doc.saveas(path)
    with pytest.raises(UnitsError, match="Unsupported.*INSUNITS"):
        read_cad(path, units="ft")


def test_closed_polyline_uses_wcs(tmp_path):
    """Hand: extrusion (0,0,-1) gives OCS x=-WCS x and elevation z=-1000.

    ezdxf 1.4.5 LWPOLYLINE vertices_in_wcs and OCS arbitrary-axis algorithm:
    OCS x = world Y cross normal = (-1,0,0), OCS y = (0,1,0).
    Divide millimetre coordinates by 1000 (NIST SI Prefixes, milli row).
    """
    doc, path = drawing(tmp_path)
    doc.modelspace().add_lwpolyline(
        [(0, 0), (1000, 0), (1000, 2000)],
        close=True,
        dxfattribs={"elevation": 1000, "extrusion": (0, 0, -1)},
    )
    doc.saveas(path)
    result = read_cad(path)
    assert result.points == ((0, 0, -1), (-1, 0, -1), (-1, 2, -1))
    assert result.entities[0].kind == "polyline"
    assert result.entities[0].indices == (0, 1, 2, 0)


def test_polyline_and_faces(tmp_path):
    """Hand: coordinates are already metres; triangle and quad order retained."""
    doc, path = drawing(tmp_path, units=6)
    model = doc.modelspace()
    model.add_polyline3d([(0, 0, 0), (1, 2, 3)])
    model.add_3dface([(0, 0, 0), (1, 0, 0), (0, 1, 0)])
    model.add_3dface([(0, 0, 1), (1, 0, 1), (1, 1, 1), (0, 1, 1)])
    doc.saveas(path)
    result = read_cad(path)
    assert [entity.kind for entity in result.entities] == [
        "polyline",
        "triangle",
        "polygon",
    ]
    assert result.points[1] == (1, 2, 3)
    assert result.entities[1].indices == (2, 3, 4)
    assert result.entities[2].indices == (5, 6, 7, 8)


@pytest.mark.parametrize("kind", ["CIRCLE", "TEXT", "INSERT"])
def test_unsupported_entities_fail_with_type_and_handle(tmp_path, kind):
    doc, path = drawing(tmp_path)
    doc.modelspace().add_point((0, 0))
    if kind == "CIRCLE":
        entity = doc.modelspace().add_circle((0, 0), 1)
    elif kind == "TEXT":
        entity = doc.modelspace().add_text("synthetic")
    else:
        doc.blocks.new("SHAPE").add_point((1, 0))
        entity = doc.modelspace().add_blockref("SHAPE", (0, 0))
    doc.saveas(path)
    with pytest.raises(CadReadError, match=kind) as caught:
        read_cad(path)
    assert entity.dxf.handle in str(caught.value)


@pytest.mark.parametrize("attribute", ["bulge", "width", "thickness"])
def test_curve_or_solid_semantics_are_not_discarded(tmp_path, attribute):
    doc, path = drawing(tmp_path)
    entity = doc.modelspace().add_lwpolyline([(0, 0), (1, 0)])
    if attribute == "bulge":
        entity.set_points([(0, 0, 0, 0, 1), (1, 0, 0, 0, 0)])
    elif attribute == "width":
        entity.dxf.const_width = 1
    else:
        entity.dxf.thickness = 1
    doc.saveas(path)
    with pytest.raises(CadReadError, match=attribute):
        read_cad(path)


def test_empty_modelspace_is_refused(tmp_path):
    doc, path = drawing(tmp_path)
    doc.saveas(path)
    with pytest.raises(CadReadError, match="empty"):
        read_cad(path)


def test_nonfinite_coordinates_are_refused(tmp_path):
    doc, path = drawing(tmp_path)
    doc.modelspace().add_point((math.nan, 0, 0))
    doc.saveas(path)
    with pytest.raises(CadReadError, match="finite"):
        read_cad(path)


def test_malformed_dxf(tmp_path):
    path = tmp_path / "broken.dxf"
    path.write_text("not a drawing", encoding="utf-8")
    with pytest.raises(CadReadError, match="DXF"):
        read_cad(path)


def test_polyline2d_ocs_elevation(tmp_path):
    """Hand: xy points (0,0),(1000,2000) at z=3000 mm -> (0,0,3),(1,2,3) m.

    NIST SI Prefixes, milli row; ezdxf 1.4.5 POLYLINE points_in_wcs.
    """
    doc, path = drawing(tmp_path)
    doc.modelspace().add_polyline2d(
        [(0, 0), (1000, 2000)], dxfattribs={"elevation": (0, 0, 3000)}
    )
    doc.saveas(path)
    assert read_cad(path).points == ((0, 0, 3), (1, 2, 3))


@pytest.mark.parametrize("feature", ["bulge", "width", "fit", "mesh"])
def test_legacy_polyline_features_are_not_discarded(tmp_path, feature):
    doc, path = drawing(tmp_path)
    if feature == "mesh":
        doc.modelspace().add_polymesh((2, 2))
    else:
        entity = doc.modelspace().add_polyline2d([(0, 0), (1000, 2000)])
        if feature == "bulge":
            entity.vertices[0].dxf.bulge = 1
        elif feature == "width":
            entity.dxf.default_start_width = 1
        else:
            from ezdxf.lldxf.const import POLYLINE_CURVE_FIT_VERTICES_ADDED

            entity.dxf.flags |= POLYLINE_CURVE_FIT_VERTICES_ADDED
    doc.saveas(path)
    with pytest.raises(CadReadError):
        read_cad(path)
