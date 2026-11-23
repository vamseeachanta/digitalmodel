import pytest
from digitalmodel.gis import CoordinatePoint, transform_points, get_utm_crs

def test_coordinate_point_init():
    pt = CoordinatePoint(x=-1.5, y=57.0)
    assert pt.x == -1.5
    assert pt.y == 57.0
    assert pt.crs == "EPSG:4326"

def test_coordinate_point_transform():
    # North Sea point (roughly)
    pt = CoordinatePoint(x=1.0, y=56.0, crs="EPSG:4326")
    # Transform to UTM Zone 31N
    pt_utm = pt.to_crs("EPSG:32631")
    assert pt_utm.crs == "EPSG:32631"
    assert pt_utm.x != pt.x
    assert pt_utm.y != pt.y

def test_batch_transform():
    pts = [
        CoordinatePoint(x=0.0, y=0.0),
        CoordinatePoint(x=1.0, y=1.0)
    ]
    transformed = transform_points(pts, "EPSG:3857")
    assert len(transformed) == 2
    for pt in transformed:
        assert pt.crs == "EPSG:3857"

def test_get_utm_crs():
    # London (roughly)
    crs = get_utm_crs(0.1, 51.5)
    assert crs == "EPSG:32631"  # UTM 31N
    
    # New Orleans (roughly)
    crs = get_utm_crs(-90.0, 30.0)
    assert crs == "EPSG:32616"  # UTM 16N
    
    # Rio (roughly)
    crs = get_utm_crs(-43.2, -22.9)
    assert crs == "EPSG:32723"  # UTM 23S

def test_invalid_crs():
    with pytest.raises(ValueError):
        CoordinatePoint(x=0, y=0, crs="INVALID")
