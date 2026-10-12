"""Offline regression checks for hand-written synthetic profiles only."""

import copy
import json
import os
import socket
from importlib.resources import files

import pytest

import digitalmodel.steel_shapes as shapes
from digitalmodel.steel_shapes import get_section, get_section_data, list_sections, validate_catalog

REFERENCE = [
    ("W", {"d": 10, "bf": 8, "tw": .5, "tf": 1, "kdes": 1.5}),
    ("HP", {"d": 8, "bf": 8, "tw": .75, "tf": .75, "kdes": 1.25}),
    ("S", {"d": 10, "bf": 5, "tw": .5, "tf": .75, "kdes": 1.5}),
    ("C", {"d": 8, "bf": 3, "tw": .5, "tf": .75, "kdes": 1.25}),
    ("L", {"d": 5, "b": 5, "t": .75, "kdes": 1.25}),
    ("HSS_RECT", {"d": 10, "b": 6, "t": .5, "tnom": .5}),
    ("HSS_ROUND", {"d": 5, "t": .5, "tnom": .5}),
    ("PIPE", {"d": 4, "t": .25, "tnom": .25}),
]


@pytest.fixture(autouse=True)
def no_network(monkeypatch):
    def denied(*args, **kwargs):
        raise AssertionError("steel shapes tests shall remain offline")
    monkeypatch.setattr(socket.socket, "connect", denied)


@pytest.fixture
def catalog():
    return json.loads(files("digitalmodel.data_systems.data").joinpath(
        "steel_shapes_sample.json").read_text())


@pytest.mark.parametrize("family,dimensions", REFERENCE)
def test_synthetic_values_and_aliases(catalog, family, dimensions):
    validate_catalog(catalog)
    assert catalog["synthetic"] is True
    data = get_section_data("SYN-" + family)
    assert data == get_section_data("SYN-METRIC-" + family)
    assert data == get_section_data("SYN-" + family + "XDEMO")
    assert data["dimensions"] == dimensions
    assert data["synthetic"] is True
    assert data["source"]["kind"] == "synthetic"


@pytest.mark.parametrize("name", ["syn-w", " SYN- W ", "syn-w × demo", "SYN-WXDEMO"])
def test_normalization(name):
    assert get_section_data(name)["name"] == "SYN-W"


@pytest.mark.parametrize("name", ["W10X49", "C12X30"])
def test_real_names_absent(name):
    with pytest.raises(KeyError, match="Unknown steel section"):
        get_section_data(name)


def test_unknown_name_and_bad_input():
    with pytest.raises(KeyError, match="Unknown steel section"):
        get_section_data("unknown")
    with pytest.raises(ValueError, match="non-empty"):
        get_section_data("  ")
    with pytest.raises(TypeError, match="string"):
        get_section_data(None)
    with pytest.raises(ValueError, match="unit"):
        get_section("SYN-W", unit="unknown")


def test_caller_mutation_does_not_change_catalog():
    data = get_section_data("SYN-W")
    data["dimensions"].clear()
    data["aliases"].clear()
    data["source"].clear()
    assert get_section_data("SYN-W")["dimensions"] == REFERENCE[0][1]
    assert get_section_data("SYN-W")["aliases"]
    assert get_section_data("SYN-W")["source"]


def test_family_listing():
    assert list_sections("W") == ["SYN-W"]
    assert set(list_sections()) == {"SYN-" + family for family, _ in REFERENCE}


@pytest.mark.parametrize("family,dimensions", REFERENCE)
def test_geometry_area_and_metric_scaling(family, dimensions):
    from sectionproperties.pre.geometry import Geometry
    geom = get_section("SYN-" + family, unit="imperial")
    metric_geom = get_section("SYN-METRIC-" + family)
    assert isinstance(geom, Geometry)
    assert geom.geom.is_valid and not geom.geom.is_empty
    width = dimensions.get("bf", dimensions.get("b", dimensions["d"]))
    x_min, y_min, x_max, y_max = geom.geom.bounds
    assert (x_max - x_min, y_max - y_min) == pytest.approx((width, dimensions["d"]))
    assert metric_geom.geom.area == pytest.approx(geom.geom.area * 25.4**2)
    assert geom.geom.area == pytest.approx(get_section_data("SYN-" + family)["area"], rel=1e-12)


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True, "10"])
def test_reject_invalid_dimensions(catalog, value):
    catalog["sections"][0]["dimensions"]["d"] = value
    with pytest.raises(ValueError, match="dimension"):
        validate_catalog(catalog)


def test_reject_alias_collision(catalog):
    duplicate = copy.deepcopy(catalog["sections"][0])
    duplicate["name"] = "SYN-DUPLICATE"
    catalog["sections"].append(duplicate)
    with pytest.raises(ValueError, match="alias"):
        validate_catalog(catalog)


def test_cross_system_collision_requires_explicit_system(catalog, tmp_path):
    first, second = catalog["sections"][:2]
    second["metric_name"] = first["name"]
    path = tmp_path / "ambiguous.json"
    path.write_text(json.dumps(catalog))
    with pytest.raises(KeyError, match="Ambiguous"):
        get_section_data(first["name"], catalog_path=path)
    assert get_section_data(first["name"], name_system="imperial", catalog_path=path) == first
    assert get_section_data(first["name"], name_system="metric", catalog_path=path) == second
    with pytest.raises(ValueError, match="name_system"):
        get_section_data(first["name"], name_system="unknown", catalog_path=path)


def test_reject_impossible_geometry(catalog):
    catalog["sections"][0]["dimensions"]["tf"] = 10
    with pytest.raises(ValueError, match="geometry"):
        validate_catalog(catalog)


def test_external_catalog_and_missing_path(catalog, tmp_path):
    path = tmp_path / "catalog.json"
    path.write_text(json.dumps(catalog))
    assert get_section_data("SYN-METRIC-W", catalog_path=path) == catalog["sections"][0]
    with pytest.raises(FileNotFoundError):
        get_section_data("SYN-W", catalog_path=tmp_path / "missing.json")


def test_missing_geometry_dependency(monkeypatch):
    import builtins
    original = builtins.__import__
    def without_sectionproperties(name, *args, **kwargs):
        if name.startswith("sectionproperties"):
            raise ImportError("missing dependency")
        return original(name, *args, **kwargs)
    monkeypatch.setattr(builtins, "__import__", without_sectionproperties)
    assert get_section_data("SYN-W")["dimensions"] == REFERENCE[0][1]
    with pytest.raises(ImportError, match="sectionproperties"):
        get_section("SYN-W")


@pytest.mark.parametrize("local", [False, True])
def test_catalog_validation_cache(catalog, tmp_path, monkeypatch, local):
    path = tmp_path / "catalog.json"
    path.write_text(json.dumps(catalog))
    source = path if local else None
    shapes.clear_catalog_cache()
    calls = []
    original = shapes.validate_catalog
    def counted(data):
        calls.append(data)
        return original(data)
    monkeypatch.setattr(shapes, "validate_catalog", counted)
    get_section_data("SYN-W", catalog_path=source)
    get_section("SYN-W", catalog_path=source)
    list_sections(catalog_path=source)
    get_section_data("SYN-METRIC-W", catalog_path=source)
    assert len(calls) == 1
    if local:
        before = path.stat()
        catalog["sections"][0]["dimensions"]["d"] = 11
        path.write_text(json.dumps(catalog))  # Same size: modification time alone.
        os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns + 1_000_000_000))
        assert path.stat().st_size == before.st_size
        assert get_section_data("SYN-W", catalog_path=path)["dimensions"]["d"] == 11
        assert len(calls) == 2
        before = path.stat()
        path.write_text(json.dumps(catalog) + "\n")  # Size alone: restore time.
        os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns))
        assert get_section_data("SYN-W", catalog_path=path)["dimensions"]["d"] == 11
        assert len(calls) == 3
    shapes.clear_catalog_cache()
    get_section_data("SYN-W", catalog_path=source)
    assert len(calls) == (4 if local else 2)


def test_synthetic_geometry_warning():
    with pytest.warns(UserWarning, match="Synthetic steel section"):
        get_section("SYN-W")


@pytest.mark.parametrize("location", ["catalog", "row", "name"])
def test_synthetic_schema_enforces_markers(catalog, location):
    if location == "catalog":
        del catalog["synthetic"]
    elif location == "row":
        del catalog["sections"][0]["synthetic"]
    else:
        catalog["sections"][0]["name"] = "UNMARKED"
    with pytest.raises(ValueError, match="metadata"):
        validate_catalog(catalog)
