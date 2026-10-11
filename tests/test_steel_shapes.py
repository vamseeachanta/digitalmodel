"""Offline behavioural checks against cited AISC v16 sample rows.

Source locators and retrieval date are in steel_shapes_provenance.yaml.
Numerical resolution reuses tests/test_sections.py (fillets and circles).
"""

import copy
import json
import math
import socket
from importlib.resources import files

import pytest

from digitalmodel.steel_shapes import (
    get_section,
    get_section_data,
    list_sections,
    validate_catalog,
)

# Independently hand-entered Database v16.0 rows; columns F, G/I/K,
# L/N/O, Q, T/V/X, Y and CH. Dimensions: in; area: in².
REFERENCE = [
    (
        "W14X90",
        "W360X134",
        26.5,
        {"d": 14, "bf": 14.5, "tw": 0.44, "tf": 0.71, "kdes": 1.31},
    ),
    (
        "W12X65",
        "W310X97",
        19.1,
        {"d": 12.1, "bf": 12, "tw": 0.39, "tf": 0.605, "kdes": 1.2},
    ),
    (
        "HP12X53",
        "HP310X79",
        15.5,
        {"d": 11.8, "bf": 12, "tw": 0.435, "tf": 0.435, "kdes": 1.13},
    ),
    (
        "S12X35",
        "S310X52",
        10.2,
        {"d": 12, "bf": 5.08, "tw": 0.428, "tf": 0.544, "kdes": 1.19},
    ),
    (
        "C9X20",
        "C230X30",
        5.87,
        {"d": 9, "bf": 2.65, "tw": 0.448, "tf": 0.413, "kdes": 1},
    ),
    ("L4X4X1/2", "L102X102X12.7", 3.75, {"d": 4, "b": 4, "t": 0.5, "kdes": 0.875}),
    (
        "HSS8X6X3/8",
        "HSS203.2X152.4X9.5",
        8.97,
        {"d": 8, "b": 6, "t": 0.349, "tnom": 0.375},
    ),
    ("HSS6.625X0.280", "HSS168.3X7.1", 5.2, {"d": 6.63, "t": 0.26, "tnom": 0.28}),
    ("Pipe6STD", "Pipe150STD", 5.2, {"d": 6.625, "t": 0.261, "tnom": 0.28}),
]


@pytest.fixture(autouse=True)
def no_network(monkeypatch):
    def denied(*args, **kwargs):
        raise AssertionError("steel shapes tests shall remain offline")

    monkeypatch.setattr(socket.socket, "connect", denied)


@pytest.fixture
def catalog():
    resource = files("digitalmodel.data_systems.data").joinpath(
        "steel_shapes_sample.json"
    )
    return json.loads(resource.read_text())


@pytest.mark.parametrize("imperial,metric,area,dimensions", REFERENCE)
def test_source_values_and_aliases(imperial, metric, area, dimensions):
    data = get_section_data(imperial)
    assert data == get_section_data(metric)
    assert data["area"] == area
    assert data["dimensions"] == dimensions
    assert data["source"]["sheet"] == "Database v16.0"


@pytest.mark.parametrize("name", ["w14x90", " W14 X 90 ", "w360 × 134", "W360X134"])
def test_normalization(name):
    assert get_section_data(name)["name"] == "W14X90"


def test_edi_and_manual_names():
    assert get_section_data("HSS8X6X.375") == get_section_data("HSS8X6X3/8")
    assert get_section_data("Pipe6SCH40") == get_section_data("Pipe6STD")


def test_unknown_name_and_bad_input():
    with pytest.raises(KeyError, match="Unknown steel section"):
        get_section_data("unknown")
    with pytest.raises(ValueError, match="non-empty"):
        get_section_data("  ")
    with pytest.raises(TypeError, match="string"):
        get_section_data(None)
    with pytest.raises(ValueError, match="unit"):
        get_section("W14X90", unit="unknown")


def test_caller_mutation_does_not_change_catalog():
    data = get_section_data("W14X90")
    data["dimensions"].clear()
    assert get_section_data("W14X90")["dimensions"] == REFERENCE[0][3]


def test_family_listing():
    assert set(list_sections("W")) == {r[0] for r in REFERENCE if r[0].startswith("W")}
    assert set(list_sections()) == {r[0] for r in REFERENCE}


def _area_allowance(imperial, dimensions):
    # Circular-root idealisation allowance: square minus quarter circle,
    # with side (kdes-thickness), plus source area rounding. This is a nominal
    # model comparison criterion; original rolled-outline validity is separate.
    # The workbook display format is retained: 5.20 must not become a one-place
    # tolerance merely because JSON serializes its numerical value as 5.2.
    number_format = get_section_data(imperial)["source"]["area_number_format"]
    rounding = (10 ** -len(number_format.split(".")[1])) / 2
    if "kdes" in dimensions:
        thickness = dimensions.get("tf", dimensions.get("t"))
        roots = 1 if imperial.startswith("L") else 2 if imperial.startswith("C") else 4
        allowance = (
            roots * (1 - math.pi / 4) * (dimensions["kdes"] - thickness) ** 2 + rounding
        )
    elif "X" in imperial and imperial.count("X") == 2:
        # HSS: bound nominal-vs-design wall corner idealisation by the area
        # change between outer radii 2*t and 2*tnom; no area fitting.
        allowance = (4 - math.pi) * 4 * dimensions["t"] * (
            dimensions["tnom"] - dimensions["t"]
        ) + rounding
    else:
        n = 64  # Existing compute_chs circle resolution.
        analytic = math.pi * dimensions["t"] * (dimensions["d"] - dimensions["t"])
        allowance = (
            analytic * (1 - n * math.sin(2 * math.pi / n) / (2 * math.pi)) + rounding
        )
    return allowance


@pytest.mark.parametrize("imperial,metric,area,dimensions", REFERENCE)
def test_geometry_area_and_metric_scaling(imperial, metric, area, dimensions):
    pytest.importorskip("sectionproperties")
    from sectionproperties.pre.geometry import Geometry

    geom = get_section(imperial, unit="imperial")
    metric_geom = get_section(metric)
    assert isinstance(geom, Geometry)
    assert geom.geom.is_valid and not geom.geom.is_empty
    width = dimensions.get("bf", dimensions.get("b", dimensions["d"]))
    x_min, y_min, x_max, y_max = geom.geom.bounds
    # Native library placement differs by family; physical extents do not.
    assert (x_max - x_min, y_max - y_min) == pytest.approx((width, dimensions["d"]))
    factor = 25.4  # Existing scripts/integrations/aisc_section_lookup.py.
    assert metric_geom.geom.area == pytest.approx(geom.geom.area * factor**2)
    assert abs(geom.geom.area - area) <= _area_allowance(imperial, dimensions)


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True, "14"])
def test_reject_invalid_dimensions(catalog, value):
    catalog["sections"][0]["dimensions"]["d"] = value
    with pytest.raises(ValueError, match="dimension"):
        validate_catalog(catalog)


def test_reject_alias_collision(catalog):
    duplicate = copy.deepcopy(catalog["sections"][0])
    duplicate["name"] = "duplicate"
    catalog["sections"].append(duplicate)
    with pytest.raises(ValueError, match="alias"):
        validate_catalog(catalog)


def test_cross_system_collision_requires_explicit_system(catalog, tmp_path):
    # Synthetic name collision reuses fixture names/values; no engineering input.
    first, second = catalog["sections"][:2]
    second["metric_name"] = first["name"]
    path = tmp_path / "ambiguous.json"
    path.write_text(json.dumps(catalog))
    with pytest.raises(KeyError, match="Ambiguous"):
        get_section_data(first["name"], catalog_path=path)
    assert (
        get_section_data(first["name"], name_system="imperial", catalog_path=path)
        == first
    )
    assert (
        get_section_data(first["name"], name_system="metric", catalog_path=path)
        == second
    )
    with pytest.raises(ValueError, match="name_system"):
        get_section_data(first["name"], name_system="unknown", catalog_path=path)


def test_reject_impossible_geometry(catalog):
    dims = catalog["sections"][0]["dimensions"]
    dims["tf"] = dims["d"]
    with pytest.raises(ValueError, match="geometry"):
        validate_catalog(catalog)


def test_external_catalog_and_missing_path(catalog, tmp_path):
    path = tmp_path / "catalog.json"
    path.write_text(json.dumps(catalog))
    assert get_section_data("W360X134", catalog_path=path)["area"] == REFERENCE[0][2]
    with pytest.raises(FileNotFoundError):
        get_section_data("W14X90", catalog_path=tmp_path / "missing.json")


def test_missing_geometry_dependency(monkeypatch):
    import builtins

    original = builtins.__import__

    def without_sectionproperties(name, *args, **kwargs):
        if name.startswith("sectionproperties"):
            raise ImportError("missing dependency")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_sectionproperties)
    assert get_section_data("W14X90")["area"] == REFERENCE[0][2]
    with pytest.raises(ImportError, match="sectionproperties"):
        get_section("W14X90")
