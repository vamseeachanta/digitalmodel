"""Exact synthetic reader examples for riser-w5-channels/1 (channels.py).

The hand-written tables are the oracle; no solver or writer builds the document.
Loads are N and N.m, arc length and stroke are m, time is s, density is kg/m³.
These are schema/selection checks, not engineering acceptance limits.
"""

from copy import deepcopy

import pytest

from digitalmodel.drilling_riser.postprocess import channels as ch


@pytest.fixture
def dynamic_document():
    """Three coincident load rows at each of two points.

    Upper: (Te, Mx)=(100,2), (20,-7), (70,8) at t=1,2,3 s.
    Lower: (Te, Mx)=(90,3), (10,-6), (60,9) at t=1,2,3 s.
    The maximum moment occurs at t=3, not the maximum tension at t=1.
    Along the line, tied Te maxima 140 N and minima 10 N occur at 5 and
    10 m. First occurrence wins, as with Python max/min and the accessor's
    forward row scan. Stroke spans 5-(-2)=7 m; its recorded mean is 1 m.
    """
    return {
        "channels": {
            "w5": {
                "schema": "riser-w5-channels/1",
                "analysis": "dynamics",
                "points": {
                    "upper": {
                        "stats": {"te": {"min": 20, "max": 100, "mean": 60}},
                        "extremes": [
                            {"t": 1, "driver": "te", "kind": "max", "te": 100, "mx": 2},
                            {"t": 2, "driver": "te", "kind": "min", "te": 20, "mx": -7},
                            {"t": 3, "driver": "mx", "kind": "max", "te": 70, "mx": 8},
                        ],
                        "tm_hull": None,
                    },
                    "lower": {
                        "stats": {"te": {"min": 10, "max": 90, "mean": 50}},
                        "extremes": [
                            {"t": 1, "driver": "te", "kind": "max", "te": 90, "mx": 3},
                            {"t": 2, "driver": "te", "kind": "min", "te": 10, "mx": -6},
                            {"t": 3, "driver": "mx", "kind": "max", "te": 60, "mx": 9},
                        ],
                    },
                },
                "range_graphs": {
                    "riser": {
                        "arc1_m": [0, 5, 10],
                        "te_max": [100, 140, 140],
                        "te_min": [20, 10, 10],
                    }
                },
                "stroke": {"stats": {"min": -2, "max": 5, "mean": 1}},
                "contents": {"density_kg_m3": 1200, "pressure_ref_z_m": 4},
            }
        }
    }


@pytest.fixture
def static_channels():
    """Two static points and a three-station graph; no dynamic metadata."""
    return {
        "analysis": "statics",
        "points": {
            "upper": {"static": {"te": 80, "mx": -4}},
            "lower": {"static": {"te": 30, "mx": 6}},
        },
        "range_graphs": {"riser": {"arc1_m": [0, 5, 10], "te": [80, 30, 50]}},
        "stroke": {"static": 2},
        "contents": {"density_kg_m3": 0, "pressure_ref_z_m": 0},
    }


@pytest.mark.parametrize(
    "point, expected", [("upper", [20, 100, 60]), ("lower", [10, 90, 50])]
)
def test_point_statistics_and_document_selection(dynamic_document, point, expected):
    w = ch.w5(dynamic_document)
    assert w is dynamic_document["channels"]["w5"]
    assert ch.is_dynamic(w) is True
    assert [
        ch.point_value(w, point, "te", stat) for stat in ("min", "max", "mean")
    ] == expected


@pytest.mark.parametrize("point, force, moment", [("upper", 100, 8), ("lower", 90, 9)])
def test_coincident_extreme_keeps_driver_and_companion_loads(
    dynamic_document, point, force, moment
):
    w = ch.w5(dynamic_document)
    rows = ch.point_rows(w, point)
    assert [r["t"] for r in rows] == [1, 2, 3]
    tension_row = ch.extreme_row(w, point, "te", "max")
    moment_row = ch.extreme_row(w, point, "mx", "max")
    assert tension_row is rows[0]
    assert tension_row["driver"] == "te"
    assert ch.row_value(tension_row, point, "te") == force
    assert moment_row is rows[2]
    assert moment_row["driver"] == "mx"
    assert ch.row_value(moment_row, point, "mx") == moment
    assert ch.row_value(moment_row, point, "te") == force - 30
    assert ch.extreme_row(w, point, "mx", "min") is None


def test_rows_append_hull_and_extreme_lookup_keeps_first_match(dynamic_document):
    w = ch.w5(dynamic_document)
    first = w["points"]["upper"]["extremes"][0]
    tied = {**first, "t": 4, "mx": 99}
    w["points"]["upper"]["extremes"].append(tied)
    hull = {"t": 5, "te": 40, "mx": 4}
    w["points"]["upper"]["tm_hull"] = [hull]
    assert ch.extreme_row(w, "upper", "te", "max") is first
    assert [r["t"] for r in ch.point_rows(w, "upper")] == [1, 2, 3, 4, 5]
    assert ch.row_value(ch.point_rows(w, "upper")[-1], "upper", "te") == 40


def test_dynamic_range_extrema_and_stroke_span(dynamic_document):
    w = ch.w5(dynamic_document)
    assert ch.range_series(w, "riser", "te", "max") == [(0, 100), (5, 140), (10, 140)]
    assert ch.range_series(w, "riser", "te", "min") == [(0, 20), (5, 10), (10, 10)]
    assert ch.range_extreme(w, "riser", "te", "max") == (140, 5)
    assert ch.range_extreme(w, "riser", "te", "min") == (10, 5)
    low, high, mean = ch.stroke_range(w)
    assert (low, high, mean) == (-2, 5, 1)
    assert high - low == 7
    assert ch.contents(w) == {"density_kg_m3": 1200, "pressure_ref_z_m": 4}


def test_static_paths_and_empty_analysis_default(static_channels):
    w = static_channels
    assert ch.is_dynamic(w) is False
    for stat in ("min", "max", "mean"):
        assert ch.point_value(w, "upper", "te", stat) == 80
    assert ch.point_value(w, "lower", "te", "max") == 30
    assert ch.point_rows(w, "upper") == [{"te": 80, "mx": -4, "t": None}]
    assert ch.extreme_row(w, "upper", "te", "max") is None
    assert ch.range_series(w, "riser", "te", "max") == [(0, 80), (5, 30), (10, 50)]
    assert ch.range_extreme(w, "riser", "te", "min") == (30, 5)
    assert ch.stroke_range(w) == (2, 2, 2)
    assert ch.contents(w) == {"density_kg_m3": 0, "pressure_ref_z_m": 0}
    assert ch.is_dynamic({}) is False


def test_variable_axis_and_short_or_absent_axes(dynamic_document):
    w = ch.w5(dynamic_document)
    rg = w["range_graphs"]["riser"]
    rg.update(axis_of={"te_max": "special_m"}, special_m=[2, 7])
    assert ch.range_series(w, "riser", "te", "max") == [(2, 100), (7, 140), (None, 140)]
    # An empty mapped axis falls back to the common arc axis.
    rg["special_m"] = []
    assert ch.range_extreme(w, "riser", "te", "max") == (140, 5)
    del rg["arc1_m"]
    assert ch.range_series(w, "riser", "te", "min") == [
        (None, 20),
        (None, 10),
        (None, 10),
    ]
    assert ch.range_extreme(w, "riser", "te", "min") == (10, None)


@pytest.mark.parametrize(
    "document",
    [
        {},
        {"channels": {}},
        {"channels": None},
        {"channels": {"w5": {"schema": "unknown"}}},
    ],
)
def test_missing_document_or_wrong_schema_refused(document):
    if document == {"channels": {"w5": {"schema": "unknown"}}}:
        with pytest.raises(ValueError, match="unsupported channel schema"):
            ch.w5(document)
    else:
        with pytest.raises(ch.MissingChannel, match=r"channels\.w5"):
            ch.w5(document)


@pytest.mark.parametrize("schema", [None, "riser-w5-channels/1"])
def test_optional_schema_accepted(schema):
    assert ch.w5({"channels": {"w5": {"schema": schema}}}) == {"schema": schema}
    assert ch.w5({"channels": {"w5": {}}}) == {}


@pytest.mark.parametrize(
    "remove, accessor, message",
    [
        (
            ("points", "upper"),
            lambda w: ch.point_value(w, "upper", "te", "max"),
            "points.upper",
        ),
        (
            ("points", "upper", "stats", "te", "max"),
            lambda w: ch.point_value(w, "upper", "te", "max"),
            "points.upper.stats.te.max",
        ),
        (
            ("points", "upper", "extremes"),
            lambda w: ch.point_rows(w, "upper"),
            "points.upper.extremes",
        ),
        (
            ("range_graphs", "riser"),
            lambda w: ch.range_series(w, "riser", "te", "max"),
            "range_graphs.riser",
        ),
        (
            ("range_graphs", "riser", "te_max"),
            lambda w: ch.range_extreme(w, "riser", "te", "max"),
            "range_graphs.riser.te_max",
        ),
        (("stroke",), ch.stroke_range, "stroke"),
        (("stroke", "stats", "mean"), ch.stroke_range, "stroke.stats"),
        (("contents", "density_kg_m3"), ch.contents, "contents"),
        (("contents", "pressure_ref_z_m"), ch.contents, "contents"),
    ],
)
def test_missing_channels_name_the_exact_path(
    dynamic_document, remove, accessor, message
):
    w = deepcopy(ch.w5(dynamic_document))
    parent = w
    for key in remove[:-1]:
        parent = parent[key]
    del parent[remove[-1]]
    with pytest.raises(ch.MissingChannel) as error:
        accessor(w)
    assert error.value.channel == message
    assert str(error.value) == f"missing channel: {message}"


@pytest.mark.parametrize(
    "row, tag",
    [
        ({"t": None}, "static"),
        ({"t": 2}, "t=2"),
        ({"t": 2, "driver": "te", "kind": "min"}, "te.min"),
    ],
)
def test_missing_row_value_retains_driver_context(row, tag):
    with pytest.raises(ch.MissingChannel) as error:
        ch.row_value(row, "upper", "mx")
    assert error.value.channel == f"points.upper.{tag}.mx"


@pytest.mark.parametrize("key", ["stroke", "contents"])
def test_none_and_empty_required_channels(dynamic_document, key):
    w = ch.w5(dynamic_document)
    for missing in (None, {}):
        w[key] = missing
        with pytest.raises(ch.MissingChannel, match=key):
            (ch.stroke_range if key == "stroke" else ch.contents)(w)


def test_static_missing_paths(static_channels):
    w = static_channels
    del w["points"]["upper"]["static"]
    for reader in (
        lambda: ch.point_rows(w, "upper"),
        lambda: ch.point_value(w, "upper", "te", "max"),
    ):
        with pytest.raises(ch.MissingChannel, match=r"points\.upper\.static"):
            reader()
    w["stroke"] = {}
    with pytest.raises(ch.MissingChannel, match=r"stroke\.static"):
        ch.stroke_range(w)


@pytest.mark.parametrize(
    "reader",
    [
        lambda w: ch.point_value(w, "upper", "te", "max"),
        lambda w: ch.row_value({"te": None}, "upper", "te"),
    ],
)
def test_none_numeric_values_raise_type_error(dynamic_document, reader):
    """None numeric values are malformed data, distinct from absent keys."""
    w = ch.w5(dynamic_document)
    w["points"]["upper"]["stats"]["te"]["max"] = None
    with pytest.raises(TypeError, match="float.*NoneType"):
        reader(w)


def test_empty_range_has_no_extreme(dynamic_document):
    w = ch.w5(dynamic_document)
    w["range_graphs"]["riser"]["te_max"] = []
    assert ch.range_series(w, "riser", "te", "max") == []
    with pytest.raises(ValueError, match="empty"):
        ch.range_extreme(w, "riser", "te", "max")
