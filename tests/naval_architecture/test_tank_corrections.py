"""Fixed-attitude correction tables, checked by independent hand arithmetic.

All rows are synthetic, not tank or liquid properties. Interpolation uses
OpenStax, Precalculus 2e (2021), section 2.1, two-point lines. The signed
correction is added to the declared reading before reference-volume lookup.
No engineering tolerance or empirical coefficient is adopted.
"""

import math

import pytest

from digitalmodel.naval_architecture.tank_gauging import CalibrationTable


def corrected_table(kind="sounding", **overrides):
    """Box area 40 m²; selected fixed slopes are metadata, not defaults."""
    kwargs = dict(
        points=((0, 0), (1, 40), (3, 120)),
        reading_kind=kind,
        reference_height_m=3,
        trim_corrections_m=(-0.1, -0.2, -0.4),
        list_corrections_m=(0.05, 0.1, 0.2),
        correction_attitude_deg=(1, -2),
    )
    if kind == "ullage":
        kwargs["points"] = ((0, 120), (1, 80), (3, 0))
    kwargs.update(overrides)
    return CalibrationTable(**kwargs)


@pytest.mark.parametrize("kind,expected", [("sounding", 50), ("ullage", 70)])
def test_signed_corrections_interpolate_at_raw_reading(kind, expected):
    """At d=1.5, fraction=(1.5-1)/(3-1)=1/4.

    trim=-0.2+(-0.4+0.2)/4=-0.25; list=0.1+(0.2-0.1)/4=0.125.
    Corrected d=1.375. Sounding V=40*1.375=55; ullage V=120-55=65.
    At d=1, corrected=0.9 gives sounding 36 or ullage 84.
    Reference inverse at V=50 (sounding) or V=70 (ullage) remains d=1.25.
    """
    table = corrected_table(kind)
    assert table.corrected_depth_at(1.5, trim_deg=1, list_deg=-2) == 1.375
    volume = 55 if kind == "sounding" else 65
    assert table.volume_at_gauge(1.5, trim_deg=1, list_deg=-2) == volume
    assert table.volume_at(1.5) == 60  # Reference lookup remains uncorrected.
    assert table.depth_at_volume(expected) == 1.25
    assert table.corrected_depth_at(1, trim_deg=1, list_deg=-2) == 0.9


def test_correction_csv_round_trip(tmp_path):
    path = tmp_path / "table.csv"
    path.write_text(
        "depth_m,volume_m3,trim_correction_m,list_correction_m\n"
        "0,0,-0.1,0.05\n1,40,-0.2,0.1\n3,120,-0.4,0.2\n",
        encoding="utf-8",
        newline="\n",
    )
    table = CalibrationTable.from_csv(
        path,
        reading_kind="sounding",
        reference_height_m=3,
        correction_attitude_deg=(1, -2),
    )
    assert table.volume_at_gauge(1.5, trim_deg=1, list_deg=-2) == 55


def test_correction_yaml_with_one_column(tmp_path):
    """At d=1.5, a constant +0.1 m ullage offset gives V=120-40*1.6=56."""
    path = tmp_path / "table.yaml"
    path.write_text(
        "reading_kind: ullage\nreference_height_m: 3\n"
        "correction_attitude_deg: [1, 0]\nrows:\n"
        "  - {depth_m: 0, volume_m3: 120, trim_correction_m: 0.1}\n"
        "  - {depth_m: 3, volume_m3: 0, trim_correction_m: 0.1}\n",
        encoding="utf-8",
        newline="\n",
    )
    table = CalibrationTable.from_yaml(path)
    assert table.volume_at_gauge(1.5, trim_deg=1, list_deg=0) == pytest.approx(56)
    assert table.list_corrections_m is None


@pytest.mark.parametrize(
    "attitude",
    [
        pytest.param("10", id="text"),
        pytest.param(b"10", id="bytes"),
        pytest.param(bytearray(b"10"), id="bytearray"),
        pytest.param({"1": 12, "0": 34}, id="mapping"),
        pytest.param({1, 0}, id="set"),
        pytest.param(iter((1, 0)), id="iterator"),
        pytest.param(10, id="scalar"),
        pytest.param([], id="empty"),
        pytest.param([1], id="short"),
        pytest.param([1, 0, 2], id="long"),
    ],
)
def test_correction_attitude_requires_two_element_sequence(attitude):
    with pytest.raises(ValueError, match="correction attitude.*trim/list pair"):
        corrected_table(correction_attitude_deg=attitude)


@pytest.mark.parametrize(
    "attitude_yaml",
    ['"10"', "!!binary MTA=", '{"1": 12, "0": 34}', "10", "[]", "[1]", "[1, 0, 2]"],
)
def test_yaml_refuses_malformed_correction_attitude(tmp_path, attitude_yaml):
    path = tmp_path / "table.yaml"
    path.write_text(
        "reading_kind: ullage\nreference_height_m: 3\n"
        f"correction_attitude_deg: {attitude_yaml}\nrows:\n"
        "  - {depth_m: 0, volume_m3: 120, trim_correction_m: 0.1}\n"
        "  - {depth_m: 3, volume_m3: 0, trim_correction_m: 0.1}\n",
        encoding="utf-8",
        newline="\n",
    )
    with pytest.raises(ValueError, match="correction attitude.*trim/list pair"):
        CalibrationTable.from_yaml(path)


@pytest.mark.parametrize("attitude", [(1, -2), [1, -2], ("1", "-2"), ["1", "-2"]])
def test_correction_attitude_normalizes_list_and_tuple_pairs(attitude):
    table = corrected_table(correction_attitude_deg=attitude)
    assert table.correction_attitude_deg == (1.0, -2.0)
    # At d=1.5, offsets -0.25+0.125 give d_ref=1.375; V=40*1.375=55.
    assert table.volume_at_gauge(1.5, trim_deg=1, list_deg=-2) == pytest.approx(55)


@pytest.mark.parametrize("angles", [(0, -2), (1, 2), (math.nan, -2)])
def test_attitude_must_match_qualified_case(angles):
    with pytest.raises(ValueError):
        corrected_table().volume_at_gauge(1.5, trim_deg=angles[0], list_deg=angles[1])


@pytest.mark.parametrize("depth", [-1, 4, math.nan, 0])
def test_raw_and_corrected_depth_bounds(depth):
    # The zero raw depth is valid, but its negative corrected depth is not.
    with pytest.raises(ValueError):
        corrected_table().volume_at_gauge(depth, trim_deg=1, list_deg=-2)


def test_corrected_lookup_refuses_partial_table_extrapolation():
    table = CalibrationTable(
        ((1, 40), (2, 80)),
        "sounding",
        3,
        trim_corrections_m=(-0.2, -0.2),
        correction_attitude_deg=(1, 0),
    )
    assert table.corrected_depth_at(1, trim_deg=1, list_deg=0) == 0.8
    with pytest.raises(ValueError):
        table.volume_at_gauge(1, trim_deg=1, list_deg=0)


@pytest.mark.parametrize(
    "overrides",
    [
        {"trim_corrections_m": (0,)},
        {"list_corrections_m": (0, math.nan, 0)},
        {"correction_attitude_deg": None},
        {"correction_attitude_deg": (1,)},
        {"correction_attitude_deg": (math.inf, 0)},
        {"correction_attitude_deg": (90, 0)},
        {"trim_corrections_m": None, "list_corrections_m": None},
    ],
)
def test_invalid_correction_metadata(overrides):
    with pytest.raises(ValueError):
        corrected_table(**overrides)


def test_reference_table_requires_explicit_geometric_correction():
    table = CalibrationTable(((0, 0), (3, 120)), "sounding", 3)
    with pytest.raises(ValueError):
        table.volume_at_gauge(1.5, trim_deg=0, list_deg=0)


def test_inconsistent_csv_columns_are_rejected(tmp_path):
    path = tmp_path / "table.csv"
    path.write_text(
        "depth_m,volume_m3,trim_correction_m\n0,0,0\n3,120,\n",
        encoding="utf-8",
        newline="\n",
    )
    with pytest.raises(ValueError):
        CalibrationTable.from_csv(
            path,
            reading_kind="sounding",
            reference_height_m=3,
            correction_attitude_deg=(1, 0),
        )


def test_one_degree_box_correction_matches_hand_check():
    """OpenStax Precalculus 2e (2021), §5.4: opposite=adjacent*tan(theta).

    For synthetic x=+2 m and trim=+1 degree, tan(1)=0.0174550649282176
    by an independent calculator. The sounding offset is -0.0349101298564352 m;
    ullage uses its negative because u=H-h. At gauge h=u=1.5 m, centre
    h=1.4650898701435648, centre u=1.5349101298564352 and box V=40*h
    =58.603594805742592 m³. Both table kinds must return the same volume.
    """
    offset = 0.0349101298564352
    for kind, points, correction in (
        ("sounding", ((0, 0), (3, 120)), -offset),
        ("ullage", ((0, 120), (3, 0)), offset),
    ):
        table = CalibrationTable(
            points,
            kind,
            3,
            trim_corrections_m=(correction, correction),
            correction_attitude_deg=(1, 0),
        )
        assert table.volume_at_gauge(1.5, trim_deg=1, list_deg=0) == pytest.approx(
            58.603594805742592
        )


def test_imported_corrections_are_immutable():
    corrections = [0, 0, 0]
    attitude = [1, 0]
    table = corrected_table(
        trim_corrections_m=corrections,
        list_corrections_m=None,
        correction_attitude_deg=attitude,
    )
    corrections[1] = 1
    attitude[0] = 2
    assert table.corrected_depth_at(1.5, trim_deg=1, list_deg=0) == 1.5


@pytest.mark.parametrize(
    "contents",
    [
        "reading_kind: sounding\nreference_height_m: 3\n"
        "correction_attitude_deg: [1, 0]\nrows:\n"
        "  - {depth_m: 0, volume_m3: 0, trim_correction_m: 0}\n"
        "  - {depth_m: 3, volume_m3: 120}\n",
        "reading_kind: sounding\nreference_height_m: 3\n"
        "correction_attitude_deg: [1, 0]\nrows:\n"
        "  - {depth_m: 0, volume_m3: 0}\n"
        "  - {depth_m: 3, volume_m3: 120}\n",
        "reading_kind: sounding\nreference_height_m: 3\n"
        "correction_attitude_deg: [1, 0]\nrows:\n"
        "  - {depth_m: 0, volume_m3: 0, trim_correction_m: 0, trim_correction_m: 1}\n"
        "  - {depth_m: 3, volume_m3: 120, trim_correction_m: 0}\n",
    ],
)
def test_yaml_refuses_inconsistent_or_duplicate_correction_data(tmp_path, contents):
    path = tmp_path / "table.yaml"
    path.write_text(contents, encoding="utf-8", newline="\n")
    with pytest.raises(ValueError):
        CalibrationTable.from_yaml(path)
