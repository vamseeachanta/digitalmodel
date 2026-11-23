"""Tests for vessel DB loading, normalisation, and provenance integrity."""

from __future__ import annotations

import pytest

from digitalmodel.marine_ops.vessel_db.loader import (
    Record,
    datasets,
    iter_records,
    load_crane_curves,
    parse_value,
    validate_provenance,
    vessels_dir,
)


@pytest.mark.parametrize(
    "value,expected_num,expected_marker",
    [
        (320.0, 320.0, "number"),
        (10000, 10000.0, "number"),
        ("320.0 m", 320.0, "number"),
        ("~27000", 27000.0, "number"),
        ("10,000 t", 10000.0, "number"),
        ("gap", None, "gap"),
        ("estimated:kxx=0.35*beam", None, "estimated"),
        ("9 headings", None, "text"),          # digit-led but descriptive
        ("1:50 (MARIN model)", None, "text"),  # ratio, not a number
        ("6750-TEU containership", None, "text"),
        ("~26 (typical)", None, "text"),       # parenthetical qualifier
        ("180 (head); oblique 30-150", None, "text"),
        (True, None, "text"),
    ],
)
def test_parse_value_strict(value, expected_num, expected_marker):
    num, marker = parse_value(value)
    assert marker == expected_marker
    if expected_num is None:
        assert num is None
    else:
        assert num == pytest.approx(expected_num)


def test_field_name_normalisation():
    rec = Record(
        name="X", scope="floating", layer="particulars",
        raw_fields={"loa_m": 250.0, "breadth_m": 40.0, "draught_m": 12.0, "block_coefficient": 0.85},
    )
    dims = rec.canonical_dimensions()
    assert dims["loa"] == 250.0
    assert dims["beam"] == 40.0
    assert dims["draft"] == 12.0
    assert dims["cb"] == 0.85


def test_vessel_id_is_stable_slug():
    rec = Record(name="FPSO P-50 Class!", scope="floating", layer="particulars")
    assert rec.vessel_id() == "floating_fpso_p_50_class"


# ----- integration against the real curated database -----

def test_database_present():
    base = vessels_dir()
    assert (base / "raw").is_dir()
    assert datasets(base), "no datasets discovered under raw/"


def test_provenance_is_clean():
    """The whole curated database must satisfy 'flag, don't fake'."""
    violations = validate_provenance()
    assert violations == [], (
        f"{len(violations)} un-cited hard numbers: "
        + "; ".join(f"{v.record}.{v.field_name}={v.value}" for v in violations[:5])
    )


def test_every_dataset_loads():
    for scope, layer in datasets():
        recs = iter_records(scope, layer)
        assert recs, f"{scope}/{layer} has no records"
        for r in recs:
            assert r.name and r.name != "?"


def test_crane_curves_build():
    curves = load_crane_curves()
    assert curves, "no crane curves built"
    # Sleipnir publishes 10,000 t SWL — sanity check magnitude.
    swls = [c.max_hook_load_te for c in curves.values()]
    assert max(swls) >= 10000.0
    # capacity_at_radius is callable and returns a number.
    any_curve = next(iter(curves.values()))
    assert isinstance(any_curve.capacity_at_radius(30.0), float)


def test_crane_curve_capacities_plausible():
    """No crane SWL should be absurd (< 50 te or > 60,000 te)."""
    for name, c in load_crane_curves().items():
        assert 50.0 <= c.max_hook_load_te <= 60000.0, f"{name}: {c.max_hook_load_te} te"
