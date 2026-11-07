"""Publication projections preserve observations; no engineering model is solved."""

import pytest

from scripts.hf import build_capability_datasets as builder


def test_ipr_keeps_both_facets_and_every_curve_point(monkeypatch):
    """Hand projection: two pressure facets, one PI each, two points = four rows.

    All fixture numbers are arbitrary projection inputs, not model coefficients.
    Expected flow and pressure values are copied literally, without recomputation.
    """
    study = {
        "series": {
            "100": {
                "2.0": {"qmax": 9, "curve": [{"pwf": 100, "q": 0}, {"pwf": 0, "q": 9}]}
            },
            "200": {
                "2.0": {
                    "qmax": 18,
                    "curve": [{"pwf": 200, "q": 0}, {"pwf": 0, "q": 18}],
                }
            },
        }
    }
    monkeypatch.setattr(builder, "_load", lambda _: study)
    assert builder.ipr_rows() == [
        {
            "reservoir_pressure_psi": 100.0,
            "productivity_index_bopd_psi": 2.0,
            "flowing_pressure_psi": 100,
            "flow_rate_bopd": 0,
            "qmax_bopd": 9,
        },
        {
            "reservoir_pressure_psi": 100.0,
            "productivity_index_bopd_psi": 2.0,
            "flowing_pressure_psi": 0,
            "flow_rate_bopd": 9,
            "qmax_bopd": 9,
        },
        {
            "reservoir_pressure_psi": 200.0,
            "productivity_index_bopd_psi": 2.0,
            "flowing_pressure_psi": 200,
            "flow_rate_bopd": 0,
            "qmax_bopd": 18,
        },
        {
            "reservoir_pressure_psi": 200.0,
            "productivity_index_bopd_psi": 2.0,
            "flowing_pressure_psi": 0,
            "flow_rate_bopd": 18,
            "qmax_bopd": 18,
        },
    ]


@pytest.mark.parametrize(
    "function,study,expected",
    [
        (
            "anchor_holding_rows",
            {"series": {"sand|drag": [{"w": 3, "h": 7}]}},
            [
                {
                    "soil": "sand",
                    "anchor": "drag",
                    "weight_kn": 3,
                    "holding_capacity_kn": 7,
                }
            ],
        ),
        (
            "ship_resistance_rows",
            {"series": {"generic": [{"v": 3, "r": 7, "p": 9, "fn": 2}]}},
            [
                {
                    "hull": "generic",
                    "speed_knots": 3,
                    "resistance_kn": 7,
                    "effective_power_kw": 9,
                    "froude_number": 2,
                }
            ],
        ),
        (
            "viv_rows",
            {"series": {"member": [{"v": 3, "vr": 7, "status": "test", "sf": 9}]}},
            [
                {
                    "member": "member",
                    "current_speed_m_s": 3,
                    "reduced_velocity": 7,
                    "status": "test",
                    "safety_factor": 9,
                }
            ],
        ),
        (
            "pore_pressure_rows",
            {"series": [{"z": 3, "sv": 7, "eaton": 9, "bowers": 2, "pp": 9, "mw": 11}]},
            [
                {
                    "depth_ft": 3,
                    "overburden_ppg": 7,
                    "eaton_ppg": 9,
                    "bowers_ppg": 2,
                    "pore_pressure_ppg": 9,
                    "mud_weight_ppg": 11,
                }
            ],
        ),
        (
            "field_economics_rows",
            {"series": {"3": [{"p": 7, "npv": -9, "pb": None}]}},
            [
                {
                    "capex_musd": 3.0,
                    "oil_price_usd_bbl": 7,
                    "npv_musd": -9,
                    "payback_years": None,
                }
            ],
        ),
    ],
)
def test_additional_projections_preserve_units_facets_and_nulls(
    monkeypatch,
    function,
    study,
    expected,
):
    """Independent hand projection: each input leaf maps to one row.

    Arbitrary fixture values test field renaming; no physical relationship or
    acceptance threshold is asserted. None represents missing payback, not zero.
    Unit names follow the corresponding committed build_*_explorer.py generator.
    """
    monkeypatch.setattr(builder, "_load", lambda _: study)
    assert getattr(builder, function)() == expected


def test_existing_wall_projection_keeps_per_standard_checks():
    """Arbitrary values: two standards with disjoint checks must retain both."""
    doc = {
        "meta": {"od_mm": 9, "grade": "test"},
        "series": {
            "A": [{"w": 2, "u": 3, "g": "a", "safe": False, "checks": {"a": 3}}],
            "B": [{"w": 2, "u": 4, "checks": {"b": 4}}],
        },
    }
    rows = builder.wall_thickness_rows(doc)
    assert rows == [
        {
            "standard": "A",
            "wall_thickness_mm": 2,
            "utilisation": 3,
            "governing_check": "a",
            "safe": False,
            "check_a": 3,
            "od_mm": 9,
            "grade": "test",
        },
        {
            "standard": "B",
            "wall_thickness_mm": 2,
            "utilisation": 4,
            "governing_check": None,
            "safe": None,
            "check_b": 4,
            "od_mm": 9,
            "grade": "test",
        },
    ]


def test_existing_cp_projection_retains_all_three_facets():
    """One structure * one climate * two coatings * one life = two rows."""
    point = {"l": 3, "mass": 7, "n": 9, "im": 2, "if": 4}
    doc = {"series": {"structure": {"climate": {"bare": [point], "coated": [point]}}}}
    rows = builder.cathodic_protection_rows(doc)
    assert rows == [
        {
            "structure": "structure",
            "climate": "climate",
            "coating": coating,
            "design_life_years": 3,
            "anode_mass_kg": 7,
            "anode_count": 9,
            "current_mean_a": 2,
            "current_final_a": 4,
        }
        for coating in ("bare", "coated")
    ]


def test_existing_galvanic_projection_preserves_ordered_pairs():
    """Hand unpivot: two metals squared * one environment = four ordered pairs.

    Off-diagonal fixture values differ to expose accidental transposition.
    """
    doc = {
        "metals": ["a", "b"],
        "labels": ["A", "B"],
        "anodic_index_v": [2, 3],
        "environments": {
            "test": {
                "label": "Test",
                "allowable_v": 7,
                "verdict": [["aa", "ab"], ["ba", "bb"]],
                "delta_v": [[0, 4], [5, 0]],
                "rank": [[0, 1], [2, 0]],
                "anodic": [[None, "a"], ["b", None]],
            }
        },
    }
    rows = builder.galvanic_rows(doc)
    assert [(r["metal_a"], r["metal_b"], r["verdict"], r["delta_v"]) for r in rows] == [
        ("a", "a", "aa", 0),
        ("a", "b", "ab", 4),
        ("b", "a", "ba", 5),
        ("b", "b", "bb", 0),
    ]
    assert rows[1]["anodic_member"] == "a"
    assert rows[2]["rank"] == 2
    assert rows[2]["metal_b_anodic_index_v"] == 2
    assert rows[2]["allowable_delta_v"] == 7


def test_existing_large_sweep_is_a_lossless_pass_through():
    """The fixture is already tidy; no recalculation or flattening is required."""
    doc = {"rows": [{"t": 3, "T": 7, "M": 9}]}
    assert builder.wall_thickness_3d_rows(doc) == [{"t": 3, "T": 7, "M": 9}]


@pytest.mark.parametrize(
    "dataset,config,expected",
    [
        ("digitalmodel-integrity", "wall_thickness", 9 * 41),
        ("digitalmodel-corrosion", "cathodic_protection", 2 * 2 * 4 * 7),
        ("digitalmodel-corrosion", "galvanic_compatibility", 3 * 16 * 16),
        ("digitalmodel-artificial-lift", "ipr", 3 * 10 * 11),
        ("digitalmodel-mooring", "anchor_holding", 3 * 2 * 19),
        ("digitalmodel-hydro", "ship_resistance", 3 * 25),
        ("digitalmodel-hydro", "viv", 3 * 25),
        ("digitalmodel-well", "pore_pressure", 19),
        ("digitalmodel-field-development", "field_economics", 3 * 15),
    ],
)
def test_committed_sidecar_cardinalities(dataset, config, expected):
    """Independent Cartesian counts from each source generator's declared grids.

    wall: build_wall_thickness_explorer.py, nine codes and 41 wall points.
    CP: build_cathodic_protection_explorer.py, structures/climates/coatings/lives.
    galvanic: build_galvanic_explorer.py, environments and ordered metal pairs.
    IPR: build_ipr_explorer.py, reservoir pressure/productivity/flowing pressure.
    anchor: build_anchor_holding_explorer.py, soils/anchors/weight points.
    ship and VIV: their build_*_explorer.py, three cases and 25 speeds each.
    pore: build_pore_pressure_explorer.py, 5000 through 14000 ft, step 500 ft:
          (14000 - 5000) / 500 + 1 = 19 points.
    economics: build_field_economics_explorer.py, three capex cases, 15 prices.
    These are export coverage checks, not independently qualified model answers.
    """
    assert len(builder.DATASETS[dataset][config]()) == expected


def test_original_json_export_command_includes_new_tables(tmp_path, monkeypatch):
    """The legacy command stays usable after the generated large input is supplied.

    Only the missing large sweep is substituted. Its arbitrary one-row fixture
    isolates the CLI contract without regenerating an engineering computation.
    """
    load = builder._load
    monkeypatch.setattr(
        builder,
        "_load",
        lambda source: (
            {"meta": {"grid": {"row_count": 1}}, "rows": [{"t": 3}]}
            if source.endswith("3d.json") else load(source)
        ),
    )
    monkeypatch.setattr(
        "sys.argv", ["build_capability_datasets", "--out", str(tmp_path)]
    )
    builder.main()
    import json

    assert (
        len(
            json.loads((tmp_path / "digitalmodel-artificial-lift/ipr.json").read_text())
        )
        == 330
    )
    # Ten original configs plus seven explicit projections; no table is dropped.
    assert len(list(tmp_path.rglob("*.json"))) == 17
