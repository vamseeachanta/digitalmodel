"""Tests for the ``foam_system_sizing`` screening module and routed basename.

Synthetic MODU-like deck fixture (round numbers, no client data) with the
demand, concentrate and hydraulics hand calculation reproduced below:

Foam demand (criteria table cited per-entry, demand_policy = max):

    helideck:    300 m2 x 6.5 L/min/m2 = 1950 L/min, 10 min -> 19500 L solution
    drill floor: 400 m2 x 6.5 L/min/m2 = 2600 L/min, 15 min -> 39000 L solution
    governing area (max policy)        = drill floor, 2600 L/min
    hose streams: 2 x 400              =  800 L/min (20 min)
    design solution flow               = 2600 + 800 = 3400 L/min

Concentrate at 3%, 100% reserve:

    area concentrate     = 39000 L x 0.03           = 1170 L
    hose concentrate     = 800 L/min x 20 min x 0.03 =  480 L
    base                 = 1650 L; reserve 100%      = 1650 L
    total concentrate    = 3300 L
    injection rate       = 3400 x 0.03               = 102 L/min

Proportioner rated 500-4000 L/min solution, 150 L/min concentrate:

    3400 in [500, 4000]  -> ok, utilization = 3400/4000 = 85%
    102 <= 150           -> ok

Hydraulic screening (Hazen-Williams, C = 120, density 1000 kg/m3;
h_f = 10.67 L Q^1.852 / (C^1.852 d^4.87), Q [m3/s], d [m];
1 bar = 1e5/(1000 x 9.80665) = 10.1972 m):

    PUMP -> J1        100 m, 200 mm, Q = 3400 L/min -> h_f = 1.8736 m
    J1 -> HELIDECK     50 m, 150 mm, Q = 1950 L/min -> h_f = 1.3581 m
    J1 -> HOSE         60 m, 100 mm, Q = 1450 L/min -> h_f = 6.7825 m

    HELIDECK terminal: 1.8736 + 1.3581 + 25.0 m static + 7.0 bar (71.380 m)
                     = 99.612 m  (governs)
    HOSE terminal:     1.8736 + 6.7825 + 10.0 m static + 5.0 bar (50.986 m)
                     = 69.642 m

    pump rated 120 m @ 3600 L/min -> head margin = 20.388 m (16.99%),
    flow margin = 3600 - 3400 = 200 L/min
"""

import csv
import math
from pathlib import Path

import pytest

from digitalmodel.foam_system.sizing import (
    Citation,
    FoamCriterion,
    HoseStream,
    PipeRun,
    Proportioner,
    ProtectedArea,
    Pump,
    Terminal,
    concentrate_quantity,
    darcy_weisbach_headloss_m,
    foam_demand,
    hazen_williams_headloss_m,
    pipe_velocity_m_s,
    proportioner_check,
    solve_tree_network,
)
from digitalmodel.foam_system.workflow import router


def _citation(clause: str) -> dict:
    return {
        "standard": "NFPA 11",
        "edition": "2021",
        "clause": clause,
        "note": "illustrative example value — verify against the purchased edition",
    }


def _base_config(tmp_path: Path) -> dict:
    return {
        "basename": "foam_system_sizing",
        "_config_dir_path": str(tmp_path),
        "foam_system_sizing": {
            "criteria": {
                "helideck_foam": {
                    "application_rate_lpm_per_m2": 6.5,
                    "discharge_time_min": 10.0,
                    "citation": _citation("example helideck monitor rate"),
                },
                "modu_deck_foam": {
                    "application_rate_lpm_per_m2": 6.5,
                    "discharge_time_min": 15.0,
                    "citation": _citation("example hydrocarbon spill-fire rate"),
                },
            },
            "protected_areas": [
                {"name": "helideck", "area_m2": 300.0, "criterion": "helideck_foam"},
                {"name": "drill floor", "area_m2": 400.0, "criterion": "modu_deck_foam"},
            ],
            "demand_policy": "max",
            "hose_streams": [
                {
                    "name": "foam hose stations",
                    "flow_lpm": 400.0,
                    "count": 2,
                    "duration_min": 20.0,
                    "citation": _citation("example supplementary hose allowance"),
                }
            ],
            "concentrate": {
                "concentration_percent": 3.0,
                "reserve_percent": 100.0,
            },
            "proportioner": {
                "min_solution_flow_lpm": 500.0,
                "max_solution_flow_lpm": 4000.0,
                "max_concentrate_flow_lpm": 150.0,
                "rated_concentration_percent": 3.0,
            },
            "hydraulics": {
                "method": "hazen_williams",
                "hazen_williams_c": 120.0,
                "density_kg_m3": 1000.0,
                "pump": {
                    "node": "PUMP",
                    "rated_flow_lpm": 3600.0,
                    "rated_head_m": 120.0,
                    "elevation_m": 0.0,
                },
                "runs": [
                    {"from": "PUMP", "to": "J1", "length_m": 100.0, "diameter_mm": 200.0},
                    {"from": "J1", "to": "HELIDECK", "length_m": 50.0, "diameter_mm": 150.0},
                    {"from": "J1", "to": "HOSE", "length_m": 60.0, "diameter_mm": 100.0},
                ],
                "terminals": [
                    {"node": "HELIDECK", "flow_lpm": 1950.0,
                     "required_pressure_bar": 7.0, "elevation_m": 25.0},
                    {"node": "HOSE", "flow_lpm": 1450.0,
                     "required_pressure_bar": 5.0, "elevation_m": 10.0},
                ],
            },
            "output_dir": "results",
        },
    }


def _read_csv(path: Path) -> list[dict]:
    with path.open() as stream:
        return list(csv.DictReader(stream))


# -- physics unit tests ------------------------------------------------------


def test_hazen_williams_hand_calc():
    # main header of the fixture: 3400 L/min in 200 mm over 100 m, C = 120
    assert hazen_williams_headloss_m(3400.0, 100.0, 200.0, 120.0) == pytest.approx(
        1.8736, abs=2e-4
    )
    assert hazen_williams_headloss_m(0.0, 100.0, 200.0) == 0.0


def test_pipe_velocity():
    # 3400 L/min in 200 mm: Q/A = 0.056667 / (pi 0.2^2 / 4) = 1.8038 m/s
    assert pipe_velocity_m_s(3400.0, 200.0) == pytest.approx(1.8038, abs=1e-3)


def test_darcy_weisbach_laminar_matches_hagen_poiseuille():
    # 9 L/min in 100 mm: v = 0.0190986 m/s, Re = 1909.9 < 2300 (laminar)
    # h_f = 32 nu L v / (g D^2) analytically
    v = pipe_velocity_m_s(9.0, 100.0)
    expected = 32.0 * 1.0e-6 * 10.0 * v / (9.80665 * 0.1**2)
    assert darcy_weisbach_headloss_m(9.0, 10.0, 100.0) == pytest.approx(
        expected, rel=1e-9
    )


def test_darcy_weisbach_turbulent_near_hazen_williams():
    # for new-ish steel and water the two methods should agree within ~30%
    hw = hazen_williams_headloss_m(3400.0, 100.0, 200.0, 120.0)
    dw = darcy_weisbach_headloss_m(3400.0, 100.0, 200.0, 0.045, 1.0e-6)
    assert dw == pytest.approx(hw, rel=0.30)


def test_citation_is_mandatory():
    with pytest.raises(ValueError, match="citation.clause"):
        Citation(standard="NFPA 11", edition="2021", clause=" ")
    with pytest.raises(ValueError, match="citation.edition"):
        Citation(standard="NFPA 11", edition="", clause="x")


def test_demand_and_concentrate_hand_calc():
    cite = Citation("NFPA 11", "2021", "example", "illustrative")
    heli = ProtectedArea(
        "helideck", 300.0, FoamCriterion("helideck_foam", 6.5, 10.0, cite)
    )
    floor = ProtectedArea(
        "drill floor", 400.0, FoamCriterion("modu_deck_foam", 6.5, 15.0, cite)
    )
    hoses = [HoseStream("hose", 400.0, count=2, duration_min=20.0)]
    demand = foam_demand([heli, floor], hoses, "max")
    assert demand.governing_area == "drill floor"
    assert demand.governing_area_flow_lpm == pytest.approx(2600.0)
    assert demand.hose_flow_lpm == pytest.approx(800.0)
    assert demand.design_solution_flow_lpm == pytest.approx(3400.0)

    conc = concentrate_quantity(demand, 3.0, 100.0, hoses)
    assert conc.area_concentrate_l == pytest.approx(1170.0)
    assert conc.hose_concentrate_l == pytest.approx(480.0)
    assert conc.total_concentrate_l == pytest.approx(3300.0)
    assert conc.injection_rate_lpm == pytest.approx(102.0)

    # sum policy: both areas simultaneously
    demand_sum = foam_demand([heli, floor], hoses, "sum")
    assert demand_sum.design_solution_flow_lpm == pytest.approx(1950.0 + 2600.0 + 800.0)
    conc_sum = concentrate_quantity(demand_sum, 3.0, 0.0, hoses)
    assert conc_sum.area_concentrate_l == pytest.approx((19500.0 + 39000.0) * 0.03)


def test_proportioner_check_flags():
    prop = Proportioner(500.0, 4000.0, max_concentrate_flow_lpm=150.0,
                        rated_concentration_percent=3.0)
    ok = proportioner_check(prop, 3400.0, 102.0, 3.0)
    assert ok.ok and ok.utilization_percent == pytest.approx(85.0)

    over = proportioner_check(prop, 4500.0, 135.0, 3.0)
    assert not over.solution_flow_ok and not over.ok

    wrong_conc = proportioner_check(prop, 3400.0, 102.0, 1.0)
    assert not wrong_conc.concentration_match_ok and not wrong_conc.ok

    too_much = proportioner_check(prop, 3400.0, 200.0, 3.0)
    assert not too_much.concentrate_flow_ok and not too_much.ok


def test_tree_network_hand_calc():
    pump = Pump("PUMP", rated_flow_lpm=3600.0, rated_head_m=120.0)
    runs = [
        PipeRun("PUMP", "J1", 100.0, 200.0),
        PipeRun("J1", "HELIDECK", 50.0, 150.0),
        PipeRun("J1", "HOSE", 60.0, 100.0),
    ]
    terminals = [
        Terminal("HELIDECK", 1950.0, 7.0, elevation_m=25.0),
        Terminal("HOSE", 1450.0, 5.0, elevation_m=10.0),
    ]
    result = solve_tree_network(pump, runs, terminals)

    by_node = {r.to_node: r for r in result.runs}
    assert by_node["J1"].flow_lpm == pytest.approx(3400.0)   # sum of terminals
    assert by_node["J1"].headloss_m == pytest.approx(1.8736, abs=2e-4)
    assert by_node["HELIDECK"].headloss_m == pytest.approx(1.3581, abs=2e-4)
    assert by_node["HOSE"].headloss_m == pytest.approx(6.7825, abs=2e-4)

    by_terminal = {t.node: t for t in result.terminals}
    assert by_terminal["HELIDECK"].path == ("PUMP", "J1", "HELIDECK")
    assert by_terminal["HELIDECK"].pressure_head_m == pytest.approx(71.380, abs=2e-3)
    assert by_terminal["HELIDECK"].required_pump_head_m == pytest.approx(
        99.612, abs=2e-3
    )
    assert by_terminal["HOSE"].required_pump_head_m == pytest.approx(69.642, abs=2e-3)

    assert result.governing_terminal == "HELIDECK"
    assert result.pump_head_margin_m == pytest.approx(20.388, abs=2e-3)
    assert result.pump_head_margin_percent == pytest.approx(16.99, abs=0.01)
    assert result.pump_flow_margin_lpm == pytest.approx(200.0)
    assert result.pump_head_ok and result.pump_flow_ok
    assert result.max_velocity_m_s == pytest.approx(3.077, abs=1e-3)


def test_tree_network_validation():
    pump = Pump("PUMP", 3600.0, 120.0)
    terminal = [Terminal("A", 1000.0, 5.0)]
    # two feeds into one node
    with pytest.raises(ValueError, match="two feeds"):
        solve_tree_network(
            pump,
            [PipeRun("PUMP", "A", 10.0, 100.0), PipeRun("PUMP2", "A", 10.0, 100.0)],
            terminal,
        )
    # disconnected terminal
    with pytest.raises(ValueError, match="not connected"):
        solve_tree_network(pump, [PipeRun("PUMP", "B", 10.0, 100.0)], terminal)
    # run feeding the pump node
    with pytest.raises(ValueError, match="feeds the pump"):
        solve_tree_network(pump, [PipeRun("A", "PUMP", 10.0, 100.0)], terminal)


# -- workflow tests ----------------------------------------------------------


def test_router_full_fixture(tmp_path):
    cfg = router(_base_config(tmp_path))
    out = cfg["foam_system_sizing"]
    assert out["method"] == "foam_system_sizing_screening_v1"
    assert "authority having jurisdiction" in out["screening_notice"]

    demand = out["demand"]
    assert demand["governing_area"] == "drill floor"
    assert demand["design_solution_flow_lpm"] == pytest.approx(3400.0)

    conc = out["concentrate_result"]
    assert conc["total_concentrate_l"] == pytest.approx(3300.0)
    assert conc["injection_rate_lpm"] == pytest.approx(102.0)

    prop = out["proportioner_result"]
    assert prop["ok"] and prop["utilization_percent"] == pytest.approx(85.0)

    hyd = out["hydraulics_result"]
    assert hyd["governing_terminal"] == "HELIDECK"
    assert hyd["required_pump_head_m"] == pytest.approx(99.612, abs=2e-3)
    assert hyd["pump_head_margin_m"] == pytest.approx(20.388, abs=2e-3)
    assert hyd["pump_head_ok"] and hyd["pump_flow_ok"]

    rows = _read_csv(tmp_path / "results" / "foam_system_sizing_foam_demand.csv")
    assert len(rows) == 3  # 2 areas + 1 hose allowance
    assert float(rows[1]["solution_flow_lpm"]) == pytest.approx(2600.0)
    assert "NFPA 11 (2021)" in rows[0]["citation"]

    run_rows = _read_csv(
        tmp_path / "results" / "foam_system_sizing_foam_hydraulic_runs.csv"
    )
    assert len(run_rows) == 3
    terminal_rows = _read_csv(
        tmp_path / "results" / "foam_system_sizing_foam_hydraulic_terminals.csv"
    )
    assert terminal_rows[0]["path"] == "PUMP -> J1 -> HELIDECK"


def test_router_darcy_weisbach_option(tmp_path):
    cfg = _base_config(tmp_path)
    cfg["foam_system_sizing"]["hydraulics"]["method"] = "darcy_weisbach"
    cfg["foam_system_sizing"]["hydraulics"]["roughness_mm"] = 0.045
    out = router(cfg)["foam_system_sizing"]
    hyd = out["hydraulics_result"]
    assert hyd["method"] == "darcy_weisbach"
    # same order of magnitude as the Hazen-Williams run
    assert hyd["required_pump_head_m"] == pytest.approx(99.6, rel=0.05)


def test_router_missing_citation_rejected(tmp_path):
    cfg = _base_config(tmp_path)
    del cfg["foam_system_sizing"]["criteria"]["helideck_foam"]["citation"]
    with pytest.raises(ValueError, match="citation is required"):
        router(cfg)

    cfg = _base_config(tmp_path)
    cfg["foam_system_sizing"]["criteria"]["helideck_foam"]["citation"]["clause"] = ""
    with pytest.raises(ValueError, match="citation.clause"):
        router(cfg)


def test_router_unknown_criterion(tmp_path):
    cfg = _base_config(tmp_path)
    cfg["foam_system_sizing"]["protected_areas"][0]["criterion"] = "nope"
    with pytest.raises(ValueError, match="'nope' not found"):
        router(cfg)


def test_router_optional_sections(tmp_path):
    cfg = _base_config(tmp_path)
    del cfg["foam_system_sizing"]["proportioner"]
    del cfg["foam_system_sizing"]["hydraulics"]
    out = router(cfg)["foam_system_sizing"]
    assert out["proportioner_result"] is None
    assert out["hydraulics_result"] is None
    assert out["concentrate_result"]["total_concentrate_l"] == pytest.approx(3300.0)


def test_engine_routes_basename():
    from digitalmodel import engine as engine_module

    source = Path(engine_module.__file__).read_text()
    assert 'basename == "foam_system_sizing"' in source
