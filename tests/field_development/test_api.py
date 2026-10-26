# ABOUTME: Tests for field_development.api (#1512, epic #1507): the representative
# ABOUTME: layout/screen/visualize/analogs surface, incl. dict-deep config overrides.
"""Tests for digitalmodel.field_development.api.

The screen() numeric anchor reuses the HAND-COMPUTED flowline-sweep values
from ``test_screening.py`` (same rate/length/fluid, elevation term dropped on
a flat surface) — never a snapshot of the code's own output.
"""

from __future__ import annotations

import json
import math
import sys
import types
from pathlib import Path

import pytest
import yaml

from digitalmodel.field_development import api
from digitalmodel.field_development.screening import load_screening_defaults, size_cable

DATA_DIR = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
)
ONSHORE_CONFIG = DATA_DIR / "onshore_demo_field.yml"
OFFSHORE_CONFIG = DATA_DIR / "offshore_demo_field.yml"

#: Flat-surface anchor config: one 2000 m flowline at constant elevation, so
#: the sweep inputs equal test_screening's SWEEP_KWARGS with the elevation
#: term dropped (hand calc reproduced in TestScreenHandAnchored).
FLAT_ANCHOR_CONFIG = {
    "field": {"name": "Flat Anchor Field"},
    "surface": {
        "kind": "synthetic",
        "x_min_m": 0.0,
        "x_max_m": 2500.0,
        "y_min_m": 0.0,
        "y_max_m": 500.0,
        "nx": 6,
        "ny": 3,
        "base_elevation_m": 100.0,
        "amplitude_m": 0.0,  # flat: route length = plan length exactly
        "x_wavelength_m": 1000.0,
        "y_wavelength_m": 1000.0,
    },
    "assets": [
        {
            "id": "WH-1",
            "kind": "well",
            "x_m": 200.0,
            "y_m": 250.0,
            "rate_m3_per_day": 300.0,
        },
        {"id": "HOST-1", "kind": "host", "x_m": 2200.0, "y_m": 250.0},
    ],
    "connections": [
        {
            "id": "FL-1",
            "kind": "flowline",
            "from": "WH-1",
            "to": "HOST-1",
            "roughness_m": 4.5e-5,
        },
    ],
}

ANCHOR_FLUID = {"density_kg_per_m3": 850.0, "viscosity_pa_s": 2.0e-3}


def _write_anchor_config(tmp_path: Path) -> Path:
    path = tmp_path / "flat_anchor_field.yml"
    path.write_text(yaml.safe_dump(FLAT_ANCHOR_CONFIG), encoding="utf-8")
    return path


# --- deep_override -----------------------------------------------------------
class TestDeepOverride:
    def test_mappings_merge_recursively(self):
        base = {"fluid": {"density_kg_per_m3": 850.0, "viscosity_pa_s": 5e-3}}
        merged = api.deep_override(base, {"fluid": {"viscosity_pa_s": 2e-3}})
        assert merged["fluid"] == {
            "density_kg_per_m3": 850.0,
            "viscosity_pa_s": 2e-3,
        }
        # No mutation of the base mapping.
        assert base["fluid"]["viscosity_pa_s"] == 5e-3

    def test_id_keyed_list_merge(self):
        base = {"wells": [{"id": "W-1", "rate": 1.0}, {"id": "W-2", "rate": 2.0}]}
        merged = api.deep_override(base, {"wells": {"W-2": {"rate": 9.0}}})
        assert merged["wells"] == [
            {"id": "W-1", "rate": 1.0},
            {"id": "W-2", "rate": 9.0},
        ]
        assert base["wells"][1]["rate"] == 2.0

    def test_unknown_id_raises_keyerror(self):
        base = {"wells": [{"id": "W-1"}]}
        with pytest.raises(KeyError, match="W-9"):
            api.deep_override(base, {"wells": {"W-9": {"rate": 1.0}}})

    def test_mapping_onto_plain_list_rejected(self):
        with pytest.raises(ValueError, match="id"):
            api.deep_override({"xs": [1, 2, 3]}, {"xs": {"a": 1}})

    def test_scalars_and_lists_replace(self):
        merged = api.deep_override({"a": 1, "xs": [1, 2]}, {"a": 2, "xs": [3]})
        assert merged == {"a": 2, "xs": [3]}

    def test_new_top_level_section_added(self):
        merged = api.deep_override({"a": 1}, {"cables": [{"id": "C-1"}]})
        assert merged["cables"] == [{"id": "C-1"}]


# --- layout ------------------------------------------------------------------
class TestLayout:
    def test_onshore_demo_tracer_schema(self):
        summary = api.layout(ONSHORE_CONFIG)
        assert summary["field"] == "Demo Onshore Field"
        assert summary["surface"]["is_subsea"] is False
        assert summary["assets_by_kind"] == {"host": 1, "well": 3}
        assert len(summary["connections"]) == 3
        for conn in summary["connections"]:
            assert conn["route_length_m"] >= conn["plan_length_m"]
        json.dumps(summary)  # JSON-serializable contract

    def test_offshore_demo_generalized_schema(self):
        summary = api.layout(OFFSHORE_CONFIG)
        assert summary["field"] == "Demo Offshore Field"
        assert summary["surface"]["is_subsea"] is True
        assert summary["assets_by_kind"] == {
            "vessel": 1,
            "manifold": 1,
            "well": 3,
            "tree": 3,
            "host": 1,
        }
        assert {c["kind"] for c in summary["connections"]} == {
            "jumper",
            "flowline",
            "pipeline",
            "umbilical",
        }

    def test_override_changes_result_without_editing_file(self):
        base = api.layout(ONSHORE_CONFIG)
        moved = api.layout(
            ONSHORE_CONFIG,
            field={"name": "Iteration 2"},
            wells={"W-1": {"x_m": 600.0}},
        )
        assert moved["field"] == "Iteration 2"
        fl1_base = next(c for c in base["connections"] if c["id"] == "FL-1")
        fl1_moved = next(c for c in moved["connections"] if c["id"] == "FL-1")
        assert fl1_moved["plan_length_m"] < fl1_base["plan_length_m"]


# --- screen: hand-anchored numbers -------------------------------------------
class TestScreenHandAnchored:
    def test_flat_single_flowline_matches_hand_calc(self, tmp_path):
        # HAND CALC — same arithmetic as test_screening's SWEEP_KWARGS anchor
        # (300 m3/d, 2000 m, rho 850, mu 2e-3, eps 4.5e-5, inlet 2000 kPa,
        # target 1500 kPa) with the elevation term dropped (flat surface,
        # dp_el = 0):
        #   NPS 2 STD: dp_f = 1041.6 kPa -> arrival = 958.4 < 1500   FAIL
        #   NPS 3 STD: ID = 0.0779272 m, v = 0.72801 m/s,
        #     dp_f = 151.2 kPa -> arrival = 2000 - 151.2 = 1848.8    PASS
        #   -> smallest passing size = NPS 3; Ve(RP 14E) = 4.18422 m/s.
        config_path = _write_anchor_config(tmp_path)
        result = api.screen(
            config_path,
            inlet_pressure_kpa=2000.0,
            target_arrival_pressure_kpa=1500.0,
            fluid=ANCHOR_FLUID,
        )
        assert result["passes"] is True
        assert result["skipped_connections"] == []
        (sweep,) = result["flowlines"]
        assert sweep["id"] == "FL-1"
        assert sweep["length_m"] == pytest.approx(2000.0)
        assert sweep["elevation_change_m"] == pytest.approx(0.0)
        assert sweep["rate_m3_per_day"] == pytest.approx(300.0)
        sel = sweep["selected"]
        assert sel["nps"] == 3.0
        assert sel["inner_diameter_m"] == pytest.approx(0.0779272, rel=1e-6)
        assert sel["velocity_m_per_s"] == pytest.approx(0.72801, rel=1e-4)
        assert sel["dp_total_kpa"] == pytest.approx(151.2, rel=5e-3)
        assert sel["arrival_pressure_kpa"] == pytest.approx(1848.8, rel=1e-3)
        assert sweep["erosional_velocity_m_per_s"] == pytest.approx(4.18422, rel=1e-5)
        by_nps = {c["nps"]: c for c in sweep["candidates"]}
        assert by_nps[2.0]["arrival_ok"] is False


# --- screen: demo configs end-to-end ------------------------------------------
class TestScreenDemoConfigs:
    def test_onshore_demo_screens_all_flowlines(self, tmp_path):
        report = tmp_path / "onshore" / "screening.md"
        result = api.screen(
            ONSHORE_CONFIG,
            inlet_pressure_kpa=2000.0,
            target_arrival_pressure_kpa=1500.0,
            report_path=report,
        )
        assert result["field"] == "Demo Onshore Field"
        assert [fl["id"] for fl in result["flowlines"]] == ["FL-1", "FL-2", "FL-3"]
        # Fluid fell back to the config's fluid section.
        assert result["flowlines"][0]["density_kg_per_m3"] == 850.0
        assert result["flowlines"][0]["viscosity_pa_s"] == 0.005
        # Line rates come from the from-well of each tracer flowline.
        assert [fl["rate_m3_per_day"] for fl in result["flowlines"]] == [
            280.0,
            320.0,
            260.0,
        ]
        assert result["layout"]["field"] == "Demo Onshore Field"
        assert result["report_path"] == str(report)
        text = report.read_text(encoding="utf-8")
        assert text.startswith("# Screening report — Demo Onshore Field")
        assert "FL-1" in text
        json.dumps(result)

    def test_offshore_demo_rate_accumulation(self):
        # Trees carry no rate but sit ON their wells (co-located), so each
        # jumper inherits its well's rate; the manifold flowline carries the
        # summed 950 + 820 + 780 = 2550 m3/d; the export pipeline carries the
        # FPSO throughput (same 2550). The umbilical is not hydraulic.
        result = api.screen(
            OFFSHORE_CONFIG,
            inlet_pressure_kpa=25000.0,
            target_arrival_pressure_kpa=5000.0,
            fluid=ANCHOR_FLUID,
        )
        rates = {fl["id"]: fl["rate_m3_per_day"] for fl in result["flowlines"]}
        assert rates == {
            "JMP-1": 950.0,
            "JMP-2": 820.0,
            "JMP-3": 780.0,
            "FL-1": 2550.0,
            "PL-1": 2550.0,
        }
        assert result["skipped_connections"] == []
        assert "UMB-1" not in rates

    def test_explicit_connection_rate_wins(self):
        result = api.screen(
            OFFSHORE_CONFIG,
            inlet_pressure_kpa=25000.0,
            target_arrival_pressure_kpa=5000.0,
            fluid=ANCHOR_FLUID,
            connections={"FL-1": {"rate_m3_per_day": 1234.0}},
        )
        rates = {fl["id"]: fl["rate_m3_per_day"] for fl in result["flowlines"]}
        assert rates["FL-1"] == 1234.0
        # Downstream accumulation uses the explicit value too.
        assert rates["PL-1"] == 1234.0

    def test_missing_inputs_raise(self):
        with pytest.raises(ValueError, match="target_arrival_pressure_kpa"):
            api.screen(OFFSHORE_CONFIG, inlet_pressure_kpa=25000.0, fluid=ANCHOR_FLUID)
        with pytest.raises(ValueError, match="fluid"):
            api.screen(
                OFFSHORE_CONFIG,
                inlet_pressure_kpa=25000.0,
                target_arrival_pressure_kpa=5000.0,
            )


# --- screen: cables ------------------------------------------------------------
class TestScreenCables:
    CABLE_OVERRIDE = {
        "cables": [
            {
                "id": "CAB-1",
                "connection": "UMB-1",  # length = the umbilical's 3D route
                "load_kw": 500.0,
                "line_voltage_v": 6600.0,
                "power_factor": 0.9,
            },
            {
                "id": "CAB-2",
                "length_m": 800.0,
                "load_kw": 500.0,
                "line_voltage_v": 6600.0,
                "power_factor": 0.9,
            },
        ]
    }

    def test_cables_screened_when_declared(self):
        result = api.screen(
            OFFSHORE_CONFIG,
            inlet_pressure_kpa=25000.0,
            target_arrival_pressure_kpa=5000.0,
            fluid=ANCHOR_FLUID,
            **self.CABLE_OVERRIDE,
        )
        assert [cb["id"] for cb in result["cables"]] == ["CAB-1", "CAB-2"]
        umb = next(c for c in result["layout"]["connections"] if c["id"] == "UMB-1")
        cab1 = result["cables"][0]
        assert cab1["length_m"] == pytest.approx(umb["route_length_m"], abs=0.01)
        # The connection-referenced screen equals a direct size_cable call.
        direct = size_cable(500.0, 6600.0, cab1["length_m"], 0.9)
        assert cab1["selected"] == direct["selected"]
        # CAB-2 at 800 m reproduces test_screening's hand-anchored selection.
        cab2 = result["cables"][1]
        assert cab2["selected"]["size_mm2"] == 6.0
        assert cab2["selected"]["ampacity_a"] == pytest.approx(68.40, rel=1e-3)

    def test_unknown_connection_reference_rejected(self):
        with pytest.raises(ValueError, match="NOPE"):
            api.screen(
                OFFSHORE_CONFIG,
                inlet_pressure_kpa=25000.0,
                target_arrival_pressure_kpa=5000.0,
                fluid=ANCHOR_FLUID,
                cables=[
                    {
                        "id": "CAB-X",
                        "connection": "NOPE",
                        "load_kw": 1.0,
                        "line_voltage_v": 400.0,
                        "power_factor": 0.9,
                    }
                ],
            )

    def test_cable_without_length_source_rejected(self):
        with pytest.raises(ValueError, match="length_m"):
            api.screen(
                OFFSHORE_CONFIG,
                inlet_pressure_kpa=25000.0,
                target_arrival_pressure_kpa=5000.0,
                fluid=ANCHOR_FLUID,
                cables=[
                    {
                        "id": "CAB-X",
                        "load_kw": 1.0,
                        "line_voltage_v": 400.0,
                        "power_factor": 0.9,
                    }
                ],
            )


# Synthetic electrical basis, independent of any external network data:
# two sources at 0.3 and 0.6 MW, 10 kV line-to-line, pf = 0.8.
# Each plan leg is 300 m; explicit endpoint elevations differ by 400 m.
# With sample spacing > leg length the true route is sqrt(300^2+400^2)
# = 500 m. SI prefixes give MW -> kW and kV -> V by a factor of 1000.
# Balanced three-phase hand calculation: I = P/(sqrt(3)*V*pf).
# Caller resistance R' = 0.001 ohm/m and X' = 0 give
# dU = sqrt(3)*I*500*0.001*0.8: 15, 45, 45 V for the chain.
def _electrical_config():
    return {
        "field": {"name": "Synthetic radial array"},
        "surface": dict(FLAT_ANCHOR_CONFIG["surface"]),
        "routing": {"sample_spacing_m": 1000.0},
        "assets": [
            {"id": "A", "kind": "wind_turbine", "x_m": 0.0, "y_m": 0.0,
             "z_m": 0.0, "rated_power_mw": 0.3},
            {"id": "B", "kind": "wind_turbine", "x_m": 300.0, "y_m": 0.0,
             "z_m": 400.0, "rated_power_mw": 0.6},
            {"id": "S", "kind": "offshore_substation", "x_m": 600.0,
             "y_m": 0.0, "z_m": 0.0},
            {"id": "H", "kind": "host", "x_m": 900.0, "y_m": 0.0,
             "z_m": 400.0},
        ],
        "connections": [
            {"id": name, "kind": kind, "from": start, "to": end,
             "voltage_kv": 10.0, "conductor_size_mm2": 25.0,
             "power_factor": 0.8, "resistance_ohm_per_m": 0.001}
            for name, kind, start, end in (
                ("AB", "array_cable", "A", "B"),
                ("BS", "array_cable", "B", "S"),
                ("SH", "export_cable", "S", "H"),
            )
        ],
    }


def _electrical_defaults():
    defaults = load_screening_defaults()
    # Synthetic thermal basis: (90 - 30)/(0.001*3.5) gives
    # ampacity sqrt(17142.857...) = 130.9 A, above the 65.0 A chain load.
    # Reuse the bundled thermal inputs; zero reactance isolates caller R'.
    defaults["cable_screening"]["reactance_ohm_per_m"] = 0.0
    return defaults


def _screen_electrical(tmp_path, config=None, **kwargs):
    path = tmp_path / "electrical.yml"
    path.write_text(yaml.safe_dump(config or _electrical_config()), encoding="utf-8")
    return api.screen(path, screening_defaults=_electrical_defaults(), **kwargs)


def test_screen_derives_routed_electrical_cables(tmp_path):
    result = _screen_electrical(tmp_path)
    assert result["passes"] is True
    assert result["flowlines"] == []
    assert result["skipped_connections"] == []
    assert [c["connection"] for c in result["cables"]] == ["AB", "BS", "SH"]
    for cable, power, drop in zip(
        result["cables"], (300.0, 900.0, 900.0), (15.0, 45.0, 45.0)
    ):
        assert cable["load_kw"] == pytest.approx(power)
        assert cable["line_voltage_v"] == 10000.0
        assert cable["length_m"] == pytest.approx(500.0)
        assert cable["current_a"] == pytest.approx(
            power * 1000 / (math.sqrt(3) * 10000 * 0.8)
        )
        assert cable["conductor_size_mm2"] == 25.0
        assert cable["selected"]["size_mm2"] == 25.0
        assert cable["selected"]["voltage_drop_v"] == pytest.approx(drop)
        assert cable["selected"]["voltage_drop_pct"] == pytest.approx(drop / 100)
        assert "Balanced three-phase" in " ".join(cable["assumptions"])
        assert "full rated output" in " ".join(cable["assumptions"])
    json.dumps(result, allow_nan=False)


def test_upstream_power_not_double_counted(tmp_path):
    config = _electrical_config()
    # A reaches S twice, directly and via B. The export carries distinct
    # sources A+B = 0.9 MW, never A+A+B = 1.2 MW. This is a connectivity
    # screen with no parallel-branch load sharing calculation.
    config["connections"].insert(1, dict(config["connections"][0], id="AS", to="S"))
    result = _screen_electrical(tmp_path, config)
    export = next(c for c in result["cables"] if c["connection"] == "SH")
    assert export["upstream_source_ids"] == ["A", "B"]
    assert export["load_kw"] == pytest.approx(900.0)
    assert export["current_a"] == pytest.approx(900000 / (math.sqrt(3) * 10000 * 0.8))


def test_explicit_cable_override_wins(tmp_path):
    config = _electrical_config()
    # Deliberately distinct explicit basis: 120 kW, 2 kV, pf 0.5,
    # length 40 m. I = 120000/(sqrt(3)*2000*0.5) = 69.3 A.
    # Explicit connection-linked declarations replace derivation exactly once.
    config["cables"] = [{"id": "override", "connection": "BS", "load_kw": 120.0,
                         "line_voltage_v": 2000.0, "power_factor": 0.5,
                         "length_m": 40.0}]
    result = _screen_electrical(tmp_path, config)
    assert len(result["cables"]) == 3
    cable = next(c for c in result["cables"] if c["connection"] == "BS")
    assert cable["id"] == "override"
    assert cable["load_kw"] == 120.0
    assert cable["line_voltage_v"] == 2000.0
    assert cable["length_m"] == 40.0
    assert cable["current_a"] == pytest.approx(120000 / (math.sqrt(3) * 2000 * 0.5))
    assert cable["conductor_size_mm2"] == 25.0
    assert cable["selected"]["size_mm2"] == 25.0
    # Repeated evaluations retain the same explicit precedence.
    assert result == _screen_electrical(tmp_path, config)


@pytest.mark.parametrize("section,index,key,reason", [
    ("assets", 0, "rated_power_mw", "rated_power_mw"),
    ("connections", 0, "voltage_kv", "voltage_kv"),
    ("connections", 0, "conductor_size_mm2", "conductor_size_mm2"),
])
def test_missing_electrical_basis_rejected(tmp_path, section, index, key, reason):
    config = _electrical_config()
    del config[section][index][key]
    with pytest.raises(ValueError, match=reason):
        _screen_electrical(tmp_path, config)


@pytest.mark.parametrize("key,value,reason", [
    ("conductor_size_mm2", 17.0, "unsupported conductor"),
    ("power_factor", 0.0, "power_factor"),
    ("power_factor", 1.1, "power_factor"),
    ("voltage_kv", float("nan"), "voltage"),
    ("resistance_ohm_per_m", -0.001, "resistance"),
])
def test_invalid_electrical_basis_rejected(tmp_path, key, value, reason):
    config = _electrical_config()
    config["connections"][0][key] = value
    with pytest.raises(ValueError, match=reason):
        _screen_electrical(tmp_path, config)


def test_electrical_cycle_rejected_even_with_explicit_override(tmp_path):
    config = _electrical_config()
    config["connections"].append(
        {**config["connections"][0], "id": "BA", "from": "B", "to": "A"}
    )
    config["cables"] = [{"connection": "AB", "load_kw": 300.0,
                         "line_voltage_v": 10000.0, "power_factor": 0.8}]
    with pytest.raises(ValueError, match="cycle"):
        _screen_electrical(tmp_path, config)


def test_supplied_conductor_failure_cannot_select_larger_size(tmp_path):
    config = _electrical_config()
    # At R' = 0.01 ohm/m the caller's 25 mm2 conductor has only
    # sqrt(60/(0.01*3.5)) = 41.4 A ampacity, below 65.0 A for BS/SH.
    config["connections"][1]["resistance_ohm_per_m"] = 0.01
    result = _screen_electrical(tmp_path, config)
    assert result["passes"] is False
    cable = next(c for c in result["cables"] if c["connection"] == "BS")
    assert cable["selected"] is None
    assert cable["candidates"][0]["size_mm2"] == 25.0
    assert cable["candidates"][0]["ampacity_ok"] is False


def test_duplicate_explicit_electrical_override_rejected(tmp_path):
    config = _electrical_config()
    cable = {"connection": "AB", "load_kw": 300.0,
             "line_voltage_v": 10000.0, "power_factor": 0.8}
    config["cables"] = [dict(cable, id="first"), dict(cable, id="second")]
    with pytest.raises(ValueError, match="duplicate"):
        _screen_electrical(tmp_path, config)


def test_unrated_upstream_turbine_cannot_hide_behind_known_source(tmp_path):
    config = _electrical_config()
    del config["assets"][1]["rated_power_mw"]
    with pytest.raises(ValueError, match="rated_power_mw for B"):
        _screen_electrical(tmp_path, config)


def test_unity_power_factor_and_material_resistance_fallback(tmp_path):
    config = _electrical_config()
    for conn in config["connections"]:
        del conn["power_factor"]
        del conn["resistance_ohm_per_m"]
    result = _screen_electrical(tmp_path, config)
    cable = result["cables"][0]
    assert cable["power_factor"] == 1.0
    # Independent Ohm-law calculation using caller-visible material constants
    # from data/screening_defaults.yml: R' = rho20*(1+alpha*(T-20))/A.
    # Square millimetres -> square metres by 1e-6; 20 C is the reference
    # temperature in the existing conductor-resistance correlation.
    defaults = _electrical_defaults()
    material = defaults["conductors"]["materials"]["copper"]
    temp = defaults["cable_screening"]["max_conductor_temp_c"]
    resistance = (
        material["resistivity_20c_ohm_m"]
        * (1 + material["temp_coefficient_per_k"] * (temp - 20))
        / (25 * 1e-6)
    )
    current = 300000 / (math.sqrt(3) * 10000)
    assert cable["candidates"][0]["resistance_ohm_per_m"] == pytest.approx(resistance)
    assert cable["current_a"] == pytest.approx(current)
    assert cable["candidates"][0]["voltage_drop_v"] == pytest.approx(
        math.sqrt(3) * current * 500 * resistance
    )


def test_electrical_report_includes_assumptions_and_failing_conductor(tmp_path):
    config = _electrical_config()
    config["connections"][1]["resistance_ohm_per_m"] = 0.01
    report_path = tmp_path / "screen.md"
    result = _screen_electrical(tmp_path, config, report_path=report_path)
    text = report_path.read_text(encoding="utf-8")
    assert result["passes"] is False
    assert "Balanced three-phase" in text
    assert "full rated output" in text
    assert "unity assumed" in text
    assert "| BS |" in text
    assert "FAIL" in text


# --- screen: iteration cheapness ----------------------------------------------
class TestScreenIteration:
    def test_three_rate_variants_give_distinct_results(self):
        # The design goal: 10-20 iterations per case, no file edits. Three
        # W-1 rate variants of the SAME demo config must yield distinct
        # screening results, monotonic in rate for a fixed candidate size.
        results = [
            api.screen(
                ONSHORE_CONFIG,
                inlet_pressure_kpa=2000.0,
                target_arrival_pressure_kpa=1500.0,
                wells={"W-1": {"rate_m3_per_day": rate}},
            )
            for rate in (280.0, 900.0, 2600.0)
        ]
        fl1 = [next(fl for fl in r["flowlines"] if fl["id"] == "FL-1") for r in results]
        assert [fl["rate_m3_per_day"] for fl in fl1] == [280.0, 900.0, 2600.0]
        # Friction dp of the FIXED NPS 4 candidate strictly increases with rate.
        dp4 = [
            {c["nps"]: c for c in fl["candidates"]}[4.0]["dp_friction_kpa"]
            for fl in fl1
        ]
        assert dp4[0] < dp4[1] < dp4[2]
        # The three iteration outcomes are pairwise distinct.
        signatures = {
            (
                None if fl["selected"] is None else fl["selected"]["nps"],
                round(dp, 6),
            )
            for fl, dp in zip(fl1, dp4)
        }
        assert len(signatures) == 3
        # Other lines are untouched by the W-1 override.
        fl2_rates = {
            next(fl for fl in r["flowlines"] if fl["id"] == "FL-2")["rate_m3_per_day"]
            for r in results
        }
        assert fl2_rates == {320.0}


# --- visualize ------------------------------------------------------------------
class TestVisualize:
    def test_offshore_demo_artifacts(self, tmp_path):
        result = api.visualize(OFFSHORE_CONFIG, tmp_path)
        plot = Path(result["plot_path"])
        scene = Path(result["scene_path"])
        assert plot.name == "offshore_demo_field_layout.png"
        assert plot.exists() and plot.stat().st_size > 0
        assert scene.exists()
        scene_data = json.loads(scene.read_text(encoding="utf-8"))
        assert scene_data["is_subsea"] is True
        assert result["surface"]["is_subsea"] is True

    def test_override_and_stem(self, tmp_path):
        result = api.visualize(
            ONSHORE_CONFIG,
            tmp_path,
            stem="iter02",
            field={"name": "Onshore Iteration 2"},
            wells={"W-1": {"x_m": 600.0}},
        )
        assert result["field"] == "Onshore Iteration 2"
        assert Path(result["plot_path"]).name == "iter02_layout.png"
        assert Path(result["scene_path"]).name == "iter02_scene.json"
        assert Path(result["plot_path"]).exists()
        scene_data = json.loads(Path(result["scene_path"]).read_text(encoding="utf-8"))
        assert scene_data["field_name"] == "Onshore Iteration 2"


# --- analogs ----------------------------------------------------------------------
class TestAnalogs:
    CRITERIA = {"water_depth_m": 1450.0, "host_kind": "fpso"}

    def test_unavailable_returns_structured_response(self):
        # worldenergydata is NOT a dependency of this repo; the adapter must
        # degrade to a structured response, never raise.
        assert "worldenergydata" not in sys.modules
        result = api.analogs(self.CRITERIA)
        assert result["available"] is False
        assert "worldenergydata" in result["reason"]
        assert result["criteria"] == self.CRITERIA
        json.dumps(result)

    def test_available_path_calls_find_analogs(self, monkeypatch):
        calls: dict[str, object] = {}
        fake = types.ModuleType("worldenergydata.field_development.analogs")

        def find_analogs(**criteria):
            calls.update(criteria)
            return [{"field": "Analog A", "score": 0.91}]

        fake.find_analogs = find_analogs
        monkeypatch.setitem(
            sys.modules, "worldenergydata.field_development.analogs", fake
        )
        result = api.analogs(self.CRITERIA)
        assert result["available"] is True
        assert result["result"] == [{"field": "Analog A", "score": 0.91}]
        assert calls == self.CRITERIA
        assert result["criteria"] == self.CRITERIA

    def test_non_mapping_criteria_rejected(self):
        with pytest.raises(TypeError, match="mapping"):
            api.analogs(["not", "a", "mapping"])  # type: ignore[arg-type]
