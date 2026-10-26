# ABOUTME: Tests for field_development.api (#1512, epic #1507): the representative
# ABOUTME: layout/screen/visualize/analogs surface, incl. dict-deep config overrides.
"""Tests for digitalmodel.field_development.api.

The screen() numeric anchor reuses the HAND-COMPUTED flowline-sweep values
from ``test_screening.py`` (same rate/length/fluid, elevation term dropped on
a flat surface) — never a snapshot of the code's own output.
"""

from __future__ import annotations

import json
import sys
import types
from pathlib import Path

import pytest
import yaml

from digitalmodel.field_development import api
from digitalmodel.field_development import screening
from digitalmodel.field_development.layout_model import build_layout_model
from digitalmodel.field_development.screening import size_cable, sweep_layout_flowlines

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
class TestScreenDelegation:
    @pytest.mark.parametrize("missing_rate", [False, True])
    def test_generalized_direct_equivalence_and_delegation(
        self, tmp_path, monkeypatch, missing_rate
    ):
        # Synthetic flat-anchor inputs above, with one straight 3D segment:
        # plan=2000 m, rise=1500 m -> sqrt(2000**2+1500**2)=2500 m.
        # Explicit 120 m3/day overrides the well's 300; both are test inputs.
        config = api.deep_override(
            FLAT_ANCHOR_CONFIG,
            {
                "assets": {"HOST-1": {"z_m": 1600.0}},
                "connections": {"FL-1": {"rate_m3_per_day": 120.0}},
                "routing": {"sample_spacing_m": 3000.0},
            },
        )
        if missing_rate:
            config["assets"][0].pop("rate_m3_per_day")
            config["connections"][0].pop("rate_m3_per_day")
        path = tmp_path / "generalized.yml"
        path.write_text(yaml.safe_dump(config), encoding="utf-8")
        model = build_layout_model(config)
        skipped = []
        defaults = screening.load_screening_defaults()
        # Restrict to the existing NPS 3 anchor to check defaults forwarding.
        defaults["flowline_sweep"]["candidate_nps"] = [3.0]
        direct = sweep_layout_flowlines(
            model, ANCHOR_FLUID, 2000.0, 1500.0,
            defaults=defaults, skipped_connections=skipped,
        )
        if missing_rate:
            assert direct == []
            assert skipped == [{
                "id": "FL-1", "kind": "flowline",
                "reason": "no resolvable rate (no explicit, asset, co-located, or upstream rate)",
            }]
        else:
            assert direct[0]["length_m"] == pytest.approx(2500.0)
            assert direct[0]["elevation_change_m"] == 1500.0
            assert direct[0]["rate_m3_per_day"] == 120.0
            assert [c["nps"] for c in direct[0]["candidates"]] == [3.0]
        calls = []
        original = screening.sweep_layout_flowlines

        def record_sweep(*args, **kwargs):
            calls.append((args, kwargs))
            return original(*args, **kwargs)

        monkeypatch.setattr(screening, "sweep_layout_flowlines", record_sweep)
        result = api.screen(
            path, fluid=ANCHOR_FLUID,
            inlet_pressure_kpa=2000.0, target_arrival_pressure_kpa=1500.0,
            screening_defaults=defaults,
        )
        assert result["flowlines"] == direct
        assert result["skipped_connections"] == skipped
        assert len(calls) == 1
        assert isinstance(calls[0][0][0], type(model))


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
