# ABOUTME: Tests for field_development.screening (#1511, epic #1507): cable sizing,
# ABOUTME: flowline diameter sweep, report. Every correlation vs a HAND-COMPUTED anchor.
"""Tests for digitalmodel.field_development.screening.

Every correlation is asserted against a value computed BY HAND in the test
comments (calculator arithmetic from the cited formula), never against a
snapshot of the code's own output.
"""

from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.field_development.onshore_layout import (
    build_layout,
    load_field_config,
)
from digitalmodel.field_development.screening import (
    cable_ampacity_screening,
    conductor_resistance_ohm_per_m,
    erosional_velocity_api_rp_14e,
    flowline_diameter_sweep,
    load_screening_defaults,
    screening_report,
    size_cable,
    sweep_layout_flowlines,
    voltage_drop_three_phase_v,
)

DEMO_CONFIG = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
    / "onshore_demo_field.yml"
)


# --- defaults loader ---------------------------------------------------------
class TestLoadScreeningDefaults:
    def test_bundled_defaults_load_and_have_sections(self):
        cfg = load_screening_defaults()
        for section in ("erosional", "conductors", "cable_screening", "flowline_sweep"):
            assert section in cfg
        assert cfg["erosional"]["c_factor"] == 100.0

    def test_non_mapping_defaults_rejected(self, tmp_path):
        bad = tmp_path / "bad.yml"
        bad.write_text("- just\n- a\n- list\n", encoding="utf-8")
        with pytest.raises(ValueError, match="mapping"):
            load_screening_defaults(bad)


# --- erosional velocity (API RP 14E) -----------------------------------------
class TestErosionalVelocity:
    def test_hand_computed_oil_density(self):
        # HAND CALC (API RP 14E, Ve = C/sqrt(rho lb/ft3), C = 100):
        #   rho = 850 kg/m3 = 850 * 0.3048^3/0.45359237 = 53.06377 lb/ft3
        #   Ve  = 100/sqrt(53.06377) = 13.72777 ft/s = 13.72777*0.3048
        #       = 4.18422 m/s
        assert erosional_velocity_api_rp_14e(850.0, 100.0) == pytest.approx(
            4.18422, rel=1e-5
        )

    def test_hand_computed_water_matches_textbook(self):
        # HAND CALC: rho = 1000 kg/m3 = 62.42796 lb/ft3;
        #   Ve = 100/sqrt(62.42796) = 12.65640 ft/s (the well-known ~12.7 ft/s
        #   RP 14E water figure) = 12.65640*0.3048 = 3.85767 m/s.
        assert erosional_velocity_api_rp_14e(1000.0, 100.0) == pytest.approx(
            3.85767, rel=1e-5
        )

    def test_scales_inversely_with_sqrt_density(self):
        v1 = erosional_velocity_api_rp_14e(400.0, 100.0)
        v2 = erosional_velocity_api_rp_14e(1600.0, 100.0)
        assert v1 / v2 == pytest.approx(2.0, rel=1e-12)

    @pytest.mark.parametrize("rho,c", [(0.0, 100.0), (850.0, 0.0), (-1.0, 100.0)])
    def test_invalid_inputs_rejected(self, rho, c):
        with pytest.raises(ValueError):
            erosional_velocity_api_rp_14e(rho, c)


# --- conductor resistance -----------------------------------------------------
class TestConductorResistance:
    def test_hand_computed_copper_95mm2_at_90c(self):
        # HAND CALC (R = rho20*(1 + alpha*(T-20))/A, IEC 60228 copper
        # rho20 = 1.7241e-8 ohm-m, alpha = 3.93e-3 /K):
        #   R20 = 1.7241e-8/95e-6 = 1.814842e-4 ohm/m
        #   R90 = 1.814842e-4 * (1 + 0.00393*70) = 1.814842e-4 * 1.2751
        #       = 2.314105e-4 ohm/m
        r = conductor_resistance_ohm_per_m(95.0, 90.0, material="copper")
        assert r == pytest.approx(2.314105e-4, rel=1e-6)

    def test_hand_computed_copper_1mm2_at_20c(self):
        # HAND CALC: R20 = 1.7241e-8/1e-6 = 1.7241e-2 ohm/m (pure-resistivity
        # value; IEC 60228 tabulated 1 mm2 class-1 is slightly higher because
        # of stranding allowances — screening tier uses resistivity only).
        r = conductor_resistance_ohm_per_m(1.0, 20.0, material="copper")
        assert r == pytest.approx(1.7241e-2, rel=1e-9)

    def test_aluminium_heavier_than_copper(self):
        r_cu = conductor_resistance_ohm_per_m(50.0, 90.0, material="copper")
        r_al = conductor_resistance_ohm_per_m(50.0, 90.0, material="aluminium")
        assert r_al > r_cu

    def test_unknown_material_and_bad_area_rejected(self):
        with pytest.raises(KeyError, match="unobtainium"):
            conductor_resistance_ohm_per_m(50.0, 90.0, material="unobtainium")
        with pytest.raises(ValueError):
            conductor_resistance_ohm_per_m(0.0, 90.0)


# --- ampacity (IEC 60287-1-1, conductor-loss-only form) ------------------------
class TestCableAmpacity:
    def test_hand_computed_95mm2_copper(self):
        # HAND CALC (I = sqrt(dTheta/(R*T)), R90 = 2.314105e-4 ohm/m from
        # the resistance hand calc, dTheta = 90-30 = 60 K, T = 3.5 K.m/W):
        #   R*T = 2.314105e-4*3.5 = 8.099368e-4
        #   I   = sqrt(60/8.099368e-4) = sqrt(74079.85) = 272.176 A
        r90 = 1.7241e-8 / 95e-6 * (1.0 + 3.93e-3 * 70.0)
        i = cable_ampacity_screening(r90, 90.0, 30.0, 3.5)
        assert i == pytest.approx(272.176, rel=1e-4)

    def test_ambient_above_conductor_limit_rejected(self):
        with pytest.raises(ValueError, match="exceed"):
            cable_ampacity_screening(2.0e-4, 40.0, 40.0, 3.5)

    def test_nonpositive_resistances_rejected(self):
        with pytest.raises(ValueError):
            cable_ampacity_screening(0.0, 90.0, 30.0, 3.5)
        with pytest.raises(ValueError):
            cable_ampacity_screening(2.0e-4, 90.0, 30.0, 0.0)


# --- three-phase voltage drop --------------------------------------------------
class TestVoltageDrop:
    def test_hand_computed_round_numbers(self):
        # HAND CALC (dU = sqrt(3)*I*L*(R'cos(phi) + X'sin(phi))):
        #   I = 200 A, L = 1000 m, R' = 2.0e-4, X' = 1.0e-4, pf = 0.8
        #   (sin(phi) = 0.6):
        #   dU = 1.7320508*200*1000*(1.6e-4 + 0.6e-4)
        #      = 1.7320508*200*1000*2.2e-4 = 76.21024 V
        du = voltage_drop_three_phase_v(200.0, 1000.0, 2.0e-4, 1.0e-4, 0.8)
        assert du == pytest.approx(76.21024, rel=1e-6)

    def test_unity_power_factor_drops_reactive_term(self):
        du = voltage_drop_three_phase_v(100.0, 500.0, 3.0e-4, 5.0e-4, 1.0)
        # HAND CALC: sin(phi) = 0 so dU = sqrt(3)*100*500*3.0e-4 = 25.98076 V.
        assert du == pytest.approx(25.98076, rel=1e-6)

    def test_invalid_power_factor_rejected(self):
        for pf in (0.0, -0.5, 1.1):
            with pytest.raises(ValueError, match="power_factor"):
                voltage_drop_three_phase_v(100.0, 500.0, 2.0e-4, 1.0e-4, pf)


# --- cable sizing sweep ---------------------------------------------------------
class TestSizeCable:
    def test_hand_computed_selection_500kw_6600v(self):
        # HAND CALC with the bundled defaults (Cu 90C, ambient 30C,
        # T = 3.5 K.m/W, X' = 1.0e-4 ohm/m, limit 5 %):
        #   I = 500e3/(sqrt(3)*6600*0.9) = 500000/10288.38 = 48.598 A
        #   4 mm2 : R90 = 1.7241e-8/4e-6*1.2751 = 5.49600e-3 ohm/m
        #           ampacity = sqrt(60/(5.496e-3*3.5)) = 55.85 A  (>= I, ok)
        #           dU = sqrt(3)*48.598*800*(5.496e-3*0.9 + 1e-4*0.43589)
        #              = 67339.9*4.98999e-3 = 336.03 V = 5.091 % > 5 %  FAIL
        #   6 mm2 : R90 = 3.66400e-3; ampacity = sqrt(60/0.0128240) = 68.40 A
        #           dU = 67339.9*(3.2976e-3 + 4.3589e-5) = 225.00 V
        #              = 3.409 %  PASS -> smallest passing size = 6 mm2
        result = size_cable(
            load_kw=500.0, line_voltage_v=6600.0, length_m=800.0, power_factor=0.9
        )
        assert result["current_a"] == pytest.approx(48.598, rel=1e-4)
        assert result["passes"] is True
        assert result["selected"]["size_mm2"] == 6.0
        assert result["selected"]["ampacity_a"] == pytest.approx(68.40, rel=1e-3)
        assert result["selected"]["voltage_drop_pct"] == pytest.approx(3.409, rel=1e-3)
        by_size = {c["size_mm2"]: c for c in result["candidates"]}
        assert by_size[4.0]["ampacity_ok"] is True
        assert by_size[4.0]["voltage_drop_ok"] is False
        assert by_size[4.0]["voltage_drop_pct"] == pytest.approx(5.091, rel=1e-3)

    def test_candidates_cover_all_standard_sizes_ascending(self):
        result = size_cable(50.0, 400.0, 100.0, 0.9)
        sizes = [c["size_mm2"] for c in result["candidates"]]
        assert sizes == sorted(sizes)
        assert sizes == load_screening_defaults()["conductors"]["standard_sizes_mm2"]

    def test_impossible_run_returns_no_selection(self):
        # 10 km at 400 V: no standard size meets a 5 % drop -> selected None.
        result = size_cable(200.0, 400.0, 10_000.0, 0.9)
        assert result["selected"] is None
        assert result["passes"] is False

    def test_invalid_inputs_rejected(self):
        with pytest.raises(ValueError):
            size_cable(0.0, 6600.0, 800.0, 0.9)
        with pytest.raises(ValueError):
            size_cable(500.0, 0.0, 800.0, 0.9)


# --- flowline diameter sweep -----------------------------------------------------
SWEEP_KWARGS = dict(
    rate_m3_per_day=300.0,
    length_m=2000.0,
    elevation_change_m=15.0,
    roughness_m=4.5e-5,
    density_kg_per_m3=850.0,
    viscosity_pa_s=2.0e-3,
    inlet_pressure_kpa=2000.0,
    target_arrival_pressure_kpa=1500.0,
)


class TestFlowlineDiameterSweep:
    def test_hand_computed_selection_nps3(self):
        # HAND CALC (defaults: NPS candidates 2..12 STD, ASME B36.10M IDs;
        # Darcy-Weisbach + Swamee-Jain; API RP 14E Ve = 4.18422 m/s for
        # rho = 850 from the erosional hand calc):
        #   NPS 2 STD: ID = 2.067 in = 0.0525018 m, v = Q/A = 1.6039 m/s,
        #     Re = 35789, eps/D = 8.5711e-4, f = 0.02501,
        #     dp_f = f*(L/D)*rho*v^2/2 = 0.02501*38094.9*1093.3 = 1041.6 kPa
        #     -> arrival = 2000 - (1041.6+125.0) = 833 kPa < 1500  FAIL
        #   NPS 3 STD: ID = 3.068 in = 0.0779272 m,
        #     A = pi/4*0.0779272^2 = 4.76944e-3 m2,
        #     v = (300/86400)/4.76944e-3 = 0.72801 m/s  (< 4.184, ok)
        #     Re = 850*0.72801*0.0779272/2e-3 = 24111, eps/D = 5.7746e-4,
        #     f = 0.25/log10(5.7746e-4/3.7 + 5.74/24111^0.9)^2 = 0.026149
        #     dp_f = 0.026149*(2000/0.0779272)*850*0.72801^2/2 = 151.2 kPa
        #     dp_el = 850*9.80665*15 = 125.03 kPa; total = 276.2 kPa
        #     arrival = 2000 - 276.2 = 1723.8 kPa >= 1500  PASS
        #   -> smallest passing size = NPS 3.
        result = flowline_diameter_sweep(**SWEEP_KWARGS)
        assert result["passes"] is True
        sel = result["selected"]
        assert sel["nps"] == 3.0
        assert sel["inner_diameter_m"] == pytest.approx(0.0779272, rel=1e-6)
        assert sel["velocity_m_per_s"] == pytest.approx(0.72801, rel=1e-4)
        assert sel["dp_total_kpa"] == pytest.approx(276.2, rel=5e-3)
        assert sel["arrival_pressure_kpa"] == pytest.approx(1723.8, rel=1e-3)
        assert result["erosional_velocity_m_per_s"] == pytest.approx(4.18422, rel=1e-5)
        by_nps = {c["nps"]: c for c in result["candidates"]}
        assert by_nps[2.0]["arrival_ok"] is False
        assert by_nps[2.0]["velocity_ok"] is True

    def test_erosional_limit_can_reject_a_size(self):
        # Push the rate up so small sizes bust the RP 14E limit: at
        # 3000 m3/day in NPS 2 (A = 2.16490e-3 m2) v = 16.04 m/s >> 4.18.
        result = flowline_diameter_sweep(**{**SWEEP_KWARGS, "rate_m3_per_day": 3000.0})
        by_nps = {c["nps"]: c for c in result["candidates"]}
        assert by_nps[2.0]["velocity_ok"] is False
        if result["selected"] is not None:
            assert result["selected"]["velocity_ok"] is True

    def test_no_size_passes_when_target_unreachable(self):
        result = flowline_diameter_sweep(
            **{**SWEEP_KWARGS, "target_arrival_pressure_kpa": 1999.0}
        )
        # Elevation head alone is 125 kPa, so no diameter can arrive >= 1999.
        assert result["selected"] is None
        assert result["passes"] is False

    def test_inlet_must_exceed_target(self):
        with pytest.raises(ValueError, match="exceed"):
            flowline_diameter_sweep(**{**SWEEP_KWARGS, "inlet_pressure_kpa": 1500.0})

    def test_candidates_ascending_and_from_config(self):
        result = flowline_diameter_sweep(**SWEEP_KWARGS)
        nps = [c["nps"] for c in result["candidates"]]
        assert nps == sorted(nps)
        assert nps == load_screening_defaults()["flowline_sweep"]["candidate_nps"]


class TestSweepLayoutFlowlines:
    def test_consumes_tracer_layout_objects(self):
        config = load_field_config(DEMO_CONFIG)
        layout = build_layout(config, base_dir=DEMO_CONFIG.parent)
        results = sweep_layout_flowlines(
            layout,
            fluid=config["fluid"],
            inlet_pressure_kpa=2000.0,
            target_arrival_pressure_kpa=1500.0,
        )
        assert len(results) == len(layout.flowlines)
        ids = [r["id"] for r in results]
        assert ids == [fl.flowline_id for fl in layout.flowlines]
        for r, fl in zip(results, layout.flowlines):
            assert r["length_m"] == pytest.approx(fl.terrain_length_m)
            assert r["rate_m3_per_day"] == fl.rate_m3_per_day
            assert r["to"] == fl.to_id


# --- screening report -------------------------------------------------------------
def _report_inputs():
    flowline = flowline_diameter_sweep(**SWEEP_KWARGS)
    flowline["id"] = "FL-A"
    cable = size_cable(500.0, 6600.0, 800.0, 0.9)
    cable["id"] = "CAB-A"
    return {
        "field": "Unit Test Field",
        "assumptions": ["Single-phase gross liquid; demo fluid properties."],
        "flowlines": [flowline],
        "cables": [cable],
    }


class TestScreeningReport:
    def test_report_written_with_expected_content(self, tmp_path):
        dest = tmp_path / "report" / "screening.md"
        written = screening_report(_report_inputs(), dest)
        assert Path(written) == dest
        text = dest.read_text(encoding="utf-8")
        assert text.startswith("# Screening report — Unit Test Field")
        assert "## Assumptions" in text
        assert "API RP 14E" in text
        assert "Swamee-Jain" in text
        assert "IEC 60287" in text
        assert "| FL-A |" in text
        assert "NPS 3 STD" in text
        assert "| CAB-A |" in text
        assert "**Overall: PASS**" in text

    def test_report_is_deterministic(self, tmp_path):
        a = tmp_path / "a.md"
        b = tmp_path / "b.md"
        screening_report(_report_inputs(), a)
        screening_report(_report_inputs(), b)
        assert a.read_bytes() == b.read_bytes()

    def test_failing_line_flags_fail(self, tmp_path):
        inputs = _report_inputs()
        failing = flowline_diameter_sweep(
            **{**SWEEP_KWARGS, "target_arrival_pressure_kpa": 1999.0}
        )
        failing["id"] = "FL-BAD"
        inputs["flowlines"].append(failing)
        dest = tmp_path / "fail.md"
        screening_report(inputs, dest)
        text = dest.read_text(encoding="utf-8")
        assert "| FL-BAD |" in text
        assert "none pass" in text
        assert "**Overall: FAIL**" in text

    def test_empty_results_report_overall_fail(self, tmp_path):
        dest = tmp_path / "empty.md"
        screening_report({"field": "Empty"}, dest)
        text = dest.read_text(encoding="utf-8")
        assert "**Overall: FAIL**" in text
        assert "## Flowline" not in text
        assert "## Cable" not in text


# --- physics cross-checks ----------------------------------------------------------
class TestPhysicsSanity:
    def test_bigger_pipe_less_friction_drop(self):
        result = flowline_diameter_sweep(**SWEEP_KWARGS)
        drops = [c["dp_friction_kpa"] for c in result["candidates"]]
        assert drops == sorted(drops, reverse=True)

    def test_velocity_times_area_recovers_rate(self):
        result = flowline_diameter_sweep(**SWEEP_KWARGS)
        for c in result["candidates"]:
            area = math.pi * c["inner_diameter_m"] ** 2 / 4.0
            rate = c["velocity_m_per_s"] * area * 86_400.0
            assert rate == pytest.approx(SWEEP_KWARGS["rate_m3_per_day"], rel=1e-9)
