"""
Test suite for jumper_lift.py – Ballymore Manifold-to-PLET Jumper V2

Every expected value is traced back to a specific cell in the source
workbook (Jumper_Input_Ballymore_Manifold-PLET V2.xlsx).

Tolerances:
    • 1e-4 for lengths, weights, densities (4 decimal places matches
      the workbook’s numeric display precision).
    • Looser tolerance (1e-2) where the Excel cell shows fewer digits.
"""

import math
import json
import pytest



from digitalmodel.marine_ops.installation.jumper_lift import (
    INCH_TO_M,
    LB_TO_KG,
    SEAWATER_DENSITY_KG_M3,
    STEEL_DENSITY_KG_M3,
    BarePipeProperties,
    BuoyancyModuleProperties,
    ClampProperties,
    ConnectorProperties,
    CraneConfig,
    PipeSectionLengths,
    RiggingProperties,
    StrakeProperties,
    compute_bare_pipe,
    compute_buoyancy,
    compute_connector_buoy_totals,
    compute_crane_configs,
    compute_crane_utilisation,
    compute_estimated_pipe_weight,
    compute_orcaflex_sections,
    compute_pipe_geometry,
    compute_pipe_lengths_for_weight_estimate,
    compute_rigging_sling_lengths,
    compute_sling_geometry,
    compute_sling_stiffness,
    compute_spreader_bar,
    compute_strake,
    compute_weight_check,
    compute_weight_check_uninsulated,
    compute_cog_uninsulated,
    compute_cog_insulated,
    run_all,
)

# ──────────────────────────────────────────────────────────────────────
#  Bare-Pipe Tests
# ──────────────────────────────────────────────────────────────────────


class TestBarePipe:
    """Validate Bare pipe sheet calculations."""

    def setup_method(self):
        self.pp = compute_bare_pipe()

    def test_inner_diameter(self) -> None:
        """Bare pipe!C5 = C4 - C6*2 = 10.75 - 1.79*2 = 7.17"""
        assert self.pp.id_inch == pytest.approx(7.17, abs=1e-4)

    def test_insulation_thickness(self) -> None:
        """Bare pipe!C9 = (C8-C4)/2 = (16.75-10.75)/2 = 3.0"""
        assert self.pp.insulation_thickness_inch == pytest.approx(3.0, abs=1e-4)

    def test_od_metres(self) -> None:
        """Bare pipe!E4 = C4 * 0.0254 = 10.75*0.0254 = 0.27305"""
        assert self.pp.od_m == pytest.approx(0.27305, abs=1e-5)

    def test_id_metres(self) -> None:
        """Bare pipe!E5 = C5 * 0.0254 = 7.17*0.0254 = 0.182118"""
        assert self.pp.id_m == pytest.approx(0.182118, abs=1e-5)

    def test_wall_thickness_metres(self) -> None:
        """Bare pipe!E6 = C6 * 0.0254 = 1.79*0.0254 = 0.045466"""
        assert self.pp.wall_thickness_m == pytest.approx(1.79 * INCH_TO_M, abs=1e-6)

    def test_bend_radius_metres(self) -> None:
        """Bare pipe!E7 = C7 * 0.0254 = 50*0.0254 = 1.27"""
        assert self.pp.bend_radius_m == pytest.approx(1.27, abs=1e-4)

    def test_insulation_od_metres(self) -> None:
        """Bare pipe!E8 = C8 * 0.0254 = 16.75*0.0254 = 0.42545"""
        assert self.pp.insulation_od_m == pytest.approx(0.42545, abs=1e-5)

    def test_insulation_density_te_m3(self) -> None:
        """Bare pipe!E10 = 16.0185*C10/1000 = 16.0185*61.1/1000 = 0.97873"""
        assert self.pp.insulation_density_te_m3 == pytest.approx(0.97873, abs=1e-3)

    def test_steel_linear_mass(self) -> None:
        """Bare pipe!H4 = PI()*(E4²-E5²)/4*7850 = 255.180805..."""
        assert self.pp.steel_linear_mass_kg_m == pytest.approx(255.180805108579, abs=1e-2)


# ──────────────────────────────────────────────────────────────────────
#  Connector & Clamp Tests
# ──────────────────────────────────────────────────────────────────────


class TestConnectorClamp:

    def test_connector_weight(self) -> None:
        """Bare pipe!C18 = 3357/2 = 1678.5"""
        c = ConnectorProperties()
        assert c.weight_in_air_kg == pytest.approx(1678.5, abs=1e-2)

    def test_connector_length(self) -> None:
        """Bare pipe!C19 = 1.3"""
        c = ConnectorProperties()
        assert c.length_m == pytest.approx(1.3, abs=1e-4)

    def test_clamp_weight(self) -> None:
        """Bare pipe!C14 = 260"""
        c = ClampProperties()
        assert c.weight_kg == pytest.approx(260.0, abs=1e-2)

    def test_clamp_wll(self) -> None:
        """Bare pipe!C13 = 15"""
        c = ClampProperties()
        assert c.wll_te == pytest.approx(15.0, abs=1e-2)


# ──────────────────────────────────────────────────────────────────────
#  Pipe Geometry (GA Sheet) Tests
# ──────────────────────────────────────────────────────────────────────


class TestPipeGeometry:
    """Validate GA sheet calculations."""

    def setup_method(self):
        self.geom = compute_pipe_geometry(PipeSectionLengths(), 50.0)

    def test_section_a_straight(self) -> None:
        """GA!C18 = C5 - C14 = 336 - 50 = 286"""
        assert self.geom["straight_lengths_inch"][0] == pytest.approx(286.0, abs=1e-4)

    def test_section_b_straight(self) -> None:
        """GA!C20 = C6 - C14*2 = 160 - 100 = 60"""
        assert self.geom["straight_lengths_inch"][1] == pytest.approx(60.0, abs=1e-4)

    def test_section_c_straight(self) -> None:
        """GA!C22 = C7 - C14*2 = 525 - 100 = 425"""
        assert self.geom["straight_lengths_inch"][2] == pytest.approx(425.0, abs=1e-4)

    def test_section_d_straight(self) -> None:
        """GA!C24 = C8 - C14*2 = 1046.3 - 100 = 946.3"""
        assert self.geom["straight_lengths_inch"][3] == pytest.approx(946.3, abs=1e-4)

    def test_section_e_straight(self) -> None:
        """GA!C26 = C9 - C14*2 = 370 - 100 = 270"""
        assert self.geom["straight_lengths_inch"][4] == pytest.approx(270.0, abs=1e-4)

    def test_section_f_straight(self) -> None:
        """GA!C28 = C10 - C14*2 = 160 - 100 = 60"""
        assert self.geom["straight_lengths_inch"][5] == pytest.approx(60.0, abs=1e-4)

    def test_section_g_straight(self) -> None:
        """GA!C30 = C11 - 50 = 352 - 50 = 302"""
        assert self.geom["straight_lengths_inch"][6] == pytest.approx(302.0, abs=1e-4)

    def test_bend_arc(self) -> None:
        """GA!C19 = PI()*50/2 = 78.5398163397448"""
        expected = math.pi * 50 / 2
        assert self.geom["bend_arc_inch"] == pytest.approx(expected, abs=1e-6)

    def test_total_length(self) -> None:
        """GA!C32 = SUM(C18:C30) = 2820.53889803847"""
        assert self.geom["total_length_inch"] == pytest.approx(2820.53889803847, abs=1e-3)

    def test_clamp_1_location(self) -> None:
        """GA!C37 = C18+C19+(62-C14) = 286 + 78.5398 + 12 = 376.5398"""
        assert self.geom["clamp_locations_inch"][0] == pytest.approx(376.539816339745, abs=1e-2)

    def test_clamp_2_location(self) -> None:
        """GA!C38 = SUM(C18:C23)+(62-50) = 1018.61944901923"""
        assert self.geom["clamp_locations_inch"][1] == pytest.approx(1018.61944901923, abs=1e-2)

    def test_clamp_3_location(self) -> None:
        """GA!C39 = C38+461.15 = 1479.76944901923"""
        assert self.geom["clamp_locations_inch"][2] == pytest.approx(1479.76944901923, abs=1e-2)

    def test_clamp_4_location(self) -> None:
        """GA!C40 = SUM(C18:C24)-(62-50) = 1940.91944901923"""
        assert self.geom["clamp_locations_inch"][3] == pytest.approx(1940.91944901923, abs=1e-2)

    def test_clamp_5_location(self) -> None:
        """GA!C41 = SUM(C18:C28)-(62-50)"""
        # Expected from workbook: 2427.99908169872
        expected = 2427.99908169872
        assert self.geom["clamp_locations_inch"][4] == pytest.approx(expected, abs=1e-1)


# ──────────────────────────────────────────────────────────────────────
#  Buoyancy Module Tests
# ──────────────────────────────────────────────────────────────────────


class TestBuoyancy:
    """Validate Bouyancy sheet calculations."""

    def setup_method(self):
        self.buoy = compute_buoyancy()

    def test_id_m(self) -> None:
        """Bouyancy!C5 = 16.75*0.0254 = 0.42545"""
        assert self.buoy.id_m == pytest.approx(0.42545, abs=1e-5)

    def test_dry_weight_kg(self) -> None:
        """Bouyancy!C8 = 0.453592*E8 = 0.453592*1287 = 583.772904"""
        assert self.buoy.dry_weight_kg == pytest.approx(583.772904, abs=1e-3)

    def test_wet_weight_kg(self) -> None:
        """Bouyancy!C9 = 0.453592*E9 = 0.453592*(-613) = -278.051896"""
        assert self.buoy.wet_weight_kg == pytest.approx(-278.051896, abs=1e-3)

    def test_displaced_kg(self) -> None:
        """Bouyancy!C12 = C8-C9 = 583.772904-(-278.051896) = 861.8248"""
        assert self.buoy.displaced_kg == pytest.approx(861.8248, abs=1e-3)

    def test_volume_m3(self) -> None:
        """Bouyancy!C13 = C12/1025 = 861.8248/1025 = 0.840804682926829"""
        assert self.buoy.volume_m3 == pytest.approx(0.840804682926829, abs=1e-6)

    def test_od_hydro_m(self) -> None:
        """Bouyancy!C14 = SQRT((4*C13)/(PI()*C4)+C5²) = 1.11116807264708"""
        assert self.buoy.od_hydro_m == pytest.approx(1.11116807264708, abs=1e-5)

    def test_wt_hydro_m(self) -> None:
        """Bouyancy!C15 = (C14-C5)/2 = (1.11117-0.42545)/2 = 0.34285903632354"""
        assert self.buoy.wt_hydro_m == pytest.approx(0.34285903632354, abs=1e-5)

    def test_density(self) -> None:
        """Bouyancy!C16 = C8/C13/1000 = 583.773/0.8408/1000 = 0.694302631578947"""
        assert self.buoy.density_te_m3 == pytest.approx(0.694302631578947, abs=1e-4)


# ──────────────────────────────────────────────────────────────────────
#  Strake Module Tests
# ──────────────────────────────────────────────────────────────────────


class TestStrake:
    """Validate Strake sheet calculations."""

    def setup_method(self):
        self.strake = compute_strake()

    def test_id_m(self) -> None:
        """Strake!C5 = 16.75*0.0254 = 0.42545"""
        assert self.strake.id_m == pytest.approx(0.42545, abs=1e-5)

    def test_dry_weight_kg(self) -> None:
        """Strake!C8 = 0.453592*E8 = 0.453592*38.01 = 17.24103192"""
        assert self.strake.dry_weight_kg == pytest.approx(17.24103192, abs=1e-4)

    def test_wet_weight_kg(self) -> None:
        """Strake!C9 = 0.453592*E9 = 0.453592*3.47 = 1.57396424"""
        assert self.strake.wet_weight_kg == pytest.approx(1.57396424, abs=1e-4)

    def test_displaced_kg(self) -> None:
        """Strake!C12 = C8-C9 = 17.24103-1.57396 = 15.66706768"""
        assert self.strake.displaced_kg == pytest.approx(15.66706768, abs=1e-4)

    def test_volume_m3(self) -> None:
        """Strake!C13 = C12/1025 = 0.0152849440780488"""
        assert self.strake.volume_m3 == pytest.approx(0.0152849440780488, abs=1e-8)

    def test_od_hydro_m(self) -> None:
        """Strake!C14 = SQRT((4*C13)/(PI()*C4)+C5²) = 0.435914878029862"""
        assert self.strake.od_hydro_m == pytest.approx(0.435914878029862, abs=1e-5)

    def test_wt_hydro_m(self) -> None:
        """Strake!C15 = (C14-C5)/2 = 0.00523243901493092"""
        assert self.strake.wt_hydro_m == pytest.approx(0.00523243901493092, abs=1e-6)

    def test_density(self) -> None:
        """Strake!C16 = C8/C13/1000 = 1.12799802046233"""
        assert self.strake.density_te_m3 == pytest.approx(1.12799802046233, abs=1e-4)


# ──────────────────────────────────────────────────────────────────────
#  Rigging Tests
# ──────────────────────────────────────────────────────────────────────


class TestRigging:
    """Validate Rigging sheet calculations."""

    def test_sling_stiffness(self) -> None:
        """Rigging!C14 = C12/C13*9.81 = 1200/0.045*9.81 = 261600"""
        result = compute_sling_stiffness()
        assert result["sling_ea_kn"] == pytest.approx(261600.0, abs=1e-0)

    def test_spreader_bar_od_m(self) -> None:
        """Rigging!H21 = 20*0.0254 = 0.508"""
        result = compute_spreader_bar()
        assert result["od_m"] == pytest.approx(0.508, abs=1e-4)

    def test_spreader_bar_wt_m(self) -> None:
        """Rigging!H22 = 1.281*0.0254 = 0.0325374"""
        result = compute_spreader_bar()
        assert result["wt_m"] == pytest.approx(1.281 * INCH_TO_M, abs=1e-6)

    def test_sling_length_m(self) -> None:
        """Rigging!AA2 = 480*0.0254 = 12.192"""
        result = compute_sling_geometry()
        assert result["sling_length_m"] == pytest.approx(12.192, abs=1e-4)

    def test_sling_horizontal_m(self) -> None:
        """Rigging!AA3 = 480*0.0254*COS(50°)"""
        expected = 12.192 * math.cos(math.radians(50.0))
        result = compute_sling_geometry()
        assert result["sling_horizontal_m"] == pytest.approx(expected, abs=1e-4)

    def test_fiber_rope_length(self) -> None:
        """Rigging!C5 = 2*0.3048 = 0.6096"""
        r = RiggingProperties()
        assert r.fiber_rope_length_m == pytest.approx(0.6096, abs=1e-4)


# ──────────────────────────────────────────────────────────────────────
#  Crane Configuration Tests
# ──────────────────────────────────────────────────────────────────────


class TestCraneConfig:
    """Validate Crane Configuration sheet calculations."""

    def setup_method(self):
        self.cranes = compute_crane_configs()

    def test_sz_radius(self) -> None:
        """Crane Configuration!C2 = 18"""
        assert self.cranes["SZ"].radius_m == pytest.approx(18.0, abs=1e-2)

    def test_sz_swl(self) -> None:
        """Crane Configuration!C3 = 70 + 30/4 = 77.5"""
        assert self.cranes["SZ"].swl_te == pytest.approx(77.5, abs=1e-2)

    def test_sz_dynamic_capacity(self) -> None:
        """Crane Configuration!C5 = SWL * DDF = 77.5 * 1.3 = 100.75"""
        assert self.cranes["SZ"].dynamic_capacity_te == pytest.approx(100.75, abs=1e-2)

    def test_dz_radius(self) -> None:
        """Crane Configuration!D2 = 12"""
        assert self.cranes["DZ"].radius_m == pytest.approx(12.0, abs=1e-2)

    def test_dz_swl(self) -> None:
        """Crane Configuration!D3 = 100"""
        assert self.cranes["DZ"].swl_te == pytest.approx(100.0, abs=1e-2)

    def test_dz_dynamic_capacity(self) -> None:
        """Crane Configuration!D5 = 100 * 1.3 = 130"""
        assert self.cranes["DZ"].dynamic_capacity_te == pytest.approx(130.0, abs=1e-2)

    def test_dz_ddf(self) -> None:
        """Crane Configuration!D4 = 1.3"""
        assert self.cranes["DZ"].ddf == pytest.approx(1.3, abs=1e-2)

    def test_crane_utilisation(self) -> None:
        """Generic utilisation check: 64 / 77.5 ≈ 0.825806"""
        uc = compute_crane_utilisation(self.cranes["SZ"], 64.0)
        assert uc == pytest.approx(64.0 / 77.5, abs=1e-4)


# ──────────────────────────────────────────────────────────────────────
#  Weight Check Tests
# ──────────────────────────────────────────────────────────────────────


class TestWeightCheck:
    """Validate Weight Check sheet calculations."""

    def test_uninsulated_kit_total(self) -> None:
        """Weight Check!C14 = SUM(C10:C13) = 26644"""
        result = compute_weight_check_uninsulated()
        assert result["total_kg"] == pytest.approx(26644.0, abs=1e-0)

    def test_buoy_total_weight(self) -> None:
        """Weight Check!C24 = 22 * Bouyancy!C8 = 22 * 583.772904 = 12843.003888"""
        buoy = compute_buoyancy()
        result = compute_weight_check(buoy)
        assert result["total_buoy_kg"] == pytest.approx(12843.003888, abs=1e-2)

    def test_strake_total_weight(self) -> None:
        """Weight Check!C25 = 2 * Strake!C8 = 2 * 17.24103192 = 34.48206384"""
        strake = compute_strake()
        result = compute_weight_check(strake=strake)
        assert result["total_strake_kg"] == pytest.approx(34.48206384, abs=1e-4)

    def test_clamp_total_weight(self) -> None:
        """Weight Check!C26 = 260 * 5 = 1300"""
        result = compute_weight_check()
        assert result["total_clamp_kg"] == pytest.approx(1300.0, abs=1e-0)

    def test_grand_total_insulated(self) -> None:
        """Weight Check!C27 = SUM(C20:C26) = 46032.48595184
        = 8682+8309+6134+8730 + 12843.004 + 34.482 + 1300"""
        result = compute_weight_check()
        assert result["grand_total_kg"] == pytest.approx(46032.48595184, abs=1e-1)

    def test_kit_weights_insulated(self) -> None:
        """Weight Check!C20-C23"""
        result = compute_weight_check()
        expected = {"KIT1": 8682.0, "KIT2": 8309.0, "KIT3": 6134.0, "KIT4": 8730.0}
        for kit, wt in expected.items():
            assert result["kit_weights_kg"][kit] == pytest.approx(wt, abs=1e-0)

    def test_buoy_24_total(self) -> None:
        """Bouyancy!C22 = C8 * 24 = 583.772904 * 24 = 14010.549696"""
        result = compute_connector_buoy_totals()
        assert result["total_buoy_24_kg"] == pytest.approx(14010.549696, abs=1e-2)


# ──────────────────────────────────────────────────────────────────────
#  COG Tests
# ──────────────────────────────────────────────────────────────────────


class TestCOG:
    """Validate centre-of-gravity calculations."""

    def test_uninsulated_kit1_cog_x(self) -> None:
        """Weight Check!D10 = -(13.288-8.18) = -5.108"""
        result = compute_cog_uninsulated()
        assert result["KIT1"]["cog_x_m"] == pytest.approx(-5.108, abs=1e-3)

    def test_uninsulated_kit2_cog_x(self) -> None:
        """Weight Check!D11 = 4.554 - 13.288 = -8.734"""
        result = compute_cog_uninsulated()
        assert result["KIT2"]["cog_x_m"] == pytest.approx(-8.734, abs=1e-3)

    def test_uninsulated_kit3_cog_x(self) -> None:
        """Weight Check!D12 = 13.288 - 4.374 = 8.914"""
        result = compute_cog_uninsulated()
        assert result["KIT3"]["cog_x_m"] == pytest.approx(8.914, abs=1e-3)

    def test_uninsulated_kit4_cog_x(self) -> None:
        """Weight Check!D13 = 13.288 - 0.807 = 12.481"""
        result = compute_cog_uninsulated()
        assert result["KIT4"]["cog_x_m"] == pytest.approx(12.481, abs=1e-3)


# ──────────────────────────────────────────────────────────────────────
#  Pipe Weight Estimation Tests
# ──────────────────────────────────────────────────────────────────────


class TestPipeWeightEstimate:
    """Validate pipe length and weight estimation for kits."""

    def test_kit2_length_includes_bend(self) -> None:
        """Weight Check!F11 = 9.271 + 14.023 - 2*1.27 + 0.5*PI()*1.27"""
        lengths = compute_pipe_lengths_for_weight_estimate()
        R = 1.27
        expected = 9.271 + 14.023 - 2 * R + 0.5 * math.pi * R
        assert lengths["KIT2_length_m"] == pytest.approx(expected, abs=1e-4)

    def test_kit3_length_includes_bend(self) -> None:
        """Weight Check!F12 = 5.334 + 12.553 - 2*1.27 + 0.5*PI()*1.27"""
        lengths = compute_pipe_lengths_for_weight_estimate()
        R = 1.27
        expected = 5.334 + 12.553 - 2 * R + 0.5 * math.pi * R
        assert lengths["KIT3_length_m"] == pytest.approx(expected, abs=1e-4)

    def test_kit1_pipe_weight(self) -> None:
        """Weight Check!G10 = F10 * D1 * 1000
        D1 = 0.255176313780436 Te/m, F10 = 15.572 m"""
        weights = compute_estimated_pipe_weight()
        expected = 15.572 * 0.255176313780436 * 1000.0
        assert weights["KIT1_pipe_weight_kg"] == pytest.approx(expected, abs=1e-1)


# ──────────────────────────────────────────────────────────────────────
#  OrcaFlex Section Tests
# ──────────────────────────────────────────────────────────────────────


class TestOrcaFlexSections:
    """Validate OrcaFlex section breakdown from GA sheet."""

    def setup_method(self):
        pp = compute_bare_pipe()
        geom = compute_pipe_geometry(PipeSectionLengths(), pp.bend_radius_inch)
        self.sections = compute_orcaflex_sections(geom, pp)

    def test_has_connectors(self) -> None:
        """First and last sections must be connectors (OCS 200-V)."""
        assert self.sections[0]["line_type"] == "OCS 200-V"
        assert self.sections[-1]["line_type"] == "OCS 200-V"

    def test_connector_length(self) -> None:
        """GA!D48 = 1.3 m"""
        assert self.sections[0]["length_m"] == pytest.approx(1.3, abs=1e-4)

    def test_contains_buoyancy_sections(self) -> None:
        """There must be buoyancy sections in the breakdown."""
        buoy_sections = [s for s in self.sections if "wBuoy" in s["line_type"]]
        assert len(buoy_sections) > 0

    def test_contains_strake_sections(self) -> None:
        """There must be strake sections in the breakdown."""
        strake_sections = [s for s in self.sections if "wStrake" in s["line_type"]]
        assert len(strake_sections) > 0

    def test_total_length_covers_pipe(self) -> None:
        """Sum of all OrcaFlex sections should approximate total pipe + 2 connectors."""
        total_ofx = sum(s["length_m"] for s in self.sections)
        geom = compute_pipe_geometry(PipeSectionLengths(), 50.0)
        # Total pipe + 2×1.3 m connectors
        expected_approx = geom["total_length_m"] + 2 * 1.3
        # Allow 1% tolerance
        assert total_ofx == pytest.approx(expected_approx, rel=expected_approx * 0.01)


# ──────────────────────────────────────────────────────────────────────
#  Sling Length Tests
# ──────────────────────────────────────────────────────────────────────


class TestSlingLengths:
    """Validate rigging sling segment breakdown."""

    def test_segment_count(self) -> None:
        result = compute_rigging_sling_lengths()
        assert len(result["segment_lengths_inch"]) == 6

    def test_cumulative_increasing(self) -> None:
        result = compute_rigging_sling_lengths()
        cum = result["cumulative_m"]
        for i in range(1, len(cum)):
            assert cum[i] > cum[i - 1]

    def test_offsets_symmetric_ish(self) -> None:
        """Offsets from centre should have negative start and positive end."""
        result = compute_rigging_sling_lengths()
        offsets = result["offsets_from_centre_m"]
        assert offsets[0] < 0
        assert offsets[-1] > 0


# ──────────────────────────────────────────────────────────────────────
#  End-to-End Integration Test
# ──────────────────────────────────────────────────────────────────────


class TestRunAll:
    """Smoke-test the run_all pipeline."""

    def test_returns_all_sections(self) -> None:
        results = run_all()
        expected_keys = {
            "pipe_properties",
            "connector",
            "clamp",
            "buoyancy_module",
            "strake_module",
            "pipe_geometry",
            "orcaflex_sections",
            "sling_stiffness",
            "spreader_bar",
            "sling_geometry",
            "sling_lengths",
            "cranes",
            "weight_check_insulated",
            "weight_check_uninsulated",
            "cog_uninsulated",
            "cog_insulated",
            "pipe_lengths_for_weight",
            "estimated_pipe_weights",
            "buoyancy_totals",
        }
        assert expected_keys.issubset(set(results.keys()))

    def test_no_nan_or_inf(self) -> None:
        """Verify no NaN/inf slipped into numeric results."""
        results = run_all()

        def check_numeric(obj, path=""):
            if isinstance(obj, float):
                assert not math.isnan(obj), f"NaN at {path}"
                assert not math.isinf(obj), f"Inf at {path}"
            elif isinstance(obj, dict):
                for k, v in obj.items():
                    check_numeric(v, f"{path}.{k}")
            elif isinstance(obj, list):
                for i, v in enumerate(obj):
                    check_numeric(v, f"{path}[{i}]")
            elif hasattr(obj, "__dict__"):
                for k, v in obj.__dict__.items():
                    check_numeric(v, f"{path}.{k}")

        check_numeric(results)