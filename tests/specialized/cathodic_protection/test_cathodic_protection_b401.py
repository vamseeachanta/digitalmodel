"""
ABOUTME: Test suite for the legacy DNV-RP-B401 offshore-platform CP solver
ABOUTME: Values re-baselined from B401 Tables 10-1/10-2/10-4/10-6/10-8 (#2207, blocker B3)
"""

import math

import pytest

from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection
from digitalmodel.infrastructure.common.cp_DNV_RP_B401_2021 import (
    _b401_anode_requirements,
    _b401_anode_resistance,
    _b401_coating_breakdown,
    _b401_current_demand,
    _b401_current_densities,
    _b401_surface_areas,
    _b401_verify_current_output,
)


@pytest.fixture
def cp_calculator():
    """Create CathodicProtection instance for testing."""
    return CathodicProtection()


@pytest.fixture
def b401_base_config():
    # 10 C -> temperate (Table 10-1 header 7-11 C / Table 10-2 7-12 C);
    # no depth_m -> 0 m -> band "0-30"; design life 25 yr.
    return {
        "inputs": {
            "calculation_type": "DNV_RP_B401_offshore",
            "design_data": {
                "design_life": 25.0,
                "structure_type": "jacket",
            },
            "structure": {
                "zones": [
                    {"zone": "submerged",   "area_m2": 5000.0, "coating_category": "I"},
                    {"zone": "splash",      "area_m2":  300.0, "coating_category": "III"},
                    {"zone": "atmospheric", "area_m2":  200.0, "coating_category": "I"},
                ]
            },
            "environment": {
                "seawater_temperature_C": 10.0,
                "seawater_resistivity_ohm_m": 0.30,
            },
            "anode": {
                "material": "aluminium",
                "type": "stand_off",
                "individual_anode_mass_kg": 200.0,
                "utilization_factor": 0.85,
                "length_m": 1.0,
                "radius_m": 0.05,
            },
        }
    }


def _pipeline(inputs, design_life=25.0):
    """Areas, densities, breakdown and demand from one inputs dict."""
    areas = _b401_surface_areas(inputs)
    densities = _b401_current_densities(inputs)
    breakdown = _b401_coating_breakdown(inputs, design_life)
    return _b401_current_demand(inputs, areas, densities, breakdown)


class TestB401CoatingBreakdown:
    """Test _b401_coating_breakdown per B401 Table 10-4 (f_c = a + b*t, clamp 1.0)."""

    def test_coating_breakdown_category_I_25yr(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        # Table 10-4 Cat I, row 0-30 m: a = 0.10, b = 0.10/yr
        # f_cf = 0.10 + 0.10 * 25 = 2.60 -> clamped to 1.0
        assert result["submerged"]["a"] == pytest.approx(0.10)
        assert result["submerged"]["b_per_yr"] == pytest.approx(0.10)
        assert result["submerged"]["f_cf"] == pytest.approx(1.0, abs=0.001)

    def test_coating_breakdown_category_III_25yr(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        # Table 10-4 Cat III, row 0-30 m: a = 0.02, b = 0.012/yr
        # f_cf = 0.02 + 0.012 * 25 = 0.32
        assert result["splash"]["f_cf"] == pytest.approx(0.32, abs=0.001)

    def test_coating_breakdown_category_II_25yr(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "II"
        result = _b401_coating_breakdown(inputs, 25.0)
        # Table 10-4 Cat II, row 0-30 m: a = 0.05, b = 0.025/yr
        # f_cm = 0.05 + 0.025 * 12.5 = 0.3625; f_cf = 0.05 + 0.025 * 25 = 0.675
        assert result["submerged"]["f_ci"] == pytest.approx(0.05, abs=0.001)
        assert result["submerged"]["f_cm"] == pytest.approx(0.3625, abs=0.001)
        assert result["submerged"]["f_cf"] == pytest.approx(0.675, abs=0.001)

    def test_coating_breakdown_bare_any_life(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "bare"
        result = _b401_coating_breakdown(inputs, 25.0)
        # bare: no Table 10-4 row, f_c = 1.0 for every phase
        assert result["submerged"]["f_cf"] == pytest.approx(1.0, abs=0.001)
        assert result["submerged"]["f_cm"] == pytest.approx(1.0, abs=0.001)
        assert result["submerged"]["f_ci"] == pytest.approx(1.0, abs=0.001)
        assert result["submerged"]["citations"] == []

    def test_coating_breakdown_mean_formula(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "III"
        result = _b401_coating_breakdown(inputs, 25.0)
        # Cat III 0-30 m: f_cm = 0.02 + 0.012 * 25/2 = 0.17
        assert result["submerged"]["f_cm"] == pytest.approx(0.17, abs=0.001)

    def test_coating_breakdown_deep_row_uses_smaller_b(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "III"
        inputs["structure"]["zones"][0]["depth_m"] = 50.0
        result = _b401_coating_breakdown(inputs, 25.0)
        # Table 10-4 Cat III, row >30 m: a = 0.02, b = 0.008/yr
        # f_cm = 0.02 + 0.008 * 12.5 = 0.12; f_cf = 0.02 + 0.008 * 25 = 0.22
        assert result["submerged"]["depth_band"] == ">30-100"
        assert result["submerged"]["b_per_yr"] == pytest.approx(0.008)
        assert result["submerged"]["f_cm"] == pytest.approx(0.12, abs=0.001)
        assert result["submerged"]["f_cf"] == pytest.approx(0.22, abs=0.001)

    def test_coating_breakdown_cites_table_10_4(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        assert result["submerged"]["citations"] == ["dnv-rp-b401 2011 Table 10-4"]

    def test_coating_breakdown_per_zone(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        assert "submerged" in result
        assert "splash" in result
        assert "atmospheric" in result

    def test_unknown_coating_category_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "INVALID"
        with pytest.raises(ValueError, match="Unknown coating category"):
            _b401_coating_breakdown(inputs, 25.0)


class TestB401CurrentDensities:
    """Test _b401_current_densities per B401 Tables 10-1 / 10-2 (bare metal)."""

    def test_current_density_submerged_10C_temperate(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_current_densities(inputs)
        # 10 C -> temperate, 0-30 m: initial 0.200 / mean 0.100 / final 0.130
        # (Table 10-1 initial and final, Table 10-2 mean); coating does not
        # change the density (it enters through f_c only)
        z = result["submerged"]
        assert z["climate"] == "temperate"
        assert z["depth_band"] == "0-30"
        assert z["i_initial_A_m2"] == pytest.approx(0.200, abs=0.001)
        assert z["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)
        assert z["i_final_A_m2"] == pytest.approx(0.130, abs=0.001)
        assert z["coated_or_bare"] == "coated"  # informational only

    def test_current_density_submerged_5C_bare(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_temperature_C"] = 5.0
        inputs["structure"]["zones"][0]["coating_category"] = "bare"
        result = _b401_current_densities(inputs)
        # 5 C -> arctic (< 7 C), 0-30 m: initial 0.250 / mean 0.120 / final 0.170
        z = result["submerged"]
        assert z["climate"] == "arctic"
        assert z["i_initial_A_m2"] == pytest.approx(0.250, abs=0.001)
        assert z["i_mean_A_m2"] == pytest.approx(0.120, abs=0.001)
        assert z["i_final_A_m2"] == pytest.approx(0.170, abs=0.001)
        assert z["coated_or_bare"] == "bare"

    def test_current_density_splash_is_zero(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_current_densities(inputs)
        # splash: B401 CP current densities apply to seawater-exposed and
        # buried surfaces only -> 0.0 for every phase, nothing to cite
        z = result["splash"]
        assert z["i_initial_A_m2"] == 0.0
        assert z["i_mean_A_m2"] == 0.0
        assert z["i_final_A_m2"] == 0.0
        assert z["citations"] == []
        assert "note" in z

    def test_current_density_atmospheric_is_zero(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][2]["coating_category"] = "bare"
        result = _b401_current_densities(inputs)
        # atmospheric: CP does not act in air -> 0.0 for every phase
        z = result["atmospheric"]
        assert z["i_initial_A_m2"] == 0.0
        assert z["i_mean_A_m2"] == 0.0
        assert z["i_final_A_m2"] == 0.0

    def test_current_density_buried_zone(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"].append(
            {"zone": "buried", "area_m2": 400.0, "coating_category": "bare"}
        )
        result = _b401_current_densities(inputs)
        # B401 Sec. 6.3: buried bare metal 0.020 A/m2, all phases
        z = result["buried"]
        assert z["i_initial_A_m2"] == pytest.approx(0.020)
        assert z["i_mean_A_m2"] == pytest.approx(0.020)
        assert z["i_final_A_m2"] == pytest.approx(0.020)
        assert z["citations"] == ["dnv-rp-b401 2011 Sec. 6.3 (buried surfaces)"]

    def test_current_density_subtropical_submerged(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_temperature_C"] = 20.0
        result = _b401_current_densities(inputs)
        # exactly 20 C -> sub-tropical (12-20 C), 0-30 m:
        # initial 0.170 / mean 0.080 / final 0.110
        z = result["submerged"]
        assert z["climate"] == "subtropical"
        assert z["i_initial_A_m2"] == pytest.approx(0.170, abs=0.001)
        assert z["i_mean_A_m2"] == pytest.approx(0.080, abs=0.001)
        assert z["i_final_A_m2"] == pytest.approx(0.110, abs=0.001)

    def test_current_density_tropical_submerged(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_temperature_C"] = 25.0
        result = _b401_current_densities(inputs)
        # 25 C -> tropical (> 20 C), 0-30 m: initial 0.150 / mean 0.070 / final 0.100
        z = result["submerged"]
        assert z["climate"] == "tropical"
        assert z["i_initial_A_m2"] == pytest.approx(0.150, abs=0.001)
        assert z["i_mean_A_m2"] == pytest.approx(0.070, abs=0.001)
        assert z["i_final_A_m2"] == pytest.approx(0.100, abs=0.001)

    def test_current_density_depth_band_from_depth_m(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["depth_m"] = 150.0
        result = _b401_current_densities(inputs)
        # temperate, 150 m -> ">100-300": initial 0.190 / mean 0.090 / final 0.140
        z = result["submerged"]
        assert z["depth_band"] == ">100-300"
        assert z["i_initial_A_m2"] == pytest.approx(0.190, abs=0.001)
        assert z["i_mean_A_m2"] == pytest.approx(0.090, abs=0.001)
        assert z["i_final_A_m2"] == pytest.approx(0.140, abs=0.001)

    def test_current_density_cites_tables_10_1_and_10_2(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_current_densities(inputs)
        assert result["submerged"]["citations"] == [
            "dnv-rp-b401 2011 Table 10-1",
            "dnv-rp-b401 2011 Table 10-2",
        ]

    def test_negative_depth_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["depth_m"] = -1.0
        with pytest.raises(ValueError, match="depth_m for zone 'submerged' must be >= 0"):
            _b401_current_densities(inputs)

    def test_unknown_zone_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["zone"] = "deep_buried"
        with pytest.raises(ValueError, match="Unknown zone"):
            _b401_current_densities(inputs)


class TestB401CurrentDemand:
    """Test _b401_current_demand: I = A * i_phase * f_c,phase per zone."""

    def test_current_demand_submerged_zone(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _pipeline(inputs)
        # submerged 5000 m2, temperate 0-30 m, Cat I 25 yr:
        # I_initial = 5000 * 0.200 * 0.10 = 100.0 A
        # I_mean    = 5000 * 0.100 * 1.0  = 500.0 A  (f_cm clamped)
        # I_final   = 5000 * 0.130 * 1.0  = 650.0 A  (f_cf clamped)
        assert result["submerged"]["I_initial_A"] == pytest.approx(100.0, abs=0.01)
        assert result["submerged"]["I_mean_A"] == pytest.approx(500.0, abs=0.01)
        assert result["submerged"]["I_final_A"] == pytest.approx(650.0, abs=0.01)

    def test_current_demand_final_uses_final_density(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "III"
        result = _pipeline(inputs)
        # Cat III: f_ci 0.02, f_cm 0.17, f_cf 0.32 with i = 0.200/0.100/0.130
        # I_initial = 5000 * 0.200 * 0.02 = 20.0; I_mean = 5000 * 0.100 * 0.17 = 85.0
        # I_final = 5000 * 0.130 * 0.32 = 208.0 (NOT 5000 * 0.100 * 0.32 = 160)
        assert result["submerged"]["I_initial_A"] == pytest.approx(20.0, abs=0.01)
        assert result["submerged"]["I_mean_A"] == pytest.approx(85.0, abs=0.01)
        assert result["submerged"]["I_final_A"] == pytest.approx(208.0, abs=0.01)

    def test_current_demand_total_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _pipeline(inputs)
        # splash and atmospheric contribute 0 A; totals are the submerged zone
        assert result["total_initial_A"] == pytest.approx(100.0)
        assert result["total_mean_A"] == pytest.approx(500.0)
        assert result["total_final_A"] == pytest.approx(650.0)
        assert result["splash"]["I_final_A"] == 0.0
        assert result["atmospheric"]["I_final_A"] == 0.0

    def test_current_demand_all_zones_present(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _pipeline(inputs)
        assert "submerged" in result
        assert "splash" in result
        assert "atmospheric" in result
        assert "total_initial_A" in result
        assert "total_mean_A" in result
        assert "total_final_A" in result

    def test_final_demand_exceeds_mean(self, b401_base_config):
        """Final density and breakdown exceed mean, so final demand must exceed mean."""
        inputs = b401_base_config["inputs"]
        result = _pipeline(inputs)
        assert result["total_final_A"] > result["total_mean_A"]


class TestB401AnodeResistance:
    """Test _b401_anode_resistance per B401 Table 10-7."""

    def test_anode_resistance_stand_off_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_anode_resistance(inputs)
        assert result > 0

    def test_anode_resistance_flush_mounted(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["type"] = "flush_mounted"
        result = _b401_anode_resistance(inputs)
        assert result > 0

    def test_anode_resistance_bracelet(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["type"] = "bracelet"
        result = _b401_anode_resistance(inputs)
        assert result > 0

    def test_anode_resistance_dwight_formula(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        # Table 10-7 long slender stand-off: R = (rho / 2piL) * (ln(4L/r) - 1)
        # rho=0.30, L=1.0, r=0.05 -> 0.047746 * (ln 80 - 1) = 0.16148 ohm
        expected = (0.30 / (2.0 * math.pi * 1.0)) * (math.log(4.0 * 1.0 / 0.05) - 1.0)
        result = _b401_anode_resistance(inputs)
        assert result == pytest.approx(expected, abs=1e-9)
        assert result == pytest.approx(0.16148, abs=1e-5)

    def test_anode_resistance_validation_zero_length(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["length_m"] = 0.0
        with pytest.raises(ValueError, match="Anode length must be > 0"):
            _b401_anode_resistance(inputs)

    def test_anode_resistance_unknown_type_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["type"] = "hanging_basket"
        with pytest.raises(ValueError, match="Unknown anode type"):
            _b401_anode_resistance(inputs)


class TestB401AnodeRequirements:
    """Test _b401_anode_requirements per B401 Sec. 7 with Tables 10-6 / 10-8."""

    def test_anode_mass_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        assert result["total_mass_kg"] > 0

    def test_anode_count_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        assert result["anode_count"] >= 1

    def test_anode_mass_formula(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        # M = (I_mean * T * 8760) / (epsilon * u); Table 10-6 Al seawater
        # epsilon = 2000 Ah/kg, u = 0.85 (input):
        # 500 * 25 * 8760 / (2000 * 0.85) = 64411.76 kg -> ceil(/200) = 323
        assert result["electrochemical_capacity_Ah_kg"] == 2000.0
        assert result["total_mass_kg"] == pytest.approx(64411.76, abs=0.01)
        assert result["anode_count"] == 323
        assert result["utilization_factor_source"] == "input"
        assert result["citations"] == ["dnv-rp-b401 2011 Table 10-6"]

    def test_zinc_capacity_from_table_10_6(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["material"] = "zinc"
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        # Table 10-6 Zn seawater: 780 Ah/kg
        assert result["electrochemical_capacity_Ah_kg"] == 780.0

    @pytest.mark.parametrize(
        ("anode_type", "expected_u"),
        [
            ("stand_off", 0.90),  # Table 10-8 long slender stand-off (L >= 4r)
            ("flush_mounted", 0.85),  # Table 10-8 long flush-mounted
            ("bracelet", 0.80),  # Table 10-8 short flush-mounted / bracelet
        ],
    )
    def test_utilisation_defaults_from_table_10_8(self, b401_base_config, anode_type, expected_u):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["type"] = anode_type
        del inputs["anode"]["utilization_factor"]
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        assert result["utilization_factor"] == pytest.approx(expected_u)
        assert result["utilization_factor_source"].startswith("Table 10-8")
        assert "dnv-rp-b401 2011 Table 10-8" in result["citations"]

    def test_explicit_utilisation_overrides_table(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["utilization_factor"] = 0.95
        demand = _pipeline(inputs)
        result = _b401_anode_requirements(inputs, demand)
        assert result["utilization_factor"] == pytest.approx(0.95)
        assert result["utilization_factor_source"] == "input"

    def test_anode_requirements_zero_life_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["design_data"]["design_life"] = 0.0
        demand = _pipeline(inputs)
        with pytest.raises(ValueError, match="Design life must be > 0"):
            _b401_anode_requirements(inputs, demand)


class TestB401VerifyCurrentOutput:
    """Test _b401_verify_current_output: initial AND final checks (B401 Sec. 7)."""

    def test_verification_returns_pass_or_fail(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        assert "adequate" in result
        assert isinstance(result["adequate"], bool)

    def test_verification_checks_initial_and_final_demand(self, b401_base_config):
        """Verification must report initial, mean and final demand."""
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        assert result["initial_current_demand_A"] == pytest.approx(100.0)
        assert result["mean_current_demand_A"] == pytest.approx(500.0)
        assert result["final_current_demand_A"] == pytest.approx(650.0)
        assert result["final_current_demand_A"] > result["mean_current_demand_A"]

    def test_verification_base_case_final_governs(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # driving voltage = -0.80 - (-1.05) = 0.25 V (Sec. 5, Table 10-6)
        # I/anode = 0.25 / 0.16148 = 1.5482 A; N (mass) = 323
        # N_initial = ceil(100 / 1.5482) = 65; N_final = ceil(650 / 1.5482) = 420
        assert result["driving_voltage_V"] == pytest.approx(0.25)
        assert result["anode_current_output_per_anode_A"] == pytest.approx(1.5482, abs=1e-4)
        assert result["count_by_initial_current"] == 65
        assert result["count_by_final_current"] == 420
        assert result["recommended_anode_count"] == 420
        assert result["governing_case"] == "final"
        assert result["initial_meets_demand"] is True
        assert result["final_meets_demand"] is False
        assert result["adequate"] is False
        assert result["citations"] == [
            "dnv-rp-b401 2011 Sec. 5 (structure-to-electrolyte potential criteria)",
            "dnv-rp-b401 2011 Table 10-6",
        ]

    def test_verification_initial_governs_for_bare_steel(self):
        """Bare steel: initial density 0.200 > final 0.130, so initial governs."""
        inputs = {
            "design_data": {"design_life": 25.0},
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": 1000.0, "coating_category": "bare"}]
            },
            "environment": {"seawater_temperature_C": 10.0, "seawater_resistivity_ohm_m": 0.30},
            "anode": {
                "material": "aluminium",
                "type": "stand_off",
                "individual_anode_mass_kg": 200.0,
                "utilization_factor": 0.85,
                "length_m": 1.0,
                "radius_m": 0.05,
            },
        }
        demand = _pipeline(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # I_initial = 1000 * 0.200 = 200 A; I_mean = 100 A; I_final = 130 A
        # mass: 100 * 25 * 8760 / (2000 * 0.85) / 200 = 64.4 -> 65 anodes
        # N_initial = ceil(200 / 1.5482) = 130; N_final = ceil(130 / 1.5482) = 84
        assert anode_req["anode_count"] == 65
        assert result["count_by_initial_current"] == 130
        assert result["count_by_final_current"] == 84
        assert result["recommended_anode_count"] == 130
        assert result["governing_case"] == "initial"
        assert result["adequate"] is False

    def test_verification_adequate_when_mass_governs(self):
        """Monopile-like case: mass count exceeds both current counts -> adequate."""
        inputs = {
            "design_data": {"design_life": 30.0},
            "structure": {
                "zones": [
                    {"zone": "submerged",   "area_m2": 1200.0, "coating_category": "I"},
                    {"zone": "splash",      "area_m2":  180.0, "coating_category": "I"},
                    {"zone": "atmospheric", "area_m2":  120.0, "coating_category": "I"},
                ]
            },
            "environment": {
                "seawater_temperature_C": 8.0,
                "seawater_resistivity_ohm_m": 0.28,
            },
            "anode": {
                "material": "aluminium",
                "type": "stand_off",
                "individual_anode_mass_kg": 150.0,
                "utilization_factor": 0.85,
                "length_m": 0.9,
                "radius_m": 0.06,
            },
        }
        demand = _pipeline(inputs, 30.0)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # 8 C temperate 0-30 m; Cat I 30 yr -> f_ci 0.10, f_cm = f_cf = 1.0
        # I_initial = 1200 * 0.200 * 0.10 = 24 A; I_mean = 120 A; I_final = 156 A
        # mass = 120 * 30 * 8760 / (2000 * 0.85) = 18550.6 kg -> 124 anodes
        # R = 0.28 / (2pi 0.9) * (ln 60 - 1) = 0.15322 ohm; I/anode = 1.6317 A
        # N_initial = 15, N_final = 96 -> mass governs, adequate
        assert anode_req["anode_count"] == 124
        assert result["count_by_final_current"] == 96
        assert result["governing_case"] == "mass"
        assert result["recommended_anode_count"] == 124
        assert result["adequate"] is True


class TestB401Router:
    """Test router dispatch for B401 route."""

    def test_router_dispatches_b401_key(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        result = cp_calculator.router(cfg)
        assert result is not None

    def test_router_writes_results(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        assert "results" in cfg
        assert "standard" in cfg["results"]

    def test_router_unknown_key_raises(self, cp_calculator):
        cfg = {"inputs": {"calculation_type": "UNKNOWN_STANDARD"}}
        with pytest.raises(ValueError, match="not IMPLEMENTED"):
            cp_calculator.router(cfg)


class TestB401Integration:
    """End-to-end integration tests."""

    def test_full_jacket_platform(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        results = cfg["results"]
        assert "surface_areas_m2" in results
        assert "coating_breakdown" in results
        assert "current_densities_A_m2" in results
        assert "current_demand_A" in results
        assert "anode_resistance_ohm" in results
        assert "anode_requirements" in results
        assert "current_output_verification" in results

    def test_results_standard_edition_and_provenance(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        # No edition key -> default 2021, whose tables are inherited from the
        # 2010/2011 print without a verified 2021 source in this checkout
        assert cfg["results"]["standard"] == "DNV-RP-B401 (2021)"
        assert cfg["results"]["edition"] == "2021"
        assert cfg["results"]["provenance"] == "inherited-2011-unverified"

    def test_results_explicit_edition_2010(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cfg["inputs"]["design_data"]["edition"] = "DNV_rp_b401_2011"
        cp_calculator.router(cfg)
        assert cfg["results"]["standard"] == "DNV-RP-B401 (October 2010)"
        assert cfg["results"]["edition"] == "2010"
        assert cfg["results"]["provenance"] == "verified-2011-tables"
        # Tables 10-1/10-2/10-4/10-6 are identical across editions
        assert cfg["results"]["current_demand_A"]["total_mean_A"] == pytest.approx(500.0)

    def test_results_no_missing_edition_warning(self, cp_calculator, b401_base_config, recwarn):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        assert not [w for w in recwarn if "No DNV-RP-B401 edition" in str(w.message)]

    def test_results_citations_are_real_b401_sections(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        assert cfg["results"]["citations"] == [
            "dnv-rp-b401 2011 Sec. 5 (structure-to-electrolyte potential criteria)",
            "dnv-rp-b401 2011 Table 10-1",
            "dnv-rp-b401 2011 Table 10-2",
            "dnv-rp-b401 2011 Table 10-4",
            "dnv-rp-b401 2011 Table 10-6",
        ]

    def test_results_design_life(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        assert cfg["results"]["design_life_years"] == 25.0

    def test_duplicate_zone_raises_in_coating_breakdown(self):
        """Duplicate zone IDs raise ValueError — zone entries must use unique IDs."""
        inputs = {
            "design_data": {"design_life": 25.0},
            "structure": {
                "zones": [
                    {"zone": "submerged", "area_m2": 2000.0, "coating_category": "I"},
                    {"zone": "submerged", "area_m2": 3000.0, "coating_category": "III"},
                ]
            },
        }
        with pytest.raises(ValueError, match="Duplicate zone ID"):
            _b401_coating_breakdown(inputs, 25.0)

    def test_duplicate_zone_id_raises_in_surface_areas(self):
        """Duplicate zone IDs in surface_areas also raise ValueError."""
        inputs = {
            "structure": {
                "zones": [
                    {"zone": "submerged", "area_m2": 2000.0, "coating_category": "I"},
                    {"zone": "submerged", "area_m2": 3000.0, "coating_category": "III"},
                ]
            },
        }
        with pytest.raises(ValueError, match="Duplicate zone ID"):
            _b401_surface_areas(inputs)

    def test_segmented_zone_with_base_zone(self):
        """base_zone field allows multiple entries for same physical zone with distinct IDs."""
        inputs = {
            "structure": {
                "zones": [
                    {"zone": "sub_cat1", "base_zone": "submerged", "area_m2": 3000.0,
                     "coating_category": "I"},
                    {"zone": "sub_cat3", "base_zone": "submerged", "area_m2": 2000.0,
                     "coating_category": "III"},
                ]
            },
            "environment": {"seawater_temperature_C": 10.0},
        }
        result = _b401_current_densities(inputs)
        # Both entries use the temperate 0-30 m bare-metal lookup (mean 0.100);
        # the coating category does not change the density
        assert result["sub_cat1"]["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)
        assert result["sub_cat3"]["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)
        assert result["sub_cat1"]["base_zone"] == "submerged"

    def test_unknown_material_raises_in_anode_requirements(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["material"] = "magnesium"
        demand = _pipeline(inputs)
        with pytest.raises(ValueError, match="Unknown anode material"):
            _b401_anode_requirements(inputs, demand)

    def test_utilization_factor_above_1_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["utilization_factor"] = 1.5
        demand = _pipeline(inputs)
        with pytest.raises(ValueError, match="Utilization factor must be in"):
            _b401_anode_requirements(inputs, demand)

    def test_zero_resistivity_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_resistivity_ohm_m"] = 0.0
        with pytest.raises(ValueError, match="Seawater resistivity must be > 0"):
            _b401_anode_resistance(inputs)

    def test_boundary_temperature_7C(self):
        """Exactly 7 C is the lower edge of the temperate band (7-11 / 7-12 C)."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 7.0},
        }
        result = _b401_current_densities(inputs)
        # temperate 0-30 m mean 0.100 (Table 10-2); 6.9 C would be arctic 0.120
        assert result["submerged"]["climate"] == "temperate"
        assert result["submerged"]["temperature_band"] == "7-12"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)

    def test_boundary_temperature_12C(self):
        """Exactly 12 C is the lower edge of the sub-tropical band (12-20 C)."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 12.0},
        }
        result = _b401_current_densities(inputs)
        # sub-tropical 0-30 m mean 0.080 (Table 10-2)
        assert result["submerged"]["climate"] == "subtropical"
        assert result["submerged"]["temperature_band"] == "12-20"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.080, abs=0.001)

    def test_boundary_temperature_20C(self):
        """Exactly 20 C stays sub-tropical; tropical is > 20 C."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 20.0},
        }
        result = _b401_current_densities(inputs)
        assert result["submerged"]["climate"] == "subtropical"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.080, abs=0.001)
        inputs["environment"]["seawater_temperature_C"] = 20.5
        result = _b401_current_densities(inputs)
        # tropical 0-30 m mean 0.070 (Table 10-2)
        assert result["submerged"]["climate"] == "tropical"
        assert result["submerged"]["temperature_band"] == ">20"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.070, abs=0.001)

    def test_boundary_depth_30m(self):
        """Exactly 30 m stays in the 0-30 row; 31 m is the >30-100 row."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0,
                                     "coating_category": "I", "depth_m": 30.0}]},
            "environment": {"seawater_temperature_C": 10.0},
        }
        result = _b401_current_densities(inputs)
        assert result["submerged"]["depth_band"] == "0-30"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)
        inputs["structure"]["zones"][0]["depth_m"] = 31.0
        result = _b401_current_densities(inputs)
        # temperate >30-100 m: initial 0.170 / mean 0.080 / final 0.110
        assert result["submerged"]["depth_band"] == ">30-100"
        assert result["submerged"]["i_initial_A_m2"] == pytest.approx(0.170, abs=0.001)
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.080, abs=0.001)
        assert result["submerged"]["i_final_A_m2"] == pytest.approx(0.110, abs=0.001)

    def test_negative_area_raises_in_surface_areas(self):
        """Negative area_m2 raises ValueError (non-physical)."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": -100.0, "coating_category": "I"}]
            }
        }
        with pytest.raises(ValueError, match="area_m2 for zone"):
            _b401_surface_areas(inputs)

    def test_negative_design_life_raises_in_coating_breakdown(self):
        """Negative design_life raises ValueError in coating breakdown."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]
            }
        }
        with pytest.raises(ValueError, match="Design life must be > 0"):
            _b401_coating_breakdown(inputs, -5.0)

    def test_recommended_count_satisfies_all_criteria(self, b401_base_config):
        """recommended_anode_count satisfies mass, initial and final criteria."""
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        resistance = _b401_anode_resistance(inputs)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # recommended_count >= mass count (always)
        assert result["recommended_anode_count"] >= result["anode_count"]
        # recommended_count satisfies both current output criteria
        I_per_anode = result["anode_current_output_per_anode_A"]
        assert result["recommended_anode_count"] * I_per_anode >= result["initial_current_demand_A"]
        assert result["recommended_anode_count"] * I_per_anode >= result["final_current_demand_A"]
        assert result["governing_case"] in ("initial", "final", "mass")

    def test_verification_result_has_recommended_count(self, b401_base_config):
        """Verification result includes an integer recommended_anode_count field."""
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        resistance = _b401_anode_resistance(inputs)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        assert "recommended_anode_count" in result
        assert isinstance(result["recommended_anode_count"], int)
        assert isinstance(result["anode_count"], int)

    def test_zero_resistance_raises_in_verify(self, b401_base_config):
        """Zero resistance raises ValueError in current output verification."""
        inputs = b401_base_config["inputs"]
        demand = _pipeline(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        with pytest.raises(ValueError, match="Anode resistance must be > 0"):
            _b401_verify_current_output(inputs, anode_req, 0.0, demand)

    def test_empty_zones_raises_in_surface_areas(self):
        """Empty zones list raises ValueError — must have at least one zone."""
        inputs = {"structure": {"zones": []}}
        with pytest.raises(ValueError, match="zones list is empty"):
            _b401_surface_areas(inputs)

    def test_nan_design_life_raises_in_coating_breakdown(self):
        """NaN design_life raises ValueError."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]
            }
        }
        with pytest.raises(ValueError, match="finite"):
            _b401_coating_breakdown(inputs, float("nan"))

    def test_inf_resistivity_raises_in_anode_resistance(self):
        """Infinite resistivity raises ValueError."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0}]},
            "environment": {"seawater_resistivity_ohm_m": float("inf")},
            "anode": {"type": "stand_off", "length_m": 1.0, "radius_m": 0.05},
        }
        with pytest.raises(ValueError, match="finite"):
            _b401_anode_resistance(inputs)

    def test_zero_final_demand_raises_in_verify(self, b401_base_config):
        """Zero final current demand raises ValueError in adequacy check."""
        inputs = b401_base_config["inputs"]
        fake_demand = {"total_initial_A": 0.0, "total_mean_A": 0.0, "total_final_A": 0.0}
        anode_req = {"anode_count": 5}
        with pytest.raises(ValueError, match="Final current demand must be > 0"):
            _b401_verify_current_output(inputs, anode_req, 0.5, fake_demand)

    def test_zero_initial_demand_raises_in_verify(self, b401_base_config):
        """Zero initial current demand raises ValueError in adequacy check."""
        inputs = b401_base_config["inputs"]
        fake_demand = {"total_initial_A": 0.0, "total_mean_A": 1.0, "total_final_A": 1.0}
        anode_req = {"anode_count": 5}
        with pytest.raises(ValueError, match="Initial current demand must be > 0"):
            _b401_verify_current_output(inputs, anode_req, 0.5, fake_demand)

    def test_unknown_coating_category_raises_in_current_densities(self):
        """Unknown coating_category in current_densities raises ValueError."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "IV"}]
            },
            "environment": {"seawater_temperature_C": 10.0},
        }
        with pytest.raises(ValueError, match="Unknown coating category"):
            _b401_current_densities(inputs)

    def test_typo_coating_category_raises_in_current_densities(self):
        """Case-sensitive coating_category mismatch raises ValueError."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "Bare"}]
            },
            "environment": {"seawater_temperature_C": 10.0},
        }
        with pytest.raises(ValueError, match="Unknown coating category"):
            _b401_current_densities(inputs)

    def test_all_zero_area_raises_in_surface_areas(self):
        """All zones with area_m2=0 raises ValueError (zero total area)."""
        inputs = {
            "structure": {
                "zones": [
                    {"zone": "submerged", "area_m2": 0.0, "coating_category": "I"},
                    {"zone": "splash", "area_m2": 0.0, "coating_category": "III"},
                ]
            }
        }
        with pytest.raises(ValueError, match="Total surface area is zero"):
            _b401_surface_areas(inputs)

    def test_missing_zone_field_raises_in_surface_areas(self):
        """Zone entry without 'zone' field raises ValueError with diagnostics."""
        inputs = {
            "structure": {
                "zones": [{"area_m2": 1000.0, "coating_category": "I"}]
            }
        }
        with pytest.raises(ValueError, match="missing required 'zone' field"):
            _b401_surface_areas(inputs)

    def test_missing_area_field_raises_in_surface_areas(self):
        """Zone entry without 'area_m2' field raises ValueError with diagnostics."""
        inputs = {
            "structure": {
                "zones": [{"zone": "submerged", "coating_category": "I"}]
            }
        }
        with pytest.raises(ValueError, match="missing required 'area_m2' field"):
            _b401_surface_areas(inputs)

    def test_bracelet_stubby_geometry_raises(self):
        """Bracelet anode with 2πL/r <= e raises ValueError."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0}]},
            "environment": {"seawater_resistivity_ohm_m": 0.30},
            "anode": {
                "type": "bracelet",
                # For bracelet to fail: 2πL/r <= e -> L/r <= e/(2π) ≈ 0.433
                # L=0.03, r=0.10 -> L/r=0.3 -> 2πL/r ≈ 1.88 < e
                "length_m": 0.03,
                "radius_m": 0.10,
            },
        }
        with pytest.raises(ValueError, match="Bracelet anode geometry invalid"):
            _b401_anode_resistance(inputs)
