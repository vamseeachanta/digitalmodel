"""
ABOUTME: Test suite for DNV-RP-B401-2021 cathodic protection calculations
ABOUTME: Tests all helper functions and router dispatch for offshore fixed platform CP design
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


class TestB401CoatingBreakdown:
    """Test _b401_coating_breakdown per B401-2021 Sec.3.4.6."""

    def test_coating_breakdown_category_I_25yr(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        # Cat I: f_cf = 0.05 + 0.020 * 25 = 0.55
        assert result["submerged"]["f_cf"] == pytest.approx(0.55, abs=0.001)

    def test_coating_breakdown_category_III_25yr(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        # Cat III: f_cf = 0.25 + 0.050 * 25 = 1.50, clamped to 1.0
        assert result["splash"]["f_cf"] == pytest.approx(1.0, abs=0.001)

    def test_coating_breakdown_bare_any_life(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["coating_category"] = "bare"
        result = _b401_coating_breakdown(inputs, 25.0)
        assert result["submerged"]["f_cf"] == pytest.approx(1.0, abs=0.001)
        assert result["submerged"]["f_cm"] == pytest.approx(1.0, abs=0.001)
        assert result["submerged"]["f_ci"] == pytest.approx(1.0, abs=0.001)

    def test_coating_breakdown_mean_formula(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_coating_breakdown(inputs, 25.0)
        # Cat I: f_cm = 0.05 + 0.020 * 12.5 = 0.30
        assert result["submerged"]["f_cm"] == pytest.approx(0.30, abs=0.001)

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
    """Test _b401_current_densities per B401-2021 Table 3-1."""

    def test_current_density_submerged_10C_coated(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_current_densities(inputs)
        # 10C -> band ">7-12" -> coated = 0.050
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.050, abs=0.001)

    def test_current_density_submerged_5C_bare(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_temperature_C"] = 5.0
        inputs["structure"]["zones"][0]["coating_category"] = "bare"
        result = _b401_current_densities(inputs)
        # 5C -> band "<=7" -> bare = 0.060
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.060, abs=0.001)

    def test_current_density_splash_coated(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        result = _b401_current_densities(inputs)
        # splash -> coated = 0.100, temp-independent
        assert result["splash"]["i_mean_A_m2"] == pytest.approx(0.100, abs=0.001)

    def test_current_density_atmospheric_bare(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][2]["coating_category"] = "bare"
        result = _b401_current_densities(inputs)
        # atmospheric -> bare = 0.030
        assert result["atmospheric"]["i_mean_A_m2"] == pytest.approx(0.030, abs=0.001)

    def test_current_density_tropical_submerged(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_temperature_C"] = 20.0
        result = _b401_current_densities(inputs)
        # 20C -> band ">17" -> coated = 0.070
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.070, abs=0.001)

    def test_unknown_zone_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["structure"]["zones"][0]["zone"] = "deep_buried"
        with pytest.raises(ValueError, match="Unknown zone"):
            _b401_current_densities(inputs)


class TestB401CurrentDemand:
    """Test _b401_current_demand per B401-2021."""

    def test_current_demand_submerged_zone(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        result = _b401_current_demand(inputs, areas, densities, breakdown)
        # submerged: I_mean = 5000 * 0.050 * 0.30 = 75.0 A
        assert result["submerged"]["I_mean_A"] == pytest.approx(75.0, abs=0.5)

    def test_current_demand_total_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        result = _b401_current_demand(inputs, areas, densities, breakdown)
        assert result["total_mean_A"] > 0
        assert result["total_final_A"] > 0

    def test_current_demand_all_zones_present(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        result = _b401_current_demand(inputs, areas, densities, breakdown)
        assert "submerged" in result
        assert "splash" in result
        assert "atmospheric" in result
        assert "total_mean_A" in result
        assert "total_final_A" in result

    def test_final_demand_exceeds_mean(self, b401_base_config):
        """Final coating breakdown > mean, so final demand must exceed mean."""
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        result = _b401_current_demand(inputs, areas, densities, breakdown)
        assert result["total_final_A"] > result["total_mean_A"]


class TestB401AnodeResistance:
    """Test _b401_anode_resistance per B401-2021 Sec.4.9."""

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
        # Dwight: R = (rho / 2piL) * (ln(4L/r) - 1)
        # rho=0.30, L=1.0, r=0.05
        expected = (0.30 / (2.0 * math.pi * 1.0)) * (math.log(4.0 * 1.0 / 0.05) - 1.0)
        result = _b401_anode_resistance(inputs)
        assert result == pytest.approx(expected, abs=0.001)

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
    """Test _b401_anode_requirements per B401-2021 Sec.4.3."""

    def test_anode_mass_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        result = _b401_anode_requirements(inputs, demand)
        assert result["total_mass_kg"] > 0

    def test_anode_count_positive(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        result = _b401_anode_requirements(inputs, demand)
        assert result["anode_count"] >= 1

    def test_anode_mass_formula(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        result = _b401_anode_requirements(inputs, demand)
        # M = (I_mean * T * 8760) / (epsilon * u); Al epsilon=2000, u=0.85
        I_mean = demand["total_mean_A"]
        expected_mass = (I_mean * 25.0 * 8760.0) / (2000.0 * 0.85)
        assert result["total_mass_kg"] == pytest.approx(expected_mass, rel=0.01)

    def test_anode_requirements_zero_life_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["design_data"]["design_life"] = 0.0
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        with pytest.raises(ValueError, match="Design life must be > 0"):
            _b401_anode_requirements(inputs, demand)


class TestB401VerifyCurrentOutput:
    """Test _b401_verify_current_output per B401-2021."""

    def _get_demand(self, inputs):
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        return _b401_current_demand(inputs, areas, densities, breakdown)

    def test_verification_returns_pass_or_fail(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        demand = self._get_demand(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        assert "adequate" in result
        assert isinstance(result["adequate"], bool)

    def test_verification_checks_final_demand(self, b401_base_config):
        """Verification must compare against final (end-of-life) current demand."""
        inputs = b401_base_config["inputs"]
        demand = self._get_demand(inputs)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # Result must include both final and mean demand values
        assert "final_current_demand_A" in result
        assert "mean_current_demand_A" in result
        assert result["final_current_demand_A"] > result["mean_current_demand_A"]

    def test_verification_adequate_with_long_anodes(self):
        """Long anodes (3 m) produce higher current output — adequate=True for small platform."""
        inputs = {
            "design_data": {"design_life": 25.0},
            "structure": {
                "zones": [
                    {"zone": "submerged",   "area_m2": 1000.0, "coating_category": "I"},
                    {"zone": "splash",      "area_m2":   50.0, "coating_category": "I"},
                    {"zone": "atmospheric", "area_m2":   30.0, "coating_category": "I"},
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
                "length_m": 3.0,   # Long anodes → lower resistance → more current
                "radius_m": 0.05,
            },
        }
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, demand)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
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

    def test_results_standard_name(self, cp_calculator, b401_base_config):
        cfg = b401_base_config
        cp_calculator.router(cfg)
        assert cfg["results"]["standard"] == "DNV-RP-B401-2021"

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
        # Both entries use submerged lookup; Cat I and Cat III both use "coated"
        assert result["sub_cat1"]["i_mean_A_m2"] == pytest.approx(0.050, abs=0.001)
        assert result["sub_cat3"]["i_mean_A_m2"] == pytest.approx(0.050, abs=0.001)
        assert result["sub_cat1"]["base_zone"] == "submerged"

    def test_unknown_material_raises_in_anode_requirements(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["material"] = "magnesium"
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        with pytest.raises(ValueError, match="Unknown anode material"):
            _b401_anode_requirements(inputs, demand)

    def test_utilization_factor_above_1_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["anode"]["utilization_factor"] = 1.5
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        with pytest.raises(ValueError, match="Utilization factor must be in"):
            _b401_anode_requirements(inputs, demand)

    def test_zero_resistivity_raises(self, b401_base_config):
        inputs = b401_base_config["inputs"]
        inputs["environment"]["seawater_resistivity_ohm_m"] = 0.0
        with pytest.raises(ValueError, match="Seawater resistivity must be > 0"):
            _b401_anode_resistance(inputs)

    def test_boundary_temperature_7C(self):
        """Temperature exactly at 7°C boundary uses <=7 band."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 7.0},
        }
        result = _b401_current_densities(inputs)
        assert result["submerged"]["temperature_band"] == "<=7"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.040, abs=0.001)

    def test_boundary_temperature_12C(self):
        """Temperature exactly at 12°C boundary uses >7-12 band."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 12.0},
        }
        result = _b401_current_densities(inputs)
        assert result["submerged"]["temperature_band"] == ">7-12"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.050, abs=0.001)

    def test_boundary_temperature_17C(self):
        """Temperature exactly at 17°C boundary uses >12-17 band."""
        inputs = {
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
            "environment": {"seawater_temperature_C": 17.0},
        }
        result = _b401_current_densities(inputs)
        assert result["submerged"]["temperature_band"] == ">12-17"
        assert result["submerged"]["i_mean_A_m2"] == pytest.approx(0.060, abs=0.001)

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

    def test_recommended_count_satisfies_both_criteria(self, b401_base_config):
        """recommended_anode_count always satisfies both mass and current criteria."""
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        anode_req = _b401_anode_requirements(inputs, demand)
        resistance = _b401_anode_resistance(inputs)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        # recommended_count >= mass count (always)
        assert result["recommended_anode_count"] >= result["anode_count"]
        # recommended_count satisfies current output criterion
        I_per_anode = result["anode_current_output_per_anode_A"]
        I_final = result["final_current_demand_A"]
        assert result["recommended_anode_count"] * I_per_anode >= I_final

    def test_verification_result_has_recommended_count(self, b401_base_config):
        """Verification result includes recommended_anode_count field."""
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
        anode_req = _b401_anode_requirements(inputs, demand)
        resistance = _b401_anode_resistance(inputs)
        result = _b401_verify_current_output(inputs, anode_req, resistance, demand)
        assert "recommended_anode_count" in result
        assert isinstance(result["recommended_anode_count"], int)

    def test_zero_resistance_raises_in_verify(self, b401_base_config):
        """Zero resistance raises ValueError in current output verification."""
        inputs = b401_base_config["inputs"]
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        demand = _b401_current_demand(inputs, areas, densities, breakdown)
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
        areas = _b401_surface_areas(inputs)
        densities = _b401_current_densities(inputs)
        breakdown = _b401_coating_breakdown(inputs, 25.0)
        # Force zero areas to get zero demand
        fake_demand = {"total_mean_A": 0.0, "total_final_A": 0.0}
        anode_req = {"anode_count": 5}
        with pytest.raises(ValueError, match="Final current demand must be > 0"):
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
                "length_m": 0.05,   # L/r = 0.05/0.05 = 1.0 → 2πL/r ≈ 6.28 > e → actually valid
                "radius_m": 0.10,   # L/r = 0.05/0.10 = 0.5 → 2πL/r ≈ 3.14 > e → still valid
            },
        }
        # For bracelet to fail: 2πL/r <= e → L/r <= e/(2π) ≈ 0.433
        # Use L=0.03, r=0.10 → L/r=0.3 → 2πL/r≈1.88 < e
        inputs["anode"]["length_m"] = 0.03
        inputs["anode"]["radius_m"] = 0.10
        with pytest.raises(ValueError, match="Bracelet anode geometry invalid"):
            _b401_anode_resistance(inputs)
