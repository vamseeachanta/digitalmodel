# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Reader imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _minimal_dnv_cfg(burial_condition="buried", internal_fluid_temp_c=20.0,
                     coating_type="FBE", design_life=25.0):
    """Return a minimal valid DNV_RP_F103_2010 configuration dictionary."""
    return {
        "inputs": {
            "calculation_type": "DNV_RP_F103_2010",
            "design_data": {"design_life": design_life},
            "pipeline": {
                "outer_diameter_m": 0.508,          # 20-inch OD
                "wall_thickness_m": 0.0159,         # 15.9 mm WT
                "length_m": 1000.0,
                "burial_condition": burial_condition,
                "internal_fluid_temperature_C": internal_fluid_temp_c,
                "coating_type": coating_type,
                "resistivity_ohm_m": 0.2e-6,        # CMn steel (F103-2010 Sec.5.6.10)
            },
            "environment": {},
            "anode": {
                "material": "aluminium",
                "utilization_factor": 0.80,
                "individual_anode_mass_kg": 150.0,
                "contingency_factor": 1.0,
                "min_spacing_m": 5.0,
                "max_spacing_m": 300.0,
            },
        }
    }


# ---------------------------------------------------------------------------
# G-1 / G-2 / G-3: _dnv_current_densities uses Table 5-1 only
# ---------------------------------------------------------------------------

def test_dnv_f103_2010_current_density_non_buried_cold_pipeline():
    """
    Table 5-1: non_buried, internal fluid temperature <= 50 degC.
    Expected mean current density: 0.050 A/m2.
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "burial_condition": "non_buried",
            "internal_fluid_temperature_C": 20.0,
        }
    }
    result = cp._dnv_current_densities(inputs)

    assert result["burial_condition"] == "non_buried"
    assert result["temperature_band"] == "<=50"
    assert math.isclose(result["mean_current_density_A_m2"], 0.050, rel_tol=1e-9), (
        f"Expected 0.050 A/m2 from Table 5-1 (non_buried, <=50 degC), "
        f"got {result['mean_current_density_A_m2']}"
    )
    # Confirm no Arrhenius correction or coating-quality key in output
    assert "temperature_correction_factor" not in result
    assert "coating_quality" not in result


def test_dnv_f103_2010_current_density_buried_warm_pipeline():
    """
    Table 5-1: buried, internal fluid temperature > 50 - 80 degC.
    Expected mean current density: 0.025 A/m2.
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "burial_condition": "buried",
            "internal_fluid_temperature_C": 65.0,
        }
    }
    result = cp._dnv_current_densities(inputs)

    assert result["burial_condition"] == "buried"
    assert result["temperature_band"] == ">50-80"
    assert math.isclose(result["mean_current_density_A_m2"], 0.025, rel_tol=1e-9), (
        f"Expected 0.025 A/m2 from Table 5-1 (buried, >50-80 degC), "
        f"got {result['mean_current_density_A_m2']}"
    )


def test_dnv_f103_2010_current_density_buried_hot_pipeline():
    """
    Table 5-1: buried, internal fluid temperature > 120 degC.
    Expected mean current density: 0.040 A/m2.
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "burial_condition": "buried",
            "internal_fluid_temperature_C": 130.0,
        }
    }
    result = cp._dnv_current_densities(inputs)

    assert result["temperature_band"] == ">120"
    assert math.isclose(result["mean_current_density_A_m2"], 0.040, rel_tol=1e-9), (
        f"Expected 0.040 A/m2 from Table 5-1 (buried, >120 degC), "
        f"got {result['mean_current_density_A_m2']}"
    )


def test_dnv_f103_2010_current_density_non_buried_high_temperature():
    """
    Table 5-1: non_buried, > 80-120 degC.
    Expected 0.070 A/m2.
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "burial_condition": "non_buried",
            "internal_fluid_temperature_C": 100.0,
        }
    }
    result = cp._dnv_current_densities(inputs)
    assert result["temperature_band"] == ">80-120"
    assert math.isclose(result["mean_current_density_A_m2"], 0.070, rel_tol=1e-9)


def test_dnv_f103_2010_table51_all_cells():
    """Verify all 8 cells of Table 5-1 against the PDF values."""
    cp = CathodicProtection()
    expected = {
        ("non_buried", 20.0):  (0.050, "<=50"),
        ("non_buried", 60.0):  (0.060, ">50-80"),
        ("non_buried", 100.0): (0.070, ">80-120"),
        ("non_buried", 130.0): (0.100, ">120"),
        ("buried",     20.0):  (0.020, "<=50"),
        ("buried",     60.0):  (0.025, ">50-80"),
        ("buried",     100.0): (0.030, ">80-120"),
        ("buried",     130.0): (0.040, ">120"),
    }
    for (burial, temp), (expected_cd, expected_band) in expected.items():
        inputs = {
            "pipeline": {
                "burial_condition": burial,
                "internal_fluid_temperature_C": temp,
            }
        }
        result = cp._dnv_current_densities(inputs)
        assert result["temperature_band"] == expected_band, (
            f"burial={burial}, temp={temp}: band mismatch"
        )
        assert math.isclose(result["mean_current_density_A_m2"], expected_cd, rel_tol=1e-9), (
            f"burial={burial}, temp={temp}: expected {expected_cd}, got "
            f"{result['mean_current_density_A_m2']}"
        )


# ---------------------------------------------------------------------------
# G-4: _dnv_coating_breakdown uses F103-2010 Annex 1 linear formula
# ---------------------------------------------------------------------------

def test_dnv_f103_2010_coating_breakdown_linear_FBE():
    """
    Annex 1 linear formula for FBE coating (Table A.1: a=0.01, b=0.0003).
    Design life t_f = 25 years.
      f_ci = a = 0.010
      f_cm = a + 0.5*b*t_f = 0.010 + 0.5*0.0003*25 = 0.010 + 0.00375 = 0.01375
      f_cf = a + b*t_f = 0.010 + 0.0003*25 = 0.010 + 0.0075 = 0.0175
    """
    cp = CathodicProtection()
    inputs = {"pipeline": {"coating_type": "FBE", "burial_condition": "buried"}}
    result = cp._dnv_coating_breakdown(inputs, design_life=25.0)

    assert math.isclose(result["initial_factor"], 0.010, rel_tol=1e-9), (
        f"f_ci: expected 0.010, got {result['initial_factor']}"
    )
    assert math.isclose(result["mean_factor"], 0.01375, rel_tol=1e-6), (
        f"f_cm: expected 0.01375, got {result['mean_factor']}"
    )
    assert math.isclose(result["final_factor"], 0.0175, rel_tol=1e-6), (
        f"f_cf: expected 0.0175, got {result['final_factor']}"
    )
    # Confirm no compound exponential artifacts
    assert "wet_storage_years" not in result
    assert "yearly_factor" not in result


def test_dnv_f103_2010_coating_breakdown_linear_3LPE():
    """
    Annex 1 linear formula for 3LPE coating (Table A.1: a=0.001, b=0.00003).
    Design life t_f = 30 years.
      f_cm = 0.001 + 0.5*0.00003*30 = 0.001 + 0.00045 = 0.00145
      f_cf = 0.001 + 0.00003*30 = 0.001 + 0.0009 = 0.0019
    """
    cp = CathodicProtection()
    inputs = {"pipeline": {"coating_type": "3LPE", "burial_condition": "buried"}}
    result = cp._dnv_coating_breakdown(inputs, design_life=30.0)

    assert math.isclose(result["initial_factor"], 0.001, rel_tol=1e-9)
    assert math.isclose(result["mean_factor"], 0.00145, rel_tol=1e-6), (
        f"f_cm: expected 0.00145, got {result['mean_factor']}"
    )
    assert math.isclose(result["final_factor"], 0.0019, rel_tol=1e-6), (
        f"f_cf: expected 0.0019, got {result['final_factor']}"
    )


def test_dnv_f103_2010_coating_breakdown_override_ab():
    """
    User can supply a and b directly to override the table lookup.
    a=0.05, b=0.02, t_f=10 years.
      f_cm = 0.05 + 0.5*0.02*10 = 0.05 + 0.10 = 0.15
      f_cf = 0.05 + 0.02*10 = 0.05 + 0.20 = 0.25
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "coating_type": "FBE",
            "burial_condition": "buried",
            "coating_breakdown_a": 0.05,
            "coating_breakdown_b": 0.02,
        }
    }
    result = cp._dnv_coating_breakdown(inputs, design_life=10.0)

    assert math.isclose(result["initial_factor"], 0.05, rel_tol=1e-9)
    assert math.isclose(result["mean_factor"], 0.15, rel_tol=1e-6)
    assert math.isclose(result["final_factor"], 0.25, rel_tol=1e-6)


def test_dnv_f103_2010_coating_breakdown_mean_is_midpoint():
    """
    For any a/b/t_f, f_cm must equal exactly (f_ci + f_cf) / 2,
    because f_cm = a + 0.5*b*t_f and f_cf = a + b*t_f imply
    f_cm = (f_ci + f_cf) / 2 when f_ci = a.
    """
    cp = CathodicProtection()
    inputs = {"pipeline": {"coating_type": "FBE", "burial_condition": "buried"}}
    result = cp._dnv_coating_breakdown(inputs, design_life=20.0)

    midpoint = (result["initial_factor"] + result["final_factor"]) / 2.0
    assert math.isclose(result["mean_factor"], midpoint, rel_tol=1e-9), (
        f"f_cm ({result['mean_factor']}) should equal (f_ci + f_cf)/2 ({midpoint})"
    )


# ---------------------------------------------------------------------------
# G-5: _dnv_pipeline_geometry longitudinal resistance uses F103-2010 Eq.11
# ---------------------------------------------------------------------------

def test_dnv_f103_2010_longitudinal_resistance_eq11():
    """
    F103-2010 Eq.11: R_Me = L * rho_Me / (pi * d * (D - d))
    Per unit length: R_Me/m = rho_Me / (pi * d * (D - d))

    Example: D=0.508 m, d=0.0159 m, rho_Me=0.2e-6 ohm.m
      denom = pi * 0.0159 * (0.508 - 0.0159) = pi * 0.0159 * 0.4921
      R_Me/m = 0.2e-6 / denom
    """
    cp = CathodicProtection()
    inputs = {
        "pipeline": {
            "outer_diameter_m": 0.508,
            "wall_thickness_m": 0.0159,
            "length_m": 1000.0,
            "resistivity_ohm_m": 0.2e-6,
        }
    }
    result = cp._dnv_pipeline_geometry(inputs)

    D = 0.508
    d = 0.0159
    rho = 0.2e-6
    expected_r_per_m = rho / (math.pi * d * (D - d))

    # rel_tol=1e-4 accommodates the round(..., 10) storage in the return dict
    assert math.isclose(
        result["longitudinal_resistance_ohm_per_m"], expected_r_per_m, rel_tol=1e-4
    ), (
        f"Expected R_Me/m = {expected_r_per_m:.6e}, got "
        f"{result['longitudinal_resistance_ohm_per_m']:.6e}"
    )
    # Default resistivity should be 0.2e-6 (CMn steel per F103-2010 Sec.5.6.10)
    assert math.isclose(result["pipe_resistivity_ohm_m"], 0.2e-6, rel_tol=1e-9)


def test_dnv_f103_2010_pipeline_default_resistivity_is_cmn_steel():
    """
    Default pipe resistivity must be 0.2e-6 ohm.m (CMn steel per F103-2010 Sec.5.6.10),
    not 2e-7 which was the previous value (same magnitude but confirm default is applied).
    """
    cp = CathodicProtection()
    # Use inputs with no resistivity specified
    inputs = {
        "pipeline": {
            "outer_diameter_m": 0.4,
            "wall_thickness_m": 0.012,
            "length_m": 500.0,
        }
    }
    result = cp._dnv_pipeline_geometry(inputs)
    assert math.isclose(result["pipe_resistivity_ohm_m"], 0.2e-6, rel_tol=1e-9), (
        "Default resistivity for CMn steel should be 0.2e-6 ohm.m per F103-2010 Sec.5.6.10"
    )


# ---------------------------------------------------------------------------
# Integration: full DNV_RP_F103_2010 route end-to-end
# ---------------------------------------------------------------------------

def test_dnv_f103_2010_full_route_buried_cold():
    """
    End-to-end DNV_RP_F103_2010 route with buried pipeline, T<=50 degC.
    Validates that i_cm = 0.020 A/m2 flows through to mean current demand.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(burial_condition="buried", internal_fluid_temp_c=20.0,
                           coating_type="FBE", design_life=25.0)
    cp.DNV_RP_F103_2010(cfg)

    results = cfg["results"]
    cd = results["current_densities_mA_m2"]
    cb = results["coating_breakdown_factors"]
    demand = results["current_demand_A"]

    # Table 5-1 value must be 0.020
    assert math.isclose(cd["mean_current_density_A_m2"], 0.020, rel_tol=1e-9)

    # Annex 1: FBE a=0.01, b=0.0003, t_f=25
    expected_f_cm = 0.01 + 0.5 * 0.0003 * 25.0
    assert math.isclose(cb["mean_factor"], expected_f_cm, rel_tol=1e-6)

    # Mean current demand: A_c * f_cm * i_cm
    # Use abs_tol=1e-3 A to accommodate round(...,3) in return dict
    A_c = results["pipeline_geometry_m"]["outer_surface_area_m2"]
    expected_mean_demand = A_c * expected_f_cm * 0.020
    assert math.isclose(
        demand["mean_current_demand_A"], expected_mean_demand, rel_tol=1e-3
    )
    # Demand must be positive and physically reasonable
    assert demand["mean_current_demand_A"] > 0.0
    assert demand["mean_current_demand_A"] < 1000.0  # sanity upper bound for 1 km pipe


def test_dnv_f103_2010_full_route_non_buried_warm():
    """
    End-to-end route: non_buried pipeline, T=65 degC (>50-80 band).
    Expected i_cm = 0.060 A/m2.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(burial_condition="non_buried", internal_fluid_temp_c=65.0,
                           coating_type="3LPE", design_life=20.0)
    cp.DNV_RP_F103_2010(cfg)

    cd = cfg["results"]["current_densities_mA_m2"]
    assert math.isclose(cd["mean_current_density_A_m2"], 0.060, rel_tol=1e-9), (
        f"Expected 0.060 A/m2 for non_buried >50-80 degC, got {cd['mean_current_density_A_m2']}"
    )
