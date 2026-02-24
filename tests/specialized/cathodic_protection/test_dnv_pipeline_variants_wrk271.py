# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Local imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Helper — minimal valid DNV_RP_F103_2010 configuration (WRK-279 structure)
# ---------------------------------------------------------------------------

def _dnv_cfg(
    burial_condition="buried",
    internal_fluid_temp_c=20.0,
    coating_type="FBE",
    design_life=25.0,
    length_m=1000.0,
    anode_material="aluminium",
    utilization_factor=0.80,
    individual_anode_mass_kg=150.0,
    contingency_factor=1.0,
):
    """Return a minimal valid DNV_RP_F103_2010 configuration dictionary."""
    return {
        "inputs": {
            "calculation_type": "DNV_RP_F103_2010",
            "design_data": {"design_life": design_life},
            "pipeline": {
                "outer_diameter_m": 0.508,          # 20-inch OD
                "wall_thickness_m": 0.0159,         # 15.9 mm WT
                "length_m": length_m,
                "burial_condition": burial_condition,
                "internal_fluid_temperature_C": internal_fluid_temp_c,
                "coating_type": coating_type,
                "resistivity_ohm_m": 0.2e-6,        # CMn steel
            },
            "environment": {},
            "anode": {
                "material": anode_material,
                "utilization_factor": utilization_factor,
                "individual_anode_mass_kg": individual_anode_mass_kg,
                "contingency_factor": contingency_factor,
                "min_spacing_m": 5.0,
                "max_spacing_m": 300.0,
            },
        }
    }


# ---------------------------------------------------------------------------
# Test 1 — Non-buried, T<=50 degC, FBE coating
# Table 5-1: i_cm = 0.050 A/m^2 (non_buried, <=50)
# ---------------------------------------------------------------------------

def test_dnv_non_buried_low_temp_fbe():
    """
    Non-buried pipeline, internal fluid temperature 30 degC (<=50 band).
    Table 5-1: i_cm = 0.050 A/m^2.
    Annex 1 FBE: a=0.010, b=0.0003.
    Mean demand at 25yr: A_c * 0.01375 * 0.050
    """
    cp = CathodicProtection()
    cfg = _dnv_cfg(
        burial_condition="non_buried",
        internal_fluid_temp_c=30.0,
        coating_type="FBE",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    r = cfg["results"]
    cd = r["current_densities_mA_m2"]
    cb = r["coating_breakdown_factors"]
    demand = r["current_demand_A"]

    # Table 5-1 check
    assert cd["temperature_band"] == "<=50"
    assert math.isclose(cd["mean_current_density_A_m2"], 0.050, rel_tol=1e-9)

    # Annex 1 FBE check
    assert math.isclose(cb["a"], 0.010, rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.0003, rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"], 0.01375, rel_tol=1e-6)

    # Demand must be positive
    assert demand["mean_current_demand_A"] > 0.0

    # Non-buried demand must exceed buried demand for same conditions
    cfg_buried = _dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=30.0,
        coating_type="FBE",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg_buried)
    buried_demand = cfg_buried["results"]["current_demand_A"]["mean_current_demand_A"]
    assert demand["mean_current_demand_A"] > buried_demand


# ---------------------------------------------------------------------------
# Test 2 — Non-buried, T>80 degC, 3LPE coating
# Table 5-1: i_cm = 0.070 A/m^2 (non_buried, >80-120)
# ---------------------------------------------------------------------------

def test_dnv_non_buried_high_temp_3lpe():
    """
    Non-buried pipeline, internal fluid temperature 90 degC (>80-120 band).
    Table 5-1: i_cm = 0.070 A/m^2.
    3LPE: a=0.001, b=0.00003.
    f_cm = 0.001 + 0.5*0.00003*25 = 0.001375
    """
    cp = CathodicProtection()
    cfg = _dnv_cfg(
        burial_condition="non_buried",
        internal_fluid_temp_c=90.0,
        coating_type="3LPE",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    r = cfg["results"]
    cd = r["current_densities_mA_m2"]
    cb = r["coating_breakdown_factors"]

    assert cd["temperature_band"] == ">80-120"
    assert math.isclose(cd["mean_current_density_A_m2"], 0.070, rel_tol=1e-9)

    # 3LPE coating constants
    assert math.isclose(cb["a"], 0.001, rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.00003, rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"], 0.001375, rel_tol=1e-6)

    # 3LPE final factor must be much lower than FBE
    assert cb["final_factor"] < 0.003


# ---------------------------------------------------------------------------
# Test 3 — Buried, T<=50 degC, asphalt enamel coating
# Table 5-1: i_cm = 0.020 A/m^2 (buried, <=50)
# ---------------------------------------------------------------------------

def test_dnv_buried_low_temp_asphalt():
    """
    Buried pipeline, internal fluid temperature 30 degC (<=50 band), asphalt enamel.
    Table 5-1: i_cm = 0.020 A/m^2.
    Asphalt: a=0.003, b=0.0001.
    f_cm = 0.003 + 0.5*0.0001*25 = 0.00425
    """
    cp = CathodicProtection()
    cfg = _dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=30.0,
        coating_type="glass_fibre_reinforced_asphalt_enamel",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    r = cfg["results"]
    cd = r["current_densities_mA_m2"]
    cb = r["coating_breakdown_factors"]

    assert cd["temperature_band"] == "<=50"
    assert math.isclose(cd["mean_current_density_A_m2"], 0.020, rel_tol=1e-9)

    # Asphalt coating constants
    assert math.isclose(cb["a"], 0.003, rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.0001, rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"], 0.00425, rel_tol=1e-6)
    assert math.isclose(cb["final_factor"], 0.0055, rel_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 4 — Buried, T>80 degC, 3LPP coating
# Table 5-1: i_cm = 0.030 A/m^2 (buried, >80-120)
# ---------------------------------------------------------------------------

def test_dnv_buried_high_temp_3lpp():
    """
    Buried pipeline, internal fluid temperature 90 degC (>80-120 band), 3LPP coating.
    Table 5-1: i_cm = 0.030 A/m^2.
    3LPP: a=0.001, b=0.00003 (same as 3LPE per Annex 1 Table A.1).
    """
    cp = CathodicProtection()
    cfg = _dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=90.0,
        coating_type="3LPP",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    r = cfg["results"]
    cd = r["current_densities_mA_m2"]
    cb = r["coating_breakdown_factors"]
    demand = r["current_demand_A"]

    assert cd["temperature_band"] == ">80-120"
    assert math.isclose(cd["mean_current_density_A_m2"], 0.030, rel_tol=1e-9)

    # 3LPP has the same Annex 1 constants as 3LPE
    assert math.isclose(cb["a"], 0.001, rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.00003, rel_tol=1e-9)

    # Demand must be positive
    assert demand["mean_current_demand_A"] > 0.0


# ---------------------------------------------------------------------------
# Test 5 — Short design life: 10 years
# ---------------------------------------------------------------------------

def test_dnv_short_design_life_10yr():
    """
    10-year design life with FBE coating.
    f_cm = 0.010 + 0.5*0.0003*10 = 0.0115
    f_cf = 0.010 + 0.0003*10     = 0.013
    Total charge must be less than for 25-year design.
    """
    cp = CathodicProtection()
    cfg_10 = _dnv_cfg(design_life=10.0, coating_type="FBE")
    cfg_25 = _dnv_cfg(design_life=25.0, coating_type="FBE")

    cp.DNV_RP_F103_2010(cfg_10)
    cp.DNV_RP_F103_2010(cfg_25)

    cb_10 = cfg_10["results"]["coating_breakdown_factors"]
    assert math.isclose(cb_10["design_life_years"], 10.0, rel_tol=1e-9)
    assert math.isclose(cb_10["mean_factor"],  0.0115, rel_tol=1e-6), (
        f"f_cm at 10yr: expected 0.0115, got {cb_10['mean_factor']}"
    )
    assert math.isclose(cb_10["final_factor"], 0.013,  rel_tol=1e-6), (
        f"f_cf at 10yr: expected 0.013, got {cb_10['final_factor']}"
    )

    q_10 = cfg_10["results"]["current_demand_A"]["total_charge_Ah"]
    q_25 = cfg_25["results"]["current_demand_A"]["total_charge_Ah"]
    assert q_10 < q_25, (
        f"10yr charge ({q_10:.1f} Ah) must be less than 25yr charge ({q_25:.1f} Ah)"
    )


# ---------------------------------------------------------------------------
# Test 6 — Long design life: 40 years
# ---------------------------------------------------------------------------

def test_dnv_long_design_life_40yr():
    """
    40-year design life with FBE coating.
    f_cf = 0.010 + 0.0003*40 = 0.022
    Anode mass must exceed 25-year case.
    """
    cp = CathodicProtection()
    cfg_40 = _dnv_cfg(design_life=40.0, coating_type="FBE")
    cfg_25 = _dnv_cfg(design_life=25.0, coating_type="FBE")

    cp.DNV_RP_F103_2010(cfg_40)
    cp.DNV_RP_F103_2010(cfg_25)

    cb_40 = cfg_40["results"]["coating_breakdown_factors"]
    assert math.isclose(cb_40["final_factor"], 0.022, rel_tol=1e-6), (
        f"f_cf at 40yr: expected 0.022, got {cb_40['final_factor']}"
    )

    mass_40 = cfg_40["results"]["anode_requirements"]["total_anode_mass_kg"]
    mass_25 = cfg_25["results"]["anode_requirements"]["total_anode_mass_kg"]
    assert mass_40 > mass_25, (
        f"40yr mass ({mass_40:.2f} kg) must exceed 25yr mass ({mass_25:.2f} kg)"
    )


# ---------------------------------------------------------------------------
# Test 7 — Short pipeline: 1 km
# ---------------------------------------------------------------------------

def test_dnv_short_pipeline_1km():
    """
    1 km (1000 m) pipeline: surface area = pi * 0.508 * 1000 = 1595.9 m^2.
    Mean current demand at buried/cold/FBE/25yr must be positive and less than 10 km.
    """
    cp = CathodicProtection()
    cfg_1km = _dnv_cfg(length_m=1000.0)
    cfg_10km = _dnv_cfg(length_m=10000.0)

    cp.DNV_RP_F103_2010(cfg_1km)
    cp.DNV_RP_F103_2010(cfg_10km)

    r_1km = cfg_1km["results"]
    # Surface area check: pi * D * L = pi * 0.508 * 1000
    expected_area = math.pi * 0.508 * 1000.0
    assert math.isclose(
        r_1km["pipeline_geometry_m"]["outer_surface_area_m2"],
        expected_area,
        rel_tol=1e-4,
    )

    demand_1km = r_1km["current_demand_A"]["mean_current_demand_A"]
    demand_10km = cfg_10km["results"]["current_demand_A"]["mean_current_demand_A"]

    assert demand_1km > 0.0
    assert demand_1km < demand_10km

    # Length ratio check: 10km demand should be ~10x 1km demand (linear)
    ratio = demand_10km / demand_1km
    assert math.isclose(ratio, 10.0, rel_tol=0.01), (
        f"Demand ratio (10km/1km) should be ~10, got {ratio:.3f}"
    )


# ---------------------------------------------------------------------------
# Test 8 — Long pipeline: 100 km
# ---------------------------------------------------------------------------

def test_dnv_long_pipeline_100km():
    """
    100 km (100,000 m) pipeline.
    Surface area = pi * 0.508 * 100000 = 159,593 m^2.
    Anode mass must scale linearly with length relative to 1 km case.
    """
    cp = CathodicProtection()
    cfg_100km = _dnv_cfg(length_m=100000.0)
    cfg_1km = _dnv_cfg(length_m=1000.0)

    cp.DNV_RP_F103_2010(cfg_100km)
    cp.DNV_RP_F103_2010(cfg_1km)

    mass_100km = cfg_100km["results"]["anode_requirements"]["total_anode_mass_kg"]
    mass_1km = cfg_1km["results"]["anode_requirements"]["total_anode_mass_kg"]

    # 100km pipeline must require far more anode mass than 1km
    assert mass_100km > mass_1km

    # Mass ratio should match length ratio (linear scaling)
    mass_ratio = mass_100km / mass_1km
    assert math.isclose(mass_ratio, 100.0, rel_tol=0.01), (
        f"Mass ratio (100km/1km) should be ~100, got {mass_ratio:.3f}"
    )

    # Verify anode count is positive
    count_100km = cfg_100km["results"]["anode_requirements"]["anode_count"]
    assert count_100km >= 1


# ---------------------------------------------------------------------------
# Test 9 — Zinc anode material (ε = 780 Ah/kg)
# ---------------------------------------------------------------------------

def test_dnv_zinc_anode_material():
    """
    Zinc anodes: electrochemical capacity = 780 Ah/kg.
    Because zinc has ~2.6x less capacity than aluminium (2000 Ah/kg),
    zinc requires more mass for the same charge demand.
    """
    cp = CathodicProtection()
    cfg_zinc = _dnv_cfg(anode_material="zinc")
    cfg_al = _dnv_cfg(anode_material="aluminium")

    cp.DNV_RP_F103_2010(cfg_zinc)
    cp.DNV_RP_F103_2010(cfg_al)

    r_zinc = cfg_zinc["results"]["anode_requirements"]
    r_al   = cfg_al["results"]["anode_requirements"]

    # Verify zinc capacity is 780 Ah/kg
    assert math.isclose(r_zinc["anode_capacity_Ah_kg"], 780.0, rel_tol=1e-9), (
        f"Expected 780 Ah/kg for zinc, got {r_zinc['anode_capacity_Ah_kg']}"
    )

    # Zinc requires more mass than aluminium for same charge demand
    assert r_zinc["total_anode_mass_kg"] > r_al["total_anode_mass_kg"], (
        f"Zinc mass ({r_zinc['total_anode_mass_kg']:.2f}) must exceed Al mass "
        f"({r_al['total_anode_mass_kg']:.2f})"
    )

    # Ratio should be approximately 2000/780 ≈ 2.564
    mass_ratio = r_zinc["total_anode_mass_kg"] / r_al["total_anode_mass_kg"]
    expected_ratio = 2000.0 / 780.0
    assert math.isclose(mass_ratio, expected_ratio, rel_tol=0.01), (
        f"Mass ratio zinc/al = {mass_ratio:.3f}, expected ~{expected_ratio:.3f}"
    )


# ---------------------------------------------------------------------------
# Test 10 — Bracelet vs elongated anode utilisation (u=0.80 vs u=0.90)
# ---------------------------------------------------------------------------

def test_dnv_bracelet_vs_elongated_utilization():
    """
    Bracelet anode: utilisation_factor = 0.80 (typical).
    Elongated stand-off: utilisation_factor = 0.90 (more efficient).

    Higher utilisation factor → less anode mass required for the same charge demand.
    Mass formula: M = Q_total / (acc * u)
    At u=0.90, M = Q / (2000 * 0.90) < Q / (2000 * 0.80) at u=0.80.
    """
    cp = CathodicProtection()
    cfg_bracelet = _dnv_cfg(utilization_factor=0.80)
    cfg_elongated = _dnv_cfg(utilization_factor=0.90)

    cp.DNV_RP_F103_2010(cfg_bracelet)
    cp.DNV_RP_F103_2010(cfg_elongated)

    mass_bracelet = cfg_bracelet["results"]["anode_requirements"]["total_anode_mass_kg"]
    mass_elongated = cfg_elongated["results"]["anode_requirements"]["total_anode_mass_kg"]

    # Higher utilisation → less required mass
    assert mass_bracelet > mass_elongated, (
        f"Bracelet mass (u=0.80, {mass_bracelet:.2f} kg) must exceed "
        f"elongated mass (u=0.90, {mass_elongated:.2f} kg)"
    )

    # Ratio should be 0.90 / 0.80 = 1.125
    mass_ratio = mass_bracelet / mass_elongated
    expected_ratio = 0.90 / 0.80
    assert math.isclose(mass_ratio, expected_ratio, rel_tol=0.01), (
        f"Mass ratio (u=0.80 / u=0.90) = {mass_ratio:.4f}, expected {expected_ratio:.4f}"
    )
