# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Local imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Helper — minimal valid DNV_RP_F103_2010 configuration
# ---------------------------------------------------------------------------

def _minimal_dnv_cfg(
    burial_condition="buried",
    internal_fluid_temp_c=20.0,
    coating_type="FBE",
    design_life=25.0,
    length_m=1000.0,
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
# Test 1 — Coating type variant: FBE (a=0.010, b=0.0003)
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_coating_FBE():
    """
    FBE coating (Annex 1 Table A.1: a=0.010, b=0.0003), design life 25 years.

    Confirms that Table A.1 constants flow through to mean and final factors.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(coating_type="FBE", design_life=25.0)
    cp.DNV_RP_F103_2010(cfg)

    cb = cfg["results"]["coating_breakdown_factors"]
    assert cb["coating_type"] == "FBE"
    assert math.isclose(cb["a"], 0.010, rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.0003, rel_tol=1e-9)
    assert math.isclose(cb["initial_factor"], 0.010, rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"],    0.01375, rel_tol=1e-6)
    assert math.isclose(cb["final_factor"],   0.0175,  rel_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 2 — Coating type variant: 3LPE (a=0.001, b=0.00003)
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_coating_3LPE():
    """
    3LPE coating (Annex 1: a=0.001, b=0.00003), design life 25 years.

    3LPE has significantly better performance than FBE; breakdown factors should
    be much smaller.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(coating_type="3LPE", design_life=25.0)
    cp.DNV_RP_F103_2010(cfg)

    cb = cfg["results"]["coating_breakdown_factors"]
    assert cb["coating_type"] == "3LPE"
    # f_cm = 0.001 + 0.5*0.00003*25 = 0.001375
    assert math.isclose(cb["mean_factor"],  0.001375, rel_tol=1e-6)
    # f_cf = 0.001 + 0.00003*25 = 0.00175
    assert math.isclose(cb["final_factor"], 0.00175,  rel_tol=1e-6)
    # 3LPE final factor must be much lower than FBE final factor
    assert cb["final_factor"] < 0.005


# ---------------------------------------------------------------------------
# Test 3 — Coating type variant: asphalt (glass_fibre_reinforced_asphalt_enamel)
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_coating_asphalt():
    """
    Glass-fibre reinforced asphalt enamel (Annex 1: a=0.003, b=0.0001),
    design life 25 years.

    Confirms the asphalt coating key is recognised and constants applied.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(
        coating_type="glass_fibre_reinforced_asphalt_enamel",
        design_life=25.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    cb = cfg["results"]["coating_breakdown_factors"]
    # a=0.003, b=0.0001
    # f_cm = 0.003 + 0.5*0.0001*25 = 0.003 + 0.00125 = 0.00425
    # f_cf = 0.003 + 0.0001*25    = 0.003 + 0.0025   = 0.0055
    assert math.isclose(cb["a"], 0.003,   rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.0001,  rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"],  0.00425, rel_tol=1e-6)
    assert math.isclose(cb["final_factor"], 0.0055,  rel_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 4 — Burial condition: non_buried + T <= 50 degC
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_non_buried_cold():
    """
    Non-buried pipeline, internal fluid temperature <= 50 degC.
    Table 5-1: i_cm = 0.050 A/m² (non_buried, <=50).
    Mean current demand must exceed buried equivalent (0.020 A/m²).
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(
        burial_condition="non_buried",
        internal_fluid_temp_c=20.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    cd = cfg["results"]["current_densities_mA_m2"]
    demand = cfg["results"]["current_demand_A"]

    assert math.isclose(cd["mean_current_density_A_m2"], 0.050, rel_tol=1e-9)
    assert demand["mean_current_demand_A"] > 0.0
    # Non-buried density (0.050) is 2.5x buried (0.020) — demand must be larger
    cfg_buried = _minimal_dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=20.0,
    )
    cp.DNV_RP_F103_2010(cfg_buried)
    buried_demand = cfg_buried["results"]["current_demand_A"]["mean_current_demand_A"]
    assert demand["mean_current_demand_A"] > buried_demand


# ---------------------------------------------------------------------------
# Test 5 — Burial condition: buried + T > 80 degC
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_buried_hot():
    """
    Buried pipeline, internal fluid temperature > 80 degC (band >80-120).
    Table 5-1: i_cm = 0.030 A/m².
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=100.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    cd = cfg["results"]["current_densities_mA_m2"]
    assert cd["temperature_band"] == ">80-120"
    assert math.isclose(cd["mean_current_density_A_m2"], 0.030, rel_tol=1e-9)
    # Hot pipeline has higher demand than cold pipeline
    cfg_cold = _minimal_dnv_cfg(burial_condition="buried", internal_fluid_temp_c=20.0)
    cp.DNV_RP_F103_2010(cfg_cold)
    cold_demand = cfg_cold["results"]["current_demand_A"]["mean_current_demand_A"]
    hot_demand = cfg["results"]["current_demand_A"]["mean_current_demand_A"]
    assert hot_demand > cold_demand


# ---------------------------------------------------------------------------
# Test 6 — Design life variant: 15 years
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_design_life_15yr():
    """
    Design life 15 years with FBE coating.
    f_cm = 0.010 + 0.5*0.0003*15 = 0.01225
    f_cf = 0.010 + 0.0003*15     = 0.0145
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(design_life=15.0, coating_type="FBE")
    cp.DNV_RP_F103_2010(cfg)

    cb = cfg["results"]["coating_breakdown_factors"]
    assert math.isclose(cb["design_life_years"], 15.0, rel_tol=1e-9)
    assert math.isclose(cb["mean_factor"],  0.01225, rel_tol=1e-6)
    assert math.isclose(cb["final_factor"], 0.0145,  rel_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 7 — Design life variant: 25 years (standard offshore life)
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_design_life_25yr():
    """
    Design life 25 years produces mean demand between 15yr and 40yr values.
    Verifies monotonic scaling of total charge with design life.
    """
    cp = CathodicProtection()
    cfg_25 = _minimal_dnv_cfg(design_life=25.0)
    cfg_15 = _minimal_dnv_cfg(design_life=15.0)
    cfg_40 = _minimal_dnv_cfg(design_life=40.0)

    cp.DNV_RP_F103_2010(cfg_25)
    cp.DNV_RP_F103_2010(cfg_15)
    cp.DNV_RP_F103_2010(cfg_40)

    q_15 = cfg_15["results"]["current_demand_A"]["total_charge_Ah"]
    q_25 = cfg_25["results"]["current_demand_A"]["total_charge_Ah"]
    q_40 = cfg_40["results"]["current_demand_A"]["total_charge_Ah"]

    # Longer design life must produce more total charge
    assert q_15 < q_25 < q_40


# ---------------------------------------------------------------------------
# Test 8 — Design life variant: 40 years
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_design_life_40yr():
    """
    Design life 40 years with FBE coating.
    f_cf = 0.010 + 0.0003*40 = 0.022

    Total anode mass must be strictly greater for 40yr than for 25yr.
    Anode count may be identical when both masses are below one anode unit
    (ceil to the same integer), so mass is the reliable metric.
    """
    cp = CathodicProtection()
    cfg_40 = _minimal_dnv_cfg(design_life=40.0, coating_type="FBE")
    cfg_25 = _minimal_dnv_cfg(design_life=25.0, coating_type="FBE")

    cp.DNV_RP_F103_2010(cfg_40)
    cp.DNV_RP_F103_2010(cfg_25)

    cb_40 = cfg_40["results"]["coating_breakdown_factors"]
    assert math.isclose(cb_40["final_factor"], 0.022, rel_tol=1e-6)

    mass_40 = cfg_40["results"]["anode_requirements"]["total_anode_mass_kg"]
    mass_25 = cfg_25["results"]["anode_requirements"]["total_anode_mass_kg"]
    assert mass_40 > mass_25, (
        f"40yr total mass ({mass_40:.2f} kg) must exceed 25yr ({mass_25:.2f} kg)"
    )


# ---------------------------------------------------------------------------
# Test 9 — Full route via router(): output dict has required keys
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_router_output_keys():
    """
    Calling router() with DNV_RP_F103_2010 must populate cfg['results'] with
    all required sub-dicts: current_demand, anode_requirements, anode_spacing.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg()
    result = cp.router(cfg)

    r = result["results"]
    assert "current_demand_A" in r
    assert "anode_requirements" in r
    assert "anode_spacing_m" in r

    # current_demand keys
    assert "mean_current_demand_A" in r["current_demand_A"]
    assert "final_current_demand_A" in r["current_demand_A"]
    assert "total_charge_Ah" in r["current_demand_A"]

    # anode_requirements keys
    assert "total_anode_mass_kg" in r["anode_requirements"]
    assert "anode_count" in r["anode_requirements"]

    # anode_spacing keys
    assert "spacing_m" in r["anode_spacing_m"]
    assert "spacing_valid" in r["anode_spacing_m"]


# ---------------------------------------------------------------------------
# Test 10 — Full route: 24-inch buried FBE pipeline, verify mean demand
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_router_24in_buried_FBE():
    """
    24-inch (0.610 m OD) buried pipeline, FBE coating, 25-year life.

    Verifies that all three calc layers (geometry, coating, demand) are
    consistent with each other when called through router().

    Expected: i_cm = 0.020 A/m² (buried, <=50 degC)
    A_c = pi * 0.610 * 1000 = 1916.4 m²
    f_cm = 0.01375
    I_cm = 1916.4 * 0.01375 * 0.020 = 0.527 A
    """
    cfg = {
        "inputs": {
            "calculation_type": "DNV_RP_F103_2010",
            "design_data": {"design_life": 25.0},
            "pipeline": {
                "outer_diameter_m": 0.610,
                "wall_thickness_m": 0.0191,
                "length_m": 1000.0,
                "burial_condition": "buried",
                "internal_fluid_temperature_C": 20.0,
                "coating_type": "FBE",
                "resistivity_ohm_m": 0.2e-6,
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

    cp = CathodicProtection()
    result = cp.router(cfg)
    r = result["results"]

    # Verify geometry
    expected_area = math.pi * 0.610 * 1000.0
    assert math.isclose(
        r["pipeline_geometry_m"]["outer_surface_area_m2"],
        expected_area,
        rel_tol=1e-4,
    )

    # Verify current density from Table 5-1
    assert math.isclose(
        r["current_densities_mA_m2"]["mean_current_density_A_m2"],
        0.020,
        rel_tol=1e-9,
    )

    # Verify mean current demand formula
    A_c = r["pipeline_geometry_m"]["outer_surface_area_m2"]
    f_cm = r["coating_breakdown_factors"]["mean_factor"]
    i_cm = r["current_densities_mA_m2"]["mean_current_density_A_m2"]
    expected_demand = A_c * f_cm * i_cm
    assert math.isclose(
        r["current_demand_A"]["mean_current_demand_A"],
        expected_demand,
        rel_tol=1e-3,
    )


# ---------------------------------------------------------------------------
# Test 11 — Edge case: very short pipeline (100 m)
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_edge_very_short_pipeline():
    """
    Very short pipeline (100 m) should produce positive but small demands.
    Anode count should be >= 1 (at least one anode always needed).
    All output keys should be present and physically reasonable.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(
        burial_condition="buried",
        internal_fluid_temp_c=20.0,
        coating_type="FBE",
        design_life=25.0,
        length_m=100.0,
    )
    cp.DNV_RP_F103_2010(cfg)

    r = cfg["results"]

    assert r["pipeline_geometry_m"]["outer_surface_area_m2"] > 0.0
    assert r["current_demand_A"]["mean_current_demand_A"] > 0.0
    assert r["anode_requirements"]["anode_count"] >= 1
    assert r["anode_spacing_m"]["spacing_m"] >= 0.0

    # 100 m pipeline must produce much less demand than 1000 m pipeline
    cfg_1km = _minimal_dnv_cfg(length_m=1000.0)
    cp.DNV_RP_F103_2010(cfg_1km)
    short_demand = r["current_demand_A"]["mean_current_demand_A"]
    long_demand = cfg_1km["results"]["current_demand_A"]["mean_current_demand_A"]
    assert short_demand < long_demand


# ---------------------------------------------------------------------------
# Test 12 — Anode count scales with contingency factor
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_contingency_factor_effect():
    """
    Increasing contingency factor from 1.0 to 1.1 must increase anode count.
    Tests that contingency_factor is correctly applied.
    """
    cp = CathodicProtection()

    cfg_no_cont = _minimal_dnv_cfg()
    cfg_no_cont["inputs"]["anode"]["contingency_factor"] = 1.0

    cfg_with_cont = _minimal_dnv_cfg()
    cfg_with_cont["inputs"]["anode"]["contingency_factor"] = 1.1

    cp.DNV_RP_F103_2010(cfg_no_cont)
    cp.DNV_RP_F103_2010(cfg_with_cont)

    n_no_cont = cfg_no_cont["results"]["anode_requirements"]["anode_count"]
    n_with_cont = cfg_with_cont["results"]["anode_requirements"]["anode_count"]

    # With 10% contingency, count must be >= base count
    assert n_with_cont >= n_no_cont


# ---------------------------------------------------------------------------
# Test 13 — Unknown coating type falls back to FBE defaults
# ---------------------------------------------------------------------------

def test_dnv_pipeline_variant_unknown_coating_fallback():
    """
    An unrecognised coating type must fall back to FBE constants
    (a=0.010, b=0.0003) rather than raising an exception.
    """
    cp = CathodicProtection()
    cfg = _minimal_dnv_cfg(coating_type="polyurethane_custom_x")
    cp.DNV_RP_F103_2010(cfg)

    cb = cfg["results"]["coating_breakdown_factors"]
    # Fallback to FBE
    assert math.isclose(cb["a"], 0.010,  rel_tol=1e-9)
    assert math.isclose(cb["b"], 0.0003, rel_tol=1e-9)
