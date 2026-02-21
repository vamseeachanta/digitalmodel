# Standard library imports
import math

# Third party imports
import pytest

# Local imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Shared helper — minimal valid cfg for ABS GN Offshore 2018
# ---------------------------------------------------------------------------

def _make_cfg(
    surface_area_m2=1000.0,
    water_depth_m=50.0,
    climatic_region="tropical",
    design_life_years=25.0,
    anode_material="aluminium",
    seawater_temp=25.0,
    coating_depth_zone="deep",   # "shallow" (<=30 m) or "deep" (>30 m)
    zone="submerged",            # "submerged" or "saline_mud"
):
    """Build a minimal configuration dict for ABS_gn_offshore_2018."""
    return {
        "inputs": {
            "calculation_type": "ABS_gn_offshore_2018",
            "design_data": {
                "design_life": design_life_years,
                "seawater_max_temperature": seawater_temp,
            },
            "structure": {
                "surface_area_m2": surface_area_m2,
                "water_depth_m": water_depth_m,
                "climatic_region": climatic_region,
                "zone": zone,
            },
            "anode": {
                "material": anode_material,
                "utilisation_factor": 0.80,
                "physical_properties": {"net_weight": 100.0},
            },
        }
    }


# ---------------------------------------------------------------------------
# Test 1 — Tropical / deep-water zone (>30 m, submerged)
#   PDF Section 2, Table 1 (p.12): tropical > 20°C, depth >30–100 m → i_ci = 120 mA/m²
#   Mean = 1/2 × i_ci = 60 mA/m²; Final = 2/3 × i_ci = 80 mA/m²
#   Coating (deep, >30 m, α=0.01, β_mean=0.004, β_final=0.008, t=25):
#     f_ci = 0.01; f_cm = 0.01 + 0.004*25 = 0.11; f_cf = 0.01 + 0.008*25 = 0.21
#   I_cm = 1000 * 60e-3 * 0.11 = 6.6 A
#   I_cf = 1000 * 80e-3 * 0.21 = 16.8 A
#   Anode mass = I_cm * t_years * 8760 / (acc * u)
#             = 6.6 * 25 * 8760 / (2000 * 0.80) = 902.25 kg  (Al at 25°C → acc=2000)
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_tropical_deep_submerged():
    """
    Tropical region, 50 m depth (>30 m band), 25-year design life.

    Verifies: current densities, coating breakdown, current demand, and anode
    mass all follow ABS GN Offshore 2018 Section 2 Table 1 and Section 2/11.7.
    """
    cfg = _make_cfg(
        surface_area_m2=1000.0,
        water_depth_m=50.0,
        climatic_region="tropical",
        design_life_years=25.0,
        seawater_temp=25.0,
        anode_material="aluminium",
        coating_depth_zone="deep",
        zone="submerged",
    )

    cp = CathodicProtection()
    result = cp.router(cfg)

    r = result["results"]

    # --- current densities (mA/m²) ---
    cd = r["current_densities_mA_m2"]
    assert math.isclose(cd["initial"], 120.0, rel_tol=1e-6), f"i_ci expected 120, got {cd['initial']}"
    assert math.isclose(cd["mean"],    60.0,  rel_tol=1e-6), f"i_cm expected 60, got {cd['mean']}"
    assert math.isclose(cd["final"],   80.0,  rel_tol=1e-6), f"i_cf expected 80, got {cd['final']}"

    # --- coating breakdown factors ---
    cb = r["coating_breakdown_factors"]
    assert math.isclose(cb["initial"], 0.01,  rel_tol=1e-6)
    assert math.isclose(cb["mean"],    0.11,  rel_tol=1e-6)
    assert math.isclose(cb["final"],   0.21,  rel_tol=1e-6)

    # --- current demand (A) ---
    dem = r["current_demand_A"]
    assert math.isclose(dem["mean"],  6.6,  rel_tol=1e-4)
    assert math.isclose(dem["final"], 16.8, rel_tol=1e-4)

    # --- anode mass ---
    assert r["anode_mass_kg"] > 0.0
    # Al anode capacity at 25°C: acc = 2000 - 27*(25-20) = 1865 Ah/kg
    acc = 2000.0 - 27.0 * (25.0 - 20.0)
    expected_mass = 6.6 * 25.0 * 8760.0 / (acc * 0.80)
    assert math.isclose(r["anode_mass_kg"], expected_mass, rel_tol=1e-4)


# ---------------------------------------------------------------------------
# Test 2 — Sub-tropical / shallow zone (<=30 m, submerged)
#   PDF Table 1 (p.12): sub-tropical 12–20°C, 0–30 m → i_ci = 170 mA/m²
#   Mean = 85 mA/m²; Final = 113.333 mA/m²
#   Coating (shallow, <=30 m, α=0.01, β_mean=0.006, β_final=0.012, t=20):
#     f_ci = 0.01; f_cm = 0.01 + 0.006*20 = 0.13; f_cf = 0.01 + 0.012*20 = 0.25
#   I_cm = 500 * 85e-3 * 0.13 = 5.525 A
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_subtropical_shallow_submerged():
    """
    Sub-tropical region, 15 m depth (<=30 m band), 20-year design life.

    Verifies Table 1 sub-tropical shallow current densities and shallow-water
    coating breakdown factors (α=0.01, β=0.006/0.012).
    """
    cfg = _make_cfg(
        surface_area_m2=500.0,
        water_depth_m=15.0,
        climatic_region="sub_tropical",
        design_life_years=20.0,
        seawater_temp=15.0,
        anode_material="aluminium",
        coating_depth_zone="shallow",
        zone="submerged",
    )

    result = CathodicProtection().router(cfg)
    r = result["results"]

    cd = r["current_densities_mA_m2"]
    assert math.isclose(cd["initial"], 170.0, rel_tol=1e-6)
    assert math.isclose(cd["mean"],    85.0,  rel_tol=1e-6)
    assert math.isclose(cd["final"],   170.0 * 2.0 / 3.0, rel_tol=1e-5)

    cb = r["coating_breakdown_factors"]
    assert math.isclose(cb["initial"], 0.01, rel_tol=1e-6)
    assert math.isclose(cb["mean"],    0.13, rel_tol=1e-6)
    assert math.isclose(cb["final"],   0.25, rel_tol=1e-6)

    dem = r["current_demand_A"]
    expected_icm = 500.0 * 85e-3 * 0.13
    assert math.isclose(dem["mean"], expected_icm, rel_tol=1e-5)
    assert dem["final"] > dem["mean"]


# ---------------------------------------------------------------------------
# Test 3 — Saline mud zone
#   PDF Section 2/11.7.2 (p.12): saline mud i_ci = 25 mA/m²; i_cm = i_cf = 20 mA/m²
#   No coating breakdown factor applied to mud zone (bare mud contact)
#   I_cm = A * i_cm = 200 * 20e-3 = 4.0 A
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_saline_mud_zone():
    """
    Saline mud / buried zone uses fixed current densities from PDF p.12:
    i_ci=25 mA/m², i_cm=i_cf=20 mA/m².  Coating breakdown is 1.0 (bare).
    """
    cfg = _make_cfg(
        surface_area_m2=200.0,
        water_depth_m=100.0,
        climatic_region="tropical",
        design_life_years=25.0,
        seawater_temp=25.0,
        anode_material="aluminium",
        zone="saline_mud",
    )

    result = CathodicProtection().router(cfg)
    r = result["results"]

    cd = r["current_densities_mA_m2"]
    assert math.isclose(cd["initial"], 25.0, rel_tol=1e-6), f"mud i_ci expected 25, got {cd['initial']}"
    assert math.isclose(cd["mean"],    20.0, rel_tol=1e-6), f"mud i_cm expected 20, got {cd['mean']}"
    assert math.isclose(cd["final"],   20.0, rel_tol=1e-6), f"mud i_cf expected 20, got {cd['final']}"

    dem = r["current_demand_A"]
    # Mud zone: coating factor = 1.0 (bare), I_cm = 200 * 20e-3 * 1.0 = 4.0 A
    assert math.isclose(dem["mean"], 4.0, rel_tol=1e-5)
    assert math.isclose(dem["initial"], 5.0, rel_tol=1e-5)


# ---------------------------------------------------------------------------
# Test 4 — Invalid climatic region raises ValueError
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_invalid_climatic_region_raises():
    """An unrecognised climatic_region string must raise ValueError."""
    cfg = _make_cfg(climatic_region="antarctic")   # not a valid key
    with pytest.raises(ValueError, match="climatic_region"):
        CathodicProtection().router(cfg)


# ---------------------------------------------------------------------------
# Test 5 — Invalid zone raises ValueError
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_invalid_zone_raises():
    """An unrecognised zone string must raise ValueError."""
    cfg = _make_cfg(zone="splash_zone_undefined")
    with pytest.raises(ValueError, match="zone"):
        CathodicProtection().router(cfg)


# ---------------------------------------------------------------------------
# Test 6 — Arctic region, very deep (>300 m)
#   PDF Table 1 (p.12): Arctic <=7°C, >300 m → i_ci = 220 mA/m²
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_arctic_very_deep():
    """
    Arctic region, depth > 300 m: i_ci = 220 mA/m² per Table 1 (p.12).
    """
    cfg = _make_cfg(
        surface_area_m2=800.0,
        water_depth_m=500.0,
        climatic_region="arctic",
        design_life_years=25.0,
        seawater_temp=2.0,
        anode_material="zinc",
        zone="submerged",
    )

    result = CathodicProtection().router(cfg)
    r = result["results"]

    cd = r["current_densities_mA_m2"]
    assert math.isclose(cd["initial"], 220.0, rel_tol=1e-6)
    assert math.isclose(cd["mean"],    110.0, rel_tol=1e-6)   # 1/2 * 220
    assert math.isclose(cd["final"],   220.0 * 2.0 / 3.0, rel_tol=1e-5)


# ---------------------------------------------------------------------------
# Test 7 — Route is registered in router()
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_registered_in_router():
    """
    Calling router() with calculation_type='ABS_gn_offshore_2018' must
    return a dict with a 'results' key — confirming the route is wired.
    """
    cfg = _make_cfg()
    result = CathodicProtection().router(cfg)
    assert "results" in result, "router() must populate cfg['results']"
    assert "current_densities_mA_m2" in result["results"]
    assert "anode_mass_kg" in result["results"]


# ---------------------------------------------------------------------------
# Test 8 — Temperate region, 100–300 m depth band
#   PDF Table 1 (p.12): temperate 7–12°C, 100–300 m → i_ci = 190 mA/m²
# ---------------------------------------------------------------------------
def test_abs_offshore_2018_temperate_mid_depth():
    """
    Temperate region, depth in the 100–300 m band: i_ci = 190 mA/m² (Table 1, p.12).
    """
    cfg = _make_cfg(
        surface_area_m2=600.0,
        water_depth_m=200.0,
        climatic_region="temperate",
        design_life_years=30.0,
        seawater_temp=10.0,
        anode_material="aluminium",
        zone="submerged",
    )

    result = CathodicProtection().router(cfg)
    r = result["results"]

    cd = r["current_densities_mA_m2"]
    assert math.isclose(cd["initial"], 190.0, rel_tol=1e-6)
    assert math.isclose(cd["mean"],     95.0, rel_tol=1e-6)
    assert math.isclose(cd["final"],   190.0 * 2.0 / 3.0, rel_tol=1e-5)
    assert r["anode_mass_kg"] > 0.0
