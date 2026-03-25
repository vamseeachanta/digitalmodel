# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Local imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Helper — minimal valid ABS_gn_ships_2018 configuration
# ---------------------------------------------------------------------------

def _make_abs_ship_cfg(
    total_area=4500.0,
    area_coverage=95.0,
    design_life=5,
    seawater_temp=20,
    resistivity=0.25,
    anode_net_weight=29.0,
    utilisation=0.825,
    coated_cd=13.5,
    uncoated_cd=200.0,
    initial_breakdown=2.0,
    initial_duration=2.0,
    yearly_breakdown=3.0,
    max_breakdown_factor=2.0,
    anode_length=0.65,
    anode_width=0.125,
    anode_type="long_flush",
):
    """Build a minimal configuration dict for ABS_gn_ships_2018."""
    return {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {
                "design_life": design_life,
                "seawater_max_temperature": seawater_temp,
            },
            "environment": {
                "seawater": {"resistivity": {"input": resistivity}},
            },
            "structure": {
                "steel_total_area": total_area,
                "area_coverage": area_coverage,
                "coating_initial_breakdown_factor": initial_breakdown,
                "coating_initial_breakdown_duration": initial_duration,
                "coating_yearly_breakdown_factor": yearly_breakdown,
                "coating_breakdown_factor_max": max_breakdown_factor,
            },
            "design_current": {
                "coated_steel_mA_m2": coated_cd,
                "uncoated_steel_mA_m2": uncoated_cd,
            },
            "anode": {
                "material": "aluminium",
                "protection_potential": 0.8,
                "closed_circuit_anode_potential": -1.09,
                "anode_Utilisation_factor": utilisation,
                "physical_properties": {"net_weight": anode_net_weight},
                "geometry": {
                    "type": anode_type,
                    "length_m": anode_length,
                    "width_m": anode_width,
                },
            },
        }
    }


# ---------------------------------------------------------------------------
# Test 1 — Temperature sensitivity: 10 degC seawater
# Aluminium capacity = 2000 - 27*(10-20) = 2000 + 270 = 2270 Ah/kg
# ---------------------------------------------------------------------------

def test_abs_ship_variant_temperature_10c():
    """
    Seawater temperature 10 degC: Al capacity = 2000 - 27*(10-20) = 2270 Ah/kg.
    Higher capacity means less anode mass needed than at 20 degC.
    """
    cp = CathodicProtection()
    cfg_10 = _make_abs_ship_cfg(seawater_temp=10)
    cfg_20 = _make_abs_ship_cfg(seawater_temp=20)

    cp.ABS_gn_ships_2018(cfg_10)
    cp.ABS_gn_ships_2018(cfg_20)

    acc_10 = cfg_10["cathodic_protection"]["anode_current_capacity"]
    acc_20 = cfg_20["cathodic_protection"]["anode_current_capacity"]

    assert math.isclose(acc_10, 2270.0, rel_tol=1e-9), (
        f"Expected 2270 Ah/kg at 10 degC, got {acc_10}"
    )
    assert math.isclose(acc_20, 2000.0, rel_tol=1e-9)

    # Lower temperature → higher capacity → less anode mass required
    mass_10 = cfg_10["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    mass_20 = cfg_20["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    assert mass_10 < mass_20


# ---------------------------------------------------------------------------
# Test 2 — Temperature sensitivity: 20 degC seawater (nominal)
# ---------------------------------------------------------------------------

def test_abs_ship_variant_temperature_20c_nominal():
    """
    At 20 degC, aluminium capacity is the nominal 2000 Ah/kg.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg(seawater_temp=20)
    cp.ABS_gn_ships_2018(cfg)

    acc = cfg["cathodic_protection"]["anode_current_capacity"]
    assert math.isclose(acc, 2000.0, rel_tol=1e-9), (
        f"Expected 2000 Ah/kg at 20 degC, got {acc}"
    )


# ---------------------------------------------------------------------------
# Test 3 — Temperature sensitivity: 30 degC seawater
# Aluminium capacity = 2000 - 27*(30-20) = 2000 - 270 = 1730 Ah/kg
# ---------------------------------------------------------------------------

def test_abs_ship_variant_temperature_30c():
    """
    Seawater temperature 30 degC: Al capacity = 1730 Ah/kg.
    Higher temperature means less capacity, so more anode mass needed.
    """
    cp = CathodicProtection()
    cfg_30 = _make_abs_ship_cfg(seawater_temp=30)
    cfg_20 = _make_abs_ship_cfg(seawater_temp=20)

    cp.ABS_gn_ships_2018(cfg_30)
    cp.ABS_gn_ships_2018(cfg_20)

    acc = cfg_30["cathodic_protection"]["anode_current_capacity"]
    assert math.isclose(acc, 1730.0, rel_tol=1e-9), (
        f"Expected 1730 Ah/kg at 30 degC, got {acc}"
    )

    mass_30 = cfg_30["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    mass_20 = cfg_20["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    assert mass_30 > mass_20


# ---------------------------------------------------------------------------
# Test 4 — Design life sensitivity: 5 years
# ---------------------------------------------------------------------------

def test_abs_ship_variant_design_life_5yr():
    """
    Design life 5 years produces positive mean current demand.
    Mean demand must be used for anode mass calculation.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg(design_life=5)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    assert r["design_life"] == 5
    assert r["current_demand_A"]["totals"]["mean"] > 0.0
    assert r["anode_requirements"]["total_mass_kg"] > 0.0


# ---------------------------------------------------------------------------
# Test 5 — Design life sensitivity: 15 years
# Mean current demand must be larger than 5-year case due to greater
# coating breakdown accumulation.
# ---------------------------------------------------------------------------

def test_abs_ship_variant_design_life_15yr():
    """
    Design life 15 years: final coating factor grows from compounding.
    Mean demand must exceed the 5-year design life result.
    """
    cp = CathodicProtection()
    cfg_5 = _make_abs_ship_cfg(design_life=5)
    cfg_15 = _make_abs_ship_cfg(design_life=15)

    cp.ABS_gn_ships_2018(cfg_5)
    cp.ABS_gn_ships_2018(cfg_15)

    fcm_5  = cfg_5["cathodic_protection"]["coating_breakdown_factors"]["mean_factor"]
    fcm_15 = cfg_15["cathodic_protection"]["coating_breakdown_factors"]["mean_factor"]

    # Longer life → more accumulated breakdown → higher mean factor
    assert fcm_15 > fcm_5

    # More coating breakdown → higher mean current demand
    mean_5  = cfg_5["cathodic_protection"]["current_demand_A"]["totals"]["mean"]
    mean_15 = cfg_15["cathodic_protection"]["current_demand_A"]["totals"]["mean"]
    assert mean_15 > mean_5


# ---------------------------------------------------------------------------
# Test 6 — Design life sensitivity: 25 years (capped by max factor 2.0)
# ---------------------------------------------------------------------------

def test_abs_ship_variant_design_life_25yr():
    """
    Design life 25 years: with α=2%, β=3%/yr the raw compounded factor
    would be 1.02^2 * 1.03^23 >> 2.0, so final_factor is capped at 2.0.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg(design_life=25, max_breakdown_factor=2.0)
    cp.ABS_gn_ships_2018(cfg)

    bf = cfg["cathodic_protection"]["coating_breakdown_factors"]
    # Raw value grows large; cap at 2.0 must apply
    assert bf["final_factor"] <= 2.0
    assert math.isclose(bf["final_factor"], 2.0, rel_tol=1e-6), (
        f"Expected final_factor to be capped at 2.0, got {bf['final_factor']}"
    )


# ---------------------------------------------------------------------------
# Test 7 — Anode geometry variant: long_flush
# Resistance formula: R = ρ / (2 * s_mean), s_mean = 0.5*(L + W)
# ---------------------------------------------------------------------------

def test_abs_ship_variant_anode_type_long_flush():
    """
    Long-flush anode: resistance = ρ / (2 × s_mean) where s_mean = (L + W) / 2.

    For ρ=0.25 Ω·m, L=0.65 m, W=0.125 m:
    s_mean = (0.65 + 0.125) / 2 = 0.3875 m
    R = 0.25 / (2 × 0.3875) = 0.3226 Ω
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg(
        resistivity=0.25,
        anode_type="long_flush",
        anode_length=0.65,
        anode_width=0.125,
    )
    cp.ABS_gn_ships_2018(cfg)

    perf = cfg["cathodic_protection"]["anode_performance"]
    assert perf["resistance_ohm"]["initial"] is not None

    expected_r = 0.25 / (2.0 * 0.3875)
    assert math.isclose(
        perf["resistance_ohm"]["initial"],
        expected_r,
        rel_tol=1e-4,
    ), f"Expected R = {expected_r:.4f} Ω, got {perf['resistance_ohm']['initial']:.4f} Ω"


# ---------------------------------------------------------------------------
# Test 8 — Coating area coverage: 80% coated
# ---------------------------------------------------------------------------

def test_abs_ship_variant_coverage_80pct():
    """
    80% coating coverage: larger uncoated area (900 m²) increases disbonding
    current contribution compared to 95% coverage.
    """
    cp = CathodicProtection()
    cfg_80 = _make_abs_ship_cfg(total_area=4500.0, area_coverage=80.0)
    cfg_95 = _make_abs_ship_cfg(total_area=4500.0, area_coverage=95.0)

    cp.ABS_gn_ships_2018(cfg_80)
    cp.ABS_gn_ships_2018(cfg_95)

    areas_80 = cfg_80["cathodic_protection"]["current_demand_A"]["areas_m2"]
    areas_95 = cfg_95["cathodic_protection"]["current_demand_A"]["areas_m2"]

    # 80% coverage has more uncoated area
    assert areas_80["uncoated"] > areas_95["uncoated"]
    assert math.isclose(areas_80["uncoated"], 900.0, rel_tol=1e-6)
    assert math.isclose(areas_95["uncoated"], 225.0, rel_tol=1e-6)

    # More uncoated area → more initial current demand
    init_80 = cfg_80["cathodic_protection"]["current_demand_A"]["totals"]["initial"]
    init_95 = cfg_95["cathodic_protection"]["current_demand_A"]["totals"]["initial"]
    assert init_80 > init_95


# ---------------------------------------------------------------------------
# Test 9 — Coating area coverage: 100% coated (no uncoated area)
# ---------------------------------------------------------------------------

def test_abs_ship_variant_coverage_100pct():
    """
    100% coating coverage: no disbonding current contribution from uncoated area.
    Uncoated area must be zero; disbonding current must be zero.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg(total_area=3000.0, area_coverage=100.0)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]["current_demand_A"]
    assert math.isclose(r["areas_m2"]["uncoated"], 0.0, abs_tol=1e-6)
    assert math.isclose(r["disbonding"]["mean"],  0.0, abs_tol=1e-6)
    assert math.isclose(r["disbonding"]["final"], 0.0, abs_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 10 — Second ship config: large vessel, 10,000 m² area
# ---------------------------------------------------------------------------

def test_abs_ship_variant_large_vessel_10000m2():
    """
    Large vessel with 10,000 m² total steel area.
    Anode mass must scale proportionally with area relative to 4,500 m² case.
    """
    cp = CathodicProtection()
    cfg_small = _make_abs_ship_cfg(total_area=4500.0, area_coverage=95.0)
    cfg_large = _make_abs_ship_cfg(total_area=10000.0, area_coverage=95.0)

    cp.ABS_gn_ships_2018(cfg_small)
    cp.ABS_gn_ships_2018(cfg_large)

    mass_small = cfg_small["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    mass_large = cfg_large["cathodic_protection"]["anode_requirements"]["total_mass_kg"]

    # Large vessel must need significantly more anode mass
    assert mass_large > mass_small
    # Ratio of masses should roughly equal ratio of areas (linear scaling)
    area_ratio = 10000.0 / 4500.0
    mass_ratio = mass_large / mass_small
    assert math.isclose(mass_ratio, area_ratio, rel_tol=0.01), (
        f"Mass ratio {mass_ratio:.4f} should be close to area ratio {area_ratio:.4f}"
    )


# ---------------------------------------------------------------------------
# Test 11 — Driving voltage calculation
# ΔE = |E_a - E_p| = |-1.09 - (-0.80)| = 0.29 V
# ---------------------------------------------------------------------------

def test_abs_ship_variant_driving_voltage():
    """
    Driving voltage: ΔE = |E_a − E_p| = |−1.09 − (−0.80)| = 0.29 V.
    Verifies the anode performance calculation is consistent.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg()
    cp.ABS_gn_ships_2018(cfg)

    perf = cfg["cathodic_protection"]["anode_performance"]
    assert math.isclose(perf["driving_voltage_V"], 0.29, abs_tol=1e-6)


# ---------------------------------------------------------------------------
# Test 12 — Mean current is the anode mass sizing basis
# Verifies formula: M = I_mean * t * 8760 / (acc * u)
# ---------------------------------------------------------------------------

def test_abs_ship_variant_anode_mass_formula():
    """
    ABS GN Ships formula: M = I_mean * t_f * 8760 / (acc * u).

    Manually compute expected mass and verify against the code output.
    """
    cp = CathodicProtection()
    design_life = 5
    utilisation = 0.825
    temp = 20

    cfg = _make_abs_ship_cfg(
        design_life=design_life,
        seawater_temp=temp,
        utilisation=utilisation,
    )
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    acc = r["anode_current_capacity"]
    mean_current = r["current_demand_A"]["totals"]["mean"]
    reported_mass = r["anode_requirements"]["total_mass_kg"]

    expected_mass = mean_current * design_life * 8760.0 / (acc * utilisation)
    assert math.isclose(reported_mass, expected_mass, rel_tol=1e-5), (
        f"Expected mass {expected_mass:.4f} kg, got {reported_mass:.4f} kg"
    )


# ---------------------------------------------------------------------------
# Test 13 — Output structure: ABS ships router returns cfg['cathodic_protection']
# ---------------------------------------------------------------------------

def test_abs_ship_variant_router_output_structure():
    """
    Calling router() with ABS_gn_ships_2018 must populate
    cfg['cathodic_protection'] (not cfg['results']) with required sub-dicts.
    """
    cp = CathodicProtection()
    cfg = _make_abs_ship_cfg()
    result = cp.router(cfg)

    assert "cathodic_protection" in result
    r = result["cathodic_protection"]

    required_keys = [
        "temperature",
        "design_life",
        "anode_current_capacity",
        "coating_breakdown_factors",
        "current_densities_mA_m2",
        "current_demand_A",
        "anode_requirements",
        "anode_performance",
    ]
    for key in required_keys:
        assert key in r, f"Missing key in cathodic_protection output: {key}"

    # current_demand_A must have 'totals' sub-dict
    assert "totals" in r["current_demand_A"]
    assert "mean" in r["current_demand_A"]["totals"]
    assert "final" in r["current_demand_A"]["totals"]
