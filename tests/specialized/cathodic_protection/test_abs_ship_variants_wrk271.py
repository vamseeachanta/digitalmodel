# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Local imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


# ---------------------------------------------------------------------------
# Helper — minimal valid ABS_gn_ships_2018 configuration
# Parameters match the FST hull reference case from calc-011 and example-02.
# ---------------------------------------------------------------------------

def _base_cfg(
    design_life=5,
    seawater_temp=20,
    resistivity=0.325,
    area=10778.0,
    coverage=100.0,
    initial_breakdown=1.0,
    initial_duration=2.0,
    yearly_breakdown=1.0,
    max_factor=2.0,
    coated_cd=13.5,
    uncoated_cd=200.0,
    anode_type="long_flush",
    anode_length=1.00,
    anode_width=0.125,
    net_weight=29.0,
    utilisation=0.825,
):
    """Build a minimal ABS_gn_ships_2018 configuration dict."""
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
                "steel_total_area": area,
                "area_coverage": coverage,
                "coating_initial_breakdown_factor": initial_breakdown,
                "coating_initial_breakdown_duration": initial_duration,
                "coating_yearly_breakdown_factor": yearly_breakdown,
                "coating_breakdown_factor_max": max_factor,
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
                "physical_properties": {"net_weight": net_weight},
                "geometry": {
                    "type": anode_type,
                    "length_m": anode_length,
                    "width_m": anode_width,
                },
            },
        }
    }


# ---------------------------------------------------------------------------
# Test 1 — Cold water (10 °C): Al capacity = 2000 - 27*(10-20) = 2270 Ah/kg
# ---------------------------------------------------------------------------

def test_abs_temperature_10c():
    """
    Cold seawater at 10 degC: aluminium capacity = 2000 - 27*(10-20) = 2270 Ah/kg.

    Higher capacity at lower temperature reduces required anode mass
    relative to the standard 20 degC case.
    """
    cp = CathodicProtection()
    cfg = _base_cfg(seawater_temp=10)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    acc = r["anode_current_capacity"]

    assert math.isclose(acc, 2270.0, rel_tol=1e-9), (
        f"Expected 2270 Ah/kg at 10 degC, got {acc}"
    )

    # Cold water → higher capacity → less anode mass than 20 degC case
    cfg_20 = _base_cfg(seawater_temp=20)
    cp.ABS_gn_ships_2018(cfg_20)
    mass_10 = r["anode_requirements"]["total_mass_kg"]
    mass_20 = cfg_20["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    assert mass_10 < mass_20, (
        f"Mass at 10 degC ({mass_10:.2f} kg) must be less than at 20 degC ({mass_20:.2f} kg)"
    )


# ---------------------------------------------------------------------------
# Test 2 — Standard baseline (20 °C): Al capacity = 2000 Ah/kg
# ---------------------------------------------------------------------------

def test_abs_temperature_20c():
    """
    Standard seawater at 20 degC: aluminium capacity is the nominal 2000 Ah/kg.
    All other outputs must be positive and physically reasonable.
    """
    cp = CathodicProtection()
    cfg = _base_cfg(seawater_temp=20)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    acc = r["anode_current_capacity"]

    assert math.isclose(acc, 2000.0, rel_tol=1e-9), (
        f"Expected 2000 Ah/kg at 20 degC, got {acc}"
    )
    assert r["current_demand_A"]["totals"]["mean"] > 0.0
    assert r["anode_requirements"]["total_mass_kg"] > 0.0


# ---------------------------------------------------------------------------
# Test 3 — Warm water (30 °C): Al capacity = 2000 - 27*(30-20) = 1730 Ah/kg
# ---------------------------------------------------------------------------

def test_abs_temperature_30c():
    """
    Warm seawater at 30 degC: aluminium capacity = 1730 Ah/kg.
    Lower capacity at higher temperature increases required anode mass.
    """
    cp = CathodicProtection()
    cfg = _base_cfg(seawater_temp=30)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    acc = r["anode_current_capacity"]

    assert math.isclose(acc, 1730.0, rel_tol=1e-9), (
        f"Expected 1730 Ah/kg at 30 degC, got {acc}"
    )

    # Warm water → lower capacity → more anode mass than 20 degC case
    cfg_20 = _base_cfg(seawater_temp=20)
    cp.ABS_gn_ships_2018(cfg_20)
    mass_30 = r["anode_requirements"]["total_mass_kg"]
    mass_20 = cfg_20["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    assert mass_30 > mass_20, (
        f"Mass at 30 degC ({mass_30:.2f} kg) must exceed 20 degC ({mass_20:.2f} kg)"
    )


# ---------------------------------------------------------------------------
# Test 4 — Short design life: 5 years
# ---------------------------------------------------------------------------

def test_abs_design_life_5yr():
    """
    5-year design life produces a positive mean current demand and anode mass.
    The result dict must carry design_life = 5.
    """
    cp = CathodicProtection()
    cfg = _base_cfg(design_life=5)
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    assert r["design_life"] == 5
    assert r["current_demand_A"]["totals"]["mean"] > 0.0
    assert r["anode_requirements"]["total_mass_kg"] > 0.0


# ---------------------------------------------------------------------------
# Test 5 — Medium design life: 15 years
# ---------------------------------------------------------------------------

def test_abs_design_life_15yr():
    """
    15-year design life: accumulated coating breakdown is larger than 5-year case.
    Mean current demand must exceed the 5-year value.
    """
    cp = CathodicProtection()
    cfg_5 = _base_cfg(design_life=5)
    cfg_15 = _base_cfg(design_life=15)

    cp.ABS_gn_ships_2018(cfg_5)
    cp.ABS_gn_ships_2018(cfg_15)

    # Longer life → more accumulated breakdown → higher mean factor
    fcm_5  = cfg_5["cathodic_protection"]["coating_breakdown_factors"]["mean_factor"]
    fcm_15 = cfg_15["cathodic_protection"]["coating_breakdown_factors"]["mean_factor"]
    assert fcm_15 > fcm_5, (
        f"fcm_15 ({fcm_15:.4f}) must exceed fcm_5 ({fcm_5:.4f})"
    )

    # More coating breakdown → higher mean current demand
    mean_5  = cfg_5["cathodic_protection"]["current_demand_A"]["totals"]["mean"]
    mean_15 = cfg_15["cathodic_protection"]["current_demand_A"]["totals"]["mean"]
    assert mean_15 > mean_5


# ---------------------------------------------------------------------------
# Test 6 — Long design life: 25 years (final factor capped at 2.0)
# ---------------------------------------------------------------------------

def test_abs_design_life_25yr():
    """
    25-year design life with α=1%/yr: fcf = 1.01^25 ≈ 1.282, well below 2.0 cap.
    Must also be strictly greater than the 15-year mean factor.
    """
    cp = CathodicProtection()
    cfg_25 = _base_cfg(design_life=25)
    cfg_15 = _base_cfg(design_life=15)

    cp.ABS_gn_ships_2018(cfg_25)
    cp.ABS_gn_ships_2018(cfg_15)

    fcf_25 = cfg_25["cathodic_protection"]["coating_breakdown_factors"]["final_factor"]
    # Cap must not be breached (1.01^25 ≈ 1.28, not 2.0)
    assert fcf_25 <= 2.0
    assert fcf_25 > 1.0   # Must show some breakdown

    # Total anode mass must scale with design life
    mass_25 = cfg_25["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    mass_15 = cfg_15["cathodic_protection"]["anode_requirements"]["total_mass_kg"]
    assert mass_25 > mass_15, (
        f"Mass at 25yr ({mass_25:.2f}) must exceed 15yr ({mass_15:.2f})"
    )


# ---------------------------------------------------------------------------
# Test 7 — Zero coating breakdown (worst-case bare steel equivalent)
# ---------------------------------------------------------------------------

def test_abs_zero_coating_breakdown():
    """
    Zero coating breakdown rates (α=β=0%) gives initial_factor = yearly_factor = 1.0,
    final_factor = 1.0, mean_factor = 1.0.

    This is an atypical edge case used to verify the formula handles zero rates.
    Mean current demand equals the bare-steel equivalent for 100%-coated area.
    """
    cp = CathodicProtection()
    cfg = _base_cfg(
        initial_breakdown=0.0,
        initial_duration=0.0,
        yearly_breakdown=0.0,
        coverage=100.0,
        area=4500.0,
    )
    cp.ABS_gn_ships_2018(cfg)

    r = cfg["cathodic_protection"]
    bf = r["coating_breakdown_factors"]

    # All factors must be 1.0 with zero breakdown
    assert math.isclose(bf["initial_factor"], 1.0, rel_tol=1e-9), (
        f"initial_factor must be 1.0, got {bf['initial_factor']}"
    )
    assert math.isclose(bf["final_factor"], 1.0, rel_tol=1e-9), (
        f"final_factor must be 1.0, got {bf['final_factor']}"
    )
    assert math.isclose(bf["mean_factor"], 1.0, rel_tol=1e-9), (
        f"mean_factor must be 1.0, got {bf['mean_factor']}"
    )

    # Mean current demand: I_cm = i_coated * A * fcm / 1000
    # = 13.5 * 4500 * 1.0 / 1000 = 60.75 A
    expected_mean = 13.5 * 4500.0 * 1.0 / 1000.0
    actual_mean = r["current_demand_A"]["totals"]["mean"]
    assert math.isclose(actual_mean, expected_mean, rel_tol=1e-5), (
        f"Mean current expected {expected_mean:.4f} A, got {actual_mean:.4f} A"
    )


# ---------------------------------------------------------------------------
# Test 8 — High resistivity water (estuarine/brackish, rho = 1.0 Ohm.m)
# ---------------------------------------------------------------------------

def test_abs_high_resistivity_water():
    """
    Brackish/estuarine water with ρ = 1.0 Ω·m (vs 0.25 Ω·m for open ocean).

    Higher resistivity → higher anode resistance → lower current output per anode.
    Anode resistance for long-flush: R = ρ / (2 * s_mean)
    At ρ=1.0, L=1.00m, W=0.125m: s_mean = 0.5625, R = 1.0 / (2*0.5625) = 0.8889 Ω
    At ρ=0.25: R = 0.25 / (2*0.5625) = 0.2222 Ω
    """
    cp = CathodicProtection()
    cfg_hi = _base_cfg(resistivity=1.0)
    cfg_lo = _base_cfg(resistivity=0.25)

    cp.ABS_gn_ships_2018(cfg_hi)
    cp.ABS_gn_ships_2018(cfg_lo)

    perf_hi = cfg_hi["cathodic_protection"]["anode_performance"]
    perf_lo = cfg_lo["cathodic_protection"]["anode_performance"]

    r_hi = perf_hi["resistance_ohm"]["initial"]
    r_lo = perf_lo["resistance_ohm"]["initial"]

    # High resistivity → higher resistance
    assert r_hi is not None and r_lo is not None
    assert r_hi > r_lo, (
        f"Resistance at rho=1.0 ({r_hi:.4f} Ω) must exceed rho=0.25 ({r_lo:.4f} Ω)"
    )

    # Verify the high-resistivity value numerically
    s_mean = 0.5 * (1.00 + 0.125)
    expected_r_hi = 1.0 / (2.0 * s_mean)
    assert math.isclose(r_hi, expected_r_hi, rel_tol=1e-4), (
        f"Expected R = {expected_r_hi:.4f} Ω at rho=1.0, got {r_hi:.4f} Ω"
    )


# ---------------------------------------------------------------------------
# Test 9 — Short flush anode geometry
# Resistance formula: R = 0.315 * rho / exposed_area
# ---------------------------------------------------------------------------

def test_abs_short_flush_anode_geometry():
    """
    Short flush-plate anode: R = 0.315 * rho / A_exposed.

    For ρ = 0.25 Ω·m and exposed_area = 0.10 m²:
    R = 0.315 * 0.25 / 0.10 = 0.7875 Ω
    """
    cp = CathodicProtection()
    cfg = {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {"design_life": 5, "seawater_max_temperature": 20},
            "environment": {"seawater": {"resistivity": {"input": 0.25}}},
            "structure": {
                "steel_total_area": 4500.0,
                "area_coverage": 100.0,
                "coating_initial_breakdown_factor": 1.0,
                "coating_initial_breakdown_duration": 2.0,
                "coating_yearly_breakdown_factor": 1.0,
                "coating_breakdown_factor_max": 2.0,
            },
            "design_current": {
                "coated_steel_mA_m2": 13.5,
                "uncoated_steel_mA_m2": 200.0,
            },
            "anode": {
                "material": "aluminium",
                "protection_potential": 0.8,
                "closed_circuit_anode_potential": -1.09,
                "anode_Utilisation_factor": 0.825,
                "physical_properties": {"net_weight": 10.0},
                "geometry": {
                    "type": "short_flush",
                    "exposed_area_m2": 0.10,
                },
            },
        }
    }
    cp.ABS_gn_ships_2018(cfg)

    perf = cfg["cathodic_protection"]["anode_performance"]
    r_initial = perf["resistance_ohm"]["initial"]

    assert r_initial is not None
    expected_r = 0.315 * 0.25 / 0.10   # = 0.7875
    assert math.isclose(r_initial, expected_r, rel_tol=1e-4), (
        f"short_flush: expected R = {expected_r:.4f} Ω, got {r_initial:.4f} Ω"
    )


# ---------------------------------------------------------------------------
# Test 10 — Slender stand-off anode geometry
# Resistance formula (long slender): R = (rho / 2*pi*L) * (ln(4L/r) - 1)
# ---------------------------------------------------------------------------

def test_abs_slender_offset_anode_geometry():
    """
    Slender stand-off anode: R = (ρ / 2πL) × (ln(4L/r) − 1)   when L >= 4r.

    For ρ = 0.25 Ω·m, L = 0.5 m, r = 0.05 m:
    L/r = 10 >= 4 → long-slender formula applies.
    R = (0.25 / (2*pi*0.5)) * (ln(4*0.5/0.05) - 1)
      = (0.25 / 3.14159) * (ln(40) - 1)
      = 0.07958 * (3.6889 - 1)
      = 0.07958 * 2.6889
      = 0.2140 Ω
    """
    cp = CathodicProtection()
    cfg = {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {"design_life": 5, "seawater_max_temperature": 20},
            "environment": {"seawater": {"resistivity": {"input": 0.25}}},
            "structure": {
                "steel_total_area": 4500.0,
                "area_coverage": 100.0,
                "coating_initial_breakdown_factor": 1.0,
                "coating_initial_breakdown_duration": 2.0,
                "coating_yearly_breakdown_factor": 1.0,
                "coating_breakdown_factor_max": 2.0,
            },
            "design_current": {
                "coated_steel_mA_m2": 13.5,
                "uncoated_steel_mA_m2": 200.0,
            },
            "anode": {
                "material": "aluminium",
                "protection_potential": 0.8,
                "closed_circuit_anode_potential": -1.09,
                "anode_Utilisation_factor": 0.825,
                "physical_properties": {"net_weight": 15.0},
                "geometry": {
                    "type": "slender_offset",
                    "length_m": 0.5,
                    "radius_m": 0.05,
                },
            },
        }
    }
    cp.ABS_gn_ships_2018(cfg)

    perf = cfg["cathodic_protection"]["anode_performance"]
    r_initial = perf["resistance_ohm"]["initial"]

    assert r_initial is not None

    # Manual computation: L=0.5, r=0.05, rho=0.25
    import math as _math
    L, r, rho = 0.5, 0.05, 0.25
    assert L >= 4 * r   # Confirm long-slender branch
    expected_r = (rho / (2 * _math.pi * L)) * (_math.log(4 * L / r) - 1)

    assert math.isclose(r_initial, expected_r, rel_tol=1e-4), (
        f"slender_offset: expected R = {expected_r:.4f} Ω, got {r_initial:.4f} Ω"
    )
