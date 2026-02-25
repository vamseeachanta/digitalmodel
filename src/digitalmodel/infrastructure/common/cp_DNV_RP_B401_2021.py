"""
DNV-RP-B401-2021 cathodic protection calculations for offshore fixed platforms.

Covers jacket structures, gravity-based structures, and topsides steel.
Anode types: flush-mounted, stand-off, bracelet.
Zones: submerged, splash, atmospheric.

Standard: DNV-RP-B401 (May 2021 edition)
Sections: 3.3 (current densities), 3.4 (coating breakdown), 4.9 (anode resistance)
"""

import math

# B401-2021 Table 3-1: Mean design current densities (A/m2) by zone and temperature
# Submerged zone: varies by seawater temperature (C)
# Splash/atmospheric zones: temperature-independent
_B401_2021_CURRENT_DENSITIES = {
    "submerged": {
        "<=7":    {"coated": 0.040, "bare": 0.060},
        ">7-12":  {"coated": 0.050, "bare": 0.080},
        ">12-17": {"coated": 0.060, "bare": 0.100},
        ">17":    {"coated": 0.070, "bare": 0.120},
    },
    "splash":      {"coated": 0.100, "bare": 0.200},
    "atmospheric": {"coated": 0.010, "bare": 0.030},
}

# B401-2021 Section 3.4.6: Coating breakdown factors
# f_c(t) = f_ci + k * t; clamp at 1.0 (bare steel)
_B401_2021_COATING_CATEGORIES = {
    "I":    {"f_ci": 0.05, "k": 0.020},  # High quality ≥300 μm epoxy
    "II":   {"f_ci": 0.10, "k": 0.030},  # Anti-friction thin film / PTFE (new in 2021)
    "III":  {"f_ci": 0.25, "k": 0.050},  # Standard quality paint system
    "bare": {"f_ci": 1.00, "k": 0.000},  # No coating — bare steel
}

# Electrochemical capacity (Ah/kg) for sacrificial anode materials — B401-2021 Sec.3.6
_B401_2021_ANODE_CAPACITY = {
    "aluminium": 2000.0,
    "zinc":       780.0,
}

# Steel protective potential and anode potential (V vs Ag/AgCl) — B401-2021 Sec.2.4
_B401_2021_POTENTIALS = {
    "structure_protection_V": -0.80,   # Carbon steel in seawater
    "aluminium_anode_V":      -1.05,   # Al-Zn-In alloy
    "zinc_anode_V":           -1.00,   # Zn anode
}

# Module-level constants derived from data tables
_B401_VALID_ZONES = frozenset(_B401_2021_CURRENT_DENSITIES)
_B401_VALID_ANODE_TYPES = frozenset({"flush_mounted", "stand_off", "bracelet"})

# Shared error text for duplicate zone ID violations
_DUPLICATE_ZONE_MSG = (
    "Each zone entry must have a unique 'zone' ID. "
    "Use distinct IDs with 'base_zone' for multi-segment zones."
)


# ---------------------------------------------------------------------------
# Shared validation helpers
# ---------------------------------------------------------------------------

def _require_zone_field(z, field):
    """Return z[field], raising ValueError with diagnostics if the field is missing."""
    val = z.get(field)
    if val is None:
        raise ValueError(f"Zone entry missing required '{field}' field: {z!r}")
    return val


def _require_finite(val, name):
    """Raise ValueError if val is NaN or infinite after float conversion."""
    if not math.isfinite(val):
        raise ValueError(f"{name} must be a finite number, got {val}")
    return val


def _require_valid_material(material):
    """Raise ValueError for unknown anode material."""
    if material not in _B401_2021_ANODE_CAPACITY:
        raise ValueError(
            f"Unknown anode material '{material}'. "
            f"Valid materials: {list(_B401_2021_ANODE_CAPACITY)}"
        )


# ---------------------------------------------------------------------------
# B401-2021 calculation helpers
# ---------------------------------------------------------------------------

def _b401_surface_areas(inputs):
    """Sum zone areas from inputs.structure.zones.

    Each zone entry must have a unique "zone" ID. To model multiple coating
    categories within the same physical zone, use distinct zone IDs and set
    "base_zone" to the physical zone name (e.g. "submerged").

    Raises:
        ValueError: If zones list is empty, duplicate zone IDs exist, required fields
                    are missing, or area_m2 is negative or non-finite.

    Returns dict: {zone_id: area_m2, ..., "total_m2": float}
    """
    structure = inputs.get("structure", {})
    zones = structure.get("zones", [])
    if not zones:
        raise ValueError("zones list is empty; at least one zone is required")
    areas = {}
    total = 0.0
    for z in zones:
        zone_name = _require_zone_field(z, "zone")
        if zone_name in areas:
            raise ValueError(f"Duplicate zone ID '{zone_name}'. {_DUPLICATE_ZONE_MSG}")
        area = _require_finite(float(_require_zone_field(z, "area_m2")), f"area_m2[{zone_name}]")
        if area < 0:
            raise ValueError(f"area_m2 for zone '{zone_name}' must be >= 0, got {area}")
        areas[zone_name] = area
        total += area
    if total == 0.0:
        raise ValueError("Total surface area is zero; all zones have area_m2 = 0")
    areas["total_m2"] = total
    return areas


def _b401_coating_breakdown(inputs, design_life):
    """Calculate coating breakdown factors per zone.

    B401-2021 Sec.3.4.6: f_c(t) = f_ci + k * t
    f_cm = f_ci + k * T/2  (mean over design life T)
    f_cf = f_ci + k * T    (final at end of design life)

    Each zone name must appear exactly once. Use distinct zone names (e.g.
    "submerged_cat1") when different coating categories apply to the same zone.

    Raises:
        ValueError: If coating_category is unknown, T is invalid, or zone IDs are
                    duplicated or missing.

    Returns dict: {zone_id: {"f_ci": ..., "f_cm": ..., "f_cf": ...}, ...}
    """
    structure = inputs.get("structure", {})
    zones = structure.get("zones", [])
    if not zones:
        raise ValueError("zones list is empty; at least one zone is required")
    T = _require_finite(float(design_life), "design_life")
    if T <= 0:
        raise ValueError(f"Design life must be > 0, got {T}")
    result = {}
    for z in zones:
        zone_name = _require_zone_field(z, "zone")
        if zone_name in result:
            raise ValueError(f"Duplicate zone ID '{zone_name}'. {_DUPLICATE_ZONE_MSG}")
        cat = z.get("coating_category", "III")
        if cat not in _B401_2021_COATING_CATEGORIES:
            raise ValueError(
                f"Unknown coating category '{cat}'. "
                f"Valid categories: {list(_B401_2021_COATING_CATEGORIES)}"
            )
        params = _B401_2021_COATING_CATEGORIES[cat]
        f_ci = params["f_ci"]
        k = params["k"]
        f_cm = min(f_ci + k * T / 2.0, 1.0)
        f_cf = min(f_ci + k * T, 1.0)
        result[zone_name] = {
            "coating_category": cat,
            "f_ci": f_ci,   # Full precision — rounded values bias downstream demand calcs
            "f_cm": f_cm,
            "f_cf": f_cf,
        }
    return result


def _b401_current_densities(inputs):
    """Look up design current densities per zone from B401-2021 Table 3-1.

    Submerged zone: temperature-dependent. Splash/atmospheric: temperature-independent.
    Selects coated vs bare based on coating_category.

    Supports segmented zone IDs: set "base_zone" to the physical zone type
    ("submerged", "splash", "atmospheric") when using a distinct zone ID such as
    "submerged_cat1". If "base_zone" is omitted, "zone" is used as the lookup key.

    Raises:
        ValueError: If base_zone/zone is not a valid physical zone type, or
                    duplicate zone IDs are found, or temperature is non-finite.

    Returns dict: {zone_id: {"i_mean_A_m2": float, "coated_or_bare": str, ...}, ...}
    """
    structure = inputs.get("structure", {})
    environment = inputs.get("environment", {})
    zones = structure.get("zones", [])
    if not zones:
        raise ValueError("zones list is empty; at least one zone is required")
    temp_c = _require_finite(
        float(environment.get("seawater_temperature_C", 10.0)), "seawater_temperature_C"
    )

    if temp_c <= 7.0:
        temp_band = "<=7"
    elif temp_c <= 12.0:
        temp_band = ">7-12"
    elif temp_c <= 17.0:
        temp_band = ">12-17"
    else:
        temp_band = ">17"

    result = {}
    for z in zones:
        zone_id = _require_zone_field(z, "zone")
        base_zone = z.get("base_zone", zone_id)
        if base_zone not in _B401_VALID_ZONES:
            raise ValueError(
                f"Unknown zone type '{base_zone}' (from zone '{zone_id}'). "
                f"Valid physical zones: {sorted(_B401_VALID_ZONES)}"
            )
        if zone_id in result:
            raise ValueError(f"Duplicate zone ID '{zone_id}'. {_DUPLICATE_ZONE_MSG}")
        cat = z.get("coating_category", "III")
        if cat not in _B401_2021_COATING_CATEGORIES:
            raise ValueError(
                f"Unknown coating category '{cat}' in zone '{zone_id}'. "
                f"Valid categories: {list(_B401_2021_COATING_CATEGORIES)}"
            )
        coated_or_bare = "bare" if cat == "bare" else "coated"

        if base_zone == "submerged":
            zone_densities = _B401_2021_CURRENT_DENSITIES["submerged"][temp_band]
        else:
            zone_densities = _B401_2021_CURRENT_DENSITIES[base_zone]
        i_mean = zone_densities[coated_or_bare]

        result[zone_id] = {
            "i_mean_A_m2": i_mean,
            "coated_or_bare": coated_or_bare,
            "base_zone": base_zone,
            "temperature_band": temp_band if base_zone == "submerged" else "N/A",
        }
    return result


def _b401_current_demand(inputs, areas, densities, breakdown):
    """Calculate cathodic protection current demand per zone and total.

    B401-2021: I = A * i_mean * f_cm  (mean current demand per zone)

    Totals are kept at full precision for downstream sizing/adequacy use.
    Per-zone display values are rounded to 3 decimal places.

    Returns dict with per-zone results and totals (total_mean_A, total_final_A).
    """
    structure = inputs.get("structure", {})
    zones = structure.get("zones", [])
    result = {}
    total_mean = 0.0
    total_final = 0.0
    for z in zones:
        zone_name = _require_zone_field(z, "zone")
        if zone_name not in areas:
            raise ValueError(
                f"Zone '{zone_name}' not found in surface areas; "
                "ensure all three pipeline dicts (areas, densities, breakdown) "
                "are computed from the same inputs."
            )
        if zone_name not in densities:
            raise ValueError(
                f"Zone '{zone_name}' not found in current densities; "
                "ensure all three pipeline dicts are computed from the same inputs."
            )
        if zone_name not in breakdown:
            raise ValueError(
                f"Zone '{zone_name}' not found in coating breakdown; "
                "ensure all three pipeline dicts are computed from the same inputs."
            )
        area = areas[zone_name]
        i_mean = densities[zone_name]["i_mean_A_m2"]
        f_cm = breakdown[zone_name]["f_cm"]
        f_cf = breakdown[zone_name]["f_cf"]
        I_mean = area * i_mean * f_cm
        I_final = area * i_mean * f_cf
        result[zone_name] = {
            "area_m2": round(area, 3),
            "i_mean_A_m2": i_mean,
            "f_cm": f_cm,
            "f_cf": f_cf,
            "I_mean_A": round(I_mean, 3),
            "I_final_A": round(I_final, 3),
        }
        total_mean += I_mean
        total_final += I_final
    # Full precision totals — downstream sizing/adequacy must not be biased by rounding
    result["total_mean_A"] = total_mean
    result["total_final_A"] = total_final
    return result


def _b401_anode_resistance(inputs):
    """Calculate anode resistance per B401-2021 Sec.4.9.

    Anode type dispatch:
    - flush_mounted / stand_off: Dwight formula R = (rho/2piL) * (ln(4L/r) - 1)
    - bracelet: Modified formula R = (rho/2piL) * (ln(2piL/r) - 1)

    Raises:
        ValueError: If anode type unknown, rho/L/r invalid, geometry too stubby, or
                    any numeric input is NaN/infinite.

    Returns: resistance in ohms (float, full precision)
    """
    anode = inputs.get("anode", {})
    environment = inputs.get("environment", {})
    rho = _require_finite(
        float(environment.get("seawater_resistivity_ohm_m", 0.30)), "seawater_resistivity_ohm_m"
    )
    if rho <= 0:
        raise ValueError(f"Seawater resistivity must be > 0, got {rho}")
    anode_type = anode.get("type", "stand_off")
    L = _require_finite(float(anode.get("length_m", 1.0)), "anode length_m")
    r = _require_finite(float(anode.get("radius_m", 0.05)), "anode radius_m")

    if anode_type not in _B401_VALID_ANODE_TYPES:
        raise ValueError(
            f"Unknown anode type '{anode_type}'. "
            f"Valid types: {sorted(_B401_VALID_ANODE_TYPES)}"
        )
    if L <= 0:
        raise ValueError(f"Anode length must be > 0, got {L}")
    if r <= 0:
        raise ValueError(f"Anode radius must be > 0, got {r}")

    if anode_type in ("flush_mounted", "stand_off"):
        # B401-2021 Sec.4.9 Dwight formula: ln(4L/r) - 1
        if 4.0 * L / r <= math.e:
            raise ValueError(
                f"Anode geometry too stubby for Dwight formula: "
                f"4L/r = {4*L/r:.2f} must be > {math.e:.2f} (require L/r >= 0.68)"
            )
        R = (rho / (2.0 * math.pi * L)) * (math.log(4.0 * L / r) - 1.0)
    else:  # bracelet — modified Dwight (B401-2021 Annex B): ln(2πL/r) - 1
        if 2.0 * math.pi * L / r <= math.e:
            raise ValueError(
                f"Bracelet anode geometry invalid: "
                f"2\u03c0L/r = {2*math.pi*L/r:.2f} must be > {math.e:.2f} (require L/r >= 0.43)"
            )
        R = (rho / (2.0 * math.pi * L)) * (math.log(2.0 * math.pi * L / r) - 1.0)

    return R  # Full precision; caller rounds for output only


def _b401_anode_requirements(inputs, current_demand):
    """Calculate anode mass and count requirements.

    B401-2021 Sec.4.3: M = (I_mean * T * 8760) / (epsilon * u)
    N = ceil(M / m_a)

    Raises:
        ValueError: If material unknown, T/u/m_a out of range, or any numeric input
                    is NaN/infinite.

    Returns dict with total_mass_kg, anode_count, etc.
    """
    design_data = inputs.get("design_data", {})
    anode = inputs.get("anode", {})
    T = _require_finite(float(design_data.get("design_life", 25.0)), "design_life")
    material = anode.get("material", "aluminium")
    u = _require_finite(float(anode.get("utilization_factor", 0.85)), "utilization_factor")
    m_a = _require_finite(
        float(anode.get("individual_anode_mass_kg", 200.0)), "individual_anode_mass_kg"
    )

    _require_valid_material(material)
    epsilon = _B401_2021_ANODE_CAPACITY[material]

    if T <= 0:
        raise ValueError(f"Design life must be > 0, got {T}")
    if u <= 0 or u > 1.0:
        raise ValueError(f"Utilization factor must be in (0, 1.0], got {u}")
    if m_a <= 0:
        raise ValueError(f"Individual anode mass must be > 0, got {m_a}")

    I_mean = float(current_demand["total_mean_A"])
    total_Ah = I_mean * T * 8760.0
    total_mass_kg = total_Ah / (epsilon * u)
    anode_count = math.ceil(total_mass_kg / m_a)

    return {
        "total_mass_kg": round(total_mass_kg, 2),
        "anode_count": anode_count,
        "individual_mass_kg": round(m_a, 2),
        "anode_material": material,
        "electrochemical_capacity_Ah_kg": epsilon,
        "utilization_factor": u,
        "design_life_hours": round(T * 8760.0, 1),
    }


def _b401_verify_current_output(inputs, anode_req, resistance, current_demand):
    """Verify total anode current output meets governing design current demand.

    Governing criterion (B401-2021): final current demand at end of design life.
      driving_voltage = E_structure - E_anode  (positive: anode more negative)
      I_output_per_anode = driving_voltage / R_a
      I_total = N * I_output_per_anode
      Adequate if I_total >= total_final_A

    Args:
        inputs: Configuration dict
        anode_req: Result from _b401_anode_requirements
        resistance: Anode resistance (ohm) from _b401_anode_resistance (must be > 0)
        current_demand: Result from _b401_current_demand (must contain total_final_A > 0)

    Returns dict with adequate bool and comparison values.
    """
    anode = inputs.get("anode", {})
    material = anode.get("material", "aluminium")
    _require_valid_material(material)

    if resistance <= 0:
        raise ValueError(f"Anode resistance must be > 0, got {resistance}")
    N = anode_req.get("anode_count")
    if not isinstance(N, int) or N < 0:
        raise ValueError(f"anode_count must be a non-negative integer, got {N!r}")
    I_final_demand = float(current_demand["total_final_A"])
    if I_final_demand <= 0:
        raise ValueError(
            f"Final current demand must be > 0, got {I_final_demand}. "
            "Check that zones have non-zero areas and valid coating breakdown."
        )
    E_anode = _B401_2021_POTENTIALS[f"{material}_anode_V"]
    E_structure = _B401_2021_POTENTIALS["structure_protection_V"]
    driving_voltage = E_structure - E_anode
    if driving_voltage <= 0:
        raise ValueError(
            f"Non-positive driving voltage {driving_voltage:.4f} V "
            f"(E_anode={E_anode}, E_structure={E_structure}). "
            "Anode potential must be more negative than structure protection potential."
        )
    I_per_anode = driving_voltage / resistance
    I_total_output = N * I_per_anode
    I_mean_demand = float(current_demand["total_mean_A"])
    adequate = I_total_output >= I_final_demand

    # Minimum count satisfying current-output criterion (B401 governing: final demand)
    count_by_current = math.ceil(I_final_demand / I_per_anode)
    # Recommended count satisfies BOTH mass formula AND current-output criteria
    recommended_count = max(N, count_by_current)

    return {
        "adequate": adequate,
        "driving_voltage_V": round(driving_voltage, 4),
        "anode_current_output_per_anode_A": round(I_per_anode, 4),
        "total_anode_current_output_A": round(I_total_output, 3),
        "final_current_demand_A": round(I_final_demand, 3),
        "mean_current_demand_A": round(I_mean_demand, 3),
        "anode_count": N,
        "recommended_anode_count": recommended_count,  # max(mass, current) — always compliant
        "anode_resistance_ohm": round(resistance, 6),
    }
