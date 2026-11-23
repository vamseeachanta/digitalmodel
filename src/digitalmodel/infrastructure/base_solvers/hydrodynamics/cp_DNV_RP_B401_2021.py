"""
DNV-RP-B401 cathodic protection calculations for offshore fixed platforms.

Covers jacket structures, gravity-based structures, subsea structures and
topsides steel. Anode types: flush-mounted, stand-off, bracelet.
Zones: submerged, buried, splash, atmospheric.

Every design table value comes from the cited, edition-keyed lookups in
``digitalmodel.cathodic_protection.b401_tables`` (issue #2207):

- Table 10-1 (initial / final) and Table 10-2 (mean) design current
  densities for seawater-exposed BARE metal, by climate and depth band.
- Sec. 6.3 buried-surface design current density (0.020 A/m2, all phases).
- Table 10-4 paint coating breakdown constants ``a`` and ``b`` by category
  (I, II, III) and depth row.
- Table 10-6 anode capacity and closed circuit potential.
- Table 10-8 anode utilisation factor by anode shape.
- Sec. 5 protective potential (-0.80 V vs Ag/AgCl/seawater).
- Sec. 7 design loop: current demand -> anode mass -> anode resistance
  (Table 10-7, Dwight long slender stand-off formula) -> current output
  check for the initial and final phases.

Splash and atmospheric zones carry 0.0 A/m2 for every phase: B401's CP
design current densities apply to seawater-exposed and buried surfaces only
(CP does not act in air; splash-zone steel is protected by coating and
corrosion allowance). The zones are still accepted so that surface areas and
coating breakdown are reported for them.

Coating is applied only through the breakdown factor ``f_c`` (Sec. 6.4):
the bare-metal current density is multiplied by ``f_ci`` / ``f_cm`` /
``f_cf``. The legacy coated/bare current-density split has been removed; the
``coated_or_bare`` key in the density result is informational only.
"""

from __future__ import annotations

from collections.abc import Iterable, Mapping
import math
from typing import Any, Final

from digitalmodel.cathodic_protection import b401_tables
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    normalize_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    Climate,
    DepthBand,
    DesignPhase,
    PaintCategory,
)
from digitalmodel.citations import CitedValue

# Physical base zones accepted in inputs.structure.zones[].base_zone (or
# zone when base_zone is omitted). Seawater-exposed and buried surfaces take
# their design current densities from B401; splash and atmospheric surfaces
# are outside the scope of B401 CP current densities and carry 0.0 A/m2.
_B401_SEAWATER_ZONE: Final = "submerged"
_B401_BURIED_ZONE: Final = "buried"
_B401_ZERO_CURRENT_ZONES: Final = frozenset({"splash", "atmospheric"})
_B401_VALID_ZONES: Final = frozenset(
    {_B401_SEAWATER_ZONE, _B401_BURIED_ZONE, *_B401_ZERO_CURRENT_ZONES}
)
_B401_ZERO_CURRENT_NOTE: Final = (
    "B401 CP design current densities apply to seawater-exposed and buried "
    "surfaces only; splash and atmospheric zones carry 0.0 A/m2"
)

# Accepted coating_category keys -> Table 10-4 paint category. "bare" is not a
# Table 10-4 category: it means no coating, f_c = 1.0 for every phase.
_B401_2021_COATING_CATEGORIES: Final[dict[str, PaintCategory | None]] = {
    "I": PaintCategory.I,  # Table 10-4 column I (a = 0.10)
    "II": PaintCategory.II,  # Table 10-4 column II (a = 0.05)
    "III": PaintCategory.III,  # Table 10-4 column III (a = 0.02)
    "bare": None,  # no coating: f_ci = f_cm = f_cf = 1.0
}
_B401_BARE_COATING: Final = "bare"
_B401_DEFAULT_COATING: Final = "III"

# Anode material keys accepted in inputs.anode.material -> Table 10-6 row.
_B401_ANODE_MATERIALS: Final[dict[str, AnodeMaterial]] = {
    "aluminium": AnodeMaterial.ALUMINIUM,
    "zinc": AnodeMaterial.ZINC,
}

# Anode type keys accepted in inputs.anode.type -> Table 10-8 shape class.
# stand_off is treated as the long slender stand-off class (L >= 4r), which
# is also the geometry the Table 10-7 Dwight formula in
# _b401_anode_resistance assumes.
_B401_ANODE_SHAPES: Final[dict[str, AnodeShape]] = {
    "stand_off": AnodeShape.LONG_SLENDER_STANDOFF,  # Table 10-8: 0.90
    "flush_mounted": AnodeShape.LONG_FLUSH,  # Table 10-8: 0.85
    "bracelet": AnodeShape.SHORT_FLUSH_BRACELET,  # Table 10-8: 0.80
}
_B401_VALID_ANODE_TYPES: Final = frozenset(_B401_ANODE_SHAPES)
_B401_DEFAULT_ANODE_TYPE: Final = "stand_off"
_B401_DEFAULT_MATERIAL: Final = "aluminium"

# Table 10-1 / 10-2 column headers (surface water temperature, deg C) for the
# informational "temperature_band" key of the density result.
_B401_CLIMATE_TEMPERATURE_BAND: Final[dict[Climate, str]] = {
    Climate.TROPICAL: ">20",
    Climate.SUBTROPICAL: "12-20",
    Climate.TEMPERATE: "7-12",
    Climate.ARCTIC: "<7",
}

_HOURS_PER_YEAR: Final = 8760.0

# Shared error text for duplicate zone ID violations
_DUPLICATE_ZONE_MSG = (
    "Each zone entry must have a unique 'zone' ID. "
    "Use distinct IDs with 'base_zone' for multi-segment zones."
)


# ---------------------------------------------------------------------------
# Shared validation helpers
# ---------------------------------------------------------------------------

def _require_zone_field(z: Mapping[str, Any], field: str) -> Any:
    """Return z[field], raising ValueError with diagnostics if the field is missing."""
    val = z.get(field)
    if val is None:
        raise ValueError(f"Zone entry missing required '{field}' field: {z!r}")
    return val


def _require_finite(val: float, name: str) -> float:
    """Raise ValueError if val is NaN or infinite after float conversion."""
    if not math.isfinite(val):
        raise ValueError(f"{name} must be a finite number, got {val}")
    return val


def _require_valid_material(material: str) -> AnodeMaterial:
    """Return the Table 10-6 material for an input key; raise if unknown."""
    try:
        return _B401_ANODE_MATERIALS[material]
    except KeyError:
        raise ValueError(
            f"Unknown anode material '{material}'. "
            f"Valid materials: {list(_B401_ANODE_MATERIALS)}"
        ) from None


def _require_valid_anode_type(anode_type: str) -> AnodeShape:
    """Return the Table 10-8 shape class for an input key; raise if unknown."""
    try:
        return _B401_ANODE_SHAPES[anode_type]
    except KeyError:
        raise ValueError(
            f"Unknown anode type '{anode_type}'. "
            f"Valid types: {sorted(_B401_VALID_ANODE_TYPES)}"
        ) from None


def _require_valid_coating(cat: str, zone_id: str | None = None) -> PaintCategory | None:
    """Return the Table 10-4 category (None for bare); raise if unknown."""
    if cat not in _B401_2021_COATING_CATEGORIES:
        where = f" in zone '{zone_id}'" if zone_id is not None else ""
        raise ValueError(
            f"Unknown coating category '{cat}'{where}. "
            f"Valid categories: {list(_B401_2021_COATING_CATEGORIES)}"
        )
    return _B401_2021_COATING_CATEGORIES[cat]


def _b401_edition(inputs: Mapping[str, Any]) -> Edition:
    """Resolve the B401 edition from inputs.design_data.edition.

    The default is passed explicitly so ``normalize_edition`` does not emit
    its missing-edition warning for the legacy YAML schema, which has no
    edition key.
    """
    design_data = inputs.get("design_data", {})
    raw = design_data.get("edition", DEFAULT_EDITION)
    return normalize_edition(str(raw), stacklevel=3)


def _b401_zone_depth_band(z: Mapping[str, Any], zone_id: str) -> DepthBand:
    """Depth band for a zone from its optional ``depth_m`` key (default 0 m)."""
    depth_m = _require_finite(float(z.get("depth_m", 0.0)), f"depth_m[{zone_id}]")
    if depth_m < 0.0:
        raise ValueError(f"depth_m for zone '{zone_id}' must be >= 0, got {depth_m}")
    return b401_tables.depth_band(depth_m)


def _citation_keys(cited_values: Iterable[CitedValue]) -> list[str]:
    """Sorted, de-duplicated ``code_id revision section`` strings."""
    return sorted({b401_tables.citation_label(cv.citation) for cv in cited_values})


# ---------------------------------------------------------------------------
# B401 calculation helpers (Sec. 7 design loop)
# ---------------------------------------------------------------------------

def _b401_surface_areas(inputs: Mapping[str, Any]) -> dict[str, float]:
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
    areas: dict[str, float] = {}
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


def _b401_coating_breakdown(
    inputs: Mapping[str, Any], design_life: float
) -> dict[str, dict[str, Any]]:
    """Calculate coating breakdown factors per zone (B401 Sec. 6.4, Table 10-4).

    f_c(t) = a + b * t, clamped at 1.0 (bare steel):
    f_ci = a              (initial)
    f_cm = a + b * T/2    (mean over design life T)
    f_cf = a + b * T      (final at end of design life)

    ``a`` and ``b`` come from Table 10-4 for paint categories I, II, III;
    ``b`` depends on the zone's depth row ("0-30" or ">30" m, from the
    optional zone key ``depth_m``, default 0 m). ``"bare"`` gives
    f_ci = f_cm = f_cf = 1.0.

    Each zone name must appear exactly once. Use distinct zone names (e.g.
    "submerged_cat1") when different coating categories apply to the same zone.

    Raises:
        ValueError: If coating_category is unknown, T is invalid, or zone IDs are
                    duplicated or missing.

    Returns dict: {zone_id: {"coating_category", "a", "b_per_yr", "depth_band",
                             "f_ci", "f_cm", "f_cf", "citations"}, ...}
    """
    structure = inputs.get("structure", {})
    zones = structure.get("zones", [])
    if not zones:
        raise ValueError("zones list is empty; at least one zone is required")
    T = _require_finite(float(design_life), "design_life")
    if T <= 0:
        raise ValueError(f"Design life must be > 0, got {T}")
    edition = _b401_edition(inputs)
    result: dict[str, dict[str, Any]] = {}
    for z in zones:
        zone_name = _require_zone_field(z, "zone")
        if zone_name in result:
            raise ValueError(f"Duplicate zone ID '{zone_name}'. {_DUPLICATE_ZONE_MSG}")
        cat = z.get("coating_category", _B401_DEFAULT_COATING)
        category = _require_valid_coating(cat)
        band = _b401_zone_depth_band(z, zone_name)
        cited: list[CitedValue] = []
        if category is None:
            a, b = 1.0, 0.0
        else:
            a_cv, b_cv = b401_tables.coating_breakdown_constants(category, band, edition)
            a, b = a_cv.value, b_cv.value
            cited = [a_cv, b_cv]
        f_ci = min(a, 1.0)
        f_cm = min(a + b * T / 2.0, 1.0)
        f_cf = min(a + b * T, 1.0)
        result[zone_name] = {
            "coating_category": cat,
            "a": a,
            "b_per_yr": b,
            "depth_band": band.value,
            "f_ci": f_ci,   # Full precision — rounded values bias downstream demand calcs
            "f_cm": f_cm,
            "f_cf": f_cf,
            "citations": _citation_keys(cited),
        }
    return result


def _b401_current_densities(inputs: Mapping[str, Any]) -> dict[str, dict[str, Any]]:
    """Look up design current densities per zone (B401 Tables 10-1, 10-2, Sec. 6.3).

    Densities are for BARE metal; coating is applied downstream through the
    breakdown factors. Per base zone:

    - "submerged": initial/final from Table 10-1, mean from Table 10-2, by
      climate (``environment.seawater_temperature_C``, default 10 C) and depth
      band (zone key ``depth_m``, default 0 m -> "0-30").
    - "buried": Sec. 6.3, 0.020 A/m2 for all phases.
    - "splash" / "atmospheric": 0.0 A/m2 for all phases. B401's CP design
      current densities apply to seawater-exposed and buried surfaces only.

    Supports segmented zone IDs: set "base_zone" to the physical zone type
    when using a distinct zone ID such as "submerged_cat1". If "base_zone" is
    omitted, "zone" is used as the lookup key.

    Raises:
        ValueError: If base_zone/zone is not a valid physical zone type, the
                    coating category is unknown, duplicate zone IDs are found,
                    or temperature / depth is non-finite.

    Returns dict: {zone_id: {"i_initial_A_m2", "i_mean_A_m2", "i_final_A_m2",
                             "coated_or_bare" (informational), "base_zone",
                             "climate", "depth_band", "temperature_band",
                             "citations"}, ...}
    """
    structure = inputs.get("structure", {})
    environment = inputs.get("environment", {})
    zones = structure.get("zones", [])
    if not zones:
        raise ValueError("zones list is empty; at least one zone is required")
    temp_c = _require_finite(
        float(environment.get("seawater_temperature_C", 10.0)), "seawater_temperature_C"
    )
    climate = b401_tables.climate_from_temperature(temp_c)
    edition = _b401_edition(inputs)

    result: dict[str, dict[str, Any]] = {}
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
        cat = z.get("coating_category", _B401_DEFAULT_COATING)
        _require_valid_coating(cat, zone_id)
        # Informational only: coating enters the demand via f_c, not via the
        # current density (B401 has no coated/bare density split).
        coated_or_bare = "bare" if cat == _B401_BARE_COATING else "coated"
        band = _b401_zone_depth_band(z, zone_id)

        cited: list[CitedValue] = []
        note = ""
        if base_zone == _B401_SEAWATER_ZONE:
            phases = {
                phase: b401_tables.design_current_density(climate, band, phase, edition)
                for phase in DesignPhase
            }
            cited = list(phases.values())
            i_initial = phases[DesignPhase.INITIAL].value
            i_mean = phases[DesignPhase.MEAN].value
            i_final = phases[DesignPhase.FINAL].value
            climate_label = climate.value
            temperature_band = _B401_CLIMATE_TEMPERATURE_BAND[climate]
        elif base_zone == _B401_BURIED_ZONE:
            buried = b401_tables.buried_current_density(edition)
            cited = [buried]
            i_initial = i_mean = i_final = buried.value
            climate_label = "N/A"
            temperature_band = "N/A"
        else:
            # splash / atmospheric: outside the scope of B401 CP current
            # densities, which cover seawater-exposed and buried surfaces only.
            i_initial = i_mean = i_final = 0.0
            note = _B401_ZERO_CURRENT_NOTE
            climate_label = "N/A"
            temperature_band = "N/A"

        result[zone_id] = {
            "i_initial_A_m2": i_initial,
            "i_mean_A_m2": i_mean,
            "i_final_A_m2": i_final,
            "coated_or_bare": coated_or_bare,
            "base_zone": base_zone,
            "climate": climate_label,
            "depth_band": band.value,
            "temperature_band": temperature_band,
            "citations": _citation_keys(cited),
        }
        if note:
            result[zone_id]["note"] = note
    return result


def _b401_current_demand(
    inputs: Mapping[str, Any],
    areas: Mapping[str, float],
    densities: Mapping[str, Mapping[str, Any]],
    breakdown: Mapping[str, Mapping[str, Any]],
) -> dict[str, Any]:
    """Calculate cathodic protection current demand per zone and total (B401 Sec. 6.4).

    I_initial = A * i_initial * f_ci
    I_mean    = A * i_mean    * f_cm
    I_final   = A * i_final   * f_cf

    Totals are kept at full precision for downstream sizing/adequacy use.
    Per-zone display values are rounded to 3 decimal places.

    Returns dict with per-zone results and totals (total_initial_A,
    total_mean_A, total_final_A).
    """
    structure = inputs.get("structure", {})
    zones = structure.get("zones", [])
    result: dict[str, Any] = {}
    total_initial = 0.0
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
        i_initial = float(densities[zone_name]["i_initial_A_m2"])
        i_mean = float(densities[zone_name]["i_mean_A_m2"])
        i_final = float(densities[zone_name]["i_final_A_m2"])
        f_ci = float(breakdown[zone_name]["f_ci"])
        f_cm = float(breakdown[zone_name]["f_cm"])
        f_cf = float(breakdown[zone_name]["f_cf"])
        I_initial = area * i_initial * f_ci
        I_mean = area * i_mean * f_cm
        I_final = area * i_final * f_cf
        result[zone_name] = {
            "area_m2": round(area, 3),
            "i_initial_A_m2": i_initial,
            "i_mean_A_m2": i_mean,
            "i_final_A_m2": i_final,
            "f_ci": f_ci,
            "f_cm": f_cm,
            "f_cf": f_cf,
            "I_initial_A": round(I_initial, 3),
            "I_mean_A": round(I_mean, 3),
            "I_final_A": round(I_final, 3),
        }
        total_initial += I_initial
        total_mean += I_mean
        total_final += I_final
    # Full precision totals — downstream sizing/adequacy must not be biased by rounding
    result["total_initial_A"] = total_initial
    result["total_mean_A"] = total_mean
    result["total_final_A"] = total_final
    return result


def _b401_anode_resistance(inputs: Mapping[str, Any]) -> float:
    """Calculate anode resistance per B401 Table 10-7.

    Anode type dispatch:
    - flush_mounted / stand_off: long slender stand-off (Dwight) formula
      R = (rho/2piL) * (ln(4L/r) - 1), valid for L >= 4r
    - bracelet: modified formula R = (rho/2piL) * (ln(2piL/r) - 1)

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
    anode_type = anode.get("type", _B401_DEFAULT_ANODE_TYPE)
    L = _require_finite(float(anode.get("length_m", 1.0)), "anode length_m")
    r = _require_finite(float(anode.get("radius_m", 0.05)), "anode radius_m")

    _require_valid_anode_type(anode_type)
    if L <= 0:
        raise ValueError(f"Anode length must be > 0, got {L}")
    if r <= 0:
        raise ValueError(f"Anode radius must be > 0, got {r}")

    if anode_type in ("flush_mounted", "stand_off"):
        # B401 Table 10-7 long slender stand-off (Dwight): ln(4L/r) - 1
        if 4.0 * L / r <= math.e:
            raise ValueError(
                f"Anode geometry too stubby for Dwight formula: "
                f"4L/r = {4*L/r:.2f} must be > {math.e:.2f} (require L/r >= 0.68)"
            )
        R = (rho / (2.0 * math.pi * L)) * (math.log(4.0 * L / r) - 1.0)
    else:  # bracelet — modified Dwight: ln(2πL/r) - 1
        if 2.0 * math.pi * L / r <= math.e:
            raise ValueError(
                f"Bracelet anode geometry invalid: "
                f"2πL/r = {2*math.pi*L/r:.2f} must be > {math.e:.2f} (require L/r >= 0.43)"
            )
        R = (rho / (2.0 * math.pi * L)) * (math.log(2.0 * math.pi * L / r) - 1.0)

    return R  # Full precision; caller rounds for output only


def _b401_anode_requirements(
    inputs: Mapping[str, Any], current_demand: Mapping[str, Any]
) -> dict[str, Any]:
    """Calculate anode mass and count requirements (B401 Sec. 7, Tables 10-6 / 10-8).

    M = (I_mean * T * 8760) / (epsilon * u)
    N = ceil(M / m_a)

    epsilon is the Table 10-6 capacity for the anode material in seawater.
    u is ``anode.utilization_factor`` when supplied; otherwise the Table 10-8
    factor for the anode type (stand_off -> long slender stand-off 0.90,
    flush_mounted -> long flush-mounted 0.85, bracelet -> 0.80).

    Raises:
        ValueError: If material or anode type unknown, T/u/m_a out of range,
                    or any numeric input is NaN/infinite.

    Returns dict with total_mass_kg, anode_count, etc.
    """
    design_data = inputs.get("design_data", {})
    anode = inputs.get("anode", {})
    edition = _b401_edition(inputs)
    T = _require_finite(float(design_data.get("design_life", 25.0)), "design_life")
    material = anode.get("material", _B401_DEFAULT_MATERIAL)
    anode_type = anode.get("type", _B401_DEFAULT_ANODE_TYPE)
    m_a = _require_finite(
        float(anode.get("individual_anode_mass_kg", 200.0)), "individual_anode_mass_kg"
    )

    material_key = _require_valid_material(material)
    shape = _require_valid_anode_type(anode_type)
    capacity = b401_tables.anode_capacity(material_key, AnodeEnvironment.SEAWATER, edition)
    epsilon = capacity.value
    cited: list[CitedValue] = [capacity]

    u_input = anode.get("utilization_factor")
    if u_input is None:
        u_cited = b401_tables.utilisation_factor(shape, edition)
        u = u_cited.value
        cited.append(u_cited)
        u_source = f"{u_cited.citation.section} ({shape.value})"
    else:
        u = _require_finite(float(u_input), "utilization_factor")
        u_source = "input"

    if T <= 0:
        raise ValueError(f"Design life must be > 0, got {T}")
    if u <= 0 or u > 1.0:
        raise ValueError(f"Utilization factor must be in (0, 1.0], got {u}")
    if m_a <= 0:
        raise ValueError(f"Individual anode mass must be > 0, got {m_a}")

    I_mean = float(current_demand["total_mean_A"])
    total_Ah = I_mean * T * _HOURS_PER_YEAR
    total_mass_kg = total_Ah / (epsilon * u)
    anode_count = math.ceil(total_mass_kg / m_a)

    return {
        "total_mass_kg": round(total_mass_kg, 2),
        "anode_count": anode_count,
        "individual_mass_kg": round(m_a, 2),
        "anode_material": material,
        "anode_type": anode_type,
        "electrochemical_capacity_Ah_kg": epsilon,
        "utilization_factor": u,
        "utilization_factor_source": u_source,
        "design_life_hours": round(T * _HOURS_PER_YEAR, 1),
        "citations": _citation_keys(cited),
    }


def _b401_verify_current_output(
    inputs: Mapping[str, Any],
    anode_req: Mapping[str, Any],
    resistance: float,
    current_demand: Mapping[str, Any],
) -> dict[str, Any]:
    """Verify total anode current output against the initial and final demand.

    B401 Sec. 7 requires the anode current output to meet the initial demand
    (fresh anode geometry) and the final demand (end of design life). With
    driving voltage from Sec. 5 and Table 10-6:
      driving_voltage = E_structure - E_anode  (positive: anode more negative)
      I_output_per_anode = driving_voltage / R_a
      I_total = N * I_output_per_anode
      adequate iff I_total >= total_initial_A and I_total >= total_final_A

    Both checks use the fresh-anode resistance; the reduced final-geometry
    resistance of Sec. 7 is not modelled here.

    Args:
        inputs: Configuration dict
        anode_req: Result from _b401_anode_requirements
        resistance: Anode resistance (ohm) from _b401_anode_resistance (must be > 0)
        current_demand: Result from _b401_current_demand (total_initial_A and
                        total_final_A must be > 0)

    Returns dict with adequate bool, governing_case ("initial" | "final" |
    "mass"), recommended_anode_count (max of the three counts) and the
    comparison values.
    """
    anode = inputs.get("anode", {})
    material = anode.get("material", _B401_DEFAULT_MATERIAL)
    material_key = _require_valid_material(material)
    edition = _b401_edition(inputs)

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
    I_initial_demand = float(current_demand["total_initial_A"])
    if I_initial_demand <= 0:
        raise ValueError(
            f"Initial current demand must be > 0, got {I_initial_demand}. "
            "Check that zones have non-zero areas and valid coating breakdown."
        )
    E_anode_cv = b401_tables.anode_closed_circuit_potential(
        material_key, AnodeEnvironment.SEAWATER, edition
    )
    E_structure_cv = b401_tables.protection_potential(edition)
    E_anode = E_anode_cv.value
    E_structure = E_structure_cv.value
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
    initial_ok = I_total_output >= I_initial_demand
    final_ok = I_total_output >= I_final_demand
    adequate = initial_ok and final_ok

    # Minimum counts satisfying each current-output criterion
    count_by_initial = math.ceil(I_initial_demand / I_per_anode)
    count_by_final = math.ceil(I_final_demand / I_per_anode)
    # Recommended count satisfies mass AND both current-output criteria
    recommended_count = max(N, count_by_initial, count_by_final)
    if recommended_count == N:
        governing_case = "mass"
    elif count_by_final >= count_by_initial:
        governing_case = "final"
    else:
        governing_case = "initial"

    return {
        "adequate": adequate,
        "governing_case": governing_case,
        "initial_meets_demand": initial_ok,
        "final_meets_demand": final_ok,
        "driving_voltage_V": round(driving_voltage, 4),
        "anode_current_output_per_anode_A": round(I_per_anode, 4),
        "total_anode_current_output_A": round(I_total_output, 3),
        "initial_current_demand_A": round(I_initial_demand, 3),
        "final_current_demand_A": round(I_final_demand, 3),
        "mean_current_demand_A": round(I_mean_demand, 3),
        "anode_count": N,
        "count_by_initial_current": count_by_initial,
        "count_by_final_current": count_by_final,
        "recommended_anode_count": recommended_count,  # max(mass, initial, final)
        "anode_resistance_ohm": round(resistance, 6),
        "citations": _citation_keys([E_anode_cv, E_structure_cv]),
    }


def _b401_collect_citations(*blocks: Mapping[str, Any]) -> list[str]:
    """Sorted unique citation strings from result blocks and their per-zone entries."""
    found: set[str] = set()
    for block in blocks:
        for key, value in block.items():
            if key == "citations" and isinstance(value, list):
                found.update(str(v) for v in value)
            elif isinstance(value, Mapping):
                found.update(_b401_collect_citations(value))
    return sorted(found)


def _b401_offshore_platform(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the B401 Sec. 7 design loop and write ``cfg["results"]``.

    Reads ``inputs.design_data.edition`` (default "2021"); ``results``
    carries ``standard`` (edition-specific name), ``edition``, ``provenance``
    (from ``b401_tables.edition_provenance``) and ``citations`` (sorted unique
    "code_id revision section" strings of every table value used).
    """
    inputs = cfg.get("inputs", {})
    design_data = inputs.get("design_data", {})
    design_life = design_data.get("design_life", 25.0)
    edition = _b401_edition(inputs)

    areas = _b401_surface_areas(inputs)
    breakdown = _b401_coating_breakdown(inputs, design_life)
    densities = _b401_current_densities(inputs)
    current_demand = _b401_current_demand(inputs, areas, densities, breakdown)
    resistance = _b401_anode_resistance(inputs)
    anode_req = _b401_anode_requirements(inputs, current_demand)
    verification = _b401_verify_current_output(inputs, anode_req, resistance, current_demand)

    cfg["results"] = {
        "standard": standard_for_edition(edition),
        "edition": edition,
        "provenance": b401_tables.edition_provenance(edition),
        "design_life_years": design_life,
        "surface_areas_m2": areas,
        "coating_breakdown": breakdown,
        "current_densities_A_m2": densities,
        "current_demand_A": current_demand,
        "anode_resistance_ohm": round(resistance, 6),
        "anode_requirements": anode_req,
        "current_output_verification": verification,
        "citations": _b401_collect_citations(breakdown, densities, anode_req, verification),
    }
    return cfg
