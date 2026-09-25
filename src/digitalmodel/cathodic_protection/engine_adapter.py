"""Engine adapter for ``basename: cathodic_protection`` (issue #2210).

``run_cathodic_protection(cfg)`` is the single entry the YAML engine calls.
It dispatches on ``cfg["inputs"]["calculation_type"]``:

``DNV_RP_B401_offshore``
    The new package: zones validated as
    :class:`~digitalmodel.cathodic_protection.marine_structure_cp.StructuralZone`,
    Table 10-1 / 10-2 / Sec. 6.3 densities from
    :func:`~digitalmodel.cathodic_protection.marine_structure_cp.zone_current_density`,
    Table 10-4 coating breakdown through :mod:`b401_tables` and the kernel,
    and the full B401 Sec. 7 loop ``N = max(N_mass, N_initial, N_final)`` with
    the Table 10-7 resistance from
    :func:`~digitalmodel.cathodic_protection.anode_sizing.calculate_anode_resistance`.
``DNV_RP_F103_2010``
    :func:`~digitalmodel.cathodic_protection.dnv_rp_f103.design_bracelet_cp`
    mapped from the pipeline YAML schema.
``ABS_gn_ships_2018`` / ``ABS_gn_offshore_2018``
    The legacy implementation (no new-package equivalent), wrapped so anode
    counts are integers (``ceil``) and a ``status`` block is derived from the
    route's own adequacy checks.
``DNV_RP_B401_offshore_legacy`` / ``DNV_RP_F103_2010_legacy``
    The old code paths unchanged, with a :class:`DeprecationWarning`.

Every route writes ``cfg["results"]["status"]``::

    {"result": "PASS" | "FAIL", "governing_case": ..., "reason": ..., "checks": {...}}

A ``FAIL`` is logged as a warning through the engine's logger (loguru) and
never raises: the run completes so the report can show the failing design.
"""

from __future__ import annotations

import math
import warnings
from collections.abc import Mapping
from typing import Any, Final, NamedTuple

from loguru import logger

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    DEFAULT_F103_EDITION,
    Edition,
    F103Edition,
    normalize_edition,
    normalize_f103_edition,
    standard_for_edition,
)
from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    calculate_anode_resistance,
    depleted_equivalent_radius,
    governing_case,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    DesignPhase,
    PaintCategory,
    anode_capacity,
    citation_label,
    climate_from_temperature,
    coating_breakdown_constants,
    depth_band,
    design_driving_voltage,
    edition_provenance,
    utilisation_factor,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    DEFAULT_METALLIC_VOLTAGE_DROP_V,
    STEEL_RESISTIVITY,
    BraceletDesignInput,
    BraceletDesignResult,
    design_bracelet_cp,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure,
    FieldJointCoating,
    LinepipeCoating,
    fluid_temperature_band,
)
from digitalmodel.cathodic_protection.marine_structure_cp import (
    DEFAULT_SEAWATER_RESISTIVITY_OHM_M,
    ExposureZone,
    StructuralZone,
    zone_current_density,
)
from digitalmodel.citations import CitedValue

KEY_B401: Final = "DNV_RP_B401_offshore"
KEY_F103: Final = "DNV_RP_F103_2010"
KEY_ABS_SHIPS: Final = "ABS_gn_ships_2018"
KEY_ABS_OFFSHORE: Final = "ABS_gn_offshore_2018"
KEY_B401_LEGACY: Final = "DNV_RP_B401_offshore_legacy"
KEY_F103_LEGACY: Final = "DNV_RP_F103_2010_legacy"

CALCULATION_TYPES: Final[tuple[str, ...]] = (
    KEY_B401,
    KEY_F103,
    KEY_ABS_SHIPS,
    KEY_ABS_OFFSHORE,
    KEY_B401_LEGACY,
    KEY_F103_LEGACY,
)

STATUS_PASS: Final = "PASS"
STATUS_FAIL: Final = "FAIL"

# YAML zone names -> exposure zone of the new package. "buried" is the
# legacy spelling of the mudline zone.
_ZONE_BY_NAME: Final[dict[str, ExposureZone]] = {
    "submerged": ExposureZone.SUBMERGED,
    "tidal": ExposureZone.TIDAL,
    "splash": ExposureZone.SPLASH,
    "atmospheric": ExposureZone.ATMOSPHERIC,
    "buried": ExposureZone.BURIED_MUDLINE,
    "buried_mudline": ExposureZone.BURIED_MUDLINE,
}

_BARE_COATING: Final = "bare"
_DEFAULT_COATING: Final = "III"

_MATERIAL_BY_NAME: Final[dict[str, AnodeMaterial]] = {
    "aluminium": AnodeMaterial.ALUMINIUM,
    "aluminum": AnodeMaterial.ALUMINIUM,
    "al-based": AnodeMaterial.ALUMINIUM,
    "zinc": AnodeMaterial.ZINC,
    "zn-based": AnodeMaterial.ZINC,
}

# YAML anode types -> (anode_sizing type, Table 10-8 shape for utilisation).
_ANODE_TYPE_BY_NAME: Final[dict[str, tuple[AnodeType, AnodeShape]]] = {
    "stand_off": (AnodeType.STAND_OFF, AnodeShape.LONG_SLENDER_STANDOFF),
    "flush_mounted": (AnodeType.FLUSH_MOUNT, AnodeShape.LONG_FLUSH),
    "flush_mount": (AnodeType.FLUSH_MOUNT, AnodeShape.LONG_FLUSH),
    "bracelet": (AnodeType.BRACELET, AnodeShape.SHORT_FLUSH_BRACELET),
}

# Legacy F103 YAML coating names -> Table A.1 rows.
_LINEPIPE_COATING_BY_NAME: Final[dict[str, LinepipeCoating]] = {
    "fbe": LinepipeCoating.FBE,
    "single_layer_fbe": LinepipeCoating.FBE,
    "dual_layer_fbe": LinepipeCoating.FBE,
    "single_or_dual_layer_fbe": LinepipeCoating.FBE,
    "3lpe": LinepipeCoating.THREE_LAYER_FBE_PE,
    "3layer_fbe_pe": LinepipeCoating.THREE_LAYER_FBE_PE,
    "three_layer_fbe_pe": LinepipeCoating.THREE_LAYER_FBE_PE,
    "3lpp": LinepipeCoating.THREE_LAYER_FBE_PP,
    "3layer_fbe_pp": LinepipeCoating.THREE_LAYER_FBE_PP,
    "three_layer_fbe_pp": LinepipeCoating.THREE_LAYER_FBE_PP,
    "multi_layer_fbe_pp": LinepipeCoating.MULTI_LAYER_FBE_PP,
    "polychloroprene": LinepipeCoating.POLYCHLOROPRENE,
    "glass_fibre_reinforced_asphalt_enamel": LinepipeCoating.GFR_ASPHALT_ENAMEL,
    "glass_fibre_reinforced_coal_tar_enamel": LinepipeCoating.GFR_COAL_TAR_ENAMEL,
}

_EXPOSURE_BY_NAME: Final[dict[str, Exposure]] = {
    "buried": Exposure.BURIED,
    "non_buried": Exposure.NON_BURIED,
    "non-buried": Exposure.NON_BURIED,
    "exposed": Exposure.NON_BURIED,
}

_OHM_CM_PER_OHM_M: Final = 100.0


# ---------------------------------------------------------------------------
# Small input helpers
# ---------------------------------------------------------------------------


def _section(cfg: Mapping[str, Any], *path: str) -> Mapping[str, Any]:
    """Nested mapping at ``path`` (empty mapping when absent)."""
    node: Any = cfg
    for key in path:
        node = node.get(key, {}) if isinstance(node, Mapping) else {}
    return node if isinstance(node, Mapping) else {}


def _require(section: Mapping[str, Any], key: str, where: str) -> Any:
    if key not in section or section[key] is None:
        raise ValueError(f"inputs.{where}.{key} is required")
    return section[key]


def _positive(value: Any, name: str) -> float:
    val = float(value)
    if not math.isfinite(val) or val <= 0.0:
        raise ValueError(f"{name} must be a positive finite number, got {value!r}")
    return val


def _lookup(table: Mapping[str, Any], raw: Any, what: str) -> Any:
    key = str(raw).strip().lower()
    if key not in table:
        raise ValueError(f"unknown {what} {raw!r}; accepted: {sorted(table)}")
    return table[key]


def _cite(labels: list[str], *values: CitedValue) -> None:
    for cv in values:
        label = citation_label(cv.citation)
        if label not in labels:
            labels.append(label)


def _set_status(
    cfg: dict[str, Any],
    results: dict[str, Any],
    passed: bool,
    governing: str,
    reason: str,
    checks: Mapping[str, Any],
) -> None:
    """Write ``results["status"]`` and warn through the engine logger on FAIL."""
    status = {
        "result": STATUS_PASS if passed else STATUS_FAIL,
        "governing_case": governing,
        "reason": reason,
        "checks": dict(checks),
    }
    results["status"] = status
    calc = _section(cfg, "inputs").get("calculation_type")
    if passed:
        logger.info(f"cathodic_protection [{calc}] status PASS ({governing}): {reason}")
    else:
        logger.warning(f"cathodic_protection [{calc}] status FAIL ({governing}): {reason}")


# ---------------------------------------------------------------------------
# DNV-RP-B401 offshore structure (new package)
# ---------------------------------------------------------------------------


def _b401_zones(structure: Mapping[str, Any]) -> list[tuple[str, str, str, StructuralZone]]:
    """``(zone_id, base_zone, coating_category, StructuralZone)`` per YAML zone."""
    raw_zones = structure.get("zones") or []
    if not raw_zones:
        raise ValueError("inputs.structure.zones is required and must not be empty")
    zones: list[tuple[str, str, str, StructuralZone]] = []
    seen: set[str] = set()
    for z in raw_zones:
        zone_id = str(_require(z, "zone", "structure.zones[]"))
        if zone_id in seen:
            raise ValueError(f"duplicate zone id {zone_id!r}; use base_zone for segments")
        seen.add(zone_id)
        base_zone = str(z.get("base_zone", zone_id))
        exposure = _lookup(_ZONE_BY_NAME, base_zone, "zone")
        category = str(z.get("coating_category", _DEFAULT_COATING))
        zone = StructuralZone(
            zone_name=zone_id,
            exposure_zone=exposure,
            surface_area_m2=float(_require(z, "area_m2", f"structure.zones[{zone_id}]")),
            depth_m=float(z.get("depth_m", 0.0)),
        )
        zones.append((zone_id, base_zone, category, zone))
    return zones


def _b401_breakdown(
    category: str, zone: StructuralZone, design_life: float, edition: Edition
) -> tuple[dict[str, Any], list[CitedValue]]:
    band = depth_band(zone.depth_m)
    if category.strip().lower() == _BARE_COATING:
        a, b = 1.0, 0.0
        cited: list[CitedValue] = []
    else:
        try:
            paint = PaintCategory(category.strip().upper())
        except ValueError as exc:
            raise ValueError(
                f"unknown coating_category {category!r} for zone {zone.zone_name!r}; "
                f"accepted: I, II, III, bare"
            ) from exc
        a_cv, b_cv = coating_breakdown_constants(paint, band, edition)
        a, b = a_cv.value, b_cv.value
        cited = [a_cv, b_cv]
    return (
        {
            "coating_category": category,
            "a": a,
            "b_per_yr": b,
            "depth_band": band.value,
            "f_ci": kernel.coating_breakdown_linear(a, b, 0.0),
            "f_cm": kernel.coating_breakdown_mean(a, b, design_life),
            "f_cf": kernel.coating_breakdown_final(a, b, design_life),
        },
        cited,
    )


def _b401_densities(
    zone: StructuralZone, base_zone: str, temp_c: float, edition: Edition
) -> tuple[dict[str, Any], list[CitedValue]]:
    climate = climate_from_temperature(temp_c)
    band = depth_band(zone.depth_m)
    values: dict[DesignPhase, float] = {}
    cited: list[CitedValue] = []
    for phase in (DesignPhase.INITIAL, DesignPhase.MEAN, DesignPhase.FINAL):
        cv = zone_current_density(zone.exposure_zone, climate, zone.depth_m, phase, edition)
        if cv is None:
            values[phase] = 0.0
        else:
            values[phase] = cv.value
            cited.append(cv)
    out: dict[str, Any] = {
        "i_initial_A_m2": values[DesignPhase.INITIAL],
        "i_mean_A_m2": values[DesignPhase.MEAN],
        "i_final_A_m2": values[DesignPhase.FINAL],
        "base_zone": base_zone,
        "exposure_zone": zone.exposure_zone.value,
        "climate": climate.value if cited else "N/A",
        "depth_band": band.value,
        "seawater_temperature_C": temp_c,
    }
    if not cited:
        out["note"] = (
            "B401 CP design current densities apply to seawater-exposed and "
            "buried surfaces only; this zone draws no CP current"
        )
    return out, cited


class _AnodeGeometry(NamedTuple):
    """Fresh and depleted anode geometry for the Sec. 7.8 checks."""

    length_m: float
    net_mass_kg: float
    radius_initial_m: float
    radius_final_m: float
    radius_source: str
    width_m: float | None
    thickness_m: float | None
    exposed_area_m2: float | None


def _b401_anode_geometry(anode: Mapping[str, Any], utilization: float) -> _AnodeGeometry:
    """Anode geometry from ``inputs.anode``.

    ``length_m`` is required. With ``radius_m`` given, it is the fresh
    equivalent radius and the depleted radius scales as ``r sqrt(1 - u)``
    (remaining cross-section at the same length). Without it both radii are
    mass based (``_kernels.equivalent_radius_from_mass`` and
    ``anode_sizing.depleted_equivalent_radius``).
    """
    L = _positive(_require(anode, "length_m", "anode"), "inputs.anode.length_m")
    m_a = _positive(
        _require(anode, "individual_anode_mass_kg", "anode"),
        "inputs.anode.individual_anode_mass_kg",
    )
    density = _positive(anode.get("density_kg_m3", kernel.ANODE_DENSITY_ALZNI), "density")
    radius_in = anode.get("radius_m")
    if radius_in is not None:
        r_initial = _positive(radius_in, "inputs.anode.radius_m")
        remaining = 1.0 - utilization
        r_final = r_initial * math.sqrt(remaining) if remaining > 0.0 else r_initial
        radius_source = "input"
    else:
        r_initial = kernel.equivalent_radius_from_mass(m_a, L, density)
        r_final = depleted_equivalent_radius(m_a, L, utilization, density)
        radius_source = "mass"

    def _optional(key: str) -> float | None:
        value = anode.get(key)
        return None if value is None else _positive(value, f"inputs.anode.{key}")

    return _AnodeGeometry(
        length_m=L,
        net_mass_kg=m_a,
        radius_initial_m=r_initial,
        radius_final_m=r_final,
        radius_source=radius_source,
        width_m=_optional("width_m"),
        thickness_m=_optional("thickness_m"),
        exposed_area_m2=_optional("exposed_area_m2"),
    )


def _run_b401(cfg: dict[str, Any]) -> dict[str, Any]:
    inputs = _section(cfg, "inputs")
    design_data = _section(inputs, "design_data")
    environment = _section(inputs, "environment")
    anode = _section(inputs, "anode")
    structure = _section(inputs, "structure")

    edition = normalize_edition(str(design_data.get("edition", DEFAULT_EDITION)), stacklevel=3)
    design_life = _positive(design_data.get("design_life", 25.0), "design_data.design_life")
    temp_c = float(environment.get("seawater_temperature_C", 10.0))
    rho = _positive(
        environment.get("seawater_resistivity_ohm_m", DEFAULT_SEAWATER_RESISTIVITY_OHM_M),
        "inputs.environment.seawater_resistivity_ohm_m",
    )

    material_name = str(anode.get("material", "aluminium"))
    material = _lookup(_MATERIAL_BY_NAME, material_name, "anode material")
    type_name = str(anode.get("type", "stand_off"))
    anode_type, shape = _lookup(_ANODE_TYPE_BY_NAME, type_name, "anode type")

    citations: list[str] = []

    # Zones: areas, coating breakdown (Table 10-4), densities (10-1 / 10-2 / 6.3)
    areas: dict[str, float] = {}
    breakdown: dict[str, Any] = {}
    densities: dict[str, Any] = {}
    demand: dict[str, Any] = {}
    total_initial = total_mean = total_final = 0.0
    for zone_id, base_zone, category, zone in _b401_zones(structure):
        areas[zone_id] = zone.surface_area_m2
        fc, fc_cited = _b401_breakdown(category, zone, design_life, edition)
        dens, dens_cited = _b401_densities(zone, base_zone, temp_c, edition)
        fc["citations"] = sorted(citation_label(cv.citation) for cv in fc_cited)
        dens["citations"] = sorted({citation_label(cv.citation) for cv in dens_cited})
        _cite(citations, *fc_cited, *dens_cited)
        breakdown[zone_id] = fc
        densities[zone_id] = dens
        I_initial = kernel.current_demand(zone.surface_area_m2, dens["i_initial_A_m2"], fc["f_ci"])
        I_mean = kernel.current_demand(zone.surface_area_m2, dens["i_mean_A_m2"], fc["f_cm"])
        I_final = kernel.current_demand(zone.surface_area_m2, dens["i_final_A_m2"], fc["f_cf"])
        demand[zone_id] = {
            "area_m2": zone.surface_area_m2,
            "i_initial_A_m2": dens["i_initial_A_m2"],
            "i_mean_A_m2": dens["i_mean_A_m2"],
            "i_final_A_m2": dens["i_final_A_m2"],
            "f_ci": fc["f_ci"],
            "f_cm": fc["f_cm"],
            "f_cf": fc["f_cf"],
            "I_initial_A": round(I_initial, 4),
            "I_mean_A": round(I_mean, 4),
            "I_final_A": round(I_final, 4),
        }
        total_initial += I_initial
        total_mean += I_mean
        total_final += I_final
    areas["total_m2"] = sum(areas.values())
    demand["total_initial_A"] = total_initial
    demand["total_mean_A"] = total_mean
    demand["total_final_A"] = total_final
    if total_mean <= 0.0:
        raise ValueError(
            "mean current demand is zero: no zone is seawater-exposed or buried "
            "with a non-zero area"
        )

    # Anode mass (Eq. 2) with Table 10-6 capacity and Table 10-8 utilisation
    capacity = anode_capacity(material, AnodeEnvironment.SEAWATER, edition)
    req_cited: list[CitedValue] = [capacity]
    u_in = anode.get("utilization_factor")
    if u_in is None:
        u_cv = utilisation_factor(shape, edition)
        u = u_cv.value
        u_source = f"{u_cv.citation.section} ({shape.value})"
        req_cited.append(u_cv)
    else:
        u = float(u_in)
        if not 0.0 < u <= 1.0:
            raise ValueError(f"inputs.anode.utilization_factor must be in (0, 1], got {u}")
        u_source = "input"
    _cite(citations, *req_cited)
    geometry = _b401_anode_geometry(anode, u)
    m_a = geometry.net_mass_kg
    total_mass = kernel.anode_mass(total_mean, design_life, capacity.value, u)
    n_mass = kernel.anode_count(total_mass, m_a)

    # Sec. 7.8 current-output checks: fresh anode -> N_initial, depleted -> N_final
    delta_E = design_driving_voltage(material, edition)
    _cite(citations, delta_E)
    L = geometry.length_m
    R_initial = calculate_anode_resistance(
        anode_type,
        L,
        geometry.radius_initial_m,
        rho,
        width_m=geometry.width_m,
        thickness_m=geometry.thickness_m,
        exposed_area_m2=geometry.exposed_area_m2,
    )
    I_a_initial = kernel.anode_current_output(delta_E.value, R_initial)
    n_initial = kernel.anodes_for_current(total_initial, I_a_initial)
    R_final = calculate_anode_resistance(anode_type, L, geometry.radius_final_m, rho)
    I_a_final = kernel.anode_current_output(delta_E.value, R_final)
    n_final = kernel.anodes_for_current(total_final, I_a_final)
    recommended = max(1, n_mass, n_initial, n_final)
    governing = governing_case(n_mass, n_initial, n_final)

    # The count verified: the installed count when given, else the mass count
    count_in = anode.get("count")
    if count_in is None:
        n_verified = n_mass
        count_source = "mass"
    else:
        n_verified = int(count_in)
        if n_verified < 1:
            raise ValueError(f"inputs.anode.count must be >= 1, got {count_in!r}")
        count_source = "input"
    initial_ok = n_verified * I_a_initial >= total_initial
    final_ok = n_verified * I_a_final >= total_final
    mass_ok = n_verified * m_a >= total_mass
    adequate = initial_ok and final_ok and mass_ok

    anode_req = {
        "total_mass_kg": round(total_mass, 2),
        "anode_count": n_mass,
        "individual_mass_kg": round(m_a, 2),
        "anode_material": material_name,
        "anode_type": type_name,
        "electrochemical_capacity_Ah_kg": capacity.value,
        "utilization_factor": u,
        "utilization_factor_source": u_source,
        "design_life_hours": round(design_life * kernel.HOURS_PER_YEAR, 1),
        "citations": sorted({citation_label(cv.citation) for cv in req_cited}),
    }
    verification = {
        "adequate": adequate,
        "governing_case": governing,
        "initial_meets_demand": initial_ok,
        "final_meets_demand": final_ok,
        "mass_meets_requirement": mass_ok,
        "driving_voltage_V": round(delta_E.value, 4),
        "anode_count": n_verified,
        "anode_count_source": count_source,
        "count_by_mass": n_mass,
        "count_by_initial_current": n_initial,
        "count_by_final_current": n_final,
        "recommended_anode_count": recommended,
        "anode_resistance_initial_ohm": round(R_initial, 6),
        "anode_current_output_initial_A": round(I_a_initial, 4),
        "total_anode_current_output_initial_A": round(n_verified * I_a_initial, 3),
        "anode_resistance_final_ohm": round(R_final, 6),
        "anode_current_output_final_A": round(I_a_final, 4),
        "total_anode_current_output_final_A": round(n_verified * I_a_final, 3),
        "equivalent_radius_initial_m": round(geometry.radius_initial_m, 6),
        "equivalent_radius_final_m": round(geometry.radius_final_m, 6),
        "radius_source": geometry.radius_source,
        "initial_current_demand_A": round(total_initial, 3),
        "mean_current_demand_A": round(total_mean, 3),
        "final_current_demand_A": round(total_final, 3),
        "citations": [citation_label(delta_E.citation)],
    }

    results: dict[str, Any] = {
        "standard": standard_for_edition(edition),
        "edition": edition,
        "provenance": edition_provenance(edition),
        "design_life_years": design_life,
        "surface_areas_m2": areas,
        "coating_breakdown": breakdown,
        "current_densities_A_m2": densities,
        "current_demand_A": demand,
        "anode_resistance_ohm": round(R_initial, 6),
        "anode_requirements": anode_req,
        "current_output_verification": verification,
        "citations": sorted(citations),
    }
    cfg["results"] = results

    if adequate:
        reason = (
            f"{n_verified} x {m_a:g} kg {type_name} anodes ({count_source}) meet the "
            f"mass requirement ({total_mass:.0f} kg) and the initial "
            f"({total_initial:.1f} A) and final ({total_final:.1f} A) current demands"
        )
    else:
        failing = [
            name
            for name, ok in (
                ("mass", mass_ok),
                ("initial current output", initial_ok),
                ("final current output", final_ok),
            )
            if not ok
        ]
        reason = (
            f"{n_verified} x {m_a:g} kg {type_name} anodes ({count_source}) fail the "
            f"{', '.join(failing)} check(s); the {governing} case governs: "
            f"N_mass={n_mass}, N_initial={n_initial}, N_final={n_final}; "
            f"install {recommended} anodes (B401 Sec. 7.8)"
        )
    _set_status(
        cfg,
        results,
        adequate,
        governing,
        reason,
        {"mass": mass_ok, "initial_current_output": initial_ok, "final_current_output": final_ok},
    )
    return cfg


# ---------------------------------------------------------------------------
# DNV-RP-F103 submarine pipeline (new package)
# ---------------------------------------------------------------------------


def _f103_input(cfg: dict[str, Any]) -> tuple[BraceletDesignInput, F103Edition, dict[str, str]]:
    inputs = _section(cfg, "inputs")
    design_data = _section(inputs, "design_data")
    pipeline = _section(inputs, "pipeline")
    environment = _section(inputs, "environment")
    anode = _section(inputs, "anode")
    design = _section(inputs, "design")

    edition = normalize_f103_edition(
        str(design_data.get("edition", DEFAULT_F103_EDITION)), stacklevel=3
    )
    coating_name = str(pipeline.get("coating_type", "FBE"))
    exposure_name = str(pipeline.get("burial_condition", "non_buried"))
    material_name = str(anode.get("material", "aluminium"))

    if "seawater_resistivity_ohm_m" in environment:
        rho = float(environment["seawater_resistivity_ohm_m"])
    elif "seawater_resistivity_ohm_cm" in environment:
        rho = float(environment["seawater_resistivity_ohm_cm"]) / _OHM_CM_PER_OHM_M
    else:
        rho = DEFAULT_SEAWATER_RESISTIVITY_OHM_M

    fj_name = pipeline.get("field_joint_coating")
    fjc = FieldJointCoating.NONE if fj_name is None else FieldJointCoating(str(fj_name))
    kwargs: dict[str, Any] = {}
    for key in ("field_joint_area_fraction", "field_joint_count", "field_joint_length_m"):
        if pipeline.get(key) is not None:
            kwargs[key] = pipeline[key]
    for src, dst in (("thickness_m", "bracelet_thickness_m"), ("exposed_area_m2", "bracelet_exposed_area_m2")):
        if anode.get(src) is not None:
            kwargs[dst] = anode[src]

    inp = BraceletDesignInput(
        outer_diameter_m=_require(pipeline, "outer_diameter_m", "pipeline"),
        wall_thickness_m=_require(pipeline, "wall_thickness_m", "pipeline"),
        length_m=_require(pipeline, "length_m", "pipeline"),
        linepipe_coating=_lookup(_LINEPIPE_COATING_BY_NAME, coating_name, "coating_type"),
        field_joint_coating=fjc,
        exposure=_lookup(_EXPOSURE_BY_NAME, exposure_name, "burial_condition"),
        fluid_temperature_c=_require(pipeline, "internal_fluid_temperature_C", "pipeline"),
        design_life_years=_positive(design_data.get("design_life", 25.0), "design_life"),
        seawater_resistivity_ohm_m=rho,
        steel_resistivity_ohm_m=float(pipeline.get("resistivity_ohm_m", STEEL_RESISTIVITY)),
        anode_material=_lookup(_MATERIAL_BY_NAME, material_name, "anode material"),
        bracelet_net_mass_kg=_require(anode, "individual_anode_mass_kg", "anode"),
        bracelet_length_m=_require(anode, "length_m", "anode"),
        delta_E_me_V=float(design.get("metallic_voltage_drop_V", DEFAULT_METALLIC_VOLTAGE_DROP_V)),
        **kwargs,
    )
    names = {"coating_type": coating_name, "burial_condition": exposure_name, "material": material_name}
    return inp, edition, names


def _f103_results(
    inp: BraceletDesignInput, res: BraceletDesignResult, names: Mapping[str, str], u_source: str
) -> dict[str, Any]:
    return {
        "standard": res.standard,
        "edition": res.edition_used,
        "provenance": res.provenance,
        "design_life_years": inp.design_life_years,
        "pipeline_geometry_m": {
            "outer_diameter_m": inp.outer_diameter_m,
            "wall_thickness_m": inp.wall_thickness_m,
            "length_m": inp.length_m,
            "outer_surface_area_m2": round(res.surface_area_m2, 3),
            "linepipe_area_m2": round(res.linepipe_area_m2, 3),
            "field_joint_area_m2": round(res.field_joint_area_m2, 3),
            "steel_resistivity_ohm_m": inp.steel_resistivity_ohm_m,
        },
        "coating_breakdown_factors": {
            "linepipe_coating": names["coating_type"],
            "field_joint_coating": inp.field_joint_coating.value,
            "design_life_years": inp.design_life_years,
            "mean_factor": round(res.f_cm_linepipe, 6),
            "final_factor": round(res.f_cf_linepipe, 6),
            "mean_factor_field_joint": round(res.f_cm_field_joint, 6),
            "final_factor_field_joint": round(res.f_cf_field_joint, 6),
        },
        "current_densities_A_m2": {
            "mean_current_density_A_m2": res.mean_current_density_A_m2,
            "burial_condition": names["burial_condition"],
            "exposure": inp.exposure.value,
            "internal_fluid_temperature_C": inp.fluid_temperature_c,
            "temperature_band": fluid_temperature_band(inp.fluid_temperature_c).value,
        },
        "current_demand_A": {
            "mean_current_demand_A": round(res.mean_current_demand_A, 4),
            "final_current_demand_A": round(res.final_current_demand_A, 4),
        },
        "anode_requirements": {
            "total_anode_mass_kg": round(res.total_net_mass_kg, 3),
            "anode_count": res.number_of_anodes,
            "anode_count_by_mass": res.number_of_anodes_mass,
            "anode_count_by_final_current": res.number_of_anodes_final,
            "individual_anode_mass_kg": inp.bracelet_net_mass_kg,
            "anode_material": names["material"],
            "anode_capacity_Ah_kg": res.anode_capacity_Ah_kg,
            "utilization_factor": res.utilisation_factor,
            "utilization_factor_source": u_source,
            "driving_voltage_V": res.driving_voltage_V,
            "bracelet_exposed_area_m2": round(res.bracelet_exposed_area_m2, 6),
            "anode_resistance_ohm": round(res.anode_resistance_ohm, 6),
            "anode_current_output_A": round(res.anode_current_output_A, 4),
        },
        "anode_spacing_m": {
            "spacing_m": round(res.anode_spacing_m, 3),
            "anode_count": res.number_of_anodes,
            "max_spacing_m": round(2.0 * res.protected_length_m, 3),
            "spacing_ok": res.spacing_ok,
        },
        "attenuation_analysis": {
            "protected_length_m": round(res.protected_length_m, 3),
            "metallic_voltage_drop_V": inp.delta_E_me_V,
            "protection_adequate": res.spacing_ok and res.current_output_ok,
            "current_output_ok": res.current_output_ok,
        },
        "citations": list(res.citations),
    }


def _run_f103(cfg: dict[str, Any]) -> dict[str, Any]:
    inp, edition, names = _f103_input(cfg)
    anode = _section(cfg, "inputs", "anode")
    res = design_bracelet_cp(inp, edition=edition)
    u_in = anode.get("utilization_factor")
    u_source = "Table 10-8 (bracelet)"
    if u_in is not None:
        # design_bracelet_cp fixes u at the Table 10-8 bracelet value; an
        # explicit input re-sizes the mass and the mass-based count.
        u = float(u_in)
        if not 0.0 < u <= 1.0:
            raise ValueError(f"inputs.anode.utilization_factor must be in (0, 1], got {u}")
        total_mass = kernel.anode_mass(
            res.mean_current_demand_A, inp.design_life_years, res.anode_capacity_Ah_kg, u
        )
        n_mass = kernel.anode_count(total_mass, inp.bracelet_net_mass_kg)
        n = max(1, n_mass, res.number_of_anodes_final)
        spacing = inp.length_m / n
        res = res.model_copy(
            update={
                "citations": [c for c in res.citations if "Table 10-8" not in c],
                "utilisation_factor": u,
                "total_net_mass_kg": total_mass,
                "number_of_anodes_mass": n_mass,
                "number_of_anodes": n,
                "governing_case": "mass" if n_mass >= res.number_of_anodes_final else "final",
                "anode_spacing_m": spacing,
                "spacing_ok": spacing <= 2.0 * res.protected_length_m,
                "current_output_ok": n * res.anode_current_output_A >= res.final_current_demand_A,
            }
        )
        u_source = "input"

    results = _f103_results(inp, res, names, u_source)
    cfg["results"] = results
    passed = res.spacing_ok and res.current_output_ok
    if passed:
        reason = (
            f"{res.number_of_anodes} x {inp.bracelet_net_mass_kg:g} kg bracelets at "
            f"{res.anode_spacing_m:.1f} m meet the mass ({res.total_net_mass_kg:.1f} kg) "
            f"and final current ({res.final_current_demand_A:.3f} A) requirements within "
            f"2 x protected length ({2.0 * res.protected_length_m:.0f} m)"
        )
    else:
        reason = (
            f"{res.number_of_anodes} x {inp.bracelet_net_mass_kg:g} kg bracelets: "
            f"spacing {res.anode_spacing_m:.1f} m vs 2 x protected length "
            f"{2.0 * res.protected_length_m:.1f} m (ok={res.spacing_ok}); current output "
            f"ok={res.current_output_ok}"
        )
    _set_status(
        cfg,
        results,
        passed,
        res.governing_case,
        reason,
        {"spacing": res.spacing_ok, "final_current_output": res.current_output_ok},
    )
    return cfg


# ---------------------------------------------------------------------------
# Legacy routes (ABS ships / offshore kept; DNV legacy keys deprecated)
# ---------------------------------------------------------------------------


def _legacy_solver() -> Any:
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (
        CathodicProtection,
    )

    return CathodicProtection()


def _run_abs_ships(cfg: dict[str, Any]) -> dict[str, Any]:
    _legacy_solver().ABS_gn_ships_2018(cfg)
    block: dict[str, Any] = cfg["cathodic_protection"]
    req = block["anode_requirements"]
    raw = req.get("anode_count")
    if raw is not None:
        req["anode_count_raw"] = raw
        req["anode_count"] = int(math.ceil(float(raw)))
    perf = block.get("anode_performance", {})
    checks = perf.get("checks")
    cfg["results"] = block
    if not checks:
        reason = (
            "ABS GN Ships 2018: anode current-output check not run "
            f"({perf.get('status', 'no anode geometry / resistivity')})"
        )
        _set_status(cfg, block, False, "mass", reason, {"current_output": None})
        return cfg
    initial_ok = bool(checks.get("initial_meets_demand"))
    final_ok = bool(checks.get("final_meets_demand"))
    passed = initial_ok and final_ok
    governing = "mass" if passed else ("final" if not final_ok else "initial")
    n = req["anode_count"]
    out = perf.get("current_output_A", {})
    reason = (
        f"{n} anodes ({req['total_mass_kg']:.0f} kg by mass): initial output "
        f"{out.get('initial_total', 0.0):.1f} A vs demand "
        f"{block['current_demand_A']['totals']['initial']:.1f} A (ok={initial_ok}); final output "
        f"{out.get('final_total', 0.0):.1f} A vs demand "
        f"{block['current_demand_A']['totals']['final']:.1f} A (ok={final_ok})"
    )
    _set_status(
        cfg,
        block,
        passed,
        governing,
        reason,
        {"initial_current_output": initial_ok, "final_current_output": final_ok},
    )
    return cfg


def _run_abs_offshore(cfg: dict[str, Any]) -> dict[str, Any]:
    _legacy_solver().ABS_gn_offshore_2018(cfg)
    results: dict[str, Any] = cfg["results"]
    anode = _section(cfg, "inputs", "anode")
    net_mass = anode.get("individual_anode_mass_kg")
    if net_mass is None:
        net_mass = _section(anode, "physical_properties").get("net_weight")
    mass_kg = float(results["anode_mass_kg"])
    if net_mass is not None:
        m_a = _positive(net_mass, "inputs.anode net mass")
        count = kernel.anode_count(mass_kg, m_a)
        results["anode_requirements"] = {
            "total_mass_kg": round(mass_kg, 3),
            "individual_mass_kg": m_a,
            "anode_count": count,
        }
        reason = (
            f"{count} x {m_a:g} kg anodes provide the {mass_kg:.0f} kg mean-current mass "
            "requirement; ABS GN Offshore 2018 route has no current-output check"
        )
    else:
        reason = (
            f"{mass_kg:.0f} kg anode mass required (no individual anode mass given, "
            "no count); ABS GN Offshore 2018 route has no current-output check"
        )
    _set_status(cfg, results, True, "mass", reason, {"mass": True, "current_output": None})
    return cfg


def _run_legacy(cfg: dict[str, Any], key: str) -> dict[str, Any]:
    replacement = KEY_B401 if key == KEY_B401_LEGACY else KEY_F103
    warnings.warn(
        f"calculation_type {key!r} runs the legacy solver and is deprecated; "
        f"use {replacement!r} (digitalmodel.cathodic_protection engine adapter, #2210)",
        DeprecationWarning,
        stacklevel=3,
    )
    solver = _legacy_solver()
    if key == KEY_B401_LEGACY:
        solver.DNV_RP_B401_offshore_platform(cfg)
    else:
        solver.DNV_RP_F103_2010(cfg)
    return cfg


# ---------------------------------------------------------------------------
# Entry
# ---------------------------------------------------------------------------


def run_cathodic_protection(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the cathodic-protection calculation selected by ``inputs.calculation_type``.

    Parameters
    ----------
    cfg : dict
        Engine configuration with ``inputs.calculation_type`` and the
        route's input sections.

    Returns
    -------
    dict
        The same ``cfg`` with ``cfg["results"]`` (and, for the ABS ships
        route, ``cfg["cathodic_protection"]``) populated, always carrying a
        ``status`` block for the new and ABS routes.

    Raises
    ------
    ValueError
        Unknown ``calculation_type`` or invalid inputs.
    """
    inputs = _section(cfg, "inputs")
    key = inputs.get("calculation_type")
    if key == KEY_B401:
        return _run_b401(cfg)
    if key == KEY_F103:
        return _run_f103(cfg)
    if key == KEY_ABS_SHIPS:
        return _run_abs_ships(cfg)
    if key == KEY_ABS_OFFSHORE:
        return _run_abs_offshore(cfg)
    if key in (KEY_B401_LEGACY, KEY_F103_LEGACY):
        return _run_legacy(cfg, str(key))
    raise ValueError(
        f"inputs.calculation_type {key!r} is not implemented; accepted keys: "
        f"{list(CALCULATION_TYPES)}"
    )


__all__ = [
    "CALCULATION_TYPES",
    "KEY_ABS_OFFSHORE",
    "KEY_ABS_SHIPS",
    "KEY_B401",
    "KEY_B401_LEGACY",
    "KEY_F103",
    "KEY_F103_LEGACY",
    "STATUS_FAIL",
    "STATUS_PASS",
    "run_cathodic_protection",
]
