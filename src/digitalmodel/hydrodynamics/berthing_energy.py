"""Berthing energy of an alongside ship and screening of catalogue fenders.

Method: the deterministic approach of PIANC MarCom WG33, *Guidelines for the
Design of Fender Systems: 2002*:

* section 4.2.1  E = 0.5 * M * v^2 * Ce * Cm * Cs * Cc  (M in t, v in m/s, E in kNm)
* section 4.2.4  Ce = (K^2 + R^2 cos^2 phi) / (K^2 + R^2),  K = (0.19 Cb + 0.11) L
* section 4.2.5  Cm from keel clearance: 1.8 at UKC <= 0.1 D, 1.5 at UKC >= 0.5 D,
                 linear between; the Ueda and Vasco Costa formulae for comparison
* section 4.2.6  Cs (1.0 for soft fenders);  section 4.2.7  Cc (1.0 open berth,
                 0.9 solid quay under parallel approach)
* Table 4.2.5    factor for abnormal impact applied to the normal berthing energy

PIANC MarCom WG211 (2024) supersedes WG33 with a semi-probabilistic format: a
characteristic energy multiplied by a partial energy factor, characteristic
berthing velocities by navigation condition, and fender performance corrected
by velocity, temperature, angular and multiple-contact factors and divided by
partial resistance factors. This module does not tabulate WG211 values. A
WG211 calculation supplies its own velocity and partial energy factor (in
place of the abnormal impact factor) and its own fender correction factors.
WG211 and WG33 inputs should not be mixed in one calculation.

Fender candidates are read from a catalogue CSV at a caller-supplied path,
with the columns in ``FENDER_CATALOGUE_COLUMNS``. No manufacturer data ship
with this module.
"""

from __future__ import annotations

import csv
import math
from dataclasses import dataclass
from pathlib import Path

SEAWATER_DENSITY_T_M3 = 1.025

# PIANC 2002 Table 4.2.5, factor for abnormal impact (Cab).
# (largest vessel, smallest vessel); single-valued rows repeat the value.
_ABNORMAL_IMPACT = {
    "tanker_bulk": (1.25, 1.75),
    "container": (1.5, 2.0),
    "general_cargo": (1.75, 1.75),
    "ro_ro_ferry": (2.0, 2.0),  # "2.0 or higher"
    "tug_workboat": (2.0, 2.0),
}

FENDER_CATALOGUE_COLUMNS = (
    "fender_id",
    "manufacturer",
    "fender_type",
    "diameter_mm",
    "length_mm",
    "internal_pressure_kpa",
    "rated_energy_knm",
    "reaction_kn",
    "hull_pressure_kpa",
    "rated_deflection_pct",
    "source",
)


# --------------------------------------------------------------------------
# Energy
# --------------------------------------------------------------------------


def berthing_energy(
    displacement_t: float,
    velocity_m_s: float,
    ce: float,
    cm: float,
    cs: float = 1.0,
    cc: float = 1.0,
) -> float:
    """Normal berthing energy in kNm (PIANC 2002 section 4.2.1).

    Args:
        displacement_t: ship displacement M, tonnes.
        velocity_m_s: approach velocity perpendicular to the berth, m/s.
        ce, cm, cs, cc: eccentricity, added mass, softness and berth
            configuration factors.
    """
    if displacement_t <= 0:
        raise ValueError("displacement_t must be positive")
    if velocity_m_s < 0:
        raise ValueError("velocity_m_s must not be negative")
    for name, value in (("ce", ce), ("cm", cm), ("cs", cs), ("cc", cc)):
        if value <= 0:
            raise ValueError(f"{name} must be positive")
    return 0.5 * displacement_t * velocity_m_s**2 * ce * cm * cs * cc


def abnormal_impact_factor(vessel_type: str, largest: bool = True) -> float:
    """Factor for abnormal impact, PIANC 2002 Table 4.2.5.

    ``vessel_type`` is one of: tanker_bulk, container, general_cargo,
    ro_ro_ferry, tug_workboat. ``largest`` selects the value for the largest
    (True) or smallest (False) vessel using the berth. PIANC 2002 section
    4.2.8 bounds the derived factor between 1.1 and 2.0 except in exceptional
    circumstances and asks for it to be modified for the berth (fender
    redundancy, berthing frequency, very low design velocities, structure
    vulnerability, vessel range, hazardous cargo).
    """
    largest_value, smallest_value = _ABNORMAL_IMPACT[vessel_type]
    return largest_value if largest else smallest_value


# --------------------------------------------------------------------------
# Hull form, radius of gyration, eccentricity
# --------------------------------------------------------------------------


def block_coefficient(
    displacement_t: float,
    lbp_m: float,
    beam_m: float,
    draft_m: float,
    water_density_t_m3: float = SEAWATER_DENSITY_T_M3,
) -> float:
    """Cb = M / (rho * Lpp * B * T), PIANC 2002 section 4.2.4."""
    if min(displacement_t, lbp_m, beam_m, draft_m, water_density_t_m3) <= 0:
        raise ValueError("all inputs must be positive")
    return displacement_t / (water_density_t_m3 * lbp_m * beam_m * draft_m)


def radius_of_gyration(cb: float, lbp_m: float) -> float:
    """K = (0.19 Cb + 0.11) L in metres, PIANC 2002 section 4.2.4."""
    if cb <= 0 or lbp_m <= 0:
        raise ValueError("cb and lbp_m must be positive")
    return (0.19 * cb + 0.11) * lbp_m


def eccentricity_factor(k_m: float, r_m: float, phi_deg: float = 90.0) -> float:
    """Ce = (K^2 + R^2 cos^2 phi) / (K^2 + R^2), PIANC 2002 section 4.2.4.

    Args:
        k_m: radius of gyration K, m.
        r_m: distance from the contact point to the centre of mass, m.
        phi_deg: angle between the velocity vector and the line from the
            contact point to the centre of mass, degrees. 90 degrees (the
            default) gives Ce = K^2 / (K^2 + R^2).
    """
    if k_m <= 0 or r_m < 0:
        raise ValueError("k_m must be positive and r_m non-negative")
    k2 = k_m**2
    r2 = r_m**2
    return (k2 + r2 * math.cos(math.radians(phi_deg)) ** 2) / (k2 + r2)


def contact_geometry(
    lbp_m: float,
    beam_m: float,
    x_from_bow_m: float,
    berthing_angle_deg: float = 0.0,
) -> tuple[float, float]:
    """Distance R and angle phi for a contact on the hull side.

    The centre of mass is taken at midships on the centreline. The contact is
    on the side facing the berth, ``x_from_bow_m`` aft of the forward
    perpendicular, and the ship approaches bow-in at ``berthing_angle_deg``
    to the berthing line with its velocity perpendicular to that line.

    Returns:
        (R in m, phi in degrees) for :func:`eccentricity_factor`.
    """
    if lbp_m <= 0 or beam_m < 0:
        raise ValueError("lbp_m must be positive and beam_m non-negative")
    if not 0.0 <= x_from_bow_m <= lbp_m:
        raise ValueError("x_from_bow_m must lie on the ship")
    xg = lbp_m / 2.0 - x_from_bow_m  # forward of midships, m
    half_beam = beam_m / 2.0
    r = math.hypot(xg, half_beam)
    if r == 0.0:
        return 0.0, 90.0
    a = math.radians(berthing_angle_deg)
    # component of R along the velocity (perpendicular to the berthing line)
    r_normal = abs(xg) * math.sin(a) + half_beam * math.cos(a)
    phi = math.degrees(math.acos(min(1.0, r_normal / r)))
    return r, phi


# --------------------------------------------------------------------------
# Added mass
# --------------------------------------------------------------------------


def added_mass_factor_ukc(draft_m: float, water_depth_m: float) -> float:
    """Cm from under-keel clearance, PIANC 2002 section 4.2.5 recommendation.

    1.8 for UKC <= 0.1 D, 1.5 for UKC >= 0.5 D, linear in between
    (transverse approach). WG211 keeps this method for alongside berthing.
    """
    if draft_m <= 0:
        raise ValueError("draft_m must be positive")
    ukc = water_depth_m - draft_m
    if ukc <= 0:
        raise ValueError(
            f"no keel clearance: depth {water_depth_m} m does not exceed draft {draft_m} m"
        )
    ratio = ukc / draft_m
    if ratio <= 0.1:
        return 1.8
    if ratio >= 0.5:
        return 1.5
    return 1.8 - 0.3 * (ratio - 0.1) / 0.4


def added_mass_factor_ueda(draft_m: float, beam_m: float, cb: float) -> float:
    """Cm = 1 + pi D / (2 Cb B) (Ueda 1981), PIANC 2002 section 4.2.5."""
    if min(draft_m, beam_m, cb) <= 0:
        raise ValueError("all inputs must be positive")
    return 1.0 + math.pi * draft_m / (2.0 * cb * beam_m)


def added_mass_factor_vasco_costa(draft_m: float, beam_m: float) -> float:
    """Cm = 1 + 2 D / B (Vasco Costa 1964), PIANC 2002 section 4.2.5.

    PIANC states validity for keel clearance above 0.1 D and velocity above
    0.08 m/s.
    """
    if draft_m <= 0 or beam_m <= 0:
        raise ValueError("all inputs must be positive")
    return 1.0 + 2.0 * draft_m / beam_m


# --------------------------------------------------------------------------
# Fender catalogue and selection
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class FenderRating:
    """One catalogue row: rated (base) performance at the rated deflection."""

    fender_id: str
    manufacturer: str
    fender_type: str
    diameter_mm: float | None
    length_mm: float | None
    internal_pressure_kpa: float | None
    rated_energy_knm: float
    reaction_kn: float
    hull_pressure_kpa: float | None
    rated_deflection_pct: float | None
    source: str


@dataclass(frozen=True)
class FenderCandidate:
    """A catalogue fender that meets the energy demand and any limits."""

    fender: FenderRating
    energy_per_fender_knm: float
    characteristic_energy_knm: float
    utilisation: float
    reaction_kn: float
    hull_pressure_kpa: float | None


def _optional_float(text: str) -> float | None:
    text = (text or "").strip()
    return float(text) if text else None


def load_fender_catalogue(path: str | Path) -> list[FenderRating]:
    """Read a fender catalogue CSV with the ``FENDER_CATALOGUE_COLUMNS`` schema.

    Energy in kNm, reaction in kN, pressures in kPa (kN/m2), dimensions in mm,
    deflection in percent. Empty optional cells read as None.
    """
    path = Path(path)
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        missing = [
            c for c in FENDER_CATALOGUE_COLUMNS if c not in (reader.fieldnames or [])
        ]
        if missing:
            raise ValueError(f"{path.name}: missing columns {', '.join(missing)}")
        rows = []
        for row in reader:
            rows.append(
                FenderRating(
                    fender_id=row["fender_id"].strip(),
                    manufacturer=row["manufacturer"].strip(),
                    fender_type=row["fender_type"].strip(),
                    diameter_mm=_optional_float(row["diameter_mm"]),
                    length_mm=_optional_float(row["length_mm"]),
                    internal_pressure_kpa=_optional_float(row["internal_pressure_kpa"]),
                    rated_energy_knm=float(row["rated_energy_knm"]),
                    reaction_kn=float(row["reaction_kn"]),
                    hull_pressure_kpa=_optional_float(row["hull_pressure_kpa"]),
                    rated_deflection_pct=_optional_float(row["rated_deflection_pct"]),
                    source=row["source"].strip(),
                )
            )
    return rows


def select_fenders(
    catalogue: list[FenderRating],
    required_energy_knm: float,
    fenders_in_contact: int = 1,
    energy_tolerance: float = 0.0,
    angular_factor: float = 1.0,
    max_reaction_kn: float | None = None,
    max_hull_pressure_kpa: float | None = None,
) -> list[FenderCandidate]:
    """Catalogue fenders that absorb the required energy, smallest margin first.

    Args:
        catalogue: rows from :func:`load_fender_catalogue`.
        required_energy_knm: design energy to absorb (after any abnormal or
            partial energy factor).
        fenders_in_contact: fenders sharing the energy equally. Reaction and
            hull pressure are per fender and are not divided.
        energy_tolerance: manufacturing tolerance on rated energy as a
            fraction (0.10 for -10 %); the rated energy is reduced by it.
        angular_factor: reduction of rated energy for angular compression.
        max_reaction_kn: per-fender reaction limit (reaction at rated energy).
        max_hull_pressure_kpa: allowable hull pressure.

    Returns:
        Adequate candidates sorted by utilisation, highest (closest to 1) first.
    """
    if required_energy_knm < 0:
        raise ValueError("required_energy_knm must not be negative")
    if fenders_in_contact < 1:
        raise ValueError("fenders_in_contact must be at least 1")
    if not 0.0 <= energy_tolerance < 1.0:
        raise ValueError("energy_tolerance must be in [0, 1)")
    if not 0.0 < angular_factor <= 1.0:
        raise ValueError("angular_factor must be in (0, 1]")

    demand = required_energy_knm / fenders_in_contact
    picks = []
    for f in catalogue:
        capacity = f.rated_energy_knm * (1.0 - energy_tolerance) * angular_factor
        if capacity <= 0 or demand > capacity:
            continue
        if max_reaction_kn is not None and f.reaction_kn > max_reaction_kn:
            continue
        if (
            max_hull_pressure_kpa is not None
            and f.hull_pressure_kpa is not None
            and f.hull_pressure_kpa > max_hull_pressure_kpa
        ):
            continue
        picks.append(
            FenderCandidate(
                fender=f,
                energy_per_fender_knm=demand,
                characteristic_energy_knm=capacity,
                utilisation=demand / capacity,
                reaction_kn=f.reaction_kn,
                hull_pressure_kpa=f.hull_pressure_kpa,
            )
        )
    picks.sort(key=lambda p: -p.utilisation)
    return picks


# --------------------------------------------------------------------------
# Sensitivity tables
# --------------------------------------------------------------------------

ENERGY_INPUTS = (
    "displacement_t",
    "velocity_m_s",
    "ce",
    "cm",
    "cs",
    "cc",
    "abnormal_factor",
)


@dataclass(frozen=True)
class EnergySensitivityRow:
    """Design energy with one input changed from the base case."""

    parameter: str
    value: float
    base_value: float
    design_energy_knm: float
    base_design_energy_knm: float
    change_pct: float
    source: str


def _design_energy(inputs: dict[str, float]) -> float:
    return (
        berthing_energy(
            inputs["displacement_t"],
            inputs["velocity_m_s"],
            ce=inputs["ce"],
            cm=inputs["cm"],
            cs=inputs["cs"],
            cc=inputs["cc"],
        )
        * inputs["abnormal_factor"]
    )


def energy_sensitivity(
    base: dict[str, float],
    ranges: dict[str, list[tuple[float, str]]],
) -> list[EnergySensitivityRow]:
    """Change in design energy when each input moves alone over its range.

    Args:
        base: values for every key in ``ENERGY_INPUTS``. ``abnormal_factor``
            stands for the WG33 abnormal impact factor or a WG211 partial
            energy factor.
        ranges: for each input to vary, a list of (value, source) pairs; the
            source states where the value comes from.

    Returns:
        One row per (input, value), in the order given.
    """
    missing = [k for k in ENERGY_INPUTS if k not in base]
    if missing:
        raise KeyError(f"base is missing {', '.join(missing)}")
    e0 = _design_energy(base)
    rows = []
    for name, values in ranges.items():
        if name not in ENERGY_INPUTS:
            raise KeyError(f"unknown input {name!r}")
        for value, source in values:
            case = dict(base, **{name: value})
            e = _design_energy(case)
            rows.append(
                EnergySensitivityRow(
                    parameter=name,
                    value=value,
                    base_value=base[name],
                    design_energy_knm=e,
                    base_design_energy_knm=e0,
                    change_pct=(e / e0 - 1.0) * 100.0,
                    source=source,
                )
            )
    return rows


@dataclass(frozen=True)
class FenderComparisonRow:
    """A catalogue fender compared with the base choice.

    ``change_pct`` holds the percentage change from the base for
    rated_energy, reaction, deflection (absolute, mm) and hull_pressure, and
    the change of energy margin in percentage points (energy_margin_pp).
    A value is None where either fender lacks the datum.
    """

    fender: FenderRating
    is_base: bool
    deflection_mm: float | None
    energy_margin_pct: float
    change_pct: dict[str, float | None]


def _deflection_mm(f: FenderRating) -> float | None:
    if f.rated_deflection_pct is None or f.diameter_mm is None:
        return None
    return f.rated_deflection_pct / 100.0 * f.diameter_mm


def _pct(value: float | None, ref: float | None) -> float | None:
    if value is None or ref is None or ref == 0:
        return None
    return (value / ref - 1.0) * 100.0


def fender_size_sensitivity(
    catalogue: list[FenderRating],
    base_id: str,
    design_energy_knm: float,
    fender_ids: list[str] | None = None,
) -> list[FenderComparisonRow]:
    """Compare catalogue fenders with the base choice at one design energy.

    Energy margin is rated energy / design energy - 1, in percent. Rated
    deflection for a pneumatic fender is taken as a fraction of its diameter.
    """
    if design_energy_knm <= 0:
        raise ValueError("design_energy_knm must be positive")
    by_id = {f.fender_id: f for f in catalogue}
    base = by_id[base_id]
    ids = fender_ids if fender_ids is not None else [f.fender_id for f in catalogue]
    base_margin = (base.rated_energy_knm / design_energy_knm - 1.0) * 100.0
    rows = []
    for fid in ids:
        f = by_id[fid]
        margin = (f.rated_energy_knm / design_energy_knm - 1.0) * 100.0
        rows.append(
            FenderComparisonRow(
                fender=f,
                is_base=fid == base_id,
                deflection_mm=_deflection_mm(f),
                energy_margin_pct=margin,
                change_pct={
                    "rated_energy": _pct(f.rated_energy_knm, base.rated_energy_knm),
                    "reaction": _pct(f.reaction_kn, base.reaction_kn),
                    "deflection": _pct(_deflection_mm(f), _deflection_mm(base)),
                    "hull_pressure": _pct(f.hull_pressure_kpa, base.hull_pressure_kpa),
                    "energy_margin_pp": margin - base_margin,
                },
            )
        )
    return rows
