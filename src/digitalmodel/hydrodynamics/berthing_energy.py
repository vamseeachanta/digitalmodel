"""Berthing energy of an alongside ship and screening of catalogue fenders.

Method: the deterministic approach of PIANC MarCom WG33, *Guidelines for the
Design of Fender Systems: 2002*:

* section 4.2.1  E = 0.5 * M * v^2 * Ce * Cm * Cs * Cc  (M in t, v in m/s, E in kNm)
* section 4.2.4  Ce = (K^2 + R^2 cos^2 phi) / (K^2 + R^2),  K = (0.19 Cb + 0.11) L
* section 4.2.5  Cm from keel clearance: 1.8 at UKC <= 0.1 D, 1.5 at UKC >= 0.5 D,
                 linear between; the Ueda and Vasco Costa formulae for comparison
* section 4.2.6  Cs (1.0 for soft fenders)
* section 4.2.7  Cc. Cc and the added-mass method go together: with Cm from
                 the keel-clearance rule (``added_mass_factor_ukc``) use
                 Cc = 1.0. A lower Cc for a solid quay applies only with an
                 added-mass method that does not already account for the
                 confined water, and only under near-parallel approach. The
                 caller chooses both; this module cannot infer how Cm was found.
* section 4.2.8  a factor for abnormal impact multiplies the normal energy. The
                 caller supplies it (PIANC 2002 Table 4.2.5 and section 4.2.8
                 are the source to consult); no table values are embedded here.

Scope of WG211 (PIANC MarCom WG211, 2024). WG211 supersedes WG33 with a
semi-probabilistic format: characteristic energy times a partial energy
factor, and fender base performance converted to characteristic and design
performance with velocity, temperature, angular, multiple-contact and partial
resistance factors. This module implements none of those factors. A caller may
pass a WG211 velocity and partial energy factor as plain inputs, but fender
screening against WG211 requires catalogue rows that the caller has already
converted to design performance (energy, reaction and hull pressure), prepared
and documented outside this module. WG211 and WG33 inputs should not be mixed.

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


def _finite(name, value, *, positive=False, nonneg=False):
    """Return ``value`` as float after checking it is finite and of the right sign."""
    if isinstance(value, bool):
        raise ValueError(f"{name} must be a number, not a boolean")
    try:
        x = float(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} must be a number") from exc
    if not math.isfinite(x):
        raise ValueError(f"{name} must be finite, got {value!r}")
    if positive and x <= 0:
        raise ValueError(f"{name} must be positive, got {value!r}")
    if nonneg and x < 0:
        raise ValueError(f"{name} must not be negative, got {value!r}")
    return x


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
            configuration factors. ``cc`` must match the method used for
            ``cm``: with the keel-clearance rule, Cc = 1.0 (see module notes).

    The design energy is this value times a caller-supplied abnormal impact
    factor (WG33) or partial energy factor (WG211).
    """
    m = _finite("displacement_t", displacement_t, positive=True)
    v = _finite("velocity_m_s", velocity_m_s, nonneg=True)
    product = 1.0
    for name, value in (("ce", ce), ("cm", cm), ("cs", cs), ("cc", cc)):
        product *= _finite(name, value, positive=True)
    return 0.5 * m * v**2 * product


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
    m = _finite("displacement_t", displacement_t, positive=True)
    lpp = _finite("lbp_m", lbp_m, positive=True)
    b = _finite("beam_m", beam_m, positive=True)
    t = _finite("draft_m", draft_m, positive=True)
    rho = _finite("water_density_t_m3", water_density_t_m3, positive=True)
    return m / (rho * lpp * b * t)


def radius_of_gyration(cb: float, lbp_m: float) -> float:
    """K = (0.19 Cb + 0.11) L in metres, PIANC 2002 section 4.2.4."""
    cb = _finite("cb", cb, positive=True)
    lbp_m = _finite("lbp_m", lbp_m, positive=True)
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
    k_m = _finite("k_m", k_m, positive=True)
    r_m = _finite("r_m", r_m, nonneg=True)
    phi_deg = _finite("phi_deg", phi_deg)
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
    perpendicular. The velocity is perpendicular to the berthing line.
    ``berthing_angle_deg`` is the angle between the centreline and the
    berthing line: positive with the bow nearer the berth, negative with the
    stern nearer. The signed longitudinal offset is kept, so forward and aft
    contacts at the same orientation give different results.

    Returns:
        (R in m, phi in degrees) for :func:`eccentricity_factor`.
    """
    lbp_m = _finite("lbp_m", lbp_m, positive=True)
    beam_m = _finite("beam_m", beam_m, nonneg=True)
    x_from_bow_m = _finite("x_from_bow_m", x_from_bow_m)
    berthing_angle_deg = _finite("berthing_angle_deg", berthing_angle_deg)
    if not 0.0 <= x_from_bow_m <= lbp_m:
        raise ValueError("x_from_bow_m must lie on the ship")
    xg = lbp_m / 2.0 - x_from_bow_m  # + forward of midships, - aft, m
    half_beam = beam_m / 2.0
    r = math.hypot(xg, half_beam)
    if r == 0.0:
        return 0.0, 90.0
    a = math.radians(berthing_angle_deg)
    # signed component of R along the velocity (perpendicular to the berth);
    # only its square enters Ce, so the magnitude is taken after projecting
    r_normal = abs(xg * math.sin(a) + half_beam * math.cos(a))
    phi = math.degrees(math.acos(min(1.0, r_normal / r)))
    return r, phi


# --------------------------------------------------------------------------
# Added mass
# --------------------------------------------------------------------------


def added_mass_factor_ukc(draft_m: float, water_depth_m: float) -> float:
    """Cm from under-keel clearance, PIANC 2002 section 4.2.5 recommendation.

    1.8 for UKC <= 0.1 D, 1.5 for UKC >= 0.5 D, linear in between
    (transverse approach). Use Cc = 1.0 with a Cm from this rule.
    """
    draft_m = _finite("draft_m", draft_m, positive=True)
    water_depth_m = _finite("water_depth_m", water_depth_m, positive=True)
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
    draft_m = _finite("draft_m", draft_m, positive=True)
    beam_m = _finite("beam_m", beam_m, positive=True)
    cb = _finite("cb", cb, positive=True)
    return 1.0 + math.pi * draft_m / (2.0 * cb * beam_m)


def added_mass_factor_vasco_costa(draft_m: float, beam_m: float) -> float:
    """Cm = 1 + 2 D / B (Vasco Costa 1964), PIANC 2002 section 4.2.5.

    PIANC states validity for keel clearance above 0.1 D and velocity above
    0.08 m/s.
    """
    draft_m = _finite("draft_m", draft_m, positive=True)
    beam_m = _finite("beam_m", beam_m, positive=True)
    return 1.0 + 2.0 * draft_m / beam_m


# --------------------------------------------------------------------------
# Fender catalogue and selection
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class FenderRating:
    """One catalogue row: rated (base) performance at the rated deflection.

    Numeric values must be finite and positive; optional values may be None.
    Rated deflection, when given, must lie in (0, 100] percent.
    """

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

    def __post_init__(self) -> None:
        where = f"fender {self.fender_id!r}"
        for name in ("rated_energy_knm", "reaction_kn"):
            _finite(f"{where} {name}", getattr(self, name), positive=True)
        for name in (
            "diameter_mm",
            "length_mm",
            "internal_pressure_kpa",
            "hull_pressure_kpa",
            "rated_deflection_pct",
        ):
            value = getattr(self, name)
            if value is not None:
                _finite(f"{where} {name}", value, positive=True)
        if self.rated_deflection_pct is not None and self.rated_deflection_pct > 100.0:
            raise ValueError(f"{where} rated_deflection_pct must not exceed 100")


@dataclass(frozen=True)
class FenderCandidate:
    """A catalogue fender that meets the energy demand and any limits.

    ``characteristic_energy_knm`` is the rated energy after the energy
    tolerance and angular factor. ``reaction_kn`` and ``hull_pressure_kpa``
    are the catalogue values raised by the reaction tolerance; the unadjusted
    catalogue values remain on ``fender``.
    """

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
            fid = row["fender_id"].strip()
            try:
                rating = FenderRating(
                    fender_id=fid,
                    manufacturer=row["manufacturer"].strip(),
                    fender_type=row["fender_type"].strip(),
                    diameter_mm=_optional_float(row["diameter_mm"]),
                    length_mm=_optional_float(row["length_mm"]),
                    internal_pressure_kpa=_optional_float(row["internal_pressure_kpa"]),
                    rated_energy_knm=float(row["rated_energy_knm"].strip()),
                    reaction_kn=float(row["reaction_kn"].strip()),
                    hull_pressure_kpa=_optional_float(row["hull_pressure_kpa"]),
                    rated_deflection_pct=_optional_float(row["rated_deflection_pct"]),
                    source=row["source"].strip(),
                )
            except ValueError as exc:
                raise ValueError(f"{path.name}: row {fid!r}: {exc}") from exc
            rows.append(rating)
    return rows


def select_fenders(
    catalogue: list[FenderRating],
    required_energy_knm: float,
    fenders_in_contact: int = 1,
    energy_tolerance: float = 0.0,
    angular_factor: float = 1.0,
    max_reaction_kn: float | None = None,
    max_hull_pressure_kpa: float | None = None,
    reaction_tolerance: float = 0.0,
) -> list[FenderCandidate]:
    """Catalogue fenders that absorb the required energy, smallest margin first.

    Args:
        catalogue: rows from :func:`load_fender_catalogue`.
        required_energy_knm: design energy to absorb (after any abnormal or
            partial energy factor).
        fenders_in_contact: whole number of fenders sharing the energy
            equally. Reaction and hull pressure are per fender and are not
            divided.
        energy_tolerance: lower manufacturing tolerance on rated energy as a
            fraction (0.10 for -10 %); the rated energy is reduced by it.
        angular_factor: reduction of rated energy for angular compression.
        max_reaction_kn: per-fender reaction limit.
        max_hull_pressure_kpa: allowable hull pressure. Rows without a hull
            pressure are excluded when this limit is set, because compliance
            cannot be shown.
        reaction_tolerance: upper manufacturing tolerance on reaction as a
            fraction (0.10 for +10 %), independent of the energy tolerance.
            Reaction and hull pressure are both raised by it (pressure taken
            as proportional to reaction over the same contact area) before the
            limits are checked, and the raised values are returned.

    These are the only fender adjustments applied. Velocity, temperature,
    multiple-contact and partial resistance factors (WG211) are not applied;
    supply rows already converted to design performance where they govern.

    Returns:
        Adequate candidates sorted by utilisation, highest (closest to 1) first.
    """
    total = _finite("required_energy_knm", required_energy_knm, nonneg=True)
    if isinstance(fenders_in_contact, bool) or not isinstance(fenders_in_contact, int):
        raise ValueError("fenders_in_contact must be a whole number")
    if fenders_in_contact < 1:
        raise ValueError("fenders_in_contact must be at least 1")
    e_tol = _finite("energy_tolerance", energy_tolerance, nonneg=True)
    if e_tol >= 1.0:
        raise ValueError("energy_tolerance must be in [0, 1)")
    ang = _finite("angular_factor", angular_factor, positive=True)
    if ang > 1.0:
        raise ValueError("angular_factor must be in (0, 1]")
    r_up = 1.0 + _finite("reaction_tolerance", reaction_tolerance, nonneg=True)
    if max_reaction_kn is not None:
        max_reaction_kn = _finite("max_reaction_kn", max_reaction_kn, positive=True)
    if max_hull_pressure_kpa is not None:
        max_hull_pressure_kpa = _finite(
            "max_hull_pressure_kpa", max_hull_pressure_kpa, positive=True
        )

    demand = total / fenders_in_contact
    picks = []
    for f in catalogue:
        capacity = f.rated_energy_knm * (1.0 - e_tol) * ang
        # skip a capacity that underflows to zero as well as one below demand
        if capacity <= 0.0 or demand > capacity:
            continue
        reaction = f.reaction_kn * r_up
        pressure = None if f.hull_pressure_kpa is None else f.hull_pressure_kpa * r_up
        if max_reaction_kn is not None and reaction > max_reaction_kn:
            continue
        if max_hull_pressure_kpa is not None and (
            pressure is None or pressure > max_hull_pressure_kpa
        ):
            continue
        picks.append(
            FenderCandidate(
                fender=f,
                energy_per_fender_knm=demand,
                characteristic_energy_knm=capacity,
                utilisation=demand / capacity,
                reaction_kn=reaction,
                hull_pressure_kpa=pressure,
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
    """Design energy with one input changed from the base case.

    The first row of every table is the base case: parameter "base", value
    and base_value None, change 0 %.
    """

    parameter: str
    value: float | None
    base_value: float | None
    design_energy_knm: float
    base_design_energy_knm: float
    change_pct: float
    source: str


def _design_energy(inputs: dict[str, float]) -> float:
    factor = _finite("abnormal_factor", inputs["abnormal_factor"], positive=True)
    return (
        berthing_energy(
            inputs["displacement_t"],
            inputs["velocity_m_s"],
            ce=inputs["ce"],
            cm=inputs["cm"],
            cs=inputs["cs"],
            cc=inputs["cc"],
        )
        * factor
    )


def energy_sensitivity(
    base: dict[str, float],
    ranges: dict[str, list[tuple[float, str]]],
) -> list[EnergySensitivityRow]:
    """Change in design energy when each input moves alone over its range.

    Args:
        base: values for every key in ``ENERGY_INPUTS``. ``abnormal_factor``
            stands for the caller-supplied WG33 abnormal impact factor or
            WG211 partial energy factor and must be finite and positive.
        ranges: for each input to vary, a list of (value, source) pairs; the
            source states where the value comes from.

    Returns:
        The base row (0 %), then one row per (input, value) in the order given.

    Raises:
        ValueError: if the base design energy is zero (zero velocity), since a
            percentage change from it is undefined.
    """
    missing = [k for k in ENERGY_INPUTS if k not in base]
    if missing:
        raise KeyError(f"base is missing {', '.join(missing)}")
    e0 = _design_energy(base)
    if e0 == 0.0:
        raise ValueError("base design energy is zero; percentage changes are undefined")
    rows = [
        EnergySensitivityRow(
            parameter="base",
            value=None,
            base_value=None,
            design_energy_knm=e0,
            base_design_energy_knm=e0,
            change_pct=0.0,
            source="base case",
        )
    ]
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
    The base fender is always the first row, whether or not ``fender_ids``
    names it.
    """
    design_energy_knm = _finite("design_energy_knm", design_energy_knm, positive=True)
    by_id = {f.fender_id: f for f in catalogue}
    base = by_id[base_id]
    ids = fender_ids if fender_ids is not None else [f.fender_id for f in catalogue]
    ids = [base_id] + [i for i in ids if i != base_id]
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
