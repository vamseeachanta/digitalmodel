"""Fender design methods: berthing energy by several codes, coefficients with
their sources, fender performance, and one-at-a-time sensitivity sweeps.

Builds on :mod:`digitalmodel.hydrodynamics.berthing_energy` (energy equation,
block coefficient, radius of gyration, eccentricity, contact geometry, three
added-mass formulae, catalogue selection) and adds:

* a source register (:data:`SOURCES`): document, edition, clause and how the
  value was verified: ``full-text`` (read in the document), ``secondary``
  (read in the named document that reproduces it) or ``formula`` (closed form
  of the cited paper, reproduced in the named documents);
* coefficient tables, each tied to a source: berthing velocity (Brolsma
  curves), abnormal impact factor, softness and berth configuration factors,
  hull-pressure guidance;
* berthing energy by method (:func:`energy_by_method`, :data:`METHODS`), each
  result carrying every coefficient and the source of the value used;
* fender performance: factored (design) energy and reaction of a catalogue
  fender, reaction at part energy from a normalised curve;
* one-at-a-time sensitivity sweeps and tornado ranking for charts, each
  sweep limited to the inputs the method uses;
* a citation sidecar on every method result (:class:`Citation` records for
  the standards behind its coefficients), resolved against the wiki at
  calculation time (:data:`CITATION_TARGETS`, :func:`resolve_citations`).

Copyrighted text is not reproduced. Numeric values are tabulated with the
clause they come from; where a value was read in a vendor manual that
reproduces a standard, the source says ``secondary`` and names the manual.
A secondary value should be checked against the primary document before it
is used as a design basis.

Every function returns numbers only; nothing here clears a design. The caller
chooses method, navigation condition and factors and owns their basis.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field, replace
from pathlib import Path

from digitalmodel.citations.schema import Citation, CitationResolutionError, validate_citation
from digitalmodel.hydrodynamics.berthing_energy import (
    SEAWATER_DENSITY_T_M3,
    FenderRating,
    _finite,
    added_mass_factor_ueda,
    added_mass_factor_ukc,
    added_mass_factor_vasco_costa,
    berthing_energy,
    block_coefficient,
    contact_geometry,
    eccentricity_factor,
    radius_of_gyration,
)

# --------------------------------------------------------------------------
# Source register
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Source:
    """A document a value or formula comes from."""

    source_id: str
    title: str
    edition: str
    clause: str
    verification: str
    read_in: str = ""
    note: str = ""
    #: date the source was read for this register: the commit that first
    #: recorded it (15 Nov 2026), or the later re-reading named in the note
    accessed_on: str = "2026-11-15"


SOURCES: dict[str, Source] = {}


def _register(*sources: Source) -> None:
    for s in sources:
        SOURCES[s.source_id] = s


TFADM = "Trelleborg Fender Application Design Manual MN-FEN-V4.0-EN (2024)"
SFTDM = "ShibataFenderTeam Design Manual (English, 2026)"
#: How the PIANC WG33 (2002) report is named where it was read: the primary text.
PIANC2002_PRIMARY = "PIANC WG33 (2002), primary text"
#: Vendor manuals: a coefficient may come from them (a caller-selected option
#: or a reading the primary text does not give); they are named in the
#: result, never cited as a standard (no :data:`CITATION_TARGETS` entry).
VENDOR_SOURCES = ("tfadm-2024", "sft-2026")

_register(
    Source(
        "pianc2002", "PIANC MarCom WG33, Guidelines for the Design of Fender Systems",
        "2002", "section 4.2 (energy), Table 4.2.5 (abnormal factor), Table 4.4.1 (hull pressure)",
        "full-text", f"{PIANC2002_PRIMARY}; also {TFADM} pp. 23-28, 46; {SFTDM} pp. 17-22, 33",
        "primary text read 16 Nov 2026: Table 4.2.5 factors and Table 4.4.1 (bulk carriers below "
        "200 kN/m2) match the values used", accessed_on="2026-11-16",
    ),
    Source(
        "pianc2002-velocity", "PIANC 2002 design berthing velocity (Brolsma et al. 1977 curves a to e)",
        "2002", "Fig. 4.2.1; table keyed by DWT", "secondary", f"{TFADM} p. 23",
        "tabulated values read in the manual; the primary text (Fig. 4.2.1, read 16 Nov 2026) confirms "
        "the keying by DWT and that curves d and e are to be used with caution; values below 0.1 m/s "
        "marked for caution in the manual", accessed_on="2026-11-16",
    ),
    Source(
        "bs6349-4-velocity", "BS 6349-4:2014 design berthing velocity (Brolsma curves)",
        "2014", "Figure 9; keyed by displacement", "secondary", f"{SFTDM} p. 21",
        "speeds below 0.08 m/s not recommended in the manual",
    ),
    Source(
        "bs6349-4", "BS 6349-4:2014 Maritime works. Code of practice for design of fendering and mooring systems",
        "2014", "berthing energy (as summarised)", "secondary", f"{SFTDM} pp. 4, 18, 21-22; {TFADM} pp. 15, 23",
    ),
    Source(
        "vasco-costa-1964", "Vasco Costa, F. The berthing ship", "1964",
        "Cm = 1 + 2D/B; valid for v >= 0.08 m/s and UKC >= 0.1 D", "formula",
        f"{TFADM} p. 24; {SFTDM} p. 18; {PIANC2002_PRIMARY} section 4.2.5",
        "form and validity limits match the PIANC 2002 primary text (read 16 Nov 2026)",
        accessed_on="2026-11-16",
    ),
    Source(
        "ueda-1981", "Ueda, S. et al. added mass of berthing ships", "1981",
        "Cm = 1 + pi D / (2 Cb B)", "formula",
        f"{TFADM} p. 24; {SFTDM} p. 18; {PIANC2002_PRIMARY} section 4.2.5",
        "form matches the PIANC 2002 primary text (read 16 Nov 2026)", accessed_on="2026-11-16",
    ),
    Source(
        "pianc2002-cm", "PIANC 2002 added mass from keel clearance", "2002", "section 4.2.5",
        "full-text", f"{PIANC2002_PRIMARY}; also {SFTDM} p. 18; {TFADM} p. 24",
        "1.8 at KC/D <= 0.1; 1.875 - 0.75 KC/D between; 1.5 at KC/D >= 0.5; longitudinal approach 1.1. "
        "Cc 1.0 with this Cm is the manuals' rule (TFADM p. 27, SFT p. 18), not a clause of 4.2.7",
        accessed_on="2026-11-16",
    ),
    Source(
        "pianc2002-ce", "PIANC 2002 eccentricity", "2002", "section 4.2.4",
        "full-text", f"{PIANC2002_PRIMARY}; also {TFADM} p. 25; {SFTDM} p. 19",
        "Ce = (K^2 + R^2 cos^2 phi)/(K^2 + R^2); K = (0.19 Cb + 0.11) LBP", accessed_on="2026-11-16",
    ),
    Source(
        "pianc2002-cc", "PIANC 2002 berth configuration factor", "2002", "section 4.2.7, p. 22",
        "full-text", f"{PIANC2002_PRIMARY} p. 22",
        "1.0 for open berths and corners of quay walls; solid quay walls under parallel approach "
        "'generally taken as 0.9'; the cushion effect disappears above a 5 deg approach angle. No "
        "partly closed class and no 0.8 value (those are the SFT table, option cc_basis 'sft-2026')",
        accessed_on="2026-11-16",
    ),
    Source(
        "pianc2002-cs", "PIANC 2002 softness factor", "2002", "section 4.2.6, p. 21",
        "full-text", f"{PIANC2002_PRIMARY} p. 21",
        "1.0 for soft fenders and smaller vessels, 0.9 to 1.0 for hard fenders and larger vessels; "
        "concludes a general value Cs = 1.0. The 0.15 m deflection rule is BS 6349-4's definition of a "
        "hard fender, applied as Cs 0.9 by the manuals (option cs_basis 'tfadm-2024')",
        accessed_on="2026-11-16",
    ),
    Source("tfadm-2024", TFADM, "2024", "pp. 22-46", "full-text"),
    Source("sft-2026", SFTDM, "2026", "pp. 17-36", "full-text"),
    Source(
        "iso17357-1", "ISO 17357-1:2014 Ships and marine technology. Floating pneumatic rubber fenders",
        "2014", "performance at 60 % deflection (GEA)", "secondary",
        "Trelleborg Pneumatic Fenders BC-PNE-V6-EN (2025) pp. 8-9; Yokohama floating fender web table",
    ),
)

# --------------------------------------------------------------------------
# Ship and berth
# --------------------------------------------------------------------------

SHIP_TYPES = (
    "bulk", "tanker", "container", "general_cargo", "roro", "ferry",
    "passenger", "gas", "car_carrier", "tug",
)

STRUCTURES = ("open", "semi_closed", "closed")


@dataclass(frozen=True)
class Ship:
    """Design ship at the berthing condition.

    ``draft_m`` is the draught at berthing (never the moulded depth).
    ``displacement_t`` is the displacement at that draught. ``dwt_t`` is used
    where a table is keyed by deadweight.
    """

    displacement_t: float
    lbp_m: float
    beam_m: float
    draft_m: float
    ship_type: str = "bulk"
    dwt_t: float | None = None
    loa_m: float | None = None
    water_density_t_m3: float = SEAWATER_DENSITY_T_M3

    def __post_init__(self) -> None:
        for name in ("displacement_t", "lbp_m", "beam_m", "draft_m", "water_density_t_m3"):
            _finite(name, getattr(self, name), positive=True)
        for name in ("dwt_t", "loa_m"):
            if getattr(self, name) is not None:
                _finite(name, getattr(self, name), positive=True)
        if self.ship_type not in SHIP_TYPES:
            raise ValueError(f"ship_type must be one of {SHIP_TYPES}")
        # plausibility: a block coefficient above 1 is a hull larger than its own box
        cb = self.cb
        if cb > 1.0:
            raise ValueError(
                f"block coefficient M/(rho LBP B T) = {cb:.3f} is above 1: check displacement, LBP, beam "
                "and draught (the draught at berthing, not the moulded depth)")

    @property
    def cb(self) -> float:
        return block_coefficient(
            self.displacement_t, self.lbp_m, self.beam_m, self.draft_m, self.water_density_t_m3
        )


@dataclass(frozen=True)
class Berth:
    """Berth and approach.

    ``contact_x_from_bow_m``: first-contact point, aft of the forward
    perpendicular. ``berthing_angle_deg``: between the ship's centreline and
    the berthing line, positive with the bow nearer the berth, negative with
    the stern nearer (:func:`~digitalmodel.hydrodynamics.berthing_energy.contact_geometry`),
    below 90 deg in magnitude; the first contact must be on the end nearer
    the berth (:func:`contact_offset_m`). ``navigation``: Brolsma condition ``a`` to ``e``.
    ``structure``: ``open`` (piled or floating; water passes beneath),
    ``semi_closed`` or ``closed`` (solid quay). ``fender_deflection_m``:
    rated deflection of the fender, for the softness factor.
    """

    water_depth_m: float
    contact_x_from_bow_m: float
    berthing_angle_deg: float = 0.0
    navigation: str = "b"
    structure: str = "open"
    fender_deflection_m: float = 1.0

    def __post_init__(self) -> None:
        _finite("water_depth_m", self.water_depth_m, positive=True)
        _finite("contact_x_from_bow_m", self.contact_x_from_bow_m, nonneg=True)
        if not abs(_finite("berthing_angle_deg", self.berthing_angle_deg)) < 90.0:
            raise ValueError(f"berthing angle {self.berthing_angle_deg:g} deg must be below 90 deg in magnitude")
        _finite("fender_deflection_m", self.fender_deflection_m, positive=True)
        if self.navigation not in BROLSMA_CONDITIONS:
            raise ValueError(f"navigation must be one of {tuple(BROLSMA_CONDITIONS)}")
        if self.structure not in STRUCTURES:
            raise ValueError(f"structure must be one of {STRUCTURES}")


# --------------------------------------------------------------------------
# Berthing velocity: Brolsma curves
# --------------------------------------------------------------------------

BROLSMA_CONDITIONS = {
    "a": "easy berthing, sheltered",
    "b": "difficult berthing, sheltered",
    "c": "easy berthing, exposed",
    "d": "good berthing, exposed",
    "e": "difficult berthing, exposed",
}

#: Design berthing velocity (m/s) for tug-assisted berthing, conditions a to e,
#: by ship size (t). The PIANC 2002 table is keyed by DWT (read in TFADM p. 23);
#: the BS 6349-4 Figure 9 table by displacement (read in SFT p. 21). Values
#: agree to three decimals on every row printed in both; rows printed by one
#: manual only are included. Each condition holds its own (size, velocity)
#: points because the manuals omit low values on different rows.
BROLSMA_TABLE: dict[str, tuple[tuple[float, float], ...]] = {
    "a": ((1_000, 0.179), (2_000, 0.151), (3_000, 0.136), (4_000, 0.125), (5_000, 0.117),
          (10_000, 0.094), (15_000, 0.082), (20_000, 0.074), (30_000, 0.064), (40_000, 0.057),
          (50_000, 0.052), (100_000, 0.039), (200_000, 0.028), (300_000, 0.022),
          (400_000, 0.019), (500_000, 0.017)),
    "b": ((1_000, 0.343), (2_000, 0.296), (3_000, 0.269), (4_000, 0.250), (5_000, 0.236),
          (10_000, 0.192), (15_000, 0.169), (20_000, 0.153), (30_000, 0.133), (40_000, 0.119),
          (50_000, 0.110), (75_000, 0.094), (100_000, 0.083), (200_000, 0.062),
          (300_000, 0.052), (400_000, 0.045), (500_000, 0.041)),
    "c": ((1_000, 0.517), (2_000, 0.445), (3_000, 0.404), (4_000, 0.374), (5_000, 0.352),
          (10_000, 0.287), (15_000, 0.252), (20_000, 0.228), (30_000, 0.198), (40_000, 0.178),
          (50_000, 0.164), (75_000, 0.141), (100_000, 0.126), (150_000, 0.107),
          (200_000, 0.095), (250_000, 0.086), (300_000, 0.080), (400_000, 0.071),
          (500_000, 0.064)),
    "d": ((1_000, 0.669), (2_000, 0.577), (3_000, 0.524), (4_000, 0.487), (5_000, 0.459),
          (10_000, 0.377), (15_000, 0.332), (20_000, 0.303), (30_000, 0.264), (40_000, 0.239),
          (50_000, 0.221), (75_000, 0.190), (100_000, 0.171), (150_000, 0.146),
          (200_000, 0.131), (250_000, 0.120), (300_000, 0.111), (400_000, 0.099),
          (500_000, 0.090)),
    "e": ((1_000, 0.865), (2_000, 0.726), (3_000, 0.649), (4_000, 0.597), (5_000, 0.558),
          (10_000, 0.448), (15_000, 0.391), (20_000, 0.355), (30_000, 0.308), (40_000, 0.279),
          (50_000, 0.258), (75_000, 0.223), (100_000, 0.201), (150_000, 0.174),
          (200_000, 0.158), (250_000, 0.146), (300_000, 0.137), (400_000, 0.124),
          (500_000, 0.115)),
}

#: Below these velocities the manuals advise caution (TFADM p. 23: 0.10 m/s)
#: or do not recommend the value (SFT p. 21: 0.08 m/s).
VELOCITY_CAUTION_M_S = 0.10
VELOCITY_NOT_RECOMMENDED_M_S = 0.08


def _loglog(x, pts):
    """Interpolate linearly in log(size) and log(velocity)."""
    xs = [p[0] for p in pts]
    if not xs[0] <= x <= xs[-1]:
        raise ValueError(f"size {x:g} t outside the tabulated {xs[0]:g} to {xs[-1]:g} t")
    for (x0, y0), (x1, y1) in zip(pts, pts[1:]):
        if x0 <= x <= x1:
            t = (math.log(x) - math.log(x0)) / (math.log(x1) - math.log(x0))
            return math.exp(math.log(y0) + t * (math.log(y1) - math.log(y0)))
    raise AssertionError("unreachable")


def brolsma_velocity(size_t: float, condition: str) -> float:
    """Design berthing velocity (m/s) from the Brolsma curves.

    ``size_t`` is DWT under PIANC 2002 or displacement under BS 6349-4 (the
    two manuals print the same numbers against different measures; the
    method that calls this chooses). Log-log interpolation between rows; no
    extrapolation outside 1,000 to 500,000 t.
    """
    s = _finite("size_t", size_t, positive=True)
    if condition not in BROLSMA_TABLE:
        raise ValueError(f"condition must be one of {tuple(BROLSMA_TABLE)}")
    return _loglog(s, BROLSMA_TABLE[condition])


def velocity_flags(velocity_m_s: float) -> list[str]:
    """Advisory flags for a low design velocity (see the module constants)."""
    v = _finite("velocity_m_s", velocity_m_s, positive=True)
    flags = []
    if v < VELOCITY_NOT_RECOMMENDED_M_S:
        flags.append("below 0.08 m/s: not recommended (SFT p. 21)")
    elif v < VELOCITY_CAUTION_M_S:
        flags.append("below 0.10 m/s: use with caution (TFADM p. 23)")
    return flags


# --------------------------------------------------------------------------
# Coefficients
# --------------------------------------------------------------------------


def added_mass_factor(ship: Ship, water_depth_m: float, method: str) -> float:
    """Cm by ``method``: ``pianc2002`` (keel clearance), ``vasco_costa``,
    ``ueda`` or ``longitudinal`` (1.1, longitudinal approach, TFADM p. 24)."""
    if method == "pianc2002":
        return added_mass_factor_ukc(ship.draft_m, water_depth_m)
    if method == "vasco_costa":
        return added_mass_factor_vasco_costa(ship.draft_m, ship.beam_m)
    if method == "ueda":
        return added_mass_factor_ueda(ship.draft_m, ship.beam_m, ship.cb)
    if method == "longitudinal":
        return 1.1
    raise ValueError(f"unknown added-mass method {method!r}")


def contact_offset_m(ship: Ship, berth: Berth) -> float:
    """Distance of the first contact forward of mid-LBP (m; negative aft).

    The one check that the contact lies on the ship, used by every method's
    eccentricity: a contact point outside 0 to LBP from the forward
    perpendicular is refused, so no method returns an energy for it. The
    berthing angle must agree with it: with the bow nearer the berth
    (angle above 0) the first contact is forward of mid-LBP or at it, with
    the stern nearer (below 0) aft of it or at it. The other sign would
    put the first contact on the end further from the berth and lowers
    Ce (down to -14 % for the generic ship), so it is refused.
    """
    x = berth.contact_x_from_bow_m
    if not 0.0 <= x <= ship.lbp_m:
        raise ValueError(f"first contact {x:g} m from the bow is not on the ship (0 to LBP {ship.lbp_m:g} m)")
    arm = ship.lbp_m / 2.0 - x
    a = berth.berthing_angle_deg
    if (a > 0.0 and arm < 0.0) or (a < 0.0 and arm > 0.0):
        raise ValueError(
            f"berthing angle {a:g} deg puts the {'bow' if a > 0 else 'stern'} nearer the berth, but the first "
            f"contact {x:g} m from the bow is {'aft' if arm < 0 else 'forward'} of mid-LBP: the first contact "
            "is on the end nearer the berth (angle above 0: forward half; below 0: aft half)")
    return arm


def eccentricity(ship: Ship, berth: Berth) -> dict[str, float]:
    """K, R, phi and Ce (PIANC 2002 section 4.2.4) for the berth's first contact.

    Centre of mass at mid-LBP on the centreline; phi between the velocity
    (normal to the berth) and the line from contact to centre of mass. A
    contact off the ship is refused (:func:`contact_offset_m`).
    """
    contact_offset_m(ship, berth)
    cb = ship.cb
    k = radius_of_gyration(cb, ship.lbp_m)
    r, phi = contact_geometry(ship.lbp_m, ship.beam_m, berth.contact_x_from_bow_m, berth.berthing_angle_deg)
    return dict(cb=cb, k_m=k, r_m=r, phi_deg=phi, ce=eccentricity_factor(k, r, phi))


def eccentricity_k2_numerator(k_m: float, r_m: float, phi_deg: float) -> float:
    """The erroneous form (K^2 + K^2 cos^2 phi)/(K^2 + R^2), kept to quantify
    the error found in spreadsheets; never use it for design."""
    k2 = _finite("k_m", k_m, positive=True) ** 2
    r2 = _finite("r_m", r_m, nonneg=True) ** 2
    return (k2 + k2 * math.cos(math.radians(phi_deg)) ** 2) / (k2 + r2)


#: Bases of the softness factor Cs: ``pianc2002`` (default) is the PIANC WG33
#: (2002) 4.2.6 general value 1.0 (p. 21); ``tfadm-2024`` is the manuals' rule
#: (TFADM p. 27, SFT p. 20): 0.9 for a fender deflecting 0.15 m or less (BS
#: 6349-4's definition of a hard fender), else 1.0. The caller selects it.
CS_BASES = ("pianc2002", "tfadm-2024")

#: Bases of the berth configuration factor Cc: ``pianc2002`` (default) gives
#: only the values PIANC WG33 (2002) 4.2.7 (p. 22) prints (open berth or
#: corner 1.0, solid quay under parallel approach 0.9, above 5 deg 1.0);
#: ``sft-2026`` is the ShibataFenderTeam table (SFT p. 20: solid quay 0.8 or
#: 0.9 and partly closed 0.9 or 1.0 by KC/D), selected by the caller.
CC_BASES = ("pianc2002", "sft-2026")

#: Note on the partly closed Cc under the PIANC basis, which prints no value for it.
CC_PARTLY_CLOSED_ASSUMED = ("ASSUMED - to be confirmed: PIANC 2002 4.2.7 gives no partly closed value; "
                            "1.0 (no reduction, the open-berth value) taken")


def softness(fender_deflection_m: float, basis: str = "pianc2002") -> Coefficient:
    """Cs with its source under ``basis`` (:data:`CS_BASES`)."""
    d = _finite("fender_deflection_m", fender_deflection_m, positive=True)
    if basis not in CS_BASES:
        raise ValueError(f"cs_basis must be one of {CS_BASES}, got {basis!r}")
    if basis == "pianc2002":
        return Coefficient("cs", 1.0, "pianc2002-cs",
                           f"general value 1.0 (PIANC 2002 4.2.6, p. 21); fender deflection {d:g} m")
    return Coefficient("cs", 1.0 if d > 0.15 else 0.9, "tfadm-2024",
                       f"0.9 at or below 0.15 m deflection (TFADM p. 27, SFT p. 20; option cs_basis "
                       f"'tfadm-2024'); fender deflection {d:g} m")


def softness_factor(fender_deflection_m: float, basis: str = "pianc2002") -> float:
    """Cs under ``basis`` (:data:`CS_BASES`): 1.0, the PIANC 2002 4.2.6 general
    value (default), or the manuals' 0.9 at or below 0.15 m deflection."""
    return softness(fender_deflection_m, basis).value


def berth_configuration(
    structure: str, ukc_over_draft: float, berthing_angle_deg: float, cm_method: str,
    basis: str = "pianc2002",
) -> Coefficient:
    """Cc with its source under ``basis`` (:data:`CC_BASES`).

    Open berths and an approach angle above 5 deg: 1.0 (PIANC 2002 4.2.7,
    p. 22). With the PIANC keel-clearance Cm the confined-water effect is
    already in Cm, so Cc = 1.0: this pairing is the manuals' rule (TFADM
    p. 27 "usually assumed", SFT p. 18), not a clause of 4.2.7. Otherwise,
    at 5 deg or less: under ``pianc2002`` (default) a solid quay takes 0.9
    and a partly closed berth 1.0 (assumed: PIANC gives no value for it);
    under ``sft-2026`` the SFT p. 20 table: solid quay 0.8 (KC/D <= 0.5) or
    0.9, partly closed 0.9 or 1.0.
    """
    if structure not in STRUCTURES:
        raise ValueError(f"structure must be one of {STRUCTURES}")
    if basis not in CC_BASES:
        raise ValueError(f"cc_basis must be one of {CC_BASES}, got {basis!r}")
    ratio = _finite("ukc_over_draft", ukc_over_draft)
    angle = abs(_finite("berthing_angle_deg", berthing_angle_deg))
    where = f"{structure} structure, angle {berthing_angle_deg:g} deg"
    if structure == "open" or angle > 5.0:
        return Coefficient("cc", 1.0, "pianc2002-cc",
                           f"{where}: open berth or angle above 5 deg, 1.0 (PIANC 2002 4.2.7, p. 22)")
    if cm_method == "pianc2002":
        return Coefficient("cc", 1.0, "tfadm-2024",
                           f"{where}: 1.0 with the keel-clearance Cm (TFADM p. 27 'usually assumed'; SFT p. 18)")
    if basis == "pianc2002":
        if structure == "closed":
            return Coefficient("cc", 0.9, "pianc2002-cc",
                               f"{where}: solid quay, parallel approach, 'generally 0.9' (PIANC 2002 4.2.7, p. 22)")
        return Coefficient("cc", 1.0, "pianc2002-cc", f"{where}: {CC_PARTLY_CLOSED_ASSUMED}")
    if structure == "closed":
        v = 0.8 if ratio <= 0.5 else 0.9
    else:
        v = 0.9 if ratio <= 0.5 else 1.0
    return Coefficient("cc", v, "sft-2026",
                       f"{where}, KC/D {ratio:.3f}: SFT p. 20 table (option cc_basis 'sft-2026')")


def berth_configuration_factor(
    structure: str, ukc_over_draft: float, berthing_angle_deg: float, cm_method: str,
    basis: str = "pianc2002",
) -> float:
    """Cc under ``basis`` (:data:`CC_BASES`); see :func:`berth_configuration`."""
    return berth_configuration(structure, ukc_over_draft, berthing_angle_deg, cm_method, basis).value


#: PIANC 2002 Table 4.2.5 abnormal impact factor (largest, smallest ship of
#: the type), as read in TFADM p. 28 and SFT p. 22. Gas carriers, car
#: carriers and cruise ships are SFT additions with "no PIANC guidance".
ABNORMAL_FACTOR: dict[str, tuple[float, float, str]] = {
    "tanker": (1.25, 1.75, "PIANC 2002 Table 4.2.5"),
    "bulk": (1.25, 1.75, "PIANC 2002 Table 4.2.5"),
    "container": (1.5, 2.0, "PIANC 2002 Table 4.2.5"),
    "general_cargo": (1.75, 1.75, "PIANC 2002 Table 4.2.5"),
    "roro": (2.0, 2.0, "PIANC 2002 Table 4.2.5 (>= 2.0)"),
    "ferry": (2.0, 2.0, "PIANC 2002 Table 4.2.5 (>= 2.0)"),
    "tug": (2.0, 2.0, "PIANC 2002 Table 4.2.5"),
    "gas": (1.5, 2.0, "SFT p. 22 (no PIANC guidance)"),
    "car_carrier": (2.0, 2.0, "SFT p. 22 (no PIANC guidance)"),
    "passenger": (2.0, 2.0, "SFT p. 22 cruise ships (no PIANC guidance)"),
}


def abnormal_factor(ship_type: str, position: str = "smallest") -> float:
    """PIANC 2002 abnormal impact factor for the largest or smallest ships.

    Neither manual gives a rule between the two; ``position`` is the
    caller's choice. "Largest" in SFT p. 22 is by class (Capesize and above
    for bulk carriers, Suezmax and above for tankers; Handymax and smaller
    take the smallest-ship value).
    """
    if ship_type not in ABNORMAL_FACTOR:
        raise ValueError(f"no abnormal factor for {ship_type!r}")
    largest, smallest, _ = ABNORMAL_FACTOR[ship_type]
    if position == "largest":
        return largest
    if position == "smallest":
        return smallest
    raise ValueError("position must be 'largest' or 'smallest'")


#: PIANC 2002 Table 4.4.1 allowable hull pressure (kPa), as read in TFADM
#: p. 46: (ship type, size note, upper value). Lower bound where a range is
#: printed is in the note.
HULL_PRESSURE_KPA: tuple[tuple[str, str, float], ...] = (
    ("container", "1st/2nd generation (< 1,000 TEU)", 400.0),
    ("container", "3rd generation (< 3,000 TEU)", 300.0),
    ("container", "4th generation (< 8,000 TEU)", 250.0),
    ("container", "5th/6th generation (> 8,000 TEU)", 200.0),
    ("general_cargo", "<= 20,000 DWT (400 to 700)", 700.0),
    ("general_cargo", "> 20,000 DWT", 400.0),
    ("tanker", "<= 20,000 DWT", 250.0),
    ("tanker", "<= 60,000 DWT", 300.0),
    ("tanker", "> 60,000 DWT (VLCC 150 to 200)", 200.0),
    ("gas", "LNG/LPG", 200.0),
    ("bulk", "all sizes", 200.0),
)


def hull_pressure_limit(ship_type: str) -> list[tuple[str, float]]:
    """(size note, kPa) rows of the PIANC 2002 hull-pressure guidance for a type."""
    return [(note, p) for t, note, p in HULL_PRESSURE_KPA if t == ship_type]


# --------------------------------------------------------------------------
# Energy by method
# --------------------------------------------------------------------------

R24A = ("Roubos, Iversen, Oskamp (2024) Backgrounds partial energy factors for fender design, "
        "35th PIANC World Congress, pp. 959-966")
R24B = ("Roubos, Mirihagalla, Gaal, Blankers, Groenewegen (2024) Comparison of fender dimensions, "
        "PIANC WG211 and PIANC WG33, 35th PIANC World Congress, pp. 973-982")

_register(
    Source(
        "pianc-wg211", "PIANC MarCom WG211, Guidelines for the Design of Fender Systems", "2024",
        "semi-probabilistic format: E_k,d = gamma_E E_k,c; E_f,d = E_f,c / gamma_m; Cs and Cc removed",
        "secondary", f"{R24A} (Eq. 3-18, Tables 3, 8, 9, 12); {R24B} (Tables 1-4)",
        "Table 8 and 9 values are the background paper's derivation 'proposed by WG211'; the final "
        "guideline tables were not read. Column B of Table 8 is partly illegible in the paper.",
    ),
    Source(
        "ufc-4-152-01", "UFC 4-152-01 Design: Piers and Wharves", "2017, Change 1 2024",
        "5-2.1 (Eq. 5-1, 5-2; Ce, Cg, Cd, Cc, Cm), 5-1.5.3 (+50 % accidental), Figs. 5-3 and 5-4",
        "full-text", "WBDG copy",
        "velocity figures digitized from the gridlines (about +/-0.01 ft/s), both figures re-read 16 Nov "
        "2026; overlap 5,000 to 20,000 LT: the larger of Figs. 5-3 and 5-4", accessed_on="2026-11-16",
    ),
    Source(
        "rom-2.0-11", "ROM 2.0-11 Recomendaciones para el proyecto y ejecucion en obras de atraque y amarre",
        "June 2012", "4.6.4.4.3 (energy, Cm, Ce, Cg, Cc, Cs), Table 4.6.4.36 (velocity with tugs), "
        "Table 4.6.4.43 (accidental factors), Table 4.6.4.42 (hull pressure)",
        "full-text", "Puertos del Estado PDF (archived copy, Spanish)",
        "velocity table digitized from the figures (about +/-0.02 m/s)",
    ),
    Source(
        "eau-2012", "EAU 2012 Recommendations of the Committee for Waterfront Structures (9th English ed.)",
        "2012", "R 38, R 40 (berthing velocity), R 60 (fenders for large vessels)",
        "secondary", "Roubos et al. (2017) Eq. 8-12 and Fig. 1; SFT p. 18, 22",
        "velocities digitized from Roubos et al. (2017) Fig. 1 at two sizes only",
    ),
    Source(
        "is-4651-3-1974", "IS 4651 (Part III):1974 Code of practice for planning and design of ports and harbours, Loading",
        "1974 (withdrawn; superseded 2020)", "5.2.1 (energy, Cm, Ce, Cs), Table 2 (velocity), 5.2.2",
        "full-text", "BIS scan", "withdrawn standard, kept for comparison only",
    ),
)

#: Navigation / climate condition used by WG211, ROM, UFC, EAU and IS 4651
#: (each maps it to its own classes; the mapping is stated in the result).
CONDITIONS = ("monitored", "favourable", "moderate", "unfavourable")

#: Readings of the Cb inside the UFC 4-152-01 added-mass factor F (5-2.1.1 e,
#: p. 109). The first, the default, follows the printed wording ("Cb =
#: berthing coefficient as defined previously", Ce Cg Cd Cc of Eq. 5-2); the
#: second reads the 0.6 threshold as a hull-form limit on the block
#: coefficient. The source text does not settle which is meant.
UFC_F_BASES = ("berthing_coefficient", "block_coefficient")


@dataclass(frozen=True)
class Coefficient:
    """One coefficient of a method result, with its value and source."""

    name: str
    value: float
    source: str
    note: str = ""


@dataclass(frozen=True)
class MethodResult:
    """Berthing energy by one method, every coefficient with its source.

    ``normal_energy_knm`` = 0.5 M v^2 Cm Ce Cs Cc x product(``extra``).
    ``design_energy_knm`` = normal x ``factor`` (abnormal impact factor,
    accidental factor or WG211 partial energy factor). ``capacity_divisor``
    is the factor the fender's characteristic energy is divided by before it
    is compared (WG211 gamma_m; 1.0 elsewhere).

    ``citations`` is the sidecar: one :class:`Citation` per standard or paper
    a coefficient comes from (values given by the caller carry none).
    ``citation_status`` pairs each citation's ``code_id`` with ``resolved``,
    ``unresolved: page_missing``, ``unresolved: resolver_unconfigured`` or
    ``not checked``; an unresolved citation also adds a flag.
    """

    method: str
    label: str
    velocity_m_s: Coefficient
    cm: Coefficient
    ce: Coefficient
    cs: Coefficient
    cc: Coefficient
    factor: Coefficient
    normal_energy_knm: float
    design_energy_knm: float
    geometry: dict[str, float]
    extra: tuple[Coefficient, ...] = ()
    capacity_divisor: float = 1.0
    flags: tuple[str, ...] = ()
    citations: tuple[Citation, ...] = field(default=(), repr=False)
    citation_status: tuple[tuple[str, str], ...] = ()

    def coefficients(self) -> list[Coefficient]:
        return [self.velocity_m_s, self.cm, self.ce, self.cs, self.cc, *self.extra, self.factor]

    @property
    def citations_resolved(self) -> bool:
        """True when every citation of the result resolved against the wiki."""
        return bool(self.citation_status) and all(s == "resolved" for _, s in self.citation_status)


@dataclass(frozen=True)
class Case:
    """Berthing case: ship, berth and the conditions the methods need.

    ``condition`` is favourable, moderate, unfavourable or monitored (WG211
    only). ``consequence_class`` (A to D), ``cov_m`` (high, moderate, low:
    spread of displacements using the berth) and ``multiple_contact`` enter
    the WG211 partial energy factor. ``tugs`` and ``berthings_per_year`` enter
    the ROM accidental factor and the WG211 frequency correction.
    ``ufc_f_basis`` chooses the reading of the Cb inside the UFC 4-152-01
    added-mass factor F (one of :data:`UFC_F_BASES`; see
    :func:`ufc_added_mass`). ``cs_basis`` and ``cc_basis`` choose the
    softness and berth configuration factors of PIANC 2002, Ueda, BS 6349-4
    and EAU (:data:`CS_BASES`, :data:`CC_BASES`): the PIANC 2002 values by
    default, a vendor manual's table only when selected.

    ``cov_m`` defaults to ``low``, the column of R24A Table 8 with the
    largest factor: a smaller factor (``moderate``, ``high``) needs a basis,
    the spread of displacements of the ships using the berth.
    """

    ship: Ship
    berth: Berth
    condition: str = "moderate"
    tugs: bool = True
    berthings_per_year: float = 100.0
    consequence_class: str = "B"
    cov_m: str = "low"
    multiple_contact: bool = False
    continuous_berth: bool = True
    ufc_f_basis: str = "berthing_coefficient"
    cs_basis: str = "pianc2002"
    cc_basis: str = "pianc2002"

    def __post_init__(self) -> None:
        if self.condition not in CONDITIONS:
            raise ValueError(f"condition must be one of {CONDITIONS}")
        if self.ufc_f_basis not in UFC_F_BASES:
            raise ValueError(f"ufc_f_basis must be one of {UFC_F_BASES}, got {self.ufc_f_basis!r}")
        if self.cs_basis not in CS_BASES:
            raise ValueError(f"cs_basis must be one of {CS_BASES}, got {self.cs_basis!r}")
        if self.cc_basis not in CC_BASES:
            raise ValueError(f"cc_basis must be one of {CC_BASES}, got {self.cc_basis!r}")
        if self.consequence_class not in "ABCD" or len(self.consequence_class) != 1:
            raise ValueError("consequence_class must be A, B, C or D")
        if self.cov_m not in ("high", "moderate", "low"):
            raise ValueError("cov_m must be high, moderate or low")
        _finite("berthings_per_year", self.berthings_per_year, positive=True)
        # yes/no choices are booleans: a truthy string such as "no" must not read as yes
        for name in ("tugs", "multiple_contact", "continuous_berth"):
            if not isinstance(getattr(self, name), bool):
                raise ValueError(f"{name} must be true or false, got {getattr(self, name)!r}")


def _ukc_ratio(ship: Ship, berth: Berth) -> float:
    ukc = berth.water_depth_m - ship.draft_m
    if ukc <= 0:
        raise ValueError(
            f"no keel clearance: depth {berth.water_depth_m} m does not exceed draft {ship.draft_m} m"
        )
    return ukc / ship.draft_m


#: Plausibility range of the block coefficient: outside it a result is
#: flagged (above 1 the ship is refused). ASSUMED - to be confirmed: the span
#: of merchant hull forms, not a value from the cited standards.
CB_PLAUSIBLE = (0.45, 0.90)


def cb_flags(cb: float) -> list[str]:
    """Flag for a block coefficient outside :data:`CB_PLAUSIBLE`."""
    lo, hi = CB_PLAUSIBLE
    if lo <= cb <= hi:
        return []
    return [f"block coefficient {cb:.3f} outside {lo:g} to {hi:g} (plausibility range, ASSUMED - to be "
            "confirmed): check displacement, LBP, beam and draught"]


def _result(method, label, case, v, cm, ce, cs, cc, factor, geo, *, extra=(), divisor=1.0, flags=()):
    product = 1.0
    for c in extra:
        product *= c.value
    normal = berthing_energy(case.ship.displacement_t, v.value, ce.value, cm.value, cs.value, cc.value) * product
    flags = list(dict.fromkeys(list(flags) + cb_flags(case.ship.cb))) + velocity_flags(v.value)
    return MethodResult(method, label, v, cm, ce, cs, cc, factor, normal, normal * factor.value, geo,
                        tuple(extra), divisor, tuple(flags))


def _caller_velocity(velocity_m_s: float) -> Coefficient:
    return Coefficient("velocity_m_s", _finite("velocity_m_s", velocity_m_s, positive=True), "caller",
                       "given by the caller; velocity-dependent coefficients recomputed at this value")


def _velocity(own, velocity_m_s: float | None) -> Coefficient:
    """The method's own velocity (``own()``), or the caller's value.

    Every coefficient a method derives from the velocity is computed after
    this, from the value returned, so an override cannot leave a coefficient
    on the method's own velocity.
    """
    return own() if velocity_m_s is None else _caller_velocity(velocity_m_s)


def _pianc_ce(case: Case) -> tuple[Coefficient, dict]:
    geo = eccentricity(case.ship, case.berth)
    return Coefficient("ce", geo["ce"], "pianc2002-ce",
                       f"K {geo['k_m']:.2f} m, R {geo['r_m']:.2f} m, phi {geo['phi_deg']:.1f} deg"), geo


def _brolsma(case: Case, basis: str) -> Coefficient:
    s, b = case.ship, case.berth
    if basis == "dwt":
        if s.dwt_t is None:
            raise ValueError("the PIANC 2002 velocity table is keyed by DWT; give ship.dwt_t")
        v = brolsma_velocity(s.dwt_t, b.navigation)
        return Coefficient("velocity_m_s", v, "pianc2002-velocity",
                           f"curve {b.navigation} ({BROLSMA_CONDITIONS[b.navigation]}) at {s.dwt_t:,.0f} DWT")
    v = brolsma_velocity(s.displacement_t, b.navigation)
    return Coefficient("velocity_m_s", v, "bs6349-4-velocity",
                       f"curve {b.navigation} ({BROLSMA_CONDITIONS[b.navigation]}) at {s.displacement_t:,.0f} t")


def _abnormal(case: Case, position: str, note: str = "") -> Coefficient:
    f = abnormal_factor(case.ship.ship_type, position)
    return Coefficient("abnormal_factor", f, "pianc2002",
                       f"PIANC 2002 Table 4.2.5, {case.ship.ship_type}, {position} ships{note}")


def _common(case: Case, cm_method: str):
    r = _ukc_ratio(case.ship, case.berth)
    cm_src = {"pianc2002": "pianc2002-cm", "vasco_costa": "vasco-costa-1964", "ueda": "ueda-1981"}[cm_method]
    cm = Coefficient("cm", added_mass_factor(case.ship, case.berth.water_depth_m, cm_method), cm_src,
                     f"{cm_method}; KC/D = {r:.3f}")
    cs = softness(case.berth.fender_deflection_m, case.cs_basis)
    cc = berth_configuration(case.berth.structure, r, case.berth.berthing_angle_deg, cm_method, case.cc_basis)
    return r, cm, cs, cc


# ---- PIANC WG33 (2002) --------------------------------------------------


def _m_pianc2002(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    _, cm, cs, cc = _common(case, "pianc2002")
    ce, geo = _pianc_ce(case)
    v = _velocity(lambda: _brolsma(case, "dwt"), velocity_m_s)
    return _result("pianc2002", "PIANC WG33 (2002)", case, v, cm, ce, cs, cc,
                   _abnormal(case, position), geo)


def _m_ueda(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    _, cm, cs, cc = _common(case, "ueda")
    ce, geo = _pianc_ce(case)
    v = _velocity(lambda: _brolsma(case, "dwt"), velocity_m_s)
    return _result("ueda", "Ueda Cm (Japan), PIANC 2002 v and factor", case,
                   v, cm, ce, cs, cc, _abnormal(case, position), geo)


# ---- BS 6349-4:2014 -------------------------------------------------------


def _m_bs6349(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    r, cm, cs, cc = _common(case, "vasco_costa")
    ce, geo = _pianc_ce(case)
    flags = [f"Vasco Costa Cm outside its stated validity (KC/D {r:.3f} < 0.1)"] if r < 0.1 else []
    if case.ship.ship_type == "gas":
        fac = Coefficient("energy_factor", 2.0, "bs6349-4", "2.0 for gas carriers at LNG facilities (as summarised)")
    else:
        fac = _abnormal(case, position, "; BS 6349-4 own factor table not read")
    v = _velocity(lambda: _brolsma(case, "displacement"), velocity_m_s)
    if v.value < 0.08:
        flags.append("Vasco Costa Cm outside its stated validity (v < 0.08 m/s)")
    return _result("bs6349", "BS 6349-4:2014", case, v, cm, ce, cs, cc, fac, geo,
                   flags=flags)


# ---- PIANC WG211 (2024) ---------------------------------------------------

#: Characteristic berthing velocity (m/s, 0.02 % exceedance per berthing).
#: Moderate and unfavourable: R24B defaults without site data. Monitored and
#: favourable: derived here from the R24A Table 3 Weibull fits
#: (scale x (-ln 0.0002)^(1/shape)); not printed in either paper.
WG211_VELOCITY = {
    "monitored": (0.052 * (-math.log(0.0002)) ** (1 / 2.690), "derived from R24A Table 3 Weibull (0.052, 2.690)"),
    "favourable": (0.049 * (-math.log(0.0002)) ** (1 / 2.280), "derived from R24A Table 3 Weibull (0.049, 2.280)"),
    "moderate": (0.15, "R24B default without site data (pp. 976-979)"),
    "unfavourable": (0.25, "R24B default without site data (p. 980)"),
}

#: gamma_Eref, R24A Table 8 (pilot-assisted). Key (condition, cov_m) ->
#: (single A, B, C, D, multiple A, B, C, D).
WG211_GAMMA_EREF = {
    ("favourable", "high"): (1.32, 1.51, 1.59, 1.72, 1.16, 1.33, 1.40, 1.51),
    ("favourable", "moderate"): (1.37, 1.57, 1.65, 1.78, 1.22, 1.38, 1.46, 1.57),
    ("favourable", "low"): (1.51, 1.71, 1.80, 1.93, 1.35, 1.52, 1.60, 1.71),
    ("moderate", "high"): (1.37, 1.59, 1.69, 1.84, 1.19, 1.38, 1.46, 1.59),
    ("moderate", "moderate"): (1.43, 1.66, 1.76, 1.91, 1.25, 1.44, 1.53, 1.66),
    ("moderate", "low"): (1.59, 1.82, 1.92, 2.08, 1.39, 1.59, 1.68, 1.82),
    ("unfavourable", "high"): (1.52, 1.84, 1.98, 2.21, 1.27, 1.53, 1.65, 1.83),
    ("unfavourable", "moderate"): (1.61, 1.93, 2.07, 2.30, 1.35, 1.62, 1.74, 1.93),
    ("unfavourable", "low"): (1.80, 2.15, 2.30, 2.55, 1.52, 1.81, 1.94, 2.14),
    ("monitored", "high"): (1.25, 1.40, 1.47, 1.57, 1.12, 1.26, 1.32, 1.41),
    ("monitored", "moderate"): (1.30, 1.45, 1.52, 1.62, 1.17, 1.31, 1.37, 1.46),
    ("monitored", "low"): (1.42, 1.57, 1.64, 1.74, 1.29, 1.43, 1.49, 1.58),
}

#: gamma_n = a ln(n) + b, R24A Table 9: class -> (a, b) for n <= 100, (a, b) for n > 100.
WG211_GAMMA_N = {
    "A": ((0.0696, 0.6788), (0.0778, 0.6416)),
    "B": ((0.0605, 0.7207), (0.0669, 0.6917)),
    "C": ((0.0573, 0.7356), (0.0630, 0.7095)),
    "D": ((0.0529, 0.7561), (0.0576, 0.7340)),
}

#: Material factor used in the R24B worked example (Table 4).
WG211_GAMMA_M = 1.10


def wg211_gamma_eref(condition: str, cov_m: str, consequence_class: str, multiple_contact: bool) -> float:
    """Reference partial energy factor, R24A Table 8 (background-paper values)."""
    row = WG211_GAMMA_EREF[(condition, cov_m)]
    return row["ABCD".index(consequence_class) + (4 if multiple_contact else 0)]


#: Lowest berthing frequency (per year) at which the gamma_n fit is used. The
#: range of validity of R24A Table 9 was not read; below one berthing a year
#: the logarithm turns negative and the fit falls without bound (gamma_E below
#: 1 under 0.22 a year for class B), so the module does not extrapolate there.
WG211_GAMMA_N_MIN_PER_YEAR = 1.0


def wg211_gamma_n(consequence_class: str, berthings_per_year: float) -> float:
    """Berthing-frequency correction gamma_n = a ln(n) + b (R24A Table 9),
    for n of :data:`WG211_GAMMA_N_MIN_PER_YEAR` or more berthings a year."""
    n = _finite("berthings_per_year", berthings_per_year, positive=True)
    if n < WG211_GAMMA_N_MIN_PER_YEAR:
        raise ValueError(
            f"WG211 gamma_n: {n:g} berthings a year is below {WG211_GAMMA_N_MIN_PER_YEAR:g}; the fit "
            "a ln(n) + b is not extrapolated there (R24A Table 9 range not read)")
    lo, hi = WG211_GAMMA_N[consequence_class]
    a, b = lo if n <= 100 else hi
    return a * math.log(n) + b


def _m_wg211(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    r = _ukc_ratio(case.ship, case.berth)
    cm = Coefficient("cm", added_mass_factor(case.ship, case.berth.water_depth_m, "pianc2002"),
                     "pianc2002-cm", f"keel clearance rule (as in the R24B example); KC/D = {r:.3f}")
    ce, geo = _pianc_ce(case)
    v_val, v_note = WG211_VELOCITY[case.condition]
    v = _velocity(lambda: Coefficient("velocity_m_s", v_val, "pianc-wg211", f"{case.condition}: {v_note}"),
                  velocity_m_s)
    one = Coefficient("cs", 1.0, "pianc-wg211", "removed in WG211")
    cc = Coefficient("cc", 1.0, "pianc-wg211", "removed in WG211")
    g_ref = wg211_gamma_eref(case.condition if case.condition != "monitored" else "monitored",
                             case.cov_m, case.consequence_class, case.multiple_contact)
    g_n = wg211_gamma_n(case.consequence_class, case.berthings_per_year)
    fac = Coefficient(
        "partial_energy_factor", g_ref * g_n, "pianc-wg211",
        f"gamma_Eref {g_ref:.2f} (class {case.consequence_class}, {case.condition}, CoV_M {case.cov_m}, "
        f"{'multiple' if case.multiple_contact else 'single'} contact) x gamma_n {g_n:.3f} "
        f"({case.berthings_per_year:g}/yr); gamma_p = gamma_c = 1.0",
    )
    return _result("wg211", "PIANC WG211 (2024)", case, v, cm, ce, one, cc, fac, geo, divisor=WG211_GAMMA_M,
                   flags=["WG211 factors from the 2024 background papers; final guideline tables not read"])


# ---- UFC 4-152-01 ---------------------------------------------------------

_LT = 1.0160469088  # t per long ton
_FT = 0.3048
#: Berthing velocity normal to berth, UFC 4-152-01 Figs. 5-3 and 5-4, digitized:
#: (LT, sheltered, moderate, exposed) ft/s. Fig. 5-3 ("Small Ships (up to
#: 20,000 tons)") covers 1,000 to 20,000 LT and Fig. 5-4 ("Large Ship") 5,000
#: to 100,000 LT. Below 5,000 LT the rows are Fig. 5-3; from 5,000 to 20,000 LT,
#: where both figures apply, each class takes the larger of the two figures
#: (Fig. 5-3 is higher for the moderate class, by up to 0.08 ft/s at 5,000 LT,
#: Fig. 5-4 for the sheltered class from 8,000 LT); above 20,000 LT Fig. 5-4.
#: Both figures were digitized from the gridlines of the WBDG copy (about
#: +/-0.01 ft/s): Fig. 5-3 at 5,000 / 6,000 / 8,000 / 10,000 / 15,000 /
#: 20,000 LT gives 0.36 / 0.34 / 0.31 / 0.28 / 0.25 / 0.24 (sheltered),
#: 0.73 / 0.70 / 0.63 / 0.57 / 0.48 / 0.45 (moderate), 0.99 / 0.94 / 0.87 /
#: 0.83 / 0.76 / 0.72 (exposed); Fig. 5-4 gives 0.35 / 0.34 / 0.33 / 0.31 /
#: 0.28 / 0.25, 0.66 / 0.63 / 0.59 / 0.55 / 0.49 / 0.45, 0.99 / 0.95 / 0.88 /
#: 0.83 / 0.74 / 0.69.
UFC_VELOCITY_FT_S = (
    (1_000, 0.53, 0.95, 1.47), (2_000, 0.47, 0.87, 1.22), (4_000, 0.38, 0.77, 1.03),
    (5_000, 0.36, 0.73, 0.99), (6_000, 0.34, 0.70, 0.95), (8_000, 0.33, 0.63, 0.88),
    (10_000, 0.31, 0.57, 0.83), (15_000, 0.28, 0.49, 0.76), (20_000, 0.25, 0.45, 0.72),
    (30_000, 0.21, 0.41, 0.62), (50_000, 0.18, 0.35, 0.55), (70_000, 0.17, 0.30, 0.47),
    (100_000, 0.16, 0.28, 0.40),
)
#: How the overlap of Figs. 5-3 and 5-4 is resolved (stated with every UFC velocity).
UFC_VELOCITY_RULE = ("Fig. 5-3 below 5,000 LT; larger of Figs. 5-3 and 5-4 from 5,000 to 20,000 LT; "
                     "Fig. 5-4 above")
_UFC_CLASS = {"monitored": 1, "favourable": 1, "moderate": 2, "unfavourable": 3}


def ufc_velocity(displacement_t: float, condition: str) -> float:
    """UFC 4-152-01 Figs. 5-3/5-4 velocity (m/s), log-log between digitized points.

    favourable maps to "sheltered", moderate to "moderate", unfavourable to
    "exposed" (UFC 5-2.1.1 f).
    """
    j = _UFC_CLASS[condition]
    pts = tuple((lt * _LT, v[j - 1] * _FT) for lt, *v in UFC_VELOCITY_FT_S)
    return _loglog(_finite("displacement_t", displacement_t, positive=True), pts)


def ufc_added_mass(ship: Ship, water_depth_m: float, berthing_coefficient: float) -> float:
    """Surface-vessel Cm, UFC 4-152-01 (2017, Change 1 2024) 5-2.1.1 e, p. 109.

    Cm = C_M0 + (C_m1 - C_M0)(T/d)^3.5, C_M0 = 1.3 + 1.5 T/B,
    C_m1 = F [12.4 (T/B)^0.3 - 50 T/L], F = 1.5 Cb (Cb < 0.6) or 0.9.

    ``berthing_coefficient`` is the Cb that enters F; the caller chooses it
    by the reading (:data:`UFC_F_BASES`, :func:`ufc_f_cb`). The printed
    wording, the default, takes the berthing coefficient Cb = Ce Cg Cd Cc:
    Equation 5-2 (p. 106) defines it, and p. 109 repeats "Cb = berthing
    coefficient as defined previously" under F. The 0.6 threshold reads as a
    hull-form limit, which supports the block coefficient (C_BL, p. 107)
    instead; the text does not settle it, so both readings are offered.
    """
    t, b, l, d = ship.draft_m, ship.beam_m, ship.lbp_m, _finite("water_depth_m", water_depth_m, positive=True)
    cb = _finite("berthing_coefficient", berthing_coefficient, positive=True)
    f = 1.5 * cb if cb < 0.6 else 0.9
    cm0 = 1.3 + 1.5 * t / b
    cm1 = f * (12.4 * (t / b) ** 0.3 - 50.0 * t / l)
    return cm0 + (cm1 - cm0) * (t / d) ** 3.5


def ufc_f_cb(ship: Ship, berthing_coefficient: float, basis: str = "berthing_coefficient") -> float:
    """The Cb that enters the UFC factor F under ``basis`` (:data:`UFC_F_BASES`):
    the berthing coefficient Ce Cg Cd Cc (default, printed wording) or the
    ship's block coefficient."""
    if basis not in UFC_F_BASES:
        raise ValueError(f"ufc_f_basis must be one of {UFC_F_BASES}, got {basis!r}")
    return berthing_coefficient if basis == "berthing_coefficient" else ship.cb


def _m_ufc(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    s, b = case.ship, case.berth
    _ukc_ratio(s, b)
    v = _velocity(lambda: Coefficient("velocity_m_s", ufc_velocity(s.displacement_t, case.condition), "ufc-4-152-01",
                                      f"Figs. 5-3/5-4, {case.condition} (sheltered/moderate/exposed), digitized; {UFC_VELOCITY_RULE}"),
                  velocity_m_s)
    k = radius_of_gyration(s.cb, s.lbp_m)
    a = abs(contact_offset_m(s, b))
    ce_v = k * k / (a * a + k * k)
    geo = dict(cb=s.cb, k_m=k, a_m=a, ce=ce_v)
    ce = Coefficient("ce", ce_v, "ufc-4-152-01", f"k^2/(a^2 + k^2), k {k:.2f} m, a {a:.2f} m")
    quarter = a >= s.lbp_m / 4.0 - 1e-9
    cg = Coefficient("cg", 0.95 if quarter else 1.0, "ufc-4-152-01",
                     "0.95 at or beyond the quarter points" if quarter else "1.0 nearer midships (broadside)")
    cd = Coefficient("cs", 1.0, "ufc-4-152-01", "Cd 1.0 for larger ships on energy-absorbing fenders")
    if b.structure == "open" or abs(b.berthing_angle_deg) > 5.0:
        cc_v, note = 1.0, "open berth or angled approach"
    elif b.structure == "closed":
        cc_v, note = 0.8, "solid pier, parallel approach"
    else:
        cc_v, note = 0.9, "between open and solid (UFC: 'in between'); ASSUMED - to be confirmed: midpoint 0.9"
    cc = Coefficient("cc", cc_v, "ufc-4-152-01", note)
    cm = _ufc_cm(case, ce, cg, cd, cc, geo)
    fac = Coefficient("accidental_factor", 1.5, "ufc-4-152-01", "5-1.5.3: increase by at least 50 %")
    return _result("ufc", "UFC 4-152-01 (2017 C1)", case, v, cm, ce, cd, cc, fac, geo, extra=(cg,))


def _ufc_cm(case: Case, ce: Coefficient, cg: Coefficient, cd: Coefficient,
            cc: Coefficient, geo: dict) -> Coefficient:
    """UFC Cm through F (p. 109), with the Cb in F read per ``case.ufc_f_basis``.

    The one place the UFC Cm is derived, so an override of Cc (or of any
    other factor of the berthing coefficient) recomputes Cm through F when F
    reads the berthing coefficient. ``geo`` receives the berthing coefficient
    Ce Cg Cd Cc (``cb_berthing``) and the Cb used in F (``ufc_f_cb``); the
    reading is named in the Cm note.
    """
    cb_berth = ce.value * cg.value * cd.value * cc.value
    cb_f = ufc_f_cb(case.ship, cb_berth, case.ufc_f_basis)
    f = 1.5 * cb_f if cb_f < 0.6 else 0.9
    if case.ufc_f_basis == "berthing_coefficient":
        how = (f"F {f:.3f} from berthing coefficient Ce Cg Cd Cc = {cb_f:.3f} (Eq. 5-2, p. 106; p. 109, "
               f"printed wording; the block coefficient reading is the alternative)")
    else:
        how = (f"F {f:.3f} from block coefficient Cb = {cb_f:.3f} (reading chosen for the 0.6 hull-form "
               f"threshold; p. 109 prints berthing coefficient Ce Cg Cd Cc = {cb_berth:.3f})")
    geo.update(cb_berthing=cb_berth, ufc_f_cb=cb_f)
    return Coefficient("cm", ufc_added_mass(case.ship, case.berth.water_depth_m, cb_f), "ufc-4-152-01",
                       f"5-2.1.1 e surface vessels; {how}")


# ---- ROM 2.0-11 -------------------------------------------------------------

#: Table 4.6.4.36 (with tugs), digitized: (displacement t, favourable, moderate, unfavourable) m/s.
ROM_VELOCITY_TUGS = (
    (2_000, 0.20, 0.45, 0.60), (10_000, 0.15, 0.33, 0.45), (20_000, 0.10, 0.20, 0.30),
    (40_000, 0.08, 0.15, 0.21), (100_000, 0.08, 0.15, 0.20),
)
#: Table 4.6.4.37 (without tugs), digitized.
ROM_VELOCITY_NO_TUGS = (
    (2_000, 0.40, 0.65, 0.75), (10_000, 0.35, 0.50, 0.54), (20_000, 0.30, 0.45, 0.50),
    (40_000, 0.28, 0.43, 0.48), (100_000, 0.25, 0.40, 0.45),
)
_ROM_CLASS = {"monitored": 1, "favourable": 1, "moderate": 2, "unfavourable": 3}


def rom_velocity(displacement_t: float, condition: str, tugs: bool = True) -> float:
    """ROM 2.0-11 velocity (m/s), linear in log(displacement), constant outside 2,000 to 100,000 t."""
    m = _finite("displacement_t", displacement_t, positive=True)
    tab = ROM_VELOCITY_TUGS if tugs else ROM_VELOCITY_NO_TUGS
    j = _ROM_CLASS[condition]
    xs = [math.log(r[0]) for r in tab]
    return _interp(min(max(math.log(m), xs[0]), xs[-1]), xs, [r[j] for r in tab])


def rom_accidental_factor(displacement_t: float, tugs: bool, velocity_m_s: float,
                          berthings_per_year: float, return_period_yr: float = 500.0) -> float:
    """ROM 2.0-11 Table 4.6.4.43: product of four factors, scaled (T_R/500)^(1/20), not below 1.10."""
    g = (1.50 if displacement_t <= 85_000 else 1.25) * (1.00 if tugs else 1.25)
    g *= 1.20 if velocity_m_s <= 0.10 else 1.00
    g *= 1.15 if berthings_per_year > 300 else 1.00
    g *= (return_period_yr / 500.0) ** (1.0 / 20.0)
    return max(g, 1.10)


def _m_rom(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    s, b = case.ship, case.berth
    r = _ukc_ratio(s, b)
    v = _velocity(lambda: Coefficient("velocity_m_s", rom_velocity(s.displacement_t, case.condition, case.tugs),
                                      "rom-2.0-11", f"Table 4.6.4.{'36' if case.tugs else '37'}, {case.condition}, digitized"),
                  velocity_m_s)
    cm = Coefficient("cm", added_mass_factor(s, b.water_depth_m, "pianc2002"), "rom-2.0-11",
                     f"1.8 / 1.5 keel-clearance rule (p. 265); KC/D = {r:.3f}")
    ce, geo = _pianc_ce(case)
    ce = Coefficient("ce", ce.value, "rom-2.0-11", ce.note)
    cg = Coefficient("cg", 0.95 if case.continuous_berth else 1.0, "rom-2.0-11",
                     "0.95 continuous berth" if case.continuous_berth else "1.0 discontinuous (dolphins)")
    if r > 0.5 or abs(b.berthing_angle_deg) > 5.0 or v.value < 0.20 or b.structure == "open" or not case.continuous_berth:
        cc_v = 1.0
    else:
        cc_v = 0.9 if b.structure == "closed" else 1.0
    cc = Coefficient("cc", cc_v, "rom-2.0-11", "p. 271, 274")
    soft = b.fender_deflection_m > 0.15 and s.lbp_m < 300.0
    cs = Coefficient("cs", 1.0 if soft else 0.9, "rom-2.0-11", "p. 272")
    f = rom_accidental_factor(s.displacement_t, case.tugs, v.value, case.berthings_per_year)
    fac = Coefficient("accidental_factor", f, "rom-2.0-11",
                      f"Table 4.6.4.43 product ({'with' if case.tugs else 'without'} tugs, "
                      f"{case.berthings_per_year:g} calls/yr, T_R 500 yr)")
    return _result("rom", "ROM 2.0-11 (2012)", case, v, cm, ce, cs, cc, fac, geo, extra=(cg,))


# ---- EAU 2012 ---------------------------------------------------------------

#: Characteristic velocity, digitized from Roubos et al. (2017) Fig. 1 (EAU):
#: (DWT, favourable, normal, unfavourable); constant above 50,000 DWT.
EAU_VELOCITY = ((5_000, 0.17, 0.39, 0.53), (50_000, 0.08, 0.15, 0.20))
_EAU_CLASS = {"monitored": 1, "favourable": 1, "moderate": 2, "unfavourable": 3}


def eau_velocity(dwt_t: float, condition: str) -> float:
    d = _finite("dwt_t", dwt_t, positive=True)
    if d < EAU_VELOCITY[0][0]:
        raise ValueError("EAU velocity digitized only from 5,000 DWT")
    j = _EAU_CLASS[condition]
    pts = tuple((r[0], r[j]) for r in EAU_VELOCITY)
    return pts[-1][1] if d >= pts[-1][0] else _loglog(d, pts)


def _eau_own_velocity(case: Case) -> Coefficient:
    """The EAU table velocity, keyed by DWT: DWT is needed only here, so a
    caller's velocity override needs no DWT (as in the PIANC 2002 and Ueda
    methods and the workbook)."""
    s = case.ship
    if s.dwt_t is None:
        raise ValueError("the EAU velocity is keyed by DWT; give ship.dwt_t")
    return Coefficient("velocity_m_s", eau_velocity(s.dwt_t, case.condition), "eau-2012",
                       f"{case.condition} (normal), characteristic value, digitized")


def _m_eau(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    _, cm, cs, cc = _common(case, "pianc2002")
    ce, geo = _pianc_ce(case)
    v = _velocity(lambda: _eau_own_velocity(case), velocity_m_s)
    # the factor is the PIANC 2002 table; SFT p. 22 says it was "also adopted by EAU-2004"; EAU 2012 not read
    fac = _abnormal(case, position, "; applied to EAU (SFT p. 22: adopted by EAU-2004; the EAU 2012 text "
                                    "was not read)")
    return _result("eau", "EAU 2012", case, v, cm, ce, cs, cc, fac, geo,
                   flags=["EAU velocities digitized at two sizes only"])


# ---- IS 4651 (Part III):1974 --------------------------------------------------

#: Table 2, normal velocity (m/s) by displacement band (<= 5,000; <= 10,000; <= 100,000; > 100,000 t).
IS4651_VELOCITY = {
    "unfavourable": (0.75, 0.55, 0.40, 0.20),  # strong wind and swells, difficult
    "moderate": (0.45, 0.35, 0.20, 0.15),      # moderate wind and swell
    "favourable": (0.20, 0.15, 0.10, 0.10),    # sheltered, favourable
    "monitored": (0.20, 0.15, 0.10, 0.10),
}


def _m_is4651(case: Case, position: str, velocity_m_s: float | None = None) -> MethodResult:
    s, b = case.ship, case.berth
    _ukc_ratio(s, b)
    m = s.displacement_t
    band = 0 if m <= 5_000 else 1 if m <= 10_000 else 2 if m <= 100_000 else 3
    v = _velocity(lambda: Coefficient("velocity_m_s", IS4651_VELOCITY[case.condition][band], "is-4651-3-1974",
                                      "Table 2"), velocity_m_s)
    cm = Coefficient("cm", 1.0 + 2.0 * s.draft_m / s.beam_m, "is-4651-3-1974", "5.2.1.2: 1 + 2D/B")
    r_g = 0.25 * s.lbp_m
    l_ = abs(contact_offset_m(s, b))
    q = (l_ / r_g) ** 2
    ce_v = (1.0 + q * math.sin(math.radians(b.berthing_angle_deg)) ** 2) / (1.0 + q)
    ce = Coefficient("ce", ce_v, "is-4651-3-1974", f"5.2.1.3, r = L/4, l {l_:.2f} m, theta {b.berthing_angle_deg:g} deg")
    cs = Coefficient("cs", 0.9, "is-4651-3-1974", "5.2.1.4")
    cc = Coefficient("cc", 1.0, "is-4651-3-1974", "not in the method")
    fac = Coefficient("factor", 1.0, "is-4651-3-1974", "no abnormal factor in the method")
    return _result("is4651", "IS 4651-3:1974 (withdrawn)", case, v, cm, ce, cs, cc, fac,
                   dict(ce=ce_v, l_m=l_, r_m=r_g), flags=["withdrawn standard; comparison only"])


@dataclass(frozen=True)
class MethodSpec:
    key: str
    label: str
    evaluate: object
    source_id: str


METHODS: dict[str, MethodSpec] = {
    m.key: m for m in (
        MethodSpec("pianc2002", "PIANC WG33 (2002)", _m_pianc2002, "pianc2002"),
        MethodSpec("wg211", "PIANC WG211 (2024)", _m_wg211, "pianc-wg211"),
        MethodSpec("bs6349", "BS 6349-4:2014", _m_bs6349, "bs6349-4"),
        MethodSpec("eau", "EAU 2012", _m_eau, "eau-2012"),
        MethodSpec("rom", "ROM 2.0-11 (2012)", _m_rom, "rom-2.0-11"),
        MethodSpec("ufc", "UFC 4-152-01 (2017 C1)", _m_ufc, "ufc-4-152-01"),
        MethodSpec("ueda", "Ueda Cm (Japan), PIANC 2002 v and factor", _m_ueda, "ueda-1981"),
        MethodSpec("is4651", "IS 4651-3:1974 (withdrawn)", _m_is4651, "is-4651-3-1974"),
    )
}


def _as_case(ship_or_case, berth=None, **case_kw) -> Case:
    if isinstance(ship_or_case, Case):
        return ship_or_case
    return Case(ship_or_case, berth, **case_kw)


# --------------------------------------------------------------------------
# Citation sidecar (calculation-output citation contract)
# --------------------------------------------------------------------------

_STANDARDS = "wikis/marine-engineering/wiki/standards/"
_CONCEPTS = "wikis/marine-engineering/wiki/concepts/"

#: Wiki target of each standard or paper in :data:`SOURCES` that a method
#: coefficient can come from: source id -> (code_id, publisher, revision,
#: wiki_path). Pages are standards pages (or concept pages for the two added-
#: mass papers), never vendor-derived source pages. For the two papers the
#: publisher field holds the author, because the publisher was not verified.
#: The vendor manuals (``tfadm-2024``, ``sft-2026``) are not cited: they are
#: named in each secondary citation's section instead.
CITATION_TARGETS: dict[str, tuple[str, str, str, str]] = {
    "pianc2002": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc2002-velocity": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc2002-cm": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc2002-ce": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc2002-cc": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc2002-cs": ("PIANC-WG33-2002", "PIANC", "2002", _STANDARDS + "pianc-wg33-2002.md"),
    "pianc-wg211": ("PIANC-WG211-2024", "PIANC", "2024", _STANDARDS + "pianc-wg211-2024.md"),
    "bs6349-4": ("BS-6349-4", "BSI", "2014", _STANDARDS + "bs-6349-4.md"),
    "bs6349-4-velocity": ("BS-6349-4", "BSI", "2014", _STANDARDS + "bs-6349-4.md"),
    "eau-2012": ("EAU-2012", "HTG", "2012", _STANDARDS + "eau-2012.md"),
    "rom-2.0-11": ("ROM-2.0-11", "Puertos del Estado", "2012", _STANDARDS + "rom-2-0-11.md"),
    "ufc-4-152-01": ("UFC-4-152-01", "US DoD", "2017 Change 1 2024", _STANDARDS + "ufc-4-152-01.md"),
    "is-4651-3-1974": ("IS-4651-3", "BIS", "1974", _STANDARDS + "is-4651-3.md"),
    "iso17357-1": ("ISO-17357-1", "ISO", "2014", _STANDARDS + "iso-17357-1.md"),
    "vasco-costa-1964": ("vasco-costa-1964", "Vasco Costa", "1964",
                         _CONCEPTS + "vasco-costa-1964-berthing-added-mass.md"),
    "ueda-1981": ("ueda-1981", "Ueda et al.", "1981", _CONCEPTS + "ueda-1981-berthing-added-mass.md"),
}

#: Resolution failures that the explicit opt-out ``record`` turns into a
#: flag (the page is not yet written, or no wiki is configured on this
#: machine). In the default ``strict`` mode they raise at calculation time
#: (fail-closed, calculation-output citation contract section 4.1). A page
#: whose code_id, publisher or revision differs from the citation raises in
#: every checking mode.
_UNRESOLVED_REASONS = ("page_missing", "resolver_unconfigured")

#: ``strict`` is the default. ``record`` and ``off`` are explicit opt-outs;
#: each flags the results it touches, so the omission stays visible.
CITATION_MODES = ("strict", "record", "off")

#: Flag added to a result whose citations were not checked (``off``).
CITATIONS_NOT_CHECKED = "citations not checked (citations='off', explicit opt-out)"


def citation_for(source_id: str) -> Citation:
    """The :class:`Citation` of a source in :data:`CITATION_TARGETS`."""
    if source_id not in CITATION_TARGETS:
        raise KeyError(f"no citation target for source {source_id!r}")
    code_id, publisher, revision, wiki_path = CITATION_TARGETS[source_id]
    src = SOURCES[source_id]
    note = f"{src.verification}" + (f"; read in {src.read_in}" if src.read_in else "")
    return Citation(code_id=code_id, publisher=publisher, revision=revision,
                    section=src.clause or src.edition, wiki_path=wiki_path, note=note,
                    source_sibling="generic")


def result_citations(coefficients) -> tuple[Citation, ...]:
    """One citation per cited source among ``coefficients`` (caller values and
    vendor manuals, :data:`VENDOR_SOURCES`, excluded: a manual is named in the
    coefficient's source, not cited as a standard)."""
    out, seen = [], set()
    for c in coefficients:
        if c.source == "caller" or c.source in VENDOR_SOURCES or c.source in seen:
            continue
        seen.add(c.source)
        out.append(citation_for(c.source))
    return tuple(out)


def resolve_citations(citations, *, mode: str = "strict",
                      wiki_root: Path | None = None) -> tuple[tuple[str, str], ...]:
    """Resolve each citation against the wiki; return (code_id, status) pairs.

    ``mode``: ``strict`` (default) raises :class:`CitationResolutionError`
    on any failure, including a missing page or an unconfigured resolver
    (fail-closed). The explicit opt-out ``record`` raises on a mismatched
    or misconfigured target but records a missing page or an unconfigured
    resolver as ``unresolved: <reason>``; ``off`` checks nothing
    (``not checked``).
    ``wiki_root`` is a wiki clone root; ``None`` uses the resolver's search
    order (``LLM_WIKI_PATH`` first).
    """
    if mode not in CITATION_MODES:
        raise ValueError(f"citation mode must be one of {CITATION_MODES}")
    out = []
    for c in citations:
        if mode == "off":
            out.append((c.code_id, "not checked"))
            continue
        try:
            validate_citation(c, repo_root=wiki_root)
        except CitationResolutionError as exc:
            reason = exc.reason.split(":", 1)[0]
            if mode == "strict" or reason not in _UNRESOLVED_REASONS:
                raise CitationResolutionError(code_id=c.code_id, wiki_path=c.wiki_path,
                                              reason=exc.reason) from exc
            out.append((c.code_id, f"unresolved: {reason}"))
            continue
        out.append((c.code_id, "resolved"))
    return tuple(out)


def _citation_flags(status, mode: str) -> list[str]:
    """The flags an explicit opt-out adds for these citation statuses."""
    seen, pending = set(), []
    for code_id, st in status:
        if st.startswith("unresolved") and code_id not in seen:
            seen.add(code_id)
            pending.append(f"{code_id} ({st.split(': ', 1)[1]})")
    flags = []
    if pending:
        flags.append("citation not resolved against the wiki: " + ", ".join(pending)
                     + "; values carry the source register only (citations='record', explicit opt-out)")
    if mode == "off" and any(st == "not checked" for _, st in status):
        flags.append(CITATIONS_NOT_CHECKED)
    return flags


def _cite(res: MethodResult, mode: str, wiki_root: Path | None) -> MethodResult:
    cites = result_citations(res.coefficients())
    status = resolve_citations(cites, mode=mode, wiki_root=wiki_root)
    flags = list(res.flags) + _citation_flags(status, mode)
    return replace(res, citations=cites, citation_status=status, flags=tuple(flags))


def _with_cc(case: Case, res: MethodResult, cc_value: float) -> MethodResult:
    """``res`` with the caller's Cc and every term derived from Cc recomputed."""
    reason = SWEEP_EXCLUSIONS[res.method].get("cc")
    if reason is not None:
        raise ValueError(f"{res.label} does not take a Cc override: {reason}")
    cc = Coefficient("cc", _finite("cc", cc_value, positive=True), "caller",
                     "given by the caller; terms derived from Cc recomputed at this value")
    cm, geo = res.cm, dict(res.geometry)
    if res.method == "ufc":
        cg = next(c for c in res.extra if c.name == "cg")
        cm = _ufc_cm(case, res.ce, cg, res.cs, cc, geo)
    return _result(res.method, res.label, case, res.velocity_m_s, cm, res.ce, res.cs, cc, res.factor, geo,
                   extra=res.extra, divisor=res.capacity_divisor,
                   flags=[x for x in res.flags if not x.startswith("below 0.")])


def energy_by_method(
    ship_or_case,
    berth: Berth | None = None,
    method: str = "pianc2002",
    *,
    velocity_m_s: float | None = None,
    factor: float | None = None,
    factor_position: str = "smallest",
    cc: float | None = None,
    citations: str = "strict",
    citation_root: Path | None = None,
    **case_kw,
) -> MethodResult:
    """Normal and design berthing energy by ``method`` (a key of :data:`METHODS`).

    Accepts a :class:`Case`, or a Ship and Berth (with :class:`Case` keywords).
    ``velocity_m_s``, ``cc`` and ``factor`` override the method's own values;
    the result then names "caller" as their source. A velocity override is
    applied before the method derives its velocity-dependent coefficients
    (ROM accidental factor and Cc, validity flags), so they follow the
    effective velocity. A Cc override replaces the method's berth
    configuration factor and every term derived from it is recomputed (UFC
    4-152-01: Cm through F of the berthing coefficient Ce Cg Cd Cc, when
    ``ufc_f_basis`` is the default berthing coefficient); a
    method that fixes or removes Cc (:func:`inapplicable_sweeps`) refuses it.
    ``factor_position`` chooses the largest- or smallest-ship PIANC abnormal
    factor where a method uses it.

    The result carries its citation sidecar, resolved against
    ``citation_root`` per ``citations`` (see :func:`resolve_citations`). The
    default ``strict`` raises :class:`CitationResolutionError` when a cited
    page is missing or no wiki is configured; ``record`` (missing pages
    flagged) and ``off`` (not checked, flagged) are explicit opt-outs.
    """
    if method not in METHODS:
        raise ValueError(f"unknown method {method!r}; one of {tuple(METHODS)}")
    case = _as_case(ship_or_case, berth, **case_kw)
    res = METHODS[method].evaluate(case, factor_position, velocity_m_s)
    if cc is not None:
        res = _with_cc(case, res, cc)
    if factor is not None:
        f = Coefficient(res.factor.name, _finite("factor", factor, positive=True), "caller")
        res = _result(res.method, res.label, case, res.velocity_m_s, res.cm, res.ce, res.cs, res.cc, f,
                      res.geometry, extra=res.extra, divisor=res.capacity_divisor,
                      flags=[x for x in res.flags if not x.startswith("below 0.")])
    return _cite(res, citations, citation_root)


def compare_methods(ship_or_case, berth=None, methods=None, **kwargs) -> list[MethodResult]:
    """:func:`energy_by_method` for each method (default all); a method that
    cannot be evaluated for these inputs is left out (see
    :func:`compare_methods_skipped`)."""
    out = []
    for key in methods or METHODS:
        try:
            out.append(energy_by_method(ship_or_case, berth, key, **kwargs))
        except (ValueError, KeyError):
            continue
    return out


def compare_methods_skipped(ship_or_case, berth=None, methods=None, **kwargs) -> dict[str, str]:
    skipped = {}
    for key in methods or METHODS:
        try:
            energy_by_method(ship_or_case, berth, key, **kwargs)
        except (ValueError, KeyError) as exc:
            skipped[key] = str(exc)
    return skipped


def wg211_fender_capacity(e_base_knm: float, c_ang: float = 1.0, c_t: float = 1.0, c_v: float = 1.0,
                          c_mult: float = 1.0, gamma_m: float = WG211_GAMMA_M) -> float:
    """E_f,d = E_base C_ang C_t C_v C_mult / gamma_m (R24A Eq. 5, 10; R24B Table 4).

    C_ang is taken as given (R24B prints 1.03 to 1.05 for cone fenders), to
    reproduce the worked example; the design path (:func:`design_performance`)
    does not credit an angular factor above 1.
    """
    e = _finite("e_base_knm", e_base_knm, positive=True)
    for name, x in (("c_ang", c_ang), ("c_t", c_t), ("c_v", c_v), ("c_mult", c_mult), ("gamma_m", gamma_m)):
        _finite(name, x, positive=True)
    return e * c_ang * c_t * c_v * c_mult / gamma_m


# --------------------------------------------------------------------------
# Fender performance
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class PerformanceCurve:
    """Normalised load-deflection curve of a fender type.

    ``points`` are (deflection %, reaction % of rated, energy % of rated),
    deflection increasing and energy not decreasing, starting at energy 0 %
    and ending at or beyond the rated point (energy 100 %). A curve that does
    not cover 0 % to 100 % energy is refused: :func:`reaction_at_energy` reads
    it up to 100 % of the capacity, and a reaction outside the data is not
    supported by them.
    """

    name: str
    points: tuple[tuple[float, float, float], ...]
    source: str

    def __post_init__(self) -> None:
        if len(self.points) < 2:
            raise ValueError("a curve needs at least two points")
        prev = None
        for p in self.points:
            if len(p) != 3:
                raise ValueError("curve points are (deflection, reaction, energy) in %")
            for v in p:
                _finite("curve value", v, nonneg=True)
            if prev is not None and (p[0] <= prev[0] or p[2] < prev[2]):
                raise ValueError("curve deflection must increase and energy must not fall")
            prev = p
        if self.points[0][2] != 0.0:
            raise ValueError(
                f"curve {self.name!r} starts at {self.points[0][2]:g} % energy; it must start at 0 % "
                "(below its first point the reaction is not covered by the data)")
        if self.points[-1][2] < 100.0:
            raise ValueError(
                f"curve {self.name!r} ends at {self.points[-1][2]:g} % energy; it must reach the rated "
                "point (100 % energy), or the reaction above its last point is not covered by the data")


def _interp(x, xs, ys):
    if x <= xs[0]:
        return ys[0]
    for i in range(len(xs) - 1):
        if xs[i] <= x <= xs[i + 1]:
            if xs[i + 1] == xs[i]:
                return ys[i + 1]
            t = (x - xs[i]) / (xs[i + 1] - xs[i])
            return ys[i] + t * (ys[i + 1] - ys[i])
    return ys[-1]


def peak_reaction_pct(curve: PerformanceCurve, energy_pct: float) -> float:
    """Largest reaction (% of rated) reached along ``curve`` from 0 % up to
    ``energy_pct`` energy.

    A curve may fall after buckling and rise again, so the reaction at the
    final energy can be below a peak already passed during compression. The
    structure carries the largest reaction of the compression, so the peak
    is the value compared with a limit. For a reaction that does not fall it
    equals the value at ``energy_pct``.
    """
    e = _finite("energy_pct", energy_pct, nonneg=True)
    energies = [p[2] for p in curve.points]
    reactions = [p[1] for p in curve.points]
    passed = [r for en, r in zip(energies, reactions) if en <= e]
    return max([_interp(e, energies, reactions), *passed])


def rated_peak_factor(curve: PerformanceCurve | None) -> float:
    """Largest reaction along ``curve`` up to the rated point (100 % energy)
    over the rated reaction, and never below 1.

    The rated-deflection reaction of a catalogue row is an upper bound for
    any demand within the rating only when the curve does not exceed 100 %
    before the rated point; where it does, the rated value times this factor
    is. A rise beyond the rated point is not read. Without a curve: 1.
    """
    if curve is None:
        return 1.0
    return max(1.0, peak_reaction_pct(curve, 100.0) / 100.0)


def reaction_at_energy(
    fender: FenderRating,
    energy_knm: float,
    curve: PerformanceCurve | None = None,
    energy_capacity_knm: float | None = None,
) -> tuple[float | None, float | None]:
    """Reaction (kN) and deflection (%) when the fender absorbs ``energy_knm``.

    With a curve the energy fraction is taken of ``energy_capacity_knm``
    (default: rated energy). The reaction is the largest reached along the
    curve up to that fraction (:func:`peak_reaction_pct`), scaled by the
    rated reaction: after buckling the reaction can fall, and the value at
    the final energy would hide the earlier peak. The deflection is the
    value at that fraction. Without a curve the rated reaction is returned,
    an upper bound for a reaction that rises to the rated point, and
    deflection is None. Above the capacity both are None: the reaction is
    not rated there.
    """
    e = _finite("energy_knm", energy_knm, nonneg=True)
    if energy_capacity_knm is None:
        cap = fender.rated_energy_knm
    else:
        cap = _finite("energy_capacity_knm", energy_capacity_knm, positive=True)
    if e > cap * (1.0 + 1e-12):
        return None, None
    if curve is None:
        return fender.reaction_kn, None
    frac = 100.0 * e / cap
    energies = [p[2] for p in curve.points]
    r_pct = peak_reaction_pct(curve, frac)
    d_pct = _interp(frac, energies, [p[0] for p in curve.points])
    return fender.reaction_kn * r_pct / 100.0, d_pct


@dataclass(frozen=True)
class DesignPerformance:
    """Factored performance of one fender, with each factor."""

    fender_id: str
    energy_knm: float
    reaction_kn: float
    factors: dict[str, float]


def design_performance(
    fender: FenderRating,
    energy_tolerance: float = 0.10,
    reaction_tolerance: float = 0.10,
    angular_factor: float = 1.0,
    velocity_factor_energy: float = 1.0,
    velocity_factor_reaction: float = 1.0,
    temperature_factor_energy: float = 1.0,
    temperature_factor_reaction: float = 1.0,
) -> DesignPerformance:
    """Minimum energy and maximum reaction of a catalogue fender.

    energy = rated x (1 - energy tolerance) x angular x velocity x temperature
    factors (at the slowest velocity and highest temperature); reaction =
    rated x (1 + reaction tolerance) x velocity x temperature factors (at the
    fastest velocity and lowest temperature) (TFADM p. 30; SFT p. 26).
    Pneumatic fenders are rated at GEA with 0 % energy tolerance (SFT p. 26).
    The angular factor must be in (0, 1], as in ``select_fenders``: a vendor
    angle factor above 1 (cone fenders at small angles, TFADM p. 36 Super
    Cone 1.039 to 1.055; R24B Table 4 C_ang 1.03 to 1.05) is not credited
    here. :func:`wg211_fender_capacity` takes C_ang as given, to reproduce the
    R24B worked example only.
    """
    et = _finite("energy_tolerance", energy_tolerance, nonneg=True)
    rt = _finite("reaction_tolerance", reaction_tolerance, nonneg=True)
    if et >= 1.0:
        raise ValueError("energy_tolerance must be below 1")
    # a reduction only, as select_fenders: above 1 it would raise the rated energy
    ang = _finite("angular_factor", angular_factor)
    if not 0.0 < ang <= 1.0:
        raise ValueError(f"angular_factor must be in (0, 1], got {angular_factor!r}")
    f = dict(energy_tolerance=et, reaction_tolerance=rt, angular_factor=ang)
    for name, value in (
        ("velocity_factor_energy", velocity_factor_energy),
        ("velocity_factor_reaction", velocity_factor_reaction),
        ("temperature_factor_energy", temperature_factor_energy),
        ("temperature_factor_reaction", temperature_factor_reaction),
    ):
        f[name] = _finite(name, value, positive=True)
    e = (fender.rated_energy_knm * (1.0 - et) * f["angular_factor"]
         * f["velocity_factor_energy"] * f["temperature_factor_energy"])
    r = (fender.reaction_kn * (1.0 + rt)
         * f["velocity_factor_reaction"] * f["temperature_factor_reaction"])
    return DesignPerformance(fender.fender_id, e, r, f)


def compression_time_s(rated_deflection_mm: float, velocity_m_s: float, deceleration: float = 0.74) -> float:
    """Compression time t = d / (f Vd) for the velocity factor (TFADM p. 35)."""
    d = _finite("rated_deflection_mm", rated_deflection_mm, positive=True)
    v = _finite("velocity_m_s", velocity_m_s, positive=True)
    f = _finite("deceleration", deceleration, positive=True)
    return d / (f * v * 1000.0)


#: Velocity factor by compression time (s), TFADM p. 35: (t, blend, NR, SBR).
VELOCITY_FACTOR_TABLE = (
    (1, 1.20, 1.14, 1.31), (2, 1.16, 1.10, 1.25), (3, 1.14, 1.09, 1.22), (4, 1.13, 1.07, 1.20),
    (5, 1.11, 1.06, 1.19), (6, 1.10, 1.06, 1.17), (7, 1.09, 1.05, 1.16), (8, 1.09, 1.04, 1.15),
    (9, 1.08, 1.04, 1.14), (10, 1.07, 1.03, 1.14), (11, 1.07, 1.03, 1.13), (12, 1.06, 1.02, 1.12),
    (13, 1.06, 1.02, 1.12), (14, 1.05, 1.02, 1.11), (15, 1.05, 1.01, 1.11), (16, 1.05, 1.01, 1.10),
    (17, 1.04, 1.01, 1.10), (18, 1.04, 1.01, 1.09), (19, 1.04, 1.00, 1.09), (20, 1.03, 1.00, 1.08),
)

#: Temperature factor, TFADM p. 35: (deg C, blend, NR, SBR).
TEMPERATURE_FACTOR_TABLE = (
    (-30, 1.540, 1.315, 1.877), (-20, 1.249, 1.142, 1.410), (-10, 1.130, 1.080, 1.206),
    (0, 1.075, 1.053, 1.108), (10, 1.030, 1.025, 1.038), (23, 1.000, 1.000, 1.000),
    (30, 0.978, 0.978, 0.979), (40, 0.947, 0.946, 0.948), (50, 0.916, 0.914, 0.918),
)

_COMPOUND = {"blend": 1, "nr": 2, "sbr": 3}


VELOCITY_FACTOR_USES = ("energy", "reaction")


def velocity_factor(compression_time: float, compound: str = "blend", use: str | None = None) -> float:
    """VF from the TFADM p. 35 table (1 to 20 s), by row: the factor falls as
    the time grows, so between rows ``use="energy"`` takes the next higher
    tabulated time (the smaller factor, so the minimum energy is not
    overstated) and ``use="reaction"`` or no ``use`` the nearest lower time
    (the larger factor, an upper bound for the maximum reaction). At a
    tabulated time the three agree.

    The table stops at 20 s, where the blend and SBR factors are still above
    1.00. Beyond 20 s the factor depends on what it is used for, and ``use``
    must say which:

    * ``"energy"`` (minimum energy, slowest berthing): 1.00. The manual's
      worked example (pp. 39-40, SBR) takes VF = 1 at a 48 s compression
      time, the catalogue (slow constant-velocity) performance; no tabulated
      value is below 1.00, so energy is not overstated.
    * ``"reaction"`` (maximum reaction, fastest berthing): the 20 s value,
      an upper bound because the tabulated factor does not increase with time.

    Without ``use`` a time beyond 20 s raises ValueError rather than guess.
    """
    t = _finite("compression_time", compression_time, positive=True)
    if compound not in _COMPOUND:
        raise ValueError(f"compound must be one of {tuple(_COMPOUND)}")
    if use is not None and use not in VELOCITY_FACTOR_USES:
        raise ValueError(f"use must be one of {VELOCITY_FACTOR_USES}")
    j = _COMPOUND[compound]
    if t < 1.0:
        raise ValueError("compression time below the tabulated 1 s")
    t_max = VELOCITY_FACTOR_TABLE[-1][0]
    if t > t_max:
        if use is None:
            raise ValueError(
                f"compression time {t:.1f} s beyond the tabulated {t_max} s: pass use='energy' "
                f"(VF 1.00, TFADM pp. 39-40 example) or use='reaction' (the {t_max} s value, an upper bound)"
            )
        return 1.0 if use == "energy" else VELOCITY_FACTOR_TABLE[-1][j]
    if use == "energy":
        row = min((r for r in VELOCITY_FACTOR_TABLE if r[0] >= t), key=lambda r: r[0])
    else:
        row = max((r for r in VELOCITY_FACTOR_TABLE if r[0] <= t), key=lambda r: r[0])
    return row[j]


def temperature_factor(temperature_c: float, compound: str = "blend") -> float:
    """TF from the TFADM p. 35 table, linear between rows."""
    t = _finite("temperature_c", temperature_c)
    j = _COMPOUND[compound]
    xs = [r[0] for r in TEMPERATURE_FACTOR_TABLE]
    if not xs[0] <= t <= xs[-1]:
        raise ValueError("temperature outside -30 to 50 deg C")
    return _interp(t, xs, [r[j] for r in TEMPERATURE_FACTOR_TABLE])


# --------------------------------------------------------------------------
# One-at-a-time sensitivity sweeps
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class SweepPoint:
    """One swept value. ``reaction_kn`` is the largest curve reaction reached
    up to this energy (:func:`reaction_at_energy`) times the sweep's
    ``reaction_factor`` (tolerance, velocity, temperature);
    ``hull_pressure_kpa`` is the catalogue hull pressure at rated deflection
    times the same factor: the pressure at part load is not calculated,
    because no pressure-deflection relationship is sourced. Both are None
    above the energy capacity, where the catalogue rates neither."""

    value: float
    design_energy_knm: float
    energy_per_fender_knm: float
    utilisation: float
    reaction_kn: float | None
    deflection_pct: float | None
    hull_pressure_kpa: float | None


@dataclass(frozen=True)
class Sweep:
    """Design energy and fender response as one input moves over its range.

    ``range_low``/``range_high`` bound the credible range (``range_basis``
    says where it comes from); ``points`` may extend wider to show the
    trend. ``capacity_knm`` is the fender energy capacity for utilisation;
    ``reaction_factor`` multiplies every reaction and hull pressure before
    the comparison with ``reaction_limit_kn`` and
    ``hull_pressure_limit_kpa``, the limits drawn.

    ``citation_status`` and ``flags`` carry the citation qualification of the
    method results behind the sweep (:func:`standard_sweeps`): the status of
    each citation and the flag of an explicit opt-out (``record``, ``off``).
    Both are empty for a sweep built from a bare function.

    ``crossing`` is the swept value at which utilisation reaches 1, solved
    on the method by :func:`standard_sweeps` (``crossing_basis`` says how:
    closed form or bisection); :meth:`limit_value` returns it. A sweep built
    from a bare function has no ``crossing_basis`` and interpolates.
    """

    parameter: str
    label: str
    unit: str
    base_value: float
    base_design_energy_knm: float
    range_low: float
    range_high: float
    range_basis: str
    points: tuple[SweepPoint, ...]
    capacity_knm: float
    reaction_limit_kn: float | None
    hull_pressure_limit_kpa: float | None
    fenders_in_contact: int
    reaction_factor: float = 1.0
    citation_status: tuple[tuple[str, str], ...] = ()
    flags: tuple[str, ...] = ()
    crossing: float | None = None
    crossing_basis: str = ""

    def at(self, value: float) -> SweepPoint:
        for p in self.points:
            if math.isclose(p.value, value, rel_tol=1e-12, abs_tol=1e-12):
                return p
        raise KeyError(value)

    def change_pct(self, value: float) -> float:
        return (self.at(value).design_energy_knm / self.base_design_energy_knm - 1.0) * 100.0

    def limit_value(self) -> float | None:
        """Smallest swept value at which utilisation reaches 1, or None if it
        never does: the value solved on the method (:attr:`crossing`) when the
        sweep carries one, else linear interpolation between the points (which
        drifts where a coefficient has a break between them)."""
        if self.crossing_basis:
            return self.crossing
        br = _utilisation_bracket(self.points)
        if br is None:
            return None
        a, b = br
        return a.value + (1.0 - a.utilisation) / (b.utilisation - a.utilisation) * (b.value - a.value)

    def reaction_limit_value(self) -> float | None:
        """Smallest swept value at which the factored reaction reaches the
        reaction limit (linear between rated points), or None if it does not
        within the rated points or no limit is set."""
        if self.reaction_limit_kn is None:
            return None
        lim = self.reaction_limit_kn
        pts = [p for p in self.points if p.reaction_kn is not None]
        for a, b in zip(pts, pts[1:]):
            ra, rb = a.reaction_kn, b.reaction_kn
            if ra != rb and min(ra, rb) <= lim <= max(ra, rb):
                return a.value + (lim - ra) / (rb - ra) * (b.value - a.value)
        return None

    def over_reaction_limit(self) -> list[float]:
        """Swept values whose factored reaction exceeds the reaction limit."""
        if self.reaction_limit_kn is None:
            return []
        return [p.value for p in self.points
                if p.reaction_kn is not None and p.reaction_kn > self.reaction_limit_kn]


def _utilisation_bracket(points) -> tuple[SweepPoint, SweepPoint] | None:
    """The first pair of neighbouring points between which utilisation
    reaches 1 (either way), or None."""
    for a, b in zip(points, points[1:]):
        if a.utilisation != b.utilisation and min(a.utilisation, b.utilisation) <= 1.0 <= max(
                a.utilisation, b.utilisation):
            return a, b
    return None


def ukc_depth_for_cm(draft_m: float, cm: float) -> float:
    """Water depth (m) at which the PIANC 2002 keel-clearance Cm
    (:func:`~digitalmodel.hydrodynamics.berthing_energy.added_mass_factor_ukc`)
    equals ``cm``, on its sloped part (1.5 < cm < 1.8): the closed-form
    inverse, KC/D = 0.1 + 0.4 (1.8 - cm) / 0.3. Outside the slope the depth is
    not unique and ValueError is raised."""
    t = _finite("draft_m", draft_m, positive=True)
    c = _finite("cm", cm, positive=True)
    if not 1.5 < c < 1.8:
        raise ValueError(f"keel-clearance Cm {c:g} is not on the sloped part (1.5 to 1.8): depth not unique")
    return t * (1.0 + 0.1 + 0.4 * (1.8 - c) / 0.3)


#: Methods whose energy depends on the water depth only through the PIANC
#: keel-clearance Cm (Cc fixed at 1.0 with it, or removed): their depth at
#: utilisation 1 is solved in closed form (:func:`ukc_depth_for_cm`).
KEEL_CLEARANCE_ONLY = ("pianc2002", "eau", "wg211")


def _crossing(sweep: Sweep, fn, cap: float, n: int, *, method: str, base: MethodResult,
              draft_m: float) -> tuple[float | None, str]:
    """Swept value at which utilisation reaches 1, solved on the method.

    Water depth for :data:`KEEL_CLEARANCE_ONLY` methods: closed form, since
    utilisation is base utilisation x Cm(depth) / Cm(base). Otherwise the
    bracket found between the sweep points is bisected on ``fn`` (the
    method itself) to 1e-12 of the value, so a coefficient break between two
    points (the Cm kink at KC/D 0.1, the ROM velocity step) is not
    interpolated across.
    """
    br = _utilisation_bracket(sweep.points)
    if br is None:
        return None, "utilisation does not reach 1 within the swept values"
    a, b = br
    if sweep.parameter == "water_depth_m" and method in KEEL_CLEARANCE_ONLY:
        u0 = base.design_energy_knm / n / cap
        try:
            d = ukc_depth_for_cm(draft_m, base.cm.value / u0)
        except ValueError:
            d = None
        if d is not None and min(a.value, b.value) - 1e-9 <= d <= max(a.value, b.value) + 1e-9:
            return d, (f"closed form: keel-clearance Cm {base.cm.value / u0:.5f} needed "
                       f"(base Cm {base.cm.value:.5f} / base utilisation {u0:.5f})")
    lo, hi = a.value, b.value
    f_lo = a.utilisation - 1.0
    if f_lo == 0.0:
        return lo, "a swept point"
    if b.utilisation == 1.0:
        return hi, "a swept point"
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        f_mid = float(fn(mid)) / n / cap - 1.0
        if f_mid == 0.0:
            lo = hi = mid
            break
        if (f_mid < 0.0) == (f_lo < 0.0):
            lo, f_lo = mid, f_mid
        else:
            hi = mid
        if abs(hi - lo) <= 1e-12 * max(1.0, abs(hi)):
            break
    return 0.5 * (lo + hi), "bisection on the method between the bracketing sweep points"


def check_fenders_in_contact(fenders_in_contact) -> int:
    """The number of fenders sharing the energy: a whole number of 1 or more
    (an int, not a boolean or a float), as :func:`select_fenders` requires."""
    if isinstance(fenders_in_contact, bool) or not isinstance(fenders_in_contact, int) or fenders_in_contact < 1:
        raise ValueError("fenders_in_contact must be a whole number >= 1")
    return fenders_in_contact


def check_limit(name: str, value) -> float | None:
    """A reaction or hull-pressure limit: None (no check) or a finite number
    above 0, the rule :func:`select_fenders` applies to the same limits."""
    return None if value is None else _finite(name, value, positive=True)


def parameter_sweep(
    parameter: str,
    label: str,
    unit: str,
    base_value: float,
    values,
    design_energy,
    fender: FenderRating,
    *,
    range_low: float,
    range_high: float,
    range_basis: str,
    fenders_in_contact: int = 1,
    capacity_knm: float | None = None,
    curve: PerformanceCurve | None = None,
    reaction_limit_kn: float | None = None,
    hull_pressure_limit_kpa: float | None = None,
    reaction_factor: float = 1.0,
) -> Sweep:
    """Evaluate ``design_energy(value)`` over ``values`` and the fender response.

    ``design_energy`` maps one value of the parameter to design energy (kNm)
    with all other inputs at base. Energy per fender = design energy /
    ``fenders_in_contact``; utilisation = energy per fender / capacity
    (default rated energy: this bare function knows no method, so a method
    with a capacity divisor, WG211 gamma_m, needs ``capacity_knm``;
    :func:`standard_sweeps` derives it). Reaction = largest curve reaction reached up to
    that energy (a curve may fall after buckling) x
    ``reaction_factor`` (1 + reaction tolerance, times any velocity and
    temperature factors: :class:`DesignPerformance` reaction / rated
    reaction). Hull pressure = catalogue hull pressure (at rated deflection)
    x ``reaction_factor``; it is not scaled to part load.
    """
    fenders_in_contact = check_fenders_in_contact(fenders_in_contact)
    reaction_limit_kn = check_limit("reaction_limit_kn", reaction_limit_kn)
    hull_pressure_limit_kpa = check_limit("hull_pressure_limit_kpa", hull_pressure_limit_kpa)
    rf = _finite("reaction_factor", reaction_factor, positive=True)
    if capacity_knm is None:
        cap = fender.rated_energy_knm
    else:
        cap = _finite("capacity_knm", capacity_knm, positive=True)
    vals = sorted({float(v) for v in values} | {float(base_value), float(range_low), float(range_high)})
    pts = []
    for v in vals:
        e = float(design_energy(v))
        per = e / fenders_in_contact
        r, d = reaction_at_energy(fender, per, curve, cap)
        hp = None
        if r is not None:
            r *= rf
            if fender.hull_pressure_kpa is not None:
                hp = fender.hull_pressure_kpa * rf
        pts.append(SweepPoint(v, e, per, per / cap, r, d, hp))
    return Sweep(
        parameter=parameter, label=label, unit=unit, base_value=float(base_value),
        base_design_energy_knm=float(design_energy(base_value)), range_low=float(range_low),
        range_high=float(range_high), range_basis=range_basis, points=tuple(pts), capacity_knm=cap,
        reaction_limit_kn=reaction_limit_kn, hull_pressure_limit_kpa=hull_pressure_limit_kpa,
        fenders_in_contact=fenders_in_contact, reaction_factor=rf,
    )


@dataclass(frozen=True)
class TornadoBar:
    parameter: str
    label: str
    low_value: float
    high_value: float
    low_change_pct: float
    high_change_pct: float
    range_basis: str

    @property
    def span_pct(self) -> float:
        return abs(self.high_change_pct - self.low_change_pct)


def tornado(sweeps: list[Sweep]) -> list[TornadoBar]:
    """Change of design energy at each sweep's range ends, largest span first."""
    bars = [
        TornadoBar(s.parameter, s.label, s.range_low, s.range_high,
                   s.change_pct(s.range_low), s.change_pct(s.range_high), s.range_basis)
        for s in sweeps
    ]
    bars.sort(key=lambda b: -b.span_pct)
    return bars


@dataclass(frozen=True)
class SweepRange:
    """Credible range of one input for :func:`standard_sweeps`."""

    low: float
    high: float
    basis: str


SWEEP_KEYS = ("velocity_m_s", "displacement_t", "water_depth_m", "contact_x_over_lbp",
              "berthing_angle_deg", "cs", "cc", "factor")

#: Values the method's own rules can give Cs and Cc: method -> {key: (low, high)}.
#: A coefficient range partly outside these is clipped to them and a range
#: with no value in them is refused (:func:`constrained_sweep_range`); a
#: coefficient the method fixes or removes is not swept (:data:`SWEEP_EXCLUSIONS`).
#: Cc of Ueda and BS 6349-4 is 0.9 to 1.0 under the default PIANC 2002 basis
#: (4.2.7, p. 22); :func:`sweep_coefficient_limits` widens it to 0.8 to 1.0
#: under the SFT table (``cc_basis`` ``sft-2026``).
SWEEP_COEFFICIENT_LIMITS: dict[str, dict[str, tuple[float, float]]] = {
    "pianc2002": {"cs": (0.9, 1.0)},
    "ueda": {"cs": (0.9, 1.0), "cc": (0.9, 1.0)},
    "bs6349": {"cs": (0.9, 1.0), "cc": (0.9, 1.0)},
    "eau": {"cs": (0.9, 1.0)},
    "rom": {"cs": (0.9, 1.0), "cc": (0.9, 1.0)},
    "ufc": {"cc": (0.8, 1.0)},
    "wg211": {},
    "is4651": {},
}

_KEEL_CLEARANCE_CC = ("Cc is 1.0 with the PIANC keel-clearance Cm, which already includes the "
                      "confined-water effect (pairing rule of the manuals: TFADM p. 27, SFT p. 18)")

#: Sweeps a method does not admit, with the reason.
SWEEP_EXCLUSIONS: dict[str, dict[str, str]] = {
    "pianc2002": {"cc": _KEEL_CLEARANCE_CC},
    "eau": {"cc": _KEEL_CLEARANCE_CC},
    "ueda": {},
    "bs6349": {},
    "rom": {},
    "ufc": {"cs": "UFC 4-152-01 takes Cd = 1.0 for larger ships on energy-absorbing fenders"},
    "wg211": {"cs": "WG211 removes Cs", "cc": "WG211 removes Cc"},
    "is4651": {"cs": "IS 4651 fixes Cs at 0.9", "cc": "IS 4651 has no Cc",
               "factor": "IS 4651 applies no abnormal factor"},
}


def inapplicable_sweeps(method: str) -> dict[str, str]:
    """Sweeps :func:`standard_sweeps` leaves out for ``method``, with the reason."""
    if method not in METHODS:
        raise ValueError(f"unknown method {method!r}; one of {tuple(METHODS)}")
    return dict(SWEEP_EXCLUSIONS[method])


#: Cc limits of Ueda and BS 6349-4 under the SFT p. 20 table (``cc_basis`` ``sft-2026``).
_SFT_CC_LIMITS = (0.8, 1.0)


def sweep_coefficient_limits(method: str, cc_basis: str = "pianc2002") -> dict[str, tuple[float, float]]:
    """:data:`SWEEP_COEFFICIENT_LIMITS` of ``method`` under the case's Cc basis."""
    if cc_basis not in CC_BASES:
        raise ValueError(f"cc_basis must be one of {CC_BASES}, got {cc_basis!r}")
    lim = dict(SWEEP_COEFFICIENT_LIMITS[method])
    if cc_basis == "sft-2026" and method in ("ueda", "bs6349"):
        lim["cc"] = _SFT_CC_LIMITS
    return lim


def constrained_sweep_range(method: str, key: str, rng: SweepRange,
                            cc_basis: str = "pianc2002") -> tuple[float, float, str]:
    """``(low, high, basis)`` of ``rng`` as :func:`standard_sweeps` sweeps it
    for ``method``.

    Raises ValueError when the range is not finite or its low is above its
    high. A Cs or Cc range is intersected with
    :data:`SWEEP_COEFFICIENT_LIMITS`: a range partly outside the method's
    values is clipped to the overlap (the basis then says so); a range with
    no value in common with them is refused, since clipping each bound alone
    would invert it and sweep values the method's rule cannot give.
    """
    lo = _finite(f"{key} sweep low", rng.low)
    hi = _finite(f"{key} sweep high", rng.high)
    if lo > hi:
        raise ValueError(f"{key} sweep range: low {lo:g} is above high {hi:g}")
    basis = rng.basis
    limits = sweep_coefficient_limits(method, cc_basis)
    if key in limits:
        a, b = limits[key]
        if hi < a or lo > b:
            raise ValueError(
                f"{key} sweep range {lo:g} to {hi:g} has no value in {a:g} to {b:g}, the values "
                f"{method}'s rule gives; give a range that overlaps it")
        if lo < a or hi > b:
            lo, hi = max(lo, a), min(hi, b)
            basis = f"{basis}; clipped to {a:g} to {b:g}, the values this method's rule gives"
    return lo, hi, basis


def standard_sweeps(
    case: Case,
    method: str,
    fender: FenderRating,
    ranges: dict[str, SweepRange],
    *,
    fenders_in_contact: int = 1,
    capacity_knm: float | None = None,
    curve: PerformanceCurve | None = None,
    reaction_limit_kn: float | None = None,
    hull_pressure_limit_kpa: float | None = None,
    reaction_factor: float = 1.0,
    velocity_m_s: float | None = None,
    factor: float | None = None,
    factor_position: str = "smallest",
    points: int = 25,
    citations: str = "strict",
    citation_root: Path | None = None,
    base: MethodResult | None = None,
) -> list[Sweep]:
    """The standard set of sweeps around the ``method`` result for this case.

    Keys of ``ranges`` (each optional, see :data:`SWEEP_KEYS`):

    * ``velocity_m_s``: the method is re-evaluated at each velocity, so
      velocity-dependent coefficients (ROM accidental factor and Cc) follow;
    * ``displacement_t``: energy only (draft, Cb, velocity and every
      coefficient held), so energy is proportional to displacement;
    * ``water_depth_m`` (Cm through keel clearance, and Cc where the method
      uses it), ``contact_x_over_lbp`` and ``berthing_angle_deg`` (Ce): the
      method is re-evaluated with the berth changed;
    * ``cc``: the method is re-evaluated with Cc given (the ``cc`` override
      of :func:`energy_by_method`), so terms derived from Cc follow (UFC Cm
      through F of the berthing coefficient Ce Cg Cd Cc under the default
      ``ufc_f_basis``);
    * ``cs``, ``factor``: the coefficient alone, others held (no method
      derives another term from them where the sweep applies).

    A sweep in :func:`inapplicable_sweeps` is left out, and a Cs or Cc range
    is clipped to :data:`SWEEP_COEFFICIENT_LIMITS` (the range basis then
    says so). Each range is checked by :func:`constrained_sweep_range`
    before anything is computed: an inverted range, or a Cs or Cc range
    with no value in the method's limits, raises ValueError.

    ``capacity_knm`` is the design energy capacity per fender, used as
    given. When it is omitted, the capacity is the rated energy divided by
    the method's ``capacity_divisor`` (WG211 gamma_m; 1.0 for the other
    methods), so a WG211 utilisation is never taken against the
    characteristic energy. Tolerances and the angular factor are not known
    here; a caller that applies them passes the capacity.

    ``velocity_m_s`` and ``factor`` are the caller's overrides of the base
    case (as in :func:`energy_by_method`). ``reaction_factor`` multiplies the
    reactions and hull pressures before they are compared with the limits.
    The swept values span a little wider than each range.

    Citations follow ``citations`` and ``citation_root`` as in
    :func:`energy_by_method`: the default ``strict`` raises
    :class:`CitationResolutionError` when a cited page is missing or no wiki
    is configured; ``record`` and ``off`` are explicit, flagged opt-outs.
    The base result is checked once; a swept result citing a source the base
    does not is checked as well. ``base`` is a result already checked for
    this case, method and overrides (as the case loader holds): it is
    verified against a recomputation, then reused if its citation status
    meets ``citations`` (re-checked if it was ``record`` and ``strict`` is
    asked; refused if it was not checked and a check is asked). Each sweep
    carries the citation status and opt-out flags of its results.
    """
    if citations not in CITATION_MODES:
        raise ValueError(f"citation mode must be one of {CITATION_MODES}")
    if isinstance(points, bool) or not isinstance(points, int) or points < 2:
        raise ValueError(f"points must be a whole number of 2 or more, got {points!r}")
    # checked here as well, so an empty set of ranges does not skip them
    fenders_in_contact = check_fenders_in_contact(fenders_in_contact)
    reaction_limit_kn = check_limit("reaction_limit_kn", reaction_limit_kn)
    hull_pressure_limit_kpa = check_limit("hull_pressure_limit_kpa", hull_pressure_limit_kpa)
    excluded = inapplicable_sweeps(method)
    # every range is checked and constrained before the base result or any point is computed
    constrained = {}
    for key, rng in ranges.items():
        if key not in SWEEP_KEYS:
            raise KeyError(f"unknown sweep {key!r}")
        if key not in excluded:
            constrained[key] = constrained_sweep_range(method, key, rng, case.cc_basis)
    ship, berth = case.ship, case.berth
    kw = dict(velocity_m_s=velocity_m_s, factor=factor, factor_position=factor_position, citations="off")
    if base is None:
        base = energy_by_method(case, method=method, **{**kw, "citations": citations},
                                citation_root=citation_root)
        status = base.citation_status
    else:
        status = _checked_base_status(case, method, kw, base, citations, citation_root)
    checked = {c: st for c, (_, st) in zip(base.citations, status)}
    v0, f0 = base.velocity_m_s.value, base.factor.value
    cs0, cc0 = base.cs.value, base.cc.value
    e0 = base.design_energy_knm
    if capacity_knm is None:
        # the method's divisor applies to the default as to a caller's capacity
        capacity_knm = fender.rated_energy_knm / base.capacity_divisor
    m0 = ship.displacement_t
    limits = sweep_coefficient_limits(method, case.cc_basis)
    base_keys = set(checked)
    used: list[Citation] = []   # citations of the current sweep beyond the base's

    def energy(c, **o):
        """Design energy of one swept result; any citation the base lacks is checked."""
        res = energy_by_method(c, method=method, **{**kw, **o})
        for cit in res.citations:
            if cit not in checked:
                checked[cit] = resolve_citations((cit,), mode=citations, wiki_root=citation_root)[0][1]
            if cit not in used and cit not in base_keys:
                used.append(cit)
        return res.design_energy_knm

    def with_berth(**b_kw):
        b = Berth(**{**berth.__dict__, **b_kw})
        return energy(replace(case, berth=b))

    builders = {
        "velocity_m_s": ("berthing velocity", "m/s", v0, lambda x: energy(case, velocity_m_s=x)),
        "displacement_t": ("displacement", "t", m0, lambda x: e0 * x / m0),
        "water_depth_m": ("water depth (Cm through keel clearance)", "m", berth.water_depth_m,
                          lambda x: with_berth(water_depth_m=x)),
        "contact_x_over_lbp": ("first contact from bow / LBP (Ce)", "-",
                               berth.contact_x_from_bow_m / ship.lbp_m,
                               lambda x: with_berth(contact_x_from_bow_m=x * ship.lbp_m)),
        "berthing_angle_deg": ("berthing angle (Ce)", "deg", berth.berthing_angle_deg,
                               lambda x: with_berth(berthing_angle_deg=x)),
        "cs": ("softness factor Cs", "-", cs0, lambda x: e0 * x / cs0),
        "cc": ("berth configuration factor Cc", "-", cc0, lambda x: energy(case, cc=x)),
        "factor": ("abnormal / partial energy factor", "-", f0, lambda x: e0 * x / f0),
    }
    out = []
    for key, (r_lo, r_hi, basis) in constrained.items():
        used.clear()
        label, unit, b0, fn = builders[key]
        lo, hi = min(r_lo, b0), max(r_hi, b0)
        pad = 0.15 * (hi - lo) if hi > lo else 0.1 * abs(b0)
        lo_s, hi_s = lo - pad, hi + pad
        if key == "water_depth_m":
            lo_s = max(lo_s, ship.draft_m * 1.001)
        if key == "contact_x_over_lbp":
            lo_s, hi_s = max(lo_s, 0.0), min(hi_s, 1.0)
            # the first contact stays on the end nearer the berth (contact_offset_m)
            if berth.berthing_angle_deg > 0.0:
                hi_s = min(hi_s, 0.5)
            elif berth.berthing_angle_deg < 0.0:
                lo_s = max(lo_s, 0.5)
        if key == "berthing_angle_deg":
            lo_s, hi_s = max(lo_s, -89.0), min(hi_s, 89.0)
            arm = ship.lbp_m / 2.0 - berth.contact_x_from_bow_m
            if arm > 0.0:       # forward contact: bow nearer the berth, angle 0 or above
                lo_s = max(lo_s, 0.0)
            elif arm < 0.0:     # aft contact: stern nearer, angle 0 or below
                hi_s = min(hi_s, 0.0)
        if key in ("velocity_m_s", "displacement_t", "factor"):
            lo_s = max(lo_s, 1e-6)
        if key in limits:
            lo_s, hi_s = max(lo_s, limits[key][0]), min(hi_s, limits[key][1])
        values = [lo_s + (hi_s - lo_s) * i / (points - 1) for i in range(points)]
        sw = parameter_sweep(
            key, label, unit, b0, values, fn, fender, range_low=r_lo, range_high=r_hi,
            range_basis=basis, fenders_in_contact=fenders_in_contact, capacity_knm=capacity_knm,
            curve=curve, reaction_limit_kn=reaction_limit_kn, hull_pressure_limit_kpa=hull_pressure_limit_kpa,
            reaction_factor=reaction_factor,
        )
        cross, how = _crossing(sw, fn, sw.capacity_knm, fenders_in_contact, method=method, base=base,
                               draft_m=ship.draft_m)
        st = tuple(status) + tuple((c.code_id, checked[c]) for c in used)
        out.append(replace(sw, citation_status=st, flags=tuple(_citation_flags(st, citations)),
                           crossing=cross, crossing_basis=how))
    return out


def _checked_base_status(case: Case, method: str, kw: dict, base: MethodResult, mode: str,
                         wiki_root: Path | None) -> tuple[tuple[str, str], ...]:
    """Citation status of a caller's ``base`` for :func:`standard_sweeps`.

    Raises ValueError when ``base`` is not this case's ``method`` result with
    these overrides, or was not checked while ``mode`` asks for a check.
    A ``record`` base is re-checked when ``mode`` is ``strict``.
    """
    if base.method != method:
        raise ValueError(f"base result is for method {base.method!r}, not {method!r}")
    fresh = energy_by_method(case, method=method, **kw)
    same = [(c.name, c.value, c.source) for c in fresh.coefficients()] == \
           [(c.name, c.value, c.source) for c in base.coefficients()]
    if not same or not math.isclose(fresh.design_energy_knm, base.design_energy_knm, rel_tol=1e-12):
        raise ValueError("base result does not match this case and overrides; recompute it or omit base")
    status = base.citation_status
    if mode == "off":
        return status
    if (base.citations != fresh.citations or len(status) != len(base.citations)
            or any(st == "not checked" for _, st in status)):
        raise ValueError("base result's citations were not checked (citations='off', or no sidecar); "
                         f"pass a base checked in mode {mode!r}, or citations='off' explicitly")
    if mode == "strict" and any(st != "resolved" for _, st in status):
        status = resolve_citations(base.citations, mode="strict", wiki_root=wiki_root)
    return status
