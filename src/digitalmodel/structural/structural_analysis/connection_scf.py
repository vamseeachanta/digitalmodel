# ABOUTME: Connection & bracket stress-concentration (DNV-RP-C203) — local
# ABOUTME: detail SCFs + weld-toe hot-spot stress at loaded ship connections.
"""Connection & bracket stress-concentration factors (DNV-RP-C203).

Local fatigue-detail checks at the loaded connections of a ship/offshore hull:

* **Frame-to-girder / girder-to-shell junctions** — plate butt/cruciform
  junctions where a thickness step or fit-up eccentricity raises the stress
  (axial-misalignment SCF).
* **Soft-toe brackets** — bracket toes welded to plating; the ground/tapered
  ("soft") toe lowers the weld-toe SCF relative to a square ("hard") toe.
* **Attachment lugs / tubular brace stubs** — a loaded tubular attachment on a
  chord, governed by the Efthymiou parametric SCF (chord-saddle hot spot).

Design philosophy — this module is a thin *ship-detail orchestration* layer; it
reimplements **no** SCF physics. It reuses the validated fatigue library:

* :mod:`digitalmodel.fatigue.scf_library` — misalignment / cruciform / fillet-toe
  SCF formulae and the Efthymiou tubular set.
* :mod:`digitalmodel.fatigue.hotspot_stress` — DNV-RP-C203 surface-stress
  extrapolation to the weld toe.
* :mod:`digitalmodel.fatigue.weld_classification` — DNV-RP-C203 Tables 2-1..2-8
  S-N detail class for the connection.

DNV-RP-C203 handles welded ship attachments/brackets mainly by **S-N detail
classification** (E / F / F1 / F3 ...), *not* by one multiplicative SCF. So a
connection assessment here returns **both** a detail class (from the weld
classifier) **and** the discontinuity SCF (misalignment / toe / tubular) where
applicable. The plate thickness-effect (size) correction is deliberately kept
OUT of the SCF — it belongs in the S-N (fatigue-strength) layer.

Units: mm (lengths), MPa (stresses).

References
----------
- DNV-RP-C203 (2021), Sec. 2.3 (hot-spot stress = SCF x nominal), Sec. 4.3
  (extrapolation), App. B (Efthymiou tubular), App. C/D (plate SCFs).
- Efthymiou, M. (1988), "Development of SCF formulae...", OTC, Surrey.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.fatigue.hotspot_stress import extrapolate_hotspot_linear
from digitalmodel.fatigue.scf_library import (
    scf_butt_weld_misalignment,
    scf_cruciform_joint,
    scf_fillet_weld_toe,
)
from digitalmodel.fatigue.weld_classification import (
    WeldDetail,
    classify_weld_detail,
)

CODE_REFERENCE = "DNV-RP-C203"


# ---------------------------------------------------------------------------
# Hot-spot stress = SCF x nominal  (DNV-RP-C203 Sec. 2.3)
# ---------------------------------------------------------------------------
def hot_spot_stress(scf: float, nominal_stress_mpa: float) -> float:
    """Weld-toe hot-spot stress from a stress-concentration factor.

    DNV-RP-C203 Sec. 2.3: ``sigma_hotspot = SCF * sigma_nominal``.

    Parameters
    ----------
    scf : float
        Stress-concentration factor (>= 1.0).
    nominal_stress_mpa : float
        Nominal (far-field) stress at the connection (MPa).
    """
    if scf < 1.0:
        raise ValueError("SCF must be >= 1.0")
    return scf * nominal_stress_mpa


# ---------------------------------------------------------------------------
# Frame-to-girder / girder-to-shell junction — axial-misalignment SCF
# ---------------------------------------------------------------------------
def misalignment_scf(
    thickness_mm: float,
    eccentricity_mm: float,
    *,
    restrained: bool = False,
    t2_mm: float | None = None,
) -> float:
    """SCF from fit-up eccentricity at a plate junction (DNV-RP-C203 App. D/C).

    The restraint condition sets the bending amplification of an axial
    misalignment ``e`` over thickness ``t``:

    * ``restrained=False`` — a **pinned** / single-sided butt junction (free to
      rotate): ``SCF = 1 + 3 e/t`` (reuses
      :func:`...scf_library.scf_butt_weld_misalignment`).
    * ``restrained=True`` — a **restrained** junction held straight by adjoining
      structure (e.g. cruciform / fixed-ends, as at a girder-to-shell tee):
      ``SCF = 1 + 6 e/t`` (reuses the ``km`` term of
      :func:`...scf_library.scf_cruciform_joint`).

    Parameters
    ----------
    thickness_mm : float
        Plate thickness ``t`` (mm).
    eccentricity_mm : float
        Linear (axial) misalignment ``e`` (mm).
    restrained : bool
        Restraint condition (see above). Default ``False`` (pinned).
    t2_mm : float, optional
        Thickness of the second plate (mm) for the pinned case, to include the
        thickness-step term. Defaults to ``thickness_mm`` (pure misalignment).
    """
    if restrained:
        # a_weld >= t -> the cruciform weld-geometry factor kw collapses to 1.0,
        # leaving the restrained misalignment km = 1 + 6 e/t.
        return scf_cruciform_joint(thickness_mm, thickness_mm, eccentricity_mm)
    t2 = thickness_mm if t2_mm is None else t2_mm
    return scf_butt_weld_misalignment(thickness_mm, t2, eccentricity_mm)


# ---------------------------------------------------------------------------
# Soft-toe vs hard-toe bracket — weld-toe SCF
# ---------------------------------------------------------------------------
# Representative weld-toe geometry for ship bracket toes. A ground/tapered
# "soft" toe presents a shallow flank angle and a generous effective radius; a
# square "hard" toe is a steep, near-sharp toe. Values feed the validated
# IIW/DNV fillet-toe SCF (scf_library.scf_fillet_weld_toe).
_HARD_TOE_ANGLE_DEG = 45.0
_HARD_TOE_RADIUS_MM = 1.0
_SOFT_TOE_ANGLE_DEG = 20.0
_SOFT_TOE_RADIUS_MM = 4.0


def bracket_toe_scf(
    thickness_mm: float,
    *,
    soft_toe: bool = False,
    toe_angle_deg: float | None = None,
    toe_radius_mm: float | None = None,
) -> float:
    """Weld-toe SCF at a bracket toe (DNV-RP-C203 App. C; IIW-2259-15).

    Reuses :func:`...scf_library.scf_fillet_weld_toe`. A soft (ground/tapered)
    toe has a shallower flank angle and larger effective radius than a hard
    (square) toe, so ``bracket_toe_scf(..., soft_toe=True) <
    bracket_toe_scf(..., soft_toe=False)`` for the same plate.

    Parameters
    ----------
    thickness_mm : float
        Attached-plate thickness ``t`` (mm).
    soft_toe : bool
        Select the soft-toe geometry preset. Default ``False`` (hard toe).
    toe_angle_deg, toe_radius_mm : float, optional
        Override the preset weld-toe flank angle / radius (mm).
    """
    angle = toe_angle_deg
    radius = toe_radius_mm
    if angle is None:
        angle = _SOFT_TOE_ANGLE_DEG if soft_toe else _HARD_TOE_ANGLE_DEG
    if radius is None:
        radius = _SOFT_TOE_RADIUS_MM if soft_toe else _HARD_TOE_RADIUS_MM
    return scf_fillet_weld_toe(thickness_mm, angle, radius)


# ---------------------------------------------------------------------------
# Attachment lug / tubular brace stub — Efthymiou chord-saddle SCF
# ---------------------------------------------------------------------------
def efthymiou_chord_saddle_axial_scf(
    beta: float,
    gamma: float,
    tau: float,
    theta_deg: float = 90.0,
) -> float:
    """Chord-saddle SCF of an axially-loaded tubular T/Y joint (Efthymiou).

    DNV-RP-C203 App. B / Efthymiou (1988), chord-saddle term::

        SCF = gamma * tau^1.1 * (1.11 - 3(beta - 0.52)^2) * sin(theta)^1.6

    with non-dimensional geometry ``beta = d/D``, ``gamma = D/(2T)``,
    ``tau = t/T``. This is the **published tau^1.1** form. Note that the
    repository's :func:`...scf_library.efthymiou_ty_axial` uses ``tau^1.0`` and
    therefore returns a value ~7% higher; this function reproduces the
    published value for a loaded attachment lug / brace stub.

    Parameters
    ----------
    beta : float
        Brace/chord diameter ratio ``d/D`` (0.2..1.0).
    gamma : float
        Chord radius/thickness ratio ``D/(2T)`` (typ. 8..32).
    tau : float
        Brace/chord thickness ratio ``t/T`` (0.2..1.0).
    theta_deg : float
        Brace-to-chord angle (degrees). Default 90.
    """
    if beta <= 0 or gamma <= 0 or tau <= 0:
        raise ValueError("beta, gamma, tau must be > 0")
    sin_th = math.sin(math.radians(theta_deg))
    scf = gamma * tau**1.1 * (1.11 - 3.0 * (beta - 0.52) ** 2) * sin_th**1.6
    return max(scf, 1.0)


# ---------------------------------------------------------------------------
# Hot-spot stress at a connection by surface-stress extrapolation
# ---------------------------------------------------------------------------
def junction_hotspot_stress(
    plate_thickness_mm: float,
    stress_at_04t_mpa: float,
    stress_at_10t_mpa: float,
) -> float:
    """Weld-toe hot-spot stress by 2-point linear extrapolation.

    DNV-RP-C203 Eq. 4.1, reused from
    :func:`...hotspot_stress.extrapolate_hotspot_linear`::

        sigma_hs = 1.67 sigma(0.4t) - 0.67 sigma(1.0t)
    """
    return extrapolate_hotspot_linear(
        plate_thickness_mm, stress_at_04t_mpa, stress_at_10t_mpa
    ).hotspot_stress


# ---------------------------------------------------------------------------
# Connection assessment — detail class + SCF + hot-spot stress
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class ConnectionSCFResult:
    """Local stress-concentration assessment of a loaded connection.

    Attributes
    ----------
    detail : str
        Free-text description of the connection detail.
    scf : float
        Governing discontinuity SCF applied to the nominal stress.
    nominal_stress_mpa : float
        Nominal (far-field) stress at the connection (MPa).
    hot_spot_stress_mpa : float
        Weld-toe hot-spot stress = ``scf * nominal_stress_mpa`` (MPa).
    sn_class : str
        DNV-RP-C203 S-N detail class (Tables 2-1..2-8) for the connection.
    sn_table : str
        DNV-RP-C203 table the class was taken from.
    notes : str
        Classifier notes / caveats.
    code_reference : str
        Governing code — ``"DNV-RP-C203"``.
    """

    detail: str
    scf: float
    nominal_stress_mpa: float
    hot_spot_stress_mpa: float
    sn_class: str
    sn_table: str = ""
    notes: str = ""
    code_reference: str = CODE_REFERENCE


def assess_connection(
    detail: str,
    nominal_stress_mpa: float,
    scf: float,
    *,
    joint_type: str = "",
    transverse_load: bool = True,
    full_penetration: bool = True,
    ground_flush: bool = False,
    inspected: bool = False,
) -> ConnectionSCFResult:
    """Assess a loaded connection: S-N detail class + SCF + hot-spot stress.

    Combines the DNV-RP-C203 detail classification (S-N layer, from the weld
    classifier) with a supplied discontinuity ``scf`` (this module's
    misalignment / bracket-toe / tubular helpers) to produce the weld-toe
    hot-spot stress.

    Parameters
    ----------
    detail : str
        Description of the connection (e.g. ``"bracket toe attachment"``).
    nominal_stress_mpa : float
        Nominal stress at the connection (MPa).
    scf : float
        Discontinuity SCF (>= 1.0) from a helper in this module.
    joint_type : str
        Optional joint-type hint for the classifier (``"attachment"``,
        ``"cruciform"``, ``"butt"``, ``"fillet"``, ``"tubular"``...).
    transverse_load, full_penetration, ground_flush, inspected : bool
        Weld attributes forwarded to the classifier.
    """
    classification = classify_weld_detail(
        WeldDetail(
            description=detail,
            joint_type=joint_type,
            transverse_load=transverse_load,
            full_penetration=full_penetration,
            ground_flush=ground_flush,
            inspected=inspected,
        )
    )
    return ConnectionSCFResult(
        detail=detail,
        scf=scf,
        nominal_stress_mpa=nominal_stress_mpa,
        hot_spot_stress_mpa=hot_spot_stress(scf, nominal_stress_mpa),
        sn_class=classification.dnv_class,
        sn_table=classification.dnv_table,
        notes=classification.notes,
    )
