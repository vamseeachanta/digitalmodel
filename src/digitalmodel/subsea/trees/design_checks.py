"""API 17D / API 6A subsea-tree pressure-temperature rating envelope check.

A subsea tree (API 17D, which references API 6A for the wellhead/tree pressure-
containing equipment) is procured against a discrete **rated working pressure**
(RWP) and a **temperature class** that together define its pressure-temperature
(P-T) rating envelope.  Before a tree can be installed on a well its rating must
*contain* the well's design conditions: the maximum sustained pressure it will
see at the wellhead (typically the shut-in tubing-head pressure, SITHP, including
any specified design margin) and the maximum/minimum flowing temperatures.

This module provides the deterministic envelope check (no solver, no proprietary
data): given a tree's API 6A rating and the well design conditions, decide whether
the tree is adequately rated and report the spare pressure margin / utilisation.

Standard model (deterministic envelope, API 6A / API 17D)
---------------------------------------------------------
API 6A specifies discrete **rated working pressures** (RWP) for pressure-
containing equipment (Annex/Section: PSL ratings), in psi:

    {2000, 3000, 5000, 10000, 15000, 20000}                              (1)

and discrete **temperature classes**, each a closed operating range
[T_min, T_max] in degrees Fahrenheit (API 6A Table for operating-temperature
ranges):

    K: [-75, 180]   L: [-50, 180]   N: [-50, 140]   P: [-20, 180]
    S: [  0, 140]   T: [  0, 180]   U: [  0, 250]   V: [ 35, 250]
    X: [  0, 350]   Y: [  0, 650]                                        (2)

Within its rated temperature class the equipment is qualified to hold its full
RWP (API 6A does not derate the RWP inside the rated class; instead a higher
temperature requires selecting a higher temperature class).  The tree is
**pressure-adequate** when the well design pressure (with margin already applied
by the caller) does not exceed the RWP:

    p_design <= RWP                                            (PRESSURE OK)  (3)

The tree is **temperature-adequate** when the well's design temperature range is
contained within the class envelope:

    T_min_class <= T_min_well   and   T_max_well <= T_max_class   (TEMP OK)  (4)

The tree **passes** the API 17D envelope check when both (3) and (4) hold.  The
pressure margin and utilisation are:

    margin      = RWP - p_design                                          (5)
    utilisation = p_design / RWP                                          (6)

A full FEA / material-qualification (API 6A PSL/PR levels, load-case combination
with bending and external pressure) is deferred (see package docstring); this is
the screening-level rating-containment check.

Units: pressures in psi, temperatures in degrees Fahrenheit (the API 6A native
units for these tabulated ratings).
"""

from __future__ import annotations

from dataclasses import dataclass

# API 6A discrete rated working pressures (psi) — eq. (1).
API_6A_RATED_WORKING_PRESSURES_PSI: tuple[int, ...] = (
    2000,
    3000,
    5000,
    10000,
    15000,
    20000,
)

# API 6A temperature classes: class -> (T_min_F, T_max_F) — eq. (2).
API_6A_TEMPERATURE_CLASSES_F: dict[str, tuple[float, float]] = {
    "K": (-75.0, 180.0),
    "L": (-50.0, 180.0),
    "N": (-50.0, 140.0),
    "P": (-20.0, 180.0),
    "S": (0.0, 140.0),
    "T": (0.0, 180.0),
    "U": (0.0, 250.0),
    "V": (35.0, 250.0),
    "X": (0.0, 350.0),
    "Y": (0.0, 650.0),
}


@dataclass(frozen=True)
class TreeRatingResult:
    """Outcome of the API 17D subsea-tree P-T rating envelope check.

    Attributes
    ----------
    rwp:
        Rated working pressure of the tree (psi).
    temperature_class:
        API 6A temperature class designator used for the check.
    t_class_min, t_class_max:
        Operating-temperature envelope of the class (deg F).
    pressure_ok:
        ``True`` when ``p_design <= rwp`` (eq. 3).
    temperature_ok:
        ``True`` when the well temperature range is contained (eq. 4).
    margin:
        Spare pressure ``rwp - p_design`` (psi); negative => under-rated (eq. 5).
    utilisation:
        ``p_design / rwp`` (<= 1.0 means pressure-adequate) (eq. 6).
    passed:
        ``True`` only when both ``pressure_ok`` and ``temperature_ok``.
    """

    rwp: int
    temperature_class: str
    t_class_min: float
    t_class_max: float
    pressure_ok: bool
    temperature_ok: bool
    margin: float
    utilisation: float
    passed: bool


def select_rated_working_pressure(p_design: float) -> int:
    """Return the smallest API 6A RWP that contains ``p_design`` (psi).

    Implements the standard procurement step: choose the lowest discrete API 6A
    rated working pressure (eq. 1) that is >= the well design pressure.

    Raises
    ------
    ValueError
        If ``p_design`` is negative or exceeds the highest API 6A rating.
    """
    if p_design < 0.0:
        raise ValueError(f"p_design must be >= 0, got {p_design!r}")
    for rwp in API_6A_RATED_WORKING_PRESSURES_PSI:
        if p_design <= rwp:
            return rwp
    raise ValueError(
        f"p_design {p_design!r} psi exceeds the highest API 6A rating "
        f"({API_6A_RATED_WORKING_PRESSURES_PSI[-1]} psi)"
    )


def tree_rating_check(
    p_design: float,
    rwp: int,
    temperature_class: str,
    t_well_min: float,
    t_well_max: float,
) -> TreeRatingResult:
    """Check a subsea tree's API 6A / 17D P-T rating against well conditions.

    Parameters
    ----------
    p_design:
        Well design pressure at the wellhead (psi), e.g. SITHP with the
        caller-applied design margin already included; must be >= 0.
    rwp:
        Tree rated working pressure (psi); must be one of the discrete API 6A
        ratings in :data:`API_6A_RATED_WORKING_PRESSURES_PSI`.
    temperature_class:
        API 6A temperature-class designator (one key of
        :data:`API_6A_TEMPERATURE_CLASSES_F`), case-insensitive.
    t_well_min, t_well_max:
        Minimum and maximum well design temperatures (deg F); ``t_well_min``
        must not exceed ``t_well_max``.

    Returns
    -------
    TreeRatingResult
        Envelope-check outcome (eqs. 3-6).

    Raises
    ------
    ValueError
        On a negative ``p_design``, an RWP not in the API 6A list, an unknown
        temperature class, or ``t_well_min > t_well_max``.
    """
    if p_design < 0.0:
        raise ValueError(f"p_design must be >= 0, got {p_design!r}")
    if rwp not in API_6A_RATED_WORKING_PRESSURES_PSI:
        raise ValueError(
            f"rwp {rwp!r} is not an API 6A rated working pressure "
            f"{API_6A_RATED_WORKING_PRESSURES_PSI}"
        )
    cls = temperature_class.upper()
    if cls not in API_6A_TEMPERATURE_CLASSES_F:
        raise ValueError(
            f"unknown API 6A temperature class {temperature_class!r}; "
            f"valid classes: {sorted(API_6A_TEMPERATURE_CLASSES_F)}"
        )
    if t_well_min > t_well_max:
        raise ValueError(
            f"t_well_min ({t_well_min}) must be <= t_well_max ({t_well_max})"
        )

    t_class_min, t_class_max = API_6A_TEMPERATURE_CLASSES_F[cls]

    pressure_ok = p_design <= rwp                                  # eq. (3)
    temperature_ok = (t_class_min <= t_well_min) and (
        t_well_max <= t_class_max
    )                                                              # eq. (4)
    margin = rwp - p_design                                        # eq. (5)
    utilisation = p_design / rwp if rwp else float("inf")          # eq. (6)
    passed = pressure_ok and temperature_ok

    return TreeRatingResult(
        rwp=rwp,
        temperature_class=cls,
        t_class_min=t_class_min,
        t_class_max=t_class_max,
        pressure_ok=pressure_ok,
        temperature_ok=temperature_ok,
        margin=margin,
        utilisation=utilisation,
        passed=passed,
    )
