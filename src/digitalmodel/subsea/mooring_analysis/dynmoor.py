"""Legacy DYNMOOR 3-DOF dynamic mooring analysis - static + passing-ship port.

Port of a legacy in-house Fortran mooring program ("DYNMOOR", v4.10 1986
through v6.10 March 2007, single-file Fortran 77). The full program is a
3-degree-of-freedom (surge/sway/yaw) nonlinear time-domain simulation of a
moored vessel under wind, current, waves, wave drift and passing-ship
excitation, with up to 30 mooring lines of up to 3 mixed-material segments
(chain / wire / nylon), fender force-deflection curves and anchor pulls.

This module ports the complete *static* chain and the *passing-ship force
models* faithfully (see the TODO note at the bottom for the time-domain
integrator, which is deliberately not yet ported):

1. **Mooring-line load-excursion tables** (subroutines ``CATINB``,
   ``FULCAT``, ``CATNRY``, ``STRETC``, ``SUB``): each line type is reduced
   to an ``N = 50``-row table of horizontal fairlead tension ``H``, total
   fairlead tension ``T``, horizontal fairlead-to-anchor distance ``U``,
   horizontal anchor pull ``HA``, grounded line length ``Z`` and vertical
   anchor pull ``P``. Row ``j`` (1-based, rows 2..N) is evaluated at
   ``H = BS_min * ((j-1)/(N-1))**2`` (``FULCAT`` uses
   ``(N-1)/0.8165`` in the denominator so the table tops out at 2/3 of
   breaking strength).

   * ``CATINB`` - single segment to a drag anchor on a (possibly sloped)
     bottom with bottom friction;
   * ``FULCAT`` - single segment to an elevated pile/bollard (full
     catenary, both ends off bottom);
   * ``CATNRY`` - three segments with optional buoy at the seg2/seg3
     junction and clump weight at the seg1/seg2 junction (Newton iteration
     on the non-dimensional fairlead slope ``XI``).

2. **Table interpolation** (``INTERP``, ``ANPULL``, ``QINTER`` and the
   main-program ``QINTRP`` statement function): quadratic 3-point
   interpolation (linear near the table foot).

3. **Pretension setup**: pretension in percent of minimum segment breaking
   strength -> horizontal fairlead-anchor distance and anchor coordinates.

4. **Static restoring force** (``RFORCE``): line tensions and body-frame
   surge/sway forces + yaw moment for a vessel offset (x, y, yaw).

5. **Fender forces** (``FENDER``): piecewise-linear force-deflection.

6. **Static wind force** (``WINDAF``/``WCOEF`` + the wind term of
   ``OSCILL``): per-area 100-knot forces ``0.0338 * Ch * Cs * A`` (kips)
   scaled by ``V**2 / 10`` (lb) and resolved by wind angle off the bow.

7. **Passing-ship forces**:

   * ``PARSHIP`` (parallel passing, Wang 1975 slender-body method): double
     Simpson integration (fixed ``NT = 50`` panels) over both hulls with
     parabolic sectional-area distributions, finite-depth image sources
     ``n = -10..10``;
   * ``PERSHIP`` (perpendicular passing, v6.00 Nov 2006): regression force
     coefficients and piecewise quartic/quintic stagger shape functions
     fitted to U.S. Naval Academy model data (D. Kriebel, 2005/2006).

Units are the legacy English set throughout: ft, lb (tables and forces),
kips (inputs/reports), knots (wind/current/ship speeds), degrees clockwise
from the bow for directions. ``g = 32.174 ft/s**2``, seawater
``rho = 1.99 slug/ft**3`` (``1.9905`` inside ``PARSHIP``,
``62.4 lb/ft**3`` inside ``PERSHIP``) - preserved as in the source.

Faithfulness notes (all deliberate):

* All arithmetic is float64. The legacy binary is default ``REAL*4``;
  agreement with it is therefore limited to single precision (~1e-6
  relative), which the validation tolerances reflect.
* ``PARSHIP`` legacy quirks are preserved: the truncated
  ``PI = 3.1415279`` (a typo in the source), and the *implicit INTEGER*
  passing-ship length ``L2`` in blank COMMON, which truncates the passing
  ship length to a whole foot and makes the Simpson step
  ``step2 = L2/NT`` an integer division.
* ``CATINB`` computes the nonlinear-spring constant as ``RSP = SK2/(2 SK1)``
  while ``CATNRY`` computes ``RSP = SK1/(2 SK2)`` and normalises the spring
  load by the *segment-minimum* breaking strength instead of the spring's
  own - both preserved exactly as the source has them.
* Off-table lookups: the legacy code indexes one row past the populated
  table (reading zero-initialised memory); this port clamps to the last
  table row instead and, for pretensions below the second table row,
  raises instead of dereferencing row 0.
* ``WCOEF`` leaves the height coefficient undefined for a height of
  exactly 50 ft; this port treats ``h <= 50`` as 1.0.

TODO(dynmoor-time-domain): the time-domain integrator (``OSCILL`` -
semi-implicit Euler with linearised damping ``C(J)``), the random-sea
Fourier synthesis (``FOURIE``/``OMAP``/``SPECT``/``REGWAV``), the Morison
panel loads on the hull sub-areas (``AGEN``/``WFCE``/``AFORCE``/``WAVEL``),
the four wave-drift options (``DRIFTF``/``WAVEF``/``WFORCE``/``SEMID``/
``DIMM``), wind gusting (``WNGUST``) and the peak statistics (``SIG``:
average / significant / 1/10 / 1/100-highest excursions and tensions) are
NOT yet ported. ``tests/subsea/mooring_analysis/test_dynmoor.py`` carries
a skipped test skeleton for that follow-up.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional, Sequence

__all__ = [
    "GRAVITY_FT_S2",
    "N_TABLE",
    "WATER_WEIGHT_FACTORS",
    "KNOTS_TO_FPS_MAIN",
    "KNOTS_TO_FPS_ENVIN",
    "DynmoorSegment",
    "NonlinearSpring",
    "DynmoorLineType",
    "DynmoorLine",
    "LineTable",
    "Fender",
    "WindArea",
    "WindLoadModel",
    "PassingForce",
    "RestoringForceResult",
    "PretensionSolution",
    "build_line_table",
    "interp_table",
    "anchor_pull",
    "qinter_table",
    "solve_pretension",
    "anchor_position",
    "restoring_force",
    "fender_forces",
    "wind_height_coefficient",
    "build_wind_model",
    "static_wind_force",
    "anchor_holding_power",
    "parship_forces",
    "pership_forces",
    "passing_ship_sweep",
]

# Constants exactly as assigned in the legacy main program.
GRAVITY_FT_S2 = 32.174
N_TABLE = 50  # rows per load-excursion table ("SCM CHANGED N TO 50, DEC 1998")

# Weight-in-water factors applied to weight-in-air per material (main, DO 20).
WATER_WEIGHT_FACTORS = {"chain": 0.87, "wire": 0.795, "nylon": 1.0}

# knots -> ft/s. The main program uses 1.6888 for the passing ship speed,
# ENVIN uses 1.6889 for the current. Both preserved.
KNOTS_TO_FPS_MAIN = 1.6888
KNOTS_TO_FPS_ENVIN = 1.6889

_DEG = math.pi / 180.0  # FAC2 = 3.1415927/180. in the source


def _func1(a: float) -> float:
    """FUNC1(A) = SQRT(1 + A*A)."""
    return math.sqrt(1.0 + a * a)


def _func2(a: float) -> float:
    """FUNC2(A) = ALOG(A + SQRT(1 + A*A)) (i.e. asinh)."""
    return math.log(a + _func1(a))


def _func3(a: float, b: float, c: float, d: float) -> float:
    """FUNC3(A,B,C,D) = (A + B*SQRT(1 + 4 D^2 / (C^2 (B^2 - A^2)))) * C / 2."""
    return (a + b * math.sqrt(1.0 + 4.0 * d * d / (c * c * (b * b - a * a)))) * c / 2.0


def _qintrp(d, a, b, c, fa, fb, fc):
    """QINTRP statement function: 3-point Lagrange quadratic interpolation."""
    return (
        (d - b) * (d - c) / ((a - b) * (a - c)) * fa
        + (d - a) * (d - c) / ((b - a) * (b - c)) * fb
        + (d - a) * (d - b) / ((c - a) * (c - b)) * fc
    )


# ---------------------------------------------------------------------------
# Input model
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class DynmoorSegment:
    """One mooring-line segment (a TYPE1/TYPE2/TYPE3 input card).

    ``length_ft`` under 5 ft on segment 2 marks a single-segment line
    (legacy convention ``S2 < 5``). ``modulus_ksi`` is the apparent elastic
    modulus in ksi; ``EA = area * modulus * 1000`` lb as in the source.
    """

    material: str = "none"  # chain | wire | nylon | none
    length_ft: float = 0.0
    weight_air_lb_ft: float = 0.0  # W0, weight in air, lb/ft
    area_in2: float = 0.0  # metallic cross-section, in^2
    modulus_ksi: float = 0.0  # apparent elastic modulus, ksi
    breaking_strength_kips: float = 0.0
    size_in: float = 0.0  # nominal size (report only)

    @property
    def weight_water_lb_ft(self) -> float:
        """Weight in water, lb/ft (legacy material factors)."""
        factor = WATER_WEIGHT_FACTORS.get(self.material.lower())
        if factor is None:  # 'none' or unknown -> weight as given
            return self.weight_air_lb_ft
        return self.weight_air_lb_ft * factor

    @property
    def ea_lb(self) -> float:
        """Axial stiffness EA in lb (A[in^2] * E[ksi] * 1000)."""
        return self.area_in2 * self.modulus_ksi * 1000.0


@dataclass(frozen=True)
class NonlinearSpring:
    """Nonlinear (synthetic) line segment characteristics (TYPESP card).

    Quadratic load-stretch: at ``p1`` percent of the spring breaking
    strength the segment stretches ``ep1`` percent, at ``p2`` -> ``ep2``.
    """

    length_ft: float  # SLSP
    breaking_strength_kips: float  # BRSP
    ep1: float  # percent stretch at p1
    p1: float  # percent of breaking strength
    ep2: float
    p2: float


@dataclass(frozen=True)
class DynmoorLineType:
    """One line type (the 8-card group of the legacy deck).

    ``segment1`` is the anchor-end segment, ``segment2`` the middle
    (seg1-buoy), ``segment3`` the top (buoy-fairlead). ``water_depth_ft``
    (WDEP) is the vertical distance anchor to fairlead. ``pile_height_ft``
    (ZDEP) > 0 selects the pile/bollard full-catenary model for
    single-segment lines. ``bottom_slope_deg`` (ALPHA) tilts the bottom for
    the single-segment anchor model.
    """

    segment1: DynmoorSegment
    segment2: DynmoorSegment = DynmoorSegment()
    segment3: DynmoorSegment = DynmoorSegment()
    spring: Optional[NonlinearSpring] = None
    water_depth_ft: float = 0.0  # WDEP: vertical distance anchor->fairlead
    pile_height_ft: float = 0.0  # ZDEP
    bottom_slope_deg: float = 0.0  # ALPHA (deg)
    bottom_friction_percent: float = 0.0  # FRIC input (percent)
    anchor_weight_kips: float = 0.0  # WA0
    anchor_mud_efficiency: float = 0.0  # AEF
    anchor_sand_efficiency: float = 0.0  # SEF
    buoy_buoyancy_kips: float = 0.0  # WB0 -> WB = WB0*1000 lb
    clump_weight_kips: float = 0.0  # WC0 -> WC = WC0*870 lb (in-water chain factor)

    @property
    def brkm_kips(self) -> float:
        """Governing (minimum non-zero) breaking strength, kips (BRKM)."""
        brkm = self.segment1.breaking_strength_kips
        for candidate in (
            self.segment2.breaking_strength_kips,
            self.segment3.breaking_strength_kips,
            self.spring.breaking_strength_kips if self.spring else 0.0,
        ):
            if candidate != 0.0 and candidate < brkm:
                brkm = candidate
        return brkm

    @property
    def friction(self) -> float:
        """Bottom friction coefficient (FRIC/100)."""
        return self.bottom_friction_percent / 100.0


@dataclass(frozen=True)
class DynmoorLine:
    """One mooring line (fairlead card): geometry + pretension."""

    line_type: int  # 1-based index into the line-type list (ITYPE)
    fairlead_x_ft: float  # XSP, + fwd of midship
    fairlead_y_ft: float  # YSP, + to starboard of CL
    angle_deg: float  # THETA, deg clockwise from bow
    pretension_percent: float  # TI, percent of BRKM


@dataclass
class LineTable:
    """Load-excursion table for one line type (COMMON /TENS/ row).

    All lists have ``N_TABLE`` entries; index 0 is the slack row. Units:
    lb and ft.
    """

    h: list[float]  # HC: horizontal tension at fairlead, lb
    t: list[float]  # T:  total (maximum) line tension, lb
    u: list[float]  # U:  horizontal distance fairlead->anchor, ft
    ha: list[float]  # HFA: horizontal pull on anchor, lb
    z: list[float]  # XFL: line length lying on bottom, ft
    p: list[float]  # VFA: vertical pull on anchor, lb
    model: str = ""  # catinb | fulcat | catnry
    total_length_ft: float = 0.0  # S1+S2+S3 (for suspended-length reports)


# ---------------------------------------------------------------------------
# Nonlinear spring helper (shared preamble of CATINB / FULCAT / CATNRY)
# ---------------------------------------------------------------------------


def _spring_constants(spring: Optional[NonlinearSpring], variant: str):
    """Return (sk1, rsp, brspr) exactly as each subroutine computes them.

    ``variant`` is "catinb"/"fulcat" (RSP = SK2/(2 SK1), normalised by the
    spring breaking strength) or "catnry" (RSP = SK1/(2 SK2), normalised by
    the segment-minimum breaking strength) - a legacy inconsistency that is
    preserved.
    """
    if spring is None or spring.ep1 == 0.0:
        return 1.0, 0.0, 1.0
    ep1, ep2 = spring.ep1, spring.ep2
    p1, p2 = spring.p1, spring.p2
    den = ep1 * ep2 * (ep1 - ep2)
    sk1 = (p1 * ep2 - p2 * ep1) / den
    sk2 = (p2 * ep1 * ep1 - p1 * ep2 * ep2) / den
    if variant == "catnry":
        rsp = 0.5 * sk1 / sk2
    else:
        rsp = 0.5 * sk2 / sk1
    brspr = spring.breaking_strength_kips * 1000.0
    return sk1, rsp, brspr


def _spring_stretch(h: float, sk1: float, rsp: float, brspr: float) -> float:
    """STR = -RSP + SQRT(RSP**2 + H*100/(SK1*BRSPR)) (percent stretch)."""
    return -rsp + math.sqrt(rsp * rsp + h * 100.0 / (sk1 * brspr))


# ---------------------------------------------------------------------------
# Load-excursion tables (CATINB / FULCAT / CATNRY)
# ---------------------------------------------------------------------------


def _catinb(lt: DynmoorLineType) -> LineTable:
    """Subroutine CATINB: single segment to a drag anchor on the bottom."""
    n = N_TABLE
    fac = n - 1.0
    seg = lt.segment1
    s1 = seg.length_ft
    w = seg.weight_water_lb_ft
    ea = seg.ea_lb
    if ea == 0.0:
        ea = 1.0e20
    za = lt.water_depth_ft
    fric = lt.friction
    alpha = lt.bottom_slope_deg * _DEG
    brkmp = lt.brkm_kips * 1000.0
    cc = math.cos(alpha)
    ss = math.sin(alpha)
    c1 = ss / cc
    fact = ss + cc * fric

    sk1, rsp, brspr = _spring_constants(lt.spring, "catinb")
    has_spring = lt.spring is not None and lt.spring.ep1 != 0.0
    sp = lt.spring.length_ft if has_spring else 0.0

    h = [0.0] * n
    t = [0.0] * n
    u = [0.0] * n
    ha = [0.0] * n
    z = [0.0] * n
    p = [0.0] * n

    for j in range(1, n):  # Fortran rows 2..N
        hj = brkmp * (j / fac) ** 2
        h[j] = hj
        c = hj / w
        c2 = -c * _func1(c1)
        c3 = -c * _func2(c1)
        c4 = za - s1 * ss - c2
        c5 = 1.0 - ss * ss
        c6 = 2.0 * (c * c1 - c4 * ss)
        c7 = c * c * (1.0 + c1 * c1) - c4 * c4
        s = (-c6 + math.sqrt(c6 * c6 - 4.0 * c5 * c7)) / (2.0 * c5)
        if s <= s1:
            # part of the line lies on the bottom
            x0 = c * _func2(s / c + c1) + c3
            z[j] = s1 - s
            ue, han = _stretc(hj, s1, w, z[j], ea, fact, cc)
            ha[j] = han
            t[j] = _func1(s / c + c1) * hj
            xh = x0 + z[j] * cc
            u[j] = xh + ue
            if has_spring:
                stretch = _spring_stretch(hj, sk1, rsp, brspr)
                u[j] = xh + stretch * sp / 100.0
            p[j] = 0.0
        else:
            # fully suspended: vertical pull on the anchor
            y = math.sqrt(s1 * s1 - za * za) / (2.0 * c)
            t[j] = _func3(za, s1, w, hj)
            vt = math.sqrt(t[j] ** 2 - hj * hj)
            xh = 2.0 * c * _func2(y)
            u[j] = xh + s1 * hj / ea
            if has_spring:
                stretch = _spring_stretch(hj, sk1, rsp, brspr)
                u[j] = xh + stretch * sp / 100.0
            z[j] = 0.0
            slope = (-s1 + za * _func1(y) / y) / (2.0 * c)
            angle = slope - c1
            tb = hj / math.cos(slope)
            ha[j] = tb * math.cos(angle)
            p[j] = tb * math.sin(angle)

    # slack row (Fortran row 1)
    z[0] = (s1 - za) / (1.0 - ss)
    u[0] = z[0] * cc
    t[0] = w * (za - z[0] * ss)
    return LineTable(h, t, u, ha, z, p, model="catinb", total_length_ft=s1)


def _stretc(h, s1, w, z, ea, fact, cc):
    """Subroutine STRETC: elastic stretch + anchor pull, grounded case.

    Returns ``(ue, ha)``: total elastic elongation (ft) and horizontal
    anchor pull (lb). ``z`` is the grounded length, ``fact`` combines the
    bottom slope and friction.
    """
    if h > fact * w * z:
        ht = 2.0 * h / cc - fact * w * z
        ueb = ht * z / (2.0 * ea) * cc
        ha = h / cc - fact * w * z
    else:
        z0 = h / (fact * w)
        ueb = h * z0 / (2.0 * ea) * cc
        ha = 0.0
    z2 = s1 - z
    ue1 = h * z2 / ea
    return ue1 + ueb, ha


def _fulcat(lt: DynmoorLineType) -> LineTable:
    """Subroutine FULCAT: single segment to an elevated pile/bollard."""
    n = N_TABLE
    fac = (n - 1) / 0.8165
    seg = lt.segment1
    s = seg.length_ft
    w = seg.weight_water_lb_ft
    ea = seg.ea_lb
    zdep = lt.pile_height_ft  # DZ: pile top above ... (vertical offsets)
    zspan = lt.water_depth_ft  # Z: vertical distance between attachments
    brkmp = lt.brkm_kips * 1000.0

    sk1, rsp, brspr = _spring_constants(lt.spring, "fulcat")
    has_spring = lt.spring is not None and lt.spring.ep1 != 0.0
    sp = lt.spring.length_ft if has_spring else 0.0
    if has_spring and ea == 0.0:
        ea = 1.0e20  # "LR added"
    if ea == 0.0:
        ea = 1.0e20  # port guard: inextensible when EA is omitted

    c1 = s * s
    c2 = zspan * zspan
    d2 = c1 - c2
    y1 = zdep
    y2 = y1 + zspan

    h = [0.0] * n
    t = [0.0] * n
    u = [0.0] * n
    ha = [0.0] * n
    zb = [0.0] * n
    p = [0.0] * n

    u1 = u2 = 0.0  # NOTE: legacy stale-value bug preserved (see branch 10)
    for j in range(1, n):
        hj = brkmp * (j / fac) ** 2
        h[j] = hj
        c = hj / w
        s1 = math.sqrt(y1 * y1 + 2.0 * c * y1)
        s2 = math.sqrt(y2 * y2 + 2.0 * c * y2)
        st = s1 + s2
        taut_span = False
        if st > s:
            c3 = c * c
            b = c2 * c3 / d2 - d2 / 4.0
            rd = math.sqrt(c1 + 4.0 * b)
            s1 = (s - rd) / 2.0
            if s1 < 0.0:
                taut_span = True
            else:
                s2 = s - s1
                z2 = -c + math.sqrt(s2 * s2 + c3)
                t[j] = hj + w * z2
                zb[j] = 0.0
        else:
            zb[j] = s - st
            t[j] = hj + w * y2

        if not taut_span:
            u1 = c * _func2(s1 / c)
            u2 = c * _func2(s2 / c)
            u[j] = u1 + u2 + hj * s / ea
            if has_spring:
                stretch = _spring_stretch(hj, sk1, rsp, brspr)
                u[j] = u1 + u2 + stretch * sp / 100.0 + hj * s / ea
            if st < s:
                u[j] = u1 + u2 + zb[j]
            ha[j] = hj
            p[j] = 0.0
        else:
            y = math.sqrt(c1 - c2) / (2.0 * c)
            u[j] = 2.0 * c * _func2(y) + hj * s / ea
            if has_spring:
                # legacy uses u1/u2 from the previous row here (stale) -
                # preserved bit-for-bit.
                stretch = _spring_stretch(hj, sk1, rsp, brspr)
                u[j] = u1 + u2 + stretch * sp / 100.0 + hj * s / ea
            t[j] = _func3(zspan, s, w, hj)
            ha[j] = hj
            vt = math.sqrt(t[j] ** 2 - hj * hj)
            p[j] = vt - w * s
            zb[j] = 0.0

    t[0] = (zspan + zdep) * w
    zb[0] = s - (zspan + 2.0 * zdep)
    return LineTable(h, t, u, ha, zb, p, model="fulcat", total_length_ft=s)


def _catnry(lt: DynmoorLineType) -> LineTable:
    """Subroutine CATNRY: three-segment line with buoy and clump weight."""
    n = N_TABLE
    fac = float(n - 1)
    d = lt.water_depth_ft
    s1 = lt.segment1.length_ft
    s2 = lt.segment2.length_ft
    s3 = lt.segment3.length_ft
    w1 = lt.segment1.weight_water_lb_ft
    w2 = lt.segment2.weight_water_lb_ft
    w3 = lt.segment3.weight_water_lb_ft
    ea1 = lt.segment1.ea_lb
    ea2 = lt.segment2.ea_lb
    ea3 = lt.segment3.ea_lb
    # legacy segment blanking (main DO 20): S2/S3 < 5 ft -> unit weight/EA
    if s2 < 5.0:
        w2 = 1.0
        ea2 = 1.0
    if s3 < 5.0:
        w3 = 1.0
        ea3 = 1.0
    if ea1 == 0.0:
        ea1 = 1.0e20
    if ea2 == 0.0:
        ea2 = 1.0e20
    if ea3 == 0.0:
        ea3 = 1.0e20
    wb = lt.buoy_buoyancy_kips * 1000.0
    wc = lt.clump_weight_kips * 870.0
    brkmp = 1000.0 * lt.brkm_kips

    sk1, rsp, _ = _spring_constants(lt.spring, "catnry")
    has_spring = lt.spring is not None and lt.spring.ep1 != 0.0
    slsp = lt.spring.length_ft if has_spring else 0.0

    slope = 0.0
    xi = 6.0

    h = [0.0] * n
    t = [0.0] * n
    u = [0.0] * n
    ha = [0.0] * n
    zz = [0.0] * n
    p = [0.0] * n

    for i in range(1, n):
        hi = brkmp * (i / fac) ** 2
        h[i] = hi
        b1 = w1 / hi
        b2 = w2 / hi
        b3 = w3 / hi
        z = 0.0
        p1 = p2 = p3 = p4 = p5 = 0.0
        converged = False
        for _ in range(100):
            p1 = max(slope, xi - b2 * s2)
            p2 = max(slope, xi - b2 * s2 - wc / hi)
            p3 = max(slope, xi - b2 * s2 - wc / hi - b1 * s1)
            p4 = max(b3 * s3 / 2.0, xi - wb / hi + b3 * s3)
            p5 = max(-b3 * s3 / 2.0, xi - wb / hi)
            delt1 = 1.0 if p1 > 0.0 else 0.0
            delt2 = 1.0 if p2 > 0.0 else 0.0
            delt3 = 1.0 if p3 > 0.0 else 0.0
            delt4 = 1.0 if p5 > -b3 * s3 / 2.0 else 0.0
            z = s1 + 1.0 / b2 * (b2 * s2 - xi + p1) + 1.0 / b1 * (p3 - p2)
            d0 = (
                1.0 / b2 * (_func1(xi) - _func1(p1))
                + 1.0 / b1 * (_func1(p2) - _func1(p3))
                + 1.0 / b3 * (_func1(p4) - _func1(p5))
            )
            dd0 = (
                1.0 / b2 * (xi / _func1(xi) - delt1 * p1 / _func1(p1))
                + 1.0 / b1 * (delt2 * p2 / _func1(p2) - delt3 * p3 / _func1(p3))
                + delt4 / b3 * (p4 / _func1(p4) - p5 / _func1(p5))
            )
            if dd0 <= 0.0:
                xi = xi + 0.1
                continue
            dxi = (d - d0) / dd0
            if xi + dxi < 0.0:
                xi = 0.7 * xi
            else:
                xi = xi + dxi
            if abs(dxi / xi) >= 0.0005:
                continue
            if abs(d - d0) < 1.0:
                converged = True
                break
        if not converged:
            raise ArithmeticError(
                f"no convergence in CATNRY after 100 iterations at table row {i + 1}"
            )
        ue, han = _sub(s1, s2, s3, w1, w2, ea1, ea2, ea3, hi, z)
        ui = (
            z
            + 1.0 / b2 * (_func2(xi) - _func2(p1))
            + 1.0 / b1 * (_func2(p2) - _func2(p3))
            + 1.0 / b3 * (_func2(p4) - _func2(p5))
            + ue
        )
        u[i] = ui
        if has_spring:
            # NOTE: legacy normalises by BRKMP (segment minimum), not the
            # spring breaking strength - preserved.
            stretch = _spring_stretch(hi, sk1, rsp, brkmp)
            u[i] = ui + stretch * slsp / 100.0
        tn1 = hi * _func1(xi)
        tn2 = hi * _func1(p4)
        t[i] = max(tn1, tn2)
        zz[i] = z
        ha[i] = han
        if p2 > 0.0:
            p[i] = hi * p3
        elif p1 > 0.0:
            p[i] = -wc + hi * p1
            if z > 0.0:
                p[i] = 0.0
        else:
            p[i] = -wc
            if z > 0.0:
                p[i] = 0.0

    u[0] = s1 + s2 + s3 - d
    zz[0] = u[0]
    if s3 >= 1.0:
        t[0] = d * w3
        if s3 < d:
            t[0] = s3 * w3 + (d - s3) * w2
    else:
        t[0] = d * w2
        if s2 < d:
            t[0] = s2 * w2 + (d - s2) * w1
    return LineTable(h, t, u, ha, zz, p, model="catnry", total_length_ft=s1 + s2 + s3)


def _sub(s1, s2, s3, w1, w2, ea1, ea2, ea3, h, z):
    """Subroutine SUB: elastic stretch + horizontal anchor pull, 3-segment.

    ``z`` is the grounded length. Returns ``(ue, ha)``.
    """
    if z <= s1:
        if h > z * w1:
            ueb = (2.0 * h - z * w1) / 2.0 * z / ea1
            ha = h - z * w1
        else:
            ueb = h * h / (w1 * ea1)
            ha = 0.0
        ue = (s2 / ea2 + (s1 - z) / ea1 + s3 / ea3) * h + ueb
        return ue, ha
    z1 = z - s1
    sf = h - z1 * w2
    if sf > s1 * w1:
        ueb = (2.0 * sf - s1 * w1) / 2.0 * s1 / ea1
        ha = sf - s1 * w1
    else:
        ueb = sf * sf / (w1 * ea1)
        ha = 0.0
    if sf > 0.0:
        ue = (s2 - z1) * h / ea2 + z1 / ea2 * ((h + sf) / 2.0) + s3 * h / ea3 + ueb
    else:
        ue = (s2 - z1) * h / ea2 + h * h / (w2 * ea2) + s3 * h / ea3
    return ue, ha


def build_line_table(line_type: DynmoorLineType) -> LineTable:
    """Build the 50-row load-excursion table for a line type.

    Dispatch identical to the legacy main program (DO 30):
    3 segments (``S2 >= 5``) -> ``CATNRY``; single segment with
    ``pile_height_ft > 0`` -> ``FULCAT``; else ``CATINB``.
    """
    if line_type.segment2.length_ft >= 5.0:
        return _catnry(line_type)
    if line_type.pile_height_ft > 0.0:
        return _fulcat(line_type)
    return _catinb(line_type)


# ---------------------------------------------------------------------------
# Table lookups (INTERP / ANPULL / QINTER) and pretension setup
# ---------------------------------------------------------------------------


def interp_table(table: LineTable, distance_ft: float) -> tuple[float, float]:
    """Subroutine INTERP: (horizontal tension, total tension) in lb at a
    fairlead-anchor horizontal distance.

    Quadratic through rows i-2..i once i >= 5 (1-based); linear below;
    row-1 values at/below the table foot; last-row values off-table (the
    legacy code would index one row past the table - see module notes).
    """
    n = N_TABLE
    idx = None
    for i in range(n):
        if table.u[i] > distance_ft:
            idx = i
            break
    if idx is None:
        return table.h[n - 1], table.t[n - 1]
    i1 = idx + 1  # 1-based Fortran row
    if i1 >= 5:
        x1, x2, x3 = table.u[idx - 2], table.u[idx - 1], table.u[idx]
        hten = _qintrp(
            distance_ft, x1, x2, x3, table.h[idx - 2], table.h[idx - 1], table.h[idx]
        )
        tens = _qintrp(
            distance_ft, x1, x2, x3, table.t[idx - 2], table.t[idx - 1], table.t[idx]
        )
        if hten < 0.0:
            hten = 0.0
        if tens < table.t[0]:
            tens = table.t[0]
        return hten, tens
    if i1 == 1:
        return table.h[0], table.t[0]
    dh = table.h[idx] - table.h[idx - 1]
    dt = table.t[idx] - table.t[idx - 1]
    dx = table.u[idx] - table.u[idx - 1]
    xx3 = distance_ft - table.u[idx - 1]
    return table.h[idx - 1] + dh * xx3 / dx, table.t[idx - 1] + dt * xx3 / dx


def anchor_pull(table: LineTable, horizontal_lb: float) -> tuple[float, float]:
    """Subroutine ANPULL: (vertical anchor pull lb, grounded length ft)
    at a given horizontal fairlead tension."""
    n = N_TABLE
    k = None
    for i in range(n):
        if table.h[i] > horizontal_lb:
            k = i
            break
    if k is None:
        k = n - 1
    k1 = k + 1  # 1-based
    if k1 == 1:
        dh = table.h[1] - table.h[0]
        dz = table.z[1] - table.z[0]
        return 0.0, table.z[0] + dz * (horizontal_lb - table.h[0]) / dh
    if k1 == 2:
        k = 2  # legacy K=3 (1-based)
    f = horizontal_lb
    v = _qintrp(
        f,
        table.h[k - 2],
        table.h[k - 1],
        table.h[k],
        table.p[k - 2],
        table.p[k - 1],
        table.p[k],
    )
    s = _qintrp(
        f,
        table.h[k - 2],
        table.h[k - 1],
        table.h[k],
        table.z[k - 2],
        table.z[k - 1],
        table.z[k],
    )
    return v, s


def qinter_table(table: LineTable, distance_ft: float) -> tuple[float, float]:
    """Subroutine QINTER: (grounded length ft, vertical anchor pull kips)
    at a fairlead-anchor horizontal distance (extrapolates off-table)."""
    n = N_TABLE
    idx = None
    for i in range(1, n):
        if table.u[i] > distance_ft:
            idx = i
            break
    if idx is None:
        idx = n - 1
    i1 = idx + 1
    if i1 == 2:
        idx = 2
    a1 = (
        (distance_ft - table.u[idx - 1])
        * (distance_ft - table.u[idx])
        / ((table.u[idx - 2] - table.u[idx - 1]) * (table.u[idx - 2] - table.u[idx]))
    )
    a2 = (
        (distance_ft - table.u[idx - 2])
        * (distance_ft - table.u[idx])
        / ((table.u[idx - 1] - table.u[idx - 2]) * (table.u[idx - 1] - table.u[idx]))
    )
    a3 = (
        (distance_ft - table.u[idx - 2])
        * (distance_ft - table.u[idx - 1])
        / ((table.u[idx] - table.u[idx - 2]) * (table.u[idx] - table.u[idx - 1]))
    )
    xlb = a1 * table.z[idx - 2] + a2 * table.z[idx - 1] + a3 * table.z[idx]
    va = (a1 * table.p[idx - 2] + a2 * table.p[idx - 1] + a3 * table.p[idx]) / 1000.0
    return xlb, va


@dataclass(frozen=True)
class PretensionSolution:
    """Pretension setup for one line (main program DO 50 block)."""

    pretension_lb: float  # PT (total tension)
    horizontal_distance_ft: float  # USTAR: fairlead->anchor horizontal
    horizontal_tension_lb: float  # HORTN at pretension
    fairlead_angle_deg: float  # BETA: line angle with horizontal
    anchor_x_ft: float  # XAN
    anchor_y_ft: float  # YAN


def solve_pretension(
    line: DynmoorLine, line_type: DynmoorLineType, table: LineTable
) -> PretensionSolution:
    """Place the anchor so the line carries its specified pretension.

    ``PT = TI * BRKM * 10`` lb (TI in percent of BRKM kips), looked up on
    the *total tension* column.
    """
    pt = line.pretension_percent * line_type.brkm_kips * 10.0
    n = N_TABLE
    k = None
    for i in range(n):
        if table.t[i] > pt:
            k = i
            break
    if k is None:
        raise ValueError(
            f"pretension {pt / 1000.0:.1f} kips too large ... off table"
        )
    k1 = k + 1  # 1-based row
    if k1 == 1:
        raise ValueError(f"pretension {pt / 1000.0:.1f} kips too small ... off table")
    if k1 >= 5:
        ustar = _qintrp(
            pt,
            table.t[k - 2],
            table.t[k - 1],
            table.t[k],
            table.u[k - 2],
            table.u[k - 1],
            table.u[k],
        )
    else:
        dn = table.u[k] - table.u[k - 1]
        dd = table.t[k] - table.t[k - 1]
        ustar = table.u[k - 1] + dn * (pt - table.t[k - 1]) / dd
    kq = max(k, 2)  # port guard: legacy would index row 0 for k1 == 2
    hortn = _qintrp(
        pt,
        table.t[kq - 2],
        table.t[kq - 1],
        table.t[kq],
        table.h[kq - 2],
        table.h[kq - 1],
        table.h[kq],
    )
    vtctn = math.sqrt(pt * pt - hortn * hortn)
    beta = math.atan(vtctn / hortn) / _DEG
    gamma = line.angle_deg * _DEG
    xan = line.fairlead_x_ft + ustar * math.cos(gamma)
    yan = line.fairlead_y_ft + ustar * math.sin(gamma)
    return PretensionSolution(pt, ustar, hortn, beta, xan, yan)


def anchor_position(line: DynmoorLine, horizontal_distance_ft: float):
    """Anchor coordinates for a fairlead-anchor distance along the line."""
    gamma = line.angle_deg * _DEG
    return (
        line.fairlead_x_ft + horizontal_distance_ft * math.cos(gamma),
        line.fairlead_y_ft + horizontal_distance_ft * math.sin(gamma),
    )


# ---------------------------------------------------------------------------
# Static restoring force (RFORCE)
# ---------------------------------------------------------------------------


@dataclass
class RestoringForceResult:
    """Mooring restoring forces at a vessel offset (subroutine RFORCE)."""

    surge_lb: float  # FC(1), body frame, + fwd
    sway_lb: float  # FC(2), body frame, + stbd
    yaw_lb_ft: float  # FC(3)
    horizontal_tensions_lb: list[float]  # HF per line
    total_tensions_lb: list[float]  # TT per line
    max_tension_line: int  # ITMAX (1-based line number)


def restoring_force(
    lines: Sequence[DynmoorLine],
    tables: Sequence[LineTable],
    anchors: Sequence[tuple[float, float]],
    surge_ft: float,
    sway_ft: float,
    yaw_rad: float,
    broken: Optional[Sequence[int]] = None,
) -> RestoringForceResult:
    """Subroutine RFORCE: line tensions + restoring force at an offset.

    ``anchors`` are (XAN, YAN) pairs; ``broken`` is an optional list of
    1-based line numbers carrying zero load (broken-line option).
    """
    broken_set = set(broken or ())
    c = math.cos(yaw_rad)
    s = math.sin(yaw_rad)
    fcx = fcy = xmz = 0.0
    hf: list[float] = []
    tt: list[float] = []
    hmt = 0.0
    itmax = 1
    for i, line in enumerate(lines):
        xc = surge_ft + line.fairlead_x_ft * c - line.fairlead_y_ft * s
        yc = sway_ft + line.fairlead_x_ft * s + line.fairlead_y_ft * c
        xan, yan = anchors[i]
        hd = math.sqrt((xan - xc) ** 2 + (yan - yc) ** 2)
        hff, tnn = interp_table(tables[line.line_type - 1], hd)
        # legacy RFORCE updates ITMAX from the intact tension *before*
        # zeroing broken lines - order preserved
        if hff > hmt:
            itmax = i + 1
            hmt = hff
        if (i + 1) in broken_set:
            hff = 0.0
            tnn = 0.0
        hf.append(hff)
        tt.append(tnn)
        fx = (xan - xc) * hff / hd
        fy = (yan - yc) * hff / hd
        cmz = fy * (line.fairlead_x_ft * c - line.fairlead_y_ft * s) - fx * (
            line.fairlead_y_ft * c + line.fairlead_x_ft * s
        )
        fcx += fx
        fcy += fy
        xmz += cmz
    ffc = fcx * c + fcy * s
    flc = fcy * c - fcx * s
    return RestoringForceResult(ffc, flc, xmz, hf, tt, itmax)


# ---------------------------------------------------------------------------
# Fenders (FENDER)
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Fender:
    """One fender: station, initial gap and 4-point force-deflection curve.

    ``deflections_ft`` / ``forces_kips`` are the (E, F) breakpoints; the
    curve is piecewise linear through the origin and extrapolates beyond
    the last point with the last-segment stiffness.
    """

    station_x_ft: float  # XF: + fwd of midship
    initial_gap_ft: float  # EO
    deflections_ft: tuple[float, float, float, float]
    forces_kips: tuple[float, float, float, float]

    def stiffnesses(self) -> tuple[float, float, float, float]:
        e, f = self.deflections_ft, self.forces_kips
        return (
            f[0] / e[0],
            (f[1] - f[0]) / (e[1] - e[0]),
            (f[2] - f[1]) / (e[2] - e[1]),
            (f[3] - f[2]) / (e[3] - e[2]),
        )


def fender_forces(
    fenders: Sequence[Fender], sway_ft: float, yaw_rad: float, side: int = 1
):
    """Subroutine FENDER: (sway force lb, yaw moment lb-ft, per-fender kips).

    ``side`` is ISIDE: fenders engage for positive local deflection when
    ``side >= 0``, negative when ``side < 0``.
    """
    per_fender: list[float] = []
    for fen in fenders:
        y = sway_ft + fen.station_x_ft * yaw_rad - fen.initial_gap_ft
        engaged = (y < 0.0) if side < 0 else (y > 0.0)
        if not engaged:
            per_fender.append(0.0)
            continue
        e = fen.deflections_ft
        f = fen.forces_kips
        sk = fen.stiffnesses()
        yab = abs(y)
        if yab >= e[2]:
            fy = (f[2] + sk[3] * (yab - e[2])) * yab / (-y)
        elif yab >= e[1]:
            fy = (f[1] + sk[2] * (yab - e[1])) * yab / (-y)
        elif yab >= e[0]:
            fy = (f[0] + sk[1] * (yab - e[0])) * yab / (-y)
        else:
            fy = sk[0] * (-y)
        per_fender.append(fy)
    fl = sum(1000.0 * fy for fy in per_fender)
    fmz = sum(
        1000.0 * fen.station_x_ft * fy for fen, fy in zip(fenders, per_fender)
    )
    return fl, fmz, per_fender


# ---------------------------------------------------------------------------
# Static wind force (WINDAF / WCOEF + OSCILL wind term)
# ---------------------------------------------------------------------------

_WCOEF_STEPS = (
    (50.0, 1.1),
    (100.0, 1.2),
    (150.0, 1.3),
    (200.0, 1.37),
    (250.0, 1.43),
    (300.0, 1.48),
    (350.0, 1.52),
    (400.0, 1.56),
    (450.0, 1.60),
    (500.0, 1.63),
    (550.0, 1.67),
    (600.0, 1.7),
    (650.0, 1.72),
    (700.0, 1.75),
    (750.0, 1.77),
    (800.0, 1.79),
    (850.0, 1.8),
)


def wind_height_coefficient(height_ft: float) -> float:
    """Subroutine WCOEF: wind height coefficient step table.

    The legacy cascade leaves CH undefined at exactly 50 ft; this port
    treats ``height <= 50`` as 1.0.
    """
    ch = 1.0
    for threshold, value in _WCOEF_STEPS:
        if height_ft > threshold:
            ch = value
    return ch


@dataclass(frozen=True)
class WindArea:
    """One windage area card (WINDAF): width x height at a VCG above keel."""

    width_ft: float  # AW
    height_ft: float  # AH
    vca_above_keel_ft: float  # VCA: vertical center of area
    shape_coefficient: float  # CS
    lever_arm_ft: float = 0.0  # CLA (+ fwd), lateral areas only


@dataclass(frozen=True)
class WindLoadModel:
    """100-knot wind force sums (WNFF/WNFL/WNMY of COMMON /WNF/), kips."""

    frontal_force_100kt_kips: float  # SUM1
    lateral_force_100kt_kips: float  # SUM3
    yaw_moment_100kt_kip_ft: float  # SUM5


def build_wind_model(
    frontal_areas: Sequence[WindArea],
    lateral_areas: Sequence[WindArea],
    draft_ft: float,
) -> WindLoadModel:
    """Subroutine WINDAF: per-area force ``0.0338 * Ch * Cs * A`` (kips at
    100 knots), height coefficient at ``VCA - draft`` above the waterline."""

    def force(area: WindArea) -> float:
        ch = wind_height_coefficient(area.vca_above_keel_ft - draft_ft)
        return 0.0338 * ch * area.shape_coefficient * (area.height_ft * area.width_ft)

    sum1 = sum(force(a) for a in frontal_areas)
    sum3 = sum(force(a) for a in lateral_areas)
    sum5 = sum(force(a) * a.lever_arm_ft for a in lateral_areas)
    return WindLoadModel(sum1, sum3, sum5)


def static_wind_force(
    model: WindLoadModel,
    wind_speed_knots: float,
    wind_direction_deg: float,
    yaw_rad: float = 0.0,
):
    """Steady wind force (lb, lb, lb-ft) as applied in OSCILL:
    ``FWD = -(WNFF cos, WNFL sin, WNMY sin)(theta_rel) * V^2/10``."""
    thetr = wind_direction_deg * _DEG - yaw_rad
    fac = wind_speed_knots * wind_speed_knots / 10.0
    return (
        -model.frontal_force_100kt_kips * math.cos(thetr) * fac,
        -model.lateral_force_100kt_kips * math.sin(thetr) * fac,
        -model.yaw_moment_100kt_kip_ft * math.sin(thetr) * fac,
    )


def anchor_holding_power(line_type: DynmoorLineType) -> tuple[float, float]:
    """(mud, sand) horizontal anchor holding power, kips
    (``HMUD = AEF * WA0``, ``HSAND = SEF * WA0``)."""
    return (
        line_type.anchor_mud_efficiency * line_type.anchor_weight_kips,
        line_type.anchor_sand_efficiency * line_type.anchor_weight_kips,
    )


# ---------------------------------------------------------------------------
# Passing-ship forces (PARSHIP / PERSHIP + the OSCILL sweep wrapper)
# ---------------------------------------------------------------------------

_PARSHIP_RHO = 1.9905  # slug/ft^3, as in PARSHIP
_PARSHIP_PI = 3.1415279  # legacy typo'd PI, preserved
_PARSHIP_NT = 50


def parship_forces(
    moored_length_ft: float,
    passing_length_ft: float,
    separation_ft: float,
    water_depth_ft: float,
    stagger_ft: float,
    speed_fps: float,
    moored_midship_area_ft2: float,
    passing_midship_area_ft2: float,
) -> "PassingForce":
    """Subroutine PARSHIP (Wang 1975): parallel passing-ship forces.

    ``stagger_ft`` is the along-track position of the passing ship
    (midship-to-midship, XI in the source); ``speed_fps`` is the speed
    over ambient (SOA, ft/s). Returns surge/sway lb and yaw lb-ft.

    Legacy quirks preserved: the passing length is truncated to a whole
    foot and the passing-side Simpson step uses integer division
    (implicit-INTEGER ``L2`` in blank COMMON); ``PI = 3.1415279``.
    """
    l1 = moored_length_ft
    a1 = moored_midship_area_ft2
    a2 = passing_midship_area_ft2
    nt = _PARSHIP_NT
    l2 = int(passing_length_ft)  # implicit INTEGER L2
    step = l1 / nt
    step2 = float(l2 // nt)  # integer division L2/NT
    x1 = [i * l1 / nt - l1 / 2.0 for i in range(nt + 1)]
    x2 = [float((i * l2) // nt) - l2 / 2.0 for i in range(nt + 1)]

    def ds1(x: float) -> float:
        return -8.0 * x / (l1 * l1) * a1

    def s1ds1(x: float) -> float:
        return (-8.0 * x / (l1 * l1) * a1 * x) + (
            (1.0 - 4.0 * x * x / (l1 * l1)) * a1
        )

    def y_funcs(x: float, xx: float, eta: float) -> tuple[float, float]:
        """Subroutine Y: kernel pair (func1 -> sway, func2 -> surge)."""
        base = -8.0 * xx / float(l2 * l2) * a2
        den = ((xx - x + stagger_ft) ** 2 + eta * eta) ** 1.5
        return base / den, base * (xx - x + stagger_ft) / den

    def simpson(x: float, eta: float) -> tuple[float, float]:
        """Subroutine SIMPSON over the passing-ship stations."""
        f1, f2 = y_funcs(x, x2[0], eta)
        sum1, sum2 = f1, f2
        f1, f2 = y_funcs(x, x2[nt], eta)
        sum1 += f1
        sum2 += f2
        for i in range(1, nt, 2):
            f1, f2 = y_funcs(x, x2[i], eta)
            sum1 += 4.0 * f1
            sum2 += 4.0 * f2
        for j in range(2, nt - 1, 2):
            f1, f2 = y_funcs(x, x2[j], eta)
            sum1 += 2.0 * f1
            sum2 += 2.0 * f2
        return sum1 * step2 / 3.0, sum2 * step2 / 3.0

    surge = sway = yaw = 0.0
    u2 = speed_fps * speed_fps
    for n in range(-10, 11):  # finite-depth image sources
        eta = math.sqrt(separation_ft**2 + 4.0 * n * n * water_depth_ft**2)
        deep_surge = deep_sway = deep_yaw = 0.0
        for x, weight in _parship_stations(x1, nt):
            del_sway, del_surge = simpson(x, eta)
            deep_surge += weight * ds1(x) * del_surge
            deep_sway += weight * ds1(x) * del_sway
            deep_yaw += weight * s1ds1(x) * del_sway
        deep_surge = step / 3.0 * deep_surge * _PARSHIP_RHO * u2 / (2.0 * _PARSHIP_PI)
        deep_sway = step / 3.0 * deep_sway * _PARSHIP_RHO * u2 * eta / _PARSHIP_PI
        deep_yaw = step / 3.0 * deep_yaw * _PARSHIP_RHO * u2 * eta / _PARSHIP_PI
        surge += deep_surge
        sway += deep_sway / eta
        yaw += deep_yaw / eta
    sway *= separation_ft
    yaw *= separation_ft
    return PassingForce(surge, sway, yaw)


def _parship_stations(x1: list[float], nt: int):
    """Moored-ship Simpson stations in the legacy summation order."""
    yield x1[0], 1.0
    yield x1[nt], 1.0
    for i in range(1, nt, 2):
        yield x1[i], 4.0
    for j in range(2, nt - 1, 2):
        yield x1[j], 2.0


_PERSHIP_RHO = 62.4  # lb/ft^3
_PERSHIP_POLY = {
    # x/L bin -> (surge poly, sway poly, yaw poly), highest power first
    (-2.0, -1.0): (
        (1.419, 9.0382, 20.849, 20.649, 7.6018),
        (-9.7937, -69.192, -187.36, -241.7, -148.4, -35.033),
        (3.731, 27.595, 79.179, 109.54, 72.6, 18.243),
    ),
    (-1.0, 0.0): (
        (6.7137, 17.523, 13.05, 1.0989, -0.9719),
        (-20.71, -51.384, -37.299, -3.4041, 3.2853, -0.3051),
        (17.298, 52.456, 49.428, 11.82, -2.081, 0.2464),
    ),
    (0.0, 1.0): (
        (1.1534, -7.087, 7.8348, -0.6277, -0.9807),
        (-7.6176, 25.002, -24.951, 4.5771, 3.3527, -0.2989),
        (5.7009, -21.431, 23.595, -5.5488, -2.4947, 0.225),
    ),
    (1.0, 2.0): (
        (-0.0334, -0.5831, 3.5534, -6.3502, 3.7323),
        (-0.539, 4.2775, -14.152, 23.885, -20.221, 6.8103),
        (0.6101, -5.9171, 21.92, -38.921, 33.14, -10.778),
    ),
}


def _polyval(coeffs: Sequence[float], x: float) -> float:
    value = 0.0
    for c in coeffs:
        value = value * x + c
    return value


def pership_forces(
    moored_length_ft: float,
    moored_displacement_kips: float,
    passing_length_ft: float,
    passing_draft_ft: float,
    passing_displacement_kips: float,
    stagger_ft: float,
    speed_fps: float,
    water_depth_ft: float,
    separation_ft: float,
) -> "PassingForce":
    """Subroutine PERSHIP (v6.00, Nov 2006): perpendicular passing forces.

    Regression coefficients and stagger shape functions fitted to U.S.
    Naval Academy model-test data (D. Kriebel, 2005/2006). ``stagger_ft``
    is XI = (x/L)*L of the passing ship's track intersection; forces are
    zero outside ``-2 <= x/L <= 2`` (the caller's cutoff in the legacy
    time loop).
    """
    dr = passing_displacement_kips / moored_displacement_kips
    t_over_wd = passing_draft_ft / water_depth_ft
    cls_over_sl = separation_ft / moored_length_ft
    cx = 0.0043 * dr * math.exp(2.3 * t_over_wd) * math.exp(
        -1.8 * (cls_over_sl - 0.5)
    )
    cy = 0.0044 * dr * math.exp(4.0 * t_over_wd) * math.exp(
        -1.6 * (cls_over_sl - 0.5)
    )
    cm = 0.0007 * dr * math.exp(3.7 * t_over_wd) * math.exp(
        -2.0 * (cls_over_sl - 0.5)
    )
    fnormal = 0.5 * _PERSHIP_RHO * passing_length_ft * passing_draft_ft * (
        speed_fps * speed_fps
    ) / GRAVITY_FT_S2
    fnormalm = fnormal * passing_length_ft
    xnormal = fnormal * cx
    ynormal = fnormal * cy
    mnormal = fnormalm * cm
    x_over_l = stagger_ft / moored_length_ft
    for (lo, hi), (psurge, psway, pyaw) in _PERSHIP_POLY.items():
        inside = (lo <= x_over_l < hi) or (hi == 2.0 and lo <= x_over_l <= hi)
        if inside:
            return PassingForce(
                _polyval(psurge, x_over_l) * xnormal,
                _polyval(psway, x_over_l) * ynormal,
                _polyval(pyaw, x_over_l) * mnormal,
            )
    return PassingForce(0.0, 0.0, 0.0)


@dataclass(frozen=True)
class PassingForce:
    """Passing-ship force sample: surge/sway in lb, yaw in lb-ft."""

    surge_lb: float
    sway_lb: float
    yaw_lb_ft: float


@dataclass(frozen=True)
class PassingSweepSample:
    """One row of the passing-ship force history (the legacy .PAS file)."""

    step: int
    x_over_l: float
    surge_lb: float
    sway_lb: float
    yaw_lb_ft: float


def passing_ship_sweep(
    mode: str,
    nsteps: int,
    start_step: int,
    direction: float,
    speed_knots: float,
    moored_length_ft: float,
    moored_beam_ft: float,
    moored_displacement_kips: float,
    passing_length_ft: float,
    passing_beam_ft: float,
    separation_ft: float,
    water_depth_ft: float,
    current_knots: float = 0.0,
    current_direction_deg: float = 0.0,
    moored_midship_area_ft2: float = 0.0,
    passing_midship_area_ft2: float = 0.0,
    passing_draft_ft: float = 0.0,
    passing_displacement_kips: float = 0.0,
) -> list[PassingSweepSample]:
    """Passing-ship force time history (the OSCILL wrapper around
    PARSHIP/PERSHIP; one step = one second at the legacy DT = 1).

    ``mode`` is "parallel" (IMOVSH = 1) or "perpendicular" (IMOVSH = 2);
    ``direction`` is DIR (+/-1); speeds in knots. Stagger position
    ``x/L = DIR*((i - ISTAR)*SOG - 2 L)/L``; forces are zero (and no sample
    is emitted, matching the .PAS file) outside ``|x/L| <= 2``. Raises on
    the legacy separation guards.
    """
    if mode not in ("parallel", "perpendicular"):
        raise ValueError(f"mode must be parallel or perpendicular, got {mode!r}")
    if mode == "parallel":
        if separation_ft <= passing_beam_ft / 2.0 + moored_beam_ft / 2.0:
            raise ValueError(
                "centerline to centerline separation distance is too small"
            )
    else:
        if separation_ft <= moored_length_ft / 2.0:
            raise ValueError(
                "centerline to bow/stern separation distance is too small"
            )
    sog = speed_knots * KNOTS_TO_FPS_MAIN
    cv = current_knots * KNOTS_TO_FPS_ENVIN
    soa = sog * direction + cv * math.cos(
        current_direction_deg * (3.1415927 / 180.0)
    )
    samples: list[PassingSweepSample] = []
    for i in range(start_step, nsteps + 1):
        xloc = direction * ((i - start_step) * sog - moored_length_ft * 2.0) / (
            moored_length_ft
        )
        if abs(xloc) > 2.0:
            continue
        if mode == "parallel":
            force = parship_forces(
                moored_length_ft,
                passing_length_ft,
                separation_ft,
                water_depth_ft,
                xloc * moored_length_ft,
                soa,
                moored_midship_area_ft2,
                passing_midship_area_ft2,
            )
        else:
            force = pership_forces(
                moored_length_ft,
                moored_displacement_kips,
                passing_length_ft,
                passing_draft_ft,
                passing_displacement_kips,
                xloc * moored_length_ft,
                soa,
                water_depth_ft,
                separation_ft,
            )
        samples.append(
            PassingSweepSample(i, xloc, force.surge_lb, force.sway_lb, force.yaw_lb_ft)
        )
    return samples
