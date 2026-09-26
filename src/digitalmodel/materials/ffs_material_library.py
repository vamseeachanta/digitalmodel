# ABOUTME: FFS material library (API 579-1 Annex F / Annex 3D): lower-bound
# ABOUTME: toughness estimates, true stress-strain curve for FEA, grade lookup.
"""Material inputs for fitness-for-service assessment (#2171).

Three things a Part 9 / Level 3 assessment needs from the material, in one
place, with the unit convention of the rest of ``asset_integrity`` (bare
floats carry a unit suffix — ``_mpa``, ``_mm``, ``_degc``, ``_j`` — and pint
quantities of the right dimension are accepted and converted):

1. :func:`lower_bound_toughness` — a fracture-toughness estimate when no
   fracture test exists, from Charpy energy or from a reference temperature.
   API 579-1/ASME FFS-1 places these estimates in Annex F, paragraph F.4.4
   (2007 edition) / Annex 3D, paragraph 3D.4.4 (2016 and later); only the
   paragraph numbers are cited here, no clause text or tables are
   reproduced.  Each route is implemented from its public primary source:

   * ``"charpy"`` — the BS 7910 Annex J lower-bound Charpy correlation for the
     transition region.  This is the existing
     :func:`digitalmodel.asset_integrity.assessment.crack_fad.kmat_from_charpy`
     re-exported, so the repo has exactly one implementation.
   * ``"master_curve"`` — the ASTM E1921 Master Curve with a lower tolerance
     bound (Wallin, Eng. Fract. Mech. 69, 2002; SINTAP / FITNET procedure)
     given a reference temperature ``T0``.
   * ``"charpy_transition"`` — the 27 J Charpy transition temperature
     converted to ``T0`` (Wallin's ``T0 = T27J - 18 C``, as used in BS 7910
     Annex J and SINTAP) and then the Master Curve route.
   * ``"asme_kic"`` / ``"asme_kir"`` — the ASME BPVC Section XI, Nonmandatory
     Appendix A, A-4200 lower-bound K_Ic and K_Ia (K_IR) curves indexed to a
     reference temperature ``T_ref`` (RT_NDT).  The closed forms are public
     through EPRI NP-719-SR (Marston, 1978) and NUREG/CR-6609.

   Every route states and enforces its material-class limits (all are for
   ferritic steels).  Coefficients that have a public source are defaults
   here and are documented as such; the reference temperatures (``T0``,
   ``T27J``, ``T_ref``) are user inputs.

2. :class:`TrueStressStrainCurve` — an elastic-plastic true stress / true
   strain curve for solver decks: linear elastic to yield, power-law
   hardening on plastic strain (Ludwik, 1909) to the true ultimate, perfect
   plasticity beyond.  Annex F paragraph F.2.3 / Annex 3D paragraph 3D.2.3
   define the standard's own stress-strain model with material-specific
   tabulated coefficients; those tables are not reproduced.  This curve takes
   ``E``, ``sigma_y``, ``sigma_u`` and the hardening exponent as inputs, at a
   stated temperature, so it can be fed from a mill certificate or from the
   grade matrix.

3. :func:`lookup_grade` — the inputs above for a grade in
   :mod:`digitalmodel.materials.grades` (API 5L, IACS hull steel, EN 10025-2,
   ASTM A312 austenitic and ASTM A790 duplex stainless).

Units: MPa, mm, degC, J, MPa*sqrt(m).  Out of scope: creep (Part 10) and
fatigue curves (see :mod:`digitalmodel.fatigue.sn_library`).
"""
from __future__ import annotations

import math
import numbers
from dataclasses import dataclass, field, replace
from typing import Optional, Union

import pint

from digitalmodel.asset_integrity.assessment.crack_fad import kmat_from_charpy
from digitalmodel.units import Q_

from . import grades as _grades
from .grades import MaterialGrade

__all__ = [
    "FFSMaterialInputs",
    "ToughnessEstimate",
    "TrueStressStrainCurve",
    "kmat_from_charpy",
    "lookup_grade",
    "lower_bound_toughness",
    "TOUGHNESS_METHODS",
]

Number = Union[float, int, pint.Quantity]

# ---------------------------------------------------------------------------
# Unit handling
# ---------------------------------------------------------------------------
def _magnitude(value: object, unit: str, name: str) -> float:
    """Return ``value`` as a float in ``unit``.

    A bare number is taken to already be in ``unit`` (the argument name carries
    the tag).  A pint quantity is converted, and rejected if its dimension is
    wrong.  Anything else (a string, ``None``, a bool) is an untagged input and
    is rejected.
    """
    if value is None or isinstance(value, bool):
        raise TypeError(f"{name}: expected a number in {unit} or a pint Quantity, "
                        f"got {value!r}")
    if isinstance(value, pint.Quantity):
        try:
            out = float(value.to(unit).magnitude)
        except pint.DimensionalityError as exc:
            raise ValueError(
                f"{name}: wrong unit {value.units} (expected a quantity "
                f"convertible to {unit})") from exc
    elif isinstance(value, numbers.Real):
        out = float(value)
    else:
        raise TypeError(
            f"{name}: untagged input of type {type(value).__name__}; pass a "
            f"number in {unit} or a pint Quantity (digitalmodel.units.Q_)")
    if not math.isfinite(out):
        raise ValueError(f"{name}: must be finite, got {out!r}")
    return out


def _optional(value: object, unit: str, name: str) -> Optional[float]:
    return None if value is None else _magnitude(value, unit, name)


# ---------------------------------------------------------------------------
# Toughness
# ---------------------------------------------------------------------------
#: Toughness estimation routes accepted by :func:`lower_bound_toughness`.
TOUGHNESS_METHODS = ("charpy", "charpy_transition", "master_curve", "asme_kic", "asme_kir")

# --- public defaults (each with its source) ---------------------------------
# ASTM E1921 Master Curve, 1T (25.4 mm) reference thickness.  Median
# K_Jc = 30 + 70 exp[0.019 (T - T0)]; Weibull shape 4, threshold 20 MPa*sqrt(m),
# scale K0 - 20 = 11 + 77 exp[0.019 (T - T0)].  Source: Wallin, K., "Master
# curve analysis of the 'Euro' fracture toughness dataset", Eng. Fract. Mech.
# 69 (2002) 451-481; ASTM E1921.
_MC_THRESHOLD_MPA_SQRT_M = 20.0
_MC_SCALE_A = 11.0
_MC_SCALE_B = 77.0
_MC_SLOPE_PER_DEGC = 0.019
_MC_WEIBULL_SHAPE = 4.0
_MC_REFERENCE_THICKNESS_MM = 25.4
# ASTM E1921 scope (paragraph 1.1): ferritic steels, yield 275-825 MPa; the
# curve shape is validated for T - T0 within +/- 50 C.
_MC_SIGMA_Y_MIN_MPA = 275.0
_MC_SIGMA_Y_MAX_MPA = 825.0
_MC_WINDOW_DEGC = 50.0

# T0 from the 27 J Charpy transition temperature: T0 = T27J - 18 C (Wallin,
# K., "A simple theoretical Charpy-V - K_Ic correlation for irradiation
# embrittlement", ASME PVP 170, 1989; adopted by SINTAP and BS 7910 Annex J).
_T0_FROM_T27J_OFFSET_DEGC = -18.0

# ASME BPVC Section XI, Nonmandatory Appendix A, A-4200 lower-bound curves in
# ksi*sqrt(in) with T - RT_NDT in degF (public via EPRI NP-719-SR, Marston
# 1978, and NUREG/CR-6609):
#   K_Ic = 33.2 + 20.734 exp[0.02 (T - RT_NDT)]
#   K_Ia = 26.78 + 1.223 exp[0.0145 (T - RT_NDT + 160)]
# both capped at 200 ksi*sqrt(in) on the upper shelf.
_ASME_KIC = (33.2, 20.734, 0.02, 0.0)
_ASME_KIR = (26.78, 1.223, 0.0145, 160.0)
_ASME_CAP_KSI_SQRT_IN = 200.0
# Library applicability bound for the ASME curves: they were fitted to RPV-class
# ferritic steels; this module refuses higher-strength material.  The standard
# states its own limit in Annex F paragraph F.4.4.1 / Annex 3D paragraph
# 3D.4.4.1 -- consult it before overriding ``max_sigma_y_mpa``.
_ASME_SIGMA_Y_MAX_MPA = 620.0

_KSI_SQRT_IN_TO_MPA_SQRT_M = Q_(1.0, "ksi * inch**0.5").to("MPa * meter**0.5").magnitude
_DEGF_PER_DEGC = 1.8

# BS 7910 Annex J reference thickness for the Charpy correlation (mm).
_CHARPY_REFERENCE_THICKNESS_MM = 25.0


@dataclass(frozen=True)
class ToughnessEstimate:
    """A lower-bound fracture-toughness estimate and how it was obtained.

    ``float(estimate)`` returns ``k_mat_mpa_sqrt_m`` so the result can be fed
    straight into :func:`~digitalmodel.asset_integrity.assessment.crack_fad.assess_crack_like_flaw`.
    """

    k_mat_mpa_sqrt_m: float
    method: str
    material_class: str
    temperature_degc: Optional[float]
    thickness_mm: float
    notes: tuple[str, ...] = field(default_factory=tuple)

    def __float__(self) -> float:
        return self.k_mat_mpa_sqrt_m


def _require_ferritic(material_class: str, method: str) -> str:
    mc = material_class.strip().lower()
    if mc != "ferritic":
        raise ValueError(
            f"{method}: the correlation is limited to ferritic (carbon, C-Mn, "
            f"low-alloy) steels; material_class={material_class!r} is outside "
            f"its scope. Use measured fracture toughness for {mc} material.")
    return mc


def _master_curve_mpa_sqrt_m(
    delta_t_degc: float, fractile: float, thickness_mm: float
) -> float:
    if not 0.0 < fractile < 1.0:
        raise ValueError(f"fractile must be in (0, 1), got {fractile}")
    scale_1t = _MC_SCALE_A + _MC_SCALE_B * math.exp(_MC_SLOPE_PER_DEGC * delta_t_degc)
    k_1t = _MC_THRESHOLD_MPA_SQRT_M + scale_1t * (
        -math.log(1.0 - fractile)) ** (1.0 / _MC_WEIBULL_SHAPE)
    # Weakest-link thickness adjustment (ASTM E1921; Wallin 2002).
    return _MC_THRESHOLD_MPA_SQRT_M + (k_1t - _MC_THRESHOLD_MPA_SQRT_M) * (
        _MC_REFERENCE_THICKNESS_MM / thickness_mm) ** (1.0 / _MC_WEIBULL_SHAPE)


def _asme_curve_ksi_sqrt_in(delta_t_degc: float, coeffs) -> float:
    a, b, c, shift_degf = coeffs
    delta_t_degf = _DEGF_PER_DEGC * delta_t_degc
    return a + b * math.exp(c * (delta_t_degf + shift_degf))


def lower_bound_toughness(
    *,
    cvn_j: Optional[Number] = None,
    temperature_degc: Optional[Number] = None,
    thickness_mm: Optional[Number] = None,
    t0_degc: Optional[Number] = None,
    t27j_degc: Optional[Number] = None,
    t_ref_degc: Optional[Number] = None,
    sigma_y_mpa: Optional[Number] = None,
    method: str = "auto",
    material_class: str = "ferritic",
    fractile: float = 0.05,
    allow_extrapolation: bool = False,
    max_sigma_y_mpa: Number = _ASME_SIGMA_Y_MAX_MPA,
    cap_mpa_sqrt_m: Optional[Number] = None,
) -> ToughnessEstimate:
    """Lower-bound fracture toughness K_mat (MPa*sqrt(m)) when no test exists.

    Routes (``method``), each for ferritic steels only -- see the module
    docstring for sources and API 579-1 Annex F paragraph F.4.4 / Annex 3D
    paragraph 3D.4.4 for where the standard treats these estimates:

    ``"charpy"``
        BS 7910 Annex J transition-region correlation from ``cvn_j`` at the
        assessment temperature, thickness-corrected to ``thickness_mm``
        (default 25 mm).  Re-export of :func:`kmat_from_charpy`.
    ``"master_curve"``
        ASTM E1921 Master Curve at ``temperature_degc`` given ``t0_degc``,
        lower tolerance bound at ``fractile`` (default 5 %), adjusted from 1T
        to ``thickness_mm`` (default 25.4 mm).  Scope: ``sigma_y_mpa`` in
        275-825 MPa; ``|T - T0| <= 50 C`` unless ``allow_extrapolation``.
    ``"charpy_transition"``
        ``t27j_degc`` converted to ``T0 = T27J - 18 C`` then the Master Curve.
    ``"asme_kic"`` / ``"asme_kir"``
        ASME XI Appendix A A-4200 static-initiation / crack-arrest lower-bound
        curves at ``temperature_degc`` given ``t_ref_degc`` (RT_NDT), capped
        at the upper shelf (200 ksi*sqrt(in) unless ``cap_mpa_sqrt_m``).
        Refused for ``sigma_y_mpa > max_sigma_y_mpa`` (620 MPa default).
    ``"auto"``
        The first of ``charpy`` (only ``cvn_j`` given), ``charpy_transition``
        (``t27j_degc``), ``master_curve`` (``t0_degc``), ``asme_kic``
        (``t_ref_degc``) whose inputs are present.

    All arguments are keyword-only and unit-tagged by name; pint quantities of
    the right dimension are converted.

    Raises:
        ValueError: unknown method, missing inputs for the method, wrong-unit
            quantity, material class or scope violation.
        TypeError: untagged (positional / string / None) inputs.
    """
    cvn = _optional(cvn_j, "J", "cvn_j")
    temp = _optional(temperature_degc, "degC", "temperature_degc")
    thick = _optional(thickness_mm, "mm", "thickness_mm")
    t0 = _optional(t0_degc, "degC", "t0_degc")
    t27j = _optional(t27j_degc, "degC", "t27j_degc")
    t_ref = _optional(t_ref_degc, "degC", "t_ref_degc")
    sig_y = _optional(sigma_y_mpa, "MPa", "sigma_y_mpa")
    sig_y_max = _magnitude(max_sigma_y_mpa, "MPa", "max_sigma_y_mpa")
    cap = _optional(cap_mpa_sqrt_m, "MPa * meter**0.5", "cap_mpa_sqrt_m")

    if method == "auto":
        if cvn is not None and temp is None and t27j is None and t0 is None and t_ref is None:
            method = "charpy"
        elif t27j is not None:
            method = "charpy_transition"
        elif t0 is not None:
            method = "master_curve"
        elif t_ref is not None:
            method = "asme_kic"
        elif cvn is not None:
            method = "charpy"
        else:
            raise ValueError(
                "lower_bound_toughness: supply cvn_j (Charpy route), or "
                "temperature_degc with t0_degc / t27j_degc / t_ref_degc "
                "(reference-temperature routes).")
    if method not in TOUGHNESS_METHODS:
        raise ValueError(f"unknown method {method!r}; choose one of {TOUGHNESS_METHODS}")

    mc = _require_ferritic(material_class, method)
    notes: list[str] = []

    if method == "charpy":
        if cvn is None:
            raise ValueError("charpy: cvn_j (Charpy V-notch energy, J) is required.")
        b = _CHARPY_REFERENCE_THICKNESS_MM if thick is None else thick
        k = kmat_from_charpy(cvn, b)
        notes.append("BS 7910 Annex J lower-bound Charpy correlation (transition "
                     "region); use measured fracture toughness whenever available.")
        return ToughnessEstimate(k, method, mc, temp, b, tuple(notes))

    # --- reference-temperature routes --------------------------------------
    if temp is None:
        raise ValueError(f"{method}: temperature_degc (assessment temperature) is required.")
    if sig_y is None:
        raise ValueError(f"{method}: sigma_y_mpa is required for the material-scope check.")

    if method in ("master_curve", "charpy_transition"):
        if method == "charpy_transition":
            if t27j is None:
                raise ValueError("charpy_transition: t27j_degc (27 J transition "
                                 "temperature) is required.")
            t0 = t27j + _T0_FROM_T27J_OFFSET_DEGC
            notes.append(f"T0 = T27J {_T0_FROM_T27J_OFFSET_DEGC:+.0f} C = {t0:.1f} C "
                         "(Wallin 1989; BS 7910 Annex J / SINTAP).")
        elif t0 is None:
            raise ValueError("master_curve: t0_degc (ASTM E1921 reference "
                             "temperature) is required.")
        if not _MC_SIGMA_Y_MIN_MPA <= sig_y <= _MC_SIGMA_Y_MAX_MPA:
            raise ValueError(
                f"{method}: ASTM E1921 scope is {_MC_SIGMA_Y_MIN_MPA:.0f}-"
                f"{_MC_SIGMA_Y_MAX_MPA:.0f} MPa yield; sigma_y_mpa={sig_y} is outside.")
        dt = temp - t0
        if abs(dt) > _MC_WINDOW_DEGC:
            if not allow_extrapolation:
                raise ValueError(
                    f"{method}: T - T0 = {dt:.1f} C is outside the +/-"
                    f"{_MC_WINDOW_DEGC:.0f} C validity window; pass "
                    "allow_extrapolation=True to proceed with a flagged estimate.")
            notes.append(f"Extrapolated: T - T0 = {dt:.1f} C exceeds the "
                         f"+/-{_MC_WINDOW_DEGC:.0f} C window of ASTM E1921.")
        b = _MC_REFERENCE_THICKNESS_MM if thick is None else thick
        if b <= 0:
            raise ValueError("thickness_mm must be positive.")
        k = _master_curve_mpa_sqrt_m(dt, fractile, b)
        notes.append(f"ASTM E1921 Master Curve, {fractile:.0%} tolerance bound, "
                     f"1T -> {b:g} mm (Wallin 2002).")
        if cap is not None and k > cap:
            notes.append(f"Capped at {cap:g} MPa*sqrt(m).")
            k = cap
        return ToughnessEstimate(k, method, mc, temp, b, tuple(notes))

    # asme_kic / asme_kir
    if t_ref is None:
        raise ValueError(f"{method}: t_ref_degc (reference temperature RT_NDT) is required.")
    if sig_y > sig_y_max:
        raise ValueError(
            f"{method}: sigma_y_mpa={sig_y} exceeds the {sig_y_max:.0f} MPa "
            "applicability bound of the ASME XI lower-bound curves.")
    coeffs = _ASME_KIC if method == "asme_kic" else _ASME_KIR
    k_ksi = _asme_curve_ksi_sqrt_in(temp - t_ref, coeffs)
    cap_ksi = (_ASME_CAP_KSI_SQRT_IN if cap is None else cap / _KSI_SQRT_IN_TO_MPA_SQRT_M)
    if k_ksi > cap_ksi:
        notes.append(f"Capped at the upper-shelf value {cap_ksi * _KSI_SQRT_IN_TO_MPA_SQRT_M:.1f} "
                     "MPa*sqrt(m).")
        k_ksi = cap_ksi
    k = k_ksi * _KSI_SQRT_IN_TO_MPA_SQRT_M
    label = "K_Ic (static initiation)" if method == "asme_kic" else "K_Ia / K_IR (crack arrest)"
    notes.append(f"ASME XI App. A A-4200 lower-bound {label} curve at T - T_ref = "
                 f"{temp - t_ref:.1f} C.")
    b = _CHARPY_REFERENCE_THICKNESS_MM if thick is None else thick
    return ToughnessEstimate(k, method, mc, temp, b, tuple(notes))


# ---------------------------------------------------------------------------
# True stress-strain curve
# ---------------------------------------------------------------------------
@dataclass(frozen=True, kw_only=True)
class TrueStressStrainCurve:
    """Elastic-plastic true stress / true strain curve for FEA input.

    Three branches:

    * elastic: ``sigma = E * eps`` up to the yield strain ``sigma_y / E``;
    * hardening: power law on plastic strain (Ludwik form)
      ``sigma = sigma_y + (sigma_u_true - sigma_y) * (eps_p / eps_p_u) ** n``
      from yield to the true ultimate, where ``eps_p = eps - sigma / E`` and
      ``eps_p_u`` is the plastic strain at the ultimate; the tangent modulus
      never exceeds ``E`` and the curve passes exactly through
      ``(sigma_y / E, sigma_y)`` and ``(ultimate_true_strain, sigma_u_true)``;
    * perfectly plastic at ``sigma_u_true`` beyond the ultimate strain.

    API 579-1 Annex F paragraph F.2.3 / Annex 3D paragraph 3D.2.3 define the
    standard's own true stress-strain model with tabulated material
    coefficients; those are not reproduced -- this curve is driven by the
    inputs below, which the caller takes from a certificate, a test or
    :func:`lookup_grade`.

    Args (keyword-only; pint quantities of the right dimension accepted):
        E_mpa: Young's modulus at ``temperature_degc``.
        sigma_y_mpa: yield strength at ``temperature_degc``.
        sigma_u_mpa: ultimate strength at ``temperature_degc``; engineering
            (certificate) value unless ``sigma_u_is_true``.
        n: strain-hardening exponent, ``0 < n < 1`` (user input).
        temperature_degc: temperature the properties apply at (recorded; no
            derating is applied here).
        ultimate_true_strain: true strain at the ultimate; default ``n``, the
            Considere criterion for power-law hardening (Dieter, Mechanical
            Metallurgy, Ch. 8).
        sigma_u_is_true: ``sigma_u_mpa`` is already a true stress.
        name: free label for the deck.

    Derived: ``yield_strain``, ``sigma_u_true_mpa`` (engineering ultimate
    ``x exp(ultimate_true_strain)`` when converting), ``plastic_strain_at_ultimate``.
    """

    E_mpa: float
    sigma_y_mpa: float
    sigma_u_mpa: float
    n: float
    temperature_degc: float = 20.0
    ultimate_true_strain: Optional[float] = None
    sigma_u_is_true: bool = False
    name: str = ""

    def __post_init__(self) -> None:
        set_ = object.__setattr__
        set_(self, "E_mpa", _magnitude(self.E_mpa, "MPa", "E_mpa"))
        set_(self, "sigma_y_mpa", _magnitude(self.sigma_y_mpa, "MPa", "sigma_y_mpa"))
        set_(self, "sigma_u_mpa", _magnitude(self.sigma_u_mpa, "MPa", "sigma_u_mpa"))
        set_(self, "n", _magnitude(self.n, "dimensionless", "n"))
        set_(self, "temperature_degc",
             _magnitude(self.temperature_degc, "degC", "temperature_degc"))
        if self.E_mpa <= 0 or self.sigma_y_mpa <= 0 or self.sigma_u_mpa <= 0:
            raise ValueError("E_mpa, sigma_y_mpa and sigma_u_mpa must be positive.")
        if not 0.0 < self.n < 1.0:
            raise ValueError(f"hardening exponent n must be in (0, 1), got {self.n}")
        eps_u = (self.n if self.ultimate_true_strain is None
                 else _magnitude(self.ultimate_true_strain, "dimensionless",
                                 "ultimate_true_strain"))
        set_(self, "ultimate_true_strain", eps_u)
        if eps_u <= self.yield_strain:
            raise ValueError(
                f"ultimate_true_strain={eps_u} must exceed the yield strain "
                f"{self.yield_strain:.5g}.")
        if self.sigma_u_true_mpa <= self.sigma_y_mpa:
            raise ValueError(
                f"true ultimate {self.sigma_u_true_mpa:.1f} MPa must exceed yield "
                f"{self.sigma_y_mpa:.1f} MPa.")
        if self.plastic_strain_at_ultimate <= 0.0:
            raise ValueError("ultimate point lies inside the elastic line; check "
                             "E_mpa / sigma_u_mpa / ultimate_true_strain.")

    # --- derived ------------------------------------------------------------
    @property
    def yield_strain(self) -> float:
        """True strain at yield, ``sigma_y / E``."""
        return self.sigma_y_mpa / self.E_mpa

    @property
    def sigma_u_true_mpa(self) -> float:
        """True stress at the ultimate (MPa)."""
        if self.sigma_u_is_true:
            return self.sigma_u_mpa
        # sigma_true = sigma_eng (1 + e) = sigma_eng exp(eps_true), uniform strain.
        return self.sigma_u_mpa * math.exp(self.ultimate_true_strain)

    @property
    def plastic_strain_at_ultimate(self) -> float:
        return self.ultimate_true_strain - self.sigma_u_true_mpa / self.E_mpa

    # --- evaluation ---------------------------------------------------------
    def stress_from_plastic_strain(self, plastic_strain: float) -> float:
        """True stress (MPa) on the hardening branch at a plastic strain."""
        ep = float(plastic_strain)
        if ep < 0.0:
            raise ValueError("plastic strain must be >= 0.")
        ep_u = self.plastic_strain_at_ultimate
        if ep >= ep_u:
            return self.sigma_u_true_mpa
        return self.sigma_y_mpa + (self.sigma_u_true_mpa - self.sigma_y_mpa) * (
            ep / ep_u) ** self.n

    def stress(self, strain: float) -> float:
        """True stress (MPa) at a total true strain."""
        eps = float(strain)
        if eps < 0.0:
            raise ValueError("strain must be >= 0 (monotonic tensile curve).")
        if eps <= self.yield_strain:
            return self.E_mpa * eps
        if eps >= self.ultimate_true_strain:
            return self.sigma_u_true_mpa
        # Solve sigma = f(eps - sigma/E) on the hardening branch by bisection;
        # g(sigma) = sigma - f(eps - sigma/E) is strictly increasing in sigma.
        lo, hi = self.sigma_y_mpa, min(self.sigma_u_true_mpa, self.E_mpa * eps)
        for _ in range(200):
            mid = 0.5 * (lo + hi)
            g = mid - self.stress_from_plastic_strain(eps - mid / self.E_mpa)
            if g > 0.0:
                hi = mid
            else:
                lo = mid
            if hi - lo <= 1e-12 * hi:
                break
        return 0.5 * (lo + hi)

    def plastic_strain(self, strain: float) -> float:
        """Plastic part of a total true strain."""
        return float(strain) - self.stress(strain) / self.E_mpa

    # --- solver tables ------------------------------------------------------
    def _plastic_grid(self, n_interior: int) -> list[float]:
        ep_u = self.plastic_strain_at_ultimate
        # Quadratic spacing: denser near yield where the power law is steep.
        return [ep_u * (k / (n_interior + 1)) ** 2 for k in range(1, n_interior + 1)]

    def table(self, n_points: int = 20, strain_max: Optional[float] = None
              ) -> list[tuple[float, float]]:
        """``(true_strain, true_stress_mpa)`` rows for a total-strain deck.

        Rows: origin, yield point, ``n_points - 4`` hardening points, the
        ultimate point, and a plateau point at ``strain_max`` (default twice
        the ultimate strain).  ``n_points >= 4``.
        """
        if n_points < 4:
            raise ValueError("n_points must be >= 4 (origin, yield, ultimate, plateau).")
        eps_max = 2.0 * self.ultimate_true_strain if strain_max is None else float(strain_max)
        if eps_max <= self.ultimate_true_strain:
            raise ValueError("strain_max must exceed ultimate_true_strain.")
        rows = [(0.0, 0.0), (self.yield_strain, self.sigma_y_mpa)]
        for ep in self._plastic_grid(n_points - 4):
            s = self.stress_from_plastic_strain(ep)
            rows.append((ep + s / self.E_mpa, s))
        rows.append((self.ultimate_true_strain, self.sigma_u_true_mpa))
        rows.append((eps_max, self.sigma_u_true_mpa))
        return rows

    def plastic_table(self, n_points: int = 20, strain_max: Optional[float] = None
                      ) -> list[tuple[float, float]]:
        """``(true_stress_mpa, plastic_strain)`` rows for a ``*PLASTIC`` card.

        Rows: yield (plastic strain 0), ``n_points - 3`` hardening points, the
        ultimate point, and a plateau point at ``strain_max`` (default twice
        the ultimate strain).  ``n_points >= 3``.
        """
        if n_points < 3:
            raise ValueError("n_points must be >= 3 (yield, ultimate, plateau).")
        eps_max = 2.0 * self.ultimate_true_strain if strain_max is None else float(strain_max)
        if eps_max <= self.ultimate_true_strain:
            raise ValueError("strain_max must exceed ultimate_true_strain.")
        rows = [(self.sigma_y_mpa, 0.0)]
        for ep in self._plastic_grid(n_points - 3):
            rows.append((self.stress_from_plastic_strain(ep), ep))
        rows.append((self.sigma_u_true_mpa, self.plastic_strain_at_ultimate))
        rows.append((self.sigma_u_true_mpa, eps_max - self.sigma_u_true_mpa / self.E_mpa))
        return rows


# ---------------------------------------------------------------------------
# Grade lookup
# ---------------------------------------------------------------------------
# IACS UR W11 Charpy test temperature by toughness class (degC): normal-strength
# grades A/B/D/E and higher-strength AH/DH/EH/FH (the "A" class differs).
_HULL_NS_CHARPY_DEGC = {"A": 20.0, "B": 0.0, "D": -20.0, "E": -40.0}
_HULL_HS_CHARPY_DEGC = {"A": 0.0, "D": -20.0, "E": -40.0, "F": -60.0}


@dataclass(frozen=True)
class FFSMaterialInputs:
    """Material inputs for an FFS assessment, as returned by :func:`lookup_grade`.

    Strengths are the specified minima of the governing standard at room
    temperature; ``temperature_degc`` records the assessment temperature only
    (no derating is applied -- supply elevated-temperature properties yourself
    via :meth:`with_properties`).  ``hardening_exponent`` is a user input: no
    public grade-wise default is adopted.
    """

    name: str
    standard: str
    E_mpa: float
    sigma_y_mpa: float
    sigma_u_mpa: float
    nu: float
    rho_kg_m3: float
    material_class: str
    temperature_degc: float = 20.0
    charpy_test_temperature_degc: Optional[float] = None
    hardening_exponent: Optional[float] = None
    source: str = ""

    def with_properties(self, **changes) -> "FFSMaterialInputs":
        """Copy with fields replaced (e.g. certificate or hot properties)."""
        return replace(self, **changes)

    def true_stress_strain_curve(self, n: Optional[Number] = None, **kwargs
                                 ) -> TrueStressStrainCurve:
        """Build the :class:`TrueStressStrainCurve` for this material.

        ``n`` (or ``hardening_exponent`` on the record) is required; any other
        :class:`TrueStressStrainCurve` keyword overrides the record.
        """
        exponent = self.hardening_exponent if n is None else n
        if exponent is None:
            raise ValueError(
                f"{self.name}: a strain-hardening exponent n is required (user input; "
                "no public grade-wise default is adopted).")
        params = dict(E_mpa=self.E_mpa, sigma_y_mpa=self.sigma_y_mpa,
                      sigma_u_mpa=self.sigma_u_mpa, n=exponent,
                      temperature_degc=self.temperature_degc, name=self.name)
        params.update(kwargs)
        return TrueStressStrainCurve(**params)

    def lower_bound_toughness(self, **kwargs) -> ToughnessEstimate:
        """:func:`lower_bound_toughness` with this material's class and yield."""
        params = dict(material_class=self.material_class, sigma_y_mpa=self.sigma_y_mpa)
        params.update(kwargs)
        return lower_bound_toughness(**params)


def _charpy_test_temperature(grade: MaterialGrade) -> Optional[float]:
    if grade.standard != "IACS UR W11" or not grade.toughness_grade:
        return None
    table = _HULL_NS_CHARPY_DEGC if grade.name.startswith("Grade ") else _HULL_HS_CHARPY_DEGC
    return table.get(grade.toughness_grade.upper())


def lookup_grade(
    name: str,
    *,
    temperature_degc: Number = 20.0,
    hardening_exponent: Optional[Number] = None,
) -> FFSMaterialInputs:
    """Material inputs for a grade in :mod:`digitalmodel.materials.grades`.

    Accepts the canonical name, ISO L-grade, UNS number or stainless short form
    (``"X65"``, ``"L450"``, ``"DH36"``, ``"S355"``, ``"TP316L"``, ``"316L"``,
    ``"S31803"``).  Case-insensitive.

    Raises:
        KeyError: unknown grade.
    """
    g = _grades.get(name)
    return FFSMaterialInputs(
        name=g.name,
        standard=g.standard,
        E_mpa=g.E_mpa,
        sigma_y_mpa=g.smys_mpa,
        sigma_u_mpa=g.smts_mpa,
        nu=g.nu,
        rho_kg_m3=g.rho_kg_m3,
        material_class=g.material_class,
        temperature_degc=_magnitude(temperature_degc, "degC", "temperature_degc"),
        charpy_test_temperature_degc=_charpy_test_temperature(g),
        hardening_exponent=_optional(hardening_exponent, "dimensionless",
                                     "hardening_exponent"),
        source=g.source,
    )
