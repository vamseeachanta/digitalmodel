# ABOUTME: SI friction kernel: ITTC-57 C_F, ITTC-78 simplified model-to-ship transfer, Prohaska form factor
# ABOUTME: Fluid is a caller parameter; every allowance is declared or zero-with-flag (#2239 W1)
"""
SI friction and model-to-ship resistance transfer (issue #2239, work package W1).

* ``ittc57_cf`` is the ITTC-1957 model-ship correlation line, delegating to
  :func:`digitalmodel.naval_architecture.resistance.ittc_1957_cf` so both stay identical.
  ``ittc57_cf_cited`` reuses the existing opt-in citation wrapper
  :func:`~digitalmodel.naval_architecture.resistance.ittc_1957_cf_cited`.
* ``transfer_model_to_ship`` follows the simplified resistance transfer of
  ITTC Recommended Procedure 7.5-02-03-01.4 (1978 ITTC performance prediction method):

      C_R    = C_T,m - (1+k) C_F,m - C_A,m
      C_T,s  = (1+k) C_F,s + C_R + C_A,s + dC_F

  C_A,m, C_A,s and dC_F are caller-declared or taken as zero and flagged.
* ``prohaska_form_factor`` fits C_T/C_F = (1+k) + c Fn^4/C_F by least squares.

No fluid property is hidden in this module: the caller supplies a :class:`Fluid`
(or Reynolds numbers directly).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Sequence

import numpy as np

from digitalmodel.naval_architecture import resistance as _resistance

ITTC_TRANSFER_PROCEDURE = (
    "ITTC Recommended Procedure 7.5-02-03-01.4, 1978 ITTC Performance Prediction Method "
    "(simplified resistance transfer, form-factor approach)"
)

PROHASKA_FN_MIN = 0.10
PROHASKA_FN_MAX = 0.20
PROHASKA_MIN_POINTS = 6
PROHASKA_MAX_CONDITION = 1e6

# ITTC-57 is singular at Re = 100 (log10(Re) - 2 = 0) and meaningless below it.
_ITTC57_MIN_RE = 100.0


def _finite(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value!r}")
    return value


def _positive(name: str, value: float) -> float:
    value = _finite(name, value)
    if value <= 0.0:
        raise ValueError(f"{name} must be positive, got {value!r}")
    return value


@dataclass(frozen=True)
class Fluid:
    """Caller-declared fluid. SI: rho in kg/m^3, nu in m^2/s."""

    name: str
    rho: float
    nu: float

    def __post_init__(self) -> None:
        _positive("fluid density rho", self.rho)
        _positive("fluid kinematic viscosity nu", self.nu)


def reynolds_number_si(speed_m_s: float, length_m: float, fluid: Fluid) -> float:
    """Re = U L / nu (SI)."""
    _positive("speed", speed_m_s)
    _positive("length", length_m)
    return speed_m_s * length_m / fluid.nu


def froude_number(speed_m_s: float, length_m: float, *, g: float = 9.80665) -> float:
    """Fn = U / sqrt(g L). ``g`` defaults to standard gravity and may be overridden."""
    _positive("speed", speed_m_s)
    _positive("length", length_m)
    _positive("g", g)
    return speed_m_s / math.sqrt(g * length_m)


def _check_re(re: float) -> float:
    re = _finite("Reynolds number", re)
    if re <= _ITTC57_MIN_RE:
        raise ValueError(
            f"Reynolds number must exceed {_ITTC57_MIN_RE:g} for the ITTC-57 line "
            f"(singular at Re = 100), got {re!r}"
        )
    return re


def ittc57_cf(re: float) -> float:
    """ITTC-1957 correlation line C_F = 0.075 / (log10 Re - 2)^2, validated input.

    Scalar, no citation sidecar (same value as the legacy ``resistance.ittc_1957_cf``). Use
    :func:`ittc57_cf_cited` where the value is reported; the transfer functions cite by default.
    """
    return _resistance.ittc_1957_cf(_check_re(re))


def ittc57_cf_cited(re: float, *, repo_root: Optional[Path] = None) -> dict:
    """``ittc57_cf`` with the existing EN400 citation sidecar (``resistance.ittc_1957_cf_cited``).

    Returns ``{"value", "units", "citations"}``; fail-closed / standalone behaviour is that
    of the reused wrapper.
    """
    return _resistance.ittc_1957_cf_cited(_check_re(re), repo_root=repo_root)


# ---------------------------------------------------------------- transfer


@dataclass(frozen=True)
class AllowanceTerm:
    """An additive allowance; ``declared`` is False when it defaulted to zero."""

    value: float
    declared: bool


@dataclass(frozen=True)
class UnresolvedCitation:
    """A standards reference that the citations registry cannot resolve.

    Carried explicitly (never as a fabricated registry entry) so that the gap is visible in
    every result. ``status`` is ``"unresolved-in-registry"``.
    """

    source_id: str
    publisher: str
    title: str
    clause: str
    status: str = "unresolved-in-registry"
    note: str = ""


# ITTC 7.5-02-03-01.4 has no entry in digitalmodel.citations.registry and no wiki target
# known to the resolver (checked 2026-09-27). Open item on #2239: add a registry getter and
# wiki page with #2471 frontmatter, then replace this record by a resolved Citation.
ITTC_TRANSFER_UNRESOLVED = UnresolvedCitation(
    source_id="ITTC-7.5-02-03-01.4",
    publisher="ITTC",
    title="ITTC Recommended Procedure 7.5-02-03-01.4, 1978 ITTC Performance Prediction Method",
    clause="simplified resistance transfer (form-factor approach): C_R conserved at equal Fn",
    note="no registry getter or wiki page exists; open item on digitalmodel#2239",
)


@dataclass(frozen=True)
class TransferResult:
    ct_model: float
    ct_ship: float
    cr: float
    cf_model: float
    cf_ship: float
    form_factor_k: float
    re_model: float
    re_ship: float
    fn_model: float
    fn_ship: float
    allowances: dict
    assumptions: tuple
    omitted_corrections: tuple
    procedure: str = ITTC_TRANSFER_PROCEDURE
    citations: list = field(default_factory=list)
    unresolved_citations: tuple = (ITTC_TRANSFER_UNRESOLVED,)
    cited: bool = True


_ASSUMPTIONS = (
    "Froude similarity: model and ship are compared at equal Froude number",
    "Geometric similarity: model is a geometrically similar scale model of the ship",
    "Form factor (1+k) is invariant with Reynolds number between model and ship scale",
    "Residuary coefficient C_R is conserved between model and ship at equal Froude number",
    "Friction is the ITTC-1957 correlation line evaluated at the declared Reynolds numbers",
)

_ALWAYS_OMITTED = (
    "air resistance C_AA (not modelled by this transfer)",
    "appendage resistance (not modelled by this transfer)",
    "wind, waves, shallow water and blockage corrections (not modelled by this transfer)",
)

_ALLOWANCE_DESCRIPTIONS = {
    "ca_model": "ca_model: model correlation allowance C_A,m",
    "ca_ship": "ca_ship: ship correlation allowance C_A,s",
    "delta_cf": "delta_cf: roughness allowance dC_F",
}


def _allowance(name: str, value: Optional[float]) -> AllowanceTerm:
    if value is None:
        return AllowanceTerm(0.0, False)
    return AllowanceTerm(_finite(name, value), True)


def _common_inputs(
    re_model: float,
    re_ship: float,
    form_factor_k: float,
    fn_model: float,
    fn_ship: float,
    fn_rel_tolerance: float,
    ca_model: Optional[float],
    ca_ship: Optional[float],
    delta_cf: Optional[float],
    cite: bool,
    repo_root: Optional[Path],
):
    k = _finite("form factor k", form_factor_k)
    if k < 0.0:
        raise ValueError(f"form factor k must be non-negative, got {k!r}")
    fn_m = _positive("model Froude number", fn_model)
    fn_s = _positive("ship Froude number", fn_ship)
    tol = _positive("Froude tolerance", fn_rel_tolerance)
    if abs(fn_m - fn_s) > tol * max(fn_m, fn_s):
        raise ValueError(
            f"Froude numbers differ (model {fn_m!r}, ship {fn_s!r}) beyond relative tolerance "
            f"{tol:g}; the transfer is valid only at equal Froude number"
        )
    if cite:
        cm = ittc57_cf_cited(re_model, repo_root=repo_root)
        cs = ittc57_cf_cited(re_ship, repo_root=repo_root)
        cf_m, cf_s = cm["value"], cs["value"]
        citations = list(cm["citations"]) + [c for c in cs["citations"] if c not in cm["citations"]]
    else:
        cf_m, cf_s = ittc57_cf(re_model), ittc57_cf(re_ship)
        citations = []
    allowances = {
        "ca_model": _allowance("ca_model", ca_model),
        "ca_ship": _allowance("ca_ship", ca_ship),
        "delta_cf": _allowance("delta_cf", delta_cf),
    }
    omitted = list(_ALWAYS_OMITTED)
    omitted += [
        f"{_ALLOWANCE_DESCRIPTIONS[n]} not declared; taken as zero"
        for n, t in allowances.items()
        if not t.declared
    ]
    if not cite:
        omitted.append("citation sidecar not emitted (caller passed cite=False)")
    return k, fn_m, fn_s, cf_m, cf_s, allowances, tuple(omitted), citations


def transfer_model_to_ship(
    *,
    ct_model: float,
    re_model: float,
    re_ship: float,
    form_factor_k: float,
    fn_model: float,
    fn_ship: float,
    ca_model: Optional[float] = None,
    ca_ship: Optional[float] = None,
    delta_cf: Optional[float] = None,
    fn_rel_tolerance: float = 1e-3,
    cite: bool = True,
    repo_root: Optional[Path] = None,
) -> TransferResult:
    """Transfer a model total-resistance coefficient to ship scale (ITTC 7.5-02-03-01.4).

    Refuses (ValueError) when the Froude numbers differ beyond ``fn_rel_tolerance``,
    for invalid Reynolds numbers, a negative/non-finite form factor or a non-positive C_T,m.
    The EN400 ITTC-57 citation sidecar is emitted by default through the existing
    fail-closed wrapper (``resistance.ittc_1957_cf_cited``): a configured wiki without the
    page raises ``CitationResolutionError``; an unconfigured resolver degrades with a one-shot
    RuntimeWarning and an empty ``citations`` list. ``cite=False`` opts out and is recorded
    in ``omitted_corrections``. The ITTC 7.5-02-03-01.4 procedure is carried as an explicit
    :class:`UnresolvedCitation` in ``unresolved_citations``.
    """
    ct_m = _positive("model total resistance coefficient ct_model", ct_model)
    k, fn_m, fn_s, cf_m, cf_s, allow, omitted, citations = _common_inputs(
        re_model, re_ship, form_factor_k, fn_model, fn_ship, fn_rel_tolerance,
        ca_model, ca_ship, delta_cf, cite, repo_root,
    )
    cr = ct_m - (1.0 + k) * cf_m - allow["ca_model"].value
    ct_s = (1.0 + k) * cf_s + cr + allow["ca_ship"].value + allow["delta_cf"].value
    return TransferResult(
        ct_model=ct_m, ct_ship=ct_s, cr=cr, cf_model=cf_m, cf_ship=cf_s, form_factor_k=k,
        re_model=float(re_model), re_ship=float(re_ship), fn_model=fn_m, fn_ship=fn_s,
        allowances=allow, assumptions=_ASSUMPTIONS, omitted_corrections=omitted,
        citations=citations, cited=bool(cite),
    )


def transfer_ship_to_model(
    *,
    ct_ship: float,
    re_model: float,
    re_ship: float,
    form_factor_k: float,
    fn_model: float,
    fn_ship: float,
    ca_model: Optional[float] = None,
    ca_ship: Optional[float] = None,
    delta_cf: Optional[float] = None,
    fn_rel_tolerance: float = 1e-3,
    cite: bool = True,
    repo_root: Optional[Path] = None,
) -> TransferResult:
    """Inverse of :func:`transfer_model_to_ship` (same conserved C_R and citation behaviour)."""
    ct_s = _positive("ship total resistance coefficient ct_ship", ct_ship)
    k, fn_m, fn_s, cf_m, cf_s, allow, omitted, citations = _common_inputs(
        re_model, re_ship, form_factor_k, fn_model, fn_ship, fn_rel_tolerance,
        ca_model, ca_ship, delta_cf, cite, repo_root,
    )
    cr = ct_s - (1.0 + k) * cf_s - allow["ca_ship"].value - allow["delta_cf"].value
    ct_m = (1.0 + k) * cf_m + cr + allow["ca_model"].value
    return TransferResult(
        ct_model=ct_m, ct_ship=ct_s, cr=cr, cf_model=cf_m, cf_ship=cf_s, form_factor_k=k,
        re_model=float(re_model), re_ship=float(re_ship), fn_model=fn_m, fn_ship=fn_s,
        allowances=allow, assumptions=_ASSUMPTIONS, omitted_corrections=omitted,
        citations=citations, cited=bool(cite),
    )


# ---------------------------------------------------------------- Prohaska


@dataclass(frozen=True)
class ProhaskaFit:
    k: float
    c: float
    residual_rms: float
    k_ci95: tuple
    condition_number: float
    n_points: int
    fn_window: tuple = (PROHASKA_FN_MIN, PROHASKA_FN_MAX)
    error_model: str = (
        "y = C_T/C_F = (1+k) + c Fn^4/C_F + e, e independent, homoscedastic and additive on "
        "y (ordinary least squares); CI from Student's t with n-2 dof"
    )


def prohaska_form_factor(
    fn: Sequence[float], ct: Sequence[float], cf: Sequence[float]
) -> ProhaskaFit:
    """Least-squares Prohaska fit C_T/C_F = (1+k) + c Fn^4/C_F.

    Admissible data: every point within Fn 0.10-0.20 (points outside are refused, not
    dropped), at least 6 points. Refuses a design matrix whose 2-norm condition number
    exceeds 1e6.

    Error model: the residuals of y = C_T/C_F are independent, homoscedastic and additive
    (ordinary least squares). The parameter covariance is s^2 (R^T R)^-1 from a QR
    factorisation of the design matrix (normal equations are not formed); the 95 %
    confidence interval on k uses Student's t with n-2 dof. Multiplicative (relative) noise
    on C_T is only approximately covered by this model.
    """
    from scipy import stats
    from scipy.linalg import solve_triangular

    fn_a = np.asarray(fn, dtype=float)
    ct_a = np.asarray(ct, dtype=float)
    cf_a = np.asarray(cf, dtype=float)
    if not (fn_a.shape == ct_a.shape == cf_a.shape) or fn_a.ndim != 1:
        raise ValueError("fn, ct and cf must be 1-D sequences of equal length")
    n = fn_a.size
    if n < PROHASKA_MIN_POINTS:
        raise ValueError(f"Prohaska fit needs at least {PROHASKA_MIN_POINTS} points, got {n}")
    if not (np.all(np.isfinite(fn_a)) and np.all(np.isfinite(ct_a)) and np.all(np.isfinite(cf_a))):
        raise ValueError("Prohaska inputs must be finite")
    if np.any(ct_a <= 0) or np.any(cf_a <= 0):
        raise ValueError("C_T and C_F must be positive")
    outside = (fn_a < PROHASKA_FN_MIN - 1e-12) | (fn_a > PROHASKA_FN_MAX + 1e-12)
    if np.any(outside):
        raise ValueError(
            f"Prohaska admissible window is Fn {PROHASKA_FN_MIN:.2f}-{PROHASKA_FN_MAX:.2f}; "
            f"points outside it: {fn_a[outside].tolist()}"
        )
    y = ct_a / cf_a
    x = fn_a**4 / cf_a
    a_mat = np.column_stack([np.ones(n), x])
    cond = float(np.linalg.cond(a_mat))
    if not np.isfinite(cond) or cond > PROHASKA_MAX_CONDITION:
        raise ValueError(
            f"Prohaska design matrix condition number {cond:.3g} exceeds {PROHASKA_MAX_CONDITION:g}"
        )
    q_mat, r_mat = np.linalg.qr(a_mat)
    coef = solve_triangular(r_mat, q_mat.T @ y)
    resid = y - a_mat @ coef
    dof = n - 2
    s2 = float(resid @ resid) / dof
    r_inv = solve_triangular(r_mat, np.eye(2))
    cov = s2 * (r_inv @ r_inv.T)
    se_a = math.sqrt(max(cov[0, 0], 0.0))
    t = float(stats.t.ppf(0.975, dof))
    k = float(coef[0] - 1.0)
    return ProhaskaFit(
        k=k,
        c=float(coef[1]),
        residual_rms=float(math.sqrt(float(resid @ resid) / n)),
        k_ci95=(k - t * se_a, k + t * se_a),
        condition_number=cond,
        n_points=int(n),
    )
