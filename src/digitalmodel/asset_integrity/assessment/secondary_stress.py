# ABOUTME: Secondary (residual) stress in Kr: named API-Phi and BS 7910-rho methods,
# ABOUTME: user-supplied profiles and relaxation, labelled screening bounds (#2157 P2).
"""Secondary stress contribution to the fracture ratio Kr.

Two plasticity-interaction methods, kept separate because they are not
interchangeable (owner card G01):

- :func:`kr_api_phi`: ``Kr = (K_P + Phi K_S) / Kmat``, with a user-supplied
  ``Phi >= 1``.
- :func:`kr_bs7910_rho`: ``Kr = (K_P + K_S) / Kmat + rho``, with a user-supplied
  ``rho >= 0``.

Neither method has a default value, and no procedure for computing Phi or rho ships
here. Each needs a basis string (owner card G05).

Residual-stress profiles (owner card G02):

- :class:`UniformResidual`: ``K_S = Y sigma_R sqrt(pi a)``, with a stated geometry
  factor Y and an optional relaxation factor in (0, 1].
- :class:`PolynomialResidual`: ``sigma(x) = sum_i s_i (x/t)^i``, with
  ``K_S = sqrt(pi a) sum_i s_i (a/t)^i G_i``, where the influence coefficients G_i are
  supplied by the user with a basis. This is the general superposition form; no
  tabulated coefficients ship.

:func:`residual_screening_bounds` returns the relaxed-yield, yield and flow-magnitude
bounds as records labelled ``screening`` (owner card B11). They are sensitivities and
never the disposition. Units: MPa, mm, MPa*sqrt(m).
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import List, Sequence, Tuple


def _finite(name: str, value: float) -> float:
    if not (isinstance(value, (int, float)) and math.isfinite(value)):
        raise ValueError(f"{name} must be a finite number (got {value!r}).")
    return float(value)


def _positive(name: str, value: float) -> float:
    if _finite(name, value) <= 0:
        raise ValueError(f"{name} must be > 0 (got {value!r}).")
    return float(value)


def _basis(basis: str) -> str:
    if not basis or not basis.strip():
        raise ValueError("a stated basis is required.")
    return basis.strip()


@dataclass(frozen=True)
class UniformResidual:
    """Uniform residual stress over the crack depth: ``K_S = Y relax sigma_R sqrt(pi a)``."""

    stress_mpa: float
    y: float
    basis: str
    relaxation: float = 1.0

    def __post_init__(self) -> None:
        _finite("stress_mpa", self.stress_mpa)
        _positive("y", self.y)
        _basis(self.basis)
        if not (_finite("relaxation", self.relaxation) > 0 and self.relaxation <= 1):
            raise ValueError("relaxation must lie in (0, 1].")

    def k(self, a_mm: float) -> float:
        _positive("a_mm", a_mm)
        return (
            self.y
            * self.relaxation
            * self.stress_mpa
            * math.sqrt(math.pi * a_mm / 1000.0)
        )


@dataclass(frozen=True)
class PolynomialResidual:
    """Polynomial residual profile with user-supplied influence coefficients G_i."""

    coefficients_mpa: Tuple[float, ...]
    influence: Tuple[float, ...]
    wall_mm: float
    basis: str
    relaxation: float = 1.0

    def __post_init__(self) -> None:
        if len(self.coefficients_mpa) == 0 or len(self.coefficients_mpa) != len(
            self.influence
        ):
            raise ValueError("need one influence coefficient per polynomial term.")
        for i, (s, g) in enumerate(zip(self.coefficients_mpa, self.influence)):
            _finite(f"s{i}", s)
            _finite(f"G{i}", g)
        _positive("wall_mm", self.wall_mm)
        _basis(self.basis)
        if not (_finite("relaxation", self.relaxation) > 0 and self.relaxation <= 1):
            raise ValueError("relaxation must lie in (0, 1].")

    def k(self, a_mm: float) -> float:
        _positive("a_mm", a_mm)
        if a_mm >= self.wall_mm:
            raise ValueError("crack depth must lie inside the wall.")
        r = a_mm / self.wall_mm
        total = sum(
            s * r**i * g
            for i, (s, g) in enumerate(zip(self.coefficients_mpa, self.influence))
        )
        return self.relaxation * math.sqrt(math.pi * a_mm / 1000.0) * total


def kr_api_phi(
    k_primary: float, k_secondary: float, kmat: float, *, phi: float, basis: str
) -> float:
    """API-form Kr: ``(K_P + Phi K_S) / Kmat``, with user-supplied ``Phi >= 1``."""
    _basis(basis)
    _positive("kmat", kmat)
    if _finite("phi", phi) < 1.0:
        raise ValueError("Phi must be >= 1.")
    return (
        _finite("k_primary", k_primary) + phi * _finite("k_secondary", k_secondary)
    ) / kmat


def kr_bs7910_rho(
    k_primary: float, k_secondary: float, kmat: float, *, rho: float, basis: str
) -> float:
    """BS 7910-form Kr: ``(K_P + K_S) / Kmat + rho``, with user-supplied ``rho >= 0``."""
    _basis(basis)
    _positive("kmat", kmat)
    if _finite("rho", rho) < 0.0:
        raise ValueError("rho must be >= 0.")
    return (
        _finite("k_primary", k_primary) + _finite("k_secondary", k_secondary)
    ) / kmat + rho


METHODS = ("api579_phi", "bs7910_rho")


def kr_total(
    method: str,
    k_primary: float,
    k_secondary: float,
    kmat: float,
    *,
    value: float,
    basis: str,
) -> float:
    """Dispatch to a named method. ``value`` is Phi or rho according to ``method``."""
    if method == "api579_phi":
        return kr_api_phi(k_primary, k_secondary, kmat, phi=value, basis=basis)
    if method == "bs7910_rho":
        return kr_bs7910_rho(k_primary, k_secondary, kmat, rho=value, basis=basis)
    raise ValueError(f"unknown method {method!r}; use one of {METHODS}.")


@dataclass(frozen=True)
class ScreeningBound:
    label: str
    kind: str  # always "screening"
    sigma_r_mpa: float
    k_secondary: float
    kr: float
    basis: str


def residual_screening_bounds(
    *,
    k_primary: float,
    kmat: float,
    a_mm: float,
    y: float,
    sigma_y_mpa: float,
    sigma_u_mpa: float,
    relaxation: float,
    basis: str,
    rho: float,
) -> List[ScreeningBound]:
    """Relaxed-yield, yield and flow-magnitude residual bounds, each labelled screening.

    The bounds use the BS 7910 form with a caller-stated ``rho`` (no default, owner
    card G01). They are sensitivities, not a disposition.
    """
    _basis(basis)
    sy, su = _positive("sigma_y_mpa", sigma_y_mpa), _positive(
        "sigma_u_mpa", sigma_u_mpa
    )
    if su < sy:
        raise ValueError("need sigma_u >= sigma_y.")
    cases: Sequence[Tuple[str, float, float]] = (
        ("relaxed_yield", sy, relaxation),
        ("yield", sy, 1.0),
        ("flow", 0.5 * (sy + su), 1.0),
    )
    out = []
    for label, sigma_r, relax in cases:
        prof = UniformResidual(stress_mpa=sigma_r, y=y, basis=basis, relaxation=relax)
        ks = prof.k(a_mm)
        out.append(
            ScreeningBound(
                label=label,
                kind="screening",
                sigma_r_mpa=sigma_r,
                k_secondary=ks,
                kr=kr_bs7910_rho(k_primary, ks, kmat, rho=rho, basis=basis),
                basis=basis,
            )
        )
    return out
