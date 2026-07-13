"""Closed-form narrow-band spectral fatigue (legacy FATG method).

Port of a legacy in-house Fortran spectral-fatigue program ("FATG",
1989-1991 era) used for plate structures and tubular jack-up legs. The
method is the classic closed-form chain:

    stress RAO x Bretschneider wave spectrum x wave scatter table
    x directional spreading x S-N curve -> Miner damage summation

Algorithm (faithful to the Fortran arithmetic):

1. **Significant stress response** per sea state (``SPECTO`` subroutine)::

       sigma_s = Hs * sqrt( 2 * trapz( RAO(w)^2 * 1400/(Tz^4 w^5)
                                       * exp(-700/(Tz^4 w^4)), w ) )

   The kernel is algebraically identical to a standard Bretschneider
   spectrum ``S(w) = (5/16) Hs^2 wp^4 / w^5 * exp(-1.25 (wp/w)^4)`` with
   peak frequency ``wp = 560**0.25 / Tz`` (i.e. ``Tp ~= 1.2916 Tz``), and
   ``sigma_s`` is then the *significant stress range* ``4*sqrt(m0)`` of
   the response.  (The source comments quote ``Tp = Tz/0.71`` and print
   ``Tp = 1.3 Tz``; the integration constants above are what the code
   actually computes.)

2. **Three-block Rayleigh discretisation**: the stress-range distribution
   in each sea state is collapsed to three constant-amplitude blocks at
   ``(1.271, 0.59, 0.28) * sigma_s``, each carrying one third of the
   cycles of its wave-heading group.

3. **Directional spreading**: cycles are distributed over three heading
   groups (0deg, +/-45deg, 90deg to the primary direction) through a fixed
   spreading matrix applied to the input percentages ``(p0, p45, p90)``::

       n0   ~ 0.475*p0 + 0.5*p45 + 0.025*p90
       n45  ~ 0.25 *p0 + 0.5*p45 + 0.25 *p90
       n90  ~ 0.025*p0 + 0.5*p45 + 0.475*p90

   and per-heading stress reduction factors are applied (member-type
   presets below).  With ``spreading=None`` the program's uniform mode is
   reproduced: every heading group carries the full cycle count
   ``T * occurrence / (3 Tz)`` per block (deliberately conservative).

4. **S-N + Miner**: one-slope S-N curve ``log10(N) = log10_a - m*log10(S)``
   with legacy DNV constants (English units, stress in psi, m = 3):
   E = 18.499, F = 18.2845, T = 18.64786.  Damage per year is the Miner
   sum over sea states, blocks and headings.

Units: any consistent set. The legacy constants assume stress RAO in psi
per unit wave height (ft), Hs in ft, Tz in s. Supplying a metric S-N
intercept makes the whole chain metric.

Validated against a line-by-line transliteration of the legacy Fortran
(see ``tests/fatigue/test_fatg.py`` for the oracle and tolerances).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional, Sequence

__all__ = [
    "SECONDS_PER_YEAR",
    "FATG_SN_CURVES",
    "FATG_BLOCK_FACTORS",
    "FATG_SPREADING_MATRIX",
    "FATG_DIRECTION_FACTOR_PRESETS",
    "FatgSeaState",
    "FatgSeaStateResult",
    "FatgResult",
    "fatg_significant_stress_range",
    "fatg_heading_cycles",
    "fatg_sea_state_damage",
    "fatg_annual_damage",
]

# Seconds per (365-day) year, as hard-coded in the legacy program (TIME).
SECONDS_PER_YEAR = 31_536_000.0

# Legacy one-slope DNV S-N intercepts, log10(a), English units (psi), m = 3.
FATG_SN_CURVES = {
    "E": 18.499,
    "F": 18.2845,
    "T": 18.64786,
}

# Three-block discretisation of the Rayleigh stress-range distribution:
# block stress = factor * significant stress range, each block carries an
# equal share (1/3) of the heading-group cycle count.
FATG_BLOCK_FACTORS = (1.271, 0.59, 0.28)

# Fixed spreading matrix: rows = heading group (0, 45, 90 deg), columns =
# input percentage (p0, p45, p90).
FATG_SPREADING_MATRIX = (
    (0.475, 0.5, 0.025),
    (0.25, 0.5, 0.25),
    (0.025, 0.5, 0.475),
)

# Per-heading stress reduction factors (0deg, 45deg, 90deg groups).
#   general  : original program (cos 45deg at 45; cos 78.75deg at 90 --
#              "theoretically 0 but that would not be conservative")
#   chord    : later 3-chord-leg variant, chord members (unity at 0 and 45)
#   diagonal : later variant, diagonals/horizontals (no reduction)
FATG_DIRECTION_FACTOR_PRESETS = {
    "general": (1.0, math.cos(math.pi / 4.0), math.cos(math.pi * 7.0 / 16.0)),
    "chord": (1.0, 1.0, math.cos(math.pi * 7.0 / 16.0)),
    "diagonal": (1.0, 1.0, 1.0),
}


@dataclass(frozen=True)
class FatgSeaState:
    """One row of the wave scatter table.

    ``occurrence`` is the fraction of the year spent in this sea state.
    ``spreading = (p0, p45, p90)`` are the directional percentages of the
    legacy input deck (typically summing to 1); ``None`` selects the
    program's uniform (no-spreading-input) mode.
    """

    hs: float
    tz: float
    occurrence: float
    spreading: Optional[tuple[float, float, float]] = None


@dataclass(frozen=True)
class FatgSeaStateResult:
    hs: float
    tz: float
    tp: float
    occurrence: float
    significant_stress_range: float
    damage_per_year: float


@dataclass(frozen=True)
class FatgResult:
    """Aggregate result. ``annual_damage`` is the Miner sum per year;
    the legacy report echoes are ``life_used_percent_*``."""

    annual_damage: float
    fatigue_life_years: float
    life_used_percent_1yr: float
    life_used_percent_10yr: float
    sea_states: list[FatgSeaStateResult] = field(default_factory=list)


def fatg_significant_stress_range(
    omega: Sequence[float],
    stress_rao: Sequence[float],
    hs: float,
    tz: float,
) -> float:
    """Significant stress range of the response in one sea state.

    Trapezoidal integration of ``RAO(w)^2 * S_B(w)`` on the RAO's own
    frequency grid (rad/s), exactly as the legacy ``SPECTO`` subroutine.
    The legacy deck always used 18 points; any length >= 2 is accepted.
    """
    _validate_rao(omega, stress_rao)
    if hs < 0.0:
        raise ValueError("hs must be non-negative")
    if tz <= 0.0:
        raise ValueError("tz must be positive")

    st4 = 1.0 / tz**4
    st2 = 1400.0 * st4
    st3 = 700.0 * st4

    def ordinate(i: int) -> float:
        w = omega[i]
        return stress_rao[i] ** 2 * w ** (-5) * st2 * math.exp(-st3 * w ** (-4))

    total = 0.0
    for i in range(len(omega) - 1):
        total += 0.5 * (ordinate(i) + ordinate(i + 1)) * (omega[i + 1] - omega[i])
    return math.sqrt(2.0 * total) * hs


def fatg_heading_cycles(
    tz: float,
    occurrence: float,
    spreading: Optional[tuple[float, float, float]] = None,
    seconds_per_year: float = SECONDS_PER_YEAR,
) -> tuple[float, float, float]:
    """Cycles per year *per stress block* in each heading group (0/45/90).

    With ``spreading`` given, the fixed spreading matrix distributes the
    sea state's cycles over the heading groups. With ``spreading=None``
    (legacy uniform mode) every heading group carries the same full count
    ``T*occurrence/(3*Tz)`` -- conservative by construction.
    """
    if tz <= 0.0:
        raise ValueError("tz must be positive")
    if occurrence <= 0.0:
        raise ValueError("occurrence must be positive")
    base = seconds_per_year * occurrence / tz / 3.0
    if spreading is None:
        return (base, base, base)
    p0, p45, p90 = spreading
    if min(p0, p45, p90) < 0.0:
        raise ValueError("spreading percentages must be non-negative")
    return tuple(
        base * (row[0] * p0 + row[1] * p45 + row[2] * p90)
        for row in FATG_SPREADING_MATRIX
    )  # type: ignore[return-value]


def fatg_sea_state_damage(
    significant_stress_range: float,
    tz: float,
    occurrence: float,
    log10_a: float,
    slope: float = 3.0,
    direction_factors: tuple[float, float, float] = FATG_DIRECTION_FACTOR_PRESETS[
        "general"
    ],
    spreading: Optional[tuple[float, float, float]] = None,
    seconds_per_year: float = SECONDS_PER_YEAR,
) -> float:
    """Miner damage per year contributed by one sea state.

    3 stress blocks x 3 heading groups; each block-heading combination has
    stress ``block_factor * direction_factor * sigma_s`` and the heading's
    per-block cycle count against ``N = 10**(log10_a - slope*log10(S))``.
    """
    if significant_stress_range <= 0.0:
        raise ValueError("significant_stress_range must be positive")
    cycles = fatg_heading_cycles(tz, occurrence, spreading, seconds_per_year)
    damage = 0.0
    for n_heading, factor in zip(cycles, direction_factors):
        for block in FATG_BLOCK_FACTORS:
            stress = significant_stress_range * block * factor
            n_fail = 10.0 ** (log10_a - slope * math.log10(stress))
            damage += n_heading / n_fail
    return damage


def fatg_annual_damage(
    omega: Sequence[float],
    stress_rao: Sequence[float],
    sea_states: Sequence[FatgSeaState],
    sn_curve: str | None = "F",
    log10_a: float | None = None,
    slope: float = 3.0,
    member_type: str = "general",
    seconds_per_year: float = SECONDS_PER_YEAR,
) -> FatgResult:
    """Full legacy FATG run: scatter table -> Miner damage per year.

    ``sn_curve`` selects a legacy constant (E/F/T, psi units); passing
    ``log10_a`` explicitly overrides it (any consistent unit system).
    ``member_type`` selects the direction-factor preset
    (general/chord/diagonal).
    """
    if log10_a is None:
        if sn_curve is None:
            raise ValueError("provide sn_curve or log10_a")
        key = str(sn_curve).strip().upper()
        if key not in FATG_SN_CURVES:
            raise ValueError(
                f"sn_curve must be one of {sorted(FATG_SN_CURVES)}; got {sn_curve!r}"
            )
        log10_a = FATG_SN_CURVES[key]
    preset = str(member_type).strip().lower()
    if preset not in FATG_DIRECTION_FACTOR_PRESETS:
        raise ValueError(
            "member_type must be one of "
            f"{sorted(FATG_DIRECTION_FACTOR_PRESETS)}; got {member_type!r}"
        )
    direction_factors = FATG_DIRECTION_FACTOR_PRESETS[preset]
    if not sea_states:
        raise ValueError("sea_states must be non-empty")

    results: list[FatgSeaStateResult] = []
    total = 0.0
    for state in sea_states:
        sigma_s = fatg_significant_stress_range(omega, stress_rao, state.hs, state.tz)
        damage = fatg_sea_state_damage(
            sigma_s,
            state.tz,
            state.occurrence,
            log10_a,
            slope,
            direction_factors,
            state.spreading,
            seconds_per_year,
        )
        total += damage
        results.append(
            FatgSeaStateResult(
                hs=state.hs,
                tz=state.tz,
                tp=1.3 * state.tz,  # legacy report echo
                occurrence=state.occurrence,
                significant_stress_range=sigma_s,
                damage_per_year=damage,
            )
        )

    if total <= 0.0:
        raise ValueError("accumulated zero damage; check the RAO and scatter inputs")
    return FatgResult(
        annual_damage=total,
        fatigue_life_years=1.0 / total,
        life_used_percent_1yr=total * 100.0,
        life_used_percent_10yr=total * 1000.0,
        sea_states=results,
    )


def _validate_rao(omega: Sequence[float], stress_rao: Sequence[float]) -> None:
    if len(omega) != len(stress_rao) or len(omega) < 2:
        raise ValueError("omega and stress_rao must be equal-length (>= 2)")
    if any(b <= a for a, b in zip(omega, omega[1:])):
        raise ValueError("omega must be strictly increasing")
    if omega[0] <= 0.0:
        raise ValueError("omega must be positive (rad/s)")
    if any(v < 0.0 for v in stress_rao):
        raise ValueError("stress_rao ordinates must be non-negative")
