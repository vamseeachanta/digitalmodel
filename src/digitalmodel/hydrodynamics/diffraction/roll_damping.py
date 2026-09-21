"""Convert a roll damping target between what AQWA and OrcaWave accept.

Potential-flow diffraction contains no viscous roll damping, so a roll response
is governed by a damping value the analyst supplies. The two solvers do not
accept it in the same form:

* **AQWA** accepts an absolute damping matrix on the Deck 7 ``FIDP`` record, in
  the deck's own units. It *reports* a percentage of critical damping in the
  ``.LIS``, but provides no way to specify one. The solver additionally raises
  a near-zero modal damping to a nominal 0.5% of critical in some analyses,
  which a study believing itself undamped will not see.
* **OrcaWave** accepts either an absolute ``BodyExternalDampingMatrix`` or a
  percentage-critical target via ``BodyIncreaseRollDampingToTarget``.

Matching them therefore needs the conversion below, which carries two traps.

**The critical damping is frequency dependent.** It is built from the total
inertia, structural plus added, and the added inertia varies with frequency.
The frequency at which a target is quoted must therefore be stated.
:func:`resonant_basis` pins it at roll resonance, where the relation is
implicit because the natural frequency depends on the added inertia evaluated
at that same frequency.

**The two solvers use different units for the same matrix.** An AQWA deck
declaring ``Metric: kg, m [N]`` takes N m s/rad; an OrcaWave case declaring
``UnitsSystem: SI`` is te, m, s and takes kN m s/rad. The factor of 1000 is
silent in both directions. :func:`to_orcawave_si` is the only sanctioned way to
cross it.

The formula implemented here is not asserted from documentation. It is the one
that reproduces both diagnostic columns of the AQWA ship RAO example shipped in
``docs/domains/aqwa/examples/03_dat/001_ship_raos/``, whose ``.LIS`` was
produced by ANSYS in Workbench 2022 R2:

.. code-block:: text

    zeta(w) = B(w) / ( 2 sqrt( C (I + A(w)) ) )
    T_n(w)  = 2 pi sqrt( (I + A(w)) / C )

Both hold for a mode that is hydrostatically uncoupled, which roll is for a
laterally symmetric hull. Heave and pitch are coupled and require the coupled
eigenproblem instead; this module does not cover them.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Iterable, Sequence

__all__ = [
    "ORCAWAVE_SI_PER_NEWTON_METRE_SECOND",
    "CriticalDampingBasis",
    "RollDampingError",
    "critical_damping",
    "damping_to_percent_critical",
    "fidp_row_card",
    "percent_critical_to_damping",
    "representable_value",
    "resonant_basis",
    "to_orcawave_si",
]

#: An OrcaWave case declaring ``UnitsSystem: SI`` is te, m, s, so its force unit
#: is the kilonewton. A rotational damping of 1 N m s/rad is 1e-3 in that case.
ORCAWAVE_SI_PER_NEWTON_METRE_SECOND = 1.0e-03

#: Number of decimal digits AQWA prints for a percentage critical damping.
_MODE_COUNT = 6


class RollDampingError(ValueError):
    """A damping basis or target that is not physical, or a malformed card."""


def _check_basis(stiffness: float, inertia: float, added_inertia: float) -> float:
    """Validate a basis and return the total inertia.

    The added inertia may legitimately be negative near an irregular frequency,
    so it is not bounded on its own; what must be positive is the total.
    """
    if not math.isfinite(stiffness) or stiffness <= 0.0:
        raise RollDampingError(
            f"restoring stiffness must be finite and positive, got {stiffness!r}"
        )
    if not math.isfinite(inertia) or inertia <= 0.0:
        raise RollDampingError(
            f"structural inertia must be finite and positive, got {inertia!r}"
        )
    if not math.isfinite(added_inertia):
        raise RollDampingError(
            f"added inertia must be finite, got {added_inertia!r}"
        )
    total = inertia + added_inertia
    if total <= 0.0:
        raise RollDampingError(
            "structural plus added inertia must be positive, got "
            f"{inertia!r} + {added_inertia!r} = {total!r}"
        )
    return total


def critical_damping(
    stiffness: float, inertia: float, added_inertia: float
) -> float:
    """Critical damping ``2 sqrt(C (I + A))`` for an uncoupled mode.

    Units follow the inputs: with ``stiffness`` in N m/rad and the inertias in
    kg m2 the result is N m s/rad.
    """
    total = _check_basis(stiffness, inertia, added_inertia)
    return 2.0 * math.sqrt(stiffness * total)


def percent_critical_to_damping(
    percent: float, stiffness: float, inertia: float, added_inertia: float
) -> float:
    """Absolute damping corresponding to ``percent`` of critical.

    This is the value to place in an AQWA ``FIDP`` record, in the deck's units.
    """
    if not math.isfinite(percent) or percent < 0.0:
        raise RollDampingError(
            f"target percentage must be finite and non-negative, got {percent!r}"
        )
    return 0.01 * percent * critical_damping(stiffness, inertia, added_inertia)


def damping_to_percent_critical(
    damping: float, stiffness: float, inertia: float, added_inertia: float
) -> float:
    """Percentage of critical corresponding to an absolute ``damping``.

    Reproduces the ``.LIS`` column "APPROXIMATE PERCENTAGE CRITICAL DAMPING" for
    an uncoupled mode.
    """
    if not math.isfinite(damping) or damping < 0.0:
        raise RollDampingError(
            f"damping must be finite and non-negative, got {damping!r}"
        )
    return 100.0 * damping / critical_damping(stiffness, inertia, added_inertia)


def to_orcawave_si(damping_newton_metre_second: float) -> float:
    """Convert a rotational damping from N m s/rad to an OrcaWave SI case.

    An OrcaWave case declaring ``UnitsSystem: SI`` works in te, m, s, so its
    ``BodyExternalDampingMatrix`` entries are kN m s/rad.
    """
    if not math.isfinite(damping_newton_metre_second):
        raise RollDampingError(
            f"damping must be finite, got {damping_newton_metre_second!r}"
        )
    return damping_newton_metre_second * ORCAWAVE_SI_PER_NEWTON_METRE_SECOND


@dataclass(frozen=True)
class CriticalDampingBasis:
    """The three quantities a critical damping is built from, for one mode.

    Attributes
    ----------
    stiffness:
        Restoring stiffness, N m/rad for a rotational mode.
    inertia:
        Structural inertia about the reference point, kg m2.
    added_inertia:
        Added inertia at the frequency the basis is pinned at, kg m2.
    extrapolated:
        True when ``added_inertia`` was held at a table endpoint rather than
        interpolated, which happens when resonance falls outside the solved
        frequency range. The basis remains usable; the caller is told that the
        added inertia at resonance was not computed by the solver.
    """

    stiffness: float
    inertia: float
    added_inertia: float
    extrapolated: bool = False

    def __post_init__(self) -> None:
        _check_basis(self.stiffness, self.inertia, self.added_inertia)

    @property
    def total_inertia(self) -> float:
        return self.inertia + self.added_inertia

    @property
    def critical_damping(self) -> float:
        return critical_damping(self.stiffness, self.inertia, self.added_inertia)

    @property
    def natural_frequency(self) -> float:
        """Undamped natural frequency, rad/s."""
        return math.sqrt(self.stiffness / self.total_inertia)

    @property
    def natural_period(self) -> float:
        """Undamped natural period, seconds."""
        return 2.0 * math.pi / self.natural_frequency

    def damping_for(self, percent: float) -> float:
        """Absolute damping for a percentage-critical target on this basis."""
        return percent_critical_to_damping(
            percent, self.stiffness, self.inertia, self.added_inertia
        )

    def percent_for(self, damping: float) -> float:
        """Percentage critical for an absolute damping on this basis."""
        return damping_to_percent_critical(
            damping, self.stiffness, self.inertia, self.added_inertia
        )


def _validate_table(
    table: Iterable[tuple[float, float]],
) -> list[tuple[float, float]]:
    points = [(float(w), float(a)) for w, a in table]
    if len(points) < 2:
        raise RollDampingError(
            f"an added-inertia table needs at least two points, got {len(points)}"
        )
    for (w0, _), (w1, _) in zip(points, points[1:]):
        if not w1 > w0:
            raise RollDampingError(
                "the added-inertia table must be sorted by strictly increasing "
                f"frequency; {w1!r} does not follow {w0!r}"
            )
    if any(not math.isfinite(w) or w <= 0.0 for w, _ in points):
        raise RollDampingError("frequencies must be finite and positive")
    if any(not math.isfinite(a) for _, a in points):
        raise RollDampingError("added inertias must be finite")
    return points


def _interpolate(points: Sequence[tuple[float, float]], omega: float) -> tuple[
    float, bool
]:
    """Linear interpolation in frequency, held flat outside the table."""
    if omega <= points[0][0]:
        return points[0][1], omega < points[0][0]
    if omega >= points[-1][0]:
        return points[-1][1], omega > points[-1][0]
    for (w0, a0), (w1, a1) in zip(points, points[1:]):
        if w0 <= omega <= w1:
            span = w1 - w0
            return a0 + (a1 - a0) * (omega - w0) / span, False
    raise RollDampingError(f"frequency {omega!r} could not be located in the table")


def resonant_basis(
    stiffness: float,
    inertia: float,
    added_inertia_table: Iterable[tuple[float, float]],
    tolerance: float = 1.0e-10,
    max_iterations: int = 200,
) -> CriticalDampingBasis:
    """Pin a damping basis at the mode's own natural frequency.

    The natural frequency satisfies ``w**2 = C / (I + A(w))`` with ``A``
    evaluated at that same frequency, so it is found by fixed-point iteration
    on the interpolated table. Outside the tabulated range the added inertia is
    held at the nearest endpoint rather than extrapolated, and the returned
    basis records that with ``extrapolated=True``.

    Parameters
    ----------
    added_inertia_table:
        ``(frequency, added inertia)`` pairs, sorted by strictly increasing
        frequency.
    """
    points = _validate_table(added_inertia_table)
    if not math.isfinite(stiffness) or stiffness <= 0.0:
        raise RollDampingError(
            f"restoring stiffness must be finite and positive, got {stiffness!r}"
        )
    if not math.isfinite(inertia) or inertia <= 0.0:
        raise RollDampingError(
            f"structural inertia must be finite and positive, got {inertia!r}"
        )

    added, extrapolated = points[0][1], False
    omega = math.sqrt(stiffness / _check_basis(stiffness, inertia, added))
    for _ in range(max_iterations):
        added, extrapolated = _interpolate(points, omega)
        total = _check_basis(stiffness, inertia, added)
        nxt = math.sqrt(stiffness / total)
        if abs(nxt - omega) <= tolerance * max(1.0, abs(omega)):
            omega = nxt
            break
        omega = nxt
    else:
        raise RollDampingError(
            "the natural frequency did not converge in "
            f"{max_iterations} iterations; last estimate {omega!r} rad/s"
        )
    added, extrapolated = _interpolate(points, omega)
    return CriticalDampingBasis(stiffness, inertia, added, extrapolated)


#: Format of one value field in a ``FIDP`` record. Ten columns is the narrowest
#: width that keeps a negative value separated from the field before it, and it
#: leaves room for only three decimals, so a card carries four significant
#: figures.
_FIDP_FIELD = "10.3e"


def representable_value(value: float) -> float:
    """The value AQWA reads back from a card written by :func:`fidp_row_card`.

    A ``FIDP`` field holds four significant figures. A damping target specified
    more precisely than that is silently rounded when it reaches the deck, so a
    comparison against another solver given the unrounded number is
    contaminated at the rounding level. Both solvers are given this value.
    """
    if not math.isfinite(value):
        raise RollDampingError(f"value must be finite, got {value!r}")
    return float(format(value, _FIDP_FIELD))


def fidp_row_card(mode: int, values: Sequence[float]) -> str:
    """One AQWA Deck 7 ``FIDP`` record, as a fixed-column 80-character line.

    ``mode`` is the one-based row index, 1 to 6 for surge through yaw, and
    ``values`` are that row's six entries in the deck's own units. The layout
    matches the records AQWA itself writes: six characters of leading space,
    the four-character keyword, the row index right-justified in ten columns,
    then six values in ten columns each. Ten is the narrowest width that still
    separates a negative value from the field before it, which a real deck
    relies on -- ``-9.332e+10`` fills the field exactly.
    """
    if not isinstance(mode, int) or isinstance(mode, bool):
        raise RollDampingError(f"mode must be an integer, got {mode!r}")
    if not 1 <= mode <= _MODE_COUNT:
        raise RollDampingError(f"mode must be 1 to {_MODE_COUNT}, got {mode}")
    row = list(values)
    if len(row) != _MODE_COUNT:
        raise RollDampingError(
            f"a FIDP row needs {_MODE_COUNT} values, got {len(row)}"
        )
    for value in row:
        if not math.isfinite(value):
            raise RollDampingError(f"FIDP values must be finite, got {value!r}")

    cells = "".join(format(value, _FIDP_FIELD) for value in row)
    card = f"      FIDP{mode:>10d}{cells}"
    if len(card) > 80:
        raise RollDampingError(
            f"FIDP record exceeds the 80-column deck width: {len(card)} characters"
        )
    return card.ljust(80)
