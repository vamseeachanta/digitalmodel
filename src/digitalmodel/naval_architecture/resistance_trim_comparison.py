"""Resistance-over-trim comparison arithmetic for fixed-speed trim studies.

Inputs: nonnegative resistance magnitudes in kN, speed in m/s, signed trim in
metres (forward draft minus aft draft; negative by the stern, matching
trim_hydrostatics.end_drafts). All compared rows require the same speed
and hull/force basis. Draft/loading and method must also match; those metadata
are caller responsibilities. Negative trim is valid; angles require conversion.

Method: change = value/reference - 1; difference = value - reference;
split residual = total - viscous - pressure; effective power = resistance*speed
(kN*m/s = kW). Changes are fractions; multiply by 100 for percentage changes.
No hull multiplier, propulsive efficiency, interpolation or resistance prediction
is applied. Effective power inherits the supplied force basis, not shaft power.
The split check uses only the caller's absolute tolerance, including its boundary.

Limits: arithmetic agreement does not establish CFD convergence, uncertainty,
scale effects or design suitability. Reference quantities must be nonzero for
ratios; a zero-speed reference cannot support a power ratio. Input order is kept.

Input gaps: the upstream friction-line equation, coefficients and precise method
reference are not established, so no friction line or form factor is reproduced.
Callers must supply qualified forces, speed, common hull basis, trim convention
conversion if needed, and a justified split tolerance.
No empirical constants are introduced here.
"""

from dataclasses import dataclass
from math import isfinite
from numbers import Real
from typing import Iterable

from digitalmodel.naval_architecture.holtrop_mennen import effective_power


def _finite(value: float) -> float:
    """Require a finite real scalar; booleans and numeric strings are rejected."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError("A finite numeric scalar is required")
    try:
        result = float(value)
    except OverflowError as error:
        raise ValueError("A finite numeric scalar is required") from error
    if not isfinite(result):
        raise ValueError("A finite numeric scalar is required")
    return result


def _nonnegative(value: float) -> float:
    result = _finite(value)
    if result < 0:
        raise ValueError("A nonnegative magnitude is required")
    return result


@dataclass(frozen=True)
class RelativeChangeInput:
    """Values share the explicit quantity_unit; signed scalar values are allowed."""

    value: float
    reference: float
    quantity_unit: str

    def __post_init__(self) -> None:
        _finite(self.value)
        _finite(self.reference)
        if not isinstance(self.quantity_unit, str) or not self.quantity_unit:
            raise ValueError("Comparison unit must be a non-empty string for both values")


@dataclass(frozen=True)
class ResistanceComponents:
    """Nonnegative force magnitudes in kN, all on the same hull basis."""

    total_kn: float
    viscous_kn: float
    pressure_kn: float

    def __post_init__(self) -> None:
        for value in (self.total_kn, self.viscous_kn, self.pressure_kn):
            _nonnegative(value)


@dataclass(frozen=True)
class TrimCondition:
    """Signed trim_m = draft_fwd - draft_aft; negative is by the stern.

    This matches trim_hydrostatics.end_drafts; speed_m_s is a magnitude.
    """

    trim_m: float
    speed_m_s: float
    resistance: ResistanceComponents

    def __post_init__(self) -> None:
        _finite(self.trim_m)
        _nonnegative(self.speed_m_s)
        if not isinstance(self.resistance, ResistanceComponents):
            raise ValueError("resistance must contain ResistanceComponents")


@dataclass(frozen=True)
class TrimComparison:
    """Absolute and fractional differences from the selected reference row."""

    condition: TrimCondition
    effective_power_kw: float
    total_delta_kn: float
    viscous_delta_kn: float
    pressure_delta_kn: float
    power_delta_kw: float
    total_change_fraction: float
    viscous_change_fraction: float
    pressure_change_fraction: float
    power_change_fraction: float


def relative_change(inputs: RelativeChangeInput) -> float:
    """Return value/reference - 1 (dimensionless); zero reference raises ValueError.

    Same-unit scalar arithmetic only; positive changes indicate an increase for
    positive references. Signed inputs retain the algebraic reference sign.
    """
    reference = _finite(inputs.reference)
    if reference == 0:
        raise ValueError("Relative change requires a nonzero reference")
    return _finite(_finite(inputs.value) / reference - 1)


def component_split_consistent(
    components: ResistanceComponents, *, absolute_tolerance_kn: float,
) -> bool:
    """Check abs(total - (viscous + pressure)) <= absolute_tolerance_kn.

    No implicit relative tolerance or physical acceptance criterion is applied.
    A finite nonnegative tolerance is required, and a nonfinite sum is rejected.
    """
    tolerance = _nonnegative(absolute_tolerance_kn)
    component_sum = _finite(_finite(components.viscous_kn)
                            + _finite(components.pressure_kn))
    residual = _finite(components.total_kn - component_sum)
    return abs(residual) <= tolerance


def effective_power_kw(resistance_kn: float, speed_m_s: float) -> float:
    """Return P_E = resistance_kn*speed_m_s in kW; zero magnitudes are valid.

    Reuse the existing SI power product with kN-to-N and W-to-kW conversions.
    Negative/nonfinite inputs and nonfinite intermediate/results are rejected.
    This is effective power on the supplied hull basis; no efficiency is assumed.
    """
    force_n = _finite(_nonnegative(resistance_kn) * 1000)
    speed = _nonnegative(speed_m_s)
    return _finite(_finite(effective_power(force_n, speed)) / 1000)


def _compare(condition: TrimCondition, reference: TrimCondition) -> TrimComparison:
    """Build one comparison; force and power ratios use their own references."""
    force, baseline = condition.resistance, reference.resistance
    power = effective_power_kw(force.total_kn, condition.speed_m_s)
    reference_power = effective_power_kw(baseline.total_kn, reference.speed_m_s)
    changes = tuple(relative_change(RelativeChangeInput(value, ref, "kN"))
                    for value, ref in [(force.total_kn, baseline.total_kn),
                                       (force.viscous_kn, baseline.viscous_kn),
                                       (force.pressure_kn, baseline.pressure_kn)])
    return TrimComparison(
        condition=condition, effective_power_kw=power,
        total_delta_kn=_finite(force.total_kn - baseline.total_kn),
        viscous_delta_kn=_finite(force.viscous_kn - baseline.viscous_kn),
        pressure_delta_kn=_finite(force.pressure_kn - baseline.pressure_kn),
        power_delta_kw=_finite(power - reference_power),
        total_change_fraction=changes[0], viscous_change_fraction=changes[1],
        pressure_change_fraction=changes[2],
        power_change_fraction=relative_change(
            RelativeChangeInput(power, reference_power, "kW")),
    )


def tabulate_trim(
    conditions: Iterable[TrimCondition], *, absolute_tolerance_kn: float,
    reference_index: int = 0,
) -> tuple[TrimComparison, ...]:
    """Compare fixed-speed rows to an indexed reference, preserving input order.

    Speeds must be identical values. Every row must satisfy the split tolerance.
    Empty input, invalid index, differing speeds, zero reference quantities or
    nonfinite results raise ValueError. A single row yields zero differences if
    its references are nonzero.
    Trim labels may repeat; no sorting, interpolation or optimum is inferred.
    """
    tolerance = _nonnegative(absolute_tolerance_kn)
    rows = tuple(conditions)
    if not rows:
        raise ValueError("At least one trim condition is required")
    if type(reference_index) is not int or not 0 <= reference_index < len(rows):
        raise ValueError("reference_index must identify an existing row")
    if any(not isinstance(row, TrimCondition) for row in rows):
        raise ValueError("Every row must be a TrimCondition")
    reference = rows[reference_index]
    for row in rows:
        if row.speed_m_s != reference.speed_m_s:
            raise ValueError("Trim comparisons require the same speed_m_s")
        if not component_split_consistent(row.resistance,
                                         absolute_tolerance_kn=tolerance):
            raise ValueError("Component split exceeds absolute_tolerance_kn")
    return tuple(_compare(row, reference) for row in rows)
