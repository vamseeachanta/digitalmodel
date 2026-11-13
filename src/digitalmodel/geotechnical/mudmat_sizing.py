# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Finite-grid screening using the existing undrained mudmat calculator.

Units are m, kPa, kN and kN/m3. No engineering factor is selected internally.
The scope excludes embedment, moments, inclination and drained conditions.
"""

from collections.abc import Sequence
from dataclasses import dataclass
from math import isfinite
from numbers import Real

from .mudmat import mudmat_bearing_capacity


@dataclass(frozen=True)
class CandidateEvaluation:
    """Immutable capacity snapshot and the two supplied-demand comparisons."""

    width_b_m: float
    length_l_m: float
    standard: str
    q_ult_kpa: float
    vertical_capacity_kn: float
    sliding_capacity_kn: float
    factor_of_safety: float
    allowable_vertical_kn: float
    allowable_sliding_kn: float
    bearing_pass: bool
    sliding_pass: bool


@dataclass(frozen=True)
class MudmatSizingResult:
    """Detached inputs, full ordered trace and first passing grid member."""

    width_candidates_m: tuple[float, ...]
    aspect_ratio: float
    vertical_load_kn: float
    horizontal_load_kn: float
    undrained_shear_strength_kpa: float
    submerged_unit_weight_kn_m3: float
    factor_of_safety: float
    evaluations: tuple[CandidateEvaluation, ...]
    selected: CandidateEvaluation | None
    scope: str = "centred-unembedded-undrained screening"

    @property
    def status(self) -> str:
        """Distinguish an empty feasible set from a selected candidate."""
        return "NO_CANDIDATE" if self.selected is None else "SELECTED"


def _number(name: str, value: float, lower: float, strict: bool) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite number")
    try:
        number = float(value)
    except (OverflowError, ValueError) as error:
        raise ValueError(f"{name} must be finite") from error
    if not isfinite(number) or number < lower or (strict and number == lower):
        raise ValueError(f"{name} must be finite and {'>' if strict else '>='} {lower}")
    return number


def _evaluate(width: float, length: float, vertical: float, horizontal: float,
              strength: float, unit_weight: float, factor: float) -> CandidateEvaluation:
    capacity = mudmat_bearing_capacity(
        width_b_m=width, length_l_m=length, embedment_depth_m=0.0,
        condition="undrained", submerged_unit_weight_kn_m3=unit_weight,
        vertical_load_kn=vertical, moment_knm=0.0,
        undrained_shear_strength_kpa=strength,
    )
    allowable_vertical = capacity.vertical_capacity_kn / factor
    allowable_sliding = capacity.sliding_capacity_kn / factor
    if not all(isfinite(value) and value >= 0 for value in (
        capacity.q_ult_kpa, capacity.vertical_capacity_kn,
        capacity.sliding_capacity_kn, allowable_vertical, allowable_sliding,
    )):
        raise ValueError("calculated capacities must be finite and nonnegative")
    return CandidateEvaluation(
        width, length, capacity.standard, capacity.q_ult_kpa,
        capacity.vertical_capacity_kn, capacity.sliding_capacity_kn, factor,
        allowable_vertical, allowable_sliding,
        allowable_vertical >= vertical, allowable_sliding >= horizontal,
    )


def size_mudmat(
    *, width_candidates_m: Sequence[float], aspect_ratio: float,
    vertical_load_kn: float, horizontal_load_kn: float,
    undrained_shear_strength_kpa: float, submerged_unit_weight_kn_m3: float,
    factor_of_safety: float,
) -> MudmatSizingResult:
    """Select the smallest passing supplied width; evaluate the entire grid.

    Widths shall be a nonempty strictly increasing sequence of positive finite
    numbers. Length equals aspect_ratio * width, with aspect_ratio >= 1.
    Vertical demand, shear strength and factor shall be positive; horizontal
    demand and submerged unit weight may be zero. Booleans are rejected.
    Invalid inputs or nonfinite arithmetic raise ValueError. Unsupported
    condition, embedment or moment keywords raise TypeError.

    Both ultimate capacities are divided by the supplied factor, matching the
    existing workflow. Equality meets a demand. Selection establishes this
    screen only, with no interpolation, optimum claim or full design acceptance.
    """
    aspect = _number("aspect_ratio", aspect_ratio, 1, False)
    vertical = _number("vertical_load_kn", vertical_load_kn, 0, True)
    horizontal = _number("horizontal_load_kn", horizontal_load_kn, 0, False)
    strength = _number("undrained_shear_strength_kpa", undrained_shear_strength_kpa, 0, True)
    weight = _number("submerged_unit_weight_kn_m3", submerged_unit_weight_kn_m3, 0, False)
    factor = _number("factor_of_safety", factor_of_safety, 0, True)
    if isinstance(width_candidates_m, (str, bytes)) or not isinstance(
        width_candidates_m, Sequence
    ):
        raise ValueError("width_candidates_m must be a finite sequence")
    widths = tuple(_number("width_candidates_m", w, 0, True) for w in width_candidates_m)
    if not widths or any(b <= a for a, b in zip(widths, widths[1:])):
        raise ValueError("width_candidates_m must be nonempty and strictly increasing")
    lengths = tuple(aspect * width for width in widths)
    if not all(isfinite(length) and isfinite(width * length) and width * length > 0
               for width, length in zip(widths, lengths)):
        raise ValueError("candidate dimensions and areas must be positive and finite")
    evaluations = tuple(
        _evaluate(width, length, vertical, horizontal, strength, weight, factor)
        for width, length in zip(widths, lengths)
    )
    selected = next((row for row in evaluations if row.bearing_pass and row.sliding_pass),
                    None)
    return MudmatSizingResult(widths, aspect, vertical, horizontal, strength, weight,
                             factor, evaluations, selected)
