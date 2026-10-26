# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Screen small trim changes caused by an internal longitudinal transfer.

Only an existing mass is moved: total mass is unchanged. MCT must be supplied
in tonne-metres per centimetre for the condition being screened; it is held
constant. Positive longitudinal displacement is forward and positive trim is
forward draft minus aft draft, matching :func:`trim_hydrostatics.end_drafts`.

Reference: EN400 course notes (Summer 2020), section 3.6, pages 3-28 and 3-32:
https://www.usna.edu/NAOE/_files/documents/Courses/EN400/EN400_Course_Notes,_Summer_2020.pdf
The reference gives change in trim as shifted-weight moment divided by the
moment to change trim by a unit length. Its aft-minus-forward trim convention
is reversed here; its imperial units are replaced by the explicit metric units.

This screen holds arithmetic mean draft fixed and splits trim equally between
the ends using ``end_drafts``. This is a midship-flotation approximation, not
a general consequence of unchanged displacement: the reference notes that
actual rotation is about the centre of flotation, which may be off midship.
No hull integration, tank capacity, free-surface correction, stability or fuel
optimization, or ballast compliance assessment is performed. Large changes
require updated hydrostatics and a loading analysis.
"""

from __future__ import annotations

import math

from digitalmodel.naval_architecture.trim_hydrostatics import end_drafts

__all__ = [
    "drafts_after_weight_shift",
    "even_keel_transfer_mass",
    "trim_change_from_weight_shift",
]

# Exact conversion: NIST, "Metric (SI) Prefixes", centi = one hundredth.
# https://www.nist.gov/pml/owm/metric-si-prefixes
_CENTIMETRES_PER_METRE = 100.0


def _require_finite(**values: float) -> None:
    for name, value in values.items():
        if not math.isfinite(value):
            raise ValueError(f"{name} must be finite")


def _validate_transfer(lever_arm_m: float, mct_t_m_per_cm: float) -> None:
    _require_finite(lever_arm_m=lever_arm_m, mct_t_m_per_cm=mct_t_m_per_cm)
    if mct_t_m_per_cm <= 0.0:
        raise ValueError("mct_t_m_per_cm must be positive")
    if lever_arm_m == 0.0:
        raise ValueError("lever_arm_m must be nonzero")


def trim_change_from_weight_shift(
    *, mass_t: float, lever_arm_m: float, mct_t_m_per_cm: float
) -> float:
    """Return signed change in trim in metres for an internal transfer.

    Moment balance about any fixed longitudinal origin gives
    ``mass_t * x_destination - mass_t * x_source = mass_t * lever_arm_m``.
    Thus, by the section 3.6 relation cited above,
    ``delta_trim_cm = mass_t * lever_arm_m / mct_t_m_per_cm``.
    Dividing by the exact 100 centimetres per metre gives ``delta_trim_m``.

    ``lever_arm_m = x_destination - x_source``, with x positive forward.
    Positive mass moves along that lever; negative mass denotes the reverse
    transfer. Reversing either sign reverses the moment. No mass is loaded or
    discharged, so this operation has no parallel sinkage term. MCT is never
    inferred. Inputs must be finite, MCT positive and the lever nonzero.
    A nonfinite intermediate calculation or result raises ``ValueError``.
    """
    _validate_transfer(lever_arm_m, mct_t_m_per_cm)
    _require_finite(mass_t=mass_t)
    change_m = mass_t * lever_arm_m / mct_t_m_per_cm / _CENTIMETRES_PER_METRE
    _require_finite(trim_change_m=change_m)
    return change_m


def even_keel_transfer_mass(
    *, initial_trim_m: float, lever_arm_m: float, mct_t_m_per_cm: float
) -> float:
    """Return signed tonnes to correct the supplied trim to even keel.

    Setting ``initial_trim_m + delta_trim_m = 0`` in the moment balance
    cited above gives
    ``mass_t = -initial_trim_m * 100 * mct_t_m_per_cm / lever_arm_m``.
    The factor 100 converts metres to centimetres, as in
    :func:`trim_change_from_weight_shift`. Zero initial trim returns zero.

    A negative answer means transfer opposite to the supplied signed lever;
    it does not request negative physical ballast. The caller must determine
    whether that direction and mass are available. No capacity is assumed.
    Inputs must be finite, MCT positive and the lever nonzero, even for zero
    initial trim. Nonfinite calculations raise ``ValueError``.
    """
    _validate_transfer(lever_arm_m, mct_t_m_per_cm)
    _require_finite(initial_trim_m=initial_trim_m)
    mass_t = (
        -initial_trim_m * _CENTIMETRES_PER_METRE * mct_t_m_per_cm / lever_arm_m
    )
    _require_finite(transfer_mass_t=mass_t)
    return mass_t


def drafts_after_weight_shift(
    *,
    mean_draft_m: float,
    initial_trim_m: float,
    mass_t: float,
    lever_arm_m: float,
    mct_t_m_per_cm: float,
) -> tuple[float, float]:
    """Return ``(forward, aft)`` drafts in metres at unchanged mean draft.

    Apply the internal-transfer moment balance in
    :func:`trim_change_from_weight_shift`, add its signed trim change, then
    delegate the end-draft geometry to :func:`end_drafts`. No displacement
    mass is added or removed. Fixed mean draft uses the midship-flotation
    approximation stated above; end-draft physical limits are not assessed.
    All inputs and results must be finite.
    """
    _require_finite(mean_draft_m=mean_draft_m, initial_trim_m=initial_trim_m)
    change_m = trim_change_from_weight_shift(
        mass_t=mass_t, lever_arm_m=lever_arm_m, mct_t_m_per_cm=mct_t_m_per_cm
    )
    final_trim_m = initial_trim_m + change_m
    _require_finite(final_trim_m=final_trim_m)
    forward_m, aft_m = end_drafts(mean_draft_m, final_trim_m)
    _require_finite(draft_fwd_m=forward_m, draft_aft_m=aft_m)
    return forward_m, aft_m
