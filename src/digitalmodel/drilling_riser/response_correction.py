"""Riser flex-joint response correction (twin B #1374, epic #1372).

Applies the transparent residual spine (:mod:`digitalmodel.residual`) to the
physics-predicted flex-joint angle so the twin TRACKS the actual asset, while
keeping the operability verdict standards-traceable and conservative:

  * the residual is fit on angle MAGNITUDE per joint (``|measured_mean|`` vs
    ``|predicted_static|``) — sidesteps the inclinometer sign convention and mirrors
    the envelope's ``max(abs(...))`` collapse; it corrects the STATIC angle only, so
    the envelope's separate RAO ``dyn_angle`` term is not double-counted;
  * the DECISION angle is ``max(corrected, raw_physics)`` — the correction may only
    ever TIGHTEN a go/no-go, never lower a utilisation or flip a NO-GO to GO. The
    corrected value is surfaced SEPARATELY for tracking;
  * the flex-joint LIMITS stay the cited getters (:class:`EnvelopeCriteria`,
    unchanged); the corrected value enters only the utilisation NUMERATOR;
  * an ESCALATE status (insufficient / out-of-bounds / out-of-domain fit) flags the
    channel so a downstream verdict defers to the exact physics — never a silent GO;
  * only the flex-joint ANGLE is corrected; the von-Mises (stress) channel is LEFT
    UNCORRECTED and marked so — the twin does not claim full fidelity when stress
    governs (measured moment/strain telemetry is a later slice).
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional

from digitalmodel.drilling_riser.envelope import EnvelopeCriteria
from digitalmodel.drilling_riser.riser_response import RiserResponse
from digitalmodel.residual.model import ACTIVE, ResidualModel

#: Physical clip on a corrected flex-joint angle magnitude [deg].
_ANGLE_HI_DEG = 30.0

VON_MISES_COVERAGE = "uncorrected (physics-only)"


@dataclass(frozen=True)
class FlexjointModels:
    """A fitted per-joint pair of residual models (upper, lower)."""

    upper: ResidualModel
    lower: ResidualModel


@dataclass(frozen=True)
class CorrectedFlexjoint:
    """Physics + tracking estimate + the CONSERVATIVE decision angle.

    ``decision_static_angle_deg = max(corrected, raw)`` is what an operability
    verdict must use; ``corrected_static_angle_deg`` is for tracking only.
    """

    raw_static_angle_deg: float
    corrected_static_angle_deg: float
    decision_static_angle_deg: float
    escalated: bool
    status: str
    provenance: dict


def fit_flexjoint_models(
    *,
    measured_upper_deg,
    predicted_upper_deg,
    measured_lower_deg,
    predicted_lower_deg,
) -> FlexjointModels:
    """Fit per-joint magnitude residual models from (measured, predicted) pairs.

    Inputs are angle MAGNITUDES [deg] (``abs`` is applied defensively). Each joint
    fails closed to an escalate/identity model when its pairs are insufficient or
    the fit is out of bounds.
    """
    def _mag(seq):
        return [abs(float(v)) for v in seq]

    return FlexjointModels(
        upper=ResidualModel.fit(_mag(measured_upper_deg), _mag(predicted_upper_deg), lo=0.0, hi=_ANGLE_HI_DEG),
        lower=ResidualModel.fit(_mag(measured_lower_deg), _mag(predicted_lower_deg), lo=0.0, hi=_ANGLE_HI_DEG),
    )


def correct_flexjoint_response(
    response: RiserResponse, models: FlexjointModels
) -> CorrectedFlexjoint:
    """Correct a physics ``RiserResponse``'s flex-joint angle, conservatively.

    Diffs per-joint magnitude, takes the corrected tracking estimate as the max
    over joints, and returns ``decision = max(corrected, raw)`` so a verdict can
    never be relaxed by the correction.
    """
    raw_upper = abs(response.angle_upper_deg)
    raw_lower = abs(response.angle_lower_deg)
    c_upper = models.upper.apply(raw_upper)
    c_lower = models.lower.apply(raw_lower)

    raw_static = max(raw_upper, raw_lower)
    corrected_static = max(c_upper.value, c_lower.value)
    decision_static = max(corrected_static, raw_static)  # CONSERVATIVE-ONLY
    escalated = c_upper.escalated or c_lower.escalated

    provenance = {
        "flexjoint_angle": {
            "upper": models.upper.provenance(),
            "lower": models.lower.provenance(),
            "raw_static_deg": raw_static,
            "corrected_static_deg": corrected_static,
            "decision_static_deg": decision_static,
            "decision_policy": "max(corrected, raw) — correction can only tighten",
        },
        "von_mises": VON_MISES_COVERAGE,
        "escalated": escalated,
    }
    status = ACTIVE if not escalated else "escalate"
    return CorrectedFlexjoint(
        raw_static_angle_deg=raw_static,
        corrected_static_angle_deg=corrected_static,
        decision_static_angle_deg=decision_static,
        escalated=escalated,
        status=status,
        provenance=provenance,
    )


def flexjoint_utilisation(
    decision_static_angle_deg: float,
    criteria: EnvelopeCriteria,
    *,
    dyn_angle_deg: float = 0.0,
    fj_max_limit_deg: Optional[float] = None,
) -> float:
    """Flex-joint utilisation from a (decision) static angle vs the cited limits.

    Mirrors ``envelope.compute_operating_envelope``'s flex-joint check exactly: the
    corrected/decision angle is the NUMERATOR; the denominators are the cited-getter
    limits on ``criteria`` (never touched by the correction).
    """
    mean_limit = criteria.flexjoint_angle_mean_deg
    fj_max = fj_max_limit_deg if fj_max_limit_deg is not None else criteria.flexjoint_angle_max_deg
    total_angle = decision_static_angle_deg + dyn_angle_deg
    return max(decision_static_angle_deg / mean_limit, total_angle / fj_max)
