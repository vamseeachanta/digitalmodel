"""Select smooth-wall nut treatment from supplied patch-face y+ evidence.

This is a range check, not validation of a turbulence closure or a force.
A target y+ is never evidence of the y+ a mesh actually achieved.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any

# Notes on Computational Fluid Dynamics: General Principles (2022), sec. 7.5,
# typical applicability of the log law: 30 < y+ < 500 (strict inequalities).
# https://doc.cfd.direct/notes/cfd-general-principles/wall-functions
LOG_LAW_MIN = 30.0
LOG_LAW_MAX = 500.0
RANGE_SOURCE = (
    "Notes on Computational Fluid Dynamics: General Principles (2022), sec. 7.5; "
    "https://doc.cfd.direct/notes/cfd-general-principles/wall-functions"
)
# OpenFOAM v2312, nutUSpaldingWallFunction, Description: continuous nut to wall.
SPALDING_SOURCE = (
    "https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/"
    "derived/wall/nutUSpaldingWallFunction/"
)
SUPPORTED_FUNCTIONS = ("auto", "nutkWallFunction", "nutUSpaldingWallFunction")


@dataclass(frozen=True)
class YPlusEvidence:
    """One complete wall-patch-set snapshot, with a supplied source label.

    Supply wall-adjacent cell-centre y+ for EVERY face of the assessed patches.
    Optional face areas must be from the same mesh and in the same ordering.
    Without areas the mean and fractions are explicitly face-count weighted.
    The caller owns the association to a mesh, condition, patch and time; the
    label should identify those. A log containing only extrema and a mean
    cannot supply a fraction and must not be expanded into invented samples.
    """

    values: tuple[float, ...]
    source: str
    face_areas: tuple[float, ...] | None = None

    def __post_init__(self) -> None:
        values = tuple(float(v) for v in self.values)
        if not values or any(not math.isfinite(v) or v < 0 for v in values):
            raise ValueError("y+ samples must be nonempty, finite and nonnegative")
        if not isinstance(self.source, str) or not self.source.strip():
            raise ValueError("y+ evidence requires a source label")
        object.__setattr__(self, "values", values)
        if self.face_areas is not None:
            areas = tuple(float(a) for a in self.face_areas)
            if len(areas) != len(values) or any(
                not math.isfinite(a) or a <= 0 for a in areas
            ):
                raise ValueError("face areas must be positive, finite and match samples")
            object.__setattr__(self, "face_areas", areas)

    @property
    def in_log_layer(self) -> bool:
        """Conservative policy: every supplied face must satisfy the range."""
        return min(self.values) > LOG_LAW_MIN and max(self.values) < LOG_LAW_MAX

    def summary(self) -> dict[str, Any]:
        """Weighted mean=sum(w*y+)/sum(w); fraction=sum(w below bound)/sum(w).

        These are definitions of the reported statistics, not empirical
        correlations. Normalize weights first to avoid overflow of area sums.
        """
        areas = self.face_areas or (1.0,) * len(self.values)
        scale = max(areas)
        weights = tuple(a / scale for a in areas)
        total = math.fsum(weights)
        normalized = tuple(w / total for w in weights)
        return {
            "source": self.source,
            "face_count": len(self.values),
            "weighting": "face_area" if self.face_areas is not None else "face_count",
            "min": min(self.values),
            "max": max(self.values),
            "mean": math.fsum(v * w for v, w in zip(self.values, normalized)),
            "fraction_below_30": math.fsum(
                w for v, w in zip(self.values, normalized) if v < LOG_LAW_MIN
            ),
            "all_in_log_layer": self.in_log_layer,
        }


def assess_wall_treatment(
    *,
    requested: str = "auto",
    achieved: YPlusEvidence | None = None,
    predicted: YPlusEvidence | None = None,
) -> dict[str, Any]:
    """Select using achieved evidence; separately assess a supplied prediction.

    Auto uses nutk only when every achieved sample is inside the cited range.
    With no achieved evidence, Spalding is a continuous-profile fallback and
    the achieved check remains NOT_EVALUATED. Explicit choices are retained,
    with a FAIL and warning when nutk contradicts the supplied distribution.
    PASS means this range check did not reject the choice; it is not proof
    of resolved flow or suitability for form-factor extraction.
    """
    if requested not in SUPPORTED_FUNCTIONS:
        raise ValueError(f"unsupported nut wall function: {requested!r}")
    selected = requested
    if requested == "auto":
        selected = (
            "nutkWallFunction"
            if achieved is not None and achieved.in_log_layer
            else "nutUSpaldingWallFunction"
        )
    warnings = []

    def check(evidence: YPlusEvidence | None, label: str) -> str:
        if evidence is None:
            return "NOT_EVALUATED"
        if selected == "nutkWallFunction" and not evidence.in_log_layer:
            warnings.append(
                f"{label} y+ lies outside the typical log-law range; "
                "nutkWallFunction is inconsistent with this evidence."
            )
            return "FAIL"
        return "PASS"

    achieved_check = check(achieved, "Achieved")
    predicted_check = check(predicted, "Predicted")
    return {
        "requested": requested,
        "selected": selected,
        "selection_basis": (
            "explicit" if requested != "auto"
            else "achieved" if achieved is not None else "fallback"
        ),
        "achieved_check": achieved_check,
        "predicted_check": predicted_check,
        "achieved_y_plus": achieved.summary() if achieved is not None else None,
        "predicted_y_plus": predicted.summary() if predicted is not None else None,
        "log_law_range": {"min_exclusive": LOG_LAW_MIN, "max_exclusive": LOG_LAW_MAX},
        "sources": [RANGE_SOURCE, SPALDING_SOURCE],
        "warnings": warnings,
        "limits": (
            "Caller-supplied samples must cover the assessed patch and match "
            "the mesh and condition. A predicted distribution or target is "
            "not achieved evidence. Spalding does not establish wall-resolved "
            "flow or qualify a form factor. No solver result is changed."
        ),
    }
