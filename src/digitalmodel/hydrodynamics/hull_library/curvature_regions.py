"""Area-weighted longitudinal diagnostics for supplied Gaussian curvature.

Source: Geometry-Based Metrics for Early-Stage Hull-Form Producibility Screening
(2026), arXiv:2609.27544v1, equations (3), (4), (39):
https://arxiv.org/html/2609.27544v1
Companion documentation, revision 07f23578411b26c12a28587cfe2e4f56c2190034:
https://github.com/cnr-inm-mao/hullprod/blob/07f23578411b26c12a28587cfe2e4f56c2190034/docs/metrics.md

This is a quadrature diagnostic, not a curvature estimator or acceptance gate.
The caller supplies vertex areas from the same representation as the curvature;
substituting other area weights need not reproduce HullProd's global signature.
"""

from dataclasses import dataclass

import numpy as np
from numpy.typing import ArrayLike


@dataclass(frozen=True)
class CurvatureRegionContribution:
    """Area has squared input-length units; intensity and share are dimensionless."""

    status: str
    area: float
    valid_area: float
    I_D_contribution: float | None
    intensity_share: float | None


@dataclass(frozen=True)
class CurvatureRegionSummary:
    """Contributions use the global valid-area denominator, so they are additive."""

    status: str
    lref: float
    end_fraction: float
    lower_cut: float
    upper_cut: float
    total_area: float
    valid_area: float
    I_D: float | None
    regions: dict[str, CurvatureRegionContribution]


def _validated_inputs(coordinates, curvature, area, valid, lref, end_fraction):
    x, k, a = (np.asarray(value, dtype=float) for value in (coordinates, curvature, area))
    mask = np.asarray(valid)
    if x.ndim != 1 or not x.size or any(v.shape != x.shape for v in (k, a, mask)):
        raise ValueError("coordinates, curvature, area and valid must have matching 1-D shape")
    if mask.dtype != np.dtype(bool):
        raise ValueError("valid must contain boolean flags")
    if not np.isfinite(x).all() or not np.isfinite(a).all():
        raise ValueError("coordinates and vertex_area must be finite")
    if np.any(a < 0):
        raise ValueError("vertex_area must be nonnegative")
    if not np.isfinite(k[mask]).all():
        raise ValueError("valid curvature must be finite; mask unavailable samples explicitly")
    if not np.isfinite(lref) or lref <= 0:
        raise ValueError("lref must be finite and positive in coordinate units")
    # Two disjoint end intervals require 2 * end_fraction <= 1 by construction.
    if not np.isfinite(end_fraction) or not 0 < end_fraction <= 0.5:
        raise ValueError("end_fraction must be finite and in (0, 0.5]")
    if x.max() == x.min():
        raise ValueError("longitudinal coordinates must have positive span")
    return x, k, a, mask


def _coverage_status(area, valid):
    positive_area = area > 0
    if not np.any(positive_area & valid):
        return "NOT_EVALUATED"
    # Comparing rounded sums can hide small but positive excluded areas.
    return "PARTIAL" if np.any(positive_area & ~valid) else "EVALUATED"


def _weighted_regions(x, k, area, valid, lref, end_fraction):
    span = x.max() - x.min()
    lower = x.min() + end_fraction * span
    # A shared origin keeps the two cuts identical for an exact half-span.
    upper = x.min() + (1 - end_fraction) * span
    valid_area = float(np.sum(area[valid]))
    total_area = float(np.sum(area))
    # arXiv:2609.27544v1 (2026), eqs. (3), (4), (39): q_D = |K| L_ref^2;
    # I_D = sum(A_i q_D_i) / sum(A_i) on valid samples. Use areas, not counts.
    contribution = np.zeros_like(area)
    if valid_area > 0:
        # Reject lost weights/products, even if later factors could recover them.
        with np.errstate(under="raise"):
            contribution[valid] = (area[valid] / valid_area) * np.abs(k[valid]) * lref * lref
    intensity = float(np.sum(contribution))
    # Include one adjacent float on either side of a cut in the middle.
    # This admits decimal cut samples without a scale-dependent isclose band.
    lower_boundary = np.nextafter(lower, -np.inf)
    upper_boundary = np.nextafter(upper, np.inf)
    selections = {
        "lower_end": x < lower_boundary,
        "middle": (x >= lower_boundary) & (x <= upper_boundary),
        "upper_end": x > upper_boundary,
    }
    regions = {}
    for name, selected in selections.items():
        region_area = float(np.sum(area[selected]))
        region_valid = float(np.sum(area[selected & valid]))
        value = float(np.sum(contribution[selected]))
        regions[name] = CurvatureRegionContribution(
            status=_coverage_status(area[selected], valid[selected]),
            area=region_area,
            valid_area=region_valid,
            I_D_contribution=value if region_valid > 0 else None,
            intensity_share=value / intensity if intensity > 0 and region_valid > 0 else None,
        )
    return CurvatureRegionSummary(
        status=_coverage_status(area, valid),
        lref=float(lref),
        end_fraction=float(end_fraction),
        lower_cut=float(lower),
        upper_cut=float(upper),
        total_area=total_area,
        valid_area=valid_area,
        I_D=intensity if valid_area > 0 else None,
        regions=regions,
    )


def summarize_curvature_regions(
    longitudinal_coordinates: ArrayLike,
    gaussian_curvature: ArrayLike,
    vertex_area: ArrayLike,
    valid: ArrayLike,
    *,
    lref: float,
    end_fraction: float,
) -> CurvatureRegionSummary:
    """Partition supplied vertex quadrature into lower end, middle and upper end.

    Coordinates and lref share a length unit; area uses its square and K its
    inverse square. The longitudinal axis and its direction are chosen by the
    caller. End cuts use the full coordinate span, including invalid samples.
    Cut vertices and their immediately adjacent binary64 values belong to the
    middle (one ULP outward per cut); no triangle clipping is performed.
    Underflow during weight normalization or density multiplication is rejected
    with ValueError, even when a different arithmetic ordering could recover a
    representable result. This prevents nonzero intensity being reported as zero.

    Missing curvature is excluded only through an explicit boolean valid mask.
    Status is PARTIAL when positive area is excluded and NOT_EVALUATED when no
    valid area exists. For zero total intensity, shares are undefined (None).
    No end-region fraction or engineering acceptance limit is assumed.
    """
    x, k, area, mask = _validated_inputs(
        longitudinal_coordinates, gaussian_curvature, vertex_area, valid, lref, end_fraction
    )
    try:
        with np.errstate(over="raise", invalid="raise", divide="raise"):
            return _weighted_regions(x, k, area, mask, lref, end_fraction)
    except FloatingPointError as exc:
        raise ValueError("curvature quadrature exceeds finite numerical range") from exc
