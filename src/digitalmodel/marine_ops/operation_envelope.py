"""Vessel operability envelope by typical marine operation.

For a vessel RAO set and a marine operation, compute the **limiting sea state**:
the maximum significant wave height ``Hs`` the vessel can work in at each peak
period ``Tp``, worst-case over wave headings and the operation's governing motion
DOF, with an operational contingency factor.

Physics (frequency domain, linear, narrow-band):

    S_resp(w; Hs, Tp, hdg) = |RAO(w, hdg)|^2 * S_wave(w; Hs, Tp)
    m0 = integral S_resp dw ;  significant motion s = 2 sqrt(m0)

``s`` scales linearly with ``Hs`` (the spectrum variance scales with Hs^2), so the
Hs limit for a criterion ``C`` at a given Tp/heading/DOF is closed-form:

    Hs_limit = alpha * Hs_ref * C / s(Hs_ref)

The envelope at each Tp is the minimum Hs_limit over all headings and all
criteria DOFs (the governing combination).

Criteria are **unverified screening values**, with no established DNV clause
or table provenance; tune per project. They are vessel-level significant
single-amplitude motions — crane-tip / relative-motion refinements (which need
lift geometry or a second body) are a later layer.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Union

import numpy as np

from digitalmodel.hydrodynamics.diffraction.output_schemas import RAOComponent, RAOSet
from digitalmodel.hydrodynamics.seakeeping import (
    compute_response_spectrum,
    significant_amplitude,
    spectral_moments,
)
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
from .envelope_numerics import (
    MAX_EXTRAPOLATED_ENERGY, extrapolated_fraction, finite_grid, heading_column, positive, spectrum,
    validate_component, validate_operation,
)

DEFAULT_TP_RANGE_S = [round(4.0 + 0.5 * i, 1) for i in range(33)]  # 4.0 .. 20.0 s
_HS_CAP_M = 20.0  # cap an "unlimited" DOF so a non-responding axis never governs


@dataclass(frozen=True)
class MotionLimit:
    """A significant single-amplitude motion limit for one DOF."""

    dof: str  # "heave" | "roll" | "pitch" | "surge" | "sway" | "yaw"
    max_significant: float
    unit: str  # "m" (translation) | "deg" (rotation)


@dataclass(frozen=True)
class Operation:
    """A typical marine operation and its limiting motion criteria."""

    key: str
    label: str
    limits: tuple[MotionLimit, ...]
    alpha: float  # operational contingency on the limit (<= 1.0)
    basis: str


# Unverified screening criteria; no standards qualification. Basis table B6.
OPERATIONS: dict[str, Operation] = {
    "transit": Operation(
        key="transit",
        label="Transit / field transit",
        limits=(
            MotionLimit("heave", 3.0, "m"),
            MotionLimit("roll", 6.0, "deg"),
            MotionLimit("pitch", 2.5, "deg"),
        ),
        alpha=0.90,
        basis="unverified seakeeping/comfort + green-water screening",
    ),
    "dp_station_keeping": Operation(
        key="dp_station_keeping",
        label="DP station-keeping",
        limits=(
            MotionLimit("heave", 2.0, "m"),
            MotionLimit("roll", 4.0, "deg"),
            MotionLimit("pitch", 2.0, "deg"),
        ),
        alpha=0.90,
        basis="unverified station-keeping motion screening",
    ),
    "heavy_lift": Operation(
        key="heavy_lift",
        label="Heavy lift / installation",
        limits=(
            MotionLimit("heave", 1.5, "m"),
            MotionLimit("roll", 2.0, "deg"),
            MotionLimit("pitch", 1.5, "deg"),
        ),
        alpha=0.85,
        basis="unverified lifting motion limits; crane-tip refinement later",
    ),
    "offloading": Operation(
        key="offloading",
        label="Offloading / offtake",
        limits=(
            MotionLimit("heave", 2.5, "m"),
            MotionLimit("roll", 4.0, "deg"),
            MotionLimit("pitch", 2.0, "deg"),
        ),
        alpha=0.90,
        basis="unverified relative-motion proxy; two-body refinement later",
    ),
}


def available_operations() -> list[str]:
    return list(OPERATIONS)


def resolve_operation(operation: Union[str, Operation]) -> Operation:
    if isinstance(operation, Operation):
        return operation
    try:
        return OPERATIONS[operation]
    except KeyError as exc:
        raise KeyError(
            f"unknown operation {operation!r}; known: {sorted(OPERATIONS)}"
        ) from exc


@dataclass
class EnvelopePoint:
    tp_s: float
    hs_limit_m: Optional[float]
    governing_dof: str
    governing_heading_deg: Optional[float]
    evaluated: bool = False
    status: str = "unevaluated"
    extrapolated_energy_fraction: float = 0.0
    captured_variance_fraction: float = 0.0

    @property
    def hs_forecast_limit_m(self):
        """Explicit forecast criterion; hs_limit_m is the compatibility alias."""
        return self.hs_limit_m


@dataclass
class EnvelopeResult:
    vessel: str
    operation: str
    alpha: float
    points: list[EnvelopePoint] = field(default_factory=list)
    criterion_type: str = "forecast_limit"
    alpha_source: dict = field(default_factory=dict)
    citation_sidecar: dict = field(default_factory=dict)

    def require_valid_points(self):
        """Accept explicit coverage gaps; reject unknown or contradictory states."""
        if not self.points:
            raise ValueError('envelope contains unevaluated points')
        for p in self.points:
            if (p.status == 'insufficient_coverage' and not p.evaluated
                    and p.hs_limit_m is None and p.governing_heading_deg is None
                    and MAX_EXTRAPOLATED_ENERGY < p.extrapolated_energy_fraction <= 1):
                continue
            if not p.evaluated or p.status not in ('evaluated', 'capped'):
                raise ValueError('envelope contains unevaluated points')
            if p.hs_limit_m is None or not np.isfinite(p.hs_limit_m) or p.hs_limit_m <= 0:
                raise ValueError('envelope contains invalid forecast limits')
            if not 0 <= p.extrapolated_energy_fraction <= MAX_EXTRAPOLATED_ENERGY:
                raise ValueError('envelope contains insufficient coverage')

    def require_evaluated(self):
        self.require_valid_points()
        if any(not p.evaluated for p in self.points):
            raise ValueError('envelope contains unevaluated points: insufficient coverage')

    def hs_limit_at(self, tp_s: float) -> Optional[float]:
        self.require_valid_points()
        for p in self.points:
            if abs(p.tp_s - tp_s) < 1e-9:
                return p.hs_limit_m
        return None

    def to_dict(self) -> dict:
        self.require_valid_points()
        return {
            "vessel": self.vessel,
            "operation": self.operation,
            "alpha": self.alpha,
            "criterion_type": self.criterion_type,
            "alpha_source": self.alpha_source,
            "citation_sidecar": self.citation_sidecar,
            "points": [
                {
                    "tp_s": p.tp_s,
                    "hs_limit_m": p.hs_limit_m,
                    "hs_forecast_limit_m": p.hs_forecast_limit_m,
                    "evaluated": p.evaluated,
                    "status": p.status,
                    "extrapolated_energy_fraction": p.extrapolated_energy_fraction,
                    "captured_variance_fraction": p.captured_variance_fraction,
                    "governing_dof": p.governing_dof,
                    "governing_heading_deg": p.governing_heading_deg,
                }
                for p in self.points
            ],
        }


def operation_envelope(
    rao_set: RAOSet,
    operation: Union[str, Operation],
    *,
    tp_range_s: Optional[list[float]] = None,
    headings: Optional[list[float]] = None,
    hs_ref: float = 1.0,
    gamma: float = 3.3,
    hs_cap_m: float = _HS_CAP_M,
) -> EnvelopeResult:
    """Compute the Hs/Tp limiting-sea-state envelope for one operation.

    ``rao_set`` supplies per-DOF RAO magnitude (m/m for translations, deg/m for
    rotations). The envelope at each Tp is the worst case over ``headings``
    (default: all RAO headings) and the operation's criteria DOFs.

    Numerical evaluation does not qualify the screening inputs. The citation
    sidecar records the limits used and explicit standards evidence gaps;
    alpha selection dependencies remain unknown (see envelope-basis.md).
    """
    op = resolve_operation(operation)
    validate_operation(op)
    tps = finite_grid(DEFAULT_TP_RANGE_S if tp_range_s is None else tp_range_s,
                      'Tp', positive_values=True)
    for value, name in ((hs_ref, 'Hs_ref'), (hs_cap_m, 'Hs cap'), (gamma, 'gamma')):
        positive(value, name)
    if gamma < 1:
        raise ValueError('gamma must be >= 1')
    if headings is not None:
        headings = finite_grid(headings, 'headings')
    for limit in op.limits:
        validate_component(_component_for(rao_set, limit.dof), limit.dof)
    spectra = WaveSpectra()
    points = [_evaluate_point(rao_set, op, tp, headings, hs_ref, gamma, hs_cap_m, spectra)
              for tp in tps]
    return EnvelopeResult(
        vessel=rao_set.vessel_name, operation=op.key, alpha=op.alpha, points=points,
        alpha_source={
            'status': 'unverified_screening', 'table_reference': None,
            'basis': op.basis, 'value': op.alpha,
            # Issue #2769 acceptance and audit #2747, section 12: these are
            # alpha selection dependencies, not Tp or the spectral Hs_ref.
            'dependencies': {
                'planned_operation_period_h': None,
                'design_hs_m': None,
                'forecast_level': None,
            },
        },
        citation_sidecar={
            'source_sibling': 'generic', 'limits_status': 'unverified_screening',
            'reference': 'docs/envelope-basis.md',
            'equations': ['B1', 'B2', 'B3', 'B4', 'B5'],
            'catalogue_table': 'B6' if op == OPERATIONS.get(op.key) else None,
            'standard_clause': None,
            'motion_limits': [
                {'dof': limit.dof, 'max_significant': limit.max_significant,
                 'unit': limit.unit}
                for limit in op.limits
            ],
            # No designation, edition, clause or table has been verified.
            # A caller's basis string is descriptive, not qualification evidence.
            'qualification': {
                'status': 'NOT_EVALUATED',
                'reason': 'Standard evidence and alpha selection inputs are unavailable.',
                'standard_designation': None,
                'standard_edition': None,
                'motion_limit_clause': None,
                'alpha_clause': None,
                'alpha_table': None,
                'evidence_reference': None,
            },
        },
    )


def _evaluate_point(rao_set, op, tp, headings, hs_ref, gamma, hs_cap_m, spectra):
    omega, s_wave, captured = spectrum(spectra, hs_ref, tp, gamma)
    best_hs, gov_dof, gov_hdg, excluded = hs_cap_m, 'cap', None, 0.
    for limit in op.limits:
        component = _component_for(rao_set, limit.dof)
        for hdg, rao_at_hdg in _rao_by_heading(component, omega, headings):
            excluded = max(excluded, extrapolated_fraction(component, omega, s_wave, rao_at_hdg))
            s_ref = _significant_response(rao_at_hdg, omega, s_wave)
            if s_ref == 0:
                continue  # Evaluated zero response is distinct from absent input.
            hs_allow = op.alpha * hs_ref * limit.max_significant / s_ref  # Basis B2.
            if not np.isfinite(hs_allow) or hs_allow <= 0:
                raise ValueError('invalid forecast limit from response')
            if hs_allow < best_hs:
                best_hs, gov_dof, gov_hdg = hs_allow, limit.dof, hdg
    if excluded > MAX_EXTRAPOLATED_ENERGY:
        return EnvelopePoint(float(tp), None, 'coverage', None, False,
                             'insufficient_coverage', excluded, captured)
    return EnvelopePoint(float(tp), float(best_hs), gov_dof, gov_hdg, True,
                         'capped' if gov_dof == 'cap' else 'evaluated', excluded, captured)


def envelope_for_operations(
    rao_set: RAOSet,
    operations: Optional[list[Union[str, Operation]]] = None,
    **kwargs,
) -> dict[str, EnvelopeResult]:
    """Compute envelopes for a non-empty selection of distinct operation keys.

    ``None`` selects the catalogue in insertion order. An explicit empty list
    or repeated resolved key raises ``ValueError`` before any calculation;
    an unknown catalogue name raises ``KeyError`` at the same stage. Custom
    operations and catalogue names share the result-key namespace. Results
    retain selection order. See knowledge-pack item S10 and issue #2768.
    """
    selection = list(OPERATIONS) if operations is None else operations
    ops = [resolve_operation(op) for op in selection]
    if not ops:
        raise ValueError('operations must not be empty')
    seen = set()
    for op in ops:
        if op.key in seen:
            raise ValueError(f'duplicate operation key: {op.key}')
        seen.add(op.key)
    out: dict[str, EnvelopeResult] = {}
    for op in ops:
        result = operation_envelope(rao_set, op, **kwargs)
        out[result.operation] = result
    return out


def plot_envelopes(results: dict[str, EnvelopeResult], out_path) -> Optional[object]:
    """Plot Hs limit vs Tp, one curve per operation. Returns the path, or None
    if matplotlib is unavailable (plotting is a convenience, not a contract)."""
    for result in results.values():
        result.require_valid_points()
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except Exception:  # noqa: BLE001 - optional dep
        return None
    from pathlib import Path

    out_path = Path(out_path)
    fig, ax = plt.subplots(figsize=(7.5, 4.8))
    vessel = ""
    for op_key, env in results.items():
        vessel = env.vessel
        label = OPERATIONS[op_key].label if op_key in OPERATIONS else op_key
        tps = [p.tp_s for p in env.points]
        hs = [p.hs_limit_m if p.evaluated else np.nan for p in env.points]
        ax.plot(tps, hs, marker="o", markersize=3, label=label)
    ax.set_xlabel("Peak period Tp (s)")
    ax.set_ylabel("Forecast Hs limit (m)")
    ax.set_title(f"{vessel} — operability envelope by operation")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize="small")
    fig.savefig(out_path, dpi=110, bbox_inches="tight")
    plt.close(fig)
    return out_path


def _component_for(rao_set: RAOSet, dof: str) -> Optional[RAOComponent]:
    return getattr(rao_set, dof, None)


def _rao_by_heading(component, omega, headings):
    """Yield (heading_deg, RAO magnitude interpolated onto omega) per heading."""
    avail = np.asarray(component.headings.values, dtype=float)
    freqs = np.asarray(component.frequencies.values, dtype=float)
    order = np.argsort(freqs)
    freqs_sorted = freqs[order]
    targets = avail if headings is None else np.asarray(headings, dtype=float)
    for hdg in targets:
        col = heading_column(avail, hdg)
        mag = np.asarray(component.magnitude, dtype=float)[:, col][order]
        # Held-edge continuation supplies the per-heading response bound (B4).
        yield float(avail[col]), np.interp(omega, freqs_sorted, mag)


def _significant_response(rao_on_omega, omega, s_wave) -> float:
    response = compute_response_spectrum(rao_on_omega, s_wave)
    m0 = spectral_moments(omega, response, [0])[0]
    if not np.isfinite(m0) or m0 < 0:
        raise ValueError('response moment must be finite and non-negative')
    if m0 <= 0:
        return 0.0
    return significant_amplitude(m0)
