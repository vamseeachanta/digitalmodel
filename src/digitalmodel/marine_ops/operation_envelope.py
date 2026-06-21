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

Criteria are **indicative screening values** grounded in DNV-RP-H103 (marine
operations) / DNV-ST-N001; tune per project. They are vessel-level significant
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


# Indicative screening criteria (DNV-RP-H103 / DNV-ST-N001 basis). Project-tune.
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
        basis="indicative seakeeping/comfort + green-water screening (DNV-ST-N001)",
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
        basis="indicative station-keeping motion screening (DNV-ST-N001)",
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
        basis="indicative lifting motion limits (DNV-RP-H103); crane-tip refinement later",
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
        basis="indicative relative-motion proxy (DNV-ST-N001); two-body refinement later",
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
    hs_limit_m: float
    governing_dof: str
    governing_heading_deg: float


@dataclass
class EnvelopeResult:
    vessel: str
    operation: str
    alpha: float
    points: list[EnvelopePoint] = field(default_factory=list)

    def hs_limit_at(self, tp_s: float) -> Optional[float]:
        for p in self.points:
            if abs(p.tp_s - tp_s) < 1e-9:
                return p.hs_limit_m
        return None

    def to_dict(self) -> dict:
        return {
            "vessel": self.vessel,
            "operation": self.operation,
            "alpha": self.alpha,
            "points": [
                {
                    "tp_s": p.tp_s,
                    "hs_limit_m": p.hs_limit_m,
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
    """
    op = resolve_operation(operation)
    tps = tp_range_s or DEFAULT_TP_RANGE_S
    spectra = WaveSpectra()

    points: list[EnvelopePoint] = []
    for tp in tps:
        omega, s_wave = spectra.jonswap(hs_ref, tp, gamma=gamma, n_points=200)
        best_hs = hs_cap_m
        gov_dof = "none"
        gov_hdg = float("nan")
        for limit in op.limits:
            component = _component_for(rao_set, limit.dof)
            if component is None:
                continue
            for hdg, rao_at_hdg in _rao_by_heading(component, omega, headings):
                s_ref = _significant_response(rao_at_hdg, omega, s_wave)
                if s_ref <= 0:
                    continue  # this DOF does not respond -> does not limit
                hs_allow = op.alpha * hs_ref * limit.max_significant / s_ref
                if hs_allow < best_hs:
                    best_hs = hs_allow
                    gov_dof = limit.dof
                    gov_hdg = hdg
        points.append(
            EnvelopePoint(
                tp_s=float(tp),
                hs_limit_m=round(float(best_hs), 4),
                governing_dof=gov_dof,
                governing_heading_deg=float(gov_hdg),
            )
        )
    return EnvelopeResult(
        vessel=rao_set.vessel_name, operation=op.key, alpha=op.alpha, points=points
    )


def envelope_for_operations(
    rao_set: RAOSet,
    operations: Optional[list[Union[str, Operation]]] = None,
    **kwargs,
) -> dict[str, EnvelopeResult]:
    """Compute envelopes for several operations (default: all four)."""
    ops = operations if operations is not None else list(OPERATIONS)
    out: dict[str, EnvelopeResult] = {}
    for op in ops:
        result = operation_envelope(rao_set, op, **kwargs)
        out[result.operation] = result
    return out


def plot_envelopes(results: dict[str, EnvelopeResult], out_path) -> Optional[object]:
    """Plot Hs limit vs Tp, one curve per operation. Returns the path, or None
    if matplotlib is unavailable (plotting is a convenience, not a contract)."""
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
        hs = [p.hs_limit_m for p in env.points]
        ax.plot(tps, hs, marker="o", markersize=3, label=label)
    ax.set_xlabel("Peak period Tp (s)")
    ax.set_ylabel("Limiting Hs (m)")
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
        col = int(np.argmin(np.abs(avail - hdg)))
        mag = np.asarray(component.magnitude, dtype=float)[:, col][order]
        # np.interp clamps to endpoint RAO outside the data band (screening).
        yield float(avail[col]), np.interp(omega, freqs_sorted, mag)


def _significant_response(rao_on_omega, omega, s_wave) -> float:
    response = compute_response_spectrum(rao_on_omega, s_wave)
    m0 = spectral_moments(omega, response, [0])[0]
    if m0 <= 0:
        return 0.0
    return significant_amplitude(m0)
