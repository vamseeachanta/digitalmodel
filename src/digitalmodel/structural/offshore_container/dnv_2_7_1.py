"""DNV 2.7-1 / DNV-ST-E271 offshore-container structural utilization model.

Reduced-order screening of the governing *offshore lift* load case for offshore
containers (CCUs) per **DNVGL-ST-E271 (2.7-1), Edition August 2017**. Generates
utilization curves across a container's dimensions and member shapes for early
sizing. It is **not** a substitute for the certified FEA + prototype tests the
standard ultimately requires (§4.2, §4.6).

Design basis (clause-traced)
----------------------------
- **Primary structure design load** ``FL = 2.5 · R · g`` (§4.2.3.1); the internal
  (floor) load is ``Fi = (2.5·R − T)·g``.
- **Pad eyes** carry a total vertical ``Fp = 3 · R · g`` (§4.2.3.1), distributed
  over ``(n − 1)`` pad eyes with ``n`` capped to [2, 4]; single pad eye
  ``Fp = 5 · R · g``. The resulting sling load per pad eye accounts for the sling
  angle ``v`` to the **vertical** (45° default unless a smaller angle is specified).
- **Allowable stress** (§4.2.1): Von Mises ``σe ≤ 0.85 · C``; for steel ``C = Re``
  (yield) → allowable = ``0.85 · Re`` (usage factor 0.85, no separate material factor).

The factors live on :class:`DesignFactors`; their authoritative source is the
DNVGL-ST-E271 wiki page (``code_id: dnvgl-st-e271``) — call
:func:`factor_citations` to emit calc-citation sidecars (per the calc-citation
contract), which degrade gracefully when the wiki clone is unavailable.

R = rating / maximum gross mass [kg] (permanent equipment + cargo, excluding the
lifting set, §1.6). T = tare mass; P = payload.

References: DNVGL-ST-E271 §1.6, §4.2.1, §4.2.3.1, §4.6.
"""

from __future__ import annotations

import math
import warnings
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

G = 9.80665  # m/s^2, standard gravity


@dataclass(frozen=True)
class DesignFactors:
    """DNVGL-ST-E271 design factors (clause references in the module docstring).

    Numeric values are the source of truth for the calculation; the same values
    are citation-backed via :func:`factor_citations` against the wiki page
    ``code_id: dnvgl-st-e271``.
    """

    structure_load_factor: float = 2.5    # FL = 2.5 R g  (§4.2.3.1)
    padeye_load_factor: float = 3.0       # Fp = 3 R g    (§4.2.3.1)
    single_padeye_load_factor: float = 5.0  # single pad eye (§4.2.3.1)
    usage_factor: float = 0.85            # sigma_e <= 0.85*Re  (§4.2.1)
    default_sling_angle_to_vertical_deg: float = 45.0  # v (§4.2.3.1)
    tare_fraction: float = 0.25           # T / R (for internal load Fi)


@dataclass(frozen=True)
class SHS:
    """Square hollow section, dimensions in metres."""

    b: float  # outer width [m]
    t: float  # wall thickness [m]

    @property
    def area(self) -> float:
        bi = self.b - 2.0 * self.t
        return self.b ** 2 - bi ** 2

    @property
    def inertia(self) -> float:
        bi = self.b - 2.0 * self.t
        return (self.b ** 4 - bi ** 4) / 12.0

    @property
    def modulus(self) -> float:
        return self.inertia / (self.b / 2.0)

    @classmethod
    def from_mm(cls, b_mm: float, t_mm: float) -> "SHS":
        return cls(b=b_mm / 1000.0, t=t_mm / 1000.0)

    def __str__(self) -> str:  # pragma: no cover - cosmetic
        return f"SHS{self.b*1000:.0f}x{self.t*1000:.0f}"


def sling_angle_to_vertical(length: float, width: float, lift_height: float) -> float:
    """Sling angle from the **vertical** [rad] for a 4-point lift.

    Slings run from the top corners to a master link at ``lift_height`` above the
    top frame, centred over the plan centroid. Per §4.2.3.1 the design angle is
    45° unless a smaller angle is specified; a taller ``lift_height`` reduces the
    angle (and the inward horizontal pull).
    """
    half_diag = 0.5 * math.hypot(length, width)
    return math.atan2(half_diag, lift_height)


def allowable_stress(fy: float, factors: DesignFactors) -> float:
    """Allowable Von Mises stress [Pa] = usage_factor * fy  (§4.2.1: 0.85*Re)."""
    return factors.usage_factor * fy


@dataclass(frozen=True)
class Utilization:
    top_rail: float          # axial utilization of top side rail (inward sling pull)
    floor: float             # bending utilization of bottom floor beam (Fi)
    pad_eye: float           # axial-stress utilization of a pad eye (RSL)
    governing: float         # max of the member checks
    sling_angle_to_vertical_deg: float
    rsl_kN: float            # resulting sling load per pad eye
    FL_kN: float             # primary-structure design load (2.5 R g)


def utilization(length: float, width: float, lift_height: float,
                rating_kg: float, section: SHS, fy: float = 355e6,
                factors: DesignFactors | None = None, n_padeyes: int = 4,
                padeye_area_m2: float = 0.002) -> Utilization:
    """Governing primary-structure utilization for the DNV 2.7-1 offshore lift.

    Parameters
    ----------
    length, width : container plan dimensions [m].
    lift_height : master-link height above top frame [m] (sets sling angle).
    rating_kg : gross mass R [kg] (excludes lifting set, §1.6).
    section : SHS used for the peripheral frame members.
    fy : yield strength Re [Pa] (default S355).
    n_padeyes : number of pad eyes; clamped to [2, 4] per §4.2.3.1.
    padeye_area_m2 : representative pad-eye main-plate net area [m^2].
    """
    factors = factors or DesignFactors()
    v = sling_angle_to_vertical(length, width, lift_height)
    sigma_allow = allowable_stress(fy, factors)

    # Pad-eye load: Fp = 3 R g over (n-1) pad eyes; resolve by sling angle v.
    n = max(2, min(4, n_padeyes))
    fp_total = factors.padeye_load_factor * rating_kg * G
    fp_per = fp_total / (n - 1)
    rsl = fp_per / math.cos(v)                 # resulting sling load per pad eye
    horizontal_per = fp_per * math.tan(v)      # inward pull into top frame

    # Top side rail: inward horizontal sling component carried as axial.
    util_top = (horizontal_per / section.area) / sigma_allow

    # Pad eye: axial stress over its net area.
    util_padeye = (rsl / padeye_area_m2) / sigma_allow

    # Bottom floor beam: internal load Fi = (2.5R - T) g as a UDL, simply
    # supported over the length; half to each long side rail.
    tare = factors.tare_fraction * rating_kg
    fi = (factors.structure_load_factor * rating_kg - tare) * G
    w_line = (fi / 2.0) / length
    moment = w_line * length ** 2 / 8.0
    util_floor = (moment / section.modulus) / sigma_allow

    fl = factors.structure_load_factor * rating_kg * G
    governing = max(util_top, util_floor, util_padeye)
    return Utilization(top_rail=util_top, floor=util_floor, pad_eye=util_padeye,
                       governing=governing,
                       sling_angle_to_vertical_deg=math.degrees(v),
                       rsl_kN=rsl / 1e3, FL_kN=fl / 1e3)


_CITATION_WARNED = False


def factor_citations(repo_root: Optional[Path] = None) -> dict:
    """Return ``{factor_name: CitedValue}`` for the DNV-ST-E271 design factors.

    Validates each citation against the wiki page (``code_id: dnvgl-st-e271``).
    In standalone mode (no resolvable llm-wiki clone) it degrades gracefully:
    emits a one-shot warning and returns ``{}`` rather than failing the calc —
    mirroring the calc-citation pilot. Where the wiki resolves, a missing or
    mismatched page fails closed (raises).
    """
    global _CITATION_WARNED
    from digitalmodel.citations.registry import get_offshore_container_factor
    from digitalmodel.citations.schema import CitationResolutionError

    names = ("primary_structure_load", "padeye_load", "single_padeye_load",
             "allowable_usage_factor", "default_sling_angle_to_vertical_deg")
    out: dict = {}
    for name in names:
        try:
            out[name] = get_offshore_container_factor(name, repo_root=repo_root)
        except CitationResolutionError as exc:
            if not _CITATION_WARNED:
                warnings.warn(
                    "DNV-ST-E271 citations unavailable (standalone mode): "
                    f"{exc}. Configure LLM_WIKI_PATH to enable calc citations.",
                    RuntimeWarning, stacklevel=2,
                )
                _CITATION_WARNED = True
            return {}
    return out


# Convenience: a small catalogue of representative SHS frame members.
SECTION_CATALOGUE: dict[str, SHS] = {
    "SHS150x8": SHS.from_mm(150, 8),
    "SHS150x10": SHS.from_mm(150, 10),
    "SHS200x10": SHS.from_mm(200, 10),
    "SHS200x12": SHS.from_mm(200, 12),
    "SHS250x12": SHS.from_mm(250, 12),
}
