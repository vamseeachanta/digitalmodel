"""FFS decision engine — translate assessment margins into action verdicts.

One shared verdict vocabulary for every asset class (issue #2205, owner
decision D1 = A) with a per-class *action map* that gives each class its
native report wording.  The decision tree follows API 579-1/ASME FFS-1 2021
Edition Figure 2.1 (Assessment Flowchart) and Section 2.4.2; other classes
reuse the same tree with their own margin (RSF, reserve factor, thickness
ratio, RSR, ...) and their own bands.

Verdict vocabulary (shared)
---------------------------
ACCEPT       Screening passes with margin clear of the monitor band.
MONITOR      Screening passes but margin is close to the allowable —
             increase inspection frequency.  (M_a <= M < M_a + monitor_band)
DERATE       Screening fails but a reduced rating (pressure, tension limit,
             load / exposure category) brings the component back to
             acceptance.  ``RE_RATE`` is an alias kept for existing callers.
             (derate_floor <= M < M_a)
REPAIR       Screening fails and de-rating is not viable; physical repair.
REPLACE      Margin is below the de-rating floor.
ESCALATE     Margin cannot be evaluated by screening (non-finite) — hand off
             to a higher assessment level.

Per-class action map (report wording)
-------------------------------------
pressure / pipeline / tank   ACCEPT / MONITOR / RE_RATE / REPAIR / REPLACE
mooring_chain                CONTINUE / SHORTEN INTERVAL / REDUCE TENSION
                             LIMIT / REPLACE SEGMENT / REPLACE LINE
hull_plating                 ACCEPT / SUBSTANTIAL CORROSION / RESTRICT
                             LOADING / RENEW / RENEW (EXTENSIVE)
jacket_member                ACCEPT / MONITOR / MITIGATE / REPAIR /
                             REPLACE MEMBER

Units
-----
Margins are dimensionless ratios (plain floats or dimensionless pint
quantities).  Remaining life is in years (plain float) or any pint time
quantity.  De-rating callables are built from unit-tagged limits
(:func:`reduced_mawp`, :func:`scaled_limit`) and reject bare floats or
mismatched dimensions with :class:`UnitTagError`.  The legacy
:meth:`FFSDecision.decide` signature keeps its inch / psi contract by tagging
its inputs internally.

References:
  API 579-1/ASME FFS-1 2021 §2.4.2, Figure 2.1
  ASME PCC-2-2022 (repair methods)
  API RP 2SK (mooring), IACS UR Z (hull renewal), API RP 2SIM (jackets)
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, Mapping, Optional, Union

from digitalmodel.units import Q_, ureg

Number = Union[int, float]
QuantityLike = Union[Number, "ureg.Quantity"]


class UnitTagError(TypeError):
    """An input is missing its unit tag or carries the wrong dimension."""


# ---------------------------------------------------------------------------
# Shared verdict vocabulary
# ---------------------------------------------------------------------------
class Verdict(str, Enum):
    """Shared FFS verdict set.  ``RE_RATE`` is an alias of ``DERATE``."""

    ACCEPT = "ACCEPT"
    MONITOR = "MONITOR"
    DERATE = "DERATE"
    RE_RATE = "DERATE"  # alias — legacy name
    REPAIR = "REPAIR"
    REPLACE = "REPLACE"
    ESCALATE = "ESCALATE"

    @classmethod
    def _missing_(cls, value: object):
        if value == "RE_RATE":
            return cls.DERATE
        return None


class AssetClass(str, Enum):
    PRESSURE = "pressure"
    PIPELINE = "pipeline"
    MOORING_CHAIN = "mooring_chain"
    HULL_PLATING = "hull_plating"
    JACKET_MEMBER = "jacket_member"
    TANK = "tank"


@dataclass(frozen=True)
class DecisionBands:
    """Per-class decision thresholds (defaults are today's pressure values)."""

    monitor_band: float = 0.05   # MONITOR when M_a <= M < M_a + monitor_band
    derate_floor: float = 0.50   # REPLACE when M < derate_floor
    repair_life_yr: float = 2.0  # REPAIR when screening fails and life is short


@dataclass(frozen=True)
class AssetClassPolicy:
    """Everything class-specific: bands, wording, margin label, fallback note."""

    name: str
    bands: DecisionBands
    actions: Mapping[Verdict, str]
    margin_label: str
    no_derate_note: str


_PRESSURE_ACTIONS = {
    Verdict.ACCEPT: "ACCEPT",
    Verdict.MONITOR: "MONITOR",
    Verdict.DERATE: "RE_RATE",
    Verdict.REPAIR: "REPAIR",
    Verdict.REPLACE: "REPLACE",
    Verdict.ESCALATE: "ESCALATE",
}
_MOORING_ACTIONS = {
    Verdict.ACCEPT: "CONTINUE",
    Verdict.MONITOR: "SHORTEN INTERVAL",
    Verdict.DERATE: "REDUCE TENSION LIMIT",
    Verdict.REPAIR: "REPLACE SEGMENT",
    Verdict.REPLACE: "REPLACE LINE",
    Verdict.ESCALATE: "ESCALATE",
}
_HULL_ACTIONS = {
    Verdict.ACCEPT: "ACCEPT",
    Verdict.MONITOR: "SUBSTANTIAL CORROSION",
    Verdict.DERATE: "RESTRICT LOADING",
    Verdict.REPAIR: "RENEW",
    Verdict.REPLACE: "RENEW (EXTENSIVE)",
    Verdict.ESCALATE: "ESCALATE",
}
_JACKET_ACTIONS = {
    Verdict.ACCEPT: "ACCEPT",
    Verdict.MONITOR: "MONITOR",
    Verdict.DERATE: "MITIGATE",
    Verdict.REPAIR: "REPAIR",
    Verdict.REPLACE: "REPLACE MEMBER",
    Verdict.ESCALATE: "ESCALATE",
}

_PRESSURE_NOTE = "Reduce MAWP or operating pressure."

ASSET_CLASSES: Mapping[str, AssetClassPolicy] = {
    "pressure": AssetClassPolicy(
        "pressure", DecisionBands(), _PRESSURE_ACTIONS, "RSF", _PRESSURE_NOTE
    ),
    "pipeline": AssetClassPolicy(
        "pipeline", DecisionBands(), _PRESSURE_ACTIONS, "RSF", _PRESSURE_NOTE
    ),
    "tank": AssetClassPolicy(
        "tank", DecisionBands(), _PRESSURE_ACTIONS, "RSF", _PRESSURE_NOTE
    ),
    "mooring_chain": AssetClassPolicy(
        "mooring_chain", DecisionBands(), _MOORING_ACTIONS, "RF",
        "Reduce the line tension limit or pretension.",
    ),
    "hull_plating": AssetClassPolicy(
        "hull_plating", DecisionBands(), _HULL_ACTIONS, "t_ratio",
        "Restrict loading until renewal.",
    ),
    "jacket_member": AssetClassPolicy(
        "jacket_member", DecisionBands(), _JACKET_ACTIONS, "RSR",
        "Mitigate: reduce loads or exposure category.",
    ),
}


def _policy(asset_class: Union[str, AssetClass]) -> AssetClassPolicy:
    key = asset_class.value if isinstance(asset_class, AssetClass) else str(asset_class)
    try:
        return ASSET_CLASSES[key]
    except KeyError:
        raise ValueError(
            f"Unknown asset_class {key!r}; expected one of {sorted(ASSET_CLASSES)}"
        ) from None


def action_for(verdict: Union[Verdict, str], asset_class: Union[str, AssetClass]) -> str:
    """Class-native report wording for a shared verdict."""
    return _policy(asset_class).actions[Verdict(verdict)]


# ---------------------------------------------------------------------------
# Unit tagging helpers
# ---------------------------------------------------------------------------
def _is_quantity(x: Any) -> bool:
    return isinstance(x, ureg.Quantity)


def _require(q: Any, dimension: str, what: str) -> "ureg.Quantity":
    if not _is_quantity(q):
        raise UnitTagError(f"{what} must be a unit-tagged Quantity with {dimension}, got {type(q).__name__}")
    if not q.check(dimension):
        raise UnitTagError(f"{what} must have dimension {dimension}, got {q.units}")
    return q


def _dimensionless(x: QuantityLike, what: str) -> float:
    if _is_quantity(x):
        if not x.dimensionless:
            raise UnitTagError(f"{what} is a ratio and must be dimensionless, got {x.units}")
        return float(x.to("dimensionless").magnitude)
    if isinstance(x, (int, float)):
        return float(x)
    raise UnitTagError(f"{what} must be a number or dimensionless Quantity, got {type(x).__name__}")


def _years(x: QuantityLike, what: str = "remaining_life_yr") -> float:
    if _is_quantity(x):
        return float(_require(x, "[time]", what).to("year").magnitude)
    if isinstance(x, (int, float)):
        return float(x)
    raise UnitTagError(f"{what} must be a number of years or a time Quantity, got {type(x).__name__}")


def remaining_life(t_mm: "ureg.Quantity", t_min: "ureg.Quantity",
                   rate: "ureg.Quantity") -> float:
    """Linear remaining-life projection, unit-aware.  Returns years.

    ``float('inf')`` when the rate is zero; ``0.0`` when t_mm <= t_min.
    """
    t_mm = _require(t_mm, "[length]", "t_mm")
    t_min = _require(t_min, "[length]", "t_min")
    rate = _require(rate, "[length]/[time]", "corrosion rate")
    if t_mm <= t_min:
        return 0.0
    if rate.magnitude == 0.0:
        return float("inf")
    return float(((t_mm - t_min) / rate).to("year").magnitude)


# ---------------------------------------------------------------------------
# De-rating callables
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class Derating:
    """A reduced rating and the sentence the report quotes for it."""

    value: "ureg.Quantity"
    note: str


DerateFn = Callable[[float, float], Optional[Derating]]


def scaled_limit(limit: "ureg.Quantity", dimension: str, *, label: str = "limit_r",
                 reference: str = "limit x M/M_a") -> DerateFn:
    """Build a de-rating callable: ``limit_r = limit * min(1, M / M_a)``.

    The limit must be a unit-tagged Quantity of ``dimension`` (e.g. ``[force]``
    for a mooring tension limit).  The callable returns ``None`` when the
    allowable is non-positive (no meaningful ratio).
    """
    limit = _require(limit, dimension, "limit")

    def _derate(margin: float, allowable: float) -> Optional[Derating]:
        if allowable <= 0 or not math.isfinite(margin):
            return None
        value = limit * min(1.0, max(0.0, margin) / allowable)
        note = f"{label}={value.magnitude:.0f} {value.units:~} ({reference})."
        return Derating(value, note)

    return _derate


def reduced_mawp(design_pressure: "ureg.Quantity") -> DerateFn:
    """Pressure default: MAWP_r = MAWP * min(1, RSF/RSFa), API 579-1 §2.4.2.2."""
    fn = scaled_limit(
        design_pressure, "[pressure]", label="MAWP_r",
        reference="MAWP x RSF/RSFa, API 579-1 §2.4.2.2",
    )

    def _derate(margin: float, allowable: float) -> Optional[Derating]:
        d = fn(margin, allowable)
        return None if d is None else Derating(d.value, "Re-rate to " + d.note)

    return _derate


# ---------------------------------------------------------------------------
# Decision
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class Decision:
    verdict: Verdict
    action: str
    asset_class: str
    margin: float
    margin_allowable: float
    remaining_life_yr: float
    governing_criterion: str
    derated: Optional["ureg.Quantity"] = None

    def to_dict(self) -> dict:
        """Legacy-shaped, JSON-friendly payload (``verdict`` is the shared word)."""
        rerated_psi = None
        derated_mag = derated_units = None
        if self.derated is not None:
            derated_mag = float(self.derated.magnitude)
            derated_units = f"{self.derated.units:~}"
            if self.derated.check("[pressure]"):
                rerated_psi = float(self.derated.to("psi").magnitude)
        return {
            "verdict": self.verdict.value,
            "action": self.action,
            "asset_class": self.asset_class,
            "remaining_life_yr": self.remaining_life_yr,
            "governing_criterion": self.governing_criterion,
            "margin": self.margin,
            "margin_allowable": self.margin_allowable,
            "rsf": self.margin,
            "rsf_a": self.margin_allowable,
            "rerated_mawp_psi": rerated_psi,
            "derated": derated_mag,
            "derated_units": derated_units,
        }


def decide(
    margin: QuantityLike,
    margin_allowable: QuantityLike,
    remaining_life_yr: QuantityLike,
    asset_class: Union[str, AssetClass],
    *,
    derate: Optional[DerateFn] = None,
    bands: Optional[DecisionBands] = None,
    screening_pass: Optional[bool] = None,
) -> Decision:
    """Shared FFS decision tree for any asset class.

    Args:
        margin: Dimensionless strength margin (RSF, reserve factor, thickness
            ratio, RSR ...).  Larger is safer; ``margin >= margin_allowable``
            passes.  Non-finite -> ESCALATE.
        margin_allowable: Allowable margin (e.g. RSFa = 0.90).
        remaining_life_yr: Years (float) or a pint time quantity.
        asset_class: One of :data:`ASSET_CLASSES`.
        derate: Injected de-rating callable ``(margin, allowable) -> Derating``
            built with :func:`reduced_mawp` / :func:`scaled_limit`.  Evaluated
            on every verdict (reported alongside), quoted on DERATE.
        bands: Override the class's :class:`DecisionBands`.
        screening_pass: Whether the primary screening passed.  ``None``
            derives it as ``margin >= allowable and remaining_life > 0``; the
            legacy wrapper passes the Level 1/Level 2 outcome explicitly.
    """
    policy = _policy(asset_class)
    b = bands or policy.bands
    m = _dimensionless(margin, "margin")
    m_a = _dimensionless(margin_allowable, "margin_allowable")
    life = _years(remaining_life_yr)
    lbl = policy.margin_label

    derating = derate(m, m_a) if derate is not None else None
    note = derating.note if derating is not None else policy.no_derate_note

    def _make(verdict: Verdict, criterion: str) -> Decision:
        return Decision(
            verdict=verdict,
            action=policy.actions[verdict],
            asset_class=policy.name,
            margin=m,
            margin_allowable=m_a,
            remaining_life_yr=life,
            governing_criterion=criterion,
            derated=derating.value if derating is not None else None,
        )

    if not math.isfinite(m):
        return _make(
            Verdict.ESCALATE,
            f"{lbl} could not be evaluated (non-finite margin) — escalate to a "
            "higher assessment level.",
        )

    if screening_pass is None:
        screening_pass = m >= m_a and life > 0.0

    if screening_pass:
        if m < m_a + b.monitor_band:
            return _make(
                Verdict.MONITOR,
                f"Screening ACCEPT; {lbl}={m:.3f} is within {b.monitor_band:.2f} of "
                f"{lbl}a={m_a:.2f} — increased inspection frequency recommended.",
            )
        return _make(
            Verdict.ACCEPT,
            f"Screening ACCEPT ({lbl}={m:.3f} >= {lbl}a={m_a:.2f}).",
        )

    if m < b.derate_floor:
        return _make(
            Verdict.REPLACE,
            f"{lbl}={m:.3f} is below the de-rating floor "
            f"{lbl}_floor={b.derate_floor:.2f}.  Component must be replaced.",
        )
    if m < m_a:
        return _make(
            Verdict.DERATE,
            f"Screening FAIL; {lbl}={m:.3f} in de-rating band "
            f"[{b.derate_floor:.2f}, {m_a:.2f}).  {note}",
        )
    # Margin acceptable but screening failed (e.g. thickness already below
    # t_min): repair if remaining life is short, otherwise operate de-rated.
    if life < b.repair_life_yr:
        return _make(
            Verdict.REPAIR,
            f"Screening FAIL; {lbl} acceptable but remaining life {life:.1f} yr "
            "is short.  Repair recommended.",
        )
    return _make(Verdict.DERATE, f"Screening FAIL; {lbl} acceptable.  {note}")


# ---------------------------------------------------------------------------
# Mooring traffic-light adapter (mooring_resilience.screening)
# ---------------------------------------------------------------------------
_TRAFFIC_LIGHT = {
    "GREEN": Verdict.ACCEPT,
    "AMBER": Verdict.MONITOR,
    "RED": Verdict.REPAIR,
    "ESCALATE": Verdict.ESCALATE,
}


def from_traffic_light(light: str) -> Verdict:
    """Map a mooring GREEN / AMBER / RED / ESCALATE light onto the shared set."""
    try:
        return _TRAFFIC_LIGHT[str(light).upper()]
    except KeyError:
        raise ValueError(
            f"Unknown traffic light {light!r}; expected one of {sorted(_TRAFFIC_LIGHT)}"
        ) from None


# ---------------------------------------------------------------------------
# Legacy pressure-class entry point (inch / psi contract preserved)
# ---------------------------------------------------------------------------
class FFSDecision:
    """Static decision engine for FFS assessment verdicts (pressure class)."""

    @staticmethod
    def decide(
        level1_verdict: str,
        level2_verdict: str,
        rsf: float,
        rsf_a: float,
        t_mm_in: float,
        t_min_in: float,
        corrosion_rate_in_per_yr: float,
        design_pressure_psi: float | None = None,
    ) -> dict:
        """Determine the final FFS action verdict (legacy inch / psi signature).

        Thin wrapper over :func:`decide` for ``asset_class="pressure"``; the
        inputs are unit-tagged internally.  Returns the legacy dict (``verdict``
        is the pressure wording, so ``RE_RATE`` not ``DERATE``) plus ``action``
        and ``asset_class``.
        """
        life = remaining_life(
            Q_(t_mm_in, "inch"), Q_(t_min_in, "inch"),
            Q_(corrosion_rate_in_per_yr, "inch/year"),
        )
        derate = (
            reduced_mawp(Q_(design_pressure_psi, "psi"))
            if design_pressure_psi is not None else None
        )
        d = decide(
            rsf, rsf_a, life, "pressure", derate=derate,
            screening_pass=(level1_verdict == "ACCEPT" and level2_verdict == "ACCEPT"),
        )
        out = d.to_dict()
        out["verdict"] = d.action
        return out

    # ------------------------------------------------------------------
    # Private helpers (kept for compatibility)
    # ------------------------------------------------------------------

    @staticmethod
    def _rerated_mawp(
        design_pressure_psi: float | None, rsf: float, rsf_a: float
    ) -> float | None:
        """RSF-based re-rated pressure, API 579-1 §2.4.2.2 (psi)."""
        if design_pressure_psi is None:
            return None
        d = reduced_mawp(Q_(design_pressure_psi, "psi"))(rsf, rsf_a)
        return None if d is None else float(d.value.to("psi").magnitude)

    @staticmethod
    def _remaining_life(
        t_mm: float, t_min: float, rate: float
    ) -> float:
        """Linear remaining-life projection in years (inch, inch/yr inputs)."""
        return remaining_life(Q_(t_mm, "inch"), Q_(t_min, "inch"), Q_(rate, "inch/year"))
