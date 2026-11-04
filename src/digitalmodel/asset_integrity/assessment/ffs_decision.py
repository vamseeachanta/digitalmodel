# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
             to a higher assessment level.  Also issued when an applicability
             flag was raised by a strength method (the defect lies outside
             the method's calibrated range, #1094): no numeric verdict is
             issued, the margin is echoed for reference only and a Level 3 /
             engineering review is required.

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
from dataclasses import dataclass, field, replace
from enum import Enum
from typing import Any, Callable, Mapping, Optional, Union

from digitalmodel.asset_integrity.applicability import Applicability, merge
from digitalmodel.asset_integrity._validation import finite_number
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
    if isinstance(x, bool):
        raise UnitTagError(f"{what} requires numeric evidence, not a boolean")
    if _is_quantity(x):
        if not x.dimensionless:
            raise UnitTagError(f"{what} is a ratio and must be dimensionless, got {x.units}")
        return float(x.to("dimensionless").magnitude)
    if isinstance(x, (int, float)):
        return float(x)
    raise UnitTagError(f"{what} must be a number or dimensionless Quantity, got {type(x).__name__}")


def _years(x: QuantityLike, what: str = "remaining_life_yr") -> float:
    if isinstance(x, bool):
        raise UnitTagError(f"{what} requires numeric evidence, not a boolean")
    if _is_quantity(x):
        return float(_require(x, "[time]", what).to("year").magnitude)
    if isinstance(x, (int, float)):
        return float(x)
    raise UnitTagError(f"{what} must be a number of years or a time Quantity, got {type(x).__name__}")


def remaining_life(t_mm: "ureg.Quantity", t_min: "ureg.Quantity",
                   rate: "ureg.Quantity", *, zero_rate_basis: str | None = None) -> float:
    """Linear remaining-life projection, unit-aware.  Returns years.

    ``float('inf')`` when the rate is zero; ``0.0`` when t_mm <= t_min.
    """
    t_mm = _require(t_mm, "[length]", "t_mm")
    t_min = _require(t_min, "[length]", "t_min")
    rate = _require(rate, "[length]/[time]", "corrosion rate")
    finite_number(t_mm.magnitude, "measured thickness")
    finite_number(t_min.magnitude, "required thickness")
    finite_number(rate.magnitude, "corrosion rate", positive=False)
    if rate.magnitude == 0 and not (isinstance(zero_rate_basis, str) and zero_rate_basis.strip()):
        raise ValueError("Zero corrosion rate requires an explicit qualified basis")
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
    finite_number(limit.magnitude, "limit")

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
    applicability: Applicability = field(default_factory=Applicability)
    remaining_life_basis: str | None = None

    def to_dict(self) -> dict:
        """Legacy-shaped, JSON-friendly payload (``verdict`` is the shared word).

        ``applicability`` echoes the ``{ok, flags, notes}`` validity record
        (all-clear when none was supplied, #1094).
        """
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
            "remaining_life_basis": self.remaining_life_basis,
            "governing_criterion": self.governing_criterion,
            "margin": self.margin,
            "margin_allowable": self.margin_allowable,
            "rsf": self.margin,
            "rsf_a": self.margin_allowable,
            "rerated_mawp_psi": rerated_psi,
            "derated": derated_mag,
            "derated_units": derated_units,
            "applicability": self.applicability.to_dict(),
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
    applicability: Optional[Applicability] = None,
    zero_rate_basis: str | None = None,
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
        applicability: Validity record from the strength methods. Unsupported
            checks escalate while independently established failures retain
            their required action. None means no additional method record.
        zero_rate_basis: Explicit qualification for a zero-rate infinite life.
    """
    policy = _policy(asset_class)
    app = applicability if applicability is not None else Applicability()
    if not isinstance(app, Applicability):
        raise ValueError("applicability must be an explicit validity record")
    app = merge(app)
    b = bands or policy.bands
    m = _dimensionless(margin, "margin")
    m_a = _dimensionless(margin_allowable, "margin_allowable")
    life = _years(remaining_life_yr)
    allowable_unknown = math.isnan(m_a) and not app.ok
    if not allowable_unknown:
        finite_number(m_a, "margin_allowable")
    for name, value in (("monitor_band", b.monitor_band), ("derate_floor", b.derate_floor),
                        ("repair_life_yr", b.repair_life_yr)):
        finite_number(value, name, positive=False)
    if not allowable_unknown and b.derate_floor > m_a:
        raise ValueError("Decision band ordering requires derate_floor <= allowable")
    if life < 0:
        raise ValueError("remaining life must be nonnegative or explicitly unknown")
    if screening_pass is not None and type(screening_pass) is not bool:
        raise ValueError("screening_pass must be a boolean or None")
    life_known = math.isfinite(life) or (
        life == math.inf and isinstance(zero_rate_basis, str) and bool(zero_rate_basis.strip()))
    if life == math.inf and not life_known:
        life = math.nan  # Unqualified infinity is unknown life, never unlimited.
    lbl = policy.margin_label

    derating = (derate(m, m_a) if derate is not None
                and math.isfinite(m) and math.isfinite(m_a) else None)
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
            applicability=app,
            remaining_life_basis=zero_rate_basis if life == math.inf else None,
        )

    # Independent numeric failures retain their required action even if another
    # check is unavailable; missing applicability never improves that action.
    app_note = ""
    if not app.ok:
        app_note = " Other checks are not evaluated: " + "; ".join(app.notes or app.flags)
    if math.isfinite(m) and m < b.derate_floor:
        return _make(Verdict.REPLACE,
                     f"{lbl}={m:.3f} is below the de-rating floor {b.derate_floor:.2f}. "
                     "Component must be replaced." + app_note)
    if math.isfinite(m) and m < m_a:
        return _make(Verdict.DERATE,
                     f"Screening FAIL; {lbl}={m:.3f} < allowable {m_a:.2f}. " + note + app_note)

    # Unsupported criteria with no independently established failure escalate.
    if not app.ok:
        flags = ", ".join(app.flags)
        notes = "; ".join(app.notes)
        return _make(
            Verdict.ESCALATE,
            f"Applicability flag(s) raised [{flags}]: {notes}.  Margin "
            f"{lbl}={m:.3f} is reported for reference only; a Level 3 / "
            "engineering review is required before a fitness verdict.",
        )

    if not math.isfinite(m):
        return _make(
            Verdict.ESCALATE,
            f"{lbl} could not be evaluated (non-finite margin) — escalate to a "
            "higher assessment level.",
        )

    if not life_known:
        return _make(Verdict.ESCALATE,
                     "Remaining life not evaluated: corrosion rate or qualified zero-rate "
                     "basis is missing; establish the life basis before clearance.")

    if screening_pass is None:
        screening_pass = m >= m_a and life > 0.0

    # Clearance requires both numeric strength/life and any supplied screen.
    if screening_pass and m >= m_a and life > 0:
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
        corrosion_rate_in_per_yr: float | None,
        design_pressure_psi: float | None = None,
        applicability: Optional[Applicability] = None,
        corrosion_rate_basis: str | None = None,
    ) -> dict:
        """Determine the final FFS action verdict (legacy inch / psi signature).

        Thin wrapper over :func:`decide` for ``asset_class="pressure"``; the
        inputs are unit-tagged internally.  Returns the legacy dict (``verdict``
        is the pressure wording, so ``RE_RATE`` not ``DERATE``) plus ``action``
        and ``asset_class``.

        Args:
            level1_verdict: ``'ACCEPT'`` or ``'FAIL_LEVEL_1'``.
            level2_verdict: ``'ACCEPT'`` or ``'FAIL_LEVEL_2'``.
            rsf: Computed remaining strength factor from Level 2 engine.
            rsf_a: Allowable remaining strength factor threshold.
            t_mm_in: Minimum measured wall thickness (inches).
            t_min_in: Code-required minimum wall thickness (inches).
            corrosion_rate_in_per_yr: Future corrosion rate (inches/year).
                Use 0.0 for no further degradation.
            design_pressure_psi: Design pressure / MAWP.  When supplied, the
                RSF-based re-rated pressure MAWP_r = MAWP * min(1, RSF/RSFa)
                (API 579-1 §2.4.2.2) is computed and reported on every
                verdict; the RE_RATE criterion quotes it explicitly.
            applicability: validity record from the strength method(s).
                When any flag is raised the verdict is ``ESCALATE`` and the
                numeric tree is bypassed (#1094).  ``None`` means "no flags"
                so existing callers are unaffected.

        Returns:
            dict with keys:
                verdict (str): One of ACCEPT, MONITOR, RE_RATE, REPAIR,
                    REPLACE, ESCALATE (pressure wording).
                action (str): Same as ``verdict`` for the pressure class.
                asset_class (str): ``"pressure"``.
                remaining_life_yr (float): Estimated remaining service life.
                    ``float('inf')`` when corrosion_rate_in_per_yr == 0.
                governing_criterion (str): Human-readable description of the
                    governing assessment criterion.
                rsf / margin (float): Echoed input RSF.
                rsf_a / margin_allowable (float): Echoed input RSFa.
                rerated_mawp_psi (float | None): MAWP_r when
                    design_pressure_psi was supplied, else None.
                applicability (dict): ``{ok, flags, notes}`` echo of the
                    validity record (all-clear when none was supplied).
        """
        if level1_verdict not in {"ACCEPT", "FAIL_LEVEL_1", "NOT_EVALUATED"}:
            raise ValueError("Unsupported Level 1 verdict")
        if level2_verdict not in {"ACCEPT", "FAIL_LEVEL_2", "NOT_EVALUATED"}:
            raise ValueError("Unsupported Level 2 verdict")
        finite_number(t_mm_in, "measured thickness")
        finite_number(t_min_in, "required thickness")
        life = float("nan") if corrosion_rate_in_per_yr is None else remaining_life(
            Q_(t_mm_in, "inch"), Q_(t_min_in, "inch"),
            Q_(corrosion_rate_in_per_yr, "inch/year"),
            zero_rate_basis=corrosion_rate_basis,
        )
        derate = (
            reduced_mawp(Q_(design_pressure_psi, "psi"))
            if design_pressure_psi is not None else None
        )
        d = decide(
            rsf, rsf_a, life, "pressure", derate=derate,
            screening_pass=(level1_verdict == "ACCEPT" and level2_verdict == "ACCEPT"),
            applicability=applicability,
            zero_rate_basis=corrosion_rate_basis,
        )
        if (t_mm_in < t_min_in and level1_verdict == "FAIL_LEVEL_1"
                and d.verdict == Verdict.ESCALATE):
            # A missing independent check cannot erase the measured-wall failure.
            d = replace(d, verdict=Verdict.REPAIR, action="REPAIR",
                        governing_criterion="Measured wall is below required wall; repair or "
                        "qualified detailed assessment is required. " + d.governing_criterion)
        out = d.to_dict()
        out["verdict"] = d.action
        out["level1"] = {"verdict": level1_verdict}
        out["level2"] = {"verdict": level2_verdict}
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
        t_mm: float, t_min: float, rate: float, *, zero_rate_basis: str | None = None
    ) -> float:
        """Linear remaining-life projection in years (inch, inch/yr inputs)."""
        return remaining_life(Q_(t_mm, "inch"), Q_(t_min, "inch"), Q_(rate, "inch/year"),
                              zero_rate_basis=zero_rate_basis)
