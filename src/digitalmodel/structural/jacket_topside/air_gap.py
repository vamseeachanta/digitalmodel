# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Fixed-platform subsidence clearance and engineering option screening.

SI units; elevations share a fixed vertical datum, positive upward. The deck
elevation is the original underside elevation before cumulative subsidence.
HAT + surge is the supplied coincident still-water level; crest heights are
relative to it (never absolute crest elevations). Return periods, clearance,
kinematics and coefficients come from the platform's assessment basis.

The horizontal silhouette estimate F = rho * Cd * A * U**2 / 2 is generic drag,
not a qualified API/ISO wave-in-deck procedure or a SIM acceptance verdict.
No licensed thresholds or coefficients are supplied. Slamming, uplift,
impulse, trapped air, shielding and global/local structural response require
a full assessment under the identified code and edition.
"""

from __future__ import annotations

from bisect import bisect_left
from dataclasses import dataclass
from datetime import date
import math

DAYS_PER_YEAR = 365.25
# Numerical cancellation allowance only, not an engineering clearance tolerance.
GEOMETRY_ROUNDOFF_M = 1e-9
SCREENING_LIMITS = (
    "Horizontal quasi-static drag only; no slamming, uplift or impulse.",
    "Caller supplies coincident levels, crest kinematics, solidity and coefficient basis.",
    "Code-specific method qualification and structural acceptance are not established.",
)


def _number(name: str, value: float, minimum: float | None = None) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise TypeError(f"{name} must be a real number")
    if not math.isfinite(value) or (minimum is not None and value < minimum):
        raise ValueError(f"{name} must be finite and at least {minimum}")


def _text(name: str, value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must identify its basis")


def _date(value: date) -> None:
    if type(value) is not date:
        raise TypeError("dates must be calendar dates without time components")


@dataclass(frozen=True)
class AssessmentBasis:
    """Caller-identified assessment route; identification does not certify it."""

    code: str
    edition: str
    reference: str

    def __post_init__(self) -> None:
        if self.code not in {"API RP 2A", "API RP 2SIM", "ISO 19902"}:
            raise ValueError("identify API RP 2A, API RP 2SIM or ISO 19902")
        _text("edition", self.edition)
        _text("reference", self.reference)


@dataclass(frozen=True)
class Survey:
    """Dated cumulative downward movement, in metres, from the original deck."""

    on: date
    cumulative_m: float

    def __post_init__(self) -> None:
        _date(self.on)
        _number("cumulative_m", self.cumulative_m, 0)


@dataclass(frozen=True)
class SubsidenceHistory:
    """Monotonic surveys interpolated by days, then constant-rate extrapolation.

    The forecast rate applies after the last survey. One year is 365.25 days.
    Dates before the first survey are unsupported; uplift is outside scope.
    """

    surveys: tuple[Survey, ...]
    forecast_rate_m_per_year: float

    def __post_init__(self) -> None:
        if not isinstance(self.surveys, tuple) or not self.surveys:
            raise ValueError("provide a nonempty tuple of surveys")
        _number("forecast_rate_m_per_year", self.forecast_rate_m_per_year, 0)
        for survey in self.surveys:
            if not isinstance(survey, Survey):
                raise TypeError("surveys must contain Survey values")
        for earlier, later in zip(self.surveys, self.surveys[1:]):
            if later.on <= earlier.on or later.cumulative_m < earlier.cumulative_m:
                raise ValueError("survey dates must increase and subsidence cannot decrease")

    def loss_at(self, on: date) -> float:
        """Return interpolated or forecast cumulative downward movement in m."""
        _date(on)
        if on < self.surveys[0].on:
            raise ValueError("date precedes the first subsidence survey")
        index = bisect_left([s.on for s in self.surveys], on)
        if index == len(self.surveys):
            last = self.surveys[-1]
            return last.cumulative_m + (
                (on - last.on).days / DAYS_PER_YEAR * self.forecast_rate_m_per_year
            )
        right = self.surveys[index]
        if on == right.on:
            return right.cumulative_m
        left = self.surveys[index - 1]
        fraction = (on - left.on).days / (right.on - left.on).days
        return left.cumulative_m + fraction * (right.cumulative_m - left.cumulative_m)


@dataclass(frozen=True)
class CrestCase:
    """A named return-period case with crest above still water and clearance in m."""

    name: str
    return_period_years: float
    crest_above_still_water_m: float
    required_clearance_m: float

    def __post_init__(self) -> None:
        _text("case name", self.name)
        _number("return_period_years", self.return_period_years, 0)
        if self.return_period_years == 0:
            raise ValueError("return period must be positive")
        _number("crest_above_still_water_m", self.crest_above_still_water_m, 0)
        _number("required_clearance_m", self.required_clearance_m, 0)


@dataclass(frozen=True)
class DesignBasis:
    """Original deck and coincident water levels in one fixed vertical datum.

    Required clearance is the residual separation ABOVE the design crest.
    A surveyed current deck must first be referred back to zero subsidence;
    otherwise cumulative movement would be deducted twice.
    """

    deck_underside_elevation_m: float
    hat_elevation_m: float
    surge_m: float
    vertical_datum: str
    assessment_basis: AssessmentBasis
    crest_cases: tuple[CrestCase, ...]

    def __post_init__(self) -> None:
        _number("deck_underside_elevation_m", self.deck_underside_elevation_m)
        _number("hat_elevation_m", self.hat_elevation_m)
        _number("surge_m", self.surge_m, 0)
        _text("vertical_datum", self.vertical_datum)
        if not isinstance(self.assessment_basis, AssessmentBasis):
            raise TypeError("assessment_basis must identify the code, edition and reference")
        if not isinstance(self.crest_cases, tuple) or not self.crest_cases:
            raise ValueError("provide a nonempty tuple of crest cases")
        if not all(isinstance(c, CrestCase) for c in self.crest_cases):
            raise TypeError("crest_cases must contain CrestCase values")
        if len({c.name for c in self.crest_cases}) != len(self.crest_cases):
            raise ValueError("crest case names must be unique")


@dataclass(frozen=True)
class AirGapSnapshot:
    """Clearances for one date/case; these are geometric comparisons only."""

    on: date
    case_name: str
    return_period_years: float
    deck_elevation_m: float
    still_water_air_gap_m: float
    crest_clearance_m: float
    required_clearance_m: float
    clearance_margin_m: float
    assessment_basis: AssessmentBasis

    @property
    def meets_required_clearance(self) -> bool:
        """Compare with the requirement; allow 1e-9 m numerical round-off."""
        return self.clearance_margin_m >= -GEOMETRY_ROUNDOFF_M

    @property
    def wave_contact(self) -> bool:
        """Flag crest contact, including touching with zero immersed area."""
        return self.crest_clearance_m <= GEOMETRY_ROUNDOFF_M


@dataclass(frozen=True)
class BreachForecast:
    """First daily breach within the history-to-horizon interval.

    A breach is a margin below -1e-9 m (numerical round-off only).
    None means no breach through the horizon, never an unlimited-life claim.
    If already breached at the first survey, the actual onset is unknown.
    """

    case_name: str
    first_breach_date: date | None
    breached_at_history_start: bool
    through_date: date

    @property
    def first_breach_year(self) -> int | None:
        """Return the calendar year of the first covered breach date."""
        return self.first_breach_date.year if self.first_breach_date else None


@dataclass(frozen=True)
class AirGapAssessment:
    """All requested dates and cases, plus daily breach forecasts."""

    snapshots: tuple[AirGapSnapshot, ...]
    breaches: tuple[BreachForecast, ...]


def _snapshot(basis: DesignBasis, history: SubsidenceHistory,
              on: date, case: CrestCase) -> AirGapSnapshot:
    deck = basis.deck_underside_elevation_m - history.loss_at(on)
    still_water_gap = deck - basis.hat_elevation_m - basis.surge_m
    clearance = still_water_gap - case.crest_above_still_water_m
    return AirGapSnapshot(
        on, case.name, case.return_period_years, deck, still_water_gap,
        clearance, case.required_clearance_m, clearance - case.required_clearance_m,
        basis.assessment_basis,
    )


def _first_breach(basis: DesignBasis, history: SubsidenceHistory,
                  horizon: date, case: CrestCase) -> BreachForecast:
    start = history.surveys[0].on
    already = not _snapshot(basis, history, start, case).meets_required_clearance
    if _snapshot(basis, history, horizon, case).meets_required_clearance:
        return BreachForecast(case.name, None, False, horizon)
    # Monotonic settlement allows a daily binary search across survey segments.
    low, high = start.toordinal(), horizon.toordinal()
    while low < high:
        middle = (low + high) // 2
        if _snapshot(basis, history, date.fromordinal(middle), case).meets_required_clearance:
            low = middle + 1
        else:
            high = middle
    return BreachForecast(case.name, date.fromordinal(low), already, horizon)


def assess_air_gap(basis: DesignBasis, history: SubsidenceHistory, as_of: date,
                   future_dates: tuple[date, ...] = ()) -> AirGapAssessment:
    """Evaluate current/future clearances and the first covered daily breach.

    Water levels and return-period crests stay fixed at the supplied basis;
    sea-level change and differential settlement are not modelled.
    """
    history.loss_at(as_of)
    for on in future_dates:
        _date(on)
        if on < as_of:
            raise ValueError("future dates cannot precede the assessment date")
    dates = sorted({as_of, *future_dates})
    return AirGapAssessment(
        tuple(_snapshot(basis, history, on, case)
              for on in dates for case in basis.crest_cases),
        tuple(_first_breach(basis, history, dates[-1], case) for case in basis.crest_cases),
    )


@dataclass(frozen=True)
class Silhouette:
    """User-supplied horizontal drag inputs in SI units.

    Width is normal to flow; deck_depth is the vertical height of the exposed
    deck envelope. Solidity scales its immersed rectangular area. Velocity is
    the nonnegative coincident horizontal wave/current speed at the deck.
    No coefficient or kinematic value is inferred from the code identifier.
    """

    width_m: float
    deck_depth_m: float
    solidity: float
    velocity_m_s: float
    drag_coefficient: float
    density_kg_m3: float
    coefficient_basis: str

    def __post_init__(self) -> None:
        for name in ("width_m", "deck_depth_m", "drag_coefficient", "density_kg_m3"):
            value = getattr(self, name)
            _number(name, value, 0)
            if value == 0:
                raise ValueError(f"{name} must be positive")
        _number("solidity", self.solidity, 0)
        if self.solidity > 1:
            raise ValueError("solidity cannot exceed one")
        _number("velocity_m_s", self.velocity_m_s, 0)
        _text("coefficient_basis", self.coefficient_basis)


@dataclass(frozen=True)
class WaveInDeckScreen:
    """Horizontal drag estimate; a zero estimate does not exclude impact loads."""

    wetted_silhouette_m2: float
    horizontal_force_kn: float
    requires_full_assessment: bool
    assessment_basis: AssessmentBasis
    coefficient_basis: str
    status: str = "screening_only"
    limits: tuple[str, ...] = SCREENING_LIMITS


def screen_wave_in_deck(snapshot: AirGapSnapshot, silhouette: Silhouette) -> WaveInDeckScreen:
    """Estimate horizontal silhouette drag with immersion capped at deck depth."""
    clearance = snapshot.crest_clearance_m
    immersion = (0.0 if abs(clearance) <= GEOMETRY_ROUNDOFF_M
                 else min(silhouette.deck_depth_m, max(0.0, -clearance)))
    area = silhouette.width_m * immersion * silhouette.solidity
    force_kn = (0.5 * silhouette.density_kg_m3 * silhouette.drag_coefficient
                * area * silhouette.velocity_m_s**2 / 1000)
    return WaveInDeckScreen(area, force_kn, snapshot.wave_contact,
                           snapshot.assessment_basis, silhouette.coefficient_basis)


@dataclass(frozen=True)
class RemediationOption:
    """Engineering work needed to develop an option, without cost or NPV."""

    name: str
    required_lift_m: float | None
    checklist: tuple[str, ...]


@dataclass(frozen=True)
class RemediationScreen:
    """Target-life lift geometry and inputs needed for a business case."""

    target_date: date
    governing_case: str
    options: tuple[RemediationOption, ...]
    engineering_inputs_needed: tuple[str, ...] = (
        "Survey datum, subsidence forecast uncertainty and target service end date.",
        "Return-period levels, crest kinematics and site-specific wave-in-deck assessment.",
        "Local and global load paths, jacket/leg/foundation reserve and fatigue life.",
        "Topside weight and centre of gravity, jack reactions and temporary support capacity.",
        "Riser, conductor, piping, cable, access and shutdown interface constraints.",
        "Operating exposure, manning/shut-in criteria and decommissioning constraints.",
    )
    engineering_feasibility_established: bool = False


def screen_remediation(basis: DesignBasis, history: SubsidenceHistory, as_of: date,
                       target_date: date, additional_lift_allowance_m: float = 0.0,
                       ) -> RemediationScreen:
    """Compare three engineering routes; lift restores every supplied target case.

    The lift is a geometric minimum plus an explicit allowance, not a jack
    stroke or feasibility approval. Monotonic settlement makes the target
    date govern the covered life for these fixed water levels.
    """
    _number("additional_lift_allowance_m", additional_lift_allowance_m, 0)
    result = assess_air_gap(basis, history, as_of, (target_date,))
    target = [s for s in result.snapshots if s.on == target_date]
    governing = min(target, key=lambda s: s.clearance_margin_m)
    shortfall = (0.0 if governing.meets_required_clearance
                 else -governing.clearance_margin_m)
    lift = shortfall + additional_lift_allowance_m
    return RemediationScreen(target_date, governing.case_name, (
        RemediationOption("live_with_it", None, (
            "Assess local/global wave-in-deck loads and residual structural capacity.",
            "Define weather, manning and shut-in limits through a full risk assessment.",
            "Monitor subsidence and revisit the design basis and operating limits.",
        )),
        RemediationOption("raise_topside", lift, (
            "Assess jacket, foundations, topside and interface feasibility.",
            "Design and install temporary supports and jacks at the legs.",
            "Isolate and disconnect affected interfaces under an engineered plan.",
            "Lift in engineered small increments with synchronized monitoring.",
            "Install designed leg extensions and connections.",
            "Transfer load to the permanent structure and inspect connections.",
            "Reconnect interfaces and verify clearances, loads and commissioning.",
        )),
        RemediationOption("decommission", None, (
            "Define cessation, isolation, removal and foundation/site obligations.",
            "Develop structural removal and environmental assessment inputs.",
        )),
    ))
