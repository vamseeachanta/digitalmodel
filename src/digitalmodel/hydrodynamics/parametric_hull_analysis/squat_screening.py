# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Empirical squat and under-keel-clearance (UKC) screening in SI units.

Sources opened for this implementation:

* [P11] PIANC On Course 142, January 2011, Appendix A, printed pp. 22-23:
  https://pianc-jp.org/news/images/On%20Course%20142.pdf
* [B17] Brodogradnja 68(2), 2017, doi:10.21278/brod68201, section 6,
  equations (4)-(17), Table 1; UKC accounting in equation (1):
  https://brodogradnja.fsb.hr/wp-content/uploads/sites/25/2023/01/No2_1-11.pdf
* [D] Load Line Regulations, section 8(1), density interpolation only:
  https://laws-lois.justice.gc.ca/eng/regulations/SOR-2007-99/section-8.html
* [N] NOAA Tides & Currents glossary, entry 'knot':
  https://tidesandcurrents.noaa.gov/glossary.html

Original empirical publications and PIANC Report 121 were not directly read.
These are calm-water, steady, centred, initially even-keel screens. Bank
offsets, bends, passing ships, propeller effects, acceleration, soft bottoms
and navigation approval are outside scope. An evaluated result establishes
only a model prediction within the stated screening envelope. Bow-only
methods do not establish the unmodelled stern clearance.
"""

from __future__ import annotations

from dataclasses import dataclass, replace
from enum import Enum
import math
from typing import Iterable


PIANC_2011 = "PIANC On Course 142 (2011), Appendix A, pp. 22-23"
BRODOGRADNJA_2017 = "Brodogradnja 68(2) (2017), section 6, Table 1"
# [N], entry 'knot': one international nautical mile (1852 m) per hour.
# The hour contains 60*60 SI seconds (unit definition).
METRES_PER_SECOND_PER_KNOT = 1852 / 3600


class ChannelKind(str, Enum):
    """Physical configuration; width and blockage alone cannot identify a trench."""

    OPEN = "open"
    DREDGED = "dredged"
    CANAL = "canal"


class Formula(str, Enum):
    """Separately identified published empirical formulations."""

    BARRASS_1981 = "barrass_1981"
    ROMISCH_1989 = "romisch_1989"
    HUUSKA_GULIEV = "huuska_guliev"
    ERYUZLU_1994 = "eryuzlu_1994"


class EvaluationStatus(str, Enum):
    """Applicability of a prediction, independent of any clearance verdict."""

    EVALUATED = "EVALUATED"
    NOT_EVALUATED = "NOT_EVALUATED"


def _finite(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite real number")
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(
            f"{name} must be finite" + (" and positive" if positive else "")
        )


def _nonnegative(name: str, value: float) -> None:
    _finite(name, value)
    if value < 0:
        raise ValueError(f"{name} must be nonnegative")


@dataclass(frozen=True, kw_only=True)
class SquatInputs:
    """Required geometry and speed through water, with no engineering defaults.

    Dimensions are metres, speed m/s and supplied local gravity m/s². For open
    water, channel_width_m is a supplied equivalent finite width, not infinity;
    [B17] section 5 discusses effective width. Selection of that width remains
    the caller's responsibility. Dredged widths are equivalent rectangular
    cross-section widths: this model cannot infer side slopes or trench height.
    ChannelKind is required independently of the blockage proxy.
    """

    length_bp_m: float
    beam_m: float
    draught_m: float
    block_coefficient: float
    water_depth_m: float
    channel_width_m: float
    speed_m_s: float
    gravity_m_s2: float
    channel_kind: ChannelKind

    def __post_init__(self) -> None:
        for name in (
            "length_bp_m",
            "beam_m",
            "draught_m",
            "water_depth_m",
            "channel_width_m",
            "gravity_m_s2",
        ):
            _finite(name, getattr(self, name), positive=True)
        _finite("block_coefficient", self.block_coefficient, positive=True)
        if self.block_coefficient > 1:
            raise ValueError("block_coefficient must not exceed unity")
        _nonnegative("speed_m_s", self.speed_m_s)
        if self.beam_m >= self.length_bp_m:
            raise ValueError("beam_m must be less than length_bp_m")
        if self.channel_width_m <= self.beam_m:
            raise ValueError("channel_width_m must exceed beam_m")
        if not isinstance(self.channel_kind, ChannelKind):
            raise ValueError("channel_kind must be an explicit ChannelKind")

    @property
    def depth_ratio(self) -> float:
        """Return water depth divided by static draught."""
        return self.water_depth_m / self.draught_m

    @property
    def depth_froude(self) -> float:
        """Return Fnh=V/sqrt(gh), [P11] Appendix B."""
        return self.speed_m_s / math.sqrt(self.gravity_m_s2 * self.water_depth_m)

    @property
    def blockage_proxy(self) -> float:
        """Return Cb*B*T/(W*h), an explicitly assumed area proxy.

        Neither opened source defines midship area as Cb*B*T. This requested
        proxy is retained as metadata and is never substituted into a formula.
        """
        return self.block_coefficient * self.area_blockage

    @property
    def area_blockage(self) -> float:
        """Return B*T/(W*h), the rectangular approximation of [B17] (2)."""
        return (self.beam_m / self.channel_width_m) * self.depth_ratio**-1

    @property
    def blockage_regime(self) -> str:
        """Return a proxy classification, not a physical channel determination.

        The 0.1 division is adapted from [P11] A1, following (3). Applying
        it to the Cb-weighted proxy is an implementation convention, not a
        published validity criterion. Physical channel_kind is also required.
        """
        return "open" if self.blockage_proxy < 0.1 else "restricted"


@dataclass(frozen=True)
class SuppliedCorrections:
    """Optional externally determined values, with a required reference.

    Restricted critical speed and Huuska width factor depend on geometry
    absent from the core inputs. None means unavailable, never unity. Source
    text is retained as provenance, not authenticated by this module. Synthetic
    values may be used for arithmetic examples only. Supplied corrections must
    correspond to the effective depth and draught after tide/density adjustment.
    """

    critical_speed_m_s: float | None
    huuska_width_factor: float | None
    source: str

    def __post_init__(self) -> None:
        if not isinstance(self.source, str) or not self.source.strip():
            raise ValueError("supplied corrections require a source")
        for name in ("critical_speed_m_s", "huuska_width_factor"):
            value = getattr(self, name)
            if value is not None:
                _finite(name, value, positive=True)


# [B17] Table 1. The HG depth range is also applied to its open-water
# ICORELS-equivalent form as a conservative implementation envelope; Table 1
# itself gives no numerical ICORELS bounds. Bounds are inclusive here.
_ENVELOPES = {
    Formula.BARRASS_1981: (1.1, 1.5, 0.5, 0.9),
    Formula.ROMISCH_1989: (1.19, 2.25, None, None),
    Formula.HUUSKA_GULIEV: (1.1, 2.0, None, None),
    Formula.ERYUZLU_1994: (1.1, 2.5, 0.8, None),
}


def _envelope(method: Formula) -> str:
    low, high, cb_low, cb_high = _ENVELOPES[method]
    text = f"{low} <= h/T <= {high}; Fnh < 1; {BRODOGRADNJA_2017}"
    if cb_low is not None:
        text += f"; Cb >= {cb_low}"
    if cb_high is not None:
        text += f"; Cb <= {cb_high}"
    if method is Formula.ERYUZLU_1994:
        text += "; open or dredged channel, no canal"
    if method is Formula.HUUSKA_GULIEV:
        text += "; open ICORELS equivalent or supplied restricted width factor"
    if method is Formula.ROMISCH_1989:
        text += "; V < Vcr; restricted Vcr must be supplied"
    return text


@dataclass(frozen=True)
class SquatResult:
    """Predicted end displacements, positive downward, and their applicability.

    maximum_m is the largest *predicted* end displacement. For bow-only
    methods it is a bow prediction, with the stern explicitly unavailable.
    trim_change_m is bow minus stern displacement, positive bow down; it is
    a change in draught difference, not an angle or a static trim prediction.
    """

    method: Formula
    status: EvaluationStatus
    reason: str
    maximum_m: float | None
    bow_m: float | None
    stern_m: float | None
    governing_end: str
    trim_change_m: float | None
    applicability: str
    source: str
    inputs: SquatInputs
    supplied_corrections: SuppliedCorrections | None


def _result(
    item: SquatInputs,
    method: Formula,
    corrections: SuppliedCorrections | None,
    *,
    reason: str = "",
    bow: float | None = None,
    stern: float | None = None,
    maximum: float | None = None,
) -> SquatResult:
    end = "unknown"
    trim = None
    if bow is not None and stern is not None:
        maximum = max(bow, stern)
        end = "both" if bow == stern else ("bow" if bow > stern else "stern")
        trim = bow - stern
    elif bow is not None:
        maximum = bow
        end = "bow prediction only"
    if maximum is not None and (not math.isfinite(maximum) or maximum < 0):
        raise ValueError("formula produced a nonfinite or negative displacement")
    status = EvaluationStatus.NOT_EVALUATED if reason else EvaluationStatus.EVALUATED
    return SquatResult(
        method,
        status,
        reason,
        maximum,
        bow,
        stern,
        end,
        trim,
        _envelope(method),
        f"{PIANC_2011}; {BRODOGRADNJA_2017}",
        item,
        corrections,
    )


def _gate(item: SquatInputs, method: Formula) -> str:
    if item.depth_ratio <= 1:
        return "water depth does not exceed draught; grounded static geometry"
    # [B17] section 3: subcritical domain has Fnh < 1.
    if item.depth_froude >= 1:
        return "Fnh >= 1: critical or supercritical depth speed"
    low, high, cb_low, cb_high = _ENVELOPES[method]
    if not low <= item.depth_ratio <= high:
        return f"h/T outside [{low}, {high}]"
    if (cb_low is not None and item.block_coefficient < cb_low) or (
        cb_high is not None and item.block_coefficient > cb_high
    ):
        return "Cb outside the method's published envelope"
    # Conservative guard adapted from [P11] A1 following (3), using the
    # published area definition, not the Cb-weighted proxy. No narrow channel
    # is allowed to inherit open-water correction factors by its label alone.
    if item.channel_kind is ChannelKind.OPEN and item.area_blockage >= 0.1:
        return "open channel label conflicts with area blockage >= 0.1"
    return ""


def barrass_1981(
    item: SquatInputs, corrections: SuppliedCorrections | None = None
) -> SquatResult:
    """[B17] (4)-(5): bow=Cb*(S/(1-S))**(2/3)*Vk**2.08/30.

    S=B*T/(W*h) per [B17] (2), with supplied effective W in open water.
    Range: 0.5<=Cb<=0.9, 1.1<=h/T<=1.5; all three channel kinds (Table 1),
    additionally Fnh<1. This is the 1979/1981 version, not the later B3
    expression in [P11]. Bow squat is predicted; stern and trim are unknown.
    No B3 trim rule is transferred.
    """
    method = Formula.BARRASS_1981
    if reason := _gate(item, method):
        return _result(item, method, corrections, reason=reason)
    s = item.area_blockage
    vk = item.speed_m_s / METRES_PER_SECOND_PER_KNOT
    # [B17] (4)-(5): powers 2/3 and 2.08; denominator 30.
    bow = item.block_coefficient * (s / (1 - s)) ** (2 / 3) * vk**2.08 / 30
    return _result(item, method, corrections, bow=bow)


def romisch_1989(
    item: SquatInputs, corrections: SuppliedCorrections | None = None
) -> SquatResult:
    """[P11] A4 (7)-(11), [B17] (16)-(17), bow and stern squat.

    q=Cv*Cf*.155*sqrt(h/T)*T; Cv=8*r²*((r-.5)^4+.0625), r=V/Vcr.
    Cf_bow=(10*B*Cb/Lpp)², Cf_stern=1. Open Vcr follows [P11] (11);
    restricted Vcr requires a supplied value and reference. Range:
    1.19<=h/T<=2.25 ([B17] Table 1), Fnh<1, V<Vcr. No initial trim.
    """
    method = Formula.ROMISCH_1989
    if reason := _gate(item, method):
        return _result(item, method, corrections, reason=reason)
    critical = corrections.critical_speed_m_s if corrections else None
    if critical is None:
        if item.channel_kind is not ChannelKind.OPEN:
            return _result(
                item,
                method,
                corrections,
                reason="restricted critical speed and source required",
            )
        # [P11] (11), only the unrestricted-channel form is printed there.
        critical = (
            math.sqrt(item.gravity_m_s2 * item.water_depth_m)
            * 0.58
            * (item.water_depth_m * item.length_bp_m / (item.beam_m * item.draught_m))
            ** 0.125
        )
    if item.speed_m_s >= critical:
        return _result(item, method, corrections, reason="speed >= critical speed Vcr")
    r = item.speed_m_s / critical
    # [P11] (8), (9), (10), respectively; all empirical coefficients below.
    cv = 8 * r**2 * ((r - 0.5) ** 4 + 0.0625)
    cf_bow = (10 * item.beam_m * item.block_coefficient / item.length_bp_m) ** 2
    stern = cv * 0.155 * math.sqrt(item.depth_ratio) * item.draught_m
    return _result(item, method, corrections, bow=stern * cf_bow, stern=stern)


def huuska_guliev(
    item: SquatInputs, corrections: SuppliedCorrections | None = None
) -> SquatResult:
    """[P11] A3 (6): bow=2.4*Cb*B*T/Lpp*Fnh²/sqrt(1-Fnh²)*Ks.

    Open Ks=1 is the ICORELS (1980) equivalent. Restricted Ks requires an
    externally determined factor and source: trench height/side slopes cannot
    be inferred from width. Range: 1.1<=h/T<=2 ([B17] Table 1), Fnh<1.
    Applying the HG depth range to the open equivalent is conservative policy;
    Table 1 itself supplies no numerical ICORELS bounds. Stern is unknown.
    """
    method = Formula.HUUSKA_GULIEV
    if reason := _gate(item, method):
        return _result(item, method, corrections, reason=reason)
    if item.channel_kind is ChannelKind.OPEN:
        ks = 1  # [P11] A3, text following (6).
    elif corrections is None or corrections.huuska_width_factor is None:
        return _result(
            item,
            method,
            corrections,
            reason="restricted width factor Ks and source required",
        )
    else:
        ks = corrections.huuska_width_factor
    f2 = item.depth_froude**2
    # [P11] (6): coefficient 2.4, without an assumed reduction for full hulls.
    bow = (
        2.4
        * item.block_coefficient
        * item.beam_m
        * item.draught_m
        / item.length_bp_m
        * f2
        / math.sqrt(1 - f2)
        * ks
    )
    return _result(item, method, corrections, bow=bow)


def eryuzlu_1994(
    item: SquatInputs, corrections: SuppliedCorrections | None = None
) -> SquatResult:
    """[P11] A2 (5): bow=.298*h²/T*(V/sqrt(gT))**2.289*(h/T)**-2.972*Kb.

    [B17] (8): Kb=3.1/sqrt(W/B) for W/B<9.61, otherwise unity. The
    exponent and speed units in [B17] (7) conflict with [P11]; use [P11]'s
    visually checked 2.289 and m/s. Range: Cb>=.8, 1.1<=h/T<=2.5;
    open and dredged, no canal ([B17] Table 1), Fnh<1. Bulbous-bow cargo
    hull applicability must be confirmed by the caller. Stern is unknown.
    """
    method = Formula.ERYUZLU_1994
    if reason := _gate(item, method):
        return _result(item, method, corrections, reason=reason)
    if item.channel_kind is ChannelKind.CANAL:
        return _result(item, method, corrections, reason="canal excluded by Table 1")
    ratio = item.channel_width_m / item.beam_m
    # [B17] (8); open channels use Kb=1 as stated in [P11] A2.
    kb = (
        1
        if item.channel_kind is ChannelKind.OPEN or ratio >= 9.61
        else 3.1 / math.sqrt(ratio)
    )
    # [P11] (5): .298, 2.289 and -2.972.
    bow = (
        0.298
        * item.water_depth_m**2
        / item.draught_m
        * (item.speed_m_s / math.sqrt(item.gravity_m_s2 * item.draught_m)) ** 2.289
        * item.depth_ratio**-2.972
        * kb
    )
    return _result(item, method, corrections, bow=bow)


def evaluate(
    item: SquatInputs, method: Formula, corrections: SuppliedCorrections | None = None
) -> SquatResult:
    """Evaluate one explicitly selected method; unknown identifiers are errors."""
    method = Formula(method)
    return {
        Formula.BARRASS_1981: barrass_1981,
        Formula.ROMISCH_1989: romisch_1989,
        Formula.HUUSKA_GULIEV: huuska_guliev,
        Formula.ERYUZLU_1994: eryuzlu_1994,
    }[method](item, corrections)


def density_draught_correction(
    water_density_kg_m3: float, fresh_water_allowance_m: float
) -> float:
    """Interpolate a supplied certified allowance from a salt-water reference.

    [D], section 8(1): FWA*(1.025-SG)/.025 for SG between 1 and 1.025.
    Equivalently use densities 1000 and 1025 kg/m³, without a temperature or
    salinity model. Positive output increases draught. This helper does not
    determine FWA from Cb or claim load-line compliance. It is inappropriate
    when the supplied reference draught is already corrected for dock water.
    """
    _finite("water_density_kg_m3", water_density_kg_m3)
    _nonnegative("fresh_water_allowance_m", fresh_water_allowance_m)
    # [D] section 8(1): SG 1.000 to 1.025, converted to kg/m³.
    if not 1000 <= water_density_kg_m3 <= 1025:
        raise ValueError("density outside the fresh/salt interpolation range")
    return fresh_water_allowance_m * (1025 - water_density_kg_m3) / 25


@dataclass(frozen=True, kw_only=True)
class UKCBudget:
    """Required metre allowances and criterion, without an assumed safety margin.

    Tide is positive above the depth datum. Density correction is positive
    for increased draught. Wave allowance is a supplied downward excursion,
    not significant wave height; no RAO or probability model is assumed.
    Heel allowance is the supplied additional immersed depth. Those allowances
    must avoid double counting in any combined motion assessment.
    """

    wave_allowance_m: float
    tide_m: float
    density_correction_m: float
    heel_allowance_m: float
    minimum_ukc_m: float

    def __post_init__(self) -> None:
        _finite("tide_m", self.tide_m)
        _finite("density_correction_m", self.density_correction_m)
        for name in ("wave_allowance_m", "heel_allowance_m", "minimum_ukc_m"):
            _nonnegative(name, getattr(self, name))


@dataclass(frozen=True)
class UKCRow:
    """One explicit budget row; None means the model could not assess the row."""

    speed_m_s: float
    depth_at_datum_m: float
    tide_m: float
    static_draught_m: float
    density_correction_m: float
    adjusted_draught_m: float
    wave_allowance_m: float
    heel_allowance_m: float
    minimum_ukc_m: float
    squat: SquatResult
    net_ukc_m: float | None
    margin_m: float | None
    meets_criterion: bool | None
    scope: str = "Selected model and supplied allowances only; navigation NOT_EVALUATED"


def ukc_table(
    item: SquatInputs,
    speeds_m_s: Iterable[float],
    method: Formula,
    budget: UKCBudget,
    corrections: SuppliedCorrections | None = None,
) -> tuple[UKCRow, ...]:
    """Compute [B17] (1) budgets, reevaluating squat at tide and density geometry.

    UKC=h+tide-(T+density correction)-squat-wave allowance-heel allowance.
    Negative residuals are retained. The minimum is a caller-supplied criterion;
    passing it is not an operational clearance approval. Bow-only predictions
    retain the unmodelled stern limitation of SquatResult.
    """
    rows = []
    method = Formula(method)
    for speed in speeds_m_s:
        effective = replace(
            item,
            speed_m_s=speed,
            water_depth_m=item.water_depth_m + budget.tide_m,
            draught_m=item.draught_m + budget.density_correction_m,
        )
        result = evaluate(effective, method, corrections)
        net = margin = meets = None
        if result.status is EvaluationStatus.EVALUATED:
            if result.maximum_m is None:
                raise RuntimeError("evaluated squat has no predicted displacement")
            net = (
                effective.water_depth_m
                - effective.draught_m
                - result.maximum_m
                - budget.wave_allowance_m
                - budget.heel_allowance_m
            )
            margin = net - budget.minimum_ukc_m
            meets = margin >= 0
        rows.append(
            UKCRow(
                speed,
                item.water_depth_m,
                budget.tide_m,
                item.draught_m,
                budget.density_correction_m,
                effective.draught_m,
                budget.wave_allowance_m,
                budget.heel_allowance_m,
                budget.minimum_ukc_m,
                result,
                net,
                margin,
                meets,
            )
        )
    return tuple(rows)


@dataclass(frozen=True)
class SpeedLimit:
    """Maximum screened speed within the supplied cap and the selected model."""

    status: EvaluationStatus
    speed_m_s: float | None
    limited_by: str
    reason: str
    tolerance_m_s: float
    speed_cap_m_s: float


def maximum_speed(
    item: SquatInputs,
    method: Formula,
    budget: UKCBudget,
    speed_cap_m_s: float,
    tolerance_m_s: float,
    corrections: SuppliedCorrections | None = None,
) -> SpeedLimit:
    """Bisect a monotonic model budget, returning the passing lower bracket.

    The speed cap and numerical tolerance are required. An inapplicable cap
    returns NOT_EVALUATED; no interval crosses a formula's critical speed.
    A passing cap is labelled as a cap, not a discovered clearance boundary.
    The speed is not permission to navigate; see UKCRow.scope.
    """
    _nonnegative("speed_cap_m_s", speed_cap_m_s)
    _finite("tolerance_m_s", tolerance_m_s, positive=True)

    def row(speed: float) -> UKCRow:
        return ukc_table(item, [speed], method, budget, corrections)[0]

    zero, cap = row(0), row(speed_cap_m_s)
    if zero.meets_criterion is None or cap.meets_criterion is None:
        reason = zero.squat.reason if zero.meets_criterion is None else cap.squat.reason
        return SpeedLimit(
            EvaluationStatus.NOT_EVALUATED,
            None,
            "applicability",
            reason,
            tolerance_m_s,
            speed_cap_m_s,
        )
    if not zero.meets_criterion:
        return SpeedLimit(
            EvaluationStatus.EVALUATED,
            None,
            "no feasible speed",
            "criterion fails at zero speed",
            tolerance_m_s,
            speed_cap_m_s,
        )
    if cap.meets_criterion:
        return SpeedLimit(
            EvaluationStatus.EVALUATED,
            speed_cap_m_s,
            "supplied cap",
            "criterion passes through supplied cap",
            tolerance_m_s,
            speed_cap_m_s,
        )
    lower, upper = 0.0, speed_cap_m_s
    while upper - lower > tolerance_m_s:
        middle = (lower + upper) / 2  # Numerical bisection, no empirical coefficient.
        if middle == lower or middle == upper:
            return SpeedLimit(
                EvaluationStatus.NOT_EVALUATED,
                None,
                "precision",
                "requested tolerance below floating-point resolution",
                tolerance_m_s,
                speed_cap_m_s,
            )
        if row(middle).meets_criterion:
            lower = middle
        else:
            upper = middle
    return SpeedLimit(
        EvaluationStatus.EVALUATED,
        lower,
        "criterion",
        "passing lower bracket; selected model only",
        tolerance_m_s,
        speed_cap_m_s,
    )


@dataclass(frozen=True)
class FormulaComparison:
    """Side-by-side predictions; spread is unavailable if any method is missing."""

    results: tuple[SquatResult, ...]
    all_evaluated: bool
    spread_m: float | None


def compare_formulas(
    item: SquatInputs, corrections: SuppliedCorrections | None = None
) -> FormulaComparison:
    """Report every method and the max-minus-min spread; do not rank methods.

    Bow-only and maximum-end predictions differ in scope. Spread describes
    disagreement of model outputs, not an uncertainty bound or confidence
    interval. No missing prediction is silently dropped from the comparison.
    """
    results = tuple(evaluate(item, method, corrections) for method in Formula)
    complete = all(r.status is EvaluationStatus.EVALUATED for r in results)
    spread = None
    if complete:
        values = []
        for result in results:
            if result.maximum_m is None:
                raise RuntimeError("evaluated squat has no predicted displacement")
            values.append(result.maximum_m)
        spread = max(values) - min(values)
    return FormulaComparison(results, complete, spread)
