# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""CPT/CPTU interpretation in SI units, with explicit unavailable results.

Engineering equations and their sources are indexed in
``docs/domains/geotechnical/cpt.md``. No cone factor or drainage calibration
is inferred from a soil label. These screening correlations need site calibration.
"""

from __future__ import annotations

from dataclasses import dataclass
import json
from math import hypot, isfinite, log10, sqrt
from numbers import Real
from typing import Iterable, Sequence

from .database_schema import GRADES, LICENCES, ParameterRow, Source


EVALUATED = "EVALUATED"
NOT_EVALUATED = "NOT_EVALUATED"
# G22: Guide to Cone Penetration Testing, seventh edition (2022), pp. 18, 31-32.
GUIDE_SOURCE = "Guide to Cone Penetration Testing, seventh edition (2022)"


def _number(name: str, value: float, *, minimum: float | None = None) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite number")
    try:
        result = float(value)
    except (OverflowError, ValueError) as error:
        raise ValueError(f"{name} must be finite") from error
    if not isfinite(result) or (minimum is not None and result < minimum):
        raise ValueError(f"{name} must be finite and >= {minimum}")
    return result


def _array(
    name: str,
    values: Iterable[float],
    *,
    length: int | None = None,
    minimum: float | None = None,
) -> tuple[float, ...]:
    try:
        result = tuple(_number(name, v, minimum=minimum) for v in values)
    except TypeError as error:
        raise ValueError(f"{name} must be an array") from error
    if not result or (length is not None and len(result) != length):
        raise ValueError(f"{name} must be nonempty with matching length")
    return result


@dataclass(frozen=True, init=False)
class CPTData:
    """Detached measurements; depth in m and stored pressures in Pa.

    Supply one explicit pressure unit (Pa, kPa or MPa) for qc, fs and u2.
    The calibrated net cone area ratio is required. Missing u2 may be specified
    per row or for the whole sounding; corrected resistance then stays unavailable.
    Negative pore pressures are valid. Depths must increase strictly from zero
    or a positive depth. Unit labels cannot detect incorrectly labelled numbers.
    """

    depth_m: tuple[float, ...]
    qc_pa: tuple[float, ...]
    fs_pa: tuple[float, ...]
    u2_pa: tuple[float | None, ...]
    area_ratio: float

    def __init__(
        self,
        depth_m: Iterable[float],
        qc: Iterable[float],
        fs: Iterable[float],
        *,
        area_ratio: float,
        pressure_unit: str,
        u2: Iterable[float | None] | None = None,
    ) -> None:
        """Validate and detach measurements, converting explicit pressure units."""
        # SI prefix definitions, not empirical coefficients.
        factors = {"Pa": 1, "kPa": 1000, "MPa": 1000000}
        if not isinstance(pressure_unit, str) or pressure_unit not in factors:
            raise ValueError("pressure_unit must be Pa, kPa or MPa")
        factor = factors[pressure_unit]
        depths = _array("depth_m", depth_m, minimum=0)
        if any(b <= a for a, b in zip(depths, depths[1:])):
            raise ValueError("depth_m must increase strictly")
        ratio = _number("area_ratio", area_ratio, minimum=0)
        # Area ratio is a ratio of nonnegative areas, bounded by the cone area.
        if ratio > 1:
            raise ValueError("area_ratio must be <= 1")
        size = len(depths)
        cone = _array("qc", qc, length=size, minimum=0)
        sleeve = _array("fs", fs, length=size, minimum=0)
        try:
            pores = tuple(u2) if u2 is not None else (None,) * size
        except TypeError as error:
            raise ValueError("u2 must be an array or None") from error
        if len(pores) != size:
            raise ValueError("u2 must have matching length")
        object.__setattr__(self, "depth_m", depths)
        object.__setattr__(
            self, "qc_pa", tuple(_number("qc in Pa", q * factor) for q in cone)
        )
        object.__setattr__(
            self, "fs_pa", tuple(_number("fs in Pa", f * factor) for f in sleeve)
        )
        object.__setattr__(
            self,
            "u2_pa",
            tuple(
                None if p is None else _number("u2 in Pa", _number("u2", p) * factor)
                for p in pores
            ),
        )
        object.__setattr__(self, "area_ratio", ratio)


@dataclass(frozen=True)
class CorrectedCPT:
    """One observation with applied stress basis; ratios are dimensionless."""

    depth_m: float
    qc_pa: float
    fs_pa: float
    u2_pa: float | None
    area_ratio: float
    total_stress_pa: float
    equilibrium_pore_pressure_pa: float
    effective_stress_pa: float
    qt_pa: float | None
    qn_pa: float | None
    rf_percent: float | None
    bq: float | None
    status: str
    reason: str


def correct_cpt(
    data: CPTData,
    *,
    total_stress_pa: Iterable[float],
    equilibrium_pore_pressure_pa: Iterable[float],
) -> tuple[CorrectedCPT, ...]:
    """Apply the area correction and stress basis without estimating stresses.

    G22 pp. 18, 31-32: qt=qc+u2(1-a); qn=qt-sigma_v; Rf=100 fs/qt;
    Bq=(u2-u0)/qn. Nonpositive denominators retain measured values and a reason.
    Percent is the exact dimensionless ratio multiplied by 100.
    """
    size = len(data.depth_m)
    stresses = _array("total_stress_pa", total_stress_pa, length=size, minimum=0)
    waters = _array(
        "equilibrium_pore_pressure_pa", equilibrium_pore_pressure_pa, length=size
    )
    rows = []
    for z, qc, fs, u2, total, water in zip(
        data.depth_m, data.qc_pa, data.fs_pa, data.u2_pa, stresses, waters
    ):
        effective = _number("effective stress", total - water)
        qt = qn = rf = bq = None
        reason = "u2 is missing; corrected resistance and Bq unavailable"
        if u2 is not None:
            qt = _number("qt", qc + u2 * (1 - data.area_ratio))
            qn = _number("qn", qt - total)
            rf = _number("Rf", 100 * (fs / qt)) if qt > 0 else None
            bq = _number("Bq", (u2 - water) / qn) if qn > 0 else None
            reason = "" if qt > 0 and qn > 0 else "qt or qn is nonpositive"
        rows.append(
            CorrectedCPT(
                z,
                qc,
                fs,
                u2,
                data.area_ratio,
                total,
                water,
                effective,
                qt,
                qn,
                rf,
                bq,
                NOT_EVALUATED if reason else EVALUATED,
                reason,
            )
        )
    return tuple(rows)


def ic_zone(ic: float) -> str:
    """Return an approximate Ic band, assigning equality to the finer band.

    R09, Interpretation of cone penetration tests — a unified approach (2009),
    fig. 2 p. 1342: boundaries 1.31, 2.05, 2.60, 2.95, 3.60. Labels are from
    G22 fig. 23 p. 30. Ic alone cannot resolve zones 1, 8 or 9 (G22 p. 32).
    """
    value = _number("Ic", ic, minimum=0)
    bands = (
        (1.31, "7: gravelly sand to dense sand"),
        (2.05, "6: sands (clean sand to silty sand)"),
        (2.60, "5: sand mixtures (silty sand to sandy silt)"),
        (2.95, "4: silt mixtures (clayey silt to silty clay)"),
        (3.60, "3: clays (silty clay to clay)"),
    )
    for limit, label in bands:
        if value < limit:
            return label
    return "2: organic soils (clay)"


@dataclass(frozen=True)
class NormalisedCPT:
    """One stress-normalised observation; unavailable values remain None."""

    corrected: CorrectedCPT
    qtn: float | None
    fr_percent: float | None
    ic: float | None
    stress_exponent: float | None
    zone: str | None
    iterations: int
    method: str
    atmospheric_pressure_pa: float
    status: str
    reason: str


def _normalise_row(
    row: CorrectedCPT, method: str, pa: float, tolerance: float, max_iterations: int
) -> NormalisedCPT:
    def unavailable(reason: str, iterations: int = 0) -> NormalisedCPT:
        return NormalisedCPT(
            row,
            None,
            None,
            None,
            None,
            None,
            iterations,
            method,
            pa,
            NOT_EVALUATED,
            reason,
        )

    if row.qt_pa is None or row.qn_pa is None:
        return unavailable("corrected resistance unavailable; " + row.reason)
    if row.qn_pa <= 0 or row.fs_pa <= 0 or row.effective_stress_pa <= 0:
        return unavailable(
            "normalisation requires positive qn, fs and effective stress"
        )
    # R09 eqs. 5-7: base-10 logs; Fr is percent; n starts at the clay value 1.
    # Log-space arithmetic prevents intermediate stress-ratio underflow.
    try:
        log_fr = log10(row.fs_pa) - log10(row.qn_pa) + 2  # percent: log10(100)=2
        fr = 10**log_fr
        n = 1.0
        for count in range(1, max_iterations + 1):
            log_q = (
                log10(row.qn_pa)
                - log10(pa)
                + n * (log10(pa) - log10(row.effective_stress_pa))
            )
            ic = hypot(3.47 - log_q, log_fr + 1.22)  # R09 eq. 5
            n_next = min(1.0, 0.381 * ic + 0.05 * (row.effective_stress_pa / pa) - 0.15)
            if method == "1990" or abs(n_next - n) <= tolerance:
                qtn = 10**log_q
                if not all(isfinite(v) and v > 0 for v in (qtn, fr)):
                    return unavailable("normalised ratios outside numeric range", count)
                return NormalisedCPT(
                    row, qtn, fr, ic, n, ic_zone(ic), count, method, pa, EVALUATED, ""
                )
            n = n_next
    except (OverflowError, ValueError):
        return unavailable("normalised ratios outside numeric range")
    return unavailable("stress exponent iteration did not converge", max_iterations)


def normalise_cpt(
    rows: Iterable[CorrectedCPT],
    *,
    method: str = "2009",
    atmospheric_pressure_pa: float = 100000,
    iteration_tolerance: float = 1e-10,
    max_iterations: int = 200,
) -> tuple[NormalisedCPT, ...]:
    """Compute Qtn, Fr and Ic using R09 equations 5-7, or fixed n=1 (1990).

    Reference pressure 100 kPa is printed in G22 fig. 22 p. 29. The iteration
    tolerance and iteration budget are software controls, not engineering limits.
    Failure to converge returns NOT_EVALUATED, with no usable zone or index.
    """
    if method not in ("1990", "2009"):
        raise ValueError("method must be 1990 or 2009")
    pa = _number("atmospheric_pressure_pa", atmospheric_pressure_pa, minimum=0)
    tol = _number("iteration_tolerance", iteration_tolerance, minimum=0)
    if pa == 0 or tol == 0:
        raise ValueError("reference pressure and iteration tolerance must be positive")
    if (
        isinstance(max_iterations, bool)
        or not isinstance(max_iterations, int)
        or max_iterations < 1
    ):
        raise ValueError("max_iterations must be a positive integer")
    return tuple(_normalise_row(row, method, pa, tol, max_iterations) for row in rows)


def _text(name: str, value: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be a nonempty string")
    return value


@dataclass(frozen=True)
class DrainageBands:
    """Required source-specific limits, with no default numerical bands.

    The lower boundary is included in DRAINED; the upper in UNDRAINED.
    The basis must identify vertical cv or horizontal ch; they are not interchangeable.
    A citation and locator are mandatory, but their contents require external review.
    """

    drained_max: float
    undrained_min: float
    consolidation_basis: str
    citation: str
    locator: str

    def __post_init__(self) -> None:
        """Validate the supplied boundaries and their calibration provenance."""
        lo = _number("drained_max", self.drained_max, minimum=0)
        hi = _number("undrained_min", self.undrained_min, minimum=0)
        if not 0 < lo < hi:
            raise ValueError("bands require 0 < drained_max < undrained_min")
        if self.consolidation_basis not in ("vertical", "horizontal"):
            raise ValueError("consolidation_basis must be vertical or horizontal")
        _text("citation", self.citation)
        _text("locator", self.locator)


@dataclass(frozen=True)
class DrainageAssessment:
    """Velocity-based screening, retaining the calibration and missing-data reason."""

    normalised_velocity: float | None
    regime: str
    status: str
    reason: str
    consolidation_basis: str | None
    citation: str
    locator: str
    corrected: CorrectedCPT
    penetration_rate_m_s: float | None
    cone_diameter_m: float | None
    consolidation_coefficient_m2_s: float | None
    drained_max: float | None
    undrained_min: float | None


def assess_drainage(
    row: CorrectedCPT,
    *,
    penetration_rate_m_s: float | None,
    cone_diameter_m: float | None,
    consolidation_coefficient_m2_s: float | None,
    consolidation_basis: str | None,
    bands: DrainageBands | None = None,
) -> DrainageAssessment:
    """Evaluate V=v*d/c, requiring source-specific bands to name a regime.

    This dimensionless velocity is the requested consolidation scaling, associated
    with the 1994 penetration-rate work and the 2012 partial-consolidation paper
    (DOI 10.1061/(ASCE)GT.1943-5606.0000646). Their numerical bands and fit
    constants were not readable from an open copy; none are embedded here.
    Zero velocity is a mathematical limiting case, not a penetrating CPT test.
    """
    inputs = (penetration_rate_m_s, cone_diameter_m, consolidation_coefficient_m2_s)
    values = tuple(
        None if v is None else _number(name, v, minimum=0)
        for name, v in zip(("rate", "diameter", "consolidation coefficient"), inputs)
    )
    rate, diameter, coefficient = values
    if diameter == 0 or coefficient == 0:
        raise ValueError("diameter and consolidation coefficient must be positive")
    if consolidation_basis not in (None, "vertical", "horizontal"):
        raise ValueError("consolidation_basis must be vertical or horizontal")
    citation = bands.citation if bands else ""
    locator = bands.locator if bands else ""
    metadata = (
        row,
        rate,
        diameter,
        coefficient,
        bands.drained_max if bands else None,
        bands.undrained_min if bands else None,
    )

    def unavailable(reason: str, velocity: float | None = None) -> DrainageAssessment:
        return DrainageAssessment(
            velocity,
            NOT_EVALUATED,
            NOT_EVALUATED,
            reason,
            consolidation_basis,
            citation,
            locator,
            *metadata,
        )

    if row.u2_pa is None:
        return unavailable("u2 is missing; CPTU drainage check NOT_EVALUATED")
    if (
        rate is None
        or diameter is None
        or coefficient is None
        or consolidation_basis is None
    ):
        return unavailable(
            "rate, diameter, consolidation coefficient and basis are required"
        )
    # Dimensional identity: (m/s)*m/(m^2/s) is dimensionless; no fitted coefficient.
    try:
        velocity = rate * diameter / coefficient
        if not isfinite(velocity) or (rate > 0 and velocity == 0):
            return unavailable("normalised velocity outside numeric range")
    except OverflowError:
        return unavailable("normalised velocity outside numeric range")
    if bands is None:
        return unavailable("published drainage bands are required inputs", velocity)
    if consolidation_basis != bands.consolidation_basis:
        return unavailable(
            "consolidation basis differs from the supplied bands", velocity
        )
    regime = "PARTIALLY_DRAINED"
    if velocity <= bands.drained_max:
        regime = "DRAINED"
    elif velocity >= bands.undrained_min:
        regime = "UNDRAINED"
    return DrainageAssessment(
        velocity,
        regime,
        EVALUATED,
        "",
        consolidation_basis,
        citation,
        locator,
        *metadata,
    )


@dataclass(frozen=True)
class DrainageCorrection:
    """Measured resistance retained separately from an unavailable corrected value."""

    measured_resistance_pa: float
    corrected_resistance_pa: float | None
    status: str
    reason: str


def correct_partial_drainage(
    resistance_pa: float, assessment: DrainageAssessment
) -> DrainageCorrection:
    """Expose the unresolved backbone correction without fabricating a fit.

    The 2012 partial-consolidation paper was located but its fit was not readable.
    A regime flag alone cannot identify a correction, even at a limiting velocity.
    """
    measured = _number("resistance_pa", resistance_pa, minimum=0)
    reason = "published backbone fit and its calibration are unavailable"
    if assessment.status != EVALUATED:
        reason += "; " + assessment.reason
    return DrainageCorrection(measured, None, NOT_EVALUATED, reason)


@dataclass(frozen=True)
class Estimate:
    """A conditional correlation estimate, with method and validity diagnostics."""

    value: float | None
    unit: str
    status: str
    reason: str
    method: str


def _boolean(name: str, value: bool) -> None:
    if not isinstance(value, bool):
        raise ValueError(f"{name} must be an explicit boolean")


def undrained_strength(row: CorrectedCPT, *, nkt: float, undrained: bool) -> Estimate:
    """Apply Su=qn/Nkt only with an explicit undrained interpretation basis.

    G22 p. 41 gives the equation and typical Nkt range 10-18. That range is
    informational only. Nkt is required and must be calibrated independently;
    neither the typical range nor an Ic zone supplies a cone factor.
    """
    factor = _number("Nkt", nkt, minimum=0)
    if factor == 0:
        raise ValueError("Nkt must be positive")
    _boolean("undrained", undrained)
    method = f"Su=qn/Nkt; Nkt={factor:g}; undrained={undrained}; {GUIDE_SOURCE}, p. 41"
    note = (
        "Typical Nkt is 10 to 18 (informational only); no drainage correction applied"
    )
    if not undrained:
        return Estimate(
            None, "Pa", NOT_EVALUATED, "undrained basis not confirmed", method
        )
    if row.qn_pa is None or row.qn_pa <= 0:
        return Estimate(None, "Pa", NOT_EVALUATED, "positive qn is unavailable", method)
    value = row.qn_pa / factor
    if not isfinite(value) or value <= 0:
        return Estimate(
            None, "Pa", NOT_EVALUATED, "strength outside numeric range", method
        )
    return Estimate(value, "Pa", EVALUATED, note, method)


@dataclass(frozen=True)
class SandConditions:
    """Required applicability assertions; none are inferred from CPT alone.

    G22 pp. 48-49, 53: the simplified correlations concern young, clean,
    uncemented quartz sands, with rounded grains and medium compressibility.
    Dr also requires Ic<1.6. Drained penetration must be established separately.
    """

    clean: bool
    quartz: bool
    uncemented: bool
    unaged: bool
    rounded: bool
    drained: bool
    medium_compressibility: bool

    def __post_init__(self) -> None:
        """Require explicit Boolean applicability assertions."""
        for name, value in vars(self).items():
            _boolean(name, value)


@dataclass(frozen=True)
class SandState:
    """Relative density as a fraction and peak friction angle in degrees."""

    relative_density: Estimate
    friction_angle: Estimate


def sand_state(point: NormalisedCPT, *, conditions: SandConditions) -> SandState:
    """Apply the explicitly qualified simplified clean-sand correlations.

    G22 p. 49: Dr=sqrt(Qtn/350), for young uncemented clean sands, Ic<1.6.
    G22 p. 53: phi'=17.6+11 log10(Qtn), for clean rounded uncemented quartz.
    The source supplies no universal numeric stress validity interval for phi'.
    Results outside physical Dr or angle bounds are unavailable, never clipped.
    """
    dr_method = f"Dr=sqrt(Qtn/350); {GUIDE_SOURCE}, p. 49"
    phi_method = f"phi=17.6+11 log10(Qtn); {GUIDE_SOURCE}, p. 53"
    applicability = "; " + ", ".join(
        f"{key}={value}" for key, value in vars(conditions).items()
    )
    dr_method += applicability
    phi_method += applicability

    def unavailable(reason: str) -> SandState:
        return SandState(
            Estimate(None, "-", NOT_EVALUATED, reason, dr_method),
            Estimate(None, "degrees", NOT_EVALUATED, reason, phi_method),
        )

    if point.status != EVALUATED or point.qtn is None or point.ic is None:
        return unavailable("normalised interpretation unavailable")
    missing = [name for name, value in vars(conditions).items() if not value]
    if missing:
        return unavailable("sand applicability not confirmed: " + ", ".join(missing))
    qtn = _number("Qtn", point.qtn, minimum=0)
    if qtn == 0:
        return unavailable("Qtn must be positive")
    density = sqrt(qtn / 350)  # G22 p. 49, simple young clean sand equation.
    phi = 17.6 + 11 * log10(qtn)  # G22 p. 53, clean rounded quartz equation.
    dr_reason = ""
    if point.ic >= 1.6:  # G22 p. 49 states Ic<1.6 for this simplified equation.
        dr_reason = "Ic is outside the simple clean-sand range (Ic<1.6)"
    elif not 0 < density <= 1:  # Relative density definition: G22 p. 48.
        dr_reason = "relative density outside the physical fraction interval"
    # A friction angle has physical limits 0 and 90 degrees; these are not fit limits.
    phi_reason = "" if 0 < phi < 90 else "friction angle outside physical bounds"
    return SandState(
        Estimate(
            None if dr_reason else density,
            "-",
            NOT_EVALUATED if dr_reason else EVALUATED,
            dr_reason,
            dr_method,
        ),
        Estimate(
            None if phi_reason else phi,
            "degrees",
            NOT_EVALUATED if phi_reason else EVALUATED,
            phi_reason,
            phi_method,
        ),
    )


@dataclass(frozen=True)
class ProfileProvenance:
    """Required observation provenance and reuse decisions for database export.

    The source represents the measurements. Derivation citations and calibration
    locators are retained separately in each row's method. Licence and confidence
    codes use the existing database schema; this adapter makes no reuse decision.
    """

    row_prefix: str
    soil_unit: str
    site_or_region: str
    source: Source
    locator: str
    licence: str
    confidence_grade: str

    def __post_init__(self) -> None:
        """Validate measurement provenance using the existing database codes."""
        for name in ("row_prefix", "soil_unit", "site_or_region", "locator"):
            _text(name, getattr(self, name))
        if not isinstance(self.source, Source):
            raise ValueError("source must be a database Source")
        Source.from_entry(vars(self.source))
        if self.source.read_status != "read":
            raise ValueError("measurement source must be read")
        if self.licence not in LICENCES or self.confidence_grade not in GRADES:
            raise ValueError("licence or confidence grade is not a database code")


def to_database_rows(
    points: Sequence[NormalisedCPT],
    *,
    drainage: Sequence[DrainageAssessment],
    provenance: ProfileProvenance,
    strengths: Sequence[Estimate] | None = None,
    sand_states: Sequence[SandState] | None = None,
) -> tuple[ParameterRow, ...]:
    """Convert point observations to the existing ParameterRow schema.

    No layer boundaries are inferred. Every input and calibration value is retained.
    Optional correlations must be supplied in observation order. Missing quantities
    use the literal NOT_EVALUATED, which the database does not parse as a number.
    """
    size = len(points)
    if (
        not size
        or len(drainage) != size
        or any(
            items is not None and len(items) != size
            for items in (strengths, sand_states)
        )
    ):
        raise ValueError("nonempty points and matching result lengths are required")
    if any(
        b.corrected.depth_m <= a.corrected.depth_m for a, b in zip(points, points[1:])
    ):
        raise ValueError("profile observations must increase in depth")
    rows = []
    for index, (point, check) in enumerate(zip(points, drainage)):
        row = point.corrected
        if check.corrected != row:
            raise ValueError("drainage result belongs to a different observation")

        def add(
            key: str,
            name: str,
            value: float | str | None,
            unit: str,
            method: str,
            reason: str = "",
        ) -> None:
            available = value is not None and value != NOT_EVALUATED
            if isinstance(value, Real) and not isinstance(value, bool):
                printed = repr(_number(name, value))
            else:
                printed = str(value) if available else NOT_EVALUATED
            entry = dict(
                row_id=f"{provenance.row_prefix}:{index}:{key}",
                soil_unit=provenance.soil_unit,
                parameter=name,
                value_or_range=printed,
                units_as_printed=unit,
                test_or_derivation_method=method,
                site_or_region=provenance.site_or_region,
                depth_range=f"{row.depth_m:g} m (point observation)",
                source_id=provenance.source.source_id,
                locator=provenance.locator,
                licence=provenance.licence,
                confidence_grade=provenance.confidence_grade,
                note=json.dumps(
                    dict(
                        status=EVALUATED if available else NOT_EVALUATED,
                        reason=reason,
                        depth_m=row.depth_m,
                    ),
                    sort_keys=True,
                ),
            )
            rows.append(
                ParameterRow.from_entry(
                    entry, {provenance.source.source_id: provenance.source}
                )
            )

        measurement = "Measured CPT input; explicit pressure units converted to Pa"
        stress = "Supplied stress basis; effective stress=total stress-u0"
        correction = f"qt=qc+u2(1-a); qn=qt-sigma_v; Rf=100fs/qt; Bq=(u2-u0)/qn; {GUIDE_SOURCE}, pp. 18,31-32"
        normalisation = (
            f"Interpretation of cone penetration tests — a unified approach (2009), "
            f"eqs. 5-7; method={point.method}; pa={point.atmospheric_pressure_pa:g} Pa; "
            f"iterations={point.iterations}"
        )
        drainage_method = (
            f"V=v*d/c; basis={check.consolidation_basis}; "
            f"bands citation={check.citation}; locator={check.locator}"
        )
        for key, name, value, unit in (
            ("depth", "Depth", row.depth_m, "m"),
            ("qc", "Cone resistance qc", row.qc_pa, "Pa"),
            ("fs", "Sleeve friction fs", row.fs_pa, "Pa"),
            ("u2", "Pore pressure u2", row.u2_pa, "Pa"),
            ("a", "Cone area ratio a", row.area_ratio, "-"),
        ):
            add(
                key,
                name,
                value,
                unit,
                measurement,
                "u2 missing" if value is None else "",
            )
        for key, name, value in (
            ("sigma_v", "Total vertical stress", row.total_stress_pa),
            ("u0", "Equilibrium pore pressure u0", row.equilibrium_pore_pressure_pa),
            ("sigma_effective", "Effective vertical stress", row.effective_stress_pa),
        ):
            add(key, name, value, "Pa", stress)
        for key, name, value, unit in (
            ("qt", "Corrected cone resistance qt", row.qt_pa, "Pa"),
            ("qn", "Net cone resistance qn", row.qn_pa, "Pa"),
            ("rf", "Friction ratio Rf", row.rf_percent, "%"),
            ("bq", "Pore-pressure ratio Bq", row.bq, "-"),
        ):
            add(key, name, value, unit, correction, row.reason)
        for key, name, value, unit in (
            ("qtn", "Normalised cone resistance Qtn", point.qtn, "-"),
            ("fr", "Normalised friction ratio Fr", point.fr_percent, "%"),
            ("ic", "Soil behaviour type index Ic", point.ic, "-"),
            ("n", "Stress exponent n", point.stress_exponent, "-"),
            (
                "pa",
                "Normalisation reference pressure",
                point.atmospheric_pressure_pa,
                "Pa",
            ),
            ("zone", "Soil behaviour zone", point.zone, "-"),
        ):
            add(key, name, value, unit, normalisation, point.reason)
        for key, name, value, unit in (
            ("v", "Penetration rate", check.penetration_rate_m_s, "m/s"),
            ("d", "Cone diameter", check.cone_diameter_m, "m"),
            (
                "c",
                "Consolidation coefficient",
                check.consolidation_coefficient_m2_s,
                "m2/s",
            ),
            ("basis", "Consolidation basis", check.consolidation_basis, "-"),
            ("v_drained", "Drained velocity boundary", check.drained_max, "-"),
            ("v_undrained", "Undrained velocity boundary", check.undrained_min, "-"),
            ("velocity", "Normalised velocity V", check.normalised_velocity, "-"),
            ("regime", "Drainage regime", check.regime, "-"),
        ):
            add(key, name, value, unit, drainage_method, check.reason)
        add(
            "backbone",
            "Drainage-corrected net resistance",
            None,
            "Pa",
            "Published backbone fit unavailable",
            "no quantitative drainage correction evaluated",
        )
        estimates = []
        if strengths is not None:
            estimates.append(("su", "Undrained strength Su", strengths[index]))
        if sand_states is not None:
            estimates.extend(
                (
                    ("dr", "Relative density Dr", sand_states[index].relative_density),
                    ("phi", "Peak friction angle", sand_states[index].friction_angle),
                )
            )
        for key, name, estimate in estimates:
            if estimate.status != EVALUATED and estimate.value is not None:
                raise ValueError("unavailable estimate cannot contain a numeric value")
            add(
                key,
                name,
                estimate.value,
                estimate.unit,
                estimate.method,
                estimate.reason,
            )
    return tuple(rows)
