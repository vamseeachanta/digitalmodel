"""Rank exact SLWR atlas nodes using supplied, aligned excursion responses.

No geometry solver, response generator or licensed solver is invoked. Geometry
for upstream static response shall use the canonical marine_ops LazyWaveSolver;
the quarantined subsea LazyWaveAnalyzer is not an alternative. A source string
records the caller's basis and does not establish independent qualification.

Equations: Application of computers in fatigue analysis (1985), section 3.3.1,
https://wrap.warwick.ac.uk/id/eprint/2949/ (crossing rates, Rayleigh ranges),
and the existing fatigue.spectral_fatigue.narrow_band_damage implementation.
S-N coefficients, durations and acceptance limits are required caller inputs.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from typing import Mapping

import numpy as np

from digitalmodel.fatigue.spectral_fatigue import SpectralMoments, narrow_band_damage
from digitalmodel.parametric.atlas import Atlas

VALIDITY_BOUNDARY = (
    "SCREEN: static excursion stress range is a proxy; supplied stress spectra "
    "and any scalar transfer give screening estimates only, not validated dynamic "
    "response. Narrow-band spectral assumptions apply. CERTIFY: NOT_EVALUATED; "
    "qualified dynamic stress extraction, at "
    "least two OrcaFlex candidate cross-checks and held-out corpus comparisons "
    "are required before a certified optimum or client-facing damage figure."
)
SOURCES = (
    "Application of computers in fatigue analysis (1985), section 3.3.1, "
    "equations 3.16 and 3.17; https://wrap.warwick.ac.uk/id/eprint/2949/",
    "src/digitalmodel/fatigue/spectral_fatigue.py:narrow_band_damage",
)
_REQUIRED_EXCURSIONS = {"near", "far", "cross"}


def _finite(values, name: str) -> None:
    if any(isinstance(v, bool) or not math.isfinite(v) for v in values):
        raise ValueError(f"{name} must contain finite numbers")


def _source(value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("source must describe the input basis")


def _increasing(values, name: str) -> None:
    _finite(values, name)
    if (
        len(values) < 2
        or values[0] < 0
        or any(b <= a for a, b in zip(values, values[1:]))
    ):
        raise ValueError(f"{name} must be nonnegative and strictly increasing")


@dataclass(frozen=True)
class BuoyancyLayout:
    """Uniform modules; spacing is clear gap, uplift is per-module net uplift.

    Distribution is set by the start position and gap. Irregular distributions
    require an upstream response/parameterisation extension, not interpolation.
    """

    module_count: int
    module_length_m: float
    module_spacing_m: float
    net_buoyancy_per_module_N: float
    buoyancy_start_m: float

    def __post_init__(self):
        values = tuple(asdict(self).values())
        _finite(values, "layout")
        if (
            isinstance(self.module_count, bool)
            or self.module_count <= 0
            or self.module_count != int(self.module_count)
        ):
            raise ValueError("module_count must be a positive integer")
        if self.module_length_m <= 0 or self.net_buoyancy_per_module_N <= 0:
            raise ValueError("module length and net buoyancy must be positive")
        if self.module_spacing_m < 0 or self.buoyancy_start_m < 0:
            raise ValueError("module spacing and start must be nonnegative")

    def to_dict(self) -> dict:
        """Return the exact parameter fields stored in each atlas row."""
        return asdict(self)


@dataclass(frozen=True)
class ConstraintLimits:
    """Caller-sourced criteria; equality passes each stated limit."""

    max_top_tension_N: float
    min_bend_radius_m: float
    min_hog_clearance_m: float
    min_interference_clearance_m: float
    source: str

    def __post_init__(self):
        values = (
            self.max_top_tension_N,
            self.min_bend_radius_m,
            self.min_hog_clearance_m,
            self.min_interference_clearance_m,
        )
        _finite(values, "limits")
        if (
            min(values) < 0
            or self.max_top_tension_N == 0
            or self.min_bend_radius_m == 0
        ):
            raise ValueError(
                "tension/radius limits must be positive; clearances nonnegative"
            )
        _source(self.source)


@dataclass(frozen=True)
class ExcursionResponse:
    """Same material arc stations and stress definition for every excursion.

    Radii and clearances are minima over the relevant regions in this excursion.
    Missing metrics are unknown, never replaced by zero. Stress units are MPa.
    """

    name: str
    stress_MPa: tuple[float, ...]
    top_tension_N: float | None
    sag_bend_radius_m: float | None
    hog_bend_radius_m: float | None
    hog_clearance_m: float | None
    interference_clearance_m: float | None

    def __post_init__(self):
        if not self.name:
            raise ValueError("excursion name must not be empty")
        _finite(self.stress_MPa, "stress")
        metrics = (
            self.top_tension_N,
            self.sag_bend_radius_m,
            self.hog_bend_radius_m,
            self.hog_clearance_m,
            self.interference_clearance_m,
        )
        _finite([v for v in metrics if v is not None], "response metrics")
        if any(v is not None and v < 0 for v in metrics[:3]):
            raise ValueError("tension and bend radii must be nonnegative")


@dataclass(frozen=True)
class StressSpectrum:
    """One-sided stress PSD in MPa²/Hz at one material arc station [m]."""

    arc_length_m: float
    frequency_Hz: tuple[float, ...]
    psd_MPa2_per_Hz: tuple[float, ...]
    source: str

    def __post_init__(self):
        _finite((self.arc_length_m,), "spectrum station")
        if self.arc_length_m < 0:
            raise ValueError("spectrum station must be nonnegative")
        _increasing(self.frequency_Hz, "frequency")
        _finite(self.psd_MPa2_per_Hz, "PSD")
        if len(self.frequency_Hz) != len(self.psd_MPa2_per_Hz):
            raise ValueError("frequency and PSD lengths must match")
        if any(p < 0 for p in self.psd_MPa2_per_Hz):
            raise ValueError("PSD must be nonnegative")
        _source(self.source)


@dataclass(frozen=True)
class SpectralBasis:
    """Single-slope S-N N=A/S**m, log10(A), duration [s]; no standard defaults."""

    sn_slope: float
    sn_log10_intercept: float
    duration_s: float
    source: str

    def __post_init__(self):
        _finite((self.sn_slope, self.sn_log10_intercept, self.duration_s), "S-N basis")
        if self.sn_slope <= 0 or self.duration_s <= 0:
            raise ValueError("S-N slope and duration must be positive")
        _source(self.source)


@dataclass(frozen=True)
class CandidateResponse:
    """Supplied upstream evidence; not generated or qualified by this module."""

    layout: BuoyancyLayout
    arc_length_m: tuple[float, ...]
    touchdown_interval_m: tuple[float, float]
    excursions: tuple[ExcursionResponse, ...]
    source: str
    spectra: tuple[StressSpectrum, ...] = ()

    def __post_init__(self):
        _increasing(self.arc_length_m, "arc length")
        _increasing(self.touchdown_interval_m, "touchdown interval")
        lo, hi = self.touchdown_interval_m
        if lo < self.arc_length_m[0] or hi > self.arc_length_m[-1]:
            raise ValueError("touchdown interval must lie inside the sampled profile")
        if not any(lo <= s <= hi for s in self.arc_length_m):
            raise ValueError("touchdown interval has no sampled station")
        if any(len(e.stress_MPa) != len(self.arc_length_m) for e in self.excursions):
            raise ValueError("stress profiles must align with the material arc grid")
        names = [e.name for e in self.excursions]
        if len(set(names)) != len(names):
            raise ValueError("excursion names must be unique")
        stations = [p.arc_length_m for p in self.spectra]
        if len(set(stations)) != len(stations) or any(
            s not in self.arc_length_m for s in stations
        ):
            raise ValueError("spectra must have unique, sampled arc stations")
        end = (
            self.layout.buoyancy_start_m
            + self.layout.module_count * self.layout.module_length_m
            + (self.layout.module_count - 1) * self.layout.module_spacing_m
        )
        if end > self.arc_length_m[-1]:
            raise ValueError("module layout extends beyond the sampled riser")
        _source(self.source)


@dataclass(frozen=True)
class ConstraintCheck:
    name: str
    excursion: str
    observed: float | None
    limit: float
    comparison: str
    status: str


@dataclass(frozen=True)
class CandidateResult:
    case_id: str
    layout: BuoyancyLayout
    rank: int | None
    screen_status: str
    constraint_status: str
    objective_MPa: float | None
    touchdown_interval_m: tuple[float, float] | None
    arc_length_m: tuple[float, ...]
    stress_range_MPa: tuple[float, ...]
    critical_arc_length_m: float | None
    constraints: tuple[ConstraintCheck, ...]
    spectral_status: str
    spectral_arc_length_m: tuple[float, ...]
    screen_spectral_damage: tuple[float, ...] | None
    certify_status: str
    reasons: tuple[str, ...]
    sources: tuple[str, ...]
    validity_boundary: str = VALIDITY_BOUNDARY


@dataclass(frozen=True)
class ScreeningResult:
    atlas_id: str
    candidates: tuple[CandidateResult, ...]
    spectral_basis: SpectralBasis | None = None
    citations: tuple[str, ...] = SOURCES
    validity_boundary: str = VALIDITY_BOUNDARY

    def to_dict(self) -> dict:
        """Return JSON-compatible evidence with both validity tiers on every row."""
        return asdict(self)


def _envelope(response: CandidateResponse) -> tuple[float, ...]:
    # Definition of excursion envelope, not a fatigue-damage equation.
    return tuple(
        max(v) - min(v) for v in zip(*(e.stress_MPa for e in response.excursions))
    )


def stress_spectra_from_excursions(
    response: CandidateResponse,
    frequency_Hz: tuple[float, ...],
    excursion_psd_m2_per_Hz: tuple[float, ...],
    reference_excursion_range_m: float,
    source: str,
) -> tuple[StressSpectrum, ...]:
    """Optional scalar secant transfer: G=(delta stress/delta excursion)**2*Sx.

    Linear PSD scaling follows the variance scaling of a scalar linear map;
    crossing-rate and narrow-band assumptions follow the 1985 source above.
    This envelope secant is an assumed transfer, not a dynamic response model.
    The caller shall supply a positive excursion span matching its case envelope.
    """
    _finite((reference_excursion_range_m,), "reference excursion range")
    if reference_excursion_range_m <= 0:
        raise ValueError("reference excursion range must be positive")
    if not _REQUIRED_EXCURSIONS.issubset(e.name for e in response.excursions):
        raise ValueError("near, far and cross excursions are required for transfer")
    # Reuse the spectrum validator for the input motion PSD (units differ only).
    StressSpectrum(0.0, frequency_Hz, excursion_psd_m2_per_Hz, source)
    return tuple(
        StressSpectrum(
            s,
            frequency_Hz,
            tuple(
                p * (r / reference_excursion_range_m) ** 2
                for p in excursion_psd_m2_per_Hz
            ),
            source + "; assumed scalar secant transfer; SCREEN only",
        )
        for s, r in zip(response.arc_length_m, _envelope(response))
    )


def _constraints(response: CandidateResponse, limits: ConstraintLimits):
    criteria = (
        ("top_tension", "top_tension_N", limits.max_top_tension_N, "<="),
        ("sag_bend_radius", "sag_bend_radius_m", limits.min_bend_radius_m, ">="),
        ("hog_bend_radius", "hog_bend_radius_m", limits.min_bend_radius_m, ">="),
        ("hog_clearance", "hog_clearance_m", limits.min_hog_clearance_m, ">="),
        (
            "interference_clearance",
            "interference_clearance_m",
            limits.min_interference_clearance_m,
            ">=",
        ),
    )
    checks = []
    for excursion in response.excursions:
        for name, field, limit, comparison in criteria:
            observed = getattr(excursion, field)
            if observed is None:
                status = "NOT_EVALUATED"
            else:
                passes = observed <= limit if comparison == "<=" else observed >= limit
                status = "PASS" if passes else "FAIL"
            checks.append(
                ConstraintCheck(
                    name, excursion.name, observed, limit, comparison, status
                )
            )
    return tuple(checks)


def _spectral_damage(spectrum: StressSpectrum, basis: SpectralBasis) -> float:
    f = np.asarray(spectrum.frequency_Hz)
    psd = np.asarray(spectrum.psd_MPa2_per_Hz)
    # Application of computers in fatigue analysis (1985), §3.3.1, eqs 3.16-17:
    # rate = sqrt(M2/M0)/(2*pi) for ANGULAR moments. Integrate on Hz, convert
    # moment order n by (2*pi)**n; M0 stays variance, avoiding a second Jacobian.
    moments = [float(np.trapz(psd * (2 * math.pi * f) ** n, f)) for n in (0, 1, 2, 4)]
    if moments[0] == 0:
        return 0.0  # Zero variance => zero alternating stress; no crossing rate.
    if moments[2] <= 0 or moments[3] <= 0:
        raise ValueError("nonzero PSD must resolve positive-frequency stress energy")
    result = narrow_band_damage(
        SpectralMoments(
            m0=moments[0],
            m1=moments[1],
            m2=moments[2],
            m4=moments[3],
        ),
        basis.sn_slope,
        basis.sn_log10_intercept,
    )
    # Use the unrounded rate, not the engine's calendar-year reporting field.
    damage = result.damage_rate * basis.duration_s
    _finite((damage,), "spectral damage")
    return damage


def _evaluate(case_id, layout, response, limits, basis) -> dict:
    row = dict(
        case_id=case_id,
        layout=layout,
        rank=None,
        screen_status="NOT_EVALUATED",
        constraint_status="NOT_EVALUATED",
        objective_MPa=None,
        touchdown_interval_m=None,
        arc_length_m=(),
        stress_range_MPa=(),
        critical_arc_length_m=None,
        constraints=(),
        spectral_status="NOT_EVALUATED",
        spectral_arc_length_m=(),
        screen_spectral_damage=None,
        certify_status="NOT_EVALUATED",
        reasons=[],
        sources=[limits.source],
    )
    if response is None:
        row["reasons"].append("Excursion response missing for this exact atlas node")
        return row
    if response.layout != layout:
        raise ValueError(f"response layout does not match atlas node {case_id}")
    row["sources"].append(response.source)
    row["touchdown_interval_m"] = response.touchdown_interval_m
    row["constraints"] = checks = _constraints(response, limits)
    complete = _REQUIRED_EXCURSIONS.issubset(e.name for e in response.excursions)
    failed = any(c.status == "FAIL" for c in checks)
    unknown = not complete or any(c.status == "NOT_EVALUATED" for c in checks)
    row["constraint_status"] = (
        "VIOLATED" if failed else ("NOT_EVALUATED" if unknown else "FEASIBLE")
    )
    if not complete:
        row["reasons"].append("Required near, far or cross excursion missing")
        return row
    ranges = _envelope(response)
    _finite(ranges, "stress envelope")
    lo, hi = response.touchdown_interval_m
    indices = [i for i, s in enumerate(response.arc_length_m) if lo <= s <= hi]
    critical = max(indices, key=lambda i: ranges[i])
    row.update(
        screen_status="EVALUATED",
        objective_MPa=ranges[critical],
        arc_length_m=response.arc_length_m,
        stress_range_MPa=ranges,
        critical_arc_length_m=response.arc_length_m[critical],
    )
    _evaluate_spectra(row, response, indices, basis)
    return row


def _evaluate_spectra(row, response, indices, basis):
    spectra = {s.arc_length_m: s for s in response.spectra}
    stations = tuple(response.arc_length_m[i] for i in indices)
    if basis is None or not all(s in spectra for s in stations):
        row["reasons"].append("Spectral basis or complete touchdown stress PSD missing")
        return
    row["spectral_arc_length_m"] = stations
    row["screen_spectral_damage"] = tuple(
        _spectral_damage(spectra[s], basis) for s in stations
    )
    row["spectral_status"] = "EVALUATED_SCREENING"
    row["sources"].extend([basis.source, *(spectra[s].source for s in stations)])


def rank_buoyancy_candidates(
    atlas: Atlas,
    responses: Mapping[str, CandidateResponse],
    limits: ConstraintLimits,
    spectral_basis: SpectralBasis | None = None,
) -> ScreeningResult:
    """Rank feasible exact nodes by maximum TDZ stress range, retaining all rows.

    Uses the existing atlas grid as the candidate set; no new sweep or surrogate
    is created. Missing data and violations remain unranked. The atlas response
    column is not used as a stress or damage proxy. Ties sort by stable case id.
    """
    records = atlas.grid.to_dict("records")
    ids = [r.get("case_id") for r in records]
    if not records or any(not isinstance(i, str) or not i.strip() for i in ids):
        raise ValueError("atlas must contain nonempty string case_id values")
    if len(ids) != len(set(ids)):
        raise ValueError("atlas case_id values must be unique")
    if set(responses) - set(ids):
        raise ValueError("responses contain unknown atlas case_id values")
    rows = []
    layouts = []
    for record in records:
        layout = BuoyancyLayout(
            **{k: record[k] for k in BuoyancyLayout.__dataclass_fields__}
        )
        if layout in layouts:
            raise ValueError("atlas layout values must be unique")
        layouts.append(layout)
        case_id = record["case_id"]
        rows.append(
            _evaluate(case_id, layout, responses.get(case_id), limits, spectral_basis)
        )
    eligible = [
        r
        for r in rows
        if r["constraint_status"] == "FEASIBLE" and r["screen_status"] == "EVALUATED"
    ]
    eligible.sort(key=lambda r: (r["objective_MPa"], r["case_id"]))
    for rank, row in enumerate(eligible, start=1):
        row["rank"] = rank
    rows.sort(key=lambda r: (r["rank"] is None, r["rank"] or 0, r["case_id"]))
    for row in rows:
        row["reasons"] = tuple(row["reasons"])
        row["sources"] = tuple(dict.fromkeys(row["sources"]))
    return ScreeningResult(
        atlas.atlas_id, tuple(CandidateResult(**r) for r in rows), spectral_basis
    )
