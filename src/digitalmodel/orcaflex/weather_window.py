"""Weather window analysis for marine operations.

Provides wave scatter diagram processing, operability tables from Hs/Tp limits,
persistence analysis (consecutive hours below threshold), seasonal variation,
and waiting-on-weather (WoW) statistics.

Does NOT require OrcFxAPI — operates on hindcast / metocean data.

References:
    - DNV-RP-H103: Modelling and Analysis of Marine Operations (weather criteria)
    - DNV-OS-H101 / DNV-ST-N001: Marine Operations, General
    - API RP 2MET: Metocean Design and Operating Conditions
    - Noble Denton 0027/ND: General Guidelines for Marine Operations
"""

import hashlib
import json
import math
from datetime import datetime, timedelta, timezone
from typing import Dict, List, Literal, Optional

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator


# ---------------------------------------------------------------------------
# Scatter diagram
# ---------------------------------------------------------------------------

class ScatterDiagram(BaseModel):
    """Wave scatter diagram (Hs-Tp joint probability).

    Reference: DNV-RP-C205 Section 3.3 — joint distribution of Hs and Tp.
    """
    hs_bins: List[float] = Field(
        default=[0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0],
        description="Upper bin edges for Hs (m)",
    )
    tp_bins: List[float] = Field(
        default=[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
        description="Upper bin edges for Tp (s)",
    )
    occurrences: Optional[List[List[float]]] = Field(
        None,
        description="2D array [n_hs x n_tp] of occurrence counts or probabilities",
    )

    def generate_default_scatter(self) -> List[List[float]]:
        """Generate a default scatter diagram (GOM-like distribution).

        This is a simplified synthetic distribution for demonstration.
        For real projects, use site-specific hindcast data.

        Returns:
            2D list of occurrence probabilities.
        """
        n_hs = len(self.hs_bins)
        n_tp = len(self.tp_bins)
        scatter = np.zeros((n_hs, n_tp))

        for i, hs in enumerate(self.hs_bins):
            for j, tp in enumerate(self.tp_bins):
                # Bivariate distribution (simplified log-normal-like)
                hs_mid = hs - 0.25
                tp_mid = tp - 0.5
                # Most probable Tp for given Hs: Tp ≈ 4.5 * sqrt(Hs)
                tp_peak = 4.5 * math.sqrt(max(hs_mid, 0.1))
                # Gaussian-ish kernel
                prob = math.exp(-0.5 * ((hs_mid - 1.5) / 1.2) ** 2) * \
                       math.exp(-0.5 * ((tp_mid - tp_peak) / 1.5) ** 2)
                scatter[i, j] = max(0, prob)

        # Normalise to sum to 1
        total = scatter.sum()
        if total > 0:
            scatter = scatter / total

        return scatter.tolist()

    def get_occurrences(self) -> np.ndarray:
        """Return occurrences as numpy array, generating default if not provided."""
        if self.occurrences is not None:
            return np.array(self.occurrences)
        return np.array(self.generate_default_scatter())

    def total_probability_below(self, hs_limit: float, tp_limit: Optional[float] = None) -> float:
        """Calculate total probability of sea states below thresholds.

        Args:
            hs_limit: Maximum significant wave height (m).
            tp_limit: Maximum peak period (s). If None, no Tp limit.

        Returns:
            Total probability (0-1).
        """
        scatter = self.get_occurrences()
        total = 0.0
        for i, hs in enumerate(self.hs_bins):
            for j, tp in enumerate(self.tp_bins):
                if i < scatter.shape[0] and j < scatter.shape[1]:
                    if hs <= hs_limit and (tp_limit is None or tp <= tp_limit):
                        total += scatter[i, j]
        return round(total, 4)


# ---------------------------------------------------------------------------
# Operability table
# ---------------------------------------------------------------------------

SampleField = Literal["hs_m", "tp_s", "wind_mps", "current_mps"]


class ObservationProvenance(BaseModel):
    """Source identity and supplied quality; neither establishes engineering readiness."""

    model_config = ConfigDict(extra="forbid", strict=True)
    source_id: Literal["synthetic", "noaa-ndbc", "coops"]
    record_id: str = Field(min_length=1, pattern=r"\S")
    quality: Literal["synthetic", "unverified", "missing"]
    input_sha256: Optional[str] = Field(None, pattern=r"^[a-f0-9]{64}$")


class SampleUnits(BaseModel):
    """Explicit units for synchronized input; conversion belongs at the parser."""

    model_config = ConfigDict(extra="forbid")
    duration_hours: Literal["h"]
    hs_m: Literal["m"]
    tp_s: Literal["s"]
    wind_mps: Literal["m/s"]
    current_mps: Literal["m/s"]


class SynchronizedSample(BaseModel):
    """One co-timed interval, with no interpolation or implicit series join.

    All supplied variables describe the same interval. Source selection, peak
    period qualification, averaging, spatial representativeness and deployment
    depth remain the caller's responsibility. Missing enabled values are errors.
    """

    model_config = ConfigDict(extra="forbid", strict=True, allow_inf_nan=False)
    start_utc: datetime
    duration_hours: float = Field(gt=0)
    hs_m: float = Field(ge=0)
    tp_s: float = Field(ge=0)
    wind_mps: Optional[float] = Field(None, ge=0)
    current_mps: Optional[float] = Field(None, ge=0)
    units: SampleUnits
    provenance: Dict[SampleField, ObservationProvenance]

    @field_validator("start_utc", mode="before")
    @classmethod
    def explicit_utc(cls, value: object) -> datetime:
        """Accept UTC datetimes or ISO text with an explicit zero offset."""
        if isinstance(value, str):
            value = datetime.fromisoformat(value.replace("Z", "+00:00"))
        if not isinstance(value, datetime) or value.utcoffset() != timedelta(0):
            raise ValueError("start_utc requires an explicit UTC timestamp")
        return value

    @model_validator(mode="after")
    def check_interval_and_metadata(self) -> "SynchronizedSample":
        """Reject unrepresentable exposure and untraceable supplied values."""
        try:
            end = self.start_utc + timedelta(hours=self.duration_hours)
        except OverflowError as error:
            raise ValueError("duration exceeds the UTC timestamp range") from error
        if end <= self.start_utc:
            raise ValueError("duration must form a positive UTC interval")
        names: tuple[SampleField, ...] = ("hs_m", "tp_s", "wind_mps", "current_mps")
        for name in names:
            if getattr(self, name) is not None:
                if name not in self.provenance or self.provenance[name].quality == "missing":
                    raise ValueError(f"{name} requires source and nonmissing quality metadata")
        return self


class OperabilityInput(BaseModel):
    """Input for operability calculation.

    Synchronized samples provide software joint-limit checks only. No standards
    compliance or site engineering acceptance is established by this interface.
    Without samples, the legacy wave scatter calculation is retained.
    """

    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)
    hs_limit: float = Field(2.5, gt=0.0, description="Operational Hs limit (m)")
    tp_limit: Optional[float] = Field(None, gt=0.0, description="Operational Tp limit (s)")
    wind_limit_mps: Optional[float] = Field(None, gt=0.0, description="Wind speed limit (m/s)")
    current_limit_mps: Optional[float] = Field(None, gt=0.0, description="Current speed limit (m/s)")
    scatter: ScatterDiagram = Field(default_factory=ScatterDiagram)
    samples: Optional[List[SynchronizedSample]] = None

    @model_validator(mode="after")
    def check_sample_availability(self) -> "OperabilityInput":
        """Reject unsupported joint inference and missing enabled observations."""
        enabled = [("hs_m", self.hs_limit), ("tp_s", self.tp_limit),
                   ("wind_mps", self.wind_limit_mps), ("current_mps", self.current_limit_mps)]
        if self.samples is None:
            if self.wind_limit_mps is not None or self.current_limit_mps is not None:
                raise ValueError("enabled wind/current limits require synchronized samples")
        else:
            if not self.samples:
                raise ValueError("samples must contain positive observed duration")
            previous_end = None
            for row in self.samples:
                if previous_end is not None and row.start_utc < previous_end:
                    raise ValueError("samples must be ordered, unique and nonoverlapping")
                previous_end = row.start_utc + timedelta(hours=row.duration_hours)
                for name, limit in enabled:
                    if limit is not None and getattr(row, name) is None:
                        raise ValueError(f"enabled {name} limit requires samples without missing values")
        return self

    def calculate_operability(self) -> dict:
        """Calculate overall operability percentage.

        Returns:
            Dict with operability metrics.
        """
        # Revalidate mutable nested data and trusted construction/copy bypasses.
        checked = type(self).model_validate(self.model_dump())
        if checked.samples is not None:
            return checked._sample_operability()
        prob = checked.scatter.total_probability_below(checked.hs_limit, checked.tp_limit)

        return {
            "operability_pct": round(prob * 100.0, 1),
            "non_operability_pct": round((1.0 - prob) * 100.0, 1),
            "hs_limit_m": self.hs_limit,
            "tp_limit_s": self.tp_limit,
        }

    def _sample_operability(self) -> dict:
        """Weight the joint pass mask by observed hours and break runs at gaps.

        Coverage spans the first start to the last end. Interior gaps are missing
        coverage, excluded from observed exposure; coverage outside that span is
        unknown. Percentages and windows are returned without display rounding.
        """
        rows = self.samples
        if rows is None:
            raise ValueError("synchronized samples are required")
        observed = math.fsum(row.duration_hours for row in rows)
        if not math.isfinite(observed) or observed <= 0:
            raise ValueError("observed duration denominator must be finite and positive")
        limits = (("hs_m", self.hs_limit), ("tp_s", self.tp_limit),
                  ("wind_mps", self.wind_limit_mps), ("current_mps", self.current_limit_mps))
        passes: List[bool] = []
        windows: List[float] = []
        run: List[float] = []
        gaps: List[float] = []
        previous_end = rows[0].start_utc
        for row in rows:
            gap = (row.start_utc - previous_end).total_seconds() / 3600
            gaps.append(gap)
            passed = all(limit is None or getattr(row, name) <= limit for name, limit in limits)
            passes.append(passed)
            if run and (gap > 0 or not passed):
                windows.append(math.fsum(run))
                run = []
            if passed:
                run.append(row.duration_hours)
            previous_end = row.start_utc + timedelta(hours=row.duration_hours)
        if run:
            windows.append(math.fsum(run))
        passing = math.fsum(row.duration_hours for row, passed in zip(rows, passes) if passed)
        return {
            "operability_pct": 100.0 * (passing / observed),
            "non_operability_pct": 100.0 * ((observed - passing) / observed),
            "hs_limit_m": self.hs_limit, "tp_limit_s": self.tp_limit,
            "wind_limit_mps": self.wind_limit_mps, "current_limit_mps": self.current_limit_mps,
            "observed_duration_hours": observed, "passing_duration_hours": passing,
            "missing_coverage_hours": math.fsum(gaps),
            "coverage_start_utc": rows[0].start_utc, "coverage_end_utc": previous_end,
            "sample_passes": passes, "window_durations_hours": windows,
            "engineering_readiness": "UNVERIFIED",
        }


def _parser_provenance(provenance: dict, source_id: str, payload: bytes) -> dict:
    """Preserve supplied source/quality and bind it to the exact input bytes."""
    parsed = ObservationProvenance.model_validate(provenance)
    if parsed.source_id != source_id:
        raise ValueError("provenance source_id does not match the parser")
    digest = hashlib.sha256(payload).hexdigest()
    if parsed.input_sha256 is not None and parsed.input_sha256 != digest:
        raise ValueError("supplied input digest does not match the parser bytes")
    return {**parsed.model_dump(), "input_sha256": digest}


def _unique_json_members(pairs: List[tuple]) -> dict:
    """Reject duplicate JSON fields before a later member can conceal a value."""
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate JSON member: {key}")
        result[key] = value
    return result


def _source_number(value: object, *, missing: tuple,
                   maximum: Optional[float] = None) -> Optional[float]:
    """Decode an explicit source missing token without coercing it to zero."""
    if value in missing:
        return None
    if isinstance(value, bool) or not isinstance(value, (str, int, float)):
        raise ValueError("invalid source number")
    number = float(value)
    if not math.isfinite(number) or number < 0 or (maximum is not None and number > maximum):
        raise ValueError("source number is nonfinite or outside the supported range")
    return number


def parse_ndbc_standard_met(payload: bytes, *, provenance: dict) -> List[dict]:
    """Parse supplied realtime standard-met bytes under the NOAA NDBC guide.

    https://www.ndbc.noaa.gov/faq/measdes.shtml defines UTC, MM missing values,
    WSPD in m/s and WDIR coming from true north. DPD remains dominant period,
    never Tp. Gusts and other standard columns are retained as raw fields only.
    Historical numeric sentinel formats are unsupported and rejected. No height
    correction, averaging conversion, qualification or network access occurs.
    """
    source = _parser_provenance(provenance, "noaa-ndbc", payload)
    lines = [line.split() for line in payload.decode("ascii").splitlines() if line.strip()]
    if len(lines) < 3 or lines[0][:5] != ["#YY", "MM", "DD", "hh", "mm"]:
        raise ValueError("realtime standard-met header and samples are required")
    names, units = lines[0][5:], lines[1][5:]
    known = {"WDIR": "degT", "WSPD": "m/s", "GST": "m/s", "WVHT": "m",
             "DPD": "sec", "APD": "sec", "MWD": "degT", "PRES": "hPa",
             "ATMP": "degC", "WTMP": "degC", "DEWP": "degC", "VIS": "nmi",
             "PTDY": "hPa", "TIDE": "ft"}
    if (lines[1][:5] != ["#yr", "mo", "dy", "hr", "mn"] or len(names) != len(set(names))
            or not {"WDIR", "WSPD", "WVHT", "DPD"}.issubset(names)
            or any(name not in known for name in names) or units != [known[n] for n in names]):
        raise ValueError("unsupported standard-met fields or units")
    result = []
    for tokens in lines[2:]:
        if len(tokens) != len(lines[0]):
            raise ValueError("standard-met row does not match its header")
        raw = dict(zip(names, tokens[5:]))
        values = {}
        for name in ("WDIR", "WSPD", "WVHT", "DPD"):
            sentinel = 999 if name == "WDIR" else 99
            value = _source_number(raw[name], missing=("MM",), maximum=360 if name == "WDIR" else None)
            if value == sentinel:
                raise ValueError("historical numeric missing sentinels are unsupported")
            values[name] = value
        year, month, day, hour, minute = map(int, tokens[:5])
        result.append({"start_utc": datetime(year, month, day, hour, minute, tzinfo=timezone.utc),
                       "hs_m": values["WVHT"], "dominant_period_s": values["DPD"],
                       "wind_mps": values["WSPD"], "wind_from_degrees_true": values["WDIR"],
                       "provenance": dict(source), "raw_fields": raw})
    return result


def parse_coops_currents(payload: bytes, *, provenance: dict, units: str,
                         time_zone: str, expected_bin: int) -> List[dict]:
    """Parse supplied CO-OPS core current JSON, independently of qualification.

    NOAA API/response guides: https://api.tidesandcurrents.noaa.gov/api/prod/
    and responseHelp.html. Only metric/gmt core t/s/d/b rows are supported.
    Empty/null stays missing as a defensive software policy, not a documented
    NOAA sentinel guarantee; source acceptance of that detail is blocked pending
    authoritative evidence. Other undocumented sentinels fail. Bin identifies
    the explicitly requested bin, not deployment depth.
    No timestamps or intervals are inferred or joined to another source.
    """
    if units != "metric" or time_zone != "gmt":
        raise ValueError("only metric units and gmt timestamps are supported")
    if type(expected_bin) is not int or expected_bin <= 0:
        raise ValueError("one positive expected bin identifier is required")
    source = _parser_provenance(provenance, "coops", payload)
    document = json.loads(payload, object_pairs_hook=_unique_json_members)
    if not isinstance(document, dict) or set(document) != {"data"}:
        raise ValueError("only the core currents data schema is supported")
    rows = document["data"]
    if not isinstance(rows, list) or not rows:
        raise ValueError("current samples are required")
    result = []
    for row in rows:
        if not isinstance(row, dict) or set(row) != {"t", "s", "d", "b"}:
            raise ValueError("current row requires only t/s/d/b")
        if str(row["b"]) != str(expected_bin):
            raise ValueError("unsupported or mismatched current bin")
        speed = _source_number(row["s"], missing=("", None))
        result.append({"start_utc": datetime.strptime(row["t"], "%Y-%m-%d %H:%M").replace(tzinfo=timezone.utc),
                       "current_mps": None if speed is None else speed / 100.0,
                       "current_direction_degrees": _source_number(row["d"], missing=("", None), maximum=360),
                       "bin_id": expected_bin, "provenance": dict(source)})
    return result


class OperabilityTable(BaseModel):
    """Generate operability for a range of Hs limits.

    Reference: Noble Denton 0027/ND Section 4.
    """
    hs_limits: List[float] = Field(
        default=[1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0],
        description="Hs limits to evaluate (m)",
    )
    scatter: ScatterDiagram = Field(default_factory=ScatterDiagram)

    def generate_table(self) -> List[Dict[str, float]]:
        """Generate operability vs Hs limit table.

        Returns:
            List of dicts with hs_limit and operability_pct.
        """
        return [
            {
                "hs_limit_m": hs,
                "operability_pct": round(self.scatter.total_probability_below(hs) * 100.0, 1),
            }
            for hs in self.hs_limits
        ]


# ---------------------------------------------------------------------------
# Persistence analysis
# ---------------------------------------------------------------------------

class PersistenceResult(BaseModel):
    """Result of persistence (weather window) analysis."""
    hs_limit: float = Field(..., description="Hs threshold (m)")
    mean_window_hours: float = Field(..., description="Mean weather window duration (hours)")
    median_window_hours: float = Field(..., description="Median weather window duration (hours)")
    p10_window_hours: float = Field(..., description="10th percentile window (hours)")
    p90_window_hours: float = Field(..., description="90th percentile window (hours)")
    max_window_hours: float = Field(..., description="Maximum weather window (hours)")
    num_windows: int = Field(..., description="Number of windows found")
    total_hours_below: float = Field(..., description="Total hours below threshold")


def analyse_persistence(
    hs_timeseries: np.ndarray,
    hs_limit: float,
    time_step_hours: float = 3.0,
) -> PersistenceResult:
    """Analyse persistence of weather windows in Hs time series.

    A 'weather window' is a consecutive period where Hs <= hs_limit.

    Args:
        hs_timeseries: Array of Hs values (m).
        hs_limit: Operational Hs limit (m).
        time_step_hours: Time step between observations (hours).

    Returns:
        PersistenceResult with window statistics.

    Reference: DNV-OS-H101 Section 3, Noble Denton 0027/ND.
    """
    below = hs_timeseries <= hs_limit
    total_below = float(np.sum(below)) * time_step_hours

    # Find consecutive windows
    windows: List[float] = []
    current_window = 0
    for val in below:
        if val:
            current_window += 1
        else:
            if current_window > 0:
                windows.append(current_window * time_step_hours)
            current_window = 0
    if current_window > 0:
        windows.append(current_window * time_step_hours)

    if len(windows) == 0:
        return PersistenceResult(
            hs_limit=hs_limit,
            mean_window_hours=0.0,
            median_window_hours=0.0,
            p10_window_hours=0.0,
            p90_window_hours=0.0,
            max_window_hours=0.0,
            num_windows=0,
            total_hours_below=total_below,
        )

    arr = np.array(windows)
    return PersistenceResult(
        hs_limit=hs_limit,
        mean_window_hours=round(float(np.mean(arr)), 1),
        median_window_hours=round(float(np.median(arr)), 1),
        p10_window_hours=round(float(np.percentile(arr, 10)), 1),
        p90_window_hours=round(float(np.percentile(arr, 90)), 1),
        max_window_hours=round(float(np.max(arr)), 1),
        num_windows=len(windows),
        total_hours_below=round(total_below, 1),
    )


# ---------------------------------------------------------------------------
# Seasonal variation
# ---------------------------------------------------------------------------

class SeasonalOperability(BaseModel):
    """Monthly/seasonal operability statistics.

    Reference: API RP 2MET Section 5.
    """
    month_names: List[str] = Field(
        default=["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    )
    operability_pct: List[float] = Field(
        default=[40, 45, 55, 65, 75, 85, 90, 88, 80, 65, 50, 42],
        description="Monthly operability percentages (example for North Sea)",
    )
    hs_limit: float = Field(2.5, gt=0.0, description="Hs limit used (m)")

    def best_months(self, min_operability: float = 70.0) -> List[str]:
        """Return months with operability above threshold.

        Args:
            min_operability: Minimum operability % to be considered suitable.

        Returns:
            List of suitable month names.
        """
        return [
            m for m, op in zip(self.month_names, self.operability_pct)
            if op >= min_operability
        ]

    def annual_average(self) -> float:
        """Annual average operability (%)."""
        return round(sum(self.operability_pct) / len(self.operability_pct), 1)


# ---------------------------------------------------------------------------
# Waiting on Weather (WoW) statistics
# ---------------------------------------------------------------------------

class WoWEstimate(BaseModel):
    """Waiting-on-Weather estimate for operation planning.

    Reference: Noble Denton 0027/ND Section 4, DNV-OS-H101.
    """
    operation_duration_hours: float = Field(72.0, gt=0.0, description="Reference operation duration (hours)")
    operability_pct: float = Field(70.0, gt=0.0, le=100.0, description="Operability (%)")
    contingency_factor: float = Field(1.5, ge=1.0, description="Contingency factor on WoW")

    def estimate_wow_hours(self) -> Dict[str, float]:
        """Estimate waiting-on-weather hours.

        Simple model: WoW = (1/operability - 1) * duration * contingency
        Better: use persistence analysis for access probability.

        Returns:
            Dict with WoW estimates.
        """
        op_frac = self.operability_pct / 100.0
        if op_frac >= 1.0:
            wow_base = 0.0
        else:
            wow_base = (1.0 / op_frac - 1.0) * self.operation_duration_hours

        wow_design = wow_base * self.contingency_factor
        total_duration = self.operation_duration_hours + wow_design

        return {
            "base_wow_hours": round(wow_base, 1),
            "design_wow_hours": round(wow_design, 1),
            "total_operation_hours": round(total_duration, 1),
            "total_operation_days": round(total_duration / 24.0, 1),
        }
