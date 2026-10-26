# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared exported-result types and structural and semantic validation.

The normalised strength limit is unity by the definition stress / allowable;
no standard acceptance value is inferred. Method and allowable are export
inputs. Consistency tolerances are contract policy, not engineering allowances.
"""

from __future__ import annotations

from typing import Any, Literal, NotRequired, TypedDict


class Run(TypedDict):
    """Portable native export provenance."""

    input_sha256: str
    result_file: str
    date: str
    data_basis: NotRequired[str]
    id: NotRequired[str]
    phase: NotRequired[Literal["A", "B"]]
    status: NotRequired[Literal["attempted", "completed", "failed"]]
    qualified: NotRequired[bool]


class Software(TypedDict):
    """Recorded software identity, without an inferred version."""

    name: str
    version: str


class Offset(TypedDict):
    """Horizontal offset in metres."""

    x_m: float
    y_m: float


class LoadCase(TypedDict):
    """Exported environmental parameters; wave/current retain named units."""

    id: str
    wave: dict[str, Any]
    current: dict[str, Any]
    offset: dict[str, Any]


class Allowable(TypedDict):
    """Stress denominator and its supplied basis."""

    value: float
    unit: Literal["MPa"]
    source: str
    basis_label: NotRequired[str]


class Maximum(TypedDict):
    """Maximum over the exported strength samples."""

    value: float
    arc_length_m: float
    location: str


class Touchdown(TypedDict):
    """Touchdown summary, including the exported stress range."""

    arc_length_m: float
    utilisation: float
    stress_range_MPa: float


class TopAngle(TypedDict):
    """Acute inclination from vertical, in degrees."""

    static: float
    dynamic_min: float
    dynamic_max: float


class HangOffTension(TypedDict):
    """Static and dynamic maximum effective tension in kN."""

    static: float
    dynamic_max: float


class FibreStress(TypedDict):
    """Exported nonnegative equivalent stress on both pipe fibres, in MPa."""

    inner: list[float]
    outer: list[float]


class StressStates(TypedDict):
    """Static stresses and the maximum over each dynamic fibre history."""

    static: FibreStress
    dynamic_max: FibreStress | None


class U1Inputs(TypedDict):
    """Explicit stress combination and material basis for U1."""

    method: Literal["API RP 2RD"]
    smys_MPa: float
    stress_MPa: StressStates


class TensionEnvelope(TypedDict):
    """Dynamic extrema at the same arc stations as the static export."""

    dynamic_min: list[float] | None
    dynamic_max: list[float] | None


class AnalyticalStatic(TypedDict):
    """Independent analytical catenary outputs with explicit units."""

    top_tension_kN: float
    top_angle_deg: float
    touchdown_x_m: float


class StaticCheck(TypedDict):
    """Analytical comparison inputs; touchdown position is horizontal."""

    analytical: AnalyticalStatic
    software_touchdown_x_m: float
    basis: str


class CombinedCheck(TypedDict):
    """An exported combined-loading check over the recorded history."""

    arc_length_m: float
    utilisation: float
    check: str


class CombinedLoading(TypedDict):
    """Explicit applicability and exported API RP 1111 check values."""

    in_scope: bool
    method: str
    reason: str
    checks: list[CombinedCheck]


class Screening(TypedDict):
    """Additional inputs required by the per-size strength output contract."""

    condition: Literal["functional", "extreme"]
    dynamic: bool
    qualified: bool
    u1: U1Inputs
    tension_envelope_kN: TensionEnvelope
    hang_off_dynamic_min_kN: float | None
    excursion_reference_deg: float
    excursion_limit_deg: float
    static_check: StaticCheck | None
    api_rp_1111: CombinedLoading


class StrengthResult(TypedDict):
    """One case's strength export; arc is measured from hang-off."""

    exchange_version: NotRequired[Literal["ris-exchange/1"]]
    case_id: str
    software: Software
    load_case: LoadCase
    arc_length_m: list[float]
    effective_tension_kN: list[float]
    bending_moment_kNm: list[float]
    von_mises_utilisation: list[float]
    method: NotRequired[str]
    allowable: NotRequired[Allowable]
    utilisation_method: NotRequired[str]
    utilisation_allowable: NotRequired[float]
    utilisation_basis: NotRequired[Literal["U1"]]
    max_utilisation: Maximum
    touchdown: Touchdown
    top_angle_deg: TopAngle
    hang_off_tension_kN: HangOffTension
    run: Run
    screening: NotRequired[Screening]


class RangeBin(TypedDict):
    """One stress-range histogram bin, with annual cycles."""

    stress_range_MPa: float
    cycles_per_year: float


class HotSpot(TypedDict):
    """One hot spot with either a single range or a histogram."""

    name: Literal["touchdown", "hang_off"]
    arc_length_m: float
    cycles_per_year: float
    scf: float
    stress_range_MPa: NotRequired[float]
    range_histogram: NotRequired[list[RangeBin]]
    damage_per_year: NotRequired[float]


class SeaState(TypedDict):
    """Sea-state parameters with occurrence as a fraction."""

    id: str
    Hs_m: float
    Tp_s: float
    direction_deg: float
    occurrence: float


class ThicknessCorrection(TypedDict):
    """Recorded power-law stress multiplier inputs."""

    thickness_mm: float
    reference_thickness_mm: float
    exponent: float
    source_label: str


class ArcPoint(TypedDict):
    """An unnamed fatigue station, with one range representation."""

    arc_length_m: float
    cycles_per_year: float
    scf: float
    damage_per_year: NotRequired[float]
    stress_range_MPa: NotRequired[float]
    range_histogram: NotRequired[list[RangeBin]]


class SNCurve(TypedDict):
    """Supplied curve; no licensed publication value is supplied here."""

    name: str
    source: NotRequired[str]
    source_label: NotRequired[str]
    m: float
    log_a: float
    thickness_correction: NotRequired[float | ThicknessCorrection | None]
    reference_cycles: NotRequired[float]
    reference_range_MPa: NotRequired[float]
    endurance_limit_MPa: NotRequired[float | None]


class FatigueResult(TypedDict):
    """One case and sea-state fatigue export."""

    exchange_version: NotRequired[Literal["ris-exchange/1"]]
    case_id: str
    sea_state: SeaState
    hot_spots: list[HotSpot]
    sn_curve: SNCurve
    damage_per_year: float
    life_years: NotRequired[float | None]
    arc_distribution: NotRequired[list[ArcPoint]]
    run: Run


class CampaignRun(TypedDict):
    """Explicit status with optional full export provenance."""

    id: str
    phase: Literal["A", "B"]
    status: Literal["attempted", "completed", "failed"]
    qualified: bool
    input_sha256: NotRequired[str]
    result_file: NotRequired[str]
    date: NotRequired[str]
    data_basis: NotRequired[str]
