# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""CP survey data analysis and interpretation.

Provides tools for processing and interpreting cathodic protection survey
data including potential mapping, attenuation curve analysis, DCVG/ACVG
survey interpretation, and close-interval survey (CIS) data processing.

References
----------
- NACE SP0207 "Performing Close-Interval Potential Surveys and DC Surface
  Potential Gradient Surveys on Buried or Submerged Metallic Pipelines"
- NACE TM0497 "Measurement Techniques Related to Criteria for Cathodic
  Protection"
- NACE SP0502 "Pipeline External Corrosion Direct Assessment Methodology"
"""

from __future__ import annotations

import math
from typing import Literal, Optional

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator


# Protection criteria
PROTECTION_POTENTIAL_CSE: float = -0.850  # V vs Cu/CuSO4
PROTECTION_POTENTIAL_AGAGCL: float = -0.800  # V vs Ag/AgCl
OVERPROTECTION_LIMIT_CSE: float = -1.200  # V vs CSE (risk of coating disbondment)


class CISSurveyPoint(BaseModel):
    """A single close-interval survey data point."""

    distance_m: float = Field(..., description="Distance along pipeline [m]")
    on_potential_V: float = Field(
        ..., description="ON (energized) potential [V vs CSE]"
    )
    off_potential_V: Optional[float] = Field(
        None, description="OFF (instant-off) potential [V vs CSE]"
    )


class CISAnalysisResult(BaseModel):
    """Result of CIS data analysis."""

    total_points: int = Field(..., description="Total survey points analyzed")
    protected_points: int = Field(
        ..., description="Points meeting protection criteria"
    )
    underprotected_points: int = Field(
        ..., description="Points below protection criteria"
    )
    overprotected_points: int = Field(
        ..., description="Points exceeding overprotection limit"
    )
    protection_percentage: float = Field(
        ..., description="Percentage of points meeting criteria"
    )
    min_potential_V: float = Field(
        ..., description="Most negative potential [V]"
    )
    max_potential_V: float = Field(
        ..., description="Least negative potential [V]"
    )
    mean_potential_V: float = Field(
        ..., description="Mean potential [V]"
    )
    deficiency_locations: list[float] = Field(
        default_factory=list,
        description="Distances of underprotected points [m]",
    )


class DCVGIndication(BaseModel):
    """A DCVG (Direct Current Voltage Gradient) survey indication."""

    distance_m: float = Field(..., description="Distance along pipeline [m]")
    voltage_gradient_mV: float = Field(
        ..., description="Measured voltage gradient [mV]"
    )
    ir_drop_percentage: float = Field(
        ...,
        ge=0.0,
        le=100.0,
        description="IR drop as % of total pipe-to-soil potential shift",
    )
    severity: str = Field(default="", description="Severity classification")


class AttenuationResult(BaseModel):
    """Result of potential attenuation analysis."""

    attenuation_constant_per_km: float = Field(
        ..., description="Attenuation constant [1/km]"
    )
    characteristic_length_km: float = Field(
        ..., description="Characteristic attenuation length [km]"
    )
    pipe_resistance_ohm_per_km: float = Field(
        ..., description="Pipe longitudinal resistance [ohm/km]"
    )
    leakage_conductance_S_per_km: float = Field(
        ..., description="Leakage conductance [S/km]"
    )


CRITERIA_NOTE = "user to confirm against the governing code"
SEVERITIES = ("minor", "moderate", "severe", "very_severe")


class SurveyInput(BaseModel):
    """Finite, validated inputs for synthetic and user-supplied survey criteria."""

    model_config = ConfigDict(allow_inf_nan=False, extra="forbid", frozen=True)


class PCMSurveyPoint(SurveyInput):
    """PCM signal amplitude in mA at a non-negative pipeline chainage."""

    distance_m: float = Field(ge=0)
    signal_current_mA: float = Field(gt=0)


class CriteriaProvenance(SurveyInput):
    """Explicit basis reference and confirmation supplied with assessment criteria."""

    kind: Literal["engineering", "demonstration"]
    reference: str
    confirmed: bool = Field(strict=True)

    @field_validator("reference")
    @classmethod
    def nonblank_reference(cls, value: str) -> str:
        """Require a source reference rather than an empty provenance record."""
        if not value.strip():
            raise ValueError("reference must not be blank")
        return value.strip()

    @model_validator(mode="after")
    def engineering_confirmation(self) -> CriteriaProvenance:
        """Reject engineering criteria without an explicit confirmation record."""
        if self.kind == "engineering" and not self.confirmed:
            raise ValueError("engineering criteria require confirmation")
        return self


class PCMConfig(SurveyInput):
    """Explicit inclusive segment threshold and its supplied basis; no defaults."""

    loss_unit: Literal["dB/m", "mA/m"]
    loss_threshold: float = Field(ge=0, description=CRITERIA_NOTE)
    provenance: CriteriaProvenance


class PCMSegment(SurveyInput):
    """Signed current loss over a segment; a gain remains negative."""

    start_m: float
    end_m: float
    distance_m: float
    start_current_mA: float
    end_current_mA: float
    loss_per_m: float
    loss_unit: Literal["dB/m", "mA/m"]
    loss_threshold: float
    flagged: bool
    provenance: CriteriaProvenance


def analyze_pcm_survey(
    survey_points: list[PCMSurveyPoint], config: PCMConfig | None = None,
) -> list[PCMSegment]:
    """Sort by chainage and flag signed loss >= threshold at segment midpoints.

    Decibel loss uses current amplitude: 20 log10(I_start / I_end) / length,
    following NIST SP 811 (2008), section 8.7, Logarithmic quantities and units.
    Duplicate chainages and non-positive currents are rejected. Zero or one
    reading yields no segment. Every threshold is a user input; user to confirm
    against the governing code.
    """
    if config is None:
        raise ValueError("PCM requires explicit criteria and provenance")
    config = PCMConfig.model_validate(config.model_dump())
    points = sorted(
        (PCMSurveyPoint.model_validate(p.model_dump()) for p in survey_points),
        key=lambda p: p.distance_m,
    )
    segments = []
    for start, end in zip(points, points[1:]):
        length = end.distance_m - start.distance_m
        if length == 0:
            raise ValueError("duplicate PCM chainage")
        if config.loss_unit == "dB/m":
            loss = 20 * (math.log10(start.signal_current_mA)
                         - math.log10(end.signal_current_mA)) / length
        else:
            loss = (start.signal_current_mA - end.signal_current_mA) / length
        segments.append(PCMSegment(
            start_m=start.distance_m, end_m=end.distance_m,
            distance_m=start.distance_m + length / 2,
            start_current_mA=start.signal_current_mA,
            end_current_mA=end.signal_current_mA,
            loss_per_m=loss, loss_unit=config.loss_unit,
            loss_threshold=config.loss_threshold, flagged=loss >= config.loss_threshold,
            provenance=config.provenance,
        ))
    return segments


class ACVGIndication(SurveyInput):
    """ACVG reading; direction describes an optional along-pipeline indication."""

    distance_m: float = Field(ge=0)
    reading: float
    unit: Literal["dB_uV", "relative"]
    direction: Literal["upstream", "downstream"] | None = None
    severity: str = ""
    provenance: CriteriaProvenance | None = None


class ACVGBands(SurveyInput):
    """Inclusive lower bands; user to confirm against the governing code.

    All boundaries and their provenance are required. No conversion
    between dB microvolts and instrument-relative readings is inferred.
    """

    unit: Literal["dB_uV", "relative"]
    moderate_min: float = Field(description=CRITERIA_NOTE)
    severe_min: float = Field(description=CRITERIA_NOTE)
    very_severe_min: float = Field(description=CRITERIA_NOTE)
    provenance: CriteriaProvenance

    @model_validator(mode="after")
    def ordered_bands(self) -> ACVGBands:
        """Require strictly increasing severity boundaries."""
        if not self.moderate_min < self.severe_min < self.very_severe_min:
            raise ValueError("ACVG bands must be strictly increasing")
        return self


def classify_acvg_indication(
    indication: ACVGIndication, bands: ACVGBands | None = None,
) -> ACVGIndication:
    """Return a classified copy using unit-matched, user-set severity bands."""
    if bands is None:
        raise ValueError("ACVG requires explicit criteria and provenance")
    bands = ACVGBands.model_validate(bands.model_dump())
    indication = ACVGIndication.model_validate(indication.model_dump())
    if indication.unit != bands.unit:
        raise ValueError("ACVG reading unit must match the severity bands unit")
    thresholds = (bands.moderate_min, bands.severe_min, bands.very_severe_min)
    severity = SEVERITIES[sum(indication.reading >= edge for edge in thresholds)]
    return indication.model_copy(update={"severity": severity, "provenance": bands.provenance})


def analyze_cis_survey(
    survey_points: list[CISSurveyPoint],
    use_off_potential: bool = True,
    protection_criterion_V: float = PROTECTION_POTENTIAL_CSE,
    overprotection_limit_V: float = OVERPROTECTION_LIMIT_CSE,
) -> CISAnalysisResult:
    """Analyze close-interval survey data for protection compliance.

    Evaluates each survey point against the protection criterion
    (default -0.850 V vs CSE for instant-off potentials per NACE SP0169).

    Parameters
    ----------
    survey_points : list[CISSurveyPoint]
        CIS survey data points.
    use_off_potential : bool
        Use OFF (instant-off) potentials if available (default True).
    protection_criterion_V : float
        Protection potential criterion [V vs CSE].
    overprotection_limit_V : float
        Overprotection limit [V vs CSE].

    Returns
    -------
    CISAnalysisResult
        Survey analysis statistics and deficiency locations.
    """
    if not survey_points:
        return CISAnalysisResult(
            total_points=0,
            protected_points=0,
            underprotected_points=0,
            overprotected_points=0,
            protection_percentage=0.0,
            min_potential_V=0.0,
            max_potential_V=0.0,
            mean_potential_V=0.0,
            deficiency_locations=[],
        )

    potentials: list[float] = []
    protected = 0
    underprotected = 0
    overprotected = 0
    deficiencies: list[float] = []

    for point in survey_points:
        if use_off_potential and point.off_potential_V is not None:
            v = point.off_potential_V
        else:
            v = point.on_potential_V

        potentials.append(v)

        if v <= overprotection_limit_V:
            overprotected += 1
        elif v <= protection_criterion_V:
            protected += 1
        else:
            underprotected += 1
            deficiencies.append(point.distance_m)

    total = len(potentials)
    protection_pct = (protected + overprotected) / total * 100.0 if total > 0 else 0.0

    return CISAnalysisResult(
        total_points=total,
        protected_points=protected,
        underprotected_points=underprotected,
        overprotected_points=overprotected,
        protection_percentage=round(protection_pct, 1),
        min_potential_V=min(potentials),
        max_potential_V=max(potentials),
        mean_potential_V=round(sum(potentials) / total, 4),
        deficiency_locations=deficiencies,
    )


def classify_dcvg_indication(
    voltage_gradient_mV: float,
    pipe_to_soil_shift_mV: float,
) -> DCVGIndication:
    """Classify a DCVG survey indication by severity.

    IR drop percentage = (voltage_gradient / pipe_to_soil_shift) * 100

    Severity per NACE SP0502:
        - Minor: <15% IR
        - Moderate: 15-35% IR
        - Severe: 35-60% IR
        - Very Severe: >60% IR

    Parameters
    ----------
    voltage_gradient_mV : float
        Measured voltage gradient at the indication [mV].
    pipe_to_soil_shift_mV : float
        Total pipe-to-soil potential shift [mV].

    Returns
    -------
    DCVGIndication
        Classification with severity rating.
    """
    if pipe_to_soil_shift_mV <= 0:
        ir_pct = 0.0
    else:
        ir_pct = (abs(voltage_gradient_mV) / abs(pipe_to_soil_shift_mV)) * 100.0

    ir_pct = min(ir_pct, 100.0)

    if ir_pct < 15.0:
        severity = "minor"
    elif ir_pct < 35.0:
        severity = "moderate"
    elif ir_pct < 60.0:
        severity = "severe"
    else:
        severity = "very_severe"

    return DCVGIndication(
        distance_m=0.0,
        voltage_gradient_mV=voltage_gradient_mV,
        ir_drop_percentage=round(ir_pct, 1),
        severity=severity,
    )


def attenuation_analysis(
    pipe_od_m: float,
    pipe_wt_m: float,
    pipe_resistivity_ohm_m: float = 1.7e-7,
    coating_resistance_ohm_m2: float = 10000.0,
    pipe_diameter_m_for_coating: Optional[float] = None,
) -> AttenuationResult:
    """Calculate pipeline potential attenuation parameters.

    The attenuation of CP potential along a pipeline is governed by:
        alpha = sqrt(R_pipe * G_leak)

    where R_pipe is the longitudinal pipe resistance per unit length and
    G_leak is the coating leakage conductance per unit length.

    Parameters
    ----------
    pipe_od_m : float
        Pipe outer diameter [m].
    pipe_wt_m : float
        Pipe wall thickness [m].
    pipe_resistivity_ohm_m : float
        Pipe steel resistivity [ohm-m] (default 1.7e-7 for carbon steel).
    coating_resistance_ohm_m2 : float
        Specific coating resistance [ohm-m²] (default 10000).
    pipe_diameter_m_for_coating : float, optional
        Diameter for coating leakage (defaults to pipe_od_m).

    Returns
    -------
    AttenuationResult
        Attenuation constant, characteristic length, and component values.
    """
    if pipe_diameter_m_for_coating is None:
        pipe_diameter_m_for_coating = pipe_od_m

    # Cross-sectional area of pipe wall
    r_outer = pipe_od_m / 2.0
    r_inner = r_outer - pipe_wt_m
    cross_section_m2 = math.pi * (r_outer**2 - r_inner**2)

    # Pipe longitudinal resistance [ohm/m]
    r_pipe_per_m = pipe_resistivity_ohm_m / cross_section_m2
    r_pipe_per_km = r_pipe_per_m * 1000.0

    # Coating leakage conductance [S/m]
    circumference = math.pi * pipe_diameter_m_for_coating
    g_leak_per_m = circumference / coating_resistance_ohm_m2
    g_leak_per_km = g_leak_per_m * 1000.0

    # Attenuation constant [1/m]
    alpha_per_m = math.sqrt(r_pipe_per_m * g_leak_per_m)
    alpha_per_km = alpha_per_m * 1000.0

    # Characteristic length [m]
    if alpha_per_m > 0:
        char_length_km = 1.0 / alpha_per_km
    else:
        char_length_km = float("inf")

    return AttenuationResult(
        attenuation_constant_per_km=round(alpha_per_km, 6),
        characteristic_length_km=round(char_length_km, 3),
        pipe_resistance_ohm_per_km=round(r_pipe_per_km, 6),
        leakage_conductance_S_per_km=round(g_leak_per_km, 6),
    )
