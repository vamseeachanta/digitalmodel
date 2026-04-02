"""Stray current analysis and mitigation design.

Covers AC and DC interference assessment, mitigation design including
drainage bonds and polarization cells, and railroad/pipeline proximity
effects for buried metallic structures.

References
----------
- NACE SP0169 (2013) §9 — Interference currents
- EN 50162 (2004) "Protection Against Corrosion by Stray Current from DC Systems"
- EN 15280 (2013) "Evaluation of AC Corrosion Likelihood of Buried Pipelines"
- CEN/TS 15280 — AC corrosion threshold criteria
- ISO 18086 (2019) "Corrosion of Metals and Alloys — Determination of AC
  Corrosion — Protection Criteria"
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class InterferenceType(str, Enum):
    """Type of stray current interference."""

    DC_TRANSIT = "dc_transit"
    DC_HVDC = "dc_hvdc"
    DC_MINING = "dc_mining"
    AC_POWERLINE = "ac_powerline"
    AC_RAILROAD = "ac_railroad"
    TELLURIC = "telluric"


class MitigationType(str, Enum):
    """Stray current mitigation methods."""

    DRAINAGE_BOND = "drainage_bond"
    POLARIZATION_CELL = "polarization_cell"
    GALVANIC_ANODE = "galvanic_anode"
    COATING_IMPROVEMENT = "coating_improvement"
    INSULATING_JOINT = "insulating_joint"
    GROUNDING_CELL = "grounding_cell"


# AC corrosion thresholds per EN 15280 / ISO 18086
AC_CURRENT_DENSITY_THRESHOLD_MA_M2: float = 30.0  # mA/m² — above this, AC corrosion risk
AC_VOLTAGE_THRESHOLD_V: float = 15.0  # V rms — safety and corrosion limit
AC_CORROSION_RATIO_THRESHOLD: float = 3.0  # AC/DC current density ratio

# DC stray current thresholds
DC_POTENTIAL_SHIFT_THRESHOLD_MV: float = 20.0  # mV — significant interference
DC_CURRENT_DENSITY_THRESHOLD_MA_M2: float = 1.0  # mA/m² — corrosion risk


class StrayCurrentInput(BaseModel):
    """Input for stray current assessment."""

    interference_type: InterferenceType = Field(
        ..., description="Type of interference source"
    )
    pipeline_od_m: float = Field(
        default=0.3, gt=0, description="Pipeline outer diameter [m]"
    )
    pipeline_length_m: float = Field(
        default=1000.0, gt=0, description="Pipeline length in interference zone [m]"
    )
    separation_distance_m: float = Field(
        default=50.0, gt=0,
        description="Distance from interference source to pipeline [m]",
    )
    soil_resistivity_ohm_m: float = Field(
        default=50.0, gt=0, description="Soil resistivity [ohm-m]"
    )
    coating_resistance_ohm_m2: float = Field(
        default=10000.0, gt=0,
        description="Coating specific resistance [ohm-m²]",
    )
    source_current_A: float = Field(
        default=100.0, ge=0,
        description="Interference source current [A] (DC) or [A rms] (AC)",
    )


class StrayCurrentResult(BaseModel):
    """Result of stray current assessment."""

    interference_type: str = Field(..., description="Type of interference")
    induced_voltage_V: float = Field(
        ..., description="Induced voltage on pipeline [V]"
    )
    induced_current_density_mA_m2: float = Field(
        ..., description="Induced current density [mA/m²]"
    )
    potential_shift_mV: float = Field(
        ..., description="Potential shift at pipeline [mV]"
    )
    risk_level: str = Field(
        ..., description="Risk classification (low/moderate/high/critical)"
    )
    exceeds_threshold: bool = Field(
        ..., description="Whether thresholds are exceeded"
    )
    recommended_mitigation: list[str] = Field(
        default_factory=list,
        description="Recommended mitigation measures",
    )


class MitigationDesign(BaseModel):
    """Stray current mitigation design output."""

    mitigation_type: str = Field(..., description="Selected mitigation method")
    bond_resistance_ohm: Optional[float] = Field(
        None, description="Drainage bond resistance [ohm]"
    )
    bond_current_A: Optional[float] = Field(
        None, description="Expected bond current [A]"
    )
    required_capacity_A: Optional[float] = Field(
        None, description="Required current capacity [A]"
    )
    estimated_effectiveness_pct: float = Field(
        ..., description="Estimated effectiveness [%]"
    )
    notes: str = Field(default="", description="Design notes")


def assess_stray_current(
    input_params: StrayCurrentInput,
) -> StrayCurrentResult:
    """Assess stray current interference on a buried pipeline.

    For DC interference, calculates potential shift based on soil
    resistivity and geometry. For AC interference, estimates induced
    voltage using simplified parallel exposure model.

    Parameters
    ----------
    input_params : StrayCurrentInput
        Assessment input parameters.

    Returns
    -------
    StrayCurrentResult
        Interference assessment with risk level and mitigation recommendations.
    """
    is_ac = input_params.interference_type in (
        InterferenceType.AC_POWERLINE,
        InterferenceType.AC_RAILROAD,
    )

    if is_ac:
        return _assess_ac_interference(input_params)
    else:
        return _assess_dc_interference(input_params)


def _assess_dc_interference(params: StrayCurrentInput) -> StrayCurrentResult:
    """Assess DC stray current interference."""
    # Simplified earth potential model
    # V = (I * rho) / (2 * pi * d) for a remote point source
    rho = params.soil_resistivity_ohm_m
    d = params.separation_distance_m
    I = params.source_current_A

    induced_voltage = (I * rho) / (2.0 * math.pi * d)

    # Current density through coating
    i_density = induced_voltage / params.coating_resistance_ohm_m2 * 1000.0  # mA/m²

    # Potential shift
    potential_shift = induced_voltage * 1000.0  # mV

    # Risk assessment
    if abs(potential_shift) > 100.0 or i_density > 10.0:
        risk = "critical"
        exceeds = True
        mitigations = [
            MitigationType.DRAINAGE_BOND.value,
            MitigationType.POLARIZATION_CELL.value,
            MitigationType.INSULATING_JOINT.value,
        ]
    elif abs(potential_shift) > DC_POTENTIAL_SHIFT_THRESHOLD_MV:
        risk = "high"
        exceeds = True
        mitigations = [
            MitigationType.DRAINAGE_BOND.value,
            MitigationType.GALVANIC_ANODE.value,
        ]
    elif abs(potential_shift) > 10.0:
        risk = "moderate"
        exceeds = False
        mitigations = [MitigationType.COATING_IMPROVEMENT.value]
    else:
        risk = "low"
        exceeds = False
        mitigations = []

    return StrayCurrentResult(
        interference_type=params.interference_type.value,
        induced_voltage_V=round(induced_voltage, 4),
        induced_current_density_mA_m2=round(i_density, 2),
        potential_shift_mV=round(potential_shift, 1),
        risk_level=risk,
        exceeds_threshold=exceeds,
        recommended_mitigation=mitigations,
    )


def _assess_ac_interference(params: StrayCurrentInput) -> StrayCurrentResult:
    """Assess AC stray current interference."""
    # Simplified parallel exposure model for AC interference
    # V_ac ≈ (I * omega * mu_0 * L) / (2 * pi) * ln(D/d)
    # Simplified: V_ac = E_field * L_exposure
    # where E_field ≈ (I * rho) / (2 * pi * d^2) * correction

    rho = params.soil_resistivity_ohm_m
    d = params.separation_distance_m
    L = params.pipeline_length_m
    I = params.source_current_A

    # Mutual impedance approach (simplified)
    # For 50 Hz: omega = 2*pi*50 = 314 rad/s
    omega = 314.16  # 50 Hz
    mu_0 = 4.0e-7 * math.pi

    # Carson-Clem formula (simplified)
    De = 658.0 * math.sqrt(rho / 50.0)  # equivalent earth return depth [m]
    mutual_impedance_per_m = omega * mu_0 / (2.0 * math.pi) * math.log(De / d)

    induced_voltage = I * mutual_impedance_per_m * L / 1000.0  # simplified with length factor

    # AC current density
    pipe_circumference = math.pi * params.pipeline_od_m
    ac_current_density = (
        induced_voltage / params.coating_resistance_ohm_m2
    ) * 1000.0 * pipe_circumference  # mA/m² on pipe surface

    potential_shift = induced_voltage * 1000.0  # mV

    # Risk assessment per EN 15280
    if induced_voltage > AC_VOLTAGE_THRESHOLD_V or ac_current_density > AC_CURRENT_DENSITY_THRESHOLD_MA_M2:
        risk = "critical"
        exceeds = True
        mitigations = [
            MitigationType.GROUNDING_CELL.value,
            MitigationType.POLARIZATION_CELL.value,
            MitigationType.COATING_IMPROVEMENT.value,
        ]
    elif ac_current_density > 10.0:
        risk = "high"
        exceeds = True
        mitigations = [
            MitigationType.GROUNDING_CELL.value,
            MitigationType.GALVANIC_ANODE.value,
        ]
    elif ac_current_density > 1.0:
        risk = "moderate"
        exceeds = False
        mitigations = [MitigationType.GALVANIC_ANODE.value]
    else:
        risk = "low"
        exceeds = False
        mitigations = []

    return StrayCurrentResult(
        interference_type=params.interference_type.value,
        induced_voltage_V=round(induced_voltage, 4),
        induced_current_density_mA_m2=round(ac_current_density, 2),
        potential_shift_mV=round(potential_shift, 1),
        risk_level=risk,
        exceeds_threshold=exceeds,
        recommended_mitigation=mitigations,
    )


def design_drainage_bond(
    stray_current_A: float,
    source_potential_V: float = -0.50,
    pipeline_potential_V: float = -0.85,
    max_bond_current_A: float = 10.0,
) -> MitigationDesign:
    """Design a drainage bond for DC stray current mitigation.

    A drainage bond provides a low-resistance return path for stray
    current, preventing it from discharging through the soil/coating.

    Parameters
    ----------
    stray_current_A : float
        Estimated stray current to be drained [A].
    source_potential_V : float
        Potential of the stray current source structure [V].
    pipeline_potential_V : float
        Pipeline protection potential [V].
    max_bond_current_A : float
        Maximum allowable bond current [A].

    Returns
    -------
    MitigationDesign
        Drainage bond design specification.
    """
    # Bond resistance to limit current
    delta_v = abs(source_potential_V - pipeline_potential_V)

    if stray_current_A > 0:
        target_resistance = delta_v / min(stray_current_A, max_bond_current_A)
    else:
        target_resistance = delta_v / max_bond_current_A

    bond_current = delta_v / target_resistance if target_resistance > 0 else 0.0

    # Effectiveness estimate
    if bond_current >= stray_current_A * 0.9:
        effectiveness = 95.0
    elif bond_current >= stray_current_A * 0.7:
        effectiveness = 80.0
    else:
        effectiveness = 60.0

    notes = (
        f"Bond resistance: {target_resistance:.4f} ohm. "
        f"Verify bond current direction periodically. "
        f"Install with ammeter shunt for monitoring."
    )

    return MitigationDesign(
        mitigation_type=MitigationType.DRAINAGE_BOND.value,
        bond_resistance_ohm=round(target_resistance, 4),
        bond_current_A=round(bond_current, 2),
        required_capacity_A=round(max(stray_current_A, bond_current) * 1.5, 2),
        estimated_effectiveness_pct=effectiveness,
        notes=notes,
    )
