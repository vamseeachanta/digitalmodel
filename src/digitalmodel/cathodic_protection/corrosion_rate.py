"""Corrosion rate prediction models.

Implements industry-standard corrosion rate prediction including:
- de Waard-Milliams CO2 corrosion model
- Norsok M-506 CO2 corrosion model (simplified)
- H2S corrosion / sour service assessment
- Galvanic corrosion prediction
- Pitting rate estimation

References
----------
- de Waard, C. & Milliams, D.E. "Carbonic Acid Corrosion of Steel",
  Corrosion, Vol. 31, No. 5, 1975
- NORSOK M-506 (2005) "CO2 Corrosion Rate Calculation Model"
- NACE MR0175 / ISO 15156 "Materials for Use in H2S-Containing Environments"
- DNV-RP-B101 "Corrosion Protection of Floating Production and Storage Units"
"""

from __future__ import annotations

import math

from pydantic import BaseModel, Field

from digitalmodel.cathodic_protection._experimental import require_experimental


class CO2CorrosionInput(BaseModel):
    """Input for CO2 corrosion rate prediction."""

    temperature_c: float = Field(..., description="Temperature [°C]")
    co2_partial_pressure_bar: float = Field(
        ..., gt=0, description="CO2 partial pressure [bar]"
    )
    ph: float = Field(default=4.5, ge=0, le=14, description="pH of solution")
    flow_velocity_m_s: float = Field(
        default=1.0, ge=0, description="Flow velocity [m/s]"
    )
    pipe_diameter_m: float = Field(
        default=0.2, gt=0, description="Pipe inner diameter [m]"
    )


class CO2CorrosionResult(BaseModel):
    """Result of CO2 corrosion rate prediction."""

    corrosion_rate_mm_yr: float = Field(
        ..., description="Predicted corrosion rate [mm/year]"
    )
    model_used: str = Field(..., description="Corrosion model name")
    temperature_c: float = Field(..., description="Temperature [°C]")
    co2_pressure_bar: float = Field(..., description="CO2 partial pressure [bar]")
    fugacity_correction: float = Field(
        default=1.0, description="CO2 fugacity correction factor"
    )


class GalvanicCorrosionInput(BaseModel):
    """Input for galvanic corrosion prediction."""

    anode_material: str = Field(
        default="carbon_steel", description="Anodic (corroding) material"
    )
    cathode_material: str = Field(
        default="stainless_steel", description="Cathodic (noble) material"
    )
    anode_area_m2: float = Field(..., gt=0, description="Anode surface area [m²]")
    cathode_area_m2: float = Field(..., gt=0, description="Cathode surface area [m²]")
    electrolyte_resistivity_ohm_m: float = Field(
        default=0.25, gt=0, description="Electrolyte resistivity [ohm-m]"
    )


class GalvanicCorrosionResult(BaseModel):
    """Result of galvanic corrosion prediction."""

    galvanic_current_density_mA_m2: float = Field(
        ..., description="Galvanic corrosion current density [mA/m²]"
    )
    corrosion_rate_mm_yr: float = Field(
        ..., description="Predicted galvanic corrosion rate [mm/year]"
    )
    area_ratio: float = Field(
        ..., description="Cathode-to-anode area ratio"
    )
    risk_level: str = Field(
        ..., description="Risk classification (low/moderate/high/very_high)"
    )


# Galvanic series potentials in seawater [V vs Ag/AgCl]
GALVANIC_POTENTIAL: dict[str, float] = {
    "magnesium": -1.60,
    "zinc": -1.03,
    "aluminum_alloy": -0.87,
    "carbon_steel": -0.65,
    "cast_iron": -0.61,
    "stainless_steel_304": -0.08,
    "stainless_steel_316": -0.05,
    "copper": -0.20,
    "bronze": -0.24,
    "titanium": -0.05,
    "hastelloy": -0.04,
    "platinum": +0.22,
    "graphite": +0.25,
}

# Faraday penetration-rate factors [mm/yr per mA/m²].
#
# Derivation (issue #2209 — the previous hard-coded values were 10x too high):
#     rate [mm/yr per mA/m²]
#       = (M / (z * F)) [g/C]          mass per coulomb
#       * 1e-3 [A/m² per mA/m²]        one milliampere per square metre
#       * SECONDS_PER_YEAR [s/yr]
#       / (rho * 1e3) [g/m³]           density, kg/m³ -> g/m³
#       * 1e3 [mm/m]
#       = M * SECONDS_PER_YEAR * 1e-3 / (z * F * rho)
#
#   Fe : M = 55.85 g/mol, z = 2, rho = 7870 kg/m³ -> 0.0011605
#   Al : M = 26.98,       z = 3, rho = 2700       -> 0.0010894
#   Cu : M = 63.55,       z = 2, rho = 8960       -> 0.0011599
#   Zn : M = 65.38,       z = 2, rho = 7140       -> 0.0014975
#   Cast iron is treated as Fe here (this module does not carry a separate
#   cast-iron density; with rho = 7200 the factor would be 0.0012685).
FARADAY_C_PER_MOL: float = 96485.33212
SECONDS_PER_YEAR: float = 3.15576e7  # Julian year, 365.25 d


def faraday_rate_factor(molar_mass_g_mol: float, valence: int, density_kg_m3: float) -> float:
    """Penetration rate [mm/yr] produced by 1 mA/m² of anodic current (Faraday).

    Parameters
    ----------
    molar_mass_g_mol : float
        Atomic/molar mass M [g/mol].
    valence : int
        Electrons per dissolved atom z.
    density_kg_m3 : float
        Metal density rho [kg/m³].

    Returns
    -------
    float
        Rate factor [mm/yr per mA/m²].
    """
    return (
        molar_mass_g_mol
        * SECONDS_PER_YEAR
        * 1e-3
        / (valence * FARADAY_C_PER_MOL * density_kg_m3)
    )


CORROSION_RATE_FACTOR: dict[str, float] = {
    "carbon_steel": faraday_rate_factor(55.85, 2, 7870.0),  # 0.0011605
    "cast_iron": faraday_rate_factor(55.85, 2, 7870.0),  # as Fe (see note above)
    "aluminum_alloy": faraday_rate_factor(26.98, 3, 2700.0),  # 0.0010894
    "copper": faraday_rate_factor(63.55, 2, 8960.0),  # 0.0011599
    "zinc": faraday_rate_factor(65.38, 2, 7140.0),  # 0.0014975
}


def de_waard_milliams_co2(
    input_params: CO2CorrosionInput,
) -> CO2CorrosionResult:
    """Predict CO2 corrosion rate using the de Waard-Milliams model (1975).

    The base corrosion rate is:
        log10(V_cor) = 5.8 - 1710/(T+273) + 0.67*log10(pCO2)

    where V_cor is in mm/year, T in °C, and pCO2 in bar.

    A pH correction factor is applied for pH > 4:
        f_pH = 10^(0.32 * (pH - 4))

    Parameters
    ----------
    input_params : CO2CorrosionInput
        Corrosion input parameters.

    Returns
    -------
    CO2CorrosionResult
        Predicted corrosion rate and model details.
    """
    T = input_params.temperature_c
    pCO2 = input_params.co2_partial_pressure_bar

    # de Waard-Milliams base equation
    log_vcor = 5.8 - 1710.0 / (T + 273.15) + 0.67 * math.log10(pCO2)
    vcor = 10.0**log_vcor

    # pH correction (higher pH reduces corrosion)
    if input_params.ph > 4.0:
        ph_factor = 10.0 ** (0.32 * (input_params.ph - 4.0))
        vcor = vcor / ph_factor

    # Fugacity correction for high pressures (>10 bar)
    fugacity_corr = 1.0
    if pCO2 > 10.0:
        fugacity_corr = 0.9  # simplified correction
        vcor = vcor * fugacity_corr

    return CO2CorrosionResult(
        corrosion_rate_mm_yr=round(max(vcor, 0.0), 4),
        model_used="de_Waard_Milliams_1975",
        temperature_c=T,
        co2_pressure_bar=pCO2,
        fugacity_correction=fugacity_corr,
    )


def norsok_m506_co2(
    temperature_c: float,
    co2_partial_pressure_bar: float,
    ph: float = 4.5,
    wall_shear_stress_Pa: float = 10.0,
) -> CO2CorrosionResult:
    """Predict CO2 corrosion rate using simplified NORSOK M-506 model.

    The NORSOK M-506 model uses:
        V_cor = K_t * f(pCO2) * f(pH) * f(shear)

    where K_t is a temperature-dependent rate constant.

    This is a simplified implementation suitable for screening assessments.
    The full NORSOK M-506 model uses detailed lookup tables.

    Parameters
    ----------
    temperature_c : float
        Temperature [°C].
    co2_partial_pressure_bar : float
        CO2 partial pressure [bar].
    ph : float
        Solution pH.
    wall_shear_stress_Pa : float
        Wall shear stress [Pa].

    Returns
    -------
    CO2CorrosionResult
        Predicted corrosion rate.
    """
    T = temperature_c

    # Temperature factor (NORSOK M-506 simplified)
    if T <= 15.0:
        kt = 0.42
    elif T <= 60.0:
        kt = 0.42 + (T - 15.0) * 0.066  # linear interpolation
    elif T <= 120.0:
        kt = 3.4 + (T - 60.0) * 0.023
    else:
        kt = 4.8  # cap at high temperature

    # CO2 pressure factor
    f_co2 = co2_partial_pressure_bar**0.62

    # pH factor
    if ph < 3.5:
        f_ph = 3.0
    elif ph < 4.6:
        f_ph = 3.0 - (ph - 3.5) * 1.82
    elif ph < 6.5:
        f_ph = 1.0 - (ph - 4.6) * 0.53
    else:
        f_ph = 0.01

    f_ph = max(f_ph, 0.01)

    # Shear stress factor
    f_shear = (wall_shear_stress_Pa / 10.0) ** 0.15

    vcor = kt * f_co2 * f_ph * f_shear

    return CO2CorrosionResult(
        corrosion_rate_mm_yr=round(max(vcor, 0.0), 4),
        model_used="NORSOK_M506_simplified",
        temperature_c=T,
        co2_pressure_bar=co2_partial_pressure_bar,
        fugacity_correction=1.0,
    )


def galvanic_corrosion(
    input_params: GalvanicCorrosionInput,
    *,
    experimental: bool = False,
) -> GalvanicCorrosionResult:
    """Predict galvanic corrosion rate from dissimilar metal coupling.

    Uses the galvanic series potential difference and cathode-to-anode
    area ratio to estimate the driving force and corrosion current density.

    The galvanic current density on the anode is:
        i_galv = (E_cathode - E_anode) / (rho * d_eff) * (A_cathode / A_anode)

    where d_eff is an effective electrolyte path length.

    Experimental
    ------------
    Quarantined (issue #2209): the model is ohmic-only — it divides the
    open-circuit potential difference by an assumed 0.1 m electrolyte path
    and ignores anodic/cathodic polarisation, so it overstates the coupling
    current by orders of magnitude for real couples. A re-model must follow
    BS PD 6484 (galvanic corrosion guidance: mixed-potential / polarisation
    based coupling). Calling without ``experimental=True`` raises
    :class:`~digitalmodel.cathodic_protection._experimental.ExperimentalModelError`.

    Parameters
    ----------
    input_params : GalvanicCorrosionInput
        Material, area, and electrolyte data. Both materials must be keys of
        ``GALVANIC_POTENTIAL``; unknown names raise ``ValueError``.
    experimental : bool
        Acknowledge the quarantine and run the model anyway.

    Returns
    -------
    GalvanicCorrosionResult
        Galvanic corrosion current density, rate, and risk level.

    Raises
    ------
    ExperimentalModelError
        If ``experimental`` is false.
    ValueError
        If either material is not in ``GALVANIC_POTENTIAL``.
    """
    require_experimental(
        experimental,
        model="corrosion_rate.galvanic_corrosion",
        reason="ohmic-only coupling with an assumed 0.1 m path and no polarisation",
        standard="BS PD 6484",
    )
    for label, material in (
        ("anode_material", input_params.anode_material),
        ("cathode_material", input_params.cathode_material),
    ):
        if material not in GALVANIC_POTENTIAL:
            raise ValueError(
                f"unknown {label} {material!r}; expected one of "
                f"{sorted(GALVANIC_POTENTIAL)}"
            )
    e_anode = GALVANIC_POTENTIAL[input_params.anode_material]
    e_cathode = GALVANIC_POTENTIAL[input_params.cathode_material]

    delta_e = abs(e_cathode - e_anode)
    area_ratio = input_params.cathode_area_m2 / input_params.anode_area_m2

    # Effective path length (simplified)
    d_eff = 0.1  # m, typical for close-coupled dissimilar metals

    # Galvanic current density on the anode [A/m²]
    i_galv = (delta_e / (input_params.electrolyte_resistivity_ohm_m * d_eff)) * area_ratio
    i_galv_mA = i_galv * 1000.0  # convert to mA/m²

    # Corrosion rate from Faraday's law
    cr_factor = CORROSION_RATE_FACTOR.get(
        input_params.anode_material, CORROSION_RATE_FACTOR["carbon_steel"]
    )
    corrosion_rate = i_galv_mA * cr_factor

    # Risk classification
    if corrosion_rate < 0.1:
        risk = "low"
    elif corrosion_rate < 0.5:
        risk = "moderate"
    elif corrosion_rate < 2.0:
        risk = "high"
    else:
        risk = "very_high"

    return GalvanicCorrosionResult(
        galvanic_current_density_mA_m2=round(i_galv_mA, 2),
        corrosion_rate_mm_yr=round(corrosion_rate, 4),
        area_ratio=round(area_ratio, 3),
        risk_level=risk,
    )


def pitting_rate_estimate(
    general_corrosion_rate_mm_yr: float,
    pitting_factor: float = 3.0,
    confidence_level: str = "mean",
) -> float:
    """Estimate pitting corrosion rate from general corrosion rate.

    Pitting rate = general_rate * pitting_factor

    Typical pitting factors:
        - Mean estimate: 3.0
        - 90th percentile: 5.0
        - Maximum (extreme): 8.0-10.0

    Parameters
    ----------
    general_corrosion_rate_mm_yr : float
        General (uniform) corrosion rate [mm/year].
    pitting_factor : float
        Ratio of pit depth to general corrosion depth.
    confidence_level : str
        "mean", "p90", or "maximum" — adjusts the pitting factor.

    Returns
    -------
    float
        Estimated pitting rate [mm/year].
    """
    factor_adjustment = {
        "mean": 1.0,
        "p90": 5.0 / 3.0,
        "maximum": 10.0 / 3.0,
    }

    adjustment = factor_adjustment.get(confidence_level, 1.0)
    return general_corrosion_rate_mm_yr * pitting_factor * adjustment
