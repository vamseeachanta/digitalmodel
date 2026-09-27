"""Corrosion rate prediction models.

Implements industry-standard corrosion rate prediction including:
- de Waard-Milliams CO2 corrosion model
- Norsok M-506 CO2 corrosion model (simplified)
- H2S corrosion / sour service assessment
- Galvanic corrosion by mixed-potential theory (provisional, issue #2247)
- Pitting rate estimation

References
----------
- de Waard, C. & Milliams, D.E. "Carbonic Acid Corrosion of Steel",
  Corrosion, Vol. 31, No. 5, 1975
- NORSOK M-506 (2005) "CO2 Corrosion Rate Calculation Model"
- NACE MR0175 / ISO 15156 "Materials for Use in H2S-Containing Environments"
- DNV-RP-B101 "Corrosion Protection of Floating Production and Storage Units"
- Dunn, D.S. & Cragnolino, G.A. "An Analysis of Galvanic Coupling Effects on
  the Performance of High-Level Nuclear Waste Container Materials",
  CNWRA 97-010, US NRC, 1997 (mixed-potential galvanic model)
"""

from __future__ import annotations

import math

from typing import Optional

from pydantic import BaseModel, Field, model_validator
from scipy.optimize import brentq

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


class GalvanicCorrosionInput(BaseModel):
    """Input for the mixed-potential (Evans diagram) galvanic model.

    All polarisation parameters are explicit inputs; there is no default
    table (issue #2247: no open-literature source was found that gives a
    complete, citable parameter set per material pair). Potentials must be
    on one reference scale (any, e.g. V vs Ag/AgCl/seawater). Current
    densities are in A/m² and Tafel slopes in V/decade.
    """

    anode_corrosion_potential_V: float = Field(
        ..., description="Free corrosion potential of the anode metal E_corr,a [V]"
    )
    anode_corrosion_current_density_A_m2: float = Field(
        ...,
        gt=0,
        description="Anodic Tafel intercept at E_corr,a, i_corr,a [A/m²]",
    )
    anode_tafel_slope_V: float = Field(
        ..., gt=0, description="Anodic Tafel slope beta_a [V/decade]"
    )
    cathode_corrosion_potential_V: float = Field(
        ..., description="Free corrosion potential of the cathode metal E_corr,c [V]"
    )
    cathode_corrosion_current_density_A_m2: float = Field(
        ...,
        gt=0,
        description="Cathodic (O2 reduction) Tafel intercept at E_corr,c, i_corr,c [A/m²]",
    )
    cathode_tafel_slope_V: float = Field(
        ..., gt=0, description="Cathodic Tafel slope beta_c [V/decade] (magnitude)"
    )
    cathode_limiting_current_density_A_m2: float = Field(
        ...,
        gt=0,
        description=(
            "Oxygen-diffusion limiting current density on the cathode i_L [A/m²] "
            "(see oxygen_limiting_current_density); math.inf = activation control only"
        ),
    )
    anode_area_m2: float = Field(..., gt=0, description="Anode wetted area A_a [m²]")
    cathode_area_m2: float = Field(..., gt=0, description="Cathode wetted area A_c [m²]")
    solution_resistance_ohm: float = Field(
        default=0.0,
        ge=0,
        description="Electrolyte (couple) resistance between anode and cathode R_s [ohm]",
    )
    anode_material: Optional[str] = Field(
        default=None,
        description=(
            "Key of CORROSION_RATE_FACTOR for the Faraday conversion (unknown keys "
            "raise); alternatively give molar mass, valence and density"
        ),
    )
    anode_molar_mass_g_mol: Optional[float] = Field(default=None, gt=0)
    anode_valence: Optional[int] = Field(default=None, gt=0)
    anode_density_kg_m3: Optional[float] = Field(default=None, gt=0)

    @model_validator(mode="after")
    def _check(self) -> "GalvanicCorrosionInput":
        if self.cathode_corrosion_potential_V <= self.anode_corrosion_potential_V:
            raise ValueError(
                "cathode_corrosion_potential_V must be more positive (noble) than "
                "anode_corrosion_potential_V; swap the metals"
            )
        if self.anode_material is not None:
            if self.anode_material not in CORROSION_RATE_FACTOR:
                raise ValueError(
                    f"unknown anode_material {self.anode_material!r}; expected one of "
                    f"{sorted(CORROSION_RATE_FACTOR)} or explicit molar mass, "
                    "valence and density"
                )
        elif (
            self.anode_molar_mass_g_mol is None
            or self.anode_valence is None
            or self.anode_density_kg_m3 is None
        ):
            raise ValueError(
                "give anode_material or all of anode_molar_mass_g_mol, "
                "anode_valence and anode_density_kg_m3"
            )
        return self

    def rate_factor(self) -> float:
        """Faraday penetration factor [mm/yr per mA/m²] for the anode metal."""
        if self.anode_material is not None:
            return CORROSION_RATE_FACTOR[self.anode_material]
        if (
            self.anode_molar_mass_g_mol is None
            or self.anode_valence is None
            or self.anode_density_kg_m3 is None
        ):  # pragma: no cover - guarded by the validator
            raise ValueError("anode Faraday data missing")
        return faraday_rate_factor(
            self.anode_molar_mass_g_mol, self.anode_valence, self.anode_density_kg_m3
        )


class GalvanicCorrosionResult(BaseModel):
    """Result of the mixed-potential galvanic model."""

    couple_potential_V: float = Field(
        ...,
        description=(
            "Potential of the anode surface in the couple [V]; equals the cathode "
            "potential when solution_resistance_ohm = 0"
        ),
    )
    anode_potential_V: float = Field(..., description="Polarised anode potential [V]")
    cathode_potential_V: float = Field(..., description="Polarised cathode potential [V]")
    galvanic_current_A: float = Field(..., description="Net galvanic current I_g [A]")
    galvanic_current_density_A_m2: float = Field(
        ..., description="Net galvanic current density on the anode I_g/A_a [A/m²]"
    )
    anodic_current_density_A_m2: float = Field(
        ...,
        description=(
            "Total anodic dissolution current density on the anode "
            "i_corr,a + I_g/A_a [A/m²]"
        ),
    )
    cathodic_current_density_A_m2: float = Field(
        ..., description="Net cathodic current density on the cathode I_g/A_c [A/m²]"
    )
    diffusion_fraction: float = Field(
        ...,
        description="O2-reduction current / i_L on the cathode (1 = fully diffusion limited)",
    )
    corrosion_rate_mm_yr: float = Field(
        ...,
        description="Anode penetration rate from the total anodic current density [mm/yr]",
    )
    galvanic_corrosion_rate_mm_yr: float = Field(
        ..., description="Part of the rate caused by the coupling, from I_g/A_a [mm/yr]"
    )
    area_ratio: float = Field(..., description="Cathode-to-anode area ratio A_c/A_a")
    converged: bool = Field(..., description="Bracketed root finder converged")
    residual_V: float = Field(..., description="Potential-balance residual at the root [V]")


def oxygen_limiting_current_density(
    diffusion_coefficient_m2_s: float,
    bulk_concentration_mol_m3: float,
    diffusion_layer_thickness_m: float,
    electrons: int = 4,
) -> float:
    """Oxygen-diffusion limiting current density i_L = n F D C_bulk / delta [A/m²].

    The limiting term of Dunn & Cragnolino (1997), CNWRA 97-010 eq. 2-26
    (n = 4 electrons per O2). All inputs are required physical data; this
    module carries no default dissolved-oxygen or diffusion-layer value.
    """
    for name, v in (
        ("diffusion_coefficient_m2_s", diffusion_coefficient_m2_s),
        ("bulk_concentration_mol_m3", bulk_concentration_mol_m3),
        ("diffusion_layer_thickness_m", diffusion_layer_thickness_m),
    ):
        if not v > 0:
            raise ValueError(f"{name} must be positive")
    return (
        electrons
        * FARADAY_C_PER_MOL
        * diffusion_coefficient_m2_s
        * bulk_concentration_mol_m3
        / diffusion_layer_thickness_m
    )


def _o2_reduction_density(p: GalvanicCorrosionInput, potential_V: float) -> float:
    """O2 reduction on the cathode: Tafel with diffusion limit (eq. 2-26).

    ``i_act = i_corr,c * 10**(-(E - E_corr,c)/beta_c)``;
    ``i_O2 = i_act / (1 + i_act/i_L)``, i.e. ``1/i_O2 = 1/i_act + 1/i_L``.
    """
    i_act: float = p.cathode_corrosion_current_density_A_m2 * math.pow(
        10.0, -(potential_V - p.cathode_corrosion_potential_V) / p.cathode_tafel_slope_V
    )
    i_lim = p.cathode_limiting_current_density_A_m2
    return i_act if math.isinf(i_lim) else i_act / (1.0 + i_act / i_lim)


def _anode_potential(p: GalvanicCorrosionInput, current_A: float) -> float:
    """E_a(I): invert ``I/A_a = i_corr,a * (10**((E-E_corr,a)/beta_a) - 1)``."""
    return p.anode_corrosion_potential_V + p.anode_tafel_slope_V * math.log10(
        1.0 + current_A / (p.anode_area_m2 * p.anode_corrosion_current_density_A_m2)
    )


def _cathode_potential(p: GalvanicCorrosionInput, current_A: float) -> float:
    """E_c(I): invert ``I/A_c = i_O2(E) - i_O2(E_corr,c)`` (net cathodic current)."""
    i_lim = p.cathode_limiting_current_density_A_m2
    i_o2 = current_A / p.cathode_area_m2 + _o2_reduction_density(
        p, p.cathode_corrosion_potential_V
    )
    if math.isinf(i_lim):
        i_act = i_o2
    else:
        if i_o2 >= i_lim:
            return -math.inf
        i_act = i_o2 * i_lim / (i_lim - i_o2)
    return p.cathode_corrosion_potential_V - p.cathode_tafel_slope_V * math.log10(
        i_act / p.cathode_corrosion_current_density_A_m2
    )


_LN10 = math.log(10.0)


def galvanic_corrosion(
    input_params: GalvanicCorrosionInput,
    *,
    experimental: bool = False,
) -> GalvanicCorrosionResult:
    """Galvanic coupling by mixed-potential theory (Evans diagram).

    Kinetics (Dunn & Cragnolino 1997, CNWRA 97-010, eqs. 2-2, 2-3, 2-26 and
    Fig. 2-4; Wagner-Traud mixed-potential hypothesis), decadic Tafel form:

    * anode, net anodic current density (self-corrosion cathodic reaction
      taken as potential-independent, i.e. oxygen-diffusion controlled, at
      ``i_corr,a``):
      ``I/A_a = i_corr,a * (10**((E_a - E_corr,a)/beta_a) - 1)``
    * cathode, O2 reduction with diffusion limit, net of the cathode's own
      (potential-independent, passive) anodic current that balances it at
      ``E_corr,c``:
      ``i_O2(E) = i_act/(1 + i_act/i_L)``,
      ``i_act = i_corr,c * 10**(-(E - E_corr,c)/beta_c)``,
      ``I/A_c = i_O2(E_c) - i_O2(E_corr,c)``
    * ohmic drop: ``E_c(I) - E_a(I) - I*R_s = 0``

    The balance falls strictly from ``E_corr,c - E_corr,a > 0`` at I = 0 to
    -inf (the cathode diffusion plateau, or I -> inf without a limit), so
    the root is unique and lies with ``E_corr,a < E_a <= E_c < E_corr,c``.
    It is bracketed in ln(I) and solved with Brent's method.

    Corrosion rate = total anodic density ``i_corr,a + I/A_a`` [mA/m²] x
    :func:`faraday_rate_factor`; the galvanic part alone is reported too.

    Experimental
    ------------
    Provisional literature model (issue #2247). BS PD 6484 is not on
    file, so the model stays behind ``experimental=True``; calling without
    it raises
    :class:`~digitalmodel.cathodic_protection._experimental.ExperimentalModelError`.

    Raises
    ------
    ExperimentalModelError
        If ``experimental`` is false.
    """
    require_experimental(
        experimental,
        model="corrosion_rate.galvanic_corrosion",
        reason=(
            "mixed-potential model built from open literature (CNWRA 97-010); "
            "method and parameters not yet checked against a standard"
        ),
        standard="BS PD 6484",
    )
    p = input_params

    def balance(ln_current: float) -> float:
        current = math.exp(ln_current)
        return (
            _cathode_potential(p, current)
            - _anode_potential(p, current)
            - current * p.solution_resistance_ohm
        )

    # Lower bracket: a current far below both Tafel-intercept currents.
    lo = math.log(
        min(
            p.anode_corrosion_current_density_A_m2 * p.anode_area_m2,
            p.cathode_corrosion_current_density_A_m2 * p.cathode_area_m2,
        )
    ) - 12.0 * _LN10
    while balance(lo) <= 0.0:  # pragma: no cover - balance(0+) = E_c0 - E_a0 > 0
        lo -= _LN10

    # Upper bracket: grow (no limit) or approach the diffusion plateau.
    if math.isinf(p.cathode_limiting_current_density_A_m2):
        i_cap = math.inf
    else:
        i_cap = p.cathode_area_m2 * (
            p.cathode_limiting_current_density_A_m2
            - _o2_reduction_density(p, p.cathode_corrosion_potential_V)
        )
    hi: Optional[float] = None
    if math.isinf(i_cap):
        candidate = lo
        for _ in range(400):
            candidate += _LN10
            if balance(candidate) < 0.0:
                hi = candidate
                break
    else:
        for k in range(1, 16):
            candidate = math.log(i_cap * (1.0 - 10.0 ** (-k)))
            if candidate > lo and balance(candidate) < 0.0:
                hi = candidate
                break

    if hi is not None:
        ln_root, info = brentq(
            balance, lo, hi, xtol=1e-13, rtol=1e-13, maxiter=200, full_output=True
        )
        converged = bool(info.converged)
        current = math.exp(ln_root)
    else:
        # Balance still positive within 1e-15 of the plateau: diffusion limited.
        converged = False
        current = i_cap * (1.0 - 1e-15)

    e_a = _anode_potential(p, current)
    e_c = _cathode_potential(p, current)
    residual = e_c - e_a - current * p.solution_resistance_ohm
    i_galv = current / p.anode_area_m2
    i_anodic = p.anode_corrosion_current_density_A_m2 + i_galv
    diffusion_fraction = (
        0.0
        if math.isinf(p.cathode_limiting_current_density_A_m2)
        else _o2_reduction_density(p, e_c) / p.cathode_limiting_current_density_A_m2
    )
    factor = p.rate_factor()

    return GalvanicCorrosionResult(
        couple_potential_V=e_a,
        anode_potential_V=e_a,
        cathode_potential_V=e_c,
        galvanic_current_A=current,
        galvanic_current_density_A_m2=i_galv,
        anodic_current_density_A_m2=i_anodic,
        cathodic_current_density_A_m2=current / p.cathode_area_m2,
        diffusion_fraction=diffusion_fraction,
        corrosion_rate_mm_yr=i_anodic * 1000.0 * factor,
        galvanic_corrosion_rate_mm_yr=i_galv * 1000.0 * factor,
        area_ratio=p.cathode_area_m2 / p.anode_area_m2,
        converged=converged,
        residual_V=residual,
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
