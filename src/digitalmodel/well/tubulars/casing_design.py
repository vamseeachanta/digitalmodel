# ABOUTME: Production casing design checks — load cases (tubing leak, frac
# ABOUTME: screen-out, evacuation collapse), design factors, NACE sour screen.
"""Production casing design-check layer on top of the API 5C3 ratings.

:mod:`~digitalmodel.well.tubulars.casing` computes what a casing *product* can
take (burst / collapse / body-yield ratings).  This module computes what the
*well* asks of it and judges the margin:

* **Load cases** as piecewise-linear pressure profiles vs depth — tubing leak
  (shut-in tubing pressure on the packer fluid), injection down casing / frac
  screen-out (surface pressure on the injected-fluid gradient), the standard
  external profile for burst (mud to TOC, cement mix-water to the outer shoe,
  pore pressure below) and full-evacuation collapse.
* **Design factors** ``DF = rating / load >= DF_min`` per mode, evaluated over
  depth, reporting the minimum DF and its governing depth.  Default minimums
  follow the published operator practice in the source presentation (burst
  1.25, or 1.1 when SICP < 5,000 psi; collapse 1.1; tension 1.4 on yield;
  compression 1.2; Von Mises triaxial 1.25) and are fully configurable.
* **Stimulation checks** — maximum allowable frac surface pressure for a given
  burst design factor, plus the ballooning and cooling tension adders that a
  frac applies to the string.
* **Sour-service screen** per NACE MR0175 / ISO 15156 — H2S partial-pressure
  threshold (0.05 psia) with the 65 psia (gas) / 265 psia (oil) total-pressure
  qualifiers, and the per-grade minimum service temperatures.
* **Connection classes** — STC / LTC / BTC / MTC / IJ / FJ with tension
  efficiencies relative to the pipe body and the ultimate-vs-yield basis note.

Source: B. Hansen (Devon Energy), *Production Casing Design Considerations*,
EPA Hydraulic Fracturing Technical Workshop — Session 2: Well Design, 2011.
Rating formulas per API 5C3 / API TR 5C3; sour limits per NACE MR0175 Table A.3.

Worked example preserved as a golden test: 5-1/2" 23# P110 Barlow burst
``0.875 * 2 * 110,000 * 0.415 / 5.5 = 14,525 -> 14,520 psi`` (API rounding to
the nearest 10 psi).
"""
from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional, Sequence

import numpy as np

from digitalmodel.well.tubulars.casing import Casing
from digitalmodel.well.tubulars.design_envelope import (
    compute_hoop_stress,
    compute_vme_stress,
)

SOURCE_REFERENCE = (
    "Hansen (Devon Energy), EPA Hydraulic Fracturing Technical Workshop, "
    "Session 2: Well Design, 2011; API 5C3 / NACE MR0175"
)

#: Pressure gradient of a fluid column, psi/ft per lb/gal (industry 0.052).
PPG_TO_PSI_PER_FT = 0.052

#: Density of steel expressed in ppg-equivalent for the buoyancy factor.
STEEL_DENSITY_PPG = 65.4

#: Default dry-gas static gradient used to translate reservoir pressure to a
#: shut-in surface pressure when no gas PVT is available, psi/ft.
DEFAULT_GAS_GRADIENT_PSI_FT = 0.1

# Steel elastic constants for the frac tension adders.
_STEEL_E_PSI = 30.0e6
_STEEL_POISSON = 0.30
_STEEL_ALPHA_PER_F = 6.9e-6


# ---------------------------------------------------------------------------
# API rounding conventions (API 5C3 published-rating presentation)
# ---------------------------------------------------------------------------
def api_round_pressure_psi(p_psi: float) -> float:
    """Round a pressure rating to the nearest 10 psi (API convention)."""
    return round(p_psi / 10.0) * 10.0


def api_round_force_lbf(f_lbf: float) -> float:
    """Round an axial rating to the nearest 1,000 lbf (API convention)."""
    return round(f_lbf / 1000.0) * 1000.0


# ---------------------------------------------------------------------------
# Pressure profiles (piecewise linear in depth)
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class PressureProfile:
    """A piecewise-linear pressure vs true-vertical-depth profile."""

    depths_ft: tuple[float, ...]
    pressures_psi: tuple[float, ...]
    label: str = ""

    def __post_init__(self) -> None:
        if len(self.depths_ft) != len(self.pressures_psi):
            raise ValueError("depths and pressures must have equal length")
        if len(self.depths_ft) < 2:
            raise ValueError("a profile needs at least two anchor points")
        if any(b < a for a, b in zip(self.depths_ft, self.depths_ft[1:])):
            raise ValueError("depths must be non-decreasing")

    def at(self, depth_ft: "float | np.ndarray") -> np.ndarray:
        """Interpolate the profile at one or more depths (ft)."""
        return np.interp(depth_ft, self.depths_ft, self.pressures_psi)


def fluid_column_profile(density_ppg: float, td_ft: float,
                         surface_pressure_psi: float = 0.0,
                         label: str = "") -> PressureProfile:
    """A single fluid gradient from surface, optionally pressurized on top."""
    grad = PPG_TO_PSI_PER_FT * density_ppg
    return PressureProfile(
        (0.0, td_ft),
        (surface_pressure_psi, surface_pressure_psi + grad * td_ft),
        label or f"{density_ppg:g} ppg column",
    )


def shut_in_tubing_pressure(reservoir_pressure_psi: float,
                            reservoir_depth_ft: float,
                            gas_gradient_psi_ft: float =
                            DEFAULT_GAS_GRADIENT_PSI_FT) -> float:
    """Shut-in tubing pressure: reservoir pressure minus the gas column."""
    return reservoir_pressure_psi - gas_gradient_psi_ft * reservoir_depth_ft


def tubing_leak_internal_profile(
    reservoir_pressure_psi: float,
    reservoir_depth_ft: float,
    packer_fluid_ppg: float,
    td_ft: float,
    gas_gradient_psi_ft: float = DEFAULT_GAS_GRADIENT_PSI_FT,
) -> PressureProfile:
    """Internal burst profile for the tubing-leak load case.

    A near-surface tubing leak puts the shut-in tubing pressure (gas gradient
    extended up from the reservoir) on top of the packer/completion fluid, so
    the casing sees SITP + the completion-fluid column.
    """
    sitp = shut_in_tubing_pressure(reservoir_pressure_psi, reservoir_depth_ft,
                                   gas_gradient_psi_ft)
    return fluid_column_profile(packer_fluid_ppg, td_ft,
                                surface_pressure_psi=sitp,
                                label="tubing leak (SITP on packer fluid)")


def injection_internal_profile(surface_pressure_psi: float,
                               fluid_ppg: float,
                               td_ft: float) -> PressureProfile:
    """Internal burst profile for injection down casing / frac screen-out.

    Applied surface pressure on a static column of the injected fluid —
    friction down the casing is conservatively not subtracted, which is
    analogous to a screen-out during a frac job.
    """
    p = fluid_column_profile(fluid_ppg, td_ft,
                             surface_pressure_psi=surface_pressure_psi)
    return PressureProfile(p.depths_ft, p.pressures_psi,
                           "injection down casing (screen-out)")


def burst_external_profile(
    mud_ppg: float,
    toc_ft: float,
    outer_shoe_ft: float,
    td_ft: float,
    mix_water_ppg: float = 8.4,
    pore_ppg: Optional[float] = None,
) -> PressureProfile:
    """Standard external (backup) pressure profile for the burst load cases.

    Mud (or deteriorated mud) from surface to the top of cement, cement
    mix-water gradient (typically 8.3–8.6 ppg) from TOC to the outer casing
    shoe, and the pore-pressure profile from the outer shoe to the base of the
    production casing.  ``pore_ppg`` defaults to the mix-water gradient
    continued to TD when not given.
    """
    if not 0.0 <= toc_ft <= outer_shoe_ft <= td_ft:
        raise ValueError("require 0 <= TOC <= outer shoe <= TD")
    mud_grad = PPG_TO_PSI_PER_FT * mud_ppg
    mix_grad = PPG_TO_PSI_PER_FT * mix_water_ppg
    pore_grad = PPG_TO_PSI_PER_FT * (pore_ppg if pore_ppg is not None
                                     else mix_water_ppg)
    p_toc = mud_grad * toc_ft
    p_shoe = p_toc + mix_grad * (outer_shoe_ft - toc_ft)
    p_td = p_shoe + pore_grad * (td_ft - outer_shoe_ft)
    depths = (0.0, toc_ft, outer_shoe_ft, td_ft)
    pressures = (0.0, p_toc, p_shoe, p_td)
    # Collapse duplicate anchor depths (e.g. TOC at surface) to keep the
    # profile strictly usable by np.interp.
    dedup = [(d, p) for i, (d, p) in enumerate(zip(depths, pressures))
             if i == 0 or d > depths[i - 1]]
    if len(dedup) == 1:
        dedup.append((td_ft if td_ft > 0 else 1.0, pressures[-1]))
    ds, ps = zip(*dedup)
    return PressureProfile(ds, ps, "external: mud / mix-water / pore")


def full_evacuation_internal_profile(td_ft: float) -> PressureProfile:
    """Zero internal pressure — severely depleted / blown-down well."""
    return PressureProfile((0.0, td_ft), (0.0, 0.0), "full evacuation")


# ---------------------------------------------------------------------------
# Design factors
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class DesignFactors:
    """Minimum acceptable design factors, ``DF = rating / load``.

    Defaults are the published operator minimums from the source presentation;
    override any of them to match a different company standard.  ``burst`` may
    be relaxed to ``burst_low_sicp`` when the shut-in casing pressure is below
    ``sicp_threshold_psi``.
    """

    burst: float = 1.25
    burst_low_sicp: float = 1.1
    sicp_threshold_psi: float = 5000.0
    collapse: float = 1.1
    tension: float = 1.4
    compression: float = 1.2
    triaxial: float = 1.25
    reference: str = SOURCE_REFERENCE

    def burst_for_sicp(self, sicp_psi: float) -> float:
        """The burst minimum applicable at a given shut-in casing pressure."""
        return (self.burst_low_sicp if sicp_psi < self.sicp_threshold_psi
                else self.burst)


@dataclass(frozen=True)
class ModeCheckResult:
    """Outcome of one design-factor check over the string."""

    mode: str
    rating: float
    max_load: float
    min_design_factor: float
    governing_depth_ft: float
    required_design_factor: float
    passes: bool
    load_case: str = ""

    def as_dict(self) -> dict:
        return {
            "mode": self.mode,
            "load_case": self.load_case,
            "rating": self.rating,
            "max_load": self.max_load,
            "min_design_factor": self.min_design_factor,
            "governing_depth_ft": self.governing_depth_ft,
            "required_design_factor": self.required_design_factor,
            "passes": self.passes,
        }


def _depth_grid(td_ft: float, profiles: Sequence[PressureProfile],
                n: int = 201) -> np.ndarray:
    """A depth grid including every profile anchor (kinks are candidates)."""
    pts = set(np.linspace(0.0, td_ft, n).tolist())
    for prof in profiles:
        pts.update(d for d in prof.depths_ft if 0.0 <= d <= td_ft)
    return np.array(sorted(pts))


def check_burst(casing_product: Casing,
                internal: PressureProfile,
                external: PressureProfile,
                td_ft: float,
                factors: DesignFactors = DesignFactors(),
                sicp_psi: Optional[float] = None) -> ModeCheckResult:
    """Burst design-factor check: rating vs net internal pressure with depth."""
    rating = api_round_pressure_psi(casing_product.burst_psi)
    z = _depth_grid(td_ft, (internal, external))
    net = internal.at(z) - external.at(z)
    required = (factors.burst if sicp_psi is None
                else factors.burst_for_sicp(sicp_psi))
    return _pressure_mode_result("burst", rating, net, z, required,
                                 internal.label)


def check_collapse(casing_product: Casing,
                   external: PressureProfile,
                   td_ft: float,
                   internal: Optional[PressureProfile] = None,
                   factors: DesignFactors = DesignFactors()) -> ModeCheckResult:
    """Collapse design-factor check: rating vs net external pressure.

    ``internal`` defaults to full evacuation (zero pressure inside).
    """
    rating = api_round_pressure_psi(casing_product.collapse_psi)
    if internal is None:
        internal = full_evacuation_internal_profile(td_ft)
    z = _depth_grid(td_ft, (internal, external))
    net = external.at(z) - internal.at(z)
    return _pressure_mode_result("collapse", rating, net, z, factors.collapse,
                                 external.label)


def _pressure_mode_result(mode: str, rating: float, net: np.ndarray,
                          z: np.ndarray, required: float,
                          load_case: str) -> ModeCheckResult:
    net = np.asarray(net, dtype=float)
    i = int(np.argmax(net))
    max_load = float(net[i])
    if max_load <= 0.0:
        df = math.inf
    else:
        df = rating / max_load
    return ModeCheckResult(mode=mode, rating=rating, max_load=max_load,
                           min_design_factor=df,
                           governing_depth_ft=float(z[i]),
                           required_design_factor=required,
                           passes=df >= required, load_case=load_case)


# ---------------------------------------------------------------------------
# Axial loads
# ---------------------------------------------------------------------------
def buoyancy_factor(mud_ppg: float) -> float:
    """Buoyancy factor for steel in mud, ``1 - rho_mud / rho_steel``."""
    return 1.0 - mud_ppg / STEEL_DENSITY_PPG


def axial_load_profile(weight_ppf: float, td_ft: float, mud_ppg: float,
                       extra_tension_lbf: float = 0.0,
                       n: int = 201) -> tuple[np.ndarray, np.ndarray]:
    """Buoyed-weight axial tension vs depth (positive = tension, lbf).

    Uses the buoyancy-factor method — tension at depth ``z`` is the buoyed
    weight of the string hanging below, plus any surface-applied overpull or
    frac-induced adders passed via ``extra_tension_lbf``.
    """
    z = np.linspace(0.0, td_ft, n)
    tension = weight_ppf * (td_ft - z) * buoyancy_factor(mud_ppg)
    return z, tension + extra_tension_lbf


def ballooning_tension_delta_lbf(delta_internal_psi: float,
                                 casing_product: Casing) -> float:
    """Tension increase from ballooning under raised internal pressure.

    Poisson (ballooning) effect for a string fixed at both ends:
    ``dF = 2 * nu * dP_i * A_i``.  During a frac the internal pressure rise
    balloons the casing and adds tension.
    """
    a_i = math.pi / 4.0 * casing_product.id_in ** 2
    return 2.0 * _STEEL_POISSON * delta_internal_psi * a_i


def cooling_tension_delta_lbf(delta_temp_f: float,
                              casing_product: Casing) -> float:
    """Tension increase from cooling (frac fluid is colder than the well).

    Thermal contraction restrained by the cemented string:
    ``dF = E * alpha * dT * A_steel`` for a temperature *drop* ``delta_temp_f``
    (pass the positive number of degrees cooled).
    """
    return (_STEEL_E_PSI * _STEEL_ALPHA_PER_F * delta_temp_f
            * casing_product.metal_area_in2)


def check_tension(casing_product: Casing, weight_ppf: float, td_ft: float,
                  mud_ppg: float, extra_tension_lbf: float = 0.0,
                  factors: DesignFactors = DesignFactors()) -> ModeCheckResult:
    """Tension design-factor check on pipe-body yield (surface governs)."""
    rating = api_round_force_lbf(casing_product.body_yield_lbf)
    z, tension = axial_load_profile(weight_ppf, td_ft, mud_ppg,
                                    extra_tension_lbf)
    i = int(np.argmax(tension))
    max_load = float(tension[i])
    df = math.inf if max_load <= 0.0 else rating / max_load
    return ModeCheckResult(mode="tension", rating=rating, max_load=max_load,
                           min_design_factor=df,
                           governing_depth_ft=float(z[i]),
                           required_design_factor=factors.tension,
                           passes=df >= factors.tension,
                           load_case="buoyed weight + adders")


def check_triaxial(casing_product: Casing,
                   internal: PressureProfile,
                   external: PressureProfile,
                   weight_ppf: float, td_ft: float, mud_ppg: float,
                   extra_tension_lbf: float = 0.0,
                   factors: DesignFactors = DesignFactors()) -> ModeCheckResult:
    """Von Mises triaxial design-factor check over the string.

    Thin-wall hoop stress from the net pressure plus the buoyed-weight axial
    stress, combined as a VME stress and compared with the minimum yield.
    """
    yp = casing_product.grade.min_yield_psi
    z = _depth_grid(td_ft, (internal, external))
    net = internal.at(z) - external.at(z)
    _, tension = axial_load_profile(weight_ppf, td_ft, mud_ppg,
                                    extra_tension_lbf, n=len(z))
    # axial_load_profile returns its own grid; recompute on z for alignment.
    tension = (weight_ppf * (td_ft - z) * buoyancy_factor(mud_ppg)
               + extra_tension_lbf)
    area = casing_product.metal_area_in2
    vme = np.array([
        compute_vme_stress(t / area,
                           compute_hoop_stress(p, casing_product.od_in,
                                               casing_product.id_in))
        for t, p in zip(tension, net)
    ])
    i = int(np.argmax(vme))
    max_vme = float(vme[i])
    df = math.inf if max_vme <= 0.0 else yp / max_vme
    return ModeCheckResult(mode="triaxial", rating=yp, max_load=max_vme,
                           min_design_factor=df,
                           governing_depth_ft=float(z[i]),
                           required_design_factor=factors.triaxial,
                           passes=df >= factors.triaxial,
                           load_case=internal.label)


# ---------------------------------------------------------------------------
# Stimulation (frac) checks
# ---------------------------------------------------------------------------
def max_frac_surface_pressure(
    casing_product: Casing,
    frac_fluid_ppg: float,
    external: PressureProfile,
    td_ft: float,
    factors: DesignFactors = DesignFactors(),
    design_factor: Optional[float] = None,
) -> float:
    """Maximum allowable frac surface pressure for the burst design factor.

    Solves ``DF = rating / (P_s + G_f z - p_ext(z)) >= DF_min`` for the
    surface pressure ``P_s`` — the tightest depth governs:
    ``P_s = min_z [ rating / DF_min - G_f z + p_ext(z) ]``.
    """
    rating = api_round_pressure_psi(casing_product.burst_psi)
    df_min = design_factor if design_factor is not None else factors.burst
    grad = PPG_TO_PSI_PER_FT * frac_fluid_ppg
    z = _depth_grid(td_ft, (external,))
    allowable = rating / df_min - grad * z + external.at(z)
    return float(np.min(allowable))


# ---------------------------------------------------------------------------
# Sour service (NACE MR0175 / ISO 15156)
# ---------------------------------------------------------------------------
#: Minimum service temperature (deg F) for SSC resistance per NACE MR0175
#: Table A.3.  ``0`` = acceptable at all temperatures.  N80 quenched &
#: tempered (type Q) differs from N80 type 1 — both entries are provided.
NACE_MIN_SERVICE_TEMP_F: dict[str, float] = {
    "H40": 0.0, "J55": 0.0, "K55": 0.0, "M65": 0.0,
    "L80": 0.0,     # type 1
    "C90": 0.0,     # type 1
    "T95": 0.0,     # type 1
    "N80Q": 150.0, "C95": 150.0,
    "N80": 175.0, "P110": 175.0,
    "Q125": 225.0,
}

#: NACE MR0175 sour-service definition thresholds.
H2S_PARTIAL_PRESSURE_THRESHOLD_PSIA = 0.05
SOUR_TOTAL_PRESSURE_GAS_PSIA = 65.0
SOUR_TOTAL_PRESSURE_OIL_PSIA = 265.0


def h2s_partial_pressure_psia(h2s_ppm: float,
                              total_pressure_psia: float) -> float:
    """H2S partial pressure: ``ppm * total pressure / 1,000,000`` (psia)."""
    return h2s_ppm * total_pressure_psia / 1.0e6


@dataclass(frozen=True)
class SourServiceAssessment:
    """Outcome of the NACE MR0175 sour-service screen."""

    is_sour: bool
    h2s_partial_psia: float
    total_pressure_psia: float
    well_type: str
    acceptable_grades: tuple[str, ...] = ()
    reference: str = "NACE MR0175 / ISO 15156"


def sour_service_screen(h2s_ppm: float, total_pressure_psia: float,
                        well_type: str = "gas",
                        service_temp_f: Optional[float] = None
                        ) -> SourServiceAssessment:
    """Screen an environment for sour service and list acceptable grades.

    Sour service requires the H2S partial pressure to exceed 0.05 psia *and*
    the total pressure to exceed 65 psia for a gas well or 265 psia for an
    oil well.  When ``service_temp_f`` is given, the acceptable-grade list is
    filtered by the NACE minimum service temperatures.
    """
    wt = well_type.strip().lower()
    if wt not in ("gas", "oil"):
        raise ValueError("well_type must be 'gas' or 'oil'")
    pp = h2s_partial_pressure_psia(h2s_ppm, total_pressure_psia)
    p_min = (SOUR_TOTAL_PRESSURE_GAS_PSIA if wt == "gas"
             else SOUR_TOTAL_PRESSURE_OIL_PSIA)
    is_sour = (pp > H2S_PARTIAL_PRESSURE_THRESHOLD_PSIA
               and total_pressure_psia > p_min)
    if not is_sour:
        grades = tuple(sorted(NACE_MIN_SERVICE_TEMP_F))
    elif service_temp_f is None:
        grades = tuple(g for g, t in sorted(NACE_MIN_SERVICE_TEMP_F.items())
                       if t == 0.0)
    else:
        grades = tuple(g for g, t in sorted(NACE_MIN_SERVICE_TEMP_F.items())
                       if service_temp_f >= t)
    return SourServiceAssessment(is_sour=is_sour, h2s_partial_psia=pp,
                                 total_pressure_psia=total_pressure_psia,
                                 well_type=wt, acceptable_grades=grades)


def grade_sour_ok(grade_name: str, service_temp_f: float) -> bool:
    """Is a grade SSC-acceptable at the given service temperature (deg F)?"""
    key = grade_name.strip().upper()
    if key not in NACE_MIN_SERVICE_TEMP_F:
        raise KeyError(f"No NACE MR0175 entry for grade {grade_name!r}")
    return service_temp_f >= NACE_MIN_SERVICE_TEMP_F[key]


# ---------------------------------------------------------------------------
# Connections
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class ConnectionClass:
    """A casing connection family with tension efficiency vs the pipe body."""

    name: str
    full_name: str
    threads_per_inch: Optional[int]
    tension_efficiency: tuple[float, float]
    strength_basis: str
    notes: str = ""

    def joint_strength_lbf(self, casing_product: Casing,
                           conservative: bool = True) -> float:
        """Screening joint strength: efficiency x pipe-body yield.

        A screening number only — published API joint strengths come from the
        API 5C3 thread-geometry formulas (and are ultimate-strength based for
        API connections); use those for final selection.
        """
        eff = (self.tension_efficiency[0] if conservative
               else self.tension_efficiency[1])
        return eff * casing_product.body_yield_lbf


#: Connection families: less than 3% of the string length, more than 90% of
#: pipe failures, 10–50% of the tubular cost.
CONNECTION_CLASSES: dict[str, ConnectionClass] = {
    "STC": ConnectionClass(
        "STC", "API short thread & coupled", 8, (0.60, 0.80), "ultimate",
        "8 round threads/in, rounded crests and roots."),
    "LTC": ConnectionClass(
        "LTC", "API long thread & coupled", 8, (0.70, 0.90), "ultimate",
        "Longer thread section than STC — better sealability and tensile "
        "strength."),
    "BTC": ConnectionClass(
        "BTC", "API buttress thread & coupled", 5, (0.85, 1.00), "ultimate",
        "5 threads/in; asymmetric load and stab flanks."),
    "MTC": ConnectionClass(
        "MTC", "Metal-to-metal seal thread & coupled (premium)", None,
        (1.00, 1.00), "yield",
        "Burst, collapse and tension ratings generally equal the pipe body."),
    "IJ": ConnectionClass(
        "IJ", "Integral joint (premium)", None, (0.70, 0.80), "yield",
        "Half the leak paths of T&C; smaller connection OD; metal seal."),
    "FJ": ConnectionClass(
        "FJ", "Flush joint (premium)", None, (0.45, 0.60), "yield",
        "Connection OD within ~2% of the pipe body; lowest joint strength."),
}


def connection_class(name: str) -> ConnectionClass:
    """Look up a connection family by short name (case-insensitive)."""
    key = name.strip().upper()
    if key not in CONNECTION_CLASSES:
        raise KeyError(f"Unknown connection class: {name!r}")
    return CONNECTION_CLASSES[key]


# ---------------------------------------------------------------------------
# Orchestrated design check
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class ProductionCasingWell:
    """The well-side inputs for a production casing design check."""

    td_ft: float
    mud_ppg: float
    toc_ft: float
    outer_shoe_ft: float
    reservoir_pressure_psi: float
    packer_fluid_ppg: float
    frac_surface_pressure_psi: float = 0.0
    frac_fluid_ppg: float = 8.6
    mix_water_ppg: float = 8.4
    pore_ppg: Optional[float] = None
    gas_gradient_psi_ft: float = DEFAULT_GAS_GRADIENT_PSI_FT


def check_production_casing(
    casing_product: Casing,
    weight_ppf: float,
    well: ProductionCasingWell,
    factors: DesignFactors = DesignFactors(),
) -> dict[str, ModeCheckResult]:
    """Run the standard production-casing design checks for one product.

    Returns the governing (lowest-DF) burst check across the tubing-leak and
    frac screen-out load cases, the full-evacuation collapse check, the
    buoyed-weight tension check, and the triaxial check on the governing
    burst case.
    """
    external = burst_external_profile(
        well.mud_ppg, well.toc_ft, well.outer_shoe_ft, well.td_ft,
        mix_water_ppg=well.mix_water_ppg, pore_ppg=well.pore_ppg)

    leak = tubing_leak_internal_profile(
        well.reservoir_pressure_psi, well.td_ft, well.packer_fluid_ppg,
        well.td_ft, gas_gradient_psi_ft=well.gas_gradient_psi_ft)
    sicp = shut_in_tubing_pressure(well.reservoir_pressure_psi, well.td_ft,
                                   well.gas_gradient_psi_ft)
    burst_cases = [
        check_burst(casing_product, leak, external, well.td_ft, factors,
                    sicp_psi=sicp),
    ]
    if well.frac_surface_pressure_psi > 0.0:
        frac = injection_internal_profile(well.frac_surface_pressure_psi,
                                          well.frac_fluid_ppg, well.td_ft)
        burst_cases.append(
            check_burst(casing_product, frac, external, well.td_ft, factors))
    governing_burst = min(burst_cases, key=lambda r: r.min_design_factor)

    collapse_external = fluid_column_profile(well.mud_ppg, well.td_ft,
                                             label="mud to surface")
    results = {
        "burst": governing_burst,
        "collapse": check_collapse(casing_product, collapse_external,
                                   well.td_ft, factors=factors),
        "tension": check_tension(casing_product, weight_ppf, well.td_ft,
                                 well.mud_ppg, factors=factors),
        "triaxial": check_triaxial(
            casing_product,
            leak, external, weight_ppf, well.td_ft, well.mud_ppg,
            factors=factors),
    }
    return results
