"""Static S-lay/J-lay installation screening, in m, N, Pa and strain fractions.

This composes existing bending, tension and DNV pressure helpers. It is not
installation qualification. In particular, natural-catenary curvature can
underestimate rigid-pipe touchdown bending; displacement control cannot be
inferred from the lay method. No standard design-factor table is embedded.

Public method sources (no vendor algorithms or paid tables):

* Aleyaasin (2023), Marine Systems & Ocean Technology 18, 140-150,
  section 3, equations (19)-(25), doi:10.1007/s40868-023-00131-8.
* Ahamad et al. (2022), Jurnal Kejuruteraan 34(6), 1135-1147,
  equations (14)-(20), doi:10.17576/jkukm-2022-34(6)-14. These document
  the DNVGL-ST-F101 (2017) external-pressure method, now DNV-ST-F101.
* Mustafina (2015), Anchor Damage Assessment of Subsea Pipelines,
  University of Stavanger, p.57, equation (41): external-overpressure
  displacement-controlled interaction. Capacity and exponent are inputs;
  current-edition strain-capacity and factor selection is not implemented.
* BSEE TAP 434, Guidance Document on Strain Based Design, section
  "External Overpressure": collapse cubic and minimum ovality 0.005.

See ``docs/domains/installation/pipelay-screening.md`` for the worked case,
scope, ovality carry-over and native comparison protocol. Results retain public
source locators; they do not assert resolution against a private standards wiki.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.code_checks.dnv_os_f101 import (
    collapse_pressure,
    elastic_collapse_pressure,
    plastic_collapse_pressure,
    propagation_pressure,
)
from digitalmodel.installation.pipelay.integrity import (
    bending_strain_from_radius,
    top_lay_tension,
)
from digitalmodel.installation.pipelay.stress import _require_finite


CATENARY_SOURCE = "https://doi.org/10.1007/s40868-023-00131-8#Sec3"
PRESSURE_SOURCE = "https://www.ukm.my/jkukm/wp-content/uploads/2022/3406/14.pdf#page=5"
OVALITY_SOURCE = "https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/434ab.pdf"
INTERACTION_SOURCE = (
    "https://uis.brage.unit.no/uis-xmlui/bitstream/handle/11250/1248742/"
    "Master%20Thesis_Mustafina%20Adeliya.pdf"
)


def _positive(**values: float) -> None:
    _require_finite(**values)
    for name, value in values.items():
        if value <= 0:
            raise ValueError(f"{name} must be positive")


def _nonnegative(**values: float) -> None:
    _require_finite(**values)
    for name, value in values.items():
        if value < 0:
            raise ValueError(f"{name} must be nonnegative")


def _factors(**values: float) -> None:
    _require_finite(**values)
    for name, value in values.items():
        if value < 1:
            raise ValueError(f"{name} must be >= 1")


@dataclass(frozen=True)
class LayGeometry:
    """Free-span geometry measured from its upper departure point to touchdown."""

    method: str
    sagbend_radius_m: float
    top_tension_n: float
    top_vertical_force_n: float
    suspended_length_m: float
    touchdown_offset_m: float
    departure_angle_from_horizontal_deg: float
    sagbend_strain: float
    overbend_strain: float | None
    sources: tuple[str, ...]


def lay_geometry(
    *, method: str, outer_diameter_m: float, suspended_height_m: float,
    submerged_weight_n_per_m: float, horizontal_tension_n: float,
    stinger_radius_m: float | None = None,
) -> LayGeometry:
    """Natural catenary with a horizontal tangent at touchdown; no bending EI.

    Aleyaasin (2023), section 3, equations (19)-(25): a=H/w,
    T=H+w*h, L=sqrt(h*(h+2*a)), x=a*asinh(L/a), R_min=a,
    epsilon_b=D/(2*R). The latter is also used for the stinger overbend.
    These are equilibrium/curvature identities, with no empirical coefficients.

    ``suspended_height_m`` is seabed to departure point (stinger tip for S-lay,
    tower departure for J-lay). T is at that point, not the S-lay tensioner;
    supported-pipe weight, rollers and friction are excluded. Touchdown offset
    has the same origin; vessel position requires the known departure offset.
    S-lay requires a positive pipe-centreline stinger radius. J-lay has no
    stinger check (None). Steel OD excludes coating; weight includes contents,
    coatings and buoyancy, and must be uniform and positive.
    """
    if method not in ("S-Lay", "J-Lay"):
        raise ValueError("method must be S-Lay or J-Lay")
    _positive(outer_diameter_m=outer_diameter_m,
              suspended_height_m=suspended_height_m,
              submerged_weight_n_per_m=submerged_weight_n_per_m,
              horizontal_tension_n=horizontal_tension_n)
    if method == "S-Lay" and stinger_radius_m is None:
        raise ValueError("stinger_radius_m is required for S-Lay")
    if method == "J-Lay" and stinger_radius_m is not None:
        raise ValueError("stinger_radius_m is not applicable to J-Lay")
    if stinger_radius_m is not None:
        _positive(stinger_radius_m=stinger_radius_m)
    try:
        a = horizontal_tension_n / submerged_weight_n_per_m
        length = math.sqrt(suspended_height_m) * math.sqrt(suspended_height_m + 2 * a)
        top = top_lay_tension(submerged_weight_n_per_m, suspended_height_m, a)
        vertical = submerged_weight_n_per_m * length
        offset = a * math.asinh(length / a)
        sag = bending_strain_from_radius(outer_diameter_m, a)
        over = (bending_strain_from_radius(outer_diameter_m, stinger_radius_m)
                if stinger_radius_m is not None else None)
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("derived catenary is outside numerical range") from exc
    _positive(sagbend_radius_m=a, top_tension_n=top, top_vertical_force_n=vertical,
              suspended_length_m=length, touchdown_offset_m=offset, sagbend_strain=sag)
    if over is not None:
        _positive(overbend_strain=over)
    return LayGeometry(method, a, top, vertical, length, offset,
                       math.degrees(math.atan2(vertical, horizontal_tension_n)),
                       sag, over, (CATENARY_SOURCE,))


@dataclass(frozen=True)
class PressureScreen:
    """Characteristic pressures [Pa] and factored external-pressure utilization."""

    net_external_pressure_pa: float
    applied_ovality: float
    elastic_collapse_pa: float
    plastic_collapse_pa: float
    collapse_pa: float
    propagation_pa: float
    collapse_utilisation: float
    propagation_utilisation: float
    collapse_passes: bool
    propagation_passes: bool
    sources: tuple[str, ...]


def pressure_screen(
    *, outer_diameter_m: float, wall_thickness_m: float,
    yield_strength_pa: float, youngs_modulus_pa: float, poisson_ratio: float,
    fabrication_factor: float, ovality: float, external_pressure_pa: float,
    minimum_internal_pressure_pa: float, gamma_m: float, gamma_sc: float,
) -> PressureScreen:
    """Reuse DNV pressure helpers with explicit inputs and applicability checks.

    Ahamad et al. (2022), equations (14)-(20), documenting DNVGL-ST-F101
    (2017) sections 5.4.4-5.4.5: Pel=2*E*(t/D)^3/(1-nu^2),
    Pp=2*fy*alpha_fab*t/D; (Pc-Pel)*(Pc^2-Pp^2)=Pc*Pel*Pp*f0*D/t;
    Ppr=35*fy*alpha_fab*(t/D)^2.5. The existing helper supplies the
    coefficients 35 and 2.5; the public paper gives 15 < D/t < 45.
    This wrapper enforces that common range for both reported checks.
    Utilization is (pe-pmin)*gamma_m*gamma_sc/P; <=1 meets that criterion.
    Propagation failure means propagation is possible AFTER buckle initiation;
    the calculation does not determine buckle initiation or arrestor spacing.

    BSEE TAP 434, "External Overpressure", documents f0>=0.005, applied by
    the existing collapse helper and reported here. Measured ovality is
    (Dmax-Dmin)/D, dimensionless. No dent or bending-induced ovalisation model.
    Use separate calls with the applicable characteristic wall (e.g. t1/t2),
    strength and pressure/factor combination for each governing limit state;
    wall selection, corrosion and temperature derating are caller duties.
    All pressures use one datum; pmin must be sustainable. Internal
    overpressure, lined/clad pipe and current-edition qualification are excluded.
    """
    _positive(outer_diameter_m=outer_diameter_m, wall_thickness_m=wall_thickness_m,
              yield_strength_pa=yield_strength_pa, youngs_modulus_pa=youngs_modulus_pa,
              fabrication_factor=fabrication_factor)
    _nonnegative(ovality=ovality, external_pressure_pa=external_pressure_pa,
                 minimum_internal_pressure_pa=minimum_internal_pressure_pa)
    _require_finite(poisson_ratio=poisson_ratio)
    _factors(gamma_m=gamma_m, gamma_sc=gamma_sc)
    if not -1 < poisson_ratio < 0.5:
        raise ValueError("poisson_ratio must satisfy -1 < nu < 0.5")
    if fabrication_factor > 1:
        raise ValueError("fabrication_factor must be <= 1")
    if ovality >= 1:
        raise ValueError("ovality must be < 1")
    if not 15 < outer_diameter_m / wall_thickness_m < 45:
        raise ValueError("pressure screen requires 15 < D/t < 45")
    net = external_pressure_pa - minimum_internal_pressure_pa
    if net < 0:
        raise ValueError("internal overpressure is outside this screen")
    try:
        pel = elastic_collapse_pressure(outer_diameter_m, wall_thickness_m,
                                        youngs_modulus_pa, poisson_ratio)
        pp = plastic_collapse_pressure(outer_diameter_m, wall_thickness_m,
                                      yield_strength_pa, fabrication_factor)
        pc = collapse_pressure(outer_diameter_m, wall_thickness_m, yield_strength_pa,
                               youngs_modulus_pa, ovality, poisson_ratio, fabrication_factor)
        ppr = propagation_pressure(outer_diameter_m, wall_thickness_m,
                                   yield_strength_pa, fabrication_factor)
        _positive(elastic_collapse_pa=pel, plastic_collapse_pa=pp,
                  collapse_pa=pc, propagation_pa=ppr)
        uc = net / pc * gamma_m * gamma_sc
        up = net / ppr * gamma_m * gamma_sc
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("derived pressures are outside numerical range") from exc
    _nonnegative(collapse_utilisation=uc, propagation_utilisation=up)
    # The helper's documented imperfection floor is exposed, never hidden.
    return PressureScreen(net, max(ovality, 0.005), pel, pp, pc, ppr, uc, up,
                          uc <= 1, up <= 1, (PRESSURE_SOURCE, OVALITY_SOURCE))


@dataclass(frozen=True)
class DisplacementCheck:
    """External-pressure strain interaction; acceptance criterion U <= 1."""

    strain_term: float
    pressure_term: float
    utilisation: float
    passes: bool
    sources: tuple[str, ...]


def displacement_controlled_check(
    *, design_compressive_strain: float, characteristic_strain_capacity: float,
    gamma_e: float, net_external_pressure_pa: float, collapse_pressure_pa: float,
    gamma_m: float, gamma_sc: float, interaction_exponent: float,
    displacement_controlled: bool,
) -> DisplacementCheck:
    """Evaluate a supplied external-pressure displacement-control design basis.

    Mustafina (2015), p.57 equation (41), documenting DNV-OS-F101 (2013),
    Sec.5 D, displacement-controlled combined loading:
    U=(epsilon_Sd*gamma_e/epsilon_c)^q + (pe-pmin)*gamma_m*gamma_sc/Pc <=1.
    The published interaction uses q=0.8. q, epsilon_c and every factor are
    required, with no implied DNV-ST-F101 (2021 or later) capacity selection.
    Obtain and independently qualify these inputs for the governing edition,
    D/t, weld, material, pressure regime and wall before design use.

    epsilon_Sd is FACTORED net longitudinal compressive strain (fraction,
    positive compression), including bending and direct axial strain. Effective
    catenary tension is not wall force and is not converted to strain here.
    Pass True only after establishing displacement control for the section.
    A free sagbend may require load-controlled checks instead; this function
    cannot establish that classification. Zero net pressure is permitted;
    internal-overpressure and tensile-fracture checks are outside scope.
    """
    if displacement_controlled is not True:
        raise ValueError("displacement_controlled must be explicitly True")
    _nonnegative(design_compressive_strain=design_compressive_strain,
                 net_external_pressure_pa=net_external_pressure_pa)
    _positive(characteristic_strain_capacity=characteristic_strain_capacity,
              collapse_pressure_pa=collapse_pressure_pa,
              interaction_exponent=interaction_exponent)
    _factors(gamma_e=gamma_e, gamma_m=gamma_m, gamma_sc=gamma_sc)
    if characteristic_strain_capacity >= 1 or design_compressive_strain >= 1:
        raise ValueError("strain inputs must be fractions smaller than 1")
    try:
        strain = (design_compressive_strain / characteristic_strain_capacity * gamma_e
                  ) ** interaction_exponent
        pressure = net_external_pressure_pa / collapse_pressure_pa * gamma_m * gamma_sc
        utilisation = strain + pressure
    except OverflowError as exc:
        raise ValueError("derived utilisation is outside numerical range") from exc
    _nonnegative(strain_term=strain, pressure_term=pressure, utilisation=utilisation)
    return DisplacementCheck(strain, pressure, utilisation, utilisation <= 1,
                             (INTERACTION_SOURCE,))


@dataclass(frozen=True)
class ARCableTension:
    """A&R cable top demand [N]; no winch, cable or rigging capacity implied."""

    static_top_tension_n: float
    design_top_tension_n: float
    top_vertical_force_n: float
    sources: tuple[str, ...]


def ar_cable_tension(
    *, horizontal_end_force_n: float, downward_end_force_n: float,
    suspended_cable_length_m: float, cable_submerged_weight_n_per_m: float,
    submerged_head_weight_n: float, dynamic_factor: float,
) -> ARCableTension:
    """A&R cable force balance for a specified suspended configuration.

    Derived from Aleyaasin (2023), equations (10)-(11), vertical force balance
    and resultant tension: Vtop=Vend+w_c*Lc+Whead, Ttop=hypot(Hend,Vtop).
    Here the lower-end pipe force replaces the zero-end-force boundary in (10).
    Tdesign=dynamic_factor*Ttop is a declared screening envelope, not a
    standard DAF or a time-domain solution; supply dynamic_factor >=1.
    H_end and downward V_end are pipe loads ON the cable, excluding the
    separately supplied head weight. Supply the A&R-stage pipe configuration,
    not the original lay configuration. Lc is actual suspended cable length,
    excluding cable resting on seabed. Its geometric compatibility with the
    lower force and endpoints is a caller duty. Uniform nonnegative wet cable
    weight, no current drag, seabed friction, sheaves, snap loads or buoyant head.
    Compare design demand with separately qualified cable/winch/rigging limits.
    """
    _nonnegative(horizontal_end_force_n=horizontal_end_force_n,
                 downward_end_force_n=downward_end_force_n,
                 cable_submerged_weight_n_per_m=cable_submerged_weight_n_per_m,
                 submerged_head_weight_n=submerged_head_weight_n)
    _positive(suspended_cable_length_m=suspended_cable_length_m)
    _factors(dynamic_factor=dynamic_factor)
    vertical = (downward_end_force_n + submerged_head_weight_n
                + cable_submerged_weight_n_per_m * suspended_cable_length_m)
    static = math.hypot(horizontal_end_force_n, vertical)
    design = dynamic_factor * static
    _nonnegative(top_vertical_force_n=vertical, static_top_tension_n=static,
                 design_top_tension_n=design)
    return ARCableTension(static, design, vertical, (CATENARY_SOURCE,))
