"""Axial, bending and shear interaction blocks for a stiffened panel.

Inputs use m, m2, m3, m4, Pa, N and N.m; results retain a Citation sidecar.
There is no unit inference or conversion. Existing panel_buckling methods
supply section, plate elastic and torsional calculations in their native units.

Transcribed method citations from a legacy engineering workbook:
- DnV-RP-C201,Section 7.5, eqn7.21 and 7.22; eqn7.25; eqn7.26:
  flexural slenderness and characteristic resistance.
- DnV-RP-C201,Section 7.2, eqn7.6; Section 7.6, eqn7.45; eqn7.46:
  local/global elastic shear candidates and yield resistance.
- DnV-RP-C201,Section 7.7.3, eqn7.65 through eqn7.71; the Euler load
  locator eqn7.72 is supplied by the caller.
  Axial and moment resistance products.
- DnV-RP-C201,Section 7.71, eqn7.50 through eqn7.53:
  four plate-pressure interactions. The source repeats a second set for
  stiffener-side pressure: clause not stated in source for that second set.
- DnV-RP-C201,Section 7.7, Equation: fixed-end line-load moments;
  equation number not stated in source.
- u: DnV-RP-C201, Section 7.7, eqn7.58 (as stated in source).
These are source locators, including its unusual "7.71" spelling; the source
edition is not established. Only the plate-4 and stiffener-2 axial-capacity
choices have been checked against DnV-RP-C201, October 2010, Section 7.7.1,
eqn7.53 and eqn7.55. No design approval follows.

Input gaps: the standard edition; qualification of the local elastic shear
multiplier and the global shear coefficient; effective areas and widths;
source load derivation; short-column plateau and imperfection-slenderness
selection in conflicting source branches; length conditions selecting the two compression-side
moment strengths; and a verified source for each caller-supplied coefficient.
All those numerical quantities are required inputs without design defaults.
The boundary validates caller-supplied citation metadata using the existing
resolver. Metadata resolution establishes provenance, not technical validity.
Call validate_panel_method_basis immediately before the pure numerical blocks.

Limits: compressive N >= 0 strictly below the supplied Euler load; nonnegative
moment magnitudes; signed eccentricity and shear. The eight expressions are
returned separately, with no clipping or automatic acceptance decision.
L sections, effective-width selection, tension rules, load generation and
post-Euler behavior remain outside these blocks. The source's conflicting
centroid references, girder axes and moment units are not used as defaults.
"""

from __future__ import annotations

from dataclasses import InitVar, dataclass, fields
from math import isfinite, sqrt
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation


@dataclass(frozen=True)
class PanelMethodBasis:
    """Resolved metadata for trusted in-process callers, not a security token.

    Construction checks the cited page. Deliberate object mutation is outside
    this arithmetic API's trust model. Source qualification is still required.
    """

    citation: Citation
    wiki_root: InitVar[Path]

    def __post_init__(self, wiki_root: Path) -> None:
        validate_citation(self.citation, repo_root=wiki_root)

    @property
    def citations(self) -> tuple[Citation, ...]:
        return (self.citation,)


def validate_panel_method_basis(citation: Citation, *, wiki_root: Path) -> PanelMethodBasis:
    """Fail closed on missing/mismatching metadata; never infer an edition.

    The caller supplies the cited source and its revision. Synthetic arithmetic
    tests may supply explicitly synthetic metadata. The existing validator
    verifies code_id, publisher and revision, not the technical clause content.
    """
    return PanelMethodBasis(citation, wiki_root)


def _citations(basis: PanelMethodBasis) -> tuple[Citation, ...]:
    if not isinstance(basis, PanelMethodBasis) or not isinstance(getattr(basis, 'citation', None), Citation):
        raise ValueError('a validated PanelMethodBasis is required')
    return basis.citations


def _finite(name: str, value: float, *, positive: bool = False, nonnegative: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)) or not isfinite(value):
        raise ValueError(f'{name} must be a finite number')
    if positive and value <= 0:
        raise ValueError(f'{name} must be positive')
    if nonnegative and value < 0:
        raise ValueError(f'{name} must be nonnegative')


def _positive_fields(record: object) -> None:
    for item in fields(record):
        if item.name != 'citations':
            _finite(item.name, getattr(record, item.name), positive=True)


@dataclass(frozen=True)
class FlexuralBucklingInput:
    reference_strength_pa: float
    elastic_strength_pa: float
    fibre_distance_m: float
    radius_of_gyration_m: float

    def __post_init__(self) -> None:
        _positive_fields(self)


@dataclass(frozen=True)
class FlexuralBucklingStrength:
    slenderness: float
    imperfection: float
    characteristic_strength_pa: float
    citations: tuple[Citation, ...]


def characteristic_flexural_strength(p: FlexuralBucklingInput, *, basis: PanelMethodBasis) -> FlexuralBucklingStrength:
    """Return the source column curve (Section 7.5, eqn7.21/7.22/7.25).

    lambda=sqrt(fr/fE), mu=(0.34+0.08*z/i)*(lambda-0.2).
    fk=fr for lambda<=0.2; otherwise the smaller quadratic root is evaluated
    in its rationalized form to avoid cancellation. Reported mu is unused
    and can be negative on the short-column plateau. The same slenderness
    enters both mu and the root; conflicting source cell references are not
    implied by this general relation.
    """
    citations = _citations(basis)
    lam = sqrt(p.reference_strength_pa / p.elastic_strength_pa)
    mu = (0.34 + 0.08*p.fibre_distance_m/p.radius_of_gyration_m)*(lam-0.2)
    if lam <= 0.2:
        strength = p.reference_strength_pa
    else:
        a = 1 + mu + lam*lam
        strength = 2*p.reference_strength_pa/(a + sqrt(a*a-4*lam*lam))
    _finite('characteristic_strength_pa', strength, positive=True)
    return FlexuralBucklingStrength(lam, mu, strength, citations)


@dataclass(frozen=True)
class ShearBucklingInput:
    youngs_modulus_pa: float
    yield_strength_pa: float
    width_m: float
    thickness_m: float
    span_m: float
    plate_inertia_m4: float
    stiffener_inertia_m4: float
    local_coefficient: float
    local_elastic_factor: float
    global_elastic_factor: float
    material_factor: float

    def __post_init__(self) -> None:
        _positive_fields(self)


@dataclass(frozen=True)
class ShearResistance:
    yield_design_pa: float
    local_elastic_pa: float
    local_design_pa: float
    global_elastic_pa: float
    global_design_pa: float
    design_pa: float
    citations: tuple[Citation, ...]


def panel_shear_resistance(p: ShearBucklingInput, *, basis: PanelMethodBasis) -> ShearResistance:
    """Minimum of yield, local and global shear design resistance (Pa).

    Source: Section 7.2, eqn7.6; Section 7.6, eqn7.45 and eqn7.46.
    Local elastic = kl*cl*E*(t/s)^2. Global elastic =
    cg*E/(s*t*l^2)*(Ip*Is^3)^0.25. Each is divided by gamma;
    yield design = fy/(sqrt(3)*gamma). Coefficient qualification is an input gap.
    """
    citations = _citations(basis)
    local = p.local_coefficient*p.local_elastic_factor*p.youngs_modulus_pa*(p.thickness_m/p.width_m)**2
    global_elastic = p.global_elastic_factor*p.youngs_modulus_pa/(p.width_m*p.thickness_m*p.span_m**2)*(p.plate_inertia_m4*p.stiffener_inertia_m4**3)**0.25
    yield_design = p.yield_strength_pa/(sqrt(3)*p.material_factor)
    local_design, global_design = local/p.material_factor, global_elastic/p.material_factor
    for name, value in [('local', local), ('global', global_elastic), ('yield', yield_design), ('local design', local_design), ('global design', global_design)]:
        _finite(name, value, positive=True)
    return ShearResistance(yield_design, local, local_design, global_elastic, global_design, min(yield_design, local_design, global_design), citations)


@dataclass(frozen=True)
class PanelResistanceInput:
    """Resolved effective area/moduli and strengths, all SI.

    The two compression moment strengths are separate required inputs because
    their support-length selection is not established by the source.
    """

    axial_area_m2: float
    plate_modulus_m3: float
    stiffener_modulus_m3: float
    yield_strength_pa: float
    plate_strength_pa: float
    stiffener_strength_pa: float
    moment_strength_1_pa: float
    moment_strength_2_pa: float
    material_factor: float
    euler_n: float
    shear_pa: float

    def __post_init__(self) -> None:
        _positive_fields(self)


@dataclass(frozen=True)
class PanelResistances:
    axial_yield_n: float
    axial_plate_n: float
    axial_stiffener_n: float
    moment_plate_nm: float
    moment_stiffener_1_nm: float
    moment_stiffener_2_nm: float
    moment_tension_nm: float
    euler_n: float
    shear_pa: float
    citations: tuple[Citation, ...] = ()

    def __post_init__(self) -> None:
        _positive_fields(self)


def design_panel_resistances(p: PanelResistanceInput, *, basis: PanelMethodBasis) -> PanelResistances:
    """Return A*f/gamma (N) and W*f/gamma (N.m), Section 7.7.3.

    Source locators are eqn7.65, eqn7.67, eqn7.66, eqn7.71,
    eqn7.68, eqn7.69 and eqn7.70. Euler force is supplied from an existing
    column solver; no parallel Euler or section-property implementation is used.
    """
    citations = _citations(basis)
    a, wp, ws, gamma = p.axial_area_m2, p.plate_modulus_m3, p.stiffener_modulus_m3, p.material_factor
    return PanelResistances(
        a*p.yield_strength_pa/gamma, a*p.plate_strength_pa/gamma,
        a*p.stiffener_strength_pa/gamma, wp*p.yield_strength_pa/gamma,
        ws*p.moment_strength_1_pa/gamma, ws*p.moment_strength_2_pa/gamma,
        ws*p.yield_strength_pa/gamma, p.euler_n, p.shear_pa, citations,
    )


@dataclass(frozen=True)
class FixedEndMoments:
    end_moment_nm: float
    midspan_moment_nm: float
    citations: tuple[Citation, ...]


def fixed_end_moments(line_load_n_per_m: float, span_m: float, *, basis: PanelMethodBasis) -> FixedEndMoments:
    """q*l^2/12 and q*l^2/24, Section 7.7, Equation (number unstated).

    Values are moment magnitudes for fixed-fixed uniform loading. The pressure
    face is selected when interpreting the separate interaction result tuples.
    """
    citations = _citations(basis)
    _finite('line_load_n_per_m', line_load_n_per_m, nonnegative=True)
    _finite('span_m', span_m, positive=True)
    moment = line_load_n_per_m*span_m**2/12
    _finite('end_moment_nm', moment, nonnegative=True)
    return FixedEndMoments(moment, moment/2, citations)


@dataclass(frozen=True)
class PanelActions:
    axial_force_n: float
    end_moment_nm: float
    midspan_moment_nm: float
    eccentricity_m: float
    shear_stress_pa: float

    def __post_init__(self) -> None:
        for name in ('axial_force_n', 'end_moment_nm', 'midspan_moment_nm'):
            _finite(name, getattr(self, name), nonnegative=True)
        _finite('eccentricity_m', self.eccentricity_m)
        _finite('shear_stress_pa', self.shear_stress_pa)


@dataclass(frozen=True)
class PanelInteraction:
    plate_pressure_utilizations: tuple[float, float, float, float]
    stiffener_pressure_utilizations: tuple[float, float, float, float]
    citations: tuple[Citation, ...]

    @property
    def plate_pressure_maximum(self) -> float:
        return max(self.plate_pressure_utilizations)

    @property
    def stiffener_pressure_maximum(self) -> float:
        return max(self.stiffener_pressure_utilizations)

    @property
    def maximum(self) -> float:
        return max(self.plate_pressure_maximum, self.stiffener_pressure_maximum)


def combined_panel_utilizations(a: PanelActions, r: PanelResistances, *, basis: PanelMethodBasis) -> PanelInteraction:
    """Eight dimensionless interactions with Euler moment amplification.

    Plate pressure: Section 7.71, eqn7.50, eqn7.51, eqn7.52, eqn7.53,
    as transcribed in a legacy engineering workbook. Opposite-face equations:
    clause not stated in source. N is compression; moments are magnitudes.
    Plate-4 and stiffener-2 use Nkp,Rd, checked against DnV-RP-C201,
    October 2010, Section 7.7.1, eqn7.53 and eqn7.55.
    u: DnV-RP-C201, Section 7.7, eqn7.58 (as stated in source).
    N*z uses N.m consistently on both pressure faces. At or above NE no
    interaction value is returned because the amplification has left its domain.
    """
    citations = _citations(basis)
    if a.axial_force_n >= r.euler_n:
        raise ValueError('axial force must be below the Euler load')
    n, p = a.axial_force_n/r.axial_stiffener_n, a.axial_force_n/r.axial_plate_n
    y = 2*a.axial_force_n/r.axial_yield_n
    d = 1-a.axial_force_n/r.euler_n
    e = a.axial_force_n*a.eccentricity_m
    u = (a.shear_stress_pa/r.shear_pa)**2
    m1, m2 = a.end_moment_nm, a.midspan_moment_nm
    plate = (
        n+(m1-e)/(r.moment_stiffener_1_nm*d)+u,
        p-y+(m1-e)/(r.moment_plate_nm*d)+u,
        n-y+(m2+e)/(r.moment_tension_nm*d)+u,
        p+(m2+e)/(r.moment_plate_nm*d)+u,
    )
    stiffener = (
        n-y+(m1+e)/(r.moment_tension_nm*d)+u,
        p+(m1+e)/(r.moment_plate_nm*d)+u,
        n+(m2-e)/(r.moment_stiffener_2_nm*d)+u,
        p-y+(m2-e)/(r.moment_plate_nm*d)+u,
    )
    for value in plate + stiffener:
        _finite('utilization', value)
    provenance = tuple(dict.fromkeys(citations + r.citations))
    return PanelInteraction(plate, stiffener, provenance)
