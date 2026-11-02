"""Internal floating roof routine losses and fixed-roof normal vent comparison.

Basis: US EPA AP-42, Vol. I, Ch. 7.1, June 2026, section 7.1.3.2,
Eqs. 2-1 through 2-4, 2-13 through 2-15, 2-18 and 2-19. Industry source:
API MPMS Ch. 19.2, 3rd ed. (2012), EPA reference 5; only the publicly released EPA
method and a small selection of its factors are implemented here.
https://www.epa.gov/system/files/documents/2026-08/c7s1_2026.pdf

Inputs are SI floats with units in their names; annual results are kg/year.
The existing Pint registry converts to the EPA equation units: international
feet, petroleum barrels (42 US gallons), lb/US gallon and lb/lb-mole.
Conversion basis: NIST SP 811 (2008), Appendix B.8 (foot, pound, US gallon).
Numerically, kg/kmol equals lb/lb-mole. No temperature/RVP correlation is
inferred: true vapor pressure at the average liquid surface temperature is
required. The routine method excludes landed roofs, boiling stocks,
deteriorated/permeated seals and tanks vented only through P/V valves.

Normal vent duties use explicit, cited design inputs, including thermal
duties calculated outside this module. No API 2000 table or proprietary
vent sizing method is embedded. A passing comparison establishes only the
entered normal capacity/pressure checks, never API compliance or tank approval.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass, replace
import math
from pathlib import Path
from typing import Mapping

from digitalmodel.citations.schema import (
    Citation,
    CitationResolutionError,
    CitedValue,
    validate_citation,
)
from digitalmodel.reporting.thickness import validate_number
from digitalmodel.units import Q_


_SOURCE_PATH = "wikis/engineering-standards/epa-ap42-7-1.md"
_SOURCE_URL = "https://www.epa.gov/system/files/documents/2026-08/c7s1_2026.pdf"
_SOURCE_SHA256 = "abac2f144b19c667db338c85b48ac99c83b95fc5b9258310628285df6f67a851"
_NORMAL_AIR = "air_Nm3_h_0C_101.3kPa"


def _epa(section: str) -> Citation:
    return Citation(
        code_id="EPA-AP42-7.1",
        publisher="EPA",
        revision="June 2026",
        section=section,
        wiki_path=_SOURCE_PATH,
        source_sibling="generic",
        note=f"{_SOURCE_URL}; SHA-256 {_SOURCE_SHA256}",
    )


def _resolve(citation: Citation, wiki_root: Path | None) -> None:
    """Resolve bundled EPA metadata locally; other sources require a root.

    Metadata resolution establishes source identity, not the correctness of
    arbitrary caller-supplied numerical values or edition applicability.
    No sibling repository is searched implicitly.
    """
    if not isinstance(citation, Citation):
        raise ValueError("a Citation is required for every method/factor citation")
    if citation.wiki_path == _SOURCE_PATH:
        validate_citation(citation, repo_root=Path(__file__).parent)
    elif wiki_root is not None:
        validate_citation(citation, repo_root=wiki_root)
    else:
        raise CitationResolutionError(
            code_id=citation.code_id,
            wiki_path=citation.wiki_path,
            reason="explicit_source_root_required",
        )


def _factor(value: CitedValue, units: str, wiki_root: Path | None) -> None:
    if not isinstance(value, CitedValue):
        raise ValueError("factor must be a CitedValue")
    _resolve(value.citation, wiki_root)
    if value.units != units:
        raise ValueError(f"factor units must be {units!r}")
    validate_number("factor value", value.value, zero=True)


def public_seal_factor(
    seal_type: str,
    *,
    fit: str = "average",
    secondary: str = "none",
    gaps_verified: bool = False,
) -> CitedValue:
    """Selected KRa values [lb-mole/(ft year)] for a WELDED tank shell.

    EPA AP-42 Ch. 7.1 (June 2026), Table 7.1-8: primary-only average/tight
    factors are mechanical-shoe 5.8/1.5, liquid-mounted 1.6/1.0 and
    vapor-mounted 6.7/5.6. Liquid-mounted with rim-mounted secondary is
    0.3/0.2. Other configurations require a caller-supplied CitedValue.
    Table footnote d requires gaps no greater than 1/8 inch over the full
    liquid-level range for tight-fit factors; ``gaps_verified`` attests to
    that inspection, not a dimensional calculation. Deck construction is
    independent of the welded shell classification.
    """
    factors = {
        ("mechanical_shoe", "none"): (5.8, 1.5),
        ("liquid_mounted", "none"): (1.6, 1.0),
        ("vapor_mounted", "none"): (6.7, 5.6),
        ("liquid_mounted", "rim_mounted"): (0.3, 0.2),
    }
    if fit not in ("average", "tight") or (seal_type, secondary) not in factors:
        raise ValueError("unsupported seal type, fit or secondary configuration")
    if fit == "tight" and gaps_verified is not True:
        raise ValueError(
            "tight fit requires gaps_verified=True over the full level range"
        )
    value = factors[(seal_type, secondary)][fit == "tight"]
    return CitedValue(
        value,
        _epa(f"Table 7.1-8: welded shell, {seal_type}, {secondary}, {fit}"),
        "lbmol/(ft*year)",
    )


def public_fitting_factor(name: str) -> CitedValue:
    """Selected zero-wind KFa [lb-mole/year per fitting].

    EPA AP-42 Ch. 7.1 (June 2026), Table 7.1-12: bolted gasketed access
    hatch 1.6; gasketed weighted gauge hatch .47; gasketed weighted vacuum
    breaker 6.2; unslotted guidepole with gasketed cover and pole sleeve
    8.6; round column well with fabric sleeve 10; adjustable IFR leg with
    total sleeve length <12 inches 7.9; fixed leg/hanger without a deck
    opening 0. Actual counts/configurations must be supplied by the caller.
    These are evaporation factors, not vent flow capacities.
    """
    factors = {
        "access_hatch_bolted_gasketed": 1.6,
        "gauge_hatch_gasketed": 0.47,
        "vacuum_breaker_gasketed": 6.2,
        "unslotted_guidepole_gasketed_sleeve": 8.6,
        "column_well_fabric_sleeve": 10.0,
        "ifrt_leg_adjustable": 7.9,
        "fixed_leg_no_opening": 0.0,
    }
    if name not in factors:
        raise ValueError(
            "unsupported fitting; supply a cited factor for its actual configuration"
        )
    return CitedValue(factors[name], _epa(f"Table 7.1-12: {name}"), "lbmol/year")


def gasoline_clingage(shell_condition: str = "light_rust") -> CitedValue:
    """EPA AP-42 (June 2026), Table 7.1-10: gasoline shell clingage.

    Light rust .0015 and dense rust .0075 bbl/(1000 ft2). Selection is an
    explicit shell-condition assumption; no corrosion assessment is made.
    """
    factors = {"light_rust": 0.0015, "dense_rust": 0.0075}
    if shell_condition not in factors:
        raise ValueError("unsupported shell condition; supply a cited clingage value")
    return CitedValue(
        factors[shell_condition],
        _epa(f"Table 7.1-10: gasoline, {shell_condition}"),
        "bbl/(1000*ft^2)",
    )


@dataclass(frozen=True)
class Tank:
    """SI cylinder geometry; fixed-roof column perimeters, NOT deck legs.

    Effective column diameter = perimeter/pi, AP-42 Eq. 2-19 note 4.
    Empty column tuple represents a self-supporting fixed roof.
    """

    diameter_m: float
    height_m: float
    column_perimeters_m: tuple[float, ...] = ()

    @property
    def volume_m3(self) -> float:
        """Gross cylinder capacity pi D² H/4; no operating ullage allowance."""
        validate_number("diameter_m", self.diameter_m)
        validate_number("height_m", self.height_m)
        return math.pi * self.diameter_m**2 * self.height_m / 4


@dataclass(frozen=True)
class Liquid:
    """Stable stock at the average liquid surface temperature.

    Molecular weight is for VAPOR; density is for LIQUID. Product selects
    KC=1 refined or KC=.4 crude per AP-42 Eq. 2-3 definition (June 2026).
    No product-property defaults are supplied.
    """

    vapor_pressure_kpa: float
    atmospheric_pressure_kpa: float
    vapor_molecular_weight_kg_kmol: float
    density_kg_m3: float
    product: str = "refined"


@dataclass(frozen=True)
class DeckFitting:
    name: str
    count: int
    factor: CitedValue


@dataclass(frozen=True)
class IFRTCase:
    """Annual routine operation; flags are applicability attestations.

    Defaults describe an afloat, freely vented tank with stable stock and
    intact seal/fitting materials. Any false applicability flag is rejected.
    Throughput is net liquid withdrawal, not gross pump recirculation.
    Measured seam length takes precedence over the cited AP-42 default SD.
    """

    tank: Tank
    liquid: Liquid
    rim_seal: CitedValue
    fittings: tuple[DeckFitting, ...]
    deck_type: str
    annual_net_throughput_m3: float
    clingage: CitedValue
    seam_length_m: float | None = None
    roof_afloat: bool = True
    freely_vented: bool = True
    stable_stock: bool = True
    intact_seals: bool = True


@dataclass(frozen=True)
class LossResult:
    rim_kg_year: float
    fittings_kg_year: float
    seams_kg_year: float
    withdrawal_kg_year: float
    vapor_pressure_function: float
    citations: tuple[Citation, ...]
    values: tuple[tuple[str, CitedValue], ...]

    @property
    def standing_kg_year(self) -> float:
        """AP-42 (June 2026), Eq. 2-2: LR + LF + LD."""
        return self.rim_kg_year + self.fittings_kg_year + self.seams_kg_year

    @property
    def total_kg_year(self) -> float:
        """AP-42 (June 2026), Eq. 2-1: standing plus withdrawal loss."""
        return self.standing_kg_year + self.withdrawal_kg_year

    def citation_sidecar(self) -> dict:
        """JSON-compatible resolved citations and the actual factor values."""
        return {
            "citations": [asdict(c) for c in self.citations],
            "values": [(name, asdict(v)) for name, v in self.values],
        }


def estimate_losses(case: IFRTCase, *, wiki_root: Path | None = None) -> LossResult:
    """Evaluate AP-42 Ch. 7.1 (June 2026), section 7.1.3.2.

    Eq. 2-4: P*=r/(1+sqrt(1-r))², r=PVA/PA, 0<=r<1.
    Eq. 2-3: LR=KRa D P* MV KC (v=0 for IFRT, note 1).
    Eqs. 2-13/14/15: LF=sum(Ni KFai) P* MV KC at zero wind.
    Eq. 2-18: LD=KD SD D² P* MV KC; KD=.14 bolted, 0 welded;
    SD=Lseam/(pi D²/4), or .20 ft/ft² when unknown (text after Eq. 2-18).
    Eq. 2-19: LW=.943 Q CS WL/D (1+sum(column perimeter/pi)/D).
    D, Q and WL are converted to ft, bbl/year and lb/US gal before evaluation.
    All losses are converted from lb/year to kg/year after evaluation.
    No wind term, control efficiency or gasket credit is inferred.
    """
    for flag in ("roof_afloat", "freely_vented", "stable_stock", "intact_seals"):
        if getattr(case, flag) is not True:
            raise ValueError(f"{flag} must be True for the routine IFRT method")
    tank, liquid = case.tank, case.liquid
    validate_number("diameter_m", tank.diameter_m)
    validate_number("height_m", tank.height_m)
    for perimeter in tank.column_perimeters_m:
        validate_number("column_perimeter_m", perimeter)
    for name in (
        "atmospheric_pressure_kpa",
        "vapor_molecular_weight_kg_kmol",
        "density_kg_m3",
    ):
        validate_number(name, getattr(liquid, name))
    validate_number("vapor_pressure_kpa", liquid.vapor_pressure_kpa, zero=True)
    if liquid.vapor_pressure_kpa >= liquid.atmospheric_pressure_kpa:
        raise ValueError(
            "true vapor pressure must be below absolute atmospheric pressure"
        )
    if liquid.product not in ("refined", "crude"):
        raise ValueError("product must be refined or crude")
    if case.deck_type not in ("welded", "bolted"):
        raise ValueError("deck_type must be welded or bolted")
    validate_number(
        "annual_net_throughput_m3", case.annual_net_throughput_m3, zero=True
    )
    if case.seam_length_m is not None:
        validate_number("seam_length_m", case.seam_length_m, zero=True)
    _factor(case.rim_seal, "lbmol/(ft*year)", wiki_root)
    _factor(case.clingage, "bbl/(1000*ft^2)", wiki_root)
    values = [("KRa", case.rim_seal), ("CS", case.clingage)]
    fitting_sum = 0.0
    for fitting in case.fittings:
        if not isinstance(fitting.name, str) or not fitting.name.strip():
            raise ValueError("fitting name is required")
        if type(fitting.count) is not int or fitting.count < 0:
            raise ValueError("fitting count must be a nonnegative integer")
        _factor(fitting.factor, "lbmol/year", wiki_root)
        fitting_sum += fitting.count * fitting.factor.value
        values.append((f"KFa:{fitting.name}", fitting.factor))

    d_ft = Q_(tank.diameter_m, "m").to("ft").magnitude
    ratio = liquid.vapor_pressure_kpa / liquid.atmospheric_pressure_kpa
    pstar = ratio / (1 + math.sqrt(1 - ratio)) ** 2
    kc = 0.4 if liquid.product == "crude" else 1.0
    kd = 0.14 if case.deck_type == "bolted" else 0.0
    sd = 0.20
    if case.seam_length_m is not None:
        seam_ft = Q_(case.seam_length_m, "m").to("ft").magnitude
        sd = seam_ft / (math.pi * d_ft**2 / 4)
    lb_to_kg = Q_(1, "pound").to("kg").magnitude
    common = pstar * liquid.vapor_molecular_weight_kg_kmol * kc * lb_to_kg
    q_bbl = Q_(case.annual_net_throughput_m3, "m^3").to("bbl").magnitude
    density_lb_gal = Q_(liquid.density_kg_m3, "kg/m^3").to("pound/gallon").magnitude
    column_ratio = sum(p / math.pi for p in tank.column_perimeters_m) / tank.diameter_m
    withdrawal = (
        0.943
        * q_bbl
        * case.clingage.value
        * density_lb_gal
        / d_ft
        * (1 + column_ratio)
        * lb_to_kg
    )
    method = _epa("Section 7.1.3.2; Eqs. 2-1/2/3/4/13/14/15/18/19")
    values.extend(
        (
            ("KC", CitedValue(kc, _epa("Eq. 2-3, product factor"), "dimensionless")),
            ("KD", CitedValue(kd, _epa("Eq. 2-18"), "lbmol/(ft*year)")),
            (
                "SD",
                CitedValue(
                    sd, _epa("Eq. 2-18; measured seam length or default .20"), "ft/ft^2"
                ),
            ),
            (
                "withdrawal_coefficient",
                CitedValue(0.943, _epa("Eq. 2-19"), "1000*ft^3*gal/bbl^2"),
            ),
        )
    )
    citations = tuple(dict.fromkeys([method, *(v.citation for _, v in values)]))
    for citation in citations:
        _resolve(citation, wiki_root)
    result = LossResult(
        case.rim_seal.value * d_ft * common,
        fitting_sum * common,
        kd * sd * d_ft**2 * common,
        withdrawal,
        pstar,
        citations,
        tuple(values),
    )
    for name in (
        "rim_kg_year",
        "fittings_kg_year",
        "seams_kg_year",
        "withdrawal_kg_year",
        "total_kg_year",
    ):
        validate_number(name, getattr(result, name), zero=True)
    return result


def compare_seals(
    case: IFRTCase,
    seals: Mapping[str, CitedValue],
    *,
    wiki_root: Path | None = None,
) -> dict[str, LossResult]:
    """Re-evaluate each seal with all geometry, deck and operating inputs fixed."""
    if not seals:
        raise ValueError("at least one seal is required")
    return {
        name: estimate_losses(replace(case, rim_seal=seal), wiki_root=wiki_root)
        for name, seal in seals.items()
    }


@dataclass(frozen=True)
class NormalVentingBasis:
    """Explicit edition-specific duties for a normal vent comparison.

    API 2000, 7th ed. (2014), section 3.3.2 identifies normal transfer and
    thermal effects; public API scope/contents:
    https://api.org/~/media/files/publications/whats%20new/2000%20e7%20pa.pdf
    This locator is a reference, not a claim that the 7th edition is current.
    All numerical factors and thermal duties are required caller inputs,
    with a resolvable Citation to their chosen edition/design basis.
    Factors are Nm3 air per m3 liquid; thermal duties are Nm3 air/hour.
    The source must address this tank's capacity, stock, climate, insulation
    and operating cases. No credit is taken for the floating deck.
    """

    in_transfer_nm3_per_m3: float
    out_transfer_nm3_per_m3: float
    thermal_in_nm3_h: float
    thermal_out_nm3_h: float
    citation: Citation
    flow_basis: str = _NORMAL_AIR


@dataclass(frozen=True)
class RatedVent:
    """Air capacity at a documented relieving differential, NOT set pressure.

    Pressure and loss are positive magnitudes for both directions, in kPa.
    Loss includes the connected piping, screens and flame arrester at duty.
    Manufacturer-certified curves and installation losses are caller evidence.
    Reference condition: API 2000 (2014), definitions 2.8 and 2.14;
    0 degrees C and 101.3 kPa absolute. SCFH and actual m3/h require conversion.
    """

    capacity_nm3_h: float
    relieving_differential_kpa: float
    connected_loss_kpa: float
    source: str
    flow_basis: str = _NORMAL_AIR


@dataclass(frozen=True)
class NormalVentResult:
    required_in_nm3_h: float
    required_out_nm3_h: float
    in_margin_nm3_h: float
    out_margin_nm3_h: float
    pressure_margin_kpa: float
    vacuum_margin_kpa: float
    pressure_vent: RatedVent
    vacuum_vent: RatedVent
    basis: NormalVentingBasis
    status: str = "COMPARISON_ONLY"

    @property
    def capacity_sufficient(self) -> bool:
        return self.in_margin_nm3_h >= 0 and self.out_margin_nm3_h >= 0

    @property
    def pressure_limits_satisfied(self) -> bool:
        return self.pressure_margin_kpa >= 0 and self.vacuum_margin_kpa >= 0

    @property
    def normal_check_passed(self) -> bool:
        return self.capacity_sufficient and self.pressure_limits_satisfied

    def citation_sidecar(self) -> dict:
        return {
            "citations": [asdict(self.basis.citation)],
            "basis": asdict(self.basis),
            "pressure_vent": asdict(self.pressure_vent),
            "vacuum_vent": asdict(self.vacuum_vent),
        }


def check_normal_venting(
    *,
    fill_m3_h: float,
    withdrawal_m3_h: float,
    basis: NormalVentingBasis,
    pressure_vent: RatedVent,
    vacuum_vent: RatedVent,
    tank_pressure_limit_kpag: float,
    tank_vacuum_limit_kpa: float,
    wiki_root: Path | None = None,
) -> NormalVentResult:
    """Compare normal flow and pressure duties; no device sizing/compliance.

    Duty aggregation corresponding to API 2000 (7th ed., 2014), 3.3.2:
    Qin=withdrawal*in_transfer_factor+thermal_in;
    Qout=fill*out_transfer_factor+thermal_out. Inputs must represent the
    maximum credible normal cases in each direction on a common air basis.
    Margin is rated minus required capacity; relieving differential plus
    connected loss must not exceed the tank's pressure/vacuum limit.

    Emergency fire relief, flashing, gas breakout, blanketing-regulator
    failure, inerting, API 650 Annex H circulation/opening requirements,
    and vent selection/certification are NOT evaluated. A P/V-only fixed
    roof invalidates this module's routine AP-42 emission method even when
    this independent vent comparison passes.
    """
    _resolve(basis.citation, wiki_root)
    for name, value in (("fill_m3_h", fill_m3_h), ("withdrawal_m3_h", withdrawal_m3_h)):
        validate_number(name, value, zero=True)
    for name in ("in_transfer_nm3_per_m3", "out_transfer_nm3_per_m3"):
        validate_number(name, getattr(basis, name))
    for name in ("thermal_in_nm3_h", "thermal_out_nm3_h"):
        validate_number(name, getattr(basis, name), zero=True)
    validate_number("tank_pressure_limit_kpag", tank_pressure_limit_kpag)
    validate_number("tank_vacuum_limit_kpa", tank_vacuum_limit_kpa)
    for item in (basis, pressure_vent, vacuum_vent):
        if item.flow_basis != _NORMAL_AIR:
            raise ValueError(f"flow_basis must be {_NORMAL_AIR}")
    for vent in (pressure_vent, vacuum_vent):
        validate_number("capacity_nm3_h", vent.capacity_nm3_h, zero=True)
        validate_number("relieving_differential_kpa", vent.relieving_differential_kpa)
        validate_number("connected_loss_kpa", vent.connected_loss_kpa, zero=True)
        if not isinstance(vent.source, str) or not vent.source.strip():
            raise ValueError("vent rated-curve source is required")
    required_in = (
        withdrawal_m3_h * basis.in_transfer_nm3_per_m3 + basis.thermal_in_nm3_h
    )
    required_out = fill_m3_h * basis.out_transfer_nm3_per_m3 + basis.thermal_out_nm3_h
    for name, value in (
        ("required_in_nm3_h", required_in),
        ("required_out_nm3_h", required_out),
    ):
        validate_number(name, value, zero=True)
    pressure_margin = tank_pressure_limit_kpag - (
        pressure_vent.relieving_differential_kpa + pressure_vent.connected_loss_kpa
    )
    vacuum_margin = tank_vacuum_limit_kpa - (
        vacuum_vent.relieving_differential_kpa + vacuum_vent.connected_loss_kpa
    )
    return NormalVentResult(
        required_in,
        required_out,
        vacuum_vent.capacity_nm3_h - required_in,
        pressure_vent.capacity_nm3_h - required_out,
        pressure_margin,
        vacuum_margin,
        pressure_vent,
        vacuum_vent,
        basis,
    )
