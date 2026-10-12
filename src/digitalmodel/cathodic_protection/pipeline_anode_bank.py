"""Remote terminal-anode banks and conservative F103 line attenuation."""

from __future__ import annotations

import math
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import F103Edition, normalize_f103_edition
from digitalmodel.cathodic_protection.anode_sizing import depleted_equivalent_radius
from digitalmodel.cathodic_protection.b401_tables import AnodeEnvironment, AnodeMaterial
from digitalmodel.cathodic_protection._pipeline_anode_bank_models import (
    AnodeBankDesignInput, AnodeBankDesignResult, BankAnodeInput, BankInput,
    BankResult, CoatingResult, DemandResult, EnvelopeResult, PhaseValues,
    PipelineSideInput, ResistanceResult, SideResult, StatusResult,
    StructureDemandInput,
)
from digitalmodel.cathodic_protection.f103_tables import (
    Exposure, FieldJointCoating, FieldJointCoating2019, LinepipeCoating,
    anode_bank_formula_citations, anode_capacity, anode_closed_circuit_potential,
    edition_provenance, field_joint_coating_constants, linepipe_coating_constants,
    mean_current_density, protection_potential,
)
from digitalmodel.citations import Citation, validate_citation


def _material(value: str) -> AnodeMaterial:
    names = {"aluminium": AnodeMaterial.ALUMINIUM, "aluminum": AnodeMaterial.ALUMINIUM,
             "zinc": AnodeMaterial.ZINC}
    try:
        return names[value.lower()]
    except KeyError as exc:
        raise ValueError(f"unsupported anode material {value!r}") from exc


def _factors(a: float, b: float, life: float) -> tuple[float, float, float]:
    return (kernel.coating_breakdown_linear(a, b, 0.0),
            kernel.coating_breakdown_mean(a, b, life),
            kernel.coating_breakdown_final(a, b, life))


def _joint(side: PipelineSideInput, edition: F103Edition) -> Any:
    if side.field_joint_coating is None:
        return None
    enum = FieldJointCoating if edition == "2010" else FieldJointCoating2019
    return enum(side.field_joint_coating)


def _side_basis(side: PipelineSideInput, life: float, edition: F103Edition) -> dict[str, Any]:
    density = mean_current_density(Exposure(side.exposure), side.fluid_temperature_c, edition)
    a_lp, b_lp = linepipe_coating_constants(
        LinepipeCoating(side.linepipe_coating), edition, side.concrete_weight_coating
    )
    lp = _factors(a_lp.value, b_lp.value, life)
    fjc = (0.0, 0.0, 0.0)
    citations = [density.citation, a_lp.citation, b_lp.citation]
    joint = _joint(side, edition)
    if joint is not None:
        a_fj, b_fj = field_joint_coating_constants(joint, edition)
        fjc = _factors(a_fj.value, b_fj.value, life)
        citations.extend([a_fj.citation, b_fj.citation])
    fraction = side.field_joint_area_fraction
    total_area = math.pi * side.outer_diameter_m * side.length_m
    areas = (total_area * (1.0 - fraction), total_area * fraction)
    demand = PhaseValues(**dict(zip(
        ("initial", "mean", "final"),
        [density.value * (areas[0] * lp[i] + areas[1] * fjc[i]) for i in range(3)],
    )))
    ratio = fraction / (1.0 - fraction)
    coating = CoatingResult(
        linepipe_initial_factor=lp[0], linepipe_mean_factor=lp[1],
        linepipe_final_factor=lp[2], field_joint_initial_factor=fjc[0],
        field_joint_mean_factor=fjc[1], field_joint_final_factor=fjc[2],
        field_joint_to_linepipe_ratio=ratio,
    )
    return {"input": side, "density": density.value, "demand": demand,
            "coating": coating, "effective": lp[2] + ratio * fjc[2],
            "citations": citations, "areas": areas}


def _structure(value: StructureDemandInput) -> PhaseValues:
    return PhaseValues(
        initial=kernel.current_demand(value.area_m2, value.initial_current_density_A_m2,
                                      value.initial_breakdown_factor),
        mean=kernel.current_demand(value.area_m2, value.mean_current_density_A_m2,
                                   value.mean_breakdown_factor),
        final=kernel.current_demand(value.area_m2, value.final_current_density_A_m2,
                                    value.final_breakdown_factor),
    )


def _resistance(anode: BankAnodeInput, count: int) -> ResistanceResult:
    fresh = kernel.equivalent_radius_from_mass(anode.net_mass_kg, anode.length_m,
                                               anode.density_kg_m3)
    final = depleted_equivalent_radius(anode.net_mass_kg, anode.length_m,
                                       anode.utilisation_factor, anode.density_kg_m3)
    individual = [kernel.slender_standoff(anode.electrolyte_resistivity_ohm_m,
                                          anode.length_m, radius)
                  for radius in (fresh, final)]
    parallel = [kernel.parallel_resistance([value] * count) for value in individual]
    total = [anode.interaction_factor * value + anode.cable_resistance_ohm
             for value in parallel]
    return ResistanceResult(
        individual=PhaseValues(initial=individual[0], mean=individual[0], final=individual[1]),
        parallel=PhaseValues(initial=parallel[0], mean=parallel[0], final=parallel[1]),
        total=PhaseValues(initial=total[0], mean=total[0], final=total[1]),
        interaction_factor=anode.interaction_factor, cable=anode.cable_resistance_ohm,
        group_formula="R_total = interaction_factor * R_parallel + R_cable",
    )


def _output_scores(
    structure: PhaseValues, bases: list[dict[str, Any]], resistance: ResistanceResult,
    driving: float,
) -> tuple[dict[str, float], dict[str, float]]:
    totals = {phase: getattr(structure, phase) +
              sum(getattr(basis["demand"], phase) for basis in bases)
              for phase in ("initial", "mean", "final")}
    scores = {"initial_output": _ratio(driving / resistance.total.initial,
                                        totals["initial"]),
              "final_output": _ratio(driving / resistance.total.final,
                                      totals["final"])}
    return totals, scores


def _evaluate(bank: BankInput, bases: list[dict[str, Any]], structure: PhaseValues,
              count: int, ea: float, ep: float) -> tuple[ResistanceResult, dict[str, float], list[SideResult]]:
    resistance = _resistance(bank.anode, count)
    driving = ep - ea
    totals, scores = _output_scores(structure, bases, resistance, driving)
    side_results: list[SideResult] = []
    bank_potential = ea + totals["final"] * resistance.total.final
    for basis in bases:
        side: PipelineSideInput = basis["input"]
        q_metal = math.pi * side.outer_diameter_m * basis["effective"] * basis["density"]
        q_bank = basis["demand"].final / side.length_m
        r_line = kernel.longitudinal_resistance_per_m(
            side.steel_resistivity_ohm_m, side.outer_diameter_m, side.wall_thickness_m
        )
        metallic = kernel.conservative_metallic_drop(r_line, q_metal, side.length_m)
        far = bank_potential + metallic
        scores[f"attenuation:{side.side_id}"] = driving / (
            resistance.total.final * totals["final"] + metallic
        )
        fixed = totals["final"] - basis["demand"].final
        extended = None
        try:
            extended = kernel.positive_quadratic_root(
                r_line * q_metal, resistance.total.final * q_bank,
                ea + resistance.total.final * fixed - ep,
            )
        except ValueError:
            pass
        eq20 = kernel.positive_quadratic_root(
            r_line * q_metal, 2.0 * resistance.total.final * q_metal, ea - ep
        )
        distances = [side.length_m * f for f in (0.0, 0.25, 0.5, 0.75, 1.0)]
        potentials = [bank_potential + r_line * q_metal * side.length_m * x
                      for x in distances]
        side_results.append(SideResult(
            side_id=side.side_id,
            geometry_m={"outer_diameter": side.outer_diameter_m,
                        "wall_thickness": side.wall_thickness_m, "length": side.length_m,
                        "linepipe_area": basis["areas"][0], "field_joint_area": basis["areas"][1]},
            coating=basis["coating"], current_demand_A=basis["demand"],
            effective_final_breakdown_factor=basis["effective"],
            longitudinal_resistance_ohm_m=r_line, f103_eq20_protected_length_m=eq20,
            extended_protected_length_m=extended, far_potential_V=far,
            protection_margin_V=ep - far, protection_ok=far <= ep,
            potential_envelope=EnvelopeResult(distance_m=distances, potential_V=potentials),
        ))
    return resistance, scores, side_results


def _ratio(available: float, required: float) -> float:
    """Dimensionless adequacy ratio, omitting a zero requirement from failure."""
    return math.inf if required == 0.0 else available / required


def _governing(scores: dict[str, float]) -> str:
    return min(scores, key=lambda key: (scores[key], key))


def _cable_limit_possible(
    bank: BankInput, bases: list[dict[str, Any]], total: PhaseValues, ea: float, ep: float
) -> bool:
    """Check the infinite-anode resistance asymptote set by the cable."""
    cable = bank.anode.cable_resistance_ohm
    driving = ep - ea
    scores = [_ratio(driving / cable, total.initial),
              _ratio(driving / cable, total.final)] if cable else [math.inf]
    for basis in bases:
        side: PipelineSideInput = basis["input"]
        q_metal = math.pi * side.outer_diameter_m * basis["effective"] * basis["density"]
        line_r = kernel.longitudinal_resistance_per_m(
            side.steel_resistivity_ohm_m, side.outer_diameter_m, side.wall_thickness_m
        )
        drop = cable * total.final + kernel.conservative_metallic_drop(
            line_r, q_metal, side.length_m
        )
        scores.append(driving / drop)
    return min(scores) >= 1.0


def _search_requirements(
    bank: BankInput, bases: list[dict[str, Any]], structure: PhaseValues,
    total: PhaseValues, required_mass: float, max_count: int, ea: float, ep: float,
) -> dict[str, Any]:
    counts = {"mass": math.ceil(required_mass / bank.anode.net_mass_kg),
              "initial": None, "final": None, "attenuation": None}
    recommended = None
    verification = None
    for candidate in range(1, max_count + 1):
        resistance, scores, sides = _evaluate(bank, bases, structure, candidate, ea, ep)
        scores["mass"] = candidate * bank.anode.net_mass_kg / required_mass
        for key in ("initial", "final"):
            if counts[key] is None and scores[f"{key}_output"] >= 1.0:
                counts[key] = candidate
        if counts["attenuation"] is None and all(side.protection_ok for side in sides):
            counts["attenuation"] = candidate
        if min(scores.values()) >= 1.0:
            recommended = candidate
            verification = {"minimum_score": min(scores.values()),
                            "final_resistance_ohm": resistance.total.final}
            break
    outcome = "pass" if recommended else (
        "search_cap_exhausted" if _cable_limit_possible(bank, bases, total, ea, ep)
        else "cable_limited_impossible"
    )
    return {"count_by_mass": counts["mass"],
            "count_by_initial_output": counts["initial"],
            "count_by_final_output": counts["final"],
            "count_by_attenuation": counts["attenuation"],
            "recommended_anode_count": recommended, "search_outcome": outcome,
            "recommended_count_verification": verification}


def _bank(bank: BankInput, life: float, edition: F103Edition, max_count: int,
          ea: float, ep: float, capacity: float) -> tuple[BankResult, list[Citation]]:
    bases = [_side_basis(side, life, edition) for side in bank.sides]
    structure = _structure(bank.structure)
    pipeline = PhaseValues(**{phase: sum(getattr(b["demand"], phase) for b in bases)
                              for phase in ("initial", "mean", "final")})
    total = PhaseValues(**{phase: getattr(structure, phase) + getattr(pipeline, phase)
                           for phase in ("initial", "mean", "final")})
    required_mass = kernel.anode_mass(total.mean, life, capacity,
                                      bank.anode.utilisation_factor)
    resistance, scores, sides = _evaluate(bank, bases, structure,
                                           bank.installed_anode_count, ea, ep)
    scores["mass"] = bank.installed_anode_count * bank.anode.net_mass_kg / required_mass
    search = _search_requirements(bank, bases, structure, total, required_mass,
                                  max_count, ea, ep)
    governing = _governing(scores)
    passed = min(scores.values()) >= 1.0
    checks = {key: value >= 1.0 for key, value in scores.items()}
    requirements = {
        "required_mass_kg": required_mass,
        "installed_mass_kg": bank.installed_anode_count * bank.anode.net_mass_kg,
        "installed_anode_count": bank.installed_anode_count,
        **search,
    }
    citations = [citation for basis in bases for citation in basis["citations"]]
    return BankResult(
        bank_id=bank.bank_id, current_demand_A=DemandResult(
            structure=structure, pipeline=pipeline, total=total),
        anode_resistance_ohm=resistance, anode_requirements=requirements,
        sides=sides, check_scores=scores,
        status=StatusResult(result="PASS" if passed else "FAIL", governing_case=governing,
                            reason=f"minimum adequacy ratio {scores[governing]:.3f}", checks=checks),
    ), citations


def _validate_system(data: AnodeBankDesignInput) -> None:
    bank_ids = [bank.bank_id for bank in data.banks]
    if len(bank_ids) != len(set(bank_ids)):
        raise ValueError("bank_id values must be unique")
    side_ids = [side.side_id for bank in data.banks for side in bank.sides]
    if len(side_ids) != len(set(side_ids)):
        raise ValueError("side_id values must be unique; coupled/shared sides are unsupported")


def _resistance_citation(edition: F103Edition) -> Citation:
    is_2019 = edition == "2019"
    return Citation(
        code_id="dnv-rp-b401", publisher="DNV",
        revision="2017-06" if is_2019 else "2011",
        section="Table A-7" if is_2019 else "Table 10-7",
        wiki_path=("wikis/engineering-standards/wiki/standards/dnv-rp-b401-2017.md"
                   if is_2019 else
                   "wikis/engineering-standards/wiki/standards/dnv-rp-b401.md"),
        note="individual stand-off anode resistance",
    )


def design_anode_bank_cp(data: AnodeBankDesignInput) -> AnodeBankDesignResult:
    """Design independent remote banks and check every protected pipeline side."""
    edition = normalize_f103_edition(data.edition)
    _validate_system(data)
    protection = protection_potential(edition)
    formula_citations = list(anode_bank_formula_citations(edition))
    b401 = _resistance_citation(edition)
    banks, citations = [], [protection.citation, b401, *formula_citations]
    for bank in data.banks:
        material = _material(bank.anode.material)
        capacity = anode_capacity(material, AnodeEnvironment.SEAWATER, edition,
                                  bank.anode.surface_temperature_c)
        potential = anode_closed_circuit_potential(
            material, AnodeEnvironment.SEAWATER, edition,
            bank.anode.surface_temperature_c)
        result, bank_citations = _bank(bank, data.design_life_years, edition,
                                       data.max_anode_count, potential.value,
                                       protection.value, capacity.value)
        banks.append(result)
        citations.extend([capacity.citation, potential.citation, *bank_citations])
    unique = list({(c.code_id, c.revision, c.section, c.wiki_path): c for c in citations}.values())
    for citation in unique:
        validate_citation(citation)
    bank_governing = min(
        banks, key=lambda item: (min(item.check_scores.values()), item.bank_id)
    )
    governing = _governing(bank_governing.check_scores)
    passed = all(bank.status.result == "PASS" for bank in banks)
    return AnodeBankDesignResult(
        standard="DNV-RP-F103", edition=edition, provenance=edition_provenance(edition),
        design_life_years=data.design_life_years, banks=banks,
        citations=[f"{c.code_id} {c.revision} {c.section}" for c in unique],
        formula_references=[
            "F103 Sec. 6.7 Eq. (15), Eq. (16), Eq. (18), Eq. (20)" if edition == "2019" else "F103 Sec. 5.6 Eq. (14), Eq. (15), Eq. (16), Eq. (17)",
            "B401 Table A-7 individual resistance" if edition == "2019" else "B401 Table 10-7 individual resistance",
            "ideal parallel circuit: 1/R_parallel = sum(1/R_a,j)",
        ],
        model_limitations=["independent banks only; coupled asymmetric terminal sources are not solved",
                           "conservative Eq. (15) voltage-drop envelope; not a distributed-current profile",
                           "anode interaction represented only by the supplied interaction factor"],
        status=StatusResult(result="PASS" if passed else "FAIL",
                            governing_case=f"bank:{bank_governing.bank_id}/{governing}",
                            reason=bank_governing.status.reason,
                            checks={f"bank:{b.bank_id}/{k}": v for b in banks
                                    for k, v in b.status.checks.items()},
                            use_status="engineering-validation-required"),
    )
