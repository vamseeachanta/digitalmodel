"""Offline, partial design benchmarks from issue 142 (Plan).

Sources inspected 2026-10-11, at repository revision 0bcfeabd5:
* Issue 142, Plan / Case 1: assumed jacket design inputs, not measurements.
* https://www.bsee.gov/stats-facts/offshore-information, "Links by Region":
  a public data directory, not a jacket CP calculation. The page provides no
  traceable wetted areas, anode mass or CP design values for the issue's case.
  Source identity: bsee-data-center (existing source catalogue). No data copied.
* docs/domains/cathodic_protection/examples/
  calc-010-dnv-b401-2021-fst-design-philosophy.md: Input Parameters,
  Coating Breakdown Factors, Calculation Results and Reproduction note.
* docs/domains/cathodic_protection/examples/
  calc-004-dnvgl-b401-subsea-riser-base.md: Environment, Surface Areas,
  Anode Properties, Design Results and Reproduction note.

Only numerical design inputs in these public repository notes are reused under
its LICENSE (MIT). No private original, working note or licensed table is copied.
These are computational references, not qualified complete physical designs.
Equations follow the public dnv_rp_b401 docstrings and the fixed-length
depletion/design-loop model documented by anode_sizing.design_cp_system.
Expected values are printed source figures or independent arithmetic, never
a production call.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection import b401_tables as tables
from digitalmodel.cathodic_protection import dnv_rp_b401 as b401
from digitalmodel.cathodic_protection import kernel_anode_current_output
from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeSizingInput,
    AnodeType,
    calculate_anode_resistance,
    design_cp_system,
)


HOURS_PER_YEAR = 8760  # tests/fixtures/test_vectors/cathodic_protection/anode_mass_b401.yaml


def _stand_off(rho: float, length: float, radius: float) -> float:
    """Documented long stand-off equation: rho/(2*pi*L)*(ln(4*L/r)-1)."""
    return rho / (2 * math.pi * length) * (math.log(4 * length / radius) - 1)


def _assert_rounded(actual: float, expected: float, places: int) -> None:
    """Tolerance is half the last reported decimal unit; relative slack is zero."""
    assert actual == pytest.approx(expected, rel=0, abs=0.5 * 10 ** -places)


@pytest.mark.parametrize(
    "quantity",
    ["resistance", "mean_current", "final_current", "mass_count", "current_count"],
)
def test_shallow_water_jacket(quantity: str) -> None:
    """Issue Plan: rho=0.20 ohm*m, L=1.0 m, r=0.05 m.

    R=0.20/(2*pi*1.0)*(ln(4*1.0/0.05)-1)=0.10765325 ohm,
    consistent with the plan's 0.1077 ohm at its reported precision.
    The issue assumes 25 deg C, Category I and 25 years; those do not establish
    area or anode mass. Its 114.6/210.1 A and 74/91 counts cannot be re-derived.
    """
    if quantity != "resistance":
        pytest.skip("Issue Plan omits zone areas and net anode mass; no independent demand/count")
    expected = _stand_off(0.20, 1.0, 0.05)
    actual = b401.anode_resistance_slender_standoff(0.20, 1.0, 0.05, edition="2021")
    _assert_rounded(actual, expected, 6)
    _assert_rounded(expected, 0.1077, 4)


# The source's linear coating model is retained. Only its bare-steel current
# densities are mapped to B401 as requested by the issue; this is not the
# Category III table-based coating mapping used by the note's router snippet.
# This distinguishes model selection from numerical error.


def _hull_expected() -> dict[str, float | int | str]:
    """Public hull note inputs; independent design loop with B401 densities.

    f_mean=.02+.005*40/2=.12; f_final=.02+.005*40=.22.
    I_mean=13446.04*.100*.12=161.35248 A (plan 322.7 A uses .200).
    I_final=13446.04*.130*.22=384.556744 A.
    M=I_mean*40*8760/(.85*2000)=33257.593525 kg (plan 66515 kg).
    N_mass=ceil(M/27.5)=1210 (plan 2419); mass governs.
    R_initial=.5875/(1.400+.150); delta_E=-.80-(-1.09)=.29 V.
    Depletion retains length: r_final=sqrt((1-.85)*27.5/(pi*1.4*2750));
    R_final=.5875/(1.4+2*r_final), I_anode=delta_E/R at each stage.
    N_current=ceil(I_demand/I_anode); N=max(N_mass,N_initial,N_final).
    """
    initial = 13446.04 * .200 * .02
    mean = 13446.04 * .100 * (.02 + .005 * 40 / 2)
    final = 13446.04 * .130 * (.02 + .005 * 40)
    mass = mean * 40 * HOURS_PER_YEAR / (.85 * 2000)
    resistance = .5875 / (1.400 + .150)
    radius_final = math.sqrt((1 - .85) * 27.5 / (math.pi * 1.400 * 2750))
    resistance_final = .5875 / (1.400 + 2 * radius_final)
    voltage = -.80 - (-1.09)
    counts = (math.ceil(mass / 27.5), math.ceil(initial / (voltage / resistance)),
              math.ceil(final / (voltage / resistance_final)))
    return dict(current_demand_A=mean, initial_current_demand_A=initial,
                final_current_demand_A=final, total_anode_mass_kg=mass,
                anode_resistance_ohm=resistance, anode_current_output_A=voltage / resistance,
                final_anode_resistance_ohm=resistance_final,
                final_anode_current_output_A=voltage / resistance_final,
                number_of_anodes_mass=counts[0], number_of_anodes_initial=counts[1],
                number_of_anodes_final=counts[2], number_of_anodes=max(counts),
                governing_case=("mass", "initial", "final")[counts.index(max(counts))])


def _hull_actual(*, source_basis: bool = False) -> dict:
    """Public sizing API, retaining source coating, alloy and geometry inputs."""
    densities = [tables.design_current_density(
        tables.Climate.TEMPERATE, tables.DepthBand.M0_30, phase, "2021"
    ).value for phase in (tables.DesignPhase.INITIAL, tables.DesignPhase.MEAN,
                         tables.DesignPhase.FINAL)]
    if source_basis:
        # calc-010, Input Parameters: all three bare-steel densities are 200 mA/m2.
        densities = [.200, .200, .200]
    p = AnodeSizingInput(
        surface_area_m2=13446.04,
        initial_breakdown_factor=b401.coating_breakdown_factor(.02, .005, 0, edition="2021"),
        coating_breakdown_factor=b401.coating_breakdown_factor(.02, .005, 40 / 2, edition="2021"),
        final_breakdown_factor=b401.coating_breakdown_factor(.02, .005, 40, edition="2021"),
        initial_current_density_mA_m2=densities[0] * 1000,
        current_density_mA_m2=densities[1] * 1000,
        final_current_density_mA_m2=densities[2] * 1000,
        design_life_years=40, anode_type=AnodeType.FLUSH_MOUNT,
        anode_length_m=1.400, anode_radius_m=.067,
        anode_width_m=.150, anode_thickness_m=.060,
        anode_net_mass_kg=27.5, anode_density_kg_m3=2750,
        resistivity_ohm_m=.5875, anode_capacity_Ah_kg=2000,
        utilization_factor=.85, driving_voltage_V=-.80 - (-1.09),
    )
    return design_cp_system(p, edition="2021").model_dump()


# Decimal places are the sizing API's output rounding, not an engineering
# uncertainty allowance. Integer counts and governing criterion match exactly.
HULL_METRICS = [
    ("current_demand_A", 4), ("initial_current_demand_A", 4),
    ("final_current_demand_A", 4), ("total_anode_mass_kg", 2),
    ("anode_resistance_ohm", 6), ("anode_current_output_A", 4),
    ("final_anode_resistance_ohm", 6), ("final_anode_current_output_A", 4),
    ("number_of_anodes_mass", None), ("number_of_anodes_initial", None),
    ("number_of_anodes_final", None), ("number_of_anodes", None),
    ("governing_case", None),
]


@pytest.mark.parametrize("quantity,places", HULL_METRICS)
def test_floating_storage_hull_table_mapping(quantity: str, places: int | None) -> None:
    """Mapping onto the recommended-practice table densities, not a reproduction
    of the source. The source's coating, alloy and geometry are retained.

    Independent arithmetic checks API consistency for this mapped scenario;
    it does not establish agreement with an outside design benchmark or with
    the note's router coating mapping. No licensed table text is copied.
    """
    expected = _hull_expected()[quantity]
    actual = _hull_actual()[quantity]
    if places is None:
        assert actual == expected
    else:
        _assert_rounded(actual, expected, places)


@pytest.mark.parametrize("quantity,places,source", [
    ("initial_current_demand_A", 3, 53.784),
    ("current_demand_A", 3, 322.705),
    ("final_current_demand_A", 3, 591.626),
    ("total_anode_mass_kg", 0, 66515),
    ("number_of_anodes_mass", None, 2419),
    ("governing_case", None, "mass"),
    ("anode_resistance_ohm", 3, .379),
])
def test_floating_storage_hull_source_basis(
    quantity: str, places: int | None, source: float | str,
) -> None:
    """Benchmark calc-010 on its own basis, not the router's table mapping.

    Input Parameters gives 200 mA/m2 for initial, mean and final density.
    Calculation Results prints the currents, net mass, mass count, governing
    criterion and initial resistance above; scratch arithmetic from those
    inputs reproduces each within half the last printed digit.
    Source counts by initial/final current are excluded: its depleted-anode
    geometry differs from the sizing module's fixed-length depletion method.
    """
    actual = _hull_actual(source_basis=True)[quantity]
    if places is None:
        assert actual == source
    else:
        _assert_rounded(actual, source, places)


def _riser_expected() -> dict[str, float]:
    """Public riser note: structural subtotal, not the complete group design.

    A=142.6+267.5=410.1 m2 (the source's already-rounded contingency areas).
    f_mean=.02+(.236-.02)/2=.128; f_final=.236.
    I_mean=A*.110*.128=5.774208 A; I_final=A*.170*.236=16.453212 A.
    M=I_mean*27*8760/(.90*2000)=758.730931 kg for stand-off-only sizing.
    The plan gives 9.29 A and 679.65 kg for the complete mixed-anode group.
    Its bare connector area and allocation of capacity between types are absent;
    those totals cannot be reproduced as a complete design from the public note.
    R_stand=.31/(2*pi*1.080)*(ln(4*1.080/.117)-1)=.119180437 ohm.
    Radius .117 m is the approximation explicitly supplied in the note's cfg.
    R_flush=.31/(.950+.186)=.272887324 ohm; delta_E=-.800-(-1.05)=.25 V.
    I_stand=delta_E/R_stand=2.097660 A; I_flush=delta_E/R_flush=.916129 A.
    """
    area = 142.6 + 267.5
    mean = area * .110 * (.020 + (.236 - .020) / 2)
    stand = _stand_off(.31, 1.080, .117)
    flush = .31 / (.950 + .186)
    voltage = -.800 - (-1.05)
    return dict(mean_current=mean, final_current=area * .170 * .236,
                structural_mass=mean * 27 * HOURS_PER_YEAR / (.90 * 2000),
                stand_resistance=stand, stand_current=voltage / stand,
                flush_resistance=flush, flush_current=voltage / flush)


def _riser_actual() -> dict[str, float]:
    """Combine public demand, sizing and resistance functions for known inputs."""
    densities = [tables.design_current_density(
        tables.Climate.ARCTIC, tables.DepthBand.M300_PLUS, phase, "2017"
    ).value for phase in (tables.DesignPhase.MEAN, tables.DesignPhase.FINAL)]
    rate = (.236 - .020) / 27  # Derived from the note; its t-squared prose is not used.
    mean_factor = b401.coating_breakdown_factor(.020, rate, 27 / 2, edition="2017")
    final_factor = b401.coating_breakdown_factor(.020, rate, 27, edition="2017")
    mean = sum(b401.current_demand(a, densities[0], mean_factor, edition="2017")
               for a in (142.6, 267.5))
    final = sum(b401.current_demand(a, densities[1], final_factor, edition="2017")
                for a in (142.6, 267.5))
    stand = b401.anode_resistance_slender_standoff(.31, 1.080, .117, edition="2017")
    flush = calculate_anode_resistance(
        AnodeType.FLUSH_MOUNT, .950, .186 / 2, .31,
        width_m=.186, thickness_m=.136,
    )
    voltage = -.800 - (-1.05)
    return dict(mean_current=mean, final_current=final,
                structural_mass=b401.anode_mass_requirement(
                    mean, 27, E_capacity=2000, u_f=.90, edition="2017"),
                stand_resistance=stand, stand_current=b401.anode_current_output(
                    .31, 1.080, .117, delta_E=voltage, edition="2017"),
                flush_resistance=flush, flush_current=kernel_anode_current_output(voltage, flush))


@pytest.mark.parametrize("quantity,places,plan", [
    pytest.param(q, 6, None, id=q) for q in (
        "mean_current", "final_current", "structural_mass", "stand_resistance",
        "stand_current", "flush_resistance", "flush_current",
    )
] + [
    pytest.param("stand_resistance", 3, .119, id="plan_stand_resistance"),
    pytest.param("stand_current", 2, 2.10, id="plan_stand_current"),
    pytest.param("flush_resistance", 3, .273, id="plan_flush_resistance"),
    pytest.param("flush_current", 3, .916, id="plan_flush_current"),
    pytest.param("mean_current", 2, 9.29, id="plan_group_current"),
    pytest.param("structural_mass", 2, 679.65, id="plan_group_mass"),
])
def test_deepwater_riser_base(quantity: str, places: int, plan: float | None) -> None:
    """Assert known structural/anode quantities, exposing incomplete group scope.

    Six-place arithmetic comparisons follow the resistance API's precision
    (half a micro-unit absolute, no relative slack); source comparisons allow
    only half their last printed unit. No uncertainty in the source is inferred.
    Insulated surfaces, bare connectors and mixed-type allocation are excluded
    from the subtotal. The plan's group figures include surface area absent
    from the public note; the note does not identify the current as initial,
    mean or final, and the two plan figures are inconsistent with each other.
    They cannot be reproduced from the public note and are skipped.
    """
    if plan is not None and quantity in ("mean_current", "structural_mass"):
        pytest.skip(
            "Group area is missing from the public note; current phase is unspecified; "
            "plan current/mass figures are inconsistent and cannot be reproduced"
        )
    expected = _riser_expected()[quantity] if plan is None else plan
    _assert_rounded(_riser_actual()[quantity], expected, places)
