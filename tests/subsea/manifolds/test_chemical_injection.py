# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent hand examples for proposed multi-well chemical distribution.

All dimensional inputs below are synthetic test data, not standard values or
published equipment data. Literal zero/one denote exact domain boundaries,
indices, isolation, or unity ratios. Expected values come from each test's
written arithmetic, never from a result returned by the implementation.

Sources: OpenStax, University Physics vol. 1 (2016), sec. 14.5 (continuity and
Q=dV/dt), sec. 14.7 eq. 14.19 (Poiseuille); DOE-HDBK-1012/3-92 (1992), HT-03
pp. 25–26 (pressure balance), eq. 3-15 p. 34 (quadratic branch loss). Public
source links and input units are in docs/subsea/manifold-chemical-distribution.md.
The relative comparison allowance covers rounding of the hand-calculated
values printed here to at least thirteen significant digits. It is a test
rounding allowance, not an engineering acceptance limit.
"""

import math
from dataclasses import replace

import pytest

from digitalmodel.subsea.manifolds import InjectionDemand, distribute_chemical
from digitalmodel.subsea.umbilicals.chemical_flow import ChemicalFluid, UmbilicalTube

# Rounding allowance: hand-calculation precision documented in this module docstring.
ROUNDING_REL = 1e-12
# Synthetic common-line inputs for the hand example; sources/formulae above.
TUBE = UmbilicalTube(0.01, 10)
FLUID = ChemicalFluid(1000, 0.01)


def demands():
    """Synthetic targets: .001*.001=1e-6 and .002*.001=2e-6 m3/s."""
    return (
        InjectionDemand(0.001, 0.001, 1000, 1e14),
        InjectionDemand(0.002, 0.001, 2000, 2e14),
    )


def test_multi_well_distribution_and_balancing_hand_example():
    """Q=3e-6; d=(1e-6,2e-6); proportional q=d; q/W=.001; q/d=1.

    OpenStax (2016), eq. 14.19: dp=8*mu*L*Q/(pi*r^4), r=d/2, hence
    dp=128*.01*10*3e-6/(pi*.01^4)=1222.309962945756 Pa.
    Node=10000-dp=8777.690037054244 Pa (DOE HT-03 pp. 25–26).
    DOE eq. 3-15: branch losses=(1e14*(1e-6)^2,2e14*(2e-6)^2)
    =(100,800) Pa. Margins=(7677.690037054244,5977.690037054244) Pa.
    Restrictions=margin/q^2=(7.677690037054244e15,1.494422509263561e15).
    """
    result = distribute_chemical(3e-6, demands(), TUBE, FLUID, 10000, 0)
    assert result.delivery.flow_rate == 3e-6
    assert result.delivery.friction_drop == pytest.approx(1222.309962945756, rel=ROUNDING_REL, abs=0)
    assert result.node_pressure_pa == pytest.approx(8777.690037054244, rel=ROUNDING_REL, abs=0)
    assert [w.rate_m3_s for w in result.wells] == pytest.approx([1e-6, 2e-6], rel=ROUNDING_REL, abs=0)
    assert math.fsum(w.rate_m3_s for w in result.wells) == pytest.approx(3e-6, rel=ROUNDING_REL, abs=0)
    assert [w.delivered_fraction for w in result.wells] == pytest.approx([0.001, 0.001], rel=ROUNDING_REL, abs=0)
    assert [w.dose_ratio for w in result.wells] == pytest.approx([1, 1], rel=ROUNDING_REL, abs=0)
    assert [w.restriction_pa_s2_m6 for w in result.wells] == pytest.approx(
        [7.677690037054244e15, 1.494422509263561e15], rel=ROUNDING_REL, abs=0,
    )
    assert [w.pressure_deficit_pa for w in result.wells] == [0, 0]
    assert result.hydraulically_feasible


@pytest.mark.parametrize("total, rates, ratio", [
    (1.5e-6, [0.5e-6, 1e-6], 0.5),
    (6e-6, [2e-6, 4e-6], 2),
])
def test_under_and_over_dose_preserve_the_proportional_policy(total, rates, ratio):
    """Hand division: Q/(1e-6+2e-6)=.5 or 2, so q=d*.5 or d*2.

    Supplied pressure is increased to keep both synthetic branch demands
    feasible. Dose ratios express dosage separately from pressure feasibility.
    """
    result = distribute_chemical(total, demands(), TUBE, FLUID, 100000, 0)
    assert [w.rate_m3_s for w in result.wells] == pytest.approx(rates, rel=ROUNDING_REL, abs=0)
    assert [w.dose_ratio for w in result.wells] == pytest.approx([ratio, ratio], rel=ROUNDING_REL, abs=0)
    assert result.hydraulically_feasible


def test_under_dose_and_hydraulic_deficit_are_reported():
    """Half Q gives dp=611.154981472878; node=1500-dp=888.845018527122.

    q=(.5e-6,1e-6), loss=(25,200), deficits=back+loss-node:
    (136.154981472878,1311.154981472878) Pa. No nonnegative restriction
    can supply the missing pressure (DOE HT-03 pp. 25–26, eq. 3-15).
    """
    result = distribute_chemical(1.5e-6, demands(), TUBE, FLUID, 1500, 0)
    assert [w.dose_ratio for w in result.wells] == pytest.approx([0.5, 0.5], rel=ROUNDING_REL, abs=0)
    assert [w.pressure_deficit_pa for w in result.wells] == pytest.approx(
        [136.154981472878, 1311.154981472878], rel=ROUNDING_REL, abs=0,
    )
    assert all(w.restriction_pa_s2_m6 is None for w in result.wells)
    assert not result.hydraulically_feasible


def test_zero_demand_is_isolated_while_the_other_branch_receives_all_flow():
    """Targets=(0,1e-6); Q=1e-6 gives q=(0,1e-6), ratios=(undefined,1).

    The inactive branch's synthetic backpressure exceeds supply but it is
    isolated by policy; no pressure check is performed for that branch.
    """
    branch = (InjectionDemand(1, 0, 1e9, 0), InjectionDemand(0.001, 0.001, 0, 0))
    result = distribute_chemical(1e-6, branch, TUBE, FLUID, 10000, 0)
    assert result.wells[0].rate_m3_s == 0
    assert result.wells[0].dose_ratio is None
    assert result.wells[0].restriction_pa_s2_m6 is None
    assert result.wells[1].rate_m3_s == pytest.approx(1e-6, rel=ROUNDING_REL, abs=0)
    assert result.wells[1].dose_ratio == pytest.approx(1, rel=ROUNDING_REL, abs=0)
    assert result.hydraulically_feasible


@pytest.mark.parametrize("target_fraction, expected_ratio", [(0, None), (0.001, 0)])
def test_zero_supply_bypasses_shared_delivery(target_fraction, expected_ratio):
    """Q=0 implies every allocated rate and dose is zero; no tube loss.

    An unmet positive target has dose ratio 0; a zero target has undefined
    ratio. Hydraulic feasibility of isolated branches says nothing about dose.
    """
    demand = InjectionDemand(1, target_fraction, 1000, 0)
    result = distribute_chemical(0, (demand,), TUBE, FLUID, 10000, 0)
    assert result.delivery is None
    assert result.node_pressure_pa == 10000
    assert result.wells[0].rate_m3_s == 0
    assert result.wells[0].delivered_fraction == 0
    assert result.wells[0].dose_ratio == expected_ratio
    assert result.hydraulically_feasible


def test_nonzero_supply_without_demand_is_rejected():
    """Continuity policy cannot allocate Q=1e-6 when sum(d)=0."""
    with pytest.raises(ValueError, match="without demand"):
        distribute_chemical(1e-6, (InjectionDemand(1, 0, 0, 0),), TUBE, FLUID, 10000, 0)


@pytest.mark.parametrize("field", [
    "water_rate_m3_s", "target_fraction", "backpressure_pa", "branch_resistance_pa_s2_m6",
])
@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf, -1])
def test_invalid_demand(field, invalid):
    """Every demand input must be finite and nonnegative by definition."""
    with pytest.raises(ValueError):
        replace(demands()[0], **{field: invalid})


@pytest.mark.parametrize("water, fraction", [(0, 0.1), (1, 1.1)])
def test_demand_domain_boundaries(water, fraction):
    """Zero water makes q/W undefined; fraction>1 exceeds the existing API domain."""
    with pytest.raises(ValueError):
        InjectionDemand(water, fraction, 0, 0)


@pytest.mark.parametrize("field", ["total_rate_m3_s", "available_pressure_pa", "vertical_drop_m"])
@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf])
def test_nonfinite_distribution_inputs(field, invalid):
    """Non-finite values have no physical pressure or flow interpretation."""
    args = dict(total_rate_m3_s=3e-6, demands=demands(), tube=TUBE, fluid=FLUID,
                available_pressure_pa=10000, vertical_drop_m=0)
    args[field] = invalid
    with pytest.raises(ValueError):
        distribute_chemical(**args)


@pytest.mark.parametrize("field, invalid", [
    ("total_rate_m3_s", -1), ("available_pressure_pa", -1),
    ("available_pressure_pa", 0), ("demands", ()),
])
def test_distribution_domain_boundaries(field, invalid):
    """Flow is nonnegative, available pressure positive, and branches nonempty."""
    args = dict(total_rate_m3_s=3e-6, demands=demands(), tube=TUBE, fluid=FLUID,
                available_pressure_pa=10000, vertical_drop_m=0)
    args[field] = invalid
    with pytest.raises(ValueError):
        distribute_chemical(**args)


@pytest.mark.parametrize("field", ["inner_diameter", "length", "density", "viscosity"])
@pytest.mark.parametrize("invalid", [math.nan, math.inf])
def test_nonfinite_shared_model_properties(field, invalid):
    """The shared dataclasses accept NaN/inf; this boundary must reject them."""
    tube = replace(TUBE, **{field: invalid}) if hasattr(TUBE, field) else TUBE
    fluid = replace(FLUID, **{field: invalid}) if hasattr(FLUID, field) else FLUID
    with pytest.raises(ValueError):
        distribute_chemical(3e-6, demands(), tube, fluid, 10000, 0)


def test_active_target_underflow_is_rejected():
    """Synthetic demand 1e-300*1e-100 is positive but rounds to binary64 zero.

    Expected ValueError follows representability, not an engineering cutoff.
    """
    with pytest.raises(ValueError, match="target rate"):
        InjectionDemand(1e-300, 1e-100, 0, 0)


def test_total_target_overflow_is_rejected():
    """1e308+1e308 exceeds binary64 range; it must not allocate zero everywhere."""
    huge = InjectionDemand(1e308, 1, 0, 0)
    with pytest.raises(ValueError, match="total target rate"):
        distribute_chemical(3e-6, (huge, huge), TUBE, FLUID, 10000, 0)


@pytest.mark.parametrize("density, pressure, stage", [
    (1e308, 10000, "hydrostatic head"),
    (1e307, 1e308, "node pressure"),
])
def test_head_and_node_pressure_overflow_is_rejected(density, pressure, stage):
    """Extreme synthetic positive head or head+supply exceeds binary64 range."""
    with pytest.raises(ValueError, match=stage):
        distribute_chemical(0, demands(), TUBE, ChemicalFluid(density, 0.01), pressure, 1)


@pytest.mark.parametrize("tube", [UmbilicalTube(1e-200, 10), UmbilicalTube(0.01, 1e308)])
def test_shared_delivery_range_failure_is_rejected(tube):
    """Synthetic bore area rounds to zero, or route friction overflows.

    The wrapper must raise ValueError rather than expose division by zero,
    non-finite delivery diagnostics, or a false feasibility result.
    """
    with pytest.raises(ValueError, match="shared delivery"):
        distribute_chemical(3e-6, demands(), tube, FLUID, 10000, 0)


def test_shared_delivery_validation_message_is_preserved(monkeypatch):
    """An upstream validation error must retain its explanation and cause."""
    from digitalmodel.subsea.manifolds import chemical_injection

    error = ValueError("unsupported delivery input")

    def reject_delivery(*args, **kwargs):
        raise error

    monkeypatch.setattr(chemical_injection, "check_delivery", reject_delivery)
    with pytest.raises(ValueError, match="shared delivery: unsupported delivery input") as caught:
        distribute_chemical(3e-6, demands(), TUBE, FLUID, 10000, 0)
    assert caught.value.__cause__ is error
    assert "arithmetic" not in str(caught.value)


def test_shared_delivery_reynolds_underflow_keeps_validation_message():
    """Synthetic rho*v rounds to zero before the shared Reynolds calculation."""
    with pytest.raises(ValueError, match="shared delivery: reynolds must be > 0") as caught:
        distribute_chemical(1e-300, demands(), TUBE, ChemicalFluid(1e-300, 0.01), 10000, 0)
    assert isinstance(caught.value.__cause__, ValueError)


def test_active_allocated_rate_underflow_is_rejected():
    """Tiny target / total target is ~1e-320; Q=1e-6 makes q round to zero."""
    tiny = InjectionDemand(1e-300, 1e-20, 0, 0)
    normal = InjectionDemand(1, 1, 0, 0)
    with pytest.raises(ValueError, match="allocated rate"):
        distribute_chemical(1e-6, (tiny, normal), TUBE, FLUID, 10000, 0)


@pytest.mark.parametrize("total, stage", [
    (1e-162, "squared branch rate"), (1e-160, "balancing restriction"),
])
def test_branch_arithmetic_range_failure_is_rejected(total, stage):
    """q^2=1e-324 rounds to zero; ~1e4/1e-320 instead exceeds finite range.

    Both synthetic cases have positive active flow, so neither represents an
    isolated branch. No minimum-flow design limit is inferred.
    """
    with pytest.raises(ValueError, match=stage):
        distribute_chemical(total, (InjectionDemand(1, 1, 0, 0),), TUBE, FLUID, 10000, 0)


def test_branch_loss_overflow_is_rejected():
    """q=10 and supplied R=1e308 give R*q^2=1e310, outside finite range."""
    with pytest.raises(ValueError, match="branch pressure loss"):
        distribute_chemical(10, (InjectionDemand(1, 1, 0, 1e308),), TUBE, FLUID, 10000, 0)


def test_dose_ratio_overflow_is_rejected():
    """d~1e-320 and q=1e-6 imply q/d~1e314, outside finite range."""
    with pytest.raises(ValueError, match="well dose"):
        distribute_chemical(1e-6, (InjectionDemand(1e-300, 1e-20, 0, 0),),
                            TUBE, FLUID, 10000, 0)


def test_exact_zero_margin_needs_no_added_restriction():
    """OpenStax eq. 14.19: Q=pi*1e-6 cancels pi in the shared loss.

    dp=128*.01*10*(pi*1e-6)/(pi*.01^4)=1280 Pa. With
    supply=10000 and backpressure=8720, node=8720 and margin=0.
    R=0 adds no branch loss. Exact equality is feasible by pressure balance.
    All values are synthetic; the input Q uses the mathematical constant pi.
    """
    result = distribute_chemical(
        math.pi * 1e-6, (InjectionDemand(1, 1, 8720, 0),), TUBE, FLUID, 10000, 0,
    )
    assert result.node_pressure_pa == 8720
    assert result.wells[0].restriction_pa_s2_m6 == 0
    assert result.wells[0].pressure_deficit_pa == 0
    assert result.hydraulically_feasible
