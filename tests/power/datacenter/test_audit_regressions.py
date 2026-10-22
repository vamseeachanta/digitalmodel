"""Regressions for issue 2749, items 1, 3 and 4.

All ratings and efficiencies below are synthetic test inputs, not equipment
recommendations. Expected values are derived by hand in each test docstring.
The local rules and rating convention are specified in the module knowledge pack.
"""

import math

import pytest

from digitalmodel.power.datacenter import (
    DataCenterPowerPath,
    PowerComponent,
    RedundancyScheme,
    STSModule,
    TopologyValidator,
    UPSModule,
)


# Issue 2749 item 3: finite efficiency in (0, 1]; -1 and 1.5 straddle the bounds.
@pytest.mark.parametrize(
    "efficiency", [math.nan, math.inf, -math.inf, 0.0, -0.0, -1.0, 1.5]
)
def test_sts_rejects_invalid_efficiency(efficiency):
    """Nonfinite values and values outside 0 < eta <= 1 cannot define losses.

    Removing the STS efficiency guard permits these invalid components to enter
    a path and produce undefined, negative or inflated effective capacity.
    """
    with pytest.raises(ValueError, match="efficiency"):
        STSModule(rated_kva=1.0, efficiency=efficiency)


# Synthetic boundary inputs: smallest positive float, 1/2, and lossless 1.
@pytest.mark.parametrize("efficiency", [math.nextafter(0.0, 1.0), 0.5, 1.0])
def test_sts_accepts_positive_efficiency_through_unity(efficiency):
    """At 1 kVA, multiplying by eta gives eta kVA, including both boundaries.

    An unintended positive lower threshold or exclusion of unity loses a valid
    endpoint of the specified local efficiency domain.
    """
    path = DataCenterPowerPath(
        "boundary", [STSModule(rated_kva=1.0, efficiency=efficiency)]
    )
    assert path.effective_capacity_kva == efficiency


@pytest.mark.parametrize("scheme", [RedundancyScheme.N, RedundancyScheme.N_PLUS_1])
def test_single_ups_is_local_level_two_without_redundancy_proof(scheme):
    """One UPS meets local level 2; neither declared scheme proves redundancy.

    Issue 2749 item 1, probe 1: this integer is not a certified Tier II result.
    The local rule tests UPS presence, not installed or required unit counts.
    """
    path = DataCenterPowerPath("single", [UPSModule(rated_kva=1.0)])
    validator = TopologyValidator([path], redundancy_scheme=scheme)
    assert validator.classify_local_tier() == 2
    assert validator.classify_tier() == 2


def test_dual_paths_with_one_sts_are_local_level_three_without_ups():
    """Issue 2749 item 1, probe 2: two paths, one STS, no shared objects => 3.

    UPS redundancy and concurrent maintainability are not established by these
    inputs. Treating this as an official tier would overstate the result.
    """
    paths = [
        DataCenterPowerPath("first", [STSModule(rated_kva=1.0)]),
        DataCenterPowerPath("second", [PowerComponent(rated_kva=1.0)]),
    ]
    validator = TopologyValidator(paths)
    assert validator.classify_local_tier() == 3
    assert validator.classify_tier() == 3


def test_dual_independent_sts_paths_do_not_imply_local_level_four():
    """Issue 2749 item 1, probe 3: 2N with two STS paths still gives local 3.

    The local level-4 rule requires three paths and 2N+1. Active-path state is
    absent from the model; fault tolerance cannot be determined from this test.
    """
    paths = [
        DataCenterPowerPath(label, [STSModule(rated_kva=1.0)])
        for label in ("first", "second")
    ]
    validator = TopologyValidator(paths, redundancy_scheme=RedundancyScheme.TWO_N)
    assert validator.classify_local_tier() == 3
    assert validator.classify_tier() == 3


def test_upstream_bottleneck_uses_input_rating_and_full_chain_losses():
    """Independent hand calculation for the documented local approximation.

    Synthetic inputs: source 100 kVA at eta=4/5, UPS 200 kVA at eta=3/4,
    STS 400 kVA at eta=1/2. At equal, nonzero input/output power factors,
    eta=P_out/P_in also equals S_out/S_in. Thus UPS output is 200*3/4=150 kVA;
    chain eta=(4/5)*(3/4)*(1/2)=3/10; min rating=100 kVA; effective=30 kVA.
    Basis: ENERGY STAR UPS Test Method (Dec-2017), section 5 A, equation 2;
    Eligibility Criteria v2.0, section 1 K (power factor). The min-rating
    approximation is local, not an ENERGY STAR sizing method.
    """
    ups = UPSModule(rated_kva=200.0, efficiency=0.75)
    path = DataCenterPowerPath(
        "upstream bottleneck",
        [
            PowerComponent(rated_kva=100.0, efficiency=0.8),
            ups,
            STSModule(rated_kva=400.0, efficiency=0.5),
        ],
    )
    assert ups.output_kva == 150.0
    assert path.bottleneck_kva == 100.0
    assert path.cascaded_efficiency == pytest.approx(0.3)
    assert path.effective_capacity_kva == pytest.approx(30.0)


def test_full_chain_approximation_retains_upstream_losses_at_downstream_bottleneck():
    """Hand calculation: min(400, 200, 100)*(1/2)*(3/4)*(4/5)=30 kVA.

    Synthetic inputs deliberately put the bottleneck last. The retained local
    convention applies every efficiency, including upstream losses, rather than
    claiming a position-dependent nameplate capacity. Basis: issue 2749 item 4
    and the rating convention in the module knowledge pack, section 3.
    """
    path = DataCenterPowerPath(
        "downstream bottleneck",
        [
            PowerComponent(rated_kva=400.0, efficiency=0.5),
            UPSModule(rated_kva=200.0, efficiency=0.75),
            STSModule(rated_kva=100.0, efficiency=0.8),
        ],
    )
    assert path.effective_capacity_kva == pytest.approx(30.0)
