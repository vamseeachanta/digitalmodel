# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic regressions for liquid choking differentials.

Equation source: Emerson, Control Valve Sourcebook — Oil & Gas (2013),
chapter 3, p. 3-5 (attached-fittings differential) and p. 3-3 (flow equation):
https://www.emerson.com/is/content/emerson/en/final-control/pressure-management/jt-valves/documents/guide-control-valve-sourcebook-oil-gas-fisher-en-135836.pdf
All inputs below are synthetic numerical probes, not equipment ratings.
"""

import sys

import pytest

from digitalmodel.subsea.hipps.valve_sizing import (
    liquid_capacity,
    required_liquid_coefficient,
)


def test_decimal_intermediate_underflow_and_inverse():
    """Hand calculation using the cited equations, with synthetic inputs.

    With C=N1=Fp=SG=FF=1 and p2=pv=0, choose FLP=10^-200,
    p1=10^300. Then dp_max=10^-400 * 10^300=10^-100 < p1,
    q=sqrt(10^-100)=10^-50, and C_required=10^-50/10^-50=1.
    """
    inputs = dict(
        p_upstream=1e300, p_downstream=0.0, specific_gravity=1.0,
        unit_constant=1.0, piping_factor=1.0, recovery_factor=1e-200,
        critical_pressure_ratio=1.0, vapor_pressure=0.0,
    )
    result = liquid_capacity(coefficient=1.0, **inputs)
    # Eight machine epsilons allow rounding of inputs, ratio, square, product
    # and square root; this is an arithmetic tolerance, not a design limit.
    tolerance = 8 * sys.float_info.epsilon
    assert result.effective_drop == pytest.approx(1e-100, rel=tolerance, abs=0)
    assert result.rate == pytest.approx(1e-50, rel=tolerance, abs=0)
    assert result.choked
    assert required_liquid_coefficient(rate=1e-50, **inputs) == pytest.approx(
        1.0, rel=tolerance, abs=0,
    )


def test_nonzero_vapor_extreme_differential_and_inverse():
    """Additional qualification using the cited Sourcebook equations above.

    Synthetic hand basis: p1=2^1000, pv=2^999, p2=0, FF=Fp=1,
    FLP=2^-700, SG=2, N1=2 and C=4. Then head=2^1000-2^999=2^999,
    dp_max=2^(-2*700+999)=2^-401 < p1-p2, so flow is choked.
    sqrt(dp_max/SG)=sqrt(2^-402)=2^-201 and q=2*1*4*2^-201=2^-198.
    Independently, C_required=2^-198/(2*1*2^-201)=4.
    The recovery square 2^-1400 underflows binary64; the final drop does not.
    Exact binary equality is required. These are arithmetic stress probes,
    not qualified operating conditions.
    """
    inputs = dict(
        p_upstream=2.0**1000, p_downstream=0.0, specific_gravity=2.0,
        unit_constant=2.0, piping_factor=1.0, recovery_factor=2.0**-700,
        critical_pressure_ratio=1.0, vapor_pressure=2.0**999,
    )
    result = liquid_capacity(coefficient=4.0, **inputs)
    assert result.choked is True
    assert result.effective_drop == 2.0**-401
    assert result.rate == 2.0**-198
    assert required_liquid_coefficient(rate=2.0**-198, **inputs) == 4.0


@pytest.mark.parametrize(
    "recovery_exp,piping_exp,pressure_exp,drop_exp,rate_exp,choked",
    [
        (-700, 0, 1000, -400, -200, True),
        (-700, -100, 1000, -200, -200, True),
        (-800, 0, 526, -1074, -537, True),
        (-1074, -1074, 1000, 1000, -574, True),
        (700, 0, -1000, -1000, -500, False),
    ],
)
def test_binary_scaled_differentials(
    recovery_exp, piping_exp, pressure_exp, drop_exp, rate_exp, choked,
):
    """Exact hand calculations; each supplied magnitude is a power of two.

    Set C=N1=SG=FF=1, p2=pv=0; let FLP=2^r, Fp=2^f, p1=2^p.
    The cited equations give dp_max=2^(2r-2f+p), dp=min(2^p,dp_max),
    q=2^(f+log2(dp)/2). The rows therefore give (dp,q):
    (2^-400,2^-200), (2^-200,2^-200), (2^-1074,2^-537),
    (2^1000,2^-574), (2^-1000,2^-500). The fourth row uses the least
    positive binary64 subnormal for both factors. The third differential is
    also that subnormal. The last row is subcritical:
    dp_max=2^400, although the intermediate recovery square overflows.
    Equality is required: these powers and square roots are exact in binary.
    """
    result = liquid_capacity(
        coefficient=1.0, p_upstream=2.0**pressure_exp, p_downstream=0.0,
        specific_gravity=1.0, unit_constant=1.0, piping_factor=2.0**piping_exp,
        recovery_factor=2.0**recovery_exp, critical_pressure_ratio=1.0,
        vapor_pressure=0.0,
    )
    assert result.effective_drop == 2.0**drop_exp
    assert result.rate == 2.0**rate_exp
    assert result.choked is choked


@pytest.mark.parametrize("recovery_exp", [-800, 800])
def test_unrepresentable_final_differentials_are_rejected(recovery_exp):
    """Hand calculation: p1=Fp=FF=1, pv=0 gives dp_max=2^(+/-1600).

    Neither is a finite positive binary64 result (zero or overflow).
    These synthetic probes must fail rather than return a usable capacity.
    """
    with pytest.raises(ValueError, match="choked_pressure_drop"):
        liquid_capacity(
            coefficient=1.0, p_upstream=1.0, p_downstream=0.0,
            specific_gravity=1.0, unit_constant=1.0, piping_factor=1.0,
            recovery_factor=2.0**recovery_exp, critical_pressure_ratio=1.0,
            vapor_pressure=0.0,
        )


@pytest.mark.parametrize("critical_pressure_ratio", [2.0, 3.0, 1e308])
def test_nonpositive_or_nonfinite_pressure_head_is_rejected(critical_pressure_ratio):
    """Hand calculation: supplied p1=100, pv=50 gives head=100-FF*50.

    FF=2 gives zero, FF=3 gives -50, and FF=10^308 overflows the
    product. None has a positive finite head for the cited differential.
    """
    with pytest.raises(ValueError, match="choked_pressure_drop"):
        liquid_capacity(
            coefficient=1.0, p_upstream=100.0, p_downstream=0.0,
            specific_gravity=1.0, unit_constant=1.0, piping_factor=1.0,
            recovery_factor=1.0, critical_pressure_ratio=critical_pressure_ratio,
            vapor_pressure=50.0,
        )
