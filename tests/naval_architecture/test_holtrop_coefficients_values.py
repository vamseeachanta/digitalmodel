# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent scalar examples, without the unqualified resistance fixtures.

Source H82: *An approximate power prediction method*, ISP 29 (1982),
pp. 166-168, public OpenHull reproduction, revision
6e4115f1dac86008d3bc58dcf42b4bb7dbec805c, ``src/openhull/holtrop.py``.
Source locators and qualification limits are in
``docs/domains/ship-design/naval-architecture-value-examples.md``.
All dimensions and coefficient inputs below are synthetic arithmetic examples.
Expected literals are reduced by hand in the docstrings; no production helper
computes an expected value. Relative tolerance 1e-9 allows rounding of the
transcribed decimal powers/exponentials, rather than model uncertainty.
The 1984 revision ranges remain NOT_EVALUATED: its paper was not accessible.
"""

import pytest

from digitalmodel.naval_architecture import holtrop_coefficients as hc


@pytest.mark.parametrize("lcb, expected", [(0.0, 40.0), (2.0, 45.142857142857)])
def test_length_of_run_values(lcb, expected):
    """H82 p.166: LR=L(1-Cp+0.06 Cp lcb/(4 Cp-1)).

    L=100, Cp=0.6: LR=40 for lcb=0; at lcb=2 the increment is
    100*0.072/1.4=5.142857142857 m.
    """
    assert hc.length_of_run(100.0, 0.6, lcb) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize(
    "ratio, expected",
    [
        (0.064, 0.091831641440964),
        (0.11, 0.11),
        (0.16, 0.16),
        (0.25, 0.25),
        (0.5, 0.375),
    ],
)
def test_coeff_c7_regime_values(ratio, expected):
    """H82 p.167: c7=0.229577 r^0.33333, r, or 0.5-0.0625/r.

    The three regimes are r<0.11, 0.11<=r<=0.25, r>0.25.
    For r=0.064 the rounded exponent gives 0.091831641440964;
    At the regime boundaries, r=.11 and .25 both give r.
    r=0.16 gives 0.16; r=0.5 gives 0.5-0.125=0.375.
    """
    assert hc.coeff_c7(ratio) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize(
    "ratio, expected",
    [
        (0.01, 0.479948),
        (0.02, 0.479948),
        (0.03, 0.483313480187652),
        pytest.param(
            0.08,
            0.569586289571160,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="c12 upper regime returns a constant",
            ),
        ),
    ],
)
def test_coeff_c12_regime_values(ratio, expected):
    """H82 p.166: c12=0.479948 below .02; middle branch
    48.2(r-.02)^2.078+.479948; above .05, r^0.2228446.

    At .02 the middle branch adds zero, giving .479948.
    At .03: 48.2*.01^2.078+.479948=.483313480187652.
    At .08: exp(.2228446*ln(.08))=.569586289571160.
    The upper branch is a strict expected failure; no code is changed.
    """
    assert hc.coeff_c12(ratio) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize("stern, expected", [(-10, 0.97), (0, 1.0), (10, 1.03)])
def test_coeff_c13_values(stern, expected):
    """H82 p.166: c13=1+.003*Cstern; 1-.03, 1, and 1+.03."""
    assert hc.coeff_c13(stern) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize("c3, expected", [(0.0, 1.0), (0.25, 0.388679570901753)])
def test_coeff_c2_values(c3, expected):
    """H82 p.167: c2=exp(-1.89 sqrt(c3)); exp(0)=1;
    at c3=.25, sqrt=.5 and exp(-.945)=.388679570901753.
    """
    assert hc.coeff_c2(c3) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize("area, expected", [(0.0, 0.0), (4.0, 0.085496183206107)])
def test_coeff_c3_values(area, expected):
    """H82 pp.167-168: c3=.56 Abt^1.5/[B T(.31 sqrt(Abt)+Tf-hb)].

    Set hb=0 (the API has no bulb-height argument), B=10, T=Tf=2.
    Abt=4 gives .56*8/[20*(.62+2)]=4.48/52.4=.085496183206107.
    Zero bulb area gives zero exactly.
    """
    assert hc.coeff_c3(area, 10.0, 2.0, 2.0) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize("ratio, expected", [(0.02, 0.02), (0.04, 0.04), (0.08, 0.04)])
def test_coeff_c4_values(ratio, expected):
    """H82 p.168: c4=min(Tf/L,.04); below, at, and above the cap."""
    assert hc.coeff_c4(ratio) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize("area, expected", [(0.0, 1.0), (2.0, 0.9)])
def test_coeff_c5_arithmetic_values(area, expected):
    """H82 p.167: c5=1-.8 At/(B T Cm).

    B=10,T=2,Cm=.8: At=2 gives 1-1.6/16=.9; At=0 gives 1.
    The API argument is named cwp, but the source denominator uses Cm.
    This tests scalar arithmetic only; physical parameter mapping is unqualified.
    """
    assert hc.coeff_c5(area, 10.0, 2.0, 0.8) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize(
    "cp, expected", [(0.5, 1.4461285), (0.8, 1.16478), (0.9, 1.09411)]
)
def test_coeff_c16_regime_values(cp, expected):
    """H82 p.167: below .8 use 8.07981 Cp-13.8673 Cp^2+6.984388 Cp^3;
    otherwise 1.73014-.7067 Cp.

    Cp=.5: 4.039905-3.466825+.8730485=1.4461285.
    Cp=.8: 1.73014-.56536=1.16478; Cp=.9: 1.73014-.63603=1.09411.
    """
    assert hc.coeff_c16(cp) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize(
    "ratio, expected",
    [
        (125.0, -1.69385),
        pytest.param(
            1000.0,
            -0.846392372881356,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="c15 interpolation omits cube root",
            ),
        ),
        (2000.0, 0.0),
    ],
)
def test_coeff_c15_regime_values(ratio, expected):
    """H82 p.167 reproduction: with d=L/V^(1/3), c15=-1.69385 for d<8,
    -1.69385+(d-8)/2.36 for 8<=d<=12, and zero for d>12.

    Ratios L^3/V=125,1000,2000 exercise all branches. At 1000, d=10,
    hence -1.69385+2/2.36=-.846392372881356. The source rounds the upper
    transition differently from the API; the exact 1984 boundary is NOT_EVALUATED.
    """
    assert hc.coeff_c15(ratio) == pytest.approx(expected, rel=1e-9)


def test_exponent_m1_value():
    """H82 p.167: m1=.0140407 L/T-1.75254 V^(1/3)/L-4.79323 B/L-c16.

    L=100,T=4,B=10,V=1000,Cp=.5 (c16=1.4461285 by the polynomial above):
    .3510175-.175254-.479323-1.4461285=-1.749688.
    """
    assert hc.exponent_m1(100.0, 4.0, 10.0, 1000.0, 0.5) == pytest.approx(
        -1.749688, rel=1e-9
    )


@pytest.mark.parametrize(
    "length, expected", [(100.0, 0.5676), (120.0, 0.5076), (140.0, 0.5076)]
)
def test_lambda_wave_regime_values(length, expected):
    """H82 p.167: lambda=1.446 Cp-.03 L/B below L/B=12, else 1.446 Cp-.36.

    B=10,Cp=.6: 1.446*.6=.8676; subtract .30 or .36.
    """
    assert hc.lambda_wave(length, 10.0, 0.6) == pytest.approx(expected, rel=1e-9)


@pytest.mark.parametrize(
    "angle",
    [
        pytest.param(22.41184830687802, id="physical-draft-5"),
        pytest.param(
            27.09188726102078,
            id="physical-draft-2",
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="entrance regression uses Cb/L instead of V/L^3",
            ),
        ),
    ],
)
def test_half_angle_of_entrance_value(angle):
    """H82 p.167: iE=1+89 exp[-(L/B)^.80856 (1-Cwp)^.30484
    (1-Cp-.0225 lcb)^.6367 (LR/B)^.34574 (100 V/L^3)^.16302].

    L=100,B=20,Cwp=.8,Cp=.6,lcb=0,LR=40,Cb=.5.
    The five factors reduce to 5^.80856, .2^.30484, .4^.6367,
    2^.34574, and .5^.16302 (T=5) or .2^.16302 (T=2).
    Substitution gives the two independent angle literals above.
    The API cannot receive draft, so it cannot distinguish these geometries.
    """
    assert hc.half_angle_of_entrance(
        100.0, 20.0, 0.8, 0.6, 0.0, 40.0, 0.5
    ) == pytest.approx(angle, rel=1e-9)


@pytest.mark.parametrize(
    "draft, expected",
    [
        (5.0, 3.414422313533587),
        pytest.param(
            2.0,
            1.401418547579026,
            marks=pytest.mark.xfail(
                strict=True,
                raises=AssertionError,
                reason="c1 inherits entrance regression discrepancy",
            ),
        ),
    ],
)
def test_coeff_c1_values(draft, expected):
    """H82 p.167: c1=2223105 c7^3.78613 (T/B)^1.07961 (90-iE)^-1.37565.

    For the entrance example, c7=.2. At T=5: T/B=.25 and
    90-iE=67.58815169312198, giving 3.414422313533587.
    At T=2: T/B=.1 and 90-iE=62.90811273897922, giving 1.401418547579026.
    """
    assert hc.coeff_c1(100.0, 20.0, draft, 0.5, 0.8, 0.6, 0.0, 40.0) == pytest.approx(
        expected, rel=1e-9
    )
