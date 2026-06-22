"""Oracle tests for DNV-OS-F201 LRFD/WSD combined-loading limit-state checks.

Oracle values reproduce a corpus WSD-vs-LRFD DNV-OS-F201 comparison spreadsheet
(generic pipe; no client/project identifiers). Working unit set is imperial
(ksi / kips-ft / kips) as used in the source sheet; checks at rtol ~1e-3.

Reference load case (internal-overpressure sheet, governing column):
    D = 20 in, t = 1.063 in, tcorr = 0.23622 in -> t2(operation) = 0.82678 in
    SMYS = 65 ksi (X65), SMTS = 77 ksi, alpha_u = 0.96
    Design pressure pi = 3 ksi, pe = 0, no temperature de-rating
    Safety class High: gamma_SC = 1.26, gamma_m = 1.0 (ULS), eta = 0.75
    Factored design Md = -1252.8 kips-ft, Td = 0 kips
"""

import math

import pytest

from digitalmodel.code_checks.dnv_os_f201_lrfd_wsd import (
    bursting_resistance,
    combined_loading_external_overpressure,
    combined_loading_internal_overpressure,
    factored_load_effect,
    flow_stress_coefficient,
    plastic_axial_resistance,
    plastic_moment_resistance,
    tensile_strength,
    yield_strength,
)

RTOL = 1e-3

# --- shared reference inputs (internal-overpressure sheet) -------------------
D = 20.0  # in
T2 = 0.82678  # in (t2 operation)
SMYS = 65.0  # ksi (X65)
SMTS = 77.0  # ksi
ALPHA_U = 0.96
PI = 3.0  # ksi internal design pressure
PE = 0.0  # ksi external
GAMMA_SC = 1.26  # High
GAMMA_M = 1.0  # ULS
ETA = 0.75  # WSD usage factor, High
MD = -1252.8  # kips-ft factored design moment
TD = 0.0  # kips factored design tension


def test_yield_and_tensile_strength_oracle():
    # fy = (65 - 0) * 0.96 = 62.4 ksi (R118); fu = (77 - 0)*0.96 = 73.92 ksi (R120)
    assert yield_strength(SMYS, 0.0, ALPHA_U) == pytest.approx(62.4, rel=RTOL)
    assert tensile_strength(SMTS, 0.0, ALPHA_U) == pytest.approx(73.92, rel=RTOL)


def test_bursting_resistance_oracle():
    fy = yield_strength(SMYS, 0.0, ALPHA_U)
    fu = tensile_strength(SMTS, 0.0, ALPHA_U)
    # Pb = 6.214109 ksi (R129)
    assert bursting_resistance(D, T2, fy, fu) == pytest.approx(6.214109, rel=RTOL)


def test_flow_stress_coefficient_oracle():
    fy = yield_strength(SMYS, 0.0, ALPHA_U)
    fu = tensile_strength(SMTS, 0.0, ALPHA_U)
    pb = bursting_resistance(D, T2, fy, fu)
    # alpha_c = 1.140662 (R155)
    alpha_c = flow_stress_coefficient(D, T2, fy, fu, PI, PE, pb)
    assert alpha_c == pytest.approx(1.140662, rel=RTOL)


def _resistances():
    fy = yield_strength(SMYS, 0.0, ALPHA_U)
    fu = tensile_strength(SMTS, 0.0, ALPHA_U)
    pb = bursting_resistance(D, T2, fy, fu)
    alpha_c = flow_stress_coefficient(D, T2, fy, fu, PI, PE, pb)
    mk_in = plastic_moment_resistance(D, T2, fy, alpha_c)  # inch-kips
    mk_ft = mk_in / 12.0  # ft-kips (sheet expresses Md in kips-ft)
    tk = plastic_axial_resistance(D, T2, fy, alpha_c)  # kips
    return fy, fu, pb, alpha_c, mk_ft, tk


def test_plastic_resistances_oracle():
    _, _, _, _, mk_ft, tk = _resistances()
    # Mk = 1802.77008 ft-kips (R111); Tk = 3544.674857 kips (R114)
    assert mk_ft == pytest.approx(1802.77008, rel=RTOL)
    assert tk == pytest.approx(3544.674857, rel=RTOL)


def test_factored_load_effect_oracle():
    # Md = 1.1*(-522) + 1.3*(-522) + 1.0*0 = -1252.8 kips-ft (R158, ULS factors)
    md = factored_load_effect(-522.0, -522.0, 0.0, 1.1, 1.3, 1.0)
    assert md == pytest.approx(-1252.8, rel=RTOL)


def test_combined_internal_lrfd_oracle():
    _, _, pb, _, mk_ft, tk = _resistances()
    res = combined_loading_internal_overpressure(
        design_moment=MD,
        moment_resistance=mk_ft,
        design_tension=TD,
        axial_resistance=tk,
        p_internal=PI,
        p_external=PE,
        bursting_resistance_value=pb,
        gamma_sc=GAMMA_SC,
        gamma_m=GAMMA_M,
    )
    # LRFD utilisation = 0.999883 (R171), limit 1.0 -> ACCEPTED
    assert res.utilisation == pytest.approx(0.999883, rel=RTOL)
    assert res.limit == 1.0
    assert res.passes is True


def test_combined_internal_wsd_oracle():
    _, _, pb, _, mk_ft, tk = _resistances()
    # WSD uses unfactored load effects: M = -522 + -522 = -1044 kips-ft (R163)
    m_wsd = factored_load_effect(-522.0, -522.0, 0.0, 1.0, 1.0, 1.0)
    res = combined_loading_internal_overpressure(
        design_moment=m_wsd,
        moment_resistance=mk_ft,
        design_tension=0.0,
        axial_resistance=tk,
        p_internal=PI,
        p_external=PE,
        bursting_resistance_value=pb,
        gamma_sc=1.0,
        gamma_m=1.0,
        usage_factor=ETA,
    )
    # WSD utilisation = 0.740221 (R179), limit eta^2 = 0.5625 -> NOT ACCEPTED
    assert res.utilisation == pytest.approx(0.740221, rel=RTOL)
    assert res.limit == pytest.approx(0.5625, rel=RTOL)
    assert res.passes is False


def test_combined_external_lrfd_oracle():
    # External-overpressure sheet governing column oracle values:
    #   Md = -1207.908894 kips-ft, Mk = 1759.625046 ft-kips, Tk = 3459.841457 kips
    #   pe = 1 ksi, pmin = 0, Pc (collapse) = 2.507218511 ksi
    #   gamma_SC = 1.26, gamma_m = 1.0
    res = combined_loading_external_overpressure(
        design_moment=-1207.908894,
        moment_resistance=1759.625046,
        design_tension=0.0,
        axial_resistance=3459.841457,
        p_external=1.0,
        p_min_internal=0.0,
        collapse_resistance=2.507218511,
        gamma_sc=1.26,
        gamma_m=1.0,
    )
    # External LRFD utilisation = 1.000672 (R169), limit 1.0 -> just NOT ACCEPTED
    assert res.utilisation == pytest.approx(1.000672, rel=RTOL)
    assert res.passes is False


def test_wsd_unfactored_equals_lrfd_without_factors():
    # Sanity: WSD format with gamma=1 and usage_factor None reduces to the
    # bracketed expression with limit 1.0.
    _, _, pb, _, mk_ft, tk = _resistances()
    res = combined_loading_internal_overpressure(
        design_moment=-1044.0,
        moment_resistance=mk_ft,
        design_tension=0.0,
        axial_resistance=tk,
        p_internal=PI,
        p_external=PE,
        bursting_resistance_value=pb,
        gamma_sc=1.0,
        gamma_m=1.0,
    )
    # No gamma, no eta -> 0.841652 (sheet R174 "NO Ysc & Ym" uses factored Md;
    # here unfactored Md gives the WSD bracket value 0.740221)
    assert res.utilisation == pytest.approx(0.740221, rel=RTOL)
    assert res.limit == 1.0
