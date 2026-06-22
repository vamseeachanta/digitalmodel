"""Tests for DNV-RP-C203 closed-form S-N endurance / inverse.

Oracle values are the computed cells of a generic DNV-RP-C203 S-N
design spreadsheet (in-air "Data(air)" / seawater "Data(Sea water)"
tables and the curve-C Miner's-rule worksheet). All inputs are generic
standard quantities — no project- or client-specific data.

Method validated:
    forward:  log10(N) = log10(a) - m * log10(Δσ)
    inverse:  Δσ = 10 ** ((log10(a) - log10(N)) / m)
bi-linear about the knee N_D = 1e7.
"""

import numpy as np
import pytest

from digitalmodel.fatigue.sn_endurance import (
    endurance_cycles,
    stress_range_at_cycles,
    thickness_correction,
)

# --- Oracle: in-air stress range Δσ (MPa) at a given N (cycles) -------
# Source sheet "Data(air)": first segment (N <= 1e7) and second segment.
AIR_STRESS_AT_N = {
    # N : {curve: Δσ}
    1000.0: {
        "B1": 1069.6704538393997, "B2": 935.9442919496431,
        "C": 1575.1914046947927, "C1": 1411.4537979479337,
        "C2": 1259.8920436675346, "D": 1134.139996909264,
        "E": 1007.7048141361054, "F": 894.6778113577494,
        "F1": 793.718799182992, "F3": 705.7756434983849,
        "G": 629.9895321764905,
    },
    1.0e7: {  # knee point -> fatigue limit
        "B1": 106.97, "B2": 93.59, "C": 73.1, "C1": 65.5, "C2": 58.48,
        "D": 52.63, "E": 46.78, "F": 41.52, "F1": 36.84, "F3": 32.75,
        "G": 29.24,
    },
    1.0e12: {  # second segment (m2 = 5)
        "B1": 10.695473105661607, "B2": 9.358365248668006,
        "C": 7.311390542378577, "C1": 6.549376752971101,
        "C2": 5.847900373612855, "D": 5.262595094543776,
        "E": 4.677350851590015, "F": 4.151451219202678,
        "F1": 3.684681301929866, "F3": 3.2749141660714516,
        "G": 2.9241517930132117,
    },
}

# --- Oracle: seawater-with-CP stress range Δσ (MPa) at given N --------
# Source sheet "Data(Sea water)" first segment at N = 1000.
SEAWATER_STRESS_AT_N_1000 = {
    "B1": 953.344795478314, "B2": 834.1612282410433,
    "C": 1158.7773561551273, "C1": 1038.3250539880407,
    "C2": 926.8298233793498, "D": 834.3213041991821,
    "E": 741.3102413009176, "F": 658.1628021622323,
    "F1": 583.8930868379755, "F3": 519.1983855258562,
}


class TestInverseStressRangeAtCycles:
    @pytest.mark.parametrize("N,table", list(AIR_STRESS_AT_N.items()))
    def test_air_stress_range(self, N, table):
        for curve, expected in table.items():
            got = stress_range_at_cycles(N, curve, environment="air")
            np.testing.assert_allclose(got, expected, rtol=1e-3)

    @pytest.mark.parametrize("curve,expected", list(SEAWATER_STRESS_AT_N_1000.items()))
    def test_seawater_stress_range_at_1000(self, curve, expected):
        got = stress_range_at_cycles(1000.0, curve, environment="seawater_cp")
        np.testing.assert_allclose(got, expected, rtol=1e-3)

    def test_vectorised(self):
        cycles = np.array([1000.0, 1.0e7, 1.0e12])
        got = stress_range_at_cycles(cycles, "C", environment="air")
        expected = [1575.1914046947927, 73.1, 7.311390542378577]
        np.testing.assert_allclose(got, expected, rtol=1e-3)


class TestForwardEnduranceCycles:
    def test_air_roundtrip_first_segment(self):
        # Δσ for curve C at N=1000 must map back to ~1000 cycles.
        s = AIR_STRESS_AT_N[1000.0]["C"]
        assert endurance_cycles(s, "C", "air") == pytest.approx(1000.0, rel=1e-3)

    def test_air_roundtrip_second_segment(self):
        # Below the fatigue limit -> second (m2) segment.
        s = AIR_STRESS_AT_N[1.0e12]["D"]
        assert endurance_cycles(s, "D", "air") == pytest.approx(1.0e12, rel=1e-2)

    def test_known_curve_c_failure_cycles(self):
        # Curve-C Miner's worksheet: Δσ ~339.36 MPa -> N ~ 1e5 cycles.
        assert endurance_cycles(339.3647005714516, "C", "air") == pytest.approx(
            1.0e5, rel=1e-3
        )

    def test_zero_stress_infinite_life(self):
        assert endurance_cycles(0.0, "F", "air") == np.inf


class TestThicknessCorrectionReuse:
    """The DNV thickness rule is reused from fatigue.damage.thickness_correction."""

    def test_above_ref(self):
        # Generic: F detail, t=30 mm, t_ref=25 mm, k=0.25.
        s = np.array([100.0])
        got = thickness_correction(s, t_actual=30.0, t_ref=25.0, k=0.25)
        expected = 100.0 * (30.0 / 25.0) ** 0.25
        np.testing.assert_allclose(got, [expected], rtol=1e-10)

    def test_below_ref_no_change(self):
        s = np.array([100.0, 80.0])
        got = thickness_correction(s, t_actual=20.0, t_ref=25.0, k=0.25)
        np.testing.assert_array_equal(got, s)
