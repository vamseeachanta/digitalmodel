# ABOUTME: Skeleton tests for structural fatigue sn_curves module
# ABOUTME: Tests PowerLawSNCurve, BilinearSNCurve, StandardSNCurves registry

import math
import pytest
import numpy as np


class TestPowerLawSNCurve:
    def _make(self, A=5.73e11, m=3.0, fatigue_limit=0.0):
        from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve
        return PowerLawSNCurve(name="test", A=A, m=m, fatigue_limit=fatigue_limit)

    def test_basic_creation(self):
        curve = self._make()
        assert curve.A == 5.73e11
        assert curve.m == 3.0

    def test_allowable_cycles_scalar(self):
        curve = self._make(A=5.73e11, m=3.0)
        N = curve.get_allowable_cycles(100.0)
        expected = 5.73e11 * (100.0 ** -3.0)
        assert N == pytest.approx(expected, rel=1e-4)

    def test_allowable_cycles_array(self):
        curve = self._make(A=5.73e11, m=3.0)
        stresses = np.array([50.0, 100.0, 200.0])
        N = curve.get_allowable_cycles(stresses)
        assert len(N) == 3
        assert N[0] > N[1] > N[2]  # higher stress -> fewer cycles

    def test_below_fatigue_limit_infinite_life(self):
        curve = self._make(A=5.73e11, m=3.0, fatigue_limit=52.63)
        N = curve.get_allowable_cycles(10.0)  # below limit
        assert N == math.inf

    def test_stress_range_from_cycles(self):
        curve = self._make(A=5.73e11, m=3.0)
        N_test = 1e6
        S = curve.get_stress_range(N_test)
        expected = (5.73e11 / N_test) ** (1.0 / 3.0)
        assert S == pytest.approx(expected, rel=1e-4)

    def test_round_trip_consistency(self):
        curve = self._make(A=5.73e11, m=3.0)
        S_original = 100.0
        N = curve.get_allowable_cycles(S_original)
        S_back = curve.get_stress_range(N)
        assert S_back == pytest.approx(S_original, rel=1e-3)

    def test_repr(self):
        curve = self._make()
        r = repr(curve)
        assert "PowerLawSNCurve" in r


class TestBilinearSNCurve:
    def _make(self):
        from digitalmodel.structural.fatigue.sn_curves import BilinearSNCurve
        return BilinearSNCurve(
            name="bilinear_test",
            A1=5.73e11, m1=3.0,
            A2=1.08e11, m2=5.0,
            transition_cycles=2e6,
            fatigue_limit=10.0,
        )

    def test_creation(self):
        curve = self._make()
        assert curve.name == "bilinear_test"
        assert curve.transition_cycles == 2e6

    def test_allowable_cycles_above_transition_stress(self):
        curve = self._make()
        # Above transition stress -> slope 1 (A1, m1)
        S = curve.transition_stress * 1.5
        N = curve.get_allowable_cycles(S)
        expected = curve.A1 * (S ** -curve.m1)
        assert float(N) == pytest.approx(expected, rel=1e-3)

    def test_below_fatigue_limit_infinite(self):
        curve = self._make()
        N = curve.get_allowable_cycles(5.0)  # below 10 MPa limit
        assert float(N) == math.inf


class TestStandardSNCurves:
    def test_get_dnv_d_curve(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        curve = StandardSNCurves.get_curve("DNV", "D")
        assert curve is not None
        assert curve.A > 0
        assert curve.m > 0

    def test_get_api_x_curve(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        curve = StandardSNCurves.get_curve("API", "X")
        assert curve is not None

    def test_get_bs_d_curve(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        curve = StandardSNCurves.get_curve("BS", "D")
        assert curve is not None

    def test_unknown_standard_raises(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        with pytest.raises(ValueError, match="Unknown standard"):
            StandardSNCurves.get_curve("UNKNOWN", "D")

    def test_unknown_curve_class_raises(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        with pytest.raises(ValueError, match="Unknown DNV curve class"):
            StandardSNCurves.get_curve("DNV", "Z")

    def test_dnv_d_curve_cycles_at_100mpa(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        curve = StandardSNCurves.get_curve("DNV", "D")
        N = curve.get_allowable_cycles(100.0)
        # N = 5.73e11 * 100^-3 = 5.73e5
        assert N == pytest.approx(5.73e11 * (100.0 ** -3.0), rel=1e-3)


class TestMaterialProperties:
    def test_creation(self):
        from digitalmodel.structural.fatigue.sn_curves import MaterialProperties
        mat = MaterialProperties(
            ultimate_strength=500.0,
            yield_strength=350.0,
        )
        assert mat.ultimate_strength == 500.0
        assert mat.elastic_modulus == pytest.approx(200000.0, rel=1e-6)
