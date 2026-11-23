"""Tests for DNV-RP-C203 S-N curve library."""

import math
import numpy as np
import pandas as pd
import pytest

from digitalmodel.fatigue.sn_curves import get_sn_curve, DNV_CURVES


class TestDNVCurves:
    """Verify the DNV_CURVES dictionary."""

    def test_all_14_curves_present(self):
        expected = {
            "B1", "B2", "C", "C1", "C2", "D", "E", "F",
            "F1", "F3", "G", "W1", "W2", "W3",
        }
        assert set(DNV_CURVES.keys()) == expected

    def test_curve_has_required_keys(self):
        for name, params in DNV_CURVES.items():
            for key in ("k_1", "k_2", "log_a1", "log_a2", "ND", "SD"):
                assert key in params, f"{name} missing key {key}"

    def test_f_curve_sd_matches_known_value(self):
        """DNV F curve: SD = 10^((11.855 - 7) / 3) ~ 41.52 MPa at 1e7 cycles."""
        sd = DNV_CURVES["F"]["SD"]
        expected = 10 ** ((11.855 - 7) / 3)  # ~41.52
        assert abs(sd - expected) < 0.5, f"F curve SD={sd}, expected ~{expected:.2f}"

    def test_nd_is_1e7_for_all(self):
        for name, params in DNV_CURVES.items():
            assert params["ND"] == 1e7


class TestGetSNCurve:
    """Verify WoehlerCurve construction and cycle calculations."""

    def test_f_curve_at_100mpa(self):
        """DNV F at 100 MPa: N = 10^(11.855 - 3*2) = 10^5.855 ~ 716,143."""
        wc = get_sn_curve("F")
        cycles = wc.cycles(np.array([100.0]))
        expected = 10 ** (11.855 - 3.0 * math.log10(100))
        assert abs(cycles[0] - expected) / expected < 0.01, f"Got {cycles[0]:.0f}"

    def test_f_curve_slopes(self):
        wc = get_sn_curve("F")
        assert wc.k_1 == 3.0
        assert wc.k_2 == 5.0

    def test_d_curve_at_100mpa(self):
        """DNV D at 100 MPa: N = 10^(12.164 - 3*2) = 10^6.164 ~ 1.46e6."""
        wc = get_sn_curve("D")
        cycles = wc.cycles(np.array([100.0]))
        expected = 10 ** (12.164 - 3.0 * math.log10(100))
        assert abs(cycles[0] - expected) / expected < 0.01

    def test_case_insensitive_name(self):
        wc = get_sn_curve("f")
        assert wc.k_1 == 3.0

    def test_invalid_name_raises(self):
        with pytest.raises(ValueError, match="Unknown curve"):
            get_sn_curve("Z99")

    def test_invalid_environment_raises(self):
        with pytest.raises(ValueError, match="Unknown environment"):
            get_sn_curve("F", environment="space")


class TestEnvironments:
    """Verify environment adjustments."""

    def test_seawater_cp_lower_cycles(self):
        """Seawater CP should give fewer allowable cycles than air."""
        wc_air = get_sn_curve("F", "air")
        wc_cp = get_sn_curve("F", "seawater_cp")
        s = np.array([100.0])
        assert wc_cp.cycles(s)[0] < wc_air.cycles(s)[0]

    def test_free_corrosion_single_slope(self):
        """Free corrosion: k_2 should equal k_1 (no endurance limit)."""
        wc = get_sn_curve("F", "free_corrosion")
        assert wc.k_2 == wc.k_1

    def test_free_corrosion_lower_than_cp(self):
        """Free corrosion should be same or more severe than CP at low stress."""
        wc_cp = get_sn_curve("F", "seawater_cp")
        wc_fc = get_sn_curve("F", "free_corrosion")
        # At low stress (below knee), free corrosion is more conservative
        # because single slope (k_1=3) vs bi-linear (k_2=5)
        s_low = np.array([30.0])
        assert wc_fc.cycles(s_low)[0] < wc_cp.cycles(s_low)[0]
