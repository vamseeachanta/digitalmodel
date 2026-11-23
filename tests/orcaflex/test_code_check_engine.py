"""Tests for digitalmodel.orcaflex.code_check_engine module."""

import math

import numpy as np
import pytest

from digitalmodel.orcaflex.code_check_engine import (
    APIRP2RDInput,
    APIRP2SKInput,
    DNVOSF201Input,
    LoadCondition,
    MooringCodeCheckResult,
    SafetyClass,
    UtilisationEnvelope,
    check_api_rp_2rd,
    check_dnv_os_f201,
    check_mooring_api_2sk,
)


class TestAPIRP2RD:
    """Tests for API RP 2RD riser code check."""

    def test_low_load_passes(self):
        """Low tension and bending should give utilisation < 1."""
        pipe = APIRP2RDInput()
        arc = np.array([0.0, 100.0, 200.0])
        tensions = np.array([100.0, 150.0, 200.0])
        moments = np.array([10.0, 20.0, 30.0])
        results = check_api_rp_2rd(pipe, arc, tensions, moments)
        assert len(results) == 3
        for r in results:
            assert r.utilisation < 1.0

    def test_high_load_fails(self):
        """Very high tension should give utilisation > 1."""
        pipe = APIRP2RDInput(wall_thickness=0.015)  # thin wall
        arc = np.array([0.0])
        tensions = np.array([50000.0])  # very high
        moments = np.array([5000.0])
        results = check_api_rp_2rd(pipe, arc, tensions, moments)
        assert results[0].utilisation > 1.0
        assert results[0].model_dump(by_alias=True)["pass"] is False

    def test_utilisation_increases_with_load(self):
        """Higher loads should give higher utilisation."""
        pipe = APIRP2RDInput()
        arc = np.array([0.0, 0.0])
        t_low = np.array([100.0, 500.0])
        m = np.array([10.0, 10.0])
        results = check_api_rp_2rd(pipe, arc, t_low, m)
        assert results[1].utilisation > results[0].utilisation

    def test_design_factor_effect(self):
        """Lower design factor should give higher utilisation."""
        pipe_strict = APIRP2RDInput(design_factor=0.5)
        pipe_lenient = APIRP2RDInput(design_factor=0.8)
        arc = np.array([0.0])
        t = np.array([500.0])
        m = np.array([50.0])
        r_strict = check_api_rp_2rd(pipe_strict, arc, t, m)
        r_lenient = check_api_rp_2rd(pipe_lenient, arc, t, m)
        assert r_strict[0].utilisation > r_lenient[0].utilisation


class TestDNVOSF201:
    """Tests for DNV-OS-F201 combined loading check."""

    def test_low_load_passes(self):
        """Low combined loading should pass."""
        pipe = DNVOSF201Input()
        arc = np.array([0.0, 50.0, 100.0])
        t = np.array([100.0, 200.0, 300.0])
        m = np.array([10.0, 20.0, 30.0])
        results = check_dnv_os_f201(pipe, arc, t, m)
        assert len(results) == 3
        for r in results:
            assert r.utilisation < 1.0

    def test_safety_class_factors(self):
        """Higher safety class should give stricter check."""
        pipe_low = DNVOSF201Input(safety_class=SafetyClass.LOW)
        pipe_high = DNVOSF201Input(safety_class=SafetyClass.HIGH)
        assert pipe_high.safety_class_factor() > pipe_low.safety_class_factor()

    def test_high_load_fails(self):
        """Extreme loading should fail."""
        pipe = DNVOSF201Input(wall_thickness=0.012)  # thin
        arc = np.array([0.0])
        t = np.array([20000.0])
        m = np.array([3000.0])
        results = check_dnv_os_f201(pipe, arc, t, m)
        assert results[0].utilisation > 1.0


class TestAPIRP2SK:
    """Tests for API RP 2SK mooring safety factor check."""

    def test_intact_safety_factor(self):
        """Intact condition should require SF = 1.67."""
        inp = APIRP2SKInput(mbl_kN=7000.0, condition=LoadCondition.EXTREME)
        assert inp.required_safety_factor() == pytest.approx(1.67)

    def test_damaged_safety_factor(self):
        """Damaged condition should require SF = 1.25."""
        inp = APIRP2SKInput(condition=LoadCondition.ACCIDENTAL)
        assert inp.required_safety_factor() == pytest.approx(1.25)

    def test_mooring_check_passes(self):
        """Low tension relative to MBL should pass."""
        result = check_mooring_api_2sk(
            line_name="ML1",
            max_tension_kN=3000.0,
            mbl_kN=7000.0,
            condition=LoadCondition.EXTREME,
        )
        assert result.model_dump(by_alias=True)["pass"] is True
        assert result.safety_factor > 1.67

    def test_mooring_check_fails(self):
        """High tension should fail."""
        result = check_mooring_api_2sk(
            line_name="ML1",
            max_tension_kN=6000.0,
            mbl_kN=7000.0,
            condition=LoadCondition.EXTREME,
        )
        assert result.model_dump(by_alias=True)["pass"] is False

    def test_utilisation_format(self):
        """Utilisation should be ratio of max tension to allowable."""
        result = check_mooring_api_2sk("ML1", 4000.0, 7000.0)
        assert 0 < result.utilisation < 2.0


class TestUtilisationEnvelope:
    """Tests for utilisation envelope."""

    def test_max_utilisation(self):
        """Max utilisation should be the highest value."""
        env = UtilisationEnvelope(
            arc_lengths=[0, 50, 100],
            utilisations=[0.5, 0.9, 0.7],
        )
        assert env.max_utilisation == pytest.approx(0.9)
        assert env.max_utilisation_location == pytest.approx(50.0)

    def test_passes(self):
        """Should pass if all utilisations below limit."""
        env = UtilisationEnvelope(utilisations=[0.5, 0.7, 0.9])
        assert env.passes(limit=1.0) is True
        assert env.passes(limit=0.8) is False

    def test_to_dict(self):
        """Dict export should contain required keys."""
        env = UtilisationEnvelope(
            arc_lengths=[0, 100],
            utilisations=[0.5, 0.8],
            code_standard="API_RP_2RD",
        )
        d = env.to_dict()
        assert d["code_standard"] == "API_RP_2RD"
        assert d["overall_pass"] is True
