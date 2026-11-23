# ABOUTME: TDD tests for Vertical Lift Performance (VLP) correlations
# ABOUTME: Covers the simplified traverse and deprecated published-name aliases

"""Tests for production_engineering.vlp_correlations module."""

import pytest

from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
    beggs_brill_pwf,
    hagedorn_brown_pwf,
    simplified_homogeneous_pwf,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def make_tubing(**overrides) -> TubingConfig:
    defaults = dict(depth_ft=6000.0, tubing_id_in=2.441, roughness_in=0.0006)
    defaults.update(overrides)
    return TubingConfig(**defaults)


def make_fluid(**overrides) -> FluidProperties:
    defaults = dict(oil_api=35.0, gas_gravity=0.65, temperature_f=150.0)
    defaults.update(overrides)
    return FluidProperties(**defaults)


def make_flow(**overrides) -> FlowConditions:
    defaults = dict(
        q_l_bopd=500.0,
        watercut=0.2,
        gor_scf_per_bbl=500.0,
    )
    defaults.update(overrides)
    return FlowConditions(**defaults)


# ---------------------------------------------------------------------------
# Simplified VLP physical trends
# ---------------------------------------------------------------------------

class TestSimplifiedHomogeneousVlp:

    def test_pwf_greater_than_whp(self):
        """Bottomhole pressure must exceed wellhead pressure."""
        tubing = make_tubing()
        fluid = make_fluid()
        flow = make_flow()
        pwf = simplified_homogeneous_pwf(flow, tubing, fluid, whp_psi=300.0)
        assert pwf > 300.0

    def test_deeper_well_gives_higher_pwf(self):
        """Deeper TVD → larger hydrostatic head → higher P_wf."""
        fluid = make_fluid()
        flow = make_flow()
        tubing_shallow = make_tubing(depth_ft=3000.0)
        tubing_deep = make_tubing(depth_ft=8000.0)
        pwf_shallow = simplified_homogeneous_pwf(flow, tubing_shallow, fluid, whp_psi=300.0)
        pwf_deep = simplified_homogeneous_pwf(flow, tubing_deep, fluid, whp_psi=300.0)
        assert pwf_deep > pwf_shallow

    def test_higher_gor_reduces_pwf(self):
        """Higher GOR → lighter fluid column → lower required P_wf."""
        tubing = make_tubing()
        fluid = make_fluid()
        flow_low_gor = make_flow(gor_scf_per_bbl=100.0)
        flow_high_gor = make_flow(gor_scf_per_bbl=2000.0)
        pwf_low = simplified_homogeneous_pwf(flow_low_gor, tubing, fluid, whp_psi=300.0)
        pwf_high = simplified_homogeneous_pwf(flow_high_gor, tubing, fluid, whp_psi=300.0)
        assert pwf_high < pwf_low

    def test_higher_watercut_increases_pwf(self):
        """Higher watercut → heavier fluid column → higher required P_wf."""
        tubing = make_tubing()
        fluid = make_fluid()
        flow_dry = make_flow(watercut=0.0)
        flow_wet = make_flow(watercut=0.9)
        pwf_dry = simplified_homogeneous_pwf(flow_dry, tubing, fluid, whp_psi=300.0)
        pwf_wet = simplified_homogeneous_pwf(flow_wet, tubing, fluid, whp_psi=300.0)
        assert pwf_wet > pwf_dry

    def test_higher_rate_increases_pwf_due_to_friction(self):
        """Higher flow rate → more friction → higher required P_wf."""
        tubing = make_tubing()
        fluid = make_fluid()
        flow_low = make_flow(q_l_bopd=100.0)
        flow_high = make_flow(q_l_bopd=2000.0)
        pwf_low = simplified_homogeneous_pwf(flow_low, tubing, fluid, whp_psi=300.0)
        pwf_high = simplified_homogeneous_pwf(flow_high, tubing, fluid, whp_psi=300.0)
        assert pwf_high > pwf_low

    def test_returns_positive_pressure(self):
        """P_wf must always be positive."""
        tubing = make_tubing()
        fluid = make_fluid()
        flow = make_flow()
        pwf = simplified_homogeneous_pwf(flow, tubing, fluid, whp_psi=100.0)
        assert pwf > 0.0

    def test_reasonable_pressure_range(self):
        """P_wf should be in realistic range for typical well (< 10,000 psi)."""
        tubing = make_tubing(depth_ft=6000.0)
        fluid = make_fluid()
        flow = make_flow(q_l_bopd=500.0, gor_scf_per_bbl=500.0, watercut=0.2)
        pwf = simplified_homogeneous_pwf(flow, tubing, fluid, whp_psi=300.0)
        assert 500.0 < pwf < 5000.0


# ---------------------------------------------------------------------------
# Deprecated simplified VLP aliases
# ---------------------------------------------------------------------------

class TestSimplifiedHomogeneousAliases:

    def test_both_aliases_equal_simplified_pressure(self):
        """Both deprecated names return exactly the same simplified pressure."""
        tubing, fluid, flow = make_tubing(), make_fluid(), make_flow()
        with pytest.warns(DeprecationWarning):
            pwf_hb = hagedorn_brown_pwf(flow, tubing, fluid, whp_psi=300.0)
        with pytest.warns(DeprecationWarning):
            pwf_bb = beggs_brill_pwf(flow, tubing, fluid, whp_psi=300.0)
        assert pwf_hb == pwf_bb == simplified_homogeneous_pwf(flow, tubing, fluid, 300.0)


# ---------------------------------------------------------------------------
# VLP curve generation (q vs Pwf)
# ---------------------------------------------------------------------------

class TestVlpCurve:

    def test_vlp_curve_is_monotonically_increasing_with_rate(self):
        """VLP curve: as rate increases, more friction → higher P_wf required."""
        from digitalmodel.production_engineering.vlp_correlations import vlp_curve
        tubing = make_tubing()
        fluid = make_fluid()
        rates = [100, 300, 500, 800, 1200]
        pwfs = vlp_curve(
            rates_bopd=rates, watercut=0.2, gor_scf_per_bbl=500.0,
            tubing=tubing, fluid=fluid, whp_psi=300.0
        )
        assert len(pwfs) == len(rates)
        assert all(pwfs[i] <= pwfs[i + 1] for i in range(len(pwfs) - 1))


@pytest.mark.parametrize("rate,watercut,gor", [
    (100.0, 0.0, 100.0),
    (500.0, 0.2, 500.0),
    (2000.0, 0.9, 2000.0),
])
def test_deprecated_aliases_warn_and_equal_simplified(rate, watercut, gor):
    flow = make_flow(q_l_bopd=rate, watercut=watercut, gor_scf_per_bbl=gor)
    tubing, fluid = make_tubing(), make_fluid()
    with pytest.warns(DeprecationWarning, match="does not implement.*simplified homogeneous.*same result"):
        hb = hagedorn_brown_pwf(flow, tubing, fluid, 300.0)
    with pytest.warns(DeprecationWarning, match="does not implement.*simplified homogeneous.*same result"):
        bb = beggs_brill_pwf(flow, tubing, fluid, 300.0)
    from digitalmodel.production_engineering.vlp_correlations import simplified_homogeneous_pwf
    assert hb == bb == simplified_homogeneous_pwf(flow, tubing, fluid, 300.0)


@pytest.mark.parametrize("name", ["hagedorn_brown", "beggs_brill"])
def test_curve_selectors_are_deprecated_aliases(name):
    from digitalmodel.production_engineering.vlp_correlations import vlp_curve
    args = dict(rates_bopd=[100.0, 500.0, 2000.0], watercut=0.2,
                gor_scf_per_bbl=500.0, tubing=make_tubing(),
                fluid=make_fluid(), whp_psi=300.0)
    with pytest.warns(DeprecationWarning, match="does not implement"):
        actual = vlp_curve(**args, correlation=name)
    assert actual == vlp_curve(**args, correlation="simplified_homogeneous")


@pytest.mark.parametrize("name", ["hagedorn_brown", "beggs_brill"])
def test_nodal_selectors_are_deprecated_aliases(name):
    from digitalmodel.production_engineering import NodalSolver, ReservoirConditions, VogelIpr
    ipr = VogelIpr(ReservoirConditions(3000.0, 2000.0, 1.0), qmax_bopd=3000.0)
    args = dict(ipr=ipr, tubing=make_tubing(), fluid=make_fluid(),
                watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0)
    with pytest.warns(DeprecationWarning, match="does not implement"):
        actual = NodalSolver().solve(**args, correlation=name)
    assert actual == NodalSolver().solve(**args, correlation="simplified_homogeneous")


def test_curve_rejects_unknown_correlation():
    from digitalmodel.production_engineering.vlp_correlations import vlp_curve

    with pytest.raises(
        ValueError, match="simplified_homogeneous.*hagedorn_brown.*beggs_brill"
    ):
        vlp_curve(
            rates_bopd=[100.0, 500.0, 2000.0], watercut=0.2,
            gor_scf_per_bbl=500.0, tubing=make_tubing(),
            fluid=make_fluid(), whp_psi=300.0, correlation="unknown_method",
        )


def test_nodal_rejects_unknown_correlation():
    from digitalmodel.production_engineering import NodalSolver, ReservoirConditions, VogelIpr

    ipr = VogelIpr(ReservoirConditions(3000.0, 2000.0, 1.0), qmax_bopd=3000.0)
    with pytest.raises(
        ValueError, match="simplified_homogeneous.*hagedorn_brown.*beggs_brill"
    ):
        NodalSolver().solve(
            ipr=ipr, tubing=make_tubing(), fluid=make_fluid(),
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0,
            correlation="unknown_method",
        )
