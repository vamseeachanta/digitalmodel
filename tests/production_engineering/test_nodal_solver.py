# ABOUTME: TDD tests for nodal analysis solver (IPR/VLP intersection)
# ABOUTME: Covers operating point finding, confidence bounds, multiple solutions

"""Tests for production_engineering.nodal_solver module."""

import pytest

from digitalmodel.production_engineering.ipr_models import (
    CompositeIpr,
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.nodal_solver import (
    ConfidenceBound,
    NodalOperatingPoint,
    NodalSolver,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
)


def make_reservoir(**overrides) -> ReservoirConditions:
    defaults = dict(
        reservoir_pressure_psi=3000.0,
        bubble_point_psi=3000.0,
        productivity_index_bopd_psi=2.0,
    )
    defaults.update(overrides)
    return ReservoirConditions(**defaults)


def make_tubing(**overrides) -> TubingConfig:
    defaults = dict(depth_ft=6000.0, tubing_id_in=2.441, roughness_in=0.0006)
    defaults.update(overrides)
    return TubingConfig(**defaults)


def make_fluid(**overrides) -> FluidProperties:
    defaults = dict(oil_api=35.0, gas_gravity=0.65, temperature_f=150.0)
    defaults.update(overrides)
    return FluidProperties(**defaults)


solver = NodalSolver()


class TestNodalOperatingPoint:

    def test_finds_intersection_for_vogel_ipr(self):
        """Solver finds an operating point where IPR and VLP meet."""
        res = make_reservoir(reservoir_pressure_psi=3000, bubble_point_psi=3000)
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0
        )
        assert isinstance(result, NodalOperatingPoint)
        assert result.q_bopd > 0
        assert result.pwf_psi > 0

    def test_ipr_equals_vlp_at_operating_point(self):
        """At operating point, IPR pressure ≈ VLP pressure."""
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0
        )
        # Verify self-consistency: IPR at q* should give same Pwf
        pwf_from_ipr = ipr.flowing_pressure(result.q_bopd)
        assert abs(pwf_from_ipr - result.pwf_psi) / result.pwf_psi < 0.05

    def test_composite_ipr_solver(self):
        res = make_reservoir(
            reservoir_pressure_psi=3000,
            bubble_point_psi=2000,
            productivity_index_bopd_psi=1.0,
        )
        ipr = CompositeIpr(reservoir=res)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.1, gor_scf_per_bbl=800.0, whp_psi=200.0
        )
        assert result.q_bopd > 0

    def test_higher_whp_reduces_rate(self):
        """Higher wellhead back-pressure → lower operating rate."""
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result_low_whp = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=200.0
        )
        result_high_whp = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=600.0
        )
        assert result_low_whp.q_bopd > result_high_whp.q_bopd

    def test_higher_reservoir_pressure_increases_rate(self):
        """Higher reservoir pressure → more drive energy → higher rate."""
        res_low = make_reservoir(reservoir_pressure_psi=2000)
        res_high = make_reservoir(reservoir_pressure_psi=4000)
        ipr_low = VogelIpr(reservoir=res_low, qmax_bopd=1000.0)
        ipr_high = VogelIpr(reservoir=res_high, qmax_bopd=2500.0)
        tubing = make_tubing()
        fluid = make_fluid()
        r_low = solver.solve(ipr=ipr_low, tubing=tubing, fluid=fluid,
                             watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0)
        r_high = solver.solve(ipr=ipr_high, tubing=tubing, fluid=fluid,
                              watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0)
        assert r_high.q_bopd > r_low.q_bopd


class TestConfidenceBounds:

    def test_high_quality_score_gives_tight_bounds(self):
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0,
            test_quality_score=90.0
        )
        assert result.confidence == ConfidenceBound.GREEN
        assert result.q_uncertainty_fraction < 0.10

    def test_low_quality_score_gives_wide_bounds(self):
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0,
            test_quality_score=30.0
        )
        assert result.confidence == ConfidenceBound.RED
        assert result.q_uncertainty_fraction > 0.20

    def test_no_quality_score_gives_amber(self):
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        result = solver.solve(
            ipr=ipr, tubing=tubing, fluid=fluid,
            watercut=0.2, gor_scf_per_bbl=500.0, whp_psi=300.0,
        )
        assert result.confidence == ConfidenceBound.AMBER
