"""Doc-verified tests for drilling riser stackup calculations.

References:
- 2H-TNE-0050-03 §3.1 — Tension Requirements
- API SPEC 16F §14.2.2 — Strength Analysis
- Barlow's formula for pipe wall thickness
"""

import pytest

from digitalmodel.drilling_riser.stackup import (
    effective_tension,
    minimum_slip_ring_tension,
    top_tension_required,
    wall_thickness_required,
)


class TestTopTensionRequired:
    """Per 2H-TNE-0050-03 §3.1: top tension = submerged_weight * safety_factor."""

    def test_basic_top_tension(self):
        """500 kN submerged weight with default factor 1.25 -> 625 kN."""
        result = top_tension_required(submerged_weight_kn=500.0)
        assert result == pytest.approx(625.0, rel=0.01)

    def test_top_tension_monotonic_with_weight(self):
        """More submerged weight requires more top tension."""
        t1 = top_tension_required(submerged_weight_kn=400.0)
        t2 = top_tension_required(submerged_weight_kn=600.0)
        assert t2 > t1

    def test_top_tension_never_less_than_submerged_weight(self):
        """Top tension must always exceed submerged weight (SF >= 1.0)."""
        result = top_tension_required(submerged_weight_kn=500.0)
        assert result >= 500.0

    def test_top_tension_custom_factor(self):
        """Custom dynamic factor of 1.5 applied correctly."""
        result = top_tension_required(
            submerged_weight_kn=1000.0, dynamic_factor=1.5
        )
        assert result == pytest.approx(1500.0, rel=0.01)


class TestWallThicknessRequired:
    """Per Barlow's formula: t = (P * OD) / (2 * SMYS * SF)."""

    def test_barlow_typical_riser(self):
        """21-inch OD, 10 MPa, X80 (552 MPa), SF=1.5 -> ~0.127 mm... check."""
        result = wall_thickness_required(
            od_mm=533.4,  # 21 inch
            design_pressure_mpa=10.0,
            smys_mpa=552.0,
            safety_factor=1.5,
        )
        # t = (10 * 533.4) / (2 * 552 * 1.5) = 5334 / 1656 = 3.22 mm
        assert result == pytest.approx(3.22, rel=0.02)

    def test_wall_thickness_increases_with_pressure(self):
        """Higher pressure requires thicker wall."""
        t1 = wall_thickness_required(533.4, 10.0, 552.0)
        t2 = wall_thickness_required(533.4, 20.0, 552.0)
        assert t2 > t1

    def test_wall_thickness_increases_with_diameter(self):
        """Larger OD requires thicker wall for same pressure."""
        t1 = wall_thickness_required(400.0, 15.0, 552.0)
        t2 = wall_thickness_required(600.0, 15.0, 552.0)
        assert t2 > t1

    def test_wall_thickness_decreases_with_smys(self):
        """Stronger material allows thinner wall."""
        t1 = wall_thickness_required(533.4, 15.0, 414.0)  # X60
        t2 = wall_thickness_required(533.4, 15.0, 552.0)  # X80
        assert t1 > t2


class TestEffectiveTension:
    """Effective tension at depth along riser string."""

    def test_effective_tension_at_surface(self):
        """At surface (depth_factor=0), effective = top tension."""
        result = effective_tension(
            top_tension_kn=1000.0, submerged_weight_kn=500.0, depth_factor=0.0
        )
        assert result == pytest.approx(1000.0, rel=0.01)

    def test_effective_tension_at_mudline(self):
        """At mudline (depth_factor=1), effective = top - submerged weight."""
        result = effective_tension(
            top_tension_kn=1000.0, submerged_weight_kn=500.0, depth_factor=1.0
        )
        assert result == pytest.approx(500.0, rel=0.01)

    def test_effective_tension_decreases_with_depth(self):
        """Tension decreases linearly with depth factor."""
        t_top = effective_tension(1000.0, 500.0, 0.0)
        t_mid = effective_tension(1000.0, 500.0, 0.5)
        t_bot = effective_tension(1000.0, 500.0, 1.0)
        assert t_top > t_mid > t_bot


class TestMinimumSlipRingTension:
    """Per 2H-TNE-0050-03 Eq 3.2: T_SRmin = Ws*fwt - Bn*fbt + Ai*(dm*Hm - dw*Hw)."""

    def test_typical_deepwater_slip_ring_tension(self):
        """Verify formula with typical deepwater values."""
        result = minimum_slip_ring_tension(
            submerged_weight_kn=2000.0,
            buoyancy_uplift_kn=800.0,
            internal_area_m2=0.15,
            mud_density_kn_m3=14.0,
            mud_column_m=1500.0,
            seawater_density_kn_m3=10.05,
            seawater_column_m=1500.0,
            f_wt=1.05,
            f_bt=0.96,
        )
        # T = 2000*1.05 - 800*0.96 + 0.15*(14*1500 - 10.05*1500)
        # T = 2100 - 768 + 0.15*(21000 - 15075)
        # T = 2100 - 768 + 0.15*5925
        # T = 2100 - 768 + 888.75 = 2220.75
        assert result == pytest.approx(2220.75, rel=0.01)

    def test_slip_ring_tension_increases_with_weight(self):
        """Heavier riser requires more tension."""
        t1 = minimum_slip_ring_tension(1500.0, 800.0, 0.15, 14.0, 1500.0, 10.05, 1500.0)
        t2 = minimum_slip_ring_tension(2500.0, 800.0, 0.15, 14.0, 1500.0, 10.05, 1500.0)
        assert t2 > t1

    def test_slip_ring_tension_decreases_with_buoyancy(self):
        """More buoyancy reduces required tension."""
        t1 = minimum_slip_ring_tension(2000.0, 500.0, 0.15, 14.0, 1500.0, 10.05, 1500.0)
        t2 = minimum_slip_ring_tension(2000.0, 1000.0, 0.15, 14.0, 1500.0, 10.05, 1500.0)
        assert t1 > t2
