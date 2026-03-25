# ABOUTME: Unit tests for WellboreHydraulics — ECD, annular velocity, pressure drop,
# ABOUTME: cuttings transport ratio. Covers construction, physics behaviour, and edge cases.

"""
Tests for WellboreHydraulics in digitalmodel.well.drilling.hydraulics.

Field scenario basis:
  - 8.5" hole drilled with 4.5" OD / 3.826" ID drill pipe
  - 10.5 ppg water-based mud, 400 gpm, TVD = 8 000 ft
  - Bingham Plastic: PV = 18 cp, YP = 12 lbf/100 ft^2
  - Expected ECD ~10.6–10.8 ppg (industry rule-of-thumb)
"""

import pytest

from digitalmodel.well.drilling.hydraulics import WellboreHydraulics


# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

def _standard() -> WellboreHydraulics:
    """Return a WellboreHydraulics configured for a standard 8.5-in section."""
    return WellboreHydraulics(
        pipe_od=4.5,          # inches
        pipe_id=3.826,        # inches
        hole_diameter=8.5,    # inches
        mud_weight=10.5,      # ppg
        flow_rate=400.0,      # gal/min
        tvd=8000.0,           # ft
        plastic_viscosity=18.0,   # cp
        yield_point=12.0,         # lbf/100 ft^2
    )


def _slim_hole() -> WellboreHydraulics:
    """Slim hole: 6" hole, 3.5" OD pipe — tighter annulus."""
    return WellboreHydraulics(
        pipe_od=3.5,
        pipe_id=2.992,
        hole_diameter=6.0,
        mud_weight=9.5,
        flow_rate=200.0,
        tvd=5000.0,
        plastic_viscosity=15.0,
        yield_point=10.0,
    )


# ---------------------------------------------------------------------------
# Construction and validation
# ---------------------------------------------------------------------------

class TestWellboreHydraulicsConstruction:
    def test_attributes_stored(self):
        wh = _standard()
        assert wh.pipe_od == pytest.approx(4.5)
        assert wh.pipe_id == pytest.approx(3.826)
        assert wh.hole_diameter == pytest.approx(8.5)
        assert wh.mud_weight == pytest.approx(10.5)
        assert wh.flow_rate == pytest.approx(400.0)
        assert wh.tvd == pytest.approx(8000.0)
        assert wh.plastic_viscosity == pytest.approx(18.0)
        assert wh.yield_point == pytest.approx(12.0)

    def test_hole_must_be_larger_than_pipe_od(self):
        with pytest.raises(ValueError, match="hole_diameter"):
            WellboreHydraulics(
                pipe_od=8.5,
                pipe_id=7.0,
                hole_diameter=6.0,  # smaller than pipe OD — invalid
                mud_weight=10.5,
                flow_rate=400.0,
                tvd=8000.0,
                plastic_viscosity=18.0,
                yield_point=12.0,
            )

    def test_pipe_id_must_be_less_than_pipe_od(self):
        with pytest.raises(ValueError, match="pipe_id"):
            WellboreHydraulics(
                pipe_od=4.5,
                pipe_id=5.0,  # ID > OD — invalid
                hole_diameter=8.5,
                mud_weight=10.5,
                flow_rate=400.0,
                tvd=8000.0,
                plastic_viscosity=18.0,
                yield_point=12.0,
            )

    def test_mud_weight_must_be_positive(self):
        with pytest.raises(ValueError, match="mud_weight"):
            WellboreHydraulics(
                pipe_od=4.5,
                pipe_id=3.826,
                hole_diameter=8.5,
                mud_weight=0.0,
                flow_rate=400.0,
                tvd=8000.0,
                plastic_viscosity=18.0,
                yield_point=12.0,
            )

    def test_tvd_must_be_positive(self):
        with pytest.raises(ValueError, match="tvd"):
            WellboreHydraulics(
                pipe_od=4.5,
                pipe_id=3.826,
                hole_diameter=8.5,
                mud_weight=10.5,
                flow_rate=400.0,
                tvd=0.0,
                plastic_viscosity=18.0,
                yield_point=12.0,
            )

    def test_flow_rate_must_be_non_negative(self):
        with pytest.raises(ValueError, match="flow_rate"):
            WellboreHydraulics(
                pipe_od=4.5,
                pipe_id=3.826,
                hole_diameter=8.5,
                mud_weight=10.5,
                flow_rate=-10.0,
                tvd=8000.0,
                plastic_viscosity=18.0,
                yield_point=12.0,
            )


# ---------------------------------------------------------------------------
# annular_velocity()
# ---------------------------------------------------------------------------

class TestAnnularVelocity:
    def test_returns_float(self):
        wh = _standard()
        av = wh.annular_velocity()
        assert isinstance(av, float)

    def test_positive_at_non_zero_flow(self):
        wh = _standard()
        assert wh.annular_velocity() > 0.0

    def test_formula_correctness(self):
        """
        AV = 24.51 * q / (D_h^2 - D_od^2)  [ft/min]

        Constant 24.51 is derived from unit conversion:
            231 in^3/gal * 144 in^2/ft^2 / (1728 in^3/ft^3 * pi/4)
        Source: Bourgoyne et al., SPE Textbook Vol 2, Ch. 4.
        """
        wh = _standard()
        q = 400.0
        d_h = 8.5
        d_od = 4.5
        expected = 24.51 * q / (d_h ** 2 - d_od ** 2)
        assert wh.annular_velocity() == pytest.approx(expected, rel=1e-6)

    def test_zero_flow_gives_zero_velocity(self):
        wh = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=0.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh.annular_velocity() == pytest.approx(0.0)

    def test_higher_flow_rate_increases_av(self):
        wh_lo = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=200.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        wh_hi = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=500.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh_hi.annular_velocity() > wh_lo.annular_velocity()

    def test_slim_hole_has_higher_av_than_large_hole_at_same_flow(self):
        """Smaller annular clearance → higher velocity at same flow rate."""
        wh_large = _standard()   # 8.5" hole, 4.5" pipe OD, 400 gpm
        wh_slim = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=6.0,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh_slim.annular_velocity() > wh_large.annular_velocity()


# ---------------------------------------------------------------------------
# pressure_drop_annulus()
# ---------------------------------------------------------------------------

class TestPressureDropAnnulus:
    def test_returns_float(self):
        wh = _standard()
        dp = wh.pressure_drop_annulus()
        assert isinstance(dp, float)

    def test_positive_at_non_zero_flow(self):
        wh = _standard()
        assert wh.pressure_drop_annulus() > 0.0

    def test_zero_flow_gives_zero_pressure_drop(self):
        wh = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=0.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh.pressure_drop_annulus() == pytest.approx(0.0)

    def test_higher_flow_rate_increases_pressure_drop(self):
        wh_lo = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=200.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        wh_hi = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=500.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh_hi.pressure_drop_annulus() > wh_lo.pressure_drop_annulus()

    def test_higher_plastic_viscosity_increases_pressure_drop(self):
        wh_lo = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=10.0, yield_point=12.0,
        )
        wh_hi = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=30.0, yield_point=12.0,
        )
        assert wh_hi.pressure_drop_annulus() > wh_lo.pressure_drop_annulus()

    def test_higher_yield_point_increases_pressure_drop(self):
        wh_lo = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=5.0,
        )
        wh_hi = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=25.0,
        )
        assert wh_hi.pressure_drop_annulus() > wh_lo.pressure_drop_annulus()

    def test_pressure_drop_units_reasonable_psi(self):
        """8 000 ft well with 400 gpm should give order-of-hundreds psi, not thousands."""
        wh = _standard()
        dp = wh.pressure_drop_annulus()
        assert 10.0 < dp < 1000.0


# ---------------------------------------------------------------------------
# ecd()
# ---------------------------------------------------------------------------

class TestECD:
    def test_returns_float(self):
        wh = _standard()
        assert isinstance(wh.ecd(100.0), float)

    def test_ecd_equals_mud_weight_plus_friction_component(self):
        """ECD = mud_weight + dp / (0.052 * tvd)."""
        wh = _standard()
        dp = 200.0
        expected = 10.5 + dp / (0.052 * 8000.0)
        assert wh.ecd(dp) == pytest.approx(expected, rel=1e-6)

    def test_ecd_greater_than_mud_weight_when_circulating(self):
        wh = _standard()
        dp = wh.pressure_drop_annulus()
        assert wh.ecd(dp) > wh.mud_weight

    def test_ecd_equals_mud_weight_at_zero_pressure_drop(self):
        wh = _standard()
        assert wh.ecd(0.0) == pytest.approx(wh.mud_weight)

    def test_field_realistic_ecd_range(self):
        """
        Field scenario:
          8.5-in hole, 4.5-in OD drill pipe, 10.5 ppg mud, 400 gpm, 8 000 ft TVD
          Bingham: PV=18 cp, YP=12 lbf/100 ft^2
          Expected ECD in range 10.5–11.5 ppg (generous tolerance for formula variant).
        """
        wh = _standard()
        dp = wh.pressure_drop_annulus()
        ecd = wh.ecd(dp)
        assert 10.5 < ecd < 11.5, f"ECD {ecd:.3f} ppg outside 10.5–11.5 ppg range"

    def test_deeper_well_lower_ecd_increment(self):
        """
        Same annular pressure loss, deeper TVD → smaller ECD increment over mud weight.
        Because 0.052 * tvd is in the denominator.
        """
        wh_shallow = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=4000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        wh_deep = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=12000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        fixed_dp = 300.0
        ecd_shallow = wh_shallow.ecd(fixed_dp)
        ecd_deep = wh_deep.ecd(fixed_dp)
        assert ecd_shallow > ecd_deep


# ---------------------------------------------------------------------------
# cuttings_transport_ratio()
# ---------------------------------------------------------------------------

class TestCuttingsTransportRatio:
    def test_returns_float(self):
        wh = _standard()
        assert isinstance(wh.cuttings_transport_ratio(), float)

    def test_ctr_between_zero_and_one(self):
        wh = _standard()
        ctr = wh.cuttings_transport_ratio()
        assert 0.0 <= ctr <= 1.0

    def test_zero_flow_gives_zero_ctr(self):
        wh = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=0.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh.cuttings_transport_ratio() == pytest.approx(0.0)

    def test_high_flow_rate_gives_ctr_of_one(self):
        """Very high flow rate should saturate CTR at 1.0."""
        wh = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=2000.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh.cuttings_transport_ratio() == pytest.approx(1.0)

    def test_higher_flow_rate_improves_ctr(self):
        wh_lo = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=100.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        wh_hi = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        assert wh_hi.cuttings_transport_ratio() > wh_lo.cuttings_transport_ratio()

    def test_adequate_cleaning_at_standard_conditions(self):
        """Standard drilling conditions at 400 gpm should achieve CTR > 0.5."""
        wh = _standard()
        assert wh.cuttings_transport_ratio() > 0.5


# ---------------------------------------------------------------------------
# Integration — end-to-end field scenario
# ---------------------------------------------------------------------------

class TestIntegrationFieldScenario:
    """
    Verify all methods are consistent and the workflow hangs together.
    Inputs: 8.5-in hole, 4.5-in OD drill pipe (3.826-in ID),
            10.5 ppg mud, 400 gpm, 8 000 ft TVD, PV=18 cp, YP=12.
    """

    def test_full_workflow_types(self):
        wh = _standard()
        av = wh.annular_velocity()
        dp = wh.pressure_drop_annulus()
        ecd = wh.ecd(dp)
        ctr = wh.cuttings_transport_ratio()
        assert all(isinstance(v, float) for v in [av, dp, ecd, ctr])

    def test_ecd_increases_monotonically_with_flow_rate(self):
        """More flow → higher dp → higher ECD (with dp from pressure_drop_annulus)."""
        ecds = []
        for q in [200.0, 300.0, 400.0, 500.0]:
            wh = WellboreHydraulics(
                pipe_od=4.5, pipe_id=3.826, hole_diameter=8.5,
                mud_weight=10.5, flow_rate=q, tvd=8000.0,
                plastic_viscosity=18.0, yield_point=12.0,
            )
            ecds.append(wh.ecd(wh.pressure_drop_annulus()))
        assert ecds == sorted(ecds), "ECD should increase with flow rate"

    def test_slim_hole_higher_ecd_than_large_hole(self):
        """Smaller annular clearance at same flow → higher dp → higher ECD."""
        wh_large = _standard()
        wh_slim = WellboreHydraulics(
            pipe_od=4.5, pipe_id=3.826, hole_diameter=6.0,
            mud_weight=10.5, flow_rate=400.0, tvd=8000.0,
            plastic_viscosity=18.0, yield_point=12.0,
        )
        dp_large = wh_large.pressure_drop_annulus()
        dp_slim = wh_slim.pressure_drop_annulus()
        assert dp_slim > dp_large
