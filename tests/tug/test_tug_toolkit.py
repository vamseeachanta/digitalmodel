# ABOUTME: Tests for the tug toolkit — bollard pull, girting, fendering, towline, emissions
# ABOUTME: Known-value checks tied to the tug service one-pager's numbers (epic #1193)
"""Tests for digitalmodel.tug."""

import math

import pytest

from digitalmodel.tug.bollard_pull import bollard_pull, escort_force
from digitalmodel.tug.emissions import (
    OperatingPhase,
    cycle_energy,
    fuel_and_emissions,
    imo_tier_iii_nox_limit,
    size_hybrid,
)
from digitalmodel.tug.fendering import (
    AssistedVessel,
    berthing_energy,
    optimize_fender_height,
)
from digitalmodel.tug.stability_girting import (
    angle_of_vanishing_stability,
    assess_girting,
    compute_gz_curve,
    critical_towline_force,
    heeling_arm_curve,
    is_self_righting,
)
from digitalmodel.tug.towline import (
    snapback_recoil_energy,
    speed_amplification_factor,
    towline_load,
)

# A realistic tug GZ curve: rises to a ~0.45 m peak near 35 deg, vanishes ~64 deg.
GZ_REAL = [
    (0, 0.0),
    (5, 0.09),
    (10, 0.18),
    (15, 0.27),
    (20, 0.35),
    (25, 0.41),
    (30, 0.44),
    (35, 0.45),
    (40, 0.43),
    (45, 0.38),
    (50, 0.30),
    (55, 0.20),
    (60, 0.09),
    (65, -0.03),
    (70, -0.15),
]


# ---------------------------------------------------------------------------
# bollard_pull (#1194)
# ---------------------------------------------------------------------------
class TestBollardPull:
    def test_asd_4000kw(self):
        """ASD 4000 kW -> ~58 t ahead, near-symmetric astern (brochure ~52-60 t)."""
        r = bollard_pull(4000, "asd")
        assert r.bp_ahead_t == pytest.approx(58.0, rel=1e-6)
        assert r.bp_astern_t == pytest.approx(0.95 * 58.0, rel=1e-6)
        assert 52 <= r.bp_ahead_t <= 60

    def test_conventional_lower_and_weaker_astern(self):
        conv = bollard_pull(4000, "conventional")
        asd = bollard_pull(4000, "asd")
        assert conv.bp_ahead_t < asd.bp_ahead_t
        # conventional screw loses much more going astern than an ASD
        assert conv.bp_astern_t / conv.bp_ahead_t < asd.bp_astern_t / asd.bp_ahead_t

    def test_transverse_pull_is_60pct(self):
        r = bollard_pull(5000, "conventional")
        assert r.transverse_pull_t == pytest.approx(0.60 * r.bp_ahead_t)

    def test_transverse_pull_propulsion_dependent(self):
        """ASD vectors ~full thrust sideways; conventional only ~0.6 (false-safe fix)."""
        conv = bollard_pull(4000, "conventional")
        asd = bollard_pull(4000, "asd")
        assert conv.transverse_pull_t == pytest.approx(0.60 * conv.bp_ahead_t)
        assert asd.transverse_pull_t == pytest.approx(1.00 * asd.bp_ahead_t)

    def test_zero_power_rejected(self):
        with pytest.raises(ValueError):
            bollard_pull(0, "asd")

    def test_escort_indirect_force_can_exceed_bollard_pull(self):
        """At escort speed the hydrodynamic steering force exceeds direct BP."""
        bp = bollard_pull(4000, "asd")
        e = escort_force(8.0, lateral_area_m2=60.0, bp_ahead_t=bp.bp_ahead_t)
        assert e.steering_force_t > 0
        assert e.braking_force_t > 0
        assert e.exceeds_bollard_pull is True

    def test_escort_force_scales_with_speed_squared(self):
        e1 = escort_force(4.0, 60.0, 58.0)
        e2 = escort_force(8.0, 60.0, 58.0)
        assert e2.steering_force_t == pytest.approx(4.0 * e1.steering_force_t, rel=1e-6)


# ---------------------------------------------------------------------------
# stability / girting (#1195)
# ---------------------------------------------------------------------------
class TestGirting:
    def test_heeling_arm_decreases_with_heel(self):
        ha = heeling_arm_curve(10.0, 3.0, 400.0)
        assert ha[0][1] == pytest.approx((10.0 / 400.0) * 3.0)  # base at 0 deg
        assert ha[0][1] > ha[-1][1]  # decreases with cos

    def test_safe_tow_has_low_equilibrium(self):
        r = assess_girting(
            GZ_REAL,
            towline_force_t=40.0,
            tow_point_lever_m=4.0,
            displacement_t=400.0,
            deck_edge_angle_deg=40.0,
        )
        assert r.equilibrium_heel_deg is not None
        assert r.equilibrium_heel_deg < 40.0
        assert r.girting_safe is True
        assert r.margin_deg > 0

    def test_excess_tow_capsizes(self):
        """Towline above the critical force -> no equilibrium below deck edge."""
        r = assess_girting(
            GZ_REAL,
            towline_force_t=70.0,
            tow_point_lever_m=4.0,
            displacement_t=400.0,
            deck_edge_angle_deg=40.0,
        )
        assert r.equilibrium_heel_deg is None
        assert r.girting_safe is False

    def test_critical_force_matches_deck_edge_criterion(self):
        crit = critical_towline_force(GZ_REAL, 4.0, 400.0, 40.0)
        # F_crit = (W/lever) * GZ(40)/cos(40) = (400/4)*0.43/cos40
        expected = (400.0 / 4.0) * (0.43 / math.cos(math.radians(40)))
        assert crit == pytest.approx(expected, rel=1e-6)
        # safe load is below critical, capsize load is above
        assert 40.0 < crit < 70.0

    def test_vanishing_stability_and_self_righting(self):
        avs = angle_of_vanishing_stability(GZ_REAL)
        assert 60 < avs < 65
        assert is_self_righting(GZ_REAL) is False
        # a curve positive out to 90 deg is self-righting
        positive = [
            (h, max(0.05, math.cos(math.radians(h)) * 0.5)) for h in range(0, 91, 5)
        ]
        assert is_self_righting(positive) is True

    def test_zero_towline_is_safe(self):
        """No transverse load -> upright equilibrium, not a false capsize."""
        r = assess_girting(GZ_REAL, 0.0, 4.0, 400.0, deck_edge_angle_deg=40.0)
        assert r.equilibrium_heel_deg == 0.0
        assert r.girting_safe is True

    def test_critical_force_consistent_with_verdict(self):
        """Reported critical load brackets the safe/unsafe boundary."""
        crit = critical_towline_force(GZ_REAL, 4.0, 400.0, 40.0)
        below = assess_girting(GZ_REAL, crit * 0.98, 4.0, 400.0, 40.0)
        above = assess_girting(GZ_REAL, crit * 1.02, 4.0, 400.0, 40.0)
        assert below.girting_safe is True
        assert above.girting_safe is False

    def test_gm_only_curve_reusable(self):
        """compute_gz_curve (reused from naval_architecture) integrates cleanly."""
        gz = compute_gz_curve(gm_m=1.0)
        r = assess_girting(gz, 10.0, 3.0, 400.0, deck_edge_angle_deg=30.0)
        assert r.equilibrium_heel_deg == pytest.approx(4.29, abs=0.5)


# ---------------------------------------------------------------------------
# fendering (#1196)
# ---------------------------------------------------------------------------
class TestFendering:
    def test_optimum_centre_covers_common_band(self):
        fleet = [
            AssistedVessel("coaster", 2.0, 5.0),
            AssistedVessel("feeder", 3.0, 6.0),
            AssistedVessel("handysize", 2.5, 5.5),
        ]
        r = optimize_fender_height(fleet, fender_face_height_m=2.0)
        assert r.fender_height_m == pytest.approx(4.0, abs=0.05)
        assert r.mean_overlap_fraction == pytest.approx(1.0, abs=1e-9)
        assert r.all_vessels_contact is True
        assert r.worst_overlap_fraction > 0

    def test_split_fleet_flags_partial_contact(self):
        fleet = [
            AssistedVessel("barge", 0.5, 1.5),
            AssistedVessel("vlcc", 12.0, 18.0),
        ]
        r = optimize_fender_height(fleet, fender_face_height_m=1.0)
        # no single 1 m fender can touch both a 1 m and an 18 m freeboard band
        assert r.all_vessels_contact is False

    def test_empty_fleet_rejected(self):
        with pytest.raises(ValueError):
            optimize_fender_height([], 2.0)

    def test_berthing_energy_pianc(self):
        r = berthing_energy(400.0, 0.15, fender_rated_energy_kj=10.0)
        # 0.5*400000*0.15^2*1.5*0.7*1.0 = 4725 J = 4.725 kJ
        assert r.berthing_energy_kj == pytest.approx(4.725, rel=1e-6)
        assert r.adequate is True
        assert r.utilisation == pytest.approx(0.4725, rel=1e-6)

    def test_berthing_energy_inadequate_fender(self):
        r = berthing_energy(2000.0, 0.30, fender_rated_energy_kj=10.0)
        assert r.adequate is False
        assert r.utilisation > 1.0


# ---------------------------------------------------------------------------
# towline (#1197)
# ---------------------------------------------------------------------------
class TestTowline:
    def test_speed_amplification_in_2_to_5_band(self):
        """4.6 kn vs 2.5 kn norm -> ~3.4x, inside the reported 2-5x band."""
        f = speed_amplification_factor(4.6)
        assert f == pytest.approx((4.6 / 2.5) ** 2, rel=1e-6)
        assert 2.0 <= f <= 5.0

    def test_speed_factor_floor_and_cap(self):
        assert speed_amplification_factor(1.0) == 1.0  # never below 1
        assert speed_amplification_factor(2.5) == 1.0  # at reference
        assert speed_amplification_factor(50.0) == 5.0  # capped

    def test_required_mbl_uses_class_factor(self):
        r = towline_load(60.0, society="LR", dynamic_factor=3.39)
        assert r.required_mbl_t == pytest.approx(60.0 * 3.39 * 2.5, rel=1e-6)
        assert r.adequate is None  # no line selected

    def test_selected_line_adequacy(self):
        ok = towline_load(60.0, "DNV", 1.0, selected_mbl_t=150.0)
        assert ok.safety_factor == 2.0
        assert ok.adequate is True
        short = towline_load(60.0, "DNV", 1.0, selected_mbl_t=100.0)
        assert short.adequate is False

    def test_unknown_society_rejected(self):
        with pytest.raises(ValueError):
            towline_load(60.0, society="XYZ")

    def test_snapback_energy_grows_with_elongation(self):
        low = snapback_recoil_energy(100.0, 50.0, 200.0, elongation_at_break=0.05)
        high = snapback_recoil_energy(100.0, 50.0, 200.0, elongation_at_break=0.20)
        assert high > low > 0  # stretchier line stores more recoil energy


# ---------------------------------------------------------------------------
# emissions / hybrid (#1198)
# ---------------------------------------------------------------------------
class TestEmissions:
    PROFILE = [
        OperatingPhase("transit", 0.40, 5.0),
        OperatingPhase("assist", 0.90, 1.0),
        OperatingPhase("standby", 0.10, 4.0),
    ]

    def test_tier_iii_limit_by_rpm(self):
        assert imo_tier_iii_nox_limit(120) == 3.4
        assert imo_tier_iii_nox_limit(2500) == 2.0
        assert imo_tier_iii_nox_limit(1600) == pytest.approx(9.0 * 1600**-0.2, rel=1e-9)

    def test_cycle_energy(self):
        assert cycle_energy(4000, self.PROFILE) == pytest.approx(13200.0, rel=1e-9)

    def test_hybrid_sizing_shaves_assist_peak(self):
        r = size_hybrid(4000, self.PROFILE, baseline_fraction=0.5)
        assert r.peak_power_kw == pytest.approx(3600.0)
        assert r.baseline_engine_kw == pytest.approx(2000.0)
        assert r.battery_power_kw == pytest.approx(1600.0)
        assert r.battery_energy_kwh == pytest.approx(1600.0)
        assert 0 < r.peak_energy_fraction < 1

    def test_emissions_and_tier_iii_fail(self):
        r = fuel_and_emissions(
            4000,
            self.PROFILE,
            sfoc_g_per_kwh=195.0,
            fuel="mgo",
            nox_rate_g_per_kwh=9.0,
            rated_rpm=1600,
        )
        assert r.fuel_mass_t == pytest.approx(195.0 * 13200 / 1e6, rel=1e-9)
        assert r.co2_t == pytest.approx(r.fuel_mass_t * 3.206, rel=1e-6)
        assert r.tier_iii_compliant is False  # 9 g/kWh > ~2.06 limit

    def test_emissions_tier_iii_pass_with_aftertreatment(self):
        r = fuel_and_emissions(
            4000, self.PROFILE, nox_rate_g_per_kwh=1.5, rated_rpm=1600
        )
        assert r.tier_iii_compliant is True

    def test_methanol_lower_co2_than_mgo(self):
        mgo = fuel_and_emissions(4000, self.PROFILE, fuel="mgo")
        meth = fuel_and_emissions(4000, self.PROFILE, fuel="methanol")
        assert meth.co2_t < mgo.co2_t

    def test_fuel_mass_is_fuel_specific(self):
        """Methanol's lower LHV -> ~2x the fuel mass per kWh of MGO (LHV wiring)."""
        mgo = fuel_and_emissions(4000, self.PROFILE, fuel="mgo")
        meth = fuel_and_emissions(4000, self.PROFILE, fuel="methanol")
        # default MGO SFOC stays exactly 195 g/kWh
        assert mgo.fuel_mass_t == pytest.approx(195.0 * 13200 / 1e6, rel=1e-9)
        assert meth.fuel_mass_t > 1.8 * mgo.fuel_mass_t

    def test_empty_profile_rejected(self):
        with pytest.raises(ValueError):
            cycle_energy(4000, [])
