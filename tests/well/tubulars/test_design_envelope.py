# ABOUTME: TDD tests for triaxial von Mises design envelope for casing/tubing (WRK-376)
# ABOUTME: Covers VME isotropic, anisotropic CRA, API biaxial ellipse, and envelope sweep

"""Tests for well.tubulars.design_envelope module.

Known design cases sourced from:
- API TR 5C3 / ISO 10400 worked examples
- SPE 48330 — casing design for directional wells
- Halliburton casing design manual (generic steel grades)
"""

import math

import numpy as np
import pytest

from digitalmodel.well.tubulars.design_envelope import (
    AnisotropicVmeEnvelope,
    ApiEllipseEnvelope,
    IsotropicVmeEnvelope,
    TubularGeometry,
    compute_hoop_stress,
    compute_vme_stress,
    design_envelope_points,
)


# ---------------------------------------------------------------------------
# Fixtures — representative casing / tubing strings
# ---------------------------------------------------------------------------

def make_7in_26ppf_l80() -> TubularGeometry:
    """7-inch, 26 lb/ft, L-80 casing — common production casing."""
    return TubularGeometry(
        od_in=7.0,
        id_in=6.276,
        smys_psi=80_000.0,
        burst_rating_psi=5_180.0,
        collapse_rating_psi=5_550.0,
    )


def make_9625_47ppf_n80() -> TubularGeometry:
    """9-5/8-inch, 47 lb/ft, N-80 intermediate casing."""
    return TubularGeometry(
        od_in=9.625,
        id_in=8.681,
        smys_psi=80_000.0,
        burst_rating_psi=4_760.0,
        collapse_rating_psi=4_630.0,
    )


def make_25cr_cra() -> TubularGeometry:
    """5-1/2-inch, 17 lb/ft, 25Cr duplex stainless (CRA grade).

    25Cr has anisotropic yield: Y_hoop ~ 1.05 * Y_axial due to texture.
    """
    return TubularGeometry(
        od_in=5.5,
        id_in=4.778,
        smys_psi=110_000.0,
        burst_rating_psi=8_860.0,
        collapse_rating_psi=8_120.0,
    )


# ---------------------------------------------------------------------------
# TubularGeometry helpers
# ---------------------------------------------------------------------------

class TestTubularGeometry:
    """Unit tests for TubularGeometry derived properties."""

    def test_wall_thickness_computed_correctly(self):
        geom = make_7in_26ppf_l80()
        expected_t = (7.0 - 6.276) / 2.0  # 0.362 inches
        assert geom.wall_thickness_in == pytest.approx(expected_t, rel=1e-6)

    def test_cross_sectional_area_is_positive(self):
        geom = make_7in_26ppf_l80()
        # A = pi/4 * (OD^2 - ID^2)
        expected_area = math.pi / 4.0 * (7.0**2 - 6.276**2)
        assert geom.cross_section_area_in2 == pytest.approx(expected_area, rel=1e-6)

    def test_yield_force_equals_smys_times_area(self):
        geom = make_7in_26ppf_l80()
        expected_fy = geom.smys_psi * geom.cross_section_area_in2
        assert geom.yield_force_lbf == pytest.approx(expected_fy, rel=1e-6)

    def test_mean_diameter_between_od_and_id(self):
        geom = make_7in_26ppf_l80()
        assert geom.id_in < geom.mean_diameter_in < geom.od_in

    def test_od_must_exceed_id(self):
        with pytest.raises(ValueError, match="OD must exceed ID"):
            TubularGeometry(
                od_in=5.0, id_in=6.0, smys_psi=80_000.0,
                burst_rating_psi=5_000.0, collapse_rating_psi=4_500.0,
            )

    def test_smys_must_be_positive(self):
        with pytest.raises(ValueError, match="SMYS must be positive"):
            TubularGeometry(
                od_in=7.0, id_in=6.276, smys_psi=-1.0,
                burst_rating_psi=5_000.0, collapse_rating_psi=4_500.0,
            )


# ---------------------------------------------------------------------------
# Stress helper functions
# ---------------------------------------------------------------------------

class TestHoopStress:
    """Thin-wall hoop stress: sigma_t = p_diff * D_mean / (2*t)."""

    def test_positive_internal_pressure_gives_tensile_hoop(self):
        # p_diff > 0 means internal > external (burst condition)
        sigma_t = compute_hoop_stress(
            differential_pressure_psi=1_000.0,
            od_in=7.0,
            id_in=6.276,
        )
        assert sigma_t > 0.0

    def test_zero_pressure_gives_zero_hoop(self):
        sigma_t = compute_hoop_stress(0.0, od_in=7.0, id_in=6.276)
        assert sigma_t == pytest.approx(0.0, abs=1.0)

    def test_known_value_7in_1000psi(self):
        # sigma_t = p * D_mean / (2*t); D_mean = (7+6.276)/2 = 6.638; t = 0.362
        d_mean = (7.0 + 6.276) / 2.0
        t = (7.0 - 6.276) / 2.0
        expected = 1_000.0 * d_mean / (2.0 * t)
        sigma_t = compute_hoop_stress(1_000.0, od_in=7.0, id_in=6.276)
        assert sigma_t == pytest.approx(expected, rel=1e-4)

    def test_hoop_scales_linearly_with_pressure(self):
        s1 = compute_hoop_stress(500.0, od_in=7.0, id_in=6.276)
        s2 = compute_hoop_stress(1_000.0, od_in=7.0, id_in=6.276)
        assert s2 == pytest.approx(2.0 * s1, rel=1e-6)


class TestVmeStress:
    """Von Mises equivalent: sqrt(sa^2 + st^2 - sa*st + 3*tau^2)."""

    def test_pure_axial_gives_sigma_a(self):
        # With sigma_t=0, tau=0: vme = |sigma_a|
        vme = compute_vme_stress(sigma_axial_psi=50_000.0, sigma_hoop_psi=0.0)
        assert vme == pytest.approx(50_000.0, rel=1e-6)

    def test_pure_hoop_gives_sigma_t(self):
        vme = compute_vme_stress(sigma_axial_psi=0.0, sigma_hoop_psi=40_000.0)
        assert vme == pytest.approx(40_000.0, rel=1e-6)

    def test_equal_biaxial_gives_sigma(self):
        # Both equal: vme = sqrt(s^2 + s^2 - s*s) = sqrt(s^2) = s
        s = 30_000.0
        vme = compute_vme_stress(sigma_axial_psi=s, sigma_hoop_psi=s)
        assert vme == pytest.approx(s, rel=1e-6)

    def test_vme_always_nonnegative(self):
        vme = compute_vme_stress(sigma_axial_psi=-30_000.0, sigma_hoop_psi=20_000.0)
        assert vme >= 0.0

    def test_torsion_increases_vme(self):
        vme_no_torsion = compute_vme_stress(10_000.0, 10_000.0, tau_psi=0.0)
        vme_with_torsion = compute_vme_stress(10_000.0, 10_000.0, tau_psi=5_000.0)
        assert vme_with_torsion > vme_no_torsion

    def test_known_value(self):
        # sa=40000, st=20000, tau=0
        # vme = sqrt(40000^2 + 20000^2 - 40000*20000)
        sa, st = 40_000.0, 20_000.0
        expected = math.sqrt(sa**2 + st**2 - sa * st)
        vme = compute_vme_stress(sigma_axial_psi=sa, sigma_hoop_psi=st)
        assert vme == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Isotropic VME Envelope
# ---------------------------------------------------------------------------

class TestIsotropicVmeEnvelope:
    """VME ellipse for standard isotropic steel grades (J-55, L-80, P-110)."""

    def setup_method(self):
        self.geom = make_7in_26ppf_l80()
        self.envelope = IsotropicVmeEnvelope(
            geometry=self.geom, design_factor=1.0
        )

    def test_envelope_returns_array_with_correct_shape(self):
        result = self.envelope.compute(n_points=50)
        assert result.shape == (50, 2)

    def test_first_column_is_axial_force_lbf(self):
        result = self.envelope.compute(n_points=100)
        # Axial force column — must span negative (compression) to positive (tension)
        axial = result[:, 0]
        assert axial.min() < 0.0
        assert axial.max() > 0.0

    def test_second_column_is_differential_pressure_psi(self):
        result = self.envelope.compute(n_points=100)
        dp = result[:, 1]
        # dp > 0 = burst; dp < 0 = collapse; both must appear on envelope
        assert dp.max() > 0.0
        assert dp.min() < 0.0

    def test_pure_tension_zero_pressure_equals_yield_force(self):
        # At dp=0: limiting axial = smys * area / design_factor
        result = self.envelope.compute(n_points=200)
        axial = result[:, 0]
        dp = result[:, 1]
        # Find points near dp ≈ 0
        near_zero = np.abs(dp) < 200.0
        assert near_zero.any(), "No points found near dp=0"
        max_axial_near_zero = axial[near_zero].max()
        expected_fy = self.geom.yield_force_lbf
        assert max_axial_near_zero == pytest.approx(expected_fy, rel=0.02)

    def test_design_factor_scales_envelope(self):
        env_df1 = IsotropicVmeEnvelope(self.geom, design_factor=1.0)
        env_df12 = IsotropicVmeEnvelope(self.geom, design_factor=1.2)
        pts_df1 = env_df1.compute(n_points=100)
        pts_df12 = env_df12.compute(n_points=100)
        # DF=1.2 envelope should be strictly inside DF=1.0
        assert pts_df12[:, 1].max() < pts_df1[:, 1].max()
        assert pts_df12[:, 0].max() < pts_df1[:, 0].max()

    def test_envelope_is_closed_or_bounded(self):
        result = self.envelope.compute(n_points=100)
        # All points must be finite
        assert np.all(np.isfinite(result))

    def test_minimum_n_points_respected(self):
        result = self.envelope.compute(n_points=10)
        assert result.shape[0] == 10

    def test_design_factor_must_be_positive(self):
        with pytest.raises(ValueError, match="design_factor must be positive"):
            IsotropicVmeEnvelope(self.geom, design_factor=0.0)


# ---------------------------------------------------------------------------
# API Biaxial Ellipse Envelope
# ---------------------------------------------------------------------------

class TestApiEllipseEnvelope:
    """API biaxial correction for burst/collapse with axial load."""

    def setup_method(self):
        self.geom = make_7in_26ppf_l80()
        self.envelope = ApiEllipseEnvelope(
            geometry=self.geom, design_factor=1.0
        )

    def test_envelope_returns_correct_shape(self):
        result = self.envelope.compute(n_points=100)
        assert result.shape == (100, 2)

    def test_zero_axial_burst_equals_rated_burst(self):
        # At F_axial = 0: burst limit should equal burst_rating_psi / DF
        result = self.envelope.compute(n_points=200)
        axial = result[:, 0]
        dp = result[:, 1]
        near_zero = np.abs(axial) < 1_000.0
        if near_zero.any():
            max_dp = dp[near_zero].max()
            expected = self.geom.burst_rating_psi / 1.0
            assert max_dp == pytest.approx(expected, rel=0.05)

    def test_zero_axial_collapse_equals_rated_collapse(self):
        result = self.envelope.compute(n_points=200)
        axial = result[:, 0]
        dp = result[:, 1]
        near_zero = np.abs(axial) < 1_000.0
        if near_zero.any():
            min_dp = dp[near_zero].min()
            expected_collapse = -self.geom.collapse_rating_psi / 1.0
            assert min_dp == pytest.approx(expected_collapse, rel=0.05)

    def test_axial_tension_reduces_burst_capacity(self):
        # Biaxial correction: burst is reduced by tensile axial load
        result = self.envelope.compute(n_points=200)
        axial = result[:, 0]
        dp = result[:, 1]
        # Burst side (dp > 0)
        burst_pts = dp > 0
        low_axial = (axial > -5_000) & (axial < 5_000)
        high_tension = axial > self.geom.yield_force_lbf * 0.6
        if burst_pts.any() and low_axial.any() and high_tension.any():
            dp_low = dp[burst_pts & low_axial].max()
            dp_high = dp[burst_pts & high_tension].max()
            assert dp_high < dp_low

    def test_all_points_finite(self):
        result = self.envelope.compute(n_points=100)
        assert np.all(np.isfinite(result))

    def test_design_factor_reduces_envelope(self):
        env1 = ApiEllipseEnvelope(self.geom, design_factor=1.0)
        env2 = ApiEllipseEnvelope(self.geom, design_factor=1.2)
        pts1 = env1.compute(n_points=100)
        pts2 = env2.compute(n_points=100)
        assert pts2[:, 1].max() < pts1[:, 1].max()


# ---------------------------------------------------------------------------
# Anisotropic VME Envelope (CRA grades)
# ---------------------------------------------------------------------------

class TestAnisotropicVmeEnvelope:
    """Anisotropic VME for CRA/chrome grades with different hoop and axial yield."""

    def setup_method(self):
        self.geom = make_25cr_cra()
        # 25Cr: Y_hoop ≈ 1.05 × Y_axial
        self.envelope = AnisotropicVmeEnvelope(
            geometry=self.geom,
            yield_hoop_ratio=1.05,
            design_factor=1.0,
        )

    def test_envelope_returns_correct_shape(self):
        result = self.envelope.compute(n_points=50)
        assert result.shape == (50, 2)

    def test_anisotropic_differs_from_isotropic(self):
        iso_env = IsotropicVmeEnvelope(self.geom, design_factor=1.0)
        aniso_env = AnisotropicVmeEnvelope(
            self.geom, yield_hoop_ratio=1.05, design_factor=1.0
        )
        iso_pts = iso_env.compute(n_points=100)
        aniso_pts = aniso_env.compute(n_points=100)
        # Burst capacity (dp max) should differ
        assert iso_pts[:, 1].max() != pytest.approx(aniso_pts[:, 1].max(), rel=0.001)

    def test_isotropic_ratio_matches_isotropic_vme(self):
        # When yield_hoop_ratio=1.0, anisotropic collapses to isotropic
        iso_env = IsotropicVmeEnvelope(self.geom, design_factor=1.0)
        aniso_env = AnisotropicVmeEnvelope(
            self.geom, yield_hoop_ratio=1.0, design_factor=1.0
        )
        iso_pts = iso_env.compute(n_points=100)
        aniso_pts = aniso_env.compute(n_points=100)
        assert iso_pts[:, 1].max() == pytest.approx(aniso_pts[:, 1].max(), rel=0.01)

    def test_yield_hoop_ratio_must_be_positive(self):
        with pytest.raises(ValueError, match="yield_hoop_ratio must be positive"):
            AnisotropicVmeEnvelope(self.geom, yield_hoop_ratio=0.0, design_factor=1.0)

    def test_all_points_finite(self):
        result = self.envelope.compute(n_points=80)
        assert np.all(np.isfinite(result))

    def test_higher_hoop_yield_expands_burst_side(self):
        env_105 = AnisotropicVmeEnvelope(self.geom, yield_hoop_ratio=1.05, design_factor=1.0)
        env_iso = AnisotropicVmeEnvelope(self.geom, yield_hoop_ratio=1.0, design_factor=1.0)
        burst_105 = env_105.compute(n_points=100)[:, 1].max()
        burst_iso = env_iso.compute(n_points=100)[:, 1].max()
        # Higher hoop yield means the burst envelope is expanded
        assert burst_105 > burst_iso


# ---------------------------------------------------------------------------
# design_envelope_points convenience function
# ---------------------------------------------------------------------------

class TestDesignEnvelopePoints:
    """Smoke tests for the top-level convenience function."""

    def test_returns_dict_with_required_keys(self):
        geom = make_7in_26ppf_l80()
        result = design_envelope_points(geom, design_factor=1.2, n_points=50)
        assert "vme_isotropic" in result
        assert "api_ellipse" in result

    def test_each_entry_has_axial_and_dp_arrays(self):
        geom = make_7in_26ppf_l80()
        result = design_envelope_points(geom, design_factor=1.2, n_points=50)
        for key, pts in result.items():
            assert pts.shape == (50, 2), f"{key}: unexpected shape {pts.shape}"

    def test_cra_grade_includes_anisotropic_envelope(self):
        geom = make_25cr_cra()
        result = design_envelope_points(
            geom, design_factor=1.0, n_points=50, yield_hoop_ratio=1.05
        )
        assert "vme_anisotropic" in result

    def test_isotropic_grade_no_anisotropic_key_by_default(self):
        geom = make_7in_26ppf_l80()
        result = design_envelope_points(geom, design_factor=1.0, n_points=50)
        assert "vme_anisotropic" not in result

    def test_design_factor_applied(self):
        geom = make_7in_26ppf_l80()
        result_df1 = design_envelope_points(geom, design_factor=1.0, n_points=50)
        result_df12 = design_envelope_points(geom, design_factor=1.2, n_points=50)
        # DF=1.2 envelope must be tighter than DF=1.0
        assert (
            result_df12["vme_isotropic"][:, 1].max()
            < result_df1["vme_isotropic"][:, 1].max()
        )


# ---------------------------------------------------------------------------
# Known engineering design cases (regression / validation)
# ---------------------------------------------------------------------------

class TestKnownDesignCases:
    """Regression tests against hand-calculated or reference design cases.

    Sources:
    - Bourgoyne et al., "Applied Drilling Engineering", SPE Textbook Vol. 2
    - API TR 5C3 Section 7 biaxial correction worked examples
    """

    def test_7in_l80_uniaxial_burst_capacity(self):
        """Pure burst (no axial): burst limit must equal rated burst / DF."""
        geom = make_7in_26ppf_l80()
        env = ApiEllipseEnvelope(geom, design_factor=1.0)
        pts = env.compute(n_points=400)
        axial, dp = pts[:, 0], pts[:, 1]
        # Find point closest to zero axial force
        idx_zero = np.argmin(np.abs(axial))
        burst_at_zero = dp[idx_zero]
        # Must be within 5% of rated burst
        assert burst_at_zero == pytest.approx(geom.burst_rating_psi, rel=0.05)

    def test_7in_l80_vme_pure_tension_at_yield(self):
        """At dp=0 on the tension side, axial force = yield force.

        The VME ellipse touches the axial axis at ±F_yield.  The envelope
        sweep puts the tension end (sa = +S_allow) in the second half of the
        array where the collapse branch is being traced, so we look for the
        maximum axial force among points where |dp| is small.
        """
        geom = make_7in_26ppf_l80()
        env = IsotropicVmeEnvelope(geom, design_factor=1.0)
        pts = env.compute(n_points=400)
        axial, dp = pts[:, 0], pts[:, 1]
        # At the tension tip of the VME ellipse, dp is near zero and axial
        # is at its positive maximum; use a tolerance band and take the max.
        near_zero_dp = np.abs(dp) < 200.0
        assert near_zero_dp.any(), "No points found near dp=0"
        max_tension = axial[near_zero_dp].max()
        expected_fy = geom.yield_force_lbf
        assert max_tension == pytest.approx(expected_fy, rel=0.02)

    def test_biaxial_burst_reduction_under_tension(self):
        """API biaxial: burst capacity is reduced under tensile axial load.

        At axial = 0.5 * SMYS * area, burst is reduced by the API biaxial factor.
        API TR 5C3 formula (correct form with square root):
            p_burst_axial = p_burst * (sqrt(1 - 0.75*(sa/Y)^2) - 0.5*(sa/Y))
        where sa = sigma_axial = F_axial / area, Y = SMYS.

        Note: the simplified form (1 - 0.75*r^2 - 0.5*r) is an approximation
        that is NOT used in the implementation; the sqrt form is the API standard.
        """
        geom = make_7in_26ppf_l80()
        area = geom.cross_section_area_in2
        f_axial = 0.5 * geom.smys_psi * area  # 50% of yield force (tension)
        sa_ratio = (f_axial / area) / geom.smys_psi  # = 0.5

        # API TR 5C3 biaxial factor: sqrt(1 - 0.75*r^2) - 0.5*r
        expected_factor = math.sqrt(1.0 - 0.75 * sa_ratio**2) - 0.5 * sa_ratio
        expected_burst = geom.burst_rating_psi * expected_factor

        env = ApiEllipseEnvelope(geom, design_factor=1.0)
        pts = env.compute(n_points=400)
        axial, dp = pts[:, 0], pts[:, 1]
        # Find points near this axial load level
        tol = 0.02 * abs(f_axial)
        mask = np.abs(axial - f_axial) < tol
        if mask.any():
            burst_at_tension = (
                dp[mask & (dp > 0)].max() if (mask & (dp > 0)).any() else 0
            )
            assert burst_at_tension == pytest.approx(expected_burst, rel=0.05)

    def test_vme_stress_at_design_limit(self):
        """At the VME envelope boundary, sigma_vme = SMYS / DF."""
        geom = make_7in_26ppf_l80()
        df = 1.1
        env = IsotropicVmeEnvelope(geom, design_factor=df)
        pts = env.compute(n_points=200)
        area = geom.cross_section_area_in2
        smys = geom.smys_psi
        allowable = smys / df

        for i in range(0, len(pts), 20):
            f_axial, dp = pts[i, 0], pts[i, 1]
            sigma_a = f_axial / area
            sigma_t = compute_hoop_stress(dp, geom.od_in, geom.id_in)
            vme = compute_vme_stress(sigma_a, sigma_t)
            assert vme == pytest.approx(allowable, rel=0.02), (
                f"Point {i}: F={f_axial:.0f} lbf, dp={dp:.0f} psi -> "
                f"VME={vme:.0f} != {allowable:.0f}"
            )

    def test_9625_n80_envelope_is_larger_than_7in(self):
        """9-5/8 casing has higher yield force than 7-in due to thicker wall."""
        geom_9625 = make_9625_47ppf_n80()
        geom_7in = make_7in_26ppf_l80()
        assert geom_9625.yield_force_lbf > geom_7in.yield_force_lbf

    def test_25cr_anisotropic_burst_higher_than_isotropic(self):
        """25Cr with Y_hoop=1.05*Y_axial expands the burst region of VME ellipse."""
        geom = make_25cr_cra()
        iso = IsotropicVmeEnvelope(geom, design_factor=1.0).compute(n_points=200)
        aniso = AnisotropicVmeEnvelope(
            geom, yield_hoop_ratio=1.05, design_factor=1.0
        ).compute(n_points=200)
        assert aniso[:, 1].max() > iso[:, 1].max()
