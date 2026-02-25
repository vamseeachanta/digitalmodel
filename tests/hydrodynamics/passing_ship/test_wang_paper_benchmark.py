"""
Wang Paper Benchmark Tests for Passing Ship Force Calculator.

Validates the Python implementation against reference calculations from:
- Wang, S. (1975). "Hydrodynamic Forces on a Vessel Moving Along a Bank
  or in a Narrow Channel." Journal of Ship Research, Vol. 19, No. 4.
- MathCAD PDF: "Calculation of forces and moments from Wang.pdf"
- MathCAD PDF: "Wang Paper calculations 3.pdf"
- VBA reference: modPassingShip.bas (Nick Barczak, July 2016)

Unit convention:
- Reference data is in Imperial (ft, slug/ft³, lbf, ft-lbf)
- Calculator uses SI (m, kg/m³, N, N·m)
- Tests convert at boundaries

See CONVENTIONS.md for sign convention and coordinate system documentation.
"""

import math
import numpy as np
import pytest
from assetutilities.units import TrackedQuantity
from digitalmodel.hydrodynamics.passing_ship.calculator import (
    PassingShipCalculator,
)
from digitalmodel.hydrodynamics.passing_ship.configuration import (
    VesselConfig,
    EnvironmentalConfig,
    CalculationConfig,
)
from digitalmodel.hydrodynamics.passing_ship.formulations import (
    s1_function,
    ds1_dx,
    f_kernel,
    g_kernel,
    calculate_surge_force_infinite,
    calculate_sway_force_infinite,
    calculate_yaw_moment_infinite,
    finite_depth_correction,
)


def _tq(value, unit, source='MathCAD_Doc1'):
    """Create TrackedQuantity with standard provenance source."""
    return TrackedQuantity(value, unit, source=source)


# ═══════════════════════════════════════════════════════════════════════════
# Reference Data — MathCAD Document 1 (L2=475 ft, U=11.2 ft/s)
# Source: "Calculation of forces and moments from Wang.pdf"
# ═══════════════════════════════════════════════════════════════════════════

# Vessel parameters (Imperial)
REF1_L1_FT = 950.0          # Moored vessel LBP [ft]
REF1_A1_FT2 = 3192.0        # Moored vessel midship area [ft²]
REF1_L2_FT = 475.0          # Passing vessel LBP [ft]
REF1_A2_FT2 = 6413.0        # Passing vessel midship area [ft²]
REF1_RHO_SLUG = 1.9905      # Water density [slug/ft³]
REF1_U_FT = 11.2            # Passing ship velocity [ft/s]
REF1_SEP_FT = 190.0         # Lateral separation [ft] (0.2*L)
REF1_DEPTH_FT = 95.0        # Water depth [ft] (0.1*L)

# Convert to SI using TrackedQuantity (provenance-tracked conversions)
REF1_L1_M = _tq(REF1_L1_FT, 'ft').to('m').magnitude
REF1_L2_M = _tq(REF1_L2_FT, 'ft').to('m').magnitude
REF1_U_MS = _tq(REF1_U_FT, 'ft/s').to('m/s').magnitude
REF1_SEP_M = _tq(REF1_SEP_FT, 'ft').to('m').magnitude
REF1_DEPTH_M = _tq(REF1_DEPTH_FT, 'ft').to('m').magnitude
REF1_RHO_KG = _tq(REF1_RHO_SLUG, 'slug/ft**3').to('kg/m**3').magnitude

# Back-compute beam, draft, Cb from known area and reasonable hull proportions
# A1 = B1 * T1 * Cb1 = 3192 ft² → For a 950ft vessel: B~105ft, T~38ft, Cb~0.8
# These are approximate — the Wang formulation uses area directly, not B*T*Cb
REF1_B1_FT = 105.0
REF1_T1_FT = 38.0
REF1_CB1 = REF1_A1_FT2 / (REF1_B1_FT * REF1_T1_FT)

# For passing vessel: A2=6413, need B*T*Cb = 6413 with Cb <= 1.0
REF1_B2_FT = 100.0
REF1_T2_FT = 70.0
REF1_CB2 = REF1_A2_FT2 / (REF1_B2_FT * REF1_T2_FT)  # ≈ 0.916

# Beam/draft SI conversions
REF1_B1_M = _tq(REF1_B1_FT, 'ft').to('m').magnitude
REF1_T1_M = _tq(REF1_T1_FT, 'ft').to('m').magnitude
REF1_B2_M = _tq(REF1_B2_FT, 'ft').to('m').magnitude
REF1_T2_M = _tq(REF1_T2_FT, 'ft').to('m').magnitude

# Expected results (Imperial) — from MathCAD Document 1, finite depth (h=95ft)
# At stagger = 0 (abeam), surge and yaw are zero by antisymmetry
REF1_SWAY_LBF = 76440.0     # Source: MathCAD p.1, SwayForce = 7.644e+4 lbf
REF1_SURGE_LBF = 0.0        # Source: MathCAD p.1, SurgeForce = 1.016e-11 lbf
REF1_YAW_FT_LBF = 0.0       # Source: MathCAD p.1, YawMoment = 7.851e-9 ft-lbf


# ═══════════════════════════════════════════════════════════════════════════
# Fixtures
# ═══════════════════════════════════════════════════════════════════════════

@pytest.fixture
def ref1_calculator():
    """Calculator configured with MathCAD Document 1 parameters."""
    moored = VesselConfig(
        length=REF1_L1_M,
        beam=REF1_B1_M,
        draft=REF1_T1_M,
        block_coefficient=REF1_CB1,
    )
    passing = VesselConfig(
        length=REF1_L2_M,
        beam=REF1_B2_M,
        draft=REF1_T2_M,
        block_coefficient=REF1_CB2,
    )
    env = EnvironmentalConfig(
        water_depth=REF1_DEPTH_M,
        water_density=REF1_RHO_KG,
    )
    calc = CalculationConfig(
        lateral_separation=REF1_SEP_M,
        passing_velocity=REF1_U_MS,
        stagger_distance=0.0,
    )
    return PassingShipCalculator(
        moored_vessel=moored,
        passing_vessel=passing,
        environment=env,
        calculation_config=calc,
    )


# ═══════════════════════════════════════════════════════════════════════════
# Category A: Full 3-DOF Reference Case (MathCAD Document 1)
# ═══════════════════════════════════════════════════════════════════════════

class TestWangReferenceCase:
    """Validate against the primary MathCAD reference case."""

    def test_sway_at_abeam(self, ref1_calculator):
        """Sway force at stagger=0 should match MathCAD: 76,440 lbf.

        Source: MathCAD Doc 1, SwayForce = 7.644e+4 lbf
        """
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=0.0,
            velocity=REF1_U_MS,
        )
        sway_N = result["sway"]
        sway_lbf = _tq(sway_N, 'N').to('lbf').magnitude

        # Allow 5% tolerance — Python formulation uses simplified kernel
        # compared to VBA's exact Wang formulation
        assert abs(sway_lbf - REF1_SWAY_LBF) / REF1_SWAY_LBF < 0.05, (
            f"Sway force mismatch: got {sway_lbf:.1f} lbf, "
            f"expected {REF1_SWAY_LBF:.1f} lbf"
        )

    def test_surge_near_zero_at_abeam(self, ref1_calculator):
        """Surge force at stagger=0 should be ~0 (antisymmetry).

        Source: MathCAD Doc 1, SurgeForce = 1.016e-11 lbf
        """
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=0.0,
            velocity=REF1_U_MS,
        )
        surge_N = result["surge"]
        surge_lbf = abs(_tq(surge_N, 'N').to('lbf').magnitude)

        # At stagger=0, surge must be negligible relative to sway
        assert surge_lbf < REF1_SWAY_LBF * 0.01, (
            f"Surge force at abeam should be ~0, got {surge_lbf:.3f} lbf"
        )

    def test_yaw_near_zero_at_abeam(self, ref1_calculator):
        """Yaw moment at stagger=0 should be ~0 (antisymmetry).

        Source: MathCAD Doc 1, YawMoment = 7.851e-9 ft-lbf
        """
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=0.0,
            velocity=REF1_U_MS,
        )
        yaw_Nm = result["yaw"]
        yaw_ft_lbf = abs(_tq(yaw_Nm, 'N * m').to('ft * lbf').magnitude)

        # At stagger=0, yaw must be negligible
        sway_N = abs(result["sway"])
        ref_moment = sway_N * REF1_L1_M  # rough scale
        assert yaw_ft_lbf < _tq(ref_moment * 0.01, 'N * m').to('ft * lbf').magnitude, (
            f"Yaw moment at abeam should be ~0, got {yaw_ft_lbf:.3f} ft-lbf"
        )


# ═══════════════════════════════════════════════════════════════════════════
# Category B: Velocity Squared Scaling (Analytical)
# ═══════════════════════════════════════════════════════════════════════════

class TestVelocityScaling:
    """Forces must scale with U² — this is a fundamental property."""

    def test_double_velocity_quadruples_sway(self, ref1_calculator):
        """Doubling velocity should quadruple forces (U² relationship)."""
        U1 = REF1_U_MS
        U2 = 2.0 * REF1_U_MS

        result_1 = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=U1
        )
        result_2 = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=U2
        )

        ratio = result_2["sway"] / result_1["sway"]
        assert abs(ratio - 4.0) < 0.1, (
            f"U² scaling failed: ratio = {ratio:.3f}, expected 4.0"
        )

    def test_half_velocity_quarters_sway(self, ref1_calculator):
        """Halving velocity should quarter forces."""
        U1 = REF1_U_MS
        U_half = 0.5 * REF1_U_MS

        result_full = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=U1
        )
        result_half = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=U_half
        )

        ratio = result_full["sway"] / result_half["sway"]
        assert abs(ratio - 4.0) < 0.1, (
            f"U² scaling failed: ratio = {ratio:.3f}, expected 4.0"
        )


# ═══════════════════════════════════════════════════════════════════════════
# Category C: Separation Distance Sensitivity
# ═══════════════════════════════════════════════════════════════════════════

class TestSeparationSensitivity:
    """Forces should decrease with increasing lateral separation."""

    def test_force_decreases_with_separation(self, ref1_calculator):
        """Sway force at 2x separation < force at 1x separation."""
        result_close = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )
        result_far = ref1_calculator.calculate_forces(
            separation=2.0 * REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )

        assert abs(result_close["sway"]) > abs(result_far["sway"]), (
            "Sway force should decrease with increasing separation"
        )

    def test_force_ratio_with_separation(self, ref1_calculator):
        """Check that force drops significantly (>50%) at double separation."""
        result_close = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )
        result_far = ref1_calculator.calculate_forces(
            separation=2.0 * REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )

        ratio = abs(result_far["sway"]) / abs(result_close["sway"])
        assert ratio < 0.50, (
            f"Force should drop >50% at double separation, ratio = {ratio:.3f}"
        )


# ═══════════════════════════════════════════════════════════════════════════
# Category D: Stagger Position Effects (Force Profile Shape)
# ═══════════════════════════════════════════════════════════════════════════

class TestStaggerProfile:
    """Test force behavior at different stagger positions.

    At stagger=0 (abeam): sway is max, surge and yaw are zero.
    At stagger=±L/4: surge and yaw should be non-zero.
    Force profile should be continuous and approach zero at large stagger.
    """

    def test_surge_nonzero_at_offset_stagger(self, ref1_calculator):
        """Surge force should be non-zero away from stagger=0."""
        stagger_offset = REF1_L1_M * 0.25  # L/4
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=stagger_offset,
            velocity=REF1_U_MS,
        )
        assert abs(result["surge"]) > 0.1, (
            "Surge force should be non-zero at stagger = L/4"
        )

    def test_yaw_nonzero_at_offset_stagger(self, ref1_calculator):
        """Yaw moment should be non-zero away from stagger=0."""
        stagger_offset = REF1_L1_M * 0.25
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=stagger_offset,
            velocity=REF1_U_MS,
        )
        assert abs(result["yaw"]) > 0.1, (
            "Yaw moment should be non-zero at stagger = L/4"
        )

    def test_surge_antisymmetric(self, ref1_calculator):
        """Surge force should be antisymmetric: F(+ξ) ≈ -F(-ξ)."""
        stagger = REF1_L1_M * 0.25
        result_pos = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=+stagger, velocity=REF1_U_MS
        )
        result_neg = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=-stagger, velocity=REF1_U_MS
        )
        surge_sum = result_pos["surge"] + result_neg["surge"]
        surge_max = max(abs(result_pos["surge"]), abs(result_neg["surge"]))

        if surge_max > 1.0:  # avoid division by near-zero
            assert abs(surge_sum) / surge_max < 0.05, (
                f"Surge not antisymmetric: F(+ξ)={result_pos['surge']:.1f}, "
                f"F(-ξ)={result_neg['surge']:.1f}"
            )

    def test_sway_symmetric(self, ref1_calculator):
        """Sway force should be symmetric: F(+ξ) ≈ F(-ξ)."""
        stagger = REF1_L1_M * 0.25
        result_pos = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=+stagger, velocity=REF1_U_MS
        )
        result_neg = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=-stagger, velocity=REF1_U_MS
        )
        sway_diff = abs(result_pos["sway"] - result_neg["sway"])
        sway_avg = (abs(result_pos["sway"]) + abs(result_neg["sway"])) / 2

        if sway_avg > 1.0:
            assert sway_diff / sway_avg < 0.05, (
                f"Sway not symmetric: F(+ξ)={result_pos['sway']:.1f}, "
                f"F(-ξ)={result_neg['sway']:.1f}"
            )

    def test_force_decays_at_large_stagger(self, ref1_calculator):
        """Forces should approach zero at large stagger (passing ship far away)."""
        stagger_far = REF1_L1_M * 2.0  # 2L away
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=stagger_far, velocity=REF1_U_MS
        )
        result_abeam = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )

        sway_ratio = abs(result["sway"]) / abs(result_abeam["sway"])
        assert sway_ratio < 0.10, (
            f"Sway should be <10% of peak at stagger=2L, got {sway_ratio:.3f}"
        )

    def test_sway_peaks_near_abeam(self, ref1_calculator):
        """Sway force should be maximum near stagger=0."""
        stagger_positions = [
            -REF1_L1_M * 0.5,
            -REF1_L1_M * 0.25,
            0.0,
            REF1_L1_M * 0.25,
            REF1_L1_M * 0.5,
        ]
        sways = []
        for stagger in stagger_positions:
            result = ref1_calculator.calculate_forces(
                separation=REF1_SEP_M, stagger=stagger, velocity=REF1_U_MS
            )
            sways.append(abs(result["sway"]))

        # Sway at stagger=0 should be the largest
        max_idx = np.argmax(sways)
        assert max_idx == 2, (
            f"Sway peak should be at stagger=0, but max is at index {max_idx} "
            f"(stagger={stagger_positions[max_idx]/REF1_L1_M:.2f}L)"
        )


# ═══════════════════════════════════════════════════════════════════════════
# Category E: Stagger Sweep — Force Profile Continuity
# ═══════════════════════════════════════════════════════════════════════════

class TestStaggerSweep:
    """Test force profile over a full stagger sweep (-L to +L)."""

    @pytest.fixture
    def stagger_sweep_results(self, ref1_calculator):
        """Compute forces at 21 stagger positions."""
        positions = np.linspace(-REF1_L1_M, REF1_L1_M, 21)
        results = []
        for stagger in positions:
            result = ref1_calculator.calculate_forces(
                separation=REF1_SEP_M,
                stagger=float(stagger),
                velocity=REF1_U_MS,
            )
            results.append(result)
        return positions, results

    def test_sweep_no_nans(self, stagger_sweep_results):
        """No NaN or Inf values in sweep results."""
        positions, results = stagger_sweep_results
        for i, result in enumerate(results):
            for key in ["surge", "sway", "yaw"]:
                assert math.isfinite(result[key]), (
                    f"{key} is not finite at stagger={positions[i]:.1f}m"
                )

    def test_sweep_continuous(self, stagger_sweep_results):
        """Forces should vary smoothly (no abrupt jumps)."""
        positions, results = stagger_sweep_results
        for key in ["surge", "sway", "yaw"]:
            values = [r[key] for r in results]
            for i in range(1, len(values)):
                step_change = abs(values[i] - values[i - 1])
                max_val = max(abs(v) for v in values) or 1.0
                relative_jump = step_change / max_val
                assert relative_jump < 0.70, (
                    f"Discontinuity in {key} between stagger "
                    f"{positions[i-1]:.0f}m and {positions[i]:.0f}m: "
                    f"jump = {relative_jump:.2%}"
                )


# ═══════════════════════════════════════════════════════════════════════════
# Category F: Depth Effects
# ═══════════════════════════════════════════════════════════════════════════

class TestDepthEffects:
    """Test water depth influence on forces."""

    def test_shallow_amplifies_forces(self, ref1_calculator):
        """Shallow water should produce larger forces than deep water."""
        # Shallow case (existing: h=95ft)
        result_shallow = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )

        # Deep water case — create new calculator with large depth
        moored = ref1_calculator.moored_vessel
        passing = ref1_calculator.passing_vessel
        env_deep = EnvironmentalConfig(
            water_depth=REF1_L1_M * 5.0,  # 5L — effectively infinite
            water_density=REF1_RHO_KG,
        )
        deep_calc = PassingShipCalculator(
            moored_vessel=moored,
            passing_vessel=passing,
            environment=env_deep,
        )
        result_deep = deep_calc.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )

        # Shallow water sway should be >= deep water sway
        assert abs(result_shallow["sway"]) >= abs(result_deep["sway"]) * 0.95, (
            f"Shallow water ({REF1_DEPTH_FT:.0f}ft) sway "
            f"({abs(result_shallow['sway']):.0f}N) should be >= "
            f"deep water sway ({abs(result_deep['sway']):.0f}N)"
        )


# ═══════════════════════════════════════════════════════════════════════════
# Category G: Sectional Area Functions (Unit-Level)
# ═══════════════════════════════════════════════════════════════════════════

class TestSectionalAreaFunctions:
    """Validate the parabolic sectional area distribution."""

    def test_s1_at_midship_is_one(self):
        """S1(0) = 1 (maximum at midship)."""
        assert s1_function(0.0, 100.0) == pytest.approx(1.0)

    def test_s1_at_ends_is_zero(self):
        """S1(±L/2) = 0."""
        L = 100.0
        assert s1_function(L / 2, L) == pytest.approx(0.0)
        assert s1_function(-L / 2, L) == pytest.approx(0.0)

    def test_s1_outside_ship_is_zero(self):
        """S1(x) = 0 for |x| > L/2."""
        assert s1_function(60.0, 100.0) == 0.0
        assert s1_function(-60.0, 100.0) == 0.0

    def test_s1_symmetric(self):
        """S1(x) = S1(-x) — symmetric about midship."""
        L = 100.0
        for x in [10.0, 25.0, 40.0, 49.0]:
            assert s1_function(x, L) == pytest.approx(s1_function(-x, L))

    def test_ds1_at_midship_is_zero(self):
        """dS1/dx(0) = 0 (flat at peak)."""
        assert ds1_dx(0.0, 100.0) == pytest.approx(0.0)

    def test_ds1_negative_forward(self):
        """dS1/dx > 0 at negative x (increasing toward midship from stern)."""
        # At x = -25 (quarter-length aft), slope should be positive
        assert ds1_dx(-25.0, 100.0) > 0.0

    def test_ds1_negative_aft(self):
        """dS1/dx < 0 at positive x (decreasing from midship toward bow)."""
        assert ds1_dx(25.0, 100.0) < 0.0

    def test_ds1_analytical_value(self):
        """dS1/dx = -8x/L² — verify against analytical formula.

        Source: Wang paper, S1(x) = 1 - (2x/L)², dS1/dx = -8x/L²
        """
        L = 100.0
        for x in [-25.0, 0.0, 10.0, 40.0]:
            expected = -8.0 * x / L**2
            assert ds1_dx(x, L) == pytest.approx(expected, abs=1e-10)


# ═══════════════════════════════════════════════════════════════════════════
# Category H: Edge Cases and Numerical Robustness
# ═══════════════════════════════════════════════════════════════════════════

class TestEdgeCases:
    """Test numerical behavior at extreme parameters."""

    def test_zero_velocity_gives_zero_force(self, ref1_calculator):
        """F = 0 when U = 0 (no interaction without relative motion)."""
        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=0.0
        )
        assert result["sway"] == pytest.approx(0.0, abs=1e-6)
        assert result["surge"] == pytest.approx(0.0, abs=1e-6)
        assert result["yaw"] == pytest.approx(0.0, abs=1e-6)

    def test_very_large_separation_gives_small_force(self, ref1_calculator):
        """Forces should be negligible at very large separation (10L)."""
        result = ref1_calculator.calculate_forces(
            separation=REF1_L1_M * 10.0,
            stagger=0.0,
            velocity=REF1_U_MS,
        )
        # At 10L separation, force should be < 1% of reference
        ref_result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M, stagger=0.0, velocity=REF1_U_MS
        )
        ratio = abs(result["sway"]) / abs(ref_result["sway"])
        assert ratio < 0.01, f"Force at 10L separation should be < 1%, got {ratio:.4f}"

    def test_finite_result_at_all_positions(self, ref1_calculator):
        """Calculator should never return NaN/Inf for reasonable inputs."""
        test_cases = [
            (REF1_SEP_M, 0.0),
            (REF1_SEP_M, REF1_L1_M * 0.5),
            (REF1_SEP_M, -REF1_L1_M * 0.5),
            (REF1_SEP_M * 3.0, REF1_L1_M),
            (REF1_SEP_M * 0.5, 0.0),
        ]
        for sep, stagger in test_cases:
            result = ref1_calculator.calculate_forces(
                separation=sep, stagger=stagger, velocity=REF1_U_MS
            )
            for key in ["surge", "sway", "yaw"]:
                assert math.isfinite(result[key]), (
                    f"{key} not finite at sep={sep:.0f}m, stagger={stagger:.0f}m"
                )


# ═══════════════════════════════════════════════════════════════════════════
# Category I: VBA Cross-Validation (Numerical — values TBD)
# These tests have placeholder values to be filled from VBA execution.
# Run modPassingShip.bas to generate reference values.
# ═══════════════════════════════════════════════════════════════════════════

class TestVBACrossValidation:
    """Cross-validate Python against VBA at specific parameter sets.

    Reference values marked as TBD need to be extracted by running
    the VBA module (modPassingShip.bas) with the specified parameters.
    """

    @pytest.mark.skip(reason="VBA reference values not yet extracted — run modPassingShip.bas")
    @pytest.mark.parametrize("stagger_fraction,expected_surge_lbf,expected_sway_lbf,expected_yaw_ft_lbf", [
        # (stagger/L1, expected surge [lbf], expected sway [lbf], expected yaw [ft-lbf])
        (-0.50, None, None, None),  # TBD: VBA Wang_Surge(-475, 190), etc.
        (-0.25, None, None, None),  # TBD: VBA Wang_Surge(-237.5, 190), etc.
        ( 0.00,  0.0, 76440.0, 0.0),  # Known from MathCAD
        (+0.25, None, None, None),  # TBD
        (+0.50, None, None, None),  # TBD
    ])
    def test_vba_stagger_sweep(
        self, ref1_calculator,
        stagger_fraction, expected_surge_lbf, expected_sway_lbf, expected_yaw_ft_lbf
    ):
        """Compare Python vs VBA at discrete stagger positions."""
        stagger = stagger_fraction * REF1_L1_M

        result = ref1_calculator.calculate_forces(
            separation=REF1_SEP_M,
            stagger=stagger,
            velocity=REF1_U_MS,
        )

        if expected_surge_lbf is not None:
            surge_lbf = _tq(result["surge"], 'N').to('lbf').magnitude
            assert abs(surge_lbf - expected_surge_lbf) / max(abs(expected_surge_lbf), 1.0) < 0.05

        if expected_sway_lbf is not None:
            sway_lbf = _tq(result["sway"], 'N').to('lbf').magnitude
            assert abs(sway_lbf - expected_sway_lbf) / max(abs(expected_sway_lbf), 1.0) < 0.05

        if expected_yaw_ft_lbf is not None:
            yaw_ft_lbf = _tq(result["yaw"], 'N * m').to('ft * lbf').magnitude
            assert abs(yaw_ft_lbf - expected_yaw_ft_lbf) / max(abs(expected_yaw_ft_lbf), 1.0) < 0.05
