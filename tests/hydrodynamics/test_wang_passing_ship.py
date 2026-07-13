"""Validation tests for the Wang passing-ship force port.

Two legacy oracles, both in English units (ft, ft^2, slug/ft^3, ft/s ->
lbf, ft-lbf):

* **MathCAD case** — the legacy MathCAD worksheet ("PassShip FandM Deep",
  Wang formulation with the image-method finite-depth extension) saved
  with: moored vessel L1 = 658 ft / A1 = 3936.299 ft^2, passing vessel
  L2 = 902.2 ft / A2 = 7099.888 ft^2, rho = 1.9905 slug/ft^3,
  U = 5 ft/s, separation 221.8 ft, depth 47 ft.  Oracle values are the
  worksheet's saved evaluations: the abeam (zero-stagger) forces and
  201-point stagger sweeps of the finite-depth surge/sway/yaw at steps
  of 0.1*L1 (subsampled to 21 points here).
* **Workbook case** — a saved output table of the legacy VBA workbook
  (same math, adaptive Gauss-Lobatto quadrature): equal vessels
  L = 941.10892 ft / A = 8103.87905 ft^2, rho = 1.9905 slug/ft^3,
  U = 6 ft/s, separation 364.042 ft, and both infinite depth and a
  finite depth of 55 ft.  Stored values carry ~4 significant figures,
  so the comparison tolerance is 1% (far-field entries have only 3
  significant figures).

Neither case carries any project or vessel identification; the inputs
are plain ship particulars.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.wang_passing_ship import (
    WangVessel,
    wang_forces_scalar,
    wang_passing_forces,
    wang_surge,
    wang_sway,
    wang_yaw,
)

# ----------------------------------------------------------------------
# MathCAD case definition and oracle values
# ----------------------------------------------------------------------

MC_MOORED = WangVessel(length=658.0, midship_area=3936.299)
MC_PASSING = WangVessel(length=902.2, midship_area=7099.888)
MC_RHO = 1.9905
MC_U = 5.0
MC_SEP = 221.8
MC_DEPTH = 47.0

# Saved worksheet evaluations at zero stagger, finite depth.
MC_SWAY_ABEAM = 48031.032627547444  # lbf
MC_SURGE_ABEAM = 8.0100221470303439e-12  # lbf (zero by symmetry)
MC_YAW_ABEAM = 2.5742338597266329e-09  # ft-lbf (zero by symmetry)

# Saved worksheet evaluation of the non-dimensional deep-water sway at
# zero stagger and separation 0.25*L1:
#   Wang_Sway(0, 0.25 L1) / (rho U^2 L1^-2 A1 A2) = 5.2715317599935663
MC_NONDIM_SWAY = 5.2715317599935663

# 21-point subsample (every 10th point) of the worksheet's 201-point
# finite-depth stagger sweeps; stagger = index * 0.1 * L1, index -100..100.
MC_SWEEP_STEPS = list(range(-100, 101, 10))
MC_SWEEP_STAGGER = [k * 0.1 * MC_MOORED.length for k in MC_SWEEP_STEPS]
MC_SWEEP_SURGE = [
    3.814412959239269, 5.774886646033302, 9.163973441950038,
    15.42147612537051, 27.977644450821114, 56.04420741891625,
    128.50322275823476, 356.3287670717948, 1288.1730811187028,
    -2482.6923324714444, 1.894061620110941e-09, 2482.692332469563,
    -1288.173081118602, -356.32876707177275, -128.5032227582285,
    -56.044207418914056, -27.977644450820208, -15.42147612537008,
    -9.163973441949825, -5.774886646033187, -3.814412959239199,
]
MC_SWEEP_SWAY = [
    -0.5236445322708142, -0.8845948086511841, -1.5885577818496084,
    -3.0816352611973237, -6.609754468861608, -16.242948499093107,
    -48.48391602340566, -195.68577807058824, -1368.0494890297105,
    -21376.96611644932, 48031.032627547436, -21376.96611644804,
    -1368.0494890295297, -195.685778070571, -48.48391602340245,
    -16.242948499092254, -6.609754468861323, -3.0816352611972104,
    -1.588557781849558, -0.8845948086511597, -0.5236445322708012,
]
MC_SWEEP_YAW = [
    8.55212193313188, 16.025694998287108, 32.30176256879212,
    71.37609076279183, 177.7145243693964, 519.8685228816719,
    1913.23274279341, 10045.784148425715, 101453.16684896465,
    1264388.3921437322, 3.968398638122865e-07, -1264388.3921440362,
    -101453.16684894865, -10045.784148424664, -1913.2327427932582,
    -519.8685228816404, -177.71452436938716, -71.37609076278879,
    -32.30176256879107, -16.02569499828651, -8.552121933131561,
]

# ----------------------------------------------------------------------
# Workbook case definition and oracle table
# ----------------------------------------------------------------------

WB_VESSEL = WangVessel(length=941.10892388451441, midship_area=8103.8790549803316)
WB_RHO = 1.9905
WB_U = 6.0
WB_SEP = 364.04199475065616
WB_DEPTH = 55.0

# (stagger_ft, surge_inf, sway_inf, yaw_inf, surge_fin, sway_fin, yaw_fin)
# 21-row subsample (every 10th row) of the legacy workbook's 201-row
# output table (values stored at ~4 significant figures).
WB_TABLE = [
    (-9411.09, 0.226, -0.035, 0.829, 4.63, -0.726, 16.964),
    (-8469.981, 0.345, -0.06, 1.564, 7.02, -1.227, 31.801),
    (-7528.872, 0.553, -0.108, 3.182, 11.161, -2.204, 64.131),
    (-6587.763, 0.944, -0.212, 7.123, 18.835, -4.277, 141.808),
    (-5646.654, 1.752, -0.461, 18.096, 34.317, -9.18, 353.412),
    (-4705.545, 3.642, -1.159, 54.698, 69.217, -22.584, 1035.0),
    (-3764.436, 8.933, -3.613, 213.359, 160.576, -67.494, 3813.0),
    (-2823.327, 28.462, -15.927, 1257.0, 454.545, -272.091, 19910.0),
    (-1882.218, 144.744, -136.815, 16170.0, 1683.0, -1838.0, 188400.0),
    (-941.109, 653.727, -4487.0, 701600.0, 261.57, -28340.0, 3358000.0),
    (0.0, 0.0, 15020.0, 0.0, 0.0, 87640.0, 0.0),
    (941.109, -653.727, -4487.0, -701600.0, -261.57, -28340.0, -3358000.0),
    (1882.218, -144.744, -136.815, -16170.0, -1683.0, -1838.0, -188400.0),
    (2823.327, -28.462, -15.927, -1257.0, -454.545, -272.091, -19910.0),
    (3764.436, -8.933, -3.613, -213.359, -160.576, -67.494, -3813.0),
    (4705.545, -3.642, -1.159, -54.698, -69.217, -22.584, -1035.0),
    (5646.654, -1.752, -0.461, -18.096, -34.317, -9.18, -353.412),
    (6587.763, -0.944, -0.212, -7.123, -18.835, -4.277, -141.808),
    (7528.872, -0.553, -0.108, -3.182, -11.161, -2.204, -64.131),
    (8469.981, -0.345, -0.06, -1.564, -7.02, -1.227, -31.801),
    (9411.09, -0.226, -0.035, -0.829, -4.63, -0.726, -16.964),
]


def _mc_kwargs(**overrides):
    kwargs = dict(
        separation=MC_SEP,
        moored=MC_MOORED,
        passing=MC_PASSING,
        velocity=MC_U,
        density=MC_RHO,
        depth=MC_DEPTH,
    )
    kwargs.update(overrides)
    return kwargs


class TestMathcadCase:
    """Oracle: saved evaluations of the legacy MathCAD worksheet."""

    def test_abeam_sway_vectorized(self):
        sway = wang_sway(0.0, **_mc_kwargs())
        assert sway == pytest.approx(MC_SWAY_ABEAM, rel=1e-8)

    def test_abeam_sway_scalar_reference(self):
        _, sway, _ = wang_forces_scalar(0.0, **_mc_kwargs())
        # legacy adaptive quadrature tolerance is 1e-6 per panel
        assert sway == pytest.approx(MC_SWAY_ABEAM, rel=1e-6)

    def test_abeam_surge_and_yaw_vanish(self):
        surge = wang_surge(0.0, **_mc_kwargs())
        yaw = wang_yaw(0.0, **_mc_kwargs())
        # zero by symmetry at zero stagger (MathCAD residuals ~1e-11)
        assert abs(surge) < 1e-6 * MC_SWAY_ABEAM
        assert abs(yaw) < 1e-6 * MC_SWAY_ABEAM * MC_MOORED.length

    def test_nondimensional_deep_sway(self):
        sway = wang_sway(
            0.0, separation=0.25 * MC_MOORED.length, moored=MC_MOORED,
            passing=MC_PASSING, velocity=MC_U, density=MC_RHO, depth=None,
        )
        nondim = sway / (
            MC_RHO * MC_U**2 * MC_MOORED.length**-2
            * MC_MOORED.midship_area * MC_PASSING.midship_area
        )
        assert nondim == pytest.approx(MC_NONDIM_SWAY, rel=1e-6)

    @pytest.mark.parametrize(
        "component, oracle",
        [
            ("surge", MC_SWEEP_SURGE),
            ("sway", MC_SWEEP_SWAY),
            ("yaw", MC_SWEEP_YAW),
        ],
    )
    def test_finite_depth_stagger_sweep(self, component, oracle):
        result = wang_passing_forces(MC_SWEEP_STAGGER, **_mc_kwargs())
        values = getattr(result, component)
        scale = max(abs(v) for v in oracle)
        for value, expected in zip(values, oracle):
            if abs(expected) < 1e-6 * scale:
                # symmetry zeros (stored as ~1e-9 residuals)
                assert abs(value) < 1e-8 * scale
            else:
                assert value == pytest.approx(expected, rel=1e-6)


class TestWorkbookCase:
    """Oracle: saved 201-row output table of the legacy VBA workbook."""

    @pytest.fixture(scope="class")
    def computed(self):
        stagger = [row[0] for row in WB_TABLE]
        common = dict(
            separation=WB_SEP, moored=WB_VESSEL, passing=WB_VESSEL,
            velocity=WB_U, density=WB_RHO,
        )
        infinite = wang_passing_forces(stagger, depth=None, **common)
        finite = wang_passing_forces(stagger, depth=WB_DEPTH, **common)
        return infinite, finite

    @pytest.mark.parametrize(
        "column, depth_mode, component",
        [
            (1, "infinite", "surge"),
            (2, "infinite", "sway"),
            (3, "infinite", "yaw"),
            (4, "finite", "surge"),
            (5, "finite", "sway"),
            (6, "finite", "yaw"),
        ],
    )
    def test_table_column(self, computed, column, depth_mode, component):
        infinite, finite = computed
        result = infinite if depth_mode == "infinite" else finite
        values = getattr(result, component)
        oracle = [row[column] for row in WB_TABLE]
        scale = max(abs(v) for v in oracle)
        for value, expected in zip(values, oracle):
            if expected == 0.0:
                assert abs(value) < 1e-9 * scale
            else:
                # stored at ~4 (far field ~3) significant figures
                assert value == pytest.approx(expected, rel=1e-2)


class TestNumericalConsistency:
    """Vectorised production path vs the legacy-quadrature scalar path."""

    @pytest.mark.parametrize("stagger", [-1500.0, -400.0, 250.0, 900.0])
    def test_vectorized_matches_scalar_deep(self, stagger):
        surge, sway, yaw = wang_forces_scalar(stagger, **_mc_kwargs(depth=None))
        assert wang_surge(stagger, **_mc_kwargs(depth=None)) == pytest.approx(surge, rel=1e-6)
        assert wang_sway(stagger, **_mc_kwargs(depth=None)) == pytest.approx(sway, rel=1e-6)
        assert wang_yaw(stagger, **_mc_kwargs(depth=None)) == pytest.approx(yaw, rel=1e-6)

    def test_vectorized_matches_scalar_finite_depth(self):
        stagger = -592.2
        surge, sway, yaw = wang_forces_scalar(stagger, **_mc_kwargs())
        assert wang_surge(stagger, **_mc_kwargs()) == pytest.approx(surge, rel=1e-6)
        assert wang_sway(stagger, **_mc_kwargs()) == pytest.approx(sway, rel=1e-6)
        assert wang_yaw(stagger, **_mc_kwargs()) == pytest.approx(yaw, rel=1e-6)

    def test_deep_water_limit_of_image_sum(self):
        """With a huge depth the image terms vanish; only n = 0 remains."""
        stagger = 300.0
        deep = wang_passing_forces([stagger], **_mc_kwargs(depth=None))
        limit = wang_passing_forces([stagger], **_mc_kwargs(depth=1.0e6))
        assert limit.surge[0] == pytest.approx(deep.surge[0], rel=1e-9)
        assert limit.sway[0] == pytest.approx(deep.sway[0], rel=1e-9)
        assert limit.yaw[0] == pytest.approx(deep.yaw[0], rel=1e-9)

    def test_velocity_squared_scaling(self):
        base = wang_sway(200.0, **_mc_kwargs())
        doubled = wang_sway(200.0, **_mc_kwargs(velocity=2.0 * MC_U))
        assert doubled == pytest.approx(4.0 * base, rel=1e-12)

    def test_symmetry_in_stagger(self):
        """Surge and yaw are odd, sway even, for parabolic hulls."""
        sweep = np.array([-800.0, -300.0, 300.0, 800.0])
        result = wang_passing_forces(sweep, **_mc_kwargs())
        np.testing.assert_allclose(result.surge[:2], -result.surge[:1:-1], rtol=1e-9)
        np.testing.assert_allclose(result.sway[:2], result.sway[:1:-1], rtol=1e-9)
        np.testing.assert_allclose(result.yaw[:2], -result.yaw[:1:-1], rtol=1e-9)

    def test_attraction_at_abeam_repulsion_in_far_field(self):
        result = wang_passing_forces(
            [0.0, 5.0 * MC_MOORED.length], **_mc_kwargs()
        )
        assert result.sway[0] > 0.0  # attraction toward passing ship abeam
        assert result.sway[1] < 0.0  # repulsion in the far field

    def test_scalar_input_returns_float(self):
        value = wang_sway(0.0, **_mc_kwargs())
        assert isinstance(value, float)

    def test_array_input_returns_array(self):
        values = wang_sway([0.0, 100.0], **_mc_kwargs())
        assert isinstance(values, np.ndarray)
        assert values.shape == (2,)


class TestValidation:
    def test_vessel_requires_positive_length(self):
        with pytest.raises(ValueError):
            WangVessel(length=0.0, midship_area=100.0)

    def test_vessel_requires_positive_area(self):
        with pytest.raises(ValueError):
            WangVessel(length=100.0, midship_area=-1.0)

    def test_separation_must_be_positive(self):
        with pytest.raises(ValueError):
            wang_sway(0.0, **_mc_kwargs(separation=0.0))

    def test_depth_must_be_positive_when_given(self):
        with pytest.raises(ValueError):
            wang_sway(0.0, **_mc_kwargs(depth=-10.0))
