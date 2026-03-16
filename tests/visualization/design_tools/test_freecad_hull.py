"""
ABOUTME: Tests for parametric hull NURBS generation and hydrostatics.
Validates FreeCADHullGenerator and HullHydrostatics against analytical
reference values for L=100, B=20, T=8, Cb=0.7.
"""

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.visualization.design_tools.hull_hydrostatics import (
    HullHydrostatics,
)
from digitalmodel.visualization.design_tools.freecad_hull import (
    FREECAD_AVAILABLE,
    FreeCADHullGenerator,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def _make_station(x: float, half_beam: float, draft: float, depth: float,
                  n_points: int = 11, section_power: float = 0.1,
                  ) -> HullStation:
    """Create a station with power-law waterline offsets.

    The half-breadth varies as y(z) = half_beam * (z / draft) ** section_power.
    A small exponent (e.g. 0.1) gives a very full (nearly rectangular)
    section so the section fullness (integral / rectangle) is close to 1.
    Above draft the half-breadth stays constant up to depth.
    """
    offsets: list[tuple[float, float]] = []
    for i in range(n_points):
        z = draft * i / (n_points - 1)
        if draft > 0 and z > 0:
            y = half_beam * (z / draft) ** section_power
        elif draft > 0 and z == 0:
            y = 0.0
        else:
            y = 0.0
        offsets.append((z, y))
    # Add a point at depth with same max half-breadth
    if depth > draft:
        offsets.append((depth, half_beam))
    return HullStation(x_position=x, waterline_offsets=offsets)


def _simple_block_stations(
    length: float, beam: float, draft: float, depth: float,
    cb: float, n_stations: int = 21, section_power: float = 0.1,
) -> list[HullStation]:
    """Generate stations for a hull with target block coefficient.

    Uses a longitudinal fullness distribution:
      A(x) = A_mid * f(x)
    where f(x) varies from 0 at bow/stern to 1 amidships, shaped so
    that the integral of section areas gives the target Cb.

    The section fullness (ratio of section area to bounding rectangle)
    for the power-law shape y = hb * (z/T)^p is 1/(p+1).  The
    longitudinal distribution exponent is chosen so that
    longitudinal_avg * section_fullness = Cb.
    """
    half_beam = beam / 2.0
    section_fullness = 1.0 / (section_power + 1.0)
    # Target longitudinal average: Cb / section_fullness
    long_target = cb / section_fullness

    # Solve for exponent n: average of sin(pi*x/L)^n over [0,L] ≈ long_target
    def _avg_sin_power(n_exp: float) -> float:
        xs = np.linspace(0, 1, 500)
        vals = np.sin(np.pi * xs) ** n_exp
        return float(np.mean(vals))

    n_lo, n_hi = 0.01, 20.0
    for _ in range(100):
        n_mid = (n_lo + n_hi) / 2.0
        if _avg_sin_power(n_mid) > long_target:
            n_lo = n_mid
        else:
            n_hi = n_mid
    n_exp = (n_lo + n_hi) / 2.0

    stations: list[HullStation] = []
    for i in range(n_stations):
        x = length * i / (n_stations - 1)
        xi = x / length  # normalized
        fullness = math.sin(math.pi * xi) ** n_exp if 0 < xi < 1 else 0.0
        local_half_beam = half_beam * fullness
        stations.append(
            _make_station(x, local_half_beam, draft, depth,
                          n_points=21, section_power=section_power)
        )
    return stations


@pytest.fixture
def reference_profile() -> HullProfile:
    """Hull profile: L=100, B=20, T=8, Cb≈0.7."""
    length, beam, draft, depth = 100.0, 20.0, 8.0, 12.0
    cb = 0.7
    stations = _simple_block_stations(length, beam, draft, depth, cb,
                                      n_stations=41)
    return HullProfile(
        name="test_hull_100x20x8",
        hull_type=HullType.SHIP,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="test fixture",
        block_coefficient=cb,
    )


# ---------------------------------------------------------------------------
# HullHydrostatics tests
# ---------------------------------------------------------------------------

class TestHullHydrostatics:
    """Tests for analytical hydrostatic calculations."""

    def test_compute_displaced_volume_within_tolerance(
        self, reference_profile: HullProfile
    ):
        """Volume = L*B*T*Cb = 11200 m³ within 2%."""
        hydro = HullHydrostatics(reference_profile)
        volume = hydro.compute_displaced_volume()
        expected = 100.0 * 20.0 * 8.0 * 0.7  # 11200
        assert volume == pytest.approx(expected, rel=0.02), (
            f"Displaced volume {volume:.1f} not within 2% of {expected}"
        )

    def test_compute_displacement_seawater(
        self, reference_profile: HullProfile
    ):
        """Displacement = volume * 1.025 ≈ 11480 t within 2%."""
        hydro = HullHydrostatics(reference_profile)
        disp = hydro.compute_displacement()
        expected = 11200.0 * 1.025  # 11480
        assert disp == pytest.approx(expected, rel=0.02)

    def test_compute_waterplane_area_reasonable(
        self, reference_profile: HullProfile
    ):
        """Waterplane area should be in plausible range for Cb=0.7."""
        hydro = HullHydrostatics(reference_profile)
        awp = hydro.compute_waterplane_area()
        # Cwp typically 0.80-0.90 for Cb=0.7
        lower = 100.0 * 20.0 * 0.70
        upper = 100.0 * 20.0 * 0.95
        assert lower < awp < upper, (
            f"Waterplane area {awp:.1f} outside plausible range "
            f"[{lower:.0f}, {upper:.0f}]"
        )

    def test_compute_kb_reasonable(self, reference_profile: HullProfile):
        """KB should be between 0.35T and 0.65T for a conventional hull."""
        hydro = HullHydrostatics(reference_profile)
        kb = hydro.compute_kb()
        assert 0.35 * 8.0 < kb < 0.65 * 8.0, (
            f"KB={kb:.2f} outside expected range [2.8, 5.2]"
        )

    def test_compute_bm_transverse_positive(
        self, reference_profile: HullProfile
    ):
        """BM must be positive and in reasonable range."""
        hydro = HullHydrostatics(reference_profile)
        bm = hydro.compute_bm_transverse()
        assert bm > 0, "BM must be positive"
        # BM = I/V, rough check: should be order of magnitude ~5 m
        assert 2.0 < bm < 15.0, f"BM={bm:.2f} outside plausible range"

    def test_compute_all_returns_complete_dict(
        self, reference_profile: HullProfile
    ):
        """compute_all must return all expected keys."""
        hydro = HullHydrostatics(reference_profile)
        result = hydro.compute_all()
        expected_keys = {
            "displaced_volume",
            "displacement",
            "waterplane_area",
            "kb",
            "bm_transverse",
            "gm_transverse",
        }
        assert expected_keys.issubset(result.keys())

    def test_sectional_areas_length_matches_stations(
        self, reference_profile: HullProfile
    ):
        """Number of sectional areas must match number of stations."""
        hydro = HullHydrostatics(reference_profile)
        areas = hydro.compute_sectional_areas()
        assert len(areas) == len(reference_profile.stations)


# ---------------------------------------------------------------------------
# FreeCADHullGenerator tests
# ---------------------------------------------------------------------------

class TestFreeCADHullGenerator:
    """Tests for hull NURBS generation (FreeCAD-independent logic)."""

    def test_generate_nurbs_surface_returns_expected_keys(
        self, reference_profile: HullProfile
    ):
        """Surface result dict must contain required keys."""
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.generate_nurbs_surface()
        assert "surface_points" in result
        assert "stations_used" in result
        assert "surface_type" in result

    def test_generate_nurbs_surface_point_count(
        self, reference_profile: HullProfile
    ):
        """Surface points array should have correct shape."""
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.generate_nurbs_surface()
        pts = result["surface_points"]
        assert isinstance(pts, np.ndarray)
        # Should be 3D: (n_stations, n_waterlines, 3)
        assert pts.ndim == 3
        assert pts.shape[2] == 3  # x, y, z

    def test_generate_nurbs_surface_type_scipy(
        self, reference_profile: HullProfile
    ):
        """Without FreeCAD, surface_type should be 'scipy'."""
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.generate_nurbs_surface()
        if not FREECAD_AVAILABLE:
            assert result["surface_type"] == "scipy"

    def test_create_solid_returns_metadata(
        self, reference_profile: HullProfile
    ):
        """create_solid must return dict with metadata."""
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.create_solid()
        assert isinstance(result, dict)
        assert "length_bp" in result
        assert "beam" in result

    def test_create_solid_with_export_path_no_freecad(
        self, reference_profile: HullProfile, tmp_path,
    ):
        """Without FreeCAD, export_step returns None and warns."""
        if FREECAD_AVAILABLE:
            pytest.skip("Test only for non-FreeCAD environment")
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.export_step(tmp_path / "hull.step")
        assert result is None

    def test_surface_points_within_hull_bounds(
        self, reference_profile: HullProfile
    ):
        """All surface points must lie within principal dimensions."""
        gen = FreeCADHullGenerator(reference_profile)
        result = gen.generate_nurbs_surface()
        pts = result["surface_points"]
        # x in [0, L], y in [-B/2, B/2], z in [0, depth]
        assert np.all(pts[:, :, 0] >= -0.1)
        assert np.all(pts[:, :, 0] <= 100.1)
        assert np.all(np.abs(pts[:, :, 1]) <= 10.1)
        assert np.all(pts[:, :, 2] >= -0.1)
        assert np.all(pts[:, :, 2] <= 12.1)


# ---------------------------------------------------------------------------
# Integration: generator + hydrostatics
# ---------------------------------------------------------------------------

class TestIntegration:
    """Cross-module integration checks."""

    def test_hydrostatics_volume_matches_generator_metadata(
        self, reference_profile: HullProfile
    ):
        """Hydrostatic volume and generator use same profile consistently."""
        hydro = HullHydrostatics(reference_profile)
        gen = FreeCADHullGenerator(reference_profile)
        solid = gen.create_solid()
        vol = hydro.compute_displaced_volume()
        assert solid["length_bp"] == reference_profile.length_bp
        assert vol > 0
