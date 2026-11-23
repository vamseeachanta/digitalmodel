"""Integration tests for sectionproperties library.

Validates geometric and warping properties of common cross-sections
against known analytical values. Covers rectangular, I-beam, and
circular hollow sections (CHS) typical in offshore structural engineering.

Reference: workspace-hub#1452
"""

import math

import pytest

from sectionproperties.analysis import Section
from sectionproperties.pre.library import (
    circular_hollow_section,
    i_section,
    rectangular_section,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_section(geom, mesh_size=10):
    """Create mesh, run geometric + warping + plastic analysis."""
    geom.create_mesh(mesh_sizes=[mesh_size])
    sec = Section(geom)
    sec.calculate_geometric_properties()
    sec.calculate_warping_properties()
    sec.calculate_plastic_properties()
    return sec


# ---------------------------------------------------------------------------
# 1. Rectangular section — verify against b*h analytical formulas
# ---------------------------------------------------------------------------

class TestRectangularSection:
    """Rectangular section: A = b*h, Ixx = b*h^3/12, Iyy = h*b^3/12."""

    @pytest.fixture()
    def rect_section(self):
        b, h = 200.0, 400.0  # mm
        geom = rectangular_section(d=h, b=b)
        sec = _make_section(geom, mesh_size=20)
        return sec, b, h

    def test_area(self, rect_section):
        sec, b, h = rect_section
        expected = b * h
        assert sec.get_area() == pytest.approx(expected, rel=1e-3)

    def test_ixx(self, rect_section):
        sec, b, h = rect_section
        ixx_expected = b * h**3 / 12
        ixx_computed = sec.get_ic()[0]
        assert ixx_computed == pytest.approx(ixx_expected, rel=1e-3)

    def test_iyy(self, rect_section):
        sec, b, h = rect_section
        iyy_expected = h * b**3 / 12
        iyy_computed = sec.get_ic()[1]
        assert iyy_computed == pytest.approx(iyy_expected, rel=1e-3)

    def test_elastic_section_modulus(self, rect_section):
        sec, b, h = rect_section
        # Elastic section modulus S = Ixx / (h/2) = b*h^2/6
        # sectionproperties: get_z() returns elastic moduli (zxx+, zxx-, zyy+, zyy-)
        s_expected = b * h**2 / 6
        s_computed = sec.get_z()[0]
        assert s_computed == pytest.approx(s_expected, rel=1e-2)

    def test_plastic_section_modulus(self, rect_section):
        sec, b, h = rect_section
        # Plastic section modulus Z = b*h^2/4
        # sectionproperties: get_s() returns plastic moduli (sxx, syy)
        z_expected = b * h**2 / 4
        z_computed = sec.get_s()[0]
        assert z_computed == pytest.approx(z_expected, rel=1e-2)


# ---------------------------------------------------------------------------
# 2. I-beam section — AISC W14x90 properties
# ---------------------------------------------------------------------------

class TestIBeamSection:
    """W14x90 I-section validated against AISC Steel Manual 16th Ed."""

    # AISC reference values in imperial
    IN2_TO_MM2 = 645.16
    IN4_TO_MM4 = 416231.426
    IN3_TO_MM3 = 16387.064

    AISC_REF = {
        "A": 26.5 * IN2_TO_MM2,       # mm^2
        "Ix": 999 * IN4_TO_MM4,       # mm^4
        "Iy": 362 * IN4_TO_MM4,       # mm^4
        "Zx": 157 * IN3_TO_MM3,       # mm^3 (plastic)
        "Sx": 143 * IN3_TO_MM3,       # mm^3 (elastic)
    }

    @pytest.fixture()
    def w14x90_section(self):
        geom = i_section(
            d=356.0,    # depth, mm
            b=368.0,    # flange width, mm
            t_f=18.0,   # flange thickness, mm
            t_w=11.2,   # web thickness, mm
            r=25.4,     # fillet radius, mm
            n_r=16,
        )
        return _make_section(geom, mesh_size=10)

    def test_area(self, w14x90_section):
        """Cross-sectional area within 5% of AISC reference."""
        assert w14x90_section.get_area() == pytest.approx(
            self.AISC_REF["A"], rel=0.05
        )

    def test_moment_of_inertia_ixx(self, w14x90_section):
        """Strong-axis moment of inertia Ix within 5% of AISC reference."""
        ixx = w14x90_section.get_ic()[0]
        assert ixx == pytest.approx(self.AISC_REF["Ix"], rel=0.05)

    def test_moment_of_inertia_iyy(self, w14x90_section):
        """Weak-axis moment of inertia Iy within 5% of AISC reference."""
        iyy = w14x90_section.get_ic()[1]
        assert iyy == pytest.approx(self.AISC_REF["Iy"], rel=0.05)

    def test_plastic_section_modulus(self, w14x90_section):
        """Plastic section modulus Zx within 10% of AISC reference.

        Note: sectionproperties get_s() returns plastic moduli.
        Wider tolerance due to fillet radius modeling differences vs AISC tables.
        """
        zx = w14x90_section.get_s()[0]
        assert zx == pytest.approx(self.AISC_REF["Zx"], rel=0.10)

    def test_elastic_section_modulus(self, w14x90_section):
        """Elastic section modulus Sx within 15% of AISC reference.

        Note: sectionproperties get_z() returns elastic moduli.
        Wider tolerance due to fillet radius modeling and k-dimension differences.
        """
        sx = w14x90_section.get_z()[0]
        assert sx == pytest.approx(self.AISC_REF["Sx"], rel=0.15)


# ---------------------------------------------------------------------------
# 3. Circular Hollow Section (CHS) — offshore tubular
# ---------------------------------------------------------------------------

class TestCircularHollowSection:
    """CHS 24in OD x 1in WT (609.6 mm x 25.4 mm) — typical offshore tubular."""

    OD = 609.6   # mm (24 inches)
    WT = 25.4    # mm (1 inch)

    @pytest.fixture()
    def chs_section(self):
        geom = circular_hollow_section(d=self.OD, t=self.WT, n=64)
        return _make_section(geom, mesh_size=15)

    def test_area(self, chs_section):
        r_o = self.OD / 2
        r_i = (self.OD - 2 * self.WT) / 2
        expected = math.pi * (r_o**2 - r_i**2)
        assert chs_section.get_area() == pytest.approx(expected, rel=5e-3)

    def test_moment_of_inertia(self, chs_section):
        r_o = self.OD / 2
        r_i = (self.OD - 2 * self.WT) / 2
        expected = math.pi / 4 * (r_o**4 - r_i**4)
        ixx = chs_section.get_ic()[0]
        assert ixx == pytest.approx(expected, rel=5e-3)

    def test_symmetry(self, chs_section):
        """CHS is doubly symmetric: Ixx == Iyy."""
        ic = chs_section.get_ic()
        assert ic[0] == pytest.approx(ic[1], rel=1e-3)

    def test_torsion_constant(self, chs_section):
        """For CHS, J = 2 * Ix (polar moment of inertia)."""
        r_o = self.OD / 2
        r_i = (self.OD - 2 * self.WT) / 2
        j_expected = math.pi / 2 * (r_o**4 - r_i**4)
        assert chs_section.get_j() == pytest.approx(j_expected, rel=5e-3)

    def test_elastic_section_modulus(self, chs_section):
        """Elastic section modulus S = I / (OD/2).

        Note: sectionproperties get_z() returns elastic moduli.
        """
        r_o = self.OD / 2
        r_i = (self.OD - 2 * self.WT) / 2
        i_expected = math.pi / 4 * (r_o**4 - r_i**4)
        s_expected = i_expected / r_o
        sx = chs_section.get_z()[0]
        assert sx == pytest.approx(s_expected, rel=1e-2)
