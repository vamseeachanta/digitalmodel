"""Tests for digitalmodel.sections — cross-section property computation.

Reference values:
    - W14x90: AISC Steel Construction Manual 16th Ed., converted to mm
    - CHS 914x25: Analytical closed-form formulas
"""

import json
import math

import pytest

from digitalmodel.sections import (
    SectionProperties,
    compute_chs,
    compute_i_section,
    compute_rhs,
)

# ---------------------------------------------------------------------------
# Conversion factors (imperial -> metric)
# ---------------------------------------------------------------------------
IN2_TO_MM2 = 645.16
IN4_TO_MM4 = 416231.426
IN3_TO_MM3 = 16387.064

# ---------------------------------------------------------------------------
# AISC W14x90 reference values (metric, mm)
# ---------------------------------------------------------------------------
W14X90_DIMS = dict(d=356.0, b=368.0, t_f=18.0, t_w=11.2, r=25.4)
W14X90_REF_A = 26.5 * IN2_TO_MM2  # ~17097 mm^2
W14X90_REF_IX = 999 * IN4_TO_MM4  # ~415.8e6 mm^4
W14X90_REF_IY = 362 * IN4_TO_MM4  # ~150.7e6 mm^4

# ---------------------------------------------------------------------------
# CHS 914x25 analytical reference values
# ---------------------------------------------------------------------------
CHS_OD = 914.4
CHS_WT = 25.4
_r_o = CHS_OD / 2
_r_i = (CHS_OD - 2 * CHS_WT) / 2
CHS_REF_A = math.pi * (_r_o**2 - _r_i**2)
CHS_REF_IX = math.pi / 4 * (_r_o**4 - _r_i**4)
CHS_REF_J = math.pi / 2 * (_r_o**4 - _r_i**4)


class TestISectionW14x90:
    """Validate W14x90 I-section against AISC reference values."""

    @pytest.fixture(scope="class")
    def w14x90(self):
        return compute_i_section(**W14X90_DIMS, n_r=16, mesh_size=10, label="W14x90")

    def test_area(self, w14x90):
        assert w14x90.area == pytest.approx(W14X90_REF_A, rel=0.03), (
            f"Area {w14x90.area:.1f} vs ref {W14X90_REF_A:.1f}"
        )

    def test_ixx(self, w14x90):
        assert w14x90.ixx == pytest.approx(W14X90_REF_IX, rel=0.03), (
            f"Ixx {w14x90.ixx:.1f} vs ref {W14X90_REF_IX:.1f}"
        )

    def test_iyy(self, w14x90):
        assert w14x90.iyy == pytest.approx(W14X90_REF_IY, rel=0.03), (
            f"Iyy {w14x90.iyy:.1f} vs ref {W14X90_REF_IY:.1f}"
        )

    def test_section_type(self, w14x90):
        assert w14x90.section_type == "i_section"

    def test_all_fields_populated(self, w14x90):
        assert w14x90.zx > 0
        assert w14x90.sx > 0
        assert w14x90.j > 0
        assert w14x90.cw > 0
        assert w14x90.rx > 0
        assert w14x90.ry > 0


class TestCHS914x25:
    """Validate CHS 914x25 against analytical formulas."""

    @pytest.fixture(scope="class")
    def chs(self):
        return compute_chs(d=CHS_OD, t=CHS_WT, n=64, mesh_size=15, label="CHS 914x25")

    def test_area(self, chs):
        assert chs.area == pytest.approx(CHS_REF_A, rel=0.01), (
            f"Area {chs.area:.1f} vs analytical {CHS_REF_A:.1f}"
        )

    def test_ixx(self, chs):
        assert chs.ixx == pytest.approx(CHS_REF_IX, rel=0.01), (
            f"Ixx {chs.ixx:.1f} vs analytical {CHS_REF_IX:.1f}"
        )

    def test_j(self, chs):
        assert chs.j == pytest.approx(CHS_REF_J, rel=0.01), (
            f"J {chs.j:.1f} vs analytical {CHS_REF_J:.1f}"
        )

    def test_section_type(self, chs):
        assert chs.section_type == "chs"


class TestRHSBasic:
    """Smoke test for rectangular hollow section."""

    @pytest.fixture(scope="class")
    def rhs(self):
        return compute_rhs(d=200, b=100, t=10, r_out=15, n_r=8, mesh_size=10, label="RHS 200x100x10")

    def test_area_positive(self, rhs):
        assert rhs.area > 0

    def test_ixx_gt_iyy(self, rhs):
        # For d > b, Ixx should exceed Iyy
        assert rhs.ixx > rhs.iyy

    def test_section_type(self, rhs):
        assert rhs.section_type == "rhs"

    def test_all_fields_populated(self, rhs):
        assert rhs.zx > 0
        assert rhs.sx > 0
        assert rhs.j > 0
        assert rhs.rx > 0
        assert rhs.ry > 0


class TestSectionPropertiesDataclass:
    """Verify the SectionProperties dataclass behavior."""

    def test_to_dict(self):
        props = SectionProperties(
            label="test",
            section_type="i_section",
            area=100.0,
            ixx=200.0,
            iyy=50.0,
            zx=30.0,
            sx=25.0,
            j=10.0,
            cw=5.0,
            rx=1.41,
            ry=0.71,
        )
        d = props.to_dict()
        assert isinstance(d, dict)
        assert d["label"] == "test"
        assert d["area"] == 100.0
        assert d["ixx"] == 200.0

    def test_to_dict_json_serializable(self):
        props = SectionProperties(
            label="test",
            section_type="chs",
            area=100.0,
            ixx=200.0,
            iyy=50.0,
            zx=30.0,
            sx=25.0,
            j=10.0,
            cw=5.0,
        )
        d = props.to_dict()
        # Must not raise
        json_str = json.dumps(d)
        assert isinstance(json_str, str)
        roundtrip = json.loads(json_str)
        assert roundtrip["section_type"] == "chs"

    def test_optional_fields_default_none(self):
        props = SectionProperties(
            label="minimal",
            section_type="custom",
            area=1.0,
            ixx=1.0,
            iyy=1.0,
            zx=1.0,
            sx=1.0,
            j=1.0,
            cw=1.0,
        )
        assert props.zy is None
        assert props.sy is None
        assert props.rx is None
        assert props.ry is None
